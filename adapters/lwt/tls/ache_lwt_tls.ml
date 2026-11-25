open Lwt.Infix

exception Truncated

type socket =
  { fd : Lwt_unix.file_descr
  ; tls : Tls_lwt.Unix.t
  ; output : Lwt_io.output_channel
  ; tcp_eof : bool ref
  ; closing : bool ref
  ; mutable close_delimited : bool
  }

let channel_buffer_size = 17 * 1024

let channels fd ~tcp_eof ~closing =
  let input =
    Lwt_io.make
      ~buffer:(Lwt_bytes.create channel_buffer_size)
      ~mode:Lwt_io.Input
      (fun buf off len ->
         Lwt_bytes.read fd buf off len >|= fun n ->
         if n = 0 then tcp_eof := true;
         n)
  in
  let output =
    Lwt_io.make
      ~buffer:(Lwt_bytes.create channel_buffer_size)
      ~mode:Lwt_io.Output
      (fun buf off len ->
         if !closing
         then
           match
             Unix.single_write_bigarray
               (Lwt_unix.unix_file_descr fd)
               buf
               off
               len
           with
           | n -> Lwt.return n
           | exception exn -> Lwt.fail exn
         else Lwt_bytes.write fd buf off len)
  in
  input, output

let close_fd fd =
  match Lwt_unix.state fd with
  | Closed -> Lwt.return_unit
  | _ ->
    (try Lwt_unix.shutdown fd SHUTDOWN_ALL with Unix.Unix_error _ -> ());
    Lwt.catch (fun () -> Lwt_unix.close fd) (fun _ -> Lwt.return_unit)

let handshake fd start =
  let tcp_eof = ref false
  and closing = ref false in
  let input, output = channels fd ~tcp_eof ~closing in
  Lwt.catch
    (fun () ->
       start (input, output) >|= fun tls ->
       { fd; tls; output; tcp_eof; closing; close_delimited = false })
    (fun exn -> close_fd fd >>= fun () -> Lwt.reraise exn)

let server_of_fd config fd =
  handshake fd (Tls_lwt.Unix.server_of_channels config)

type peer =
  [ `Host of [ `host ] Domain_name.t
  | `Ip of Ipaddr.t
  | `Insecure_no_name_check
  ]

let client_of_fd config ~(peer : peer) fd =
  let host, ip =
    match peer with
    | `Host host -> Some host, None
    | `Ip ip -> None, Some ip
    | `Insecure_no_name_check -> None, None
  in
  handshake fd (Tls_lwt.Unix.client_of_channels config ?host ?ip)

let handshake_limit idle_timeout head_timeout =
  match idle_timeout, head_timeout with
  | None, None -> None
  | Some seconds, None | None, Some seconds -> Some seconds
  | Some idle, Some head -> Some (Float.min idle head)

let epoch t = Tls_lwt.Unix.epoch t.tls

module Io = struct
  type nonrec socket = socket
  type addr = Unix.sockaddr

  let read t buffer ~off ~len =
    Lwt.catch
      (fun () -> Tls_lwt.Unix.read_bytes t.tls buffer off len)
      (function
        | Unix.Unix_error ((ECONNRESET | ENOTCONN | EPIPE), _, _)
          when t.close_delimited ->
          Lwt.fail Truncated
        | Unix.Unix_error ((EBADF | ECONNRESET | ENOTCONN | EPIPE), _, _)
        | Lwt_io.Channel_closed _ ->
          Lwt.return 0
        | exn -> Lwt.fail exn)
    >>= function
    | 0 when t.close_delimited && !(t.tcp_eof) -> Lwt.fail Truncated
    | 0 -> Lwt.fail End_of_file
    | n -> Lwt.return n

  let writev t iovecs =
    let length, strings =
      List.fold_left_map
        (fun acc { Ache.IOVec.buffer; off; len } ->
           acc + len, Ache.Bigstring.substring buffer ~off ~len)
        0
        iovecs
    in
    Lwt.catch
      (fun () ->
         Tls_lwt.Unix.writev t.tls strings >>= fun () ->
         Lwt_io.flush t.output >|= fun () -> `Ok length)
      (function
        | Unix.Unix_error ((EBADF | EPIPE | ECONNRESET | ENOTCONN), _, _)
        | Lwt_io.Channel_closed _ ->
          Lwt.return `Closed
        | exn -> Lwt.fail exn)

  let shutdown_receive t =
    if Lwt_unix.state t.fd <> Lwt_unix.Closed
    then
      try Lwt_unix.shutdown t.fd Unix.SHUTDOWN_RECEIVE with
      | Unix.Unix_error _ -> ()

  let close t =
    t.closing := true;
    (if Lwt_io.is_busy t.output
     then Lwt.return_unit
     else
       Lwt.catch (fun () -> Tls_lwt.Unix.close t.tls) (fun _ -> Lwt.return_unit))
    >>= fun () -> close_fd t.fd
end

include Ache_lwt.Make (Io)

module Server = struct
  include Server

  let create_connection_handler_with_fd
        ?config
        ?idle_timeout
        ?head_timeout
        ?date
        ~tls
        ~request_handler
        ~error_handler
        client_addr
        fd
    =
    List.iter
      (Option.iter (fun seconds ->
         if not (seconds > 0.)
         then
           invalid_arg
             "Ache_lwt_tls.Server.create_connection_handler_with_fd: a timeout \
              must be positive"))
      [ idle_timeout; head_timeout ];
    let handshake = server_of_fd tls fd in
    let handshake =
      match handshake_limit idle_timeout head_timeout with
      | None -> handshake
      | Some seconds ->
        let timer = Ache_lwt.Timeout.create seconds in
        Ache_lwt.Timeout.set timer;
        Lwt.finalize
          (fun () ->
             Lwt.pick
               [ handshake
               ; ( Ache_lwt.Timeout.expired timer >>= fun () ->
                   Lwt.fail Lwt_unix.Timeout )
               ])
          (fun () ->
             Ache_lwt.Timeout.stop timer;
             Lwt.return_unit)
    in
    Lwt.try_bind
      (fun () -> handshake)
      (fun socket ->
         create_connection_handler
           ?config
           ?idle_timeout
           ?head_timeout
           ?date
           ~request_handler
           ~error_handler
           client_addr
           socket)
      (fun (_ : exn) -> close_fd fd)
end

let close_delimited (request : Ache.Request.t) (response : Ache.Response.t) =
  let request_method =
    match request.meth with
    | #Ache.Method.standard as meth -> meth
    | `Other _ -> `GET
  in
  match response.status, request_method with
  | `Switching_protocols, _ -> false
  | status, `CONNECT when Ache.Status.is_successful status -> false
  | _ -> Ache.Response.body_length ~request_method response = `Close_delimited

module Client = struct
  type socket = Io.socket

  type t =
    { client : Client.t
    ; socket : socket
    }

  let create_connection ?config socket =
    Client.create_connection ?config socket >|= fun client -> { client; socket }

  let create_connection_with_fd ?config ~tls ~peer fd =
    client_of_fd tls ~peer fd >>= create_connection ?config

  let request
        t
        ?flush_headers_immediately
        request
        ~error_handler
        ~response_handler
    =
    Client.request
      t.client
      ?flush_headers_immediately
      request
      ~error_handler
      ~response_handler:(fun response body ->
        if close_delimited request response
        then t.socket.close_delimited <- true;
        response_handler response body)

  let shutdown t = Client.shutdown t.client
  let is_closed t = Client.is_closed t.client
  let upgrade t impl = Client.upgrade t.client impl
end
