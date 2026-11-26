open Lwt.Infix

exception Truncated

(* A TLS session driven by [Tls.Engine] on the file descriptor, so that an idle
   client can read the records that wait without blocking (see
   [input_waiting]). *)
type socket =
  { fd : Lwt_unix.file_descr
  ; mutable state : Tls.Engine.state
  ; input : Bytes.t
  ; mutable plain : string
  ; mutable plain_off : int
  ; mutable close_notify_received : bool
  ; mutable tcp_eof : bool
  ; mutable close_notify_sent : bool
  ; writing : Lwt_mutex.t
  ; mutable close_delimited : bool
  }

let create fd state =
  { fd
  ; state
  ; input = Bytes.create 0x4000
  ; plain = ""
  ; plain_off = 0
  ; close_notify_received = false
  ; tcp_eof = false
  ; close_notify_sent = false
  ; writing = Lwt_mutex.create ()
  ; close_delimited = false
  }

let close_fd fd =
  match Lwt_unix.state fd with
  | Closed -> Lwt.return_unit
  | _ ->
    (try Lwt_unix.shutdown fd SHUTDOWN_ALL with Unix.Unix_error _ -> ());
    Lwt.catch (fun () -> Lwt_unix.close fd) (fun _ -> Lwt.return_unit)

(* Records go out in the order the engine made them: [send] takes the lock in
   the same step as the engine call that made [data]. *)
let send t data =
  Lwt_mutex.with_lock t.writing (fun () ->
    let rec write off =
      if off = String.length data
      then Lwt.return_unit
      else
        Lwt_unix.write_string t.fd data off (String.length data - off)
        >>= fun n -> write (off + n)
    in
    write 0)

let send_ignoring_errors t data =
  Lwt.async (fun () ->
    Lwt.catch (fun () -> send t data) (fun _ -> Lwt.return_unit))

let feed t raw =
  match Tls.Engine.handle_tls t.state raw with
  | Ok (state, eof, `Response response, `Data data) ->
    t.state <- state;
    if Option.is_some eof then t.close_notify_received <- true;
    Option.iter
      (fun data ->
         t.plain <-
           String.sub t.plain t.plain_off (String.length t.plain - t.plain_off)
           ^ data;
         t.plain_off <- 0)
      data;
    Option.iter (send_ignoring_errors t) response
  | Error (failure, `Response response) ->
    send_ignoring_errors t response;
    raise
      (match failure with
      | `Alert alert -> Tls_lwt.Tls_alert alert
      | failure -> Tls_lwt.Tls_failure failure)

let pull t =
  Lwt.catch
    (fun () -> Lwt_unix.read t.fd t.input 0 (Bytes.length t.input))
    (function
      | Unix.Unix_error ((EBADF | ECONNRESET | ENOTCONN | EPIPE), _, _) ->
        Lwt.return 0
      | exn -> Lwt.reraise exn)
  >|= function
  | 0 -> t.tcp_eof <- true
  | n -> feed t (Bytes.sub_string t.input 0 n)

let ended t = t.close_notify_received || t.tcp_eof

let handshake fd state ~init =
  let t = create fd state in
  Lwt.catch
    (fun () ->
       Option.iter (send_ignoring_errors t) init;
       let rec loop () =
         if not (Tls.Engine.handshake_in_progress t.state)
         then Lwt.return t
         else if ended t
         then Lwt.fail End_of_file
         else pull t >>= loop
       in
       loop ())
    (fun exn -> close_fd fd >>= fun () -> Lwt.reraise exn)

let server_of_fd config fd =
  Lwt.wrap (fun () -> Tls.Engine.server config) >>= fun state ->
  handshake fd state ~init:None

type peer =
  [ `Host of [ `host ] Domain_name.t
  | `Ip of Ipaddr.t
  | `Insecure_no_name_check
  ]

let client_of_fd config ~(peer : peer) fd =
  let config =
    match peer with
    | `Host host -> Tls.Config.peer config host
    | `Ip ip -> Tls.Config.ip config ip
    | `Insecure_no_name_check -> config
  in
  let state, init = Tls.Engine.client config in
  handshake fd state ~init:(Some init)

(* An idle client connection: without blocking, read the TCP bytes that wait and
   give them to the TLS engine. Response bytes other than CR and LF, a
   close_notify or the end of TCP mean that the connection must not be reused
   (undici GHSA-35p6-xmwp-9g52). Decrypted bytes stay for the next read; a
   session ticket or a key update is not input. A blocking descriptor may be
   read by a Lwt thread at the same time: only the bytes that reads returned are
   looked at then. *)
let input_waiting t =
  let rec response_byte i =
    i < String.length t.plain
    && match t.plain.[i] with '\r' | '\n' -> response_byte (i + 1) | _ -> true
  in
  let fd = Lwt_unix.unix_file_descr t.fd in
  let buf = Bytes.create 0x4000 in
  let rec poll () =
    if not (ended t)
    then
      match Unix.read fd buf 0 (Bytes.length buf) with
      | 0 -> t.tcp_eof <- true
      | n ->
        feed t (Bytes.sub_string buf 0 n);
        poll ()
  in
  match
    if
      Lwt_unix.state t.fd = Lwt_unix.Opened
      &&
      match Lwt.state (Lwt_unix.blocking t.fd) with
      | Lwt.Return false -> true
      | Lwt.Return true | Lwt.Fail _ | Lwt.Sleep -> false
    then poll ()
  with
  | () | (exception Unix.Unix_error ((EAGAIN | EWOULDBLOCK | EINTR), _, _)) ->
    ended t || response_byte t.plain_off
  | exception (Unix.Unix_error _ | Tls_lwt.Tls_alert _ | Tls_lwt.Tls_failure _)
    ->
    true

let handshake_limit idle_timeout head_timeout =
  match idle_timeout, head_timeout with
  | None, None -> None
  | Some seconds, None | None, Some seconds -> Some seconds
  | Some idle, Some head -> Some (Float.min idle head)

let epoch t = Tls.Engine.epoch t.state

(* ocaml-tls makes one record for each string it is given, so the iovecs are
   joined into strings of up to 16 KiB, the largest record. *)
let record_size = 0x4000

let coalesce iovecs =
  let rec fill chunk pos iovecs src_off =
    let size = Bytes.length chunk in
    match iovecs with
    | [] -> [], 0
    | _ when pos = size -> iovecs, src_off
    | { Ache.IOVec.buffer; off; len } :: rest ->
      let n = min (len - src_off) (size - pos) in
      Ache.Bigstring.unsafe_blit_to_bytes
        buffer
        ~src_off:(off + src_off)
        chunk
        ~dst_off:pos
        ~len:n;
      if src_off + n = len
      then fill chunk (pos + n) rest 0
      else iovecs, src_off + n
  in
  let rec chunks acc remaining iovecs src_off =
    if remaining = 0
    then List.rev acc
    else
      let chunk = Bytes.create (min record_size remaining) in
      let iovecs, src_off = fill chunk 0 iovecs src_off in
      chunks
        (Bytes.unsafe_to_string chunk :: acc)
        (remaining - Bytes.length chunk)
        iovecs
        src_off
  in
  let length = Ache.IOVec.lengthv iovecs in
  length, chunks [] length iovecs 0

module Io = struct
  type nonrec socket = socket
  type addr = Unix.sockaddr

  let rec read t buffer ~off ~len =
    let available = String.length t.plain - t.plain_off in
    if available > 0
    then (
      let n = min len available in
      Ache.Bigstring.blit_from_string
        t.plain
        ~src_off:t.plain_off
        buffer
        ~dst_off:off
        ~len:n;
      t.plain_off <- t.plain_off + n;
      Lwt.return n)
    else if t.close_notify_received
    then Lwt.fail End_of_file
    else if t.tcp_eof
    then Lwt.fail (if t.close_delimited then Truncated else End_of_file)
    else pull t >>= fun () -> read t buffer ~off ~len

  let writev t iovecs =
    if t.close_notify_sent
    then Lwt.return `Closed
    else
      let length, strings = coalesce iovecs in
      match Tls.Engine.send_application_data t.state strings with
      | None ->
        Lwt.fail_invalid_arg "Ache_lwt_tls: the TLS session is not ready"
      | Some (state, data) ->
        t.state <- state;
        Lwt.catch
          (fun () -> send t data >|= fun () -> `Ok length)
          (function
            | Unix.Unix_error ((EBADF | EPIPE | ECONNRESET | ENOTCONN), _, _) ->
              Lwt.return `Closed
            | exn -> Lwt.reraise exn)

  let shutdown_receive t =
    if Lwt_unix.state t.fd <> Lwt_unix.Closed
    then
      try Lwt_unix.shutdown t.fd Unix.SHUTDOWN_RECEIVE with
      | Unix.Unix_error _ -> ()

  (* close_notify, then the end of the TCP output, after the writes in
     progress. *)
  let shutdown_send t =
    if (not t.close_notify_sent) && Lwt_unix.state t.fd = Lwt_unix.Opened
    then (
      t.close_notify_sent <- true;
      let state, data = Tls.Engine.send_close_notify t.state in
      t.state <- state;
      Lwt.async (fun () ->
        Lwt.catch
          (fun () ->
             send t data >|= fun () ->
             if Lwt_unix.state t.fd = Lwt_unix.Opened
             then Lwt_unix.shutdown t.fd Unix.SHUTDOWN_SEND)
          (fun (_ : exn) -> Lwt.return_unit)))

  (* close_notify goes out only if it does not wait: not behind a write in
     progress, and in one write that does not block. *)
  let close t =
    if
      (not t.close_notify_sent)
      && (not (Lwt_mutex.is_locked t.writing))
      && Lwt_unix.state t.fd = Lwt_unix.Opened
    then (
      t.close_notify_sent <- true;
      let state, data = Tls.Engine.send_close_notify t.state in
      t.state <- state;
      try
        ignore
          (Unix.single_write_substring
             (Lwt_unix.unix_file_descr t.fd)
             data
             0
             (String.length data)
           : int)
      with
      | Unix.Unix_error _ -> ());
    t.close_notify_sent <- true;
    close_fd t.fd
end

include
  Ache_lwt.Make_with_idle_check
    (Io)
    (struct
      let input_waiting = input_waiting
    end)

module Server = struct
  include Server

  let create_connection_handler_with_fd
        ?config
        ?(idle_timeout = Some 60.)
        ?(head_timeout = Some 30.)
        ?body_timeout
        ?write_timeout
        ?close_timeout
        ?date
        ?stop
        ?grace
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
      [ idle_timeout; head_timeout; grace ];
    List.iter
      (Option.iter
         (Option.iter (fun seconds ->
            if not (seconds > 0.)
            then
              invalid_arg
                "Ache_lwt_tls.Server.create_connection_handler_with_fd: a \
                 timeout must be positive")))
      [ body_timeout; write_timeout; close_timeout ];
    match stop with
    | Some stop when not (Lwt.is_sleeping stop) -> close_fd fd
    | _ ->
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
      (* A handshake in progress at [stop] is closed, as an idle connection. *)
      Option.iter
        (fun stop ->
           Lwt.on_termination
             (Lwt.choose [ stop; Lwt.map ignore handshake ])
             (fun () ->
                if Lwt.is_sleeping handshake
                then Lwt.async (fun () -> close_fd fd)))
        stop;
      Lwt.try_bind
        (fun () -> handshake)
        (fun socket ->
           create_connection_handler
             ?config
             ~idle_timeout
             ~head_timeout
             ?body_timeout
             ?write_timeout
             ?close_timeout
             ?date
             ?stop
             ?grace
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
