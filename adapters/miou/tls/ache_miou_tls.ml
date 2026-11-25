exception Truncated

type socket =
  { fd : Miou_unix.file_descr
  ; mutable state : Tls.Engine.state
  ; input : bytes
  ; mutable plain : string
  ; mutable plain_off : int
  ; mutable close_notify_received : bool
  ; mutable tcp_eof : bool
  ; mutable close_notify_sent : bool
  ; output : string Queue.t
  ; mutable writing : bool
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
  ; output = Queue.create ()
  ; writing = false
  ; close_delimited = false
  }

let rec drain t =
  if (not t.writing) && not (Queue.is_empty t.output)
  then (
    t.writing <- true;
    match Miou_unix.write t.fd (Queue.pop t.output) with
    | () ->
      t.writing <- false;
      drain t
    | exception exn ->
      t.writing <- false;
      raise exn)

let send t data =
  Queue.push data t.output;
  drain t

let send_ignoring_errors t data = try send t data with Unix.Unix_error _ -> ()

let pull t =
  match Miou_unix.read t.fd t.input with
  | 0 | (exception Unix.Unix_error ((ECONNRESET | ENOTCONN), _, _)) ->
    t.tcp_eof <- true
  | n ->
    (match Tls.Engine.handle_tls t.state (Bytes.sub_string t.input 0 n) with
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
        | `Alert alert -> Tls_miou_unix.Tls_alert alert
        | failure -> Tls_miou_unix.Tls_failure failure))

let ended t = t.close_notify_received || t.tcp_eof

let handshake fd state ~init =
  let t = create fd state in
  Option.iter (send t) init;
  while Tls.Engine.handshake_in_progress t.state do
    if ended t then raise End_of_file;
    pull t
  done;
  t

let server_of_fd config fd = handshake fd (Tls.Engine.server config) ~init:None

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

let epoch t = Tls.Engine.epoch t.state

module Io = struct
  type nonrec socket = socket

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
      n)
    else if t.close_notify_received
    then raise End_of_file
    else if t.tcp_eof
    then raise (if t.close_delimited then Truncated else End_of_file)
    else (
      pull t;
      read t buffer ~off ~len)

  let writev t iovecs =
    if t.close_notify_sent
    then `Closed
    else
      let length, strings =
        List.fold_left_map
          (fun acc { Ache.IOVec.buffer; off; len } ->
             acc + len, Ache.Bigstring.substring buffer ~off ~len)
          0
          iovecs
      in
      match Tls.Engine.send_application_data t.state strings with
      | None -> invalid_arg "Ache_miou_tls: the TLS session is not ready"
      | Some (state, data) ->
        t.state <- state;
        (match send t data with
        | () -> `Ok length
        | exception Unix.Unix_error ((EPIPE | ECONNRESET | ENOTCONN), _, _) ->
          `Closed)

  let shutdown_tcp t command =
    try Unix.shutdown (Miou_unix.to_file_descr t.fd) command with
    | Unix.Unix_error _ -> ()

  let shutdown t command =
    (match command with
    | Unix.SHUTDOWN_SEND | SHUTDOWN_ALL ->
      if not t.close_notify_sent
      then (
        t.close_notify_sent <- true;
        let state, data = Tls.Engine.send_close_notify t.state in
        t.state <- state;
        send_ignoring_errors t data)
    | SHUTDOWN_RECEIVE -> ());
    shutdown_tcp t command
end

include Ache_miou.Make (Io)

let handshake_limit idle_timeout head_timeout =
  match idle_timeout, head_timeout with
  | None, None -> None
  | Some seconds, None | None, Some seconds -> Some seconds
  | Some idle, Some head -> Some (Float.min idle head)

let longest_sleep = 0.5

let sleep_until at =
  let rec loop () =
    let left = at -. Unix.gettimeofday () in
    if left > 0.
    then (
      Miou_unix.sleep (Float.min longest_sleep left);
      loop ())
  in
  loop ()

module Server = struct
  include Server

  let create_connection_handler_with_fd
        ?config
        ?idle_timeout
        ?head_timeout
        ?date
        ?stop
        ?grace
        ~tls
        ~request_handler
        ~error_handler
    =
    let handler =
      create_connection_handler
        ?config
        ?idle_timeout
        ?head_timeout
        ?date
        ?stop
        ?grace
        ~request_handler
        ~error_handler
    in
    fun fd client_addr ->
      let stopped =
        match stop with
        | None -> []
        | Some stop ->
          [ Miou.async (fun () ->
              (match Miou.Computation.await stop with
              | Error (exn, bt) when Miou.Computation.is_running stop ->
                Printexc.raise_with_backtrace exn bt
              | Ok () | Error _ -> ());
              None)
          ]
      in
      let timer =
        match handshake_limit idle_timeout head_timeout with
        | None -> []
        | Some seconds ->
          let at = Unix.gettimeofday () +. seconds in
          [ Miou.async (fun () ->
              sleep_until at;
              None)
          ]
      in
      match
        Miou.await_first
          ((Miou.async (fun () -> Some (server_of_fd tls fd)) :: timer)
          @ stopped)
      with
      | Ok (Some socket) -> handler socket client_addr
      | Ok None | Error _ -> ()
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
    { client = Client.create_connection ?config socket; socket }

  let create_connection_with_fd ?config ~tls ~peer fd =
    create_connection ?config (client_of_fd tls ~peer fd)

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
