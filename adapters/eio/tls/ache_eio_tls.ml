exception Truncated

type flow = [ Eio.Flow.two_way_ty | Eio.Resource.close_ty ] Eio.Resource.t

(* tls-eio makes one record for each buffer it is given. [single_write] joins
   the leading buffers into one record of up to 16 KiB, the largest record,
   through [scratch], one buffer for each connection. A first buffer of 16 KiB
   or more goes as is. [Eio.Flow.write] calls it again for the rest. *)
let record_size = 0x4000
let scratch () = Cstruct.create record_size

let single_write tls scratch = function
  | first :: _ when Cstruct.length first >= record_size ->
    Eio.Flow.single_write tls [ first ]
  | bufs ->
    let rec fill pos = function
      | [] -> pos
      | buf :: rest ->
        let n = min (Cstruct.length buf) (record_size - pos) in
        Cstruct.blit buf 0 scratch pos n;
        if pos + n < record_size then fill (pos + n) rest else pos + n
    in
    Eio.Flow.single_write tls [ Cstruct.sub scratch 0 (fill 0 bufs) ]

(* The client side: a TLS session driven by [Tls.Engine] on the TCP flow, so
   that an idle connection can read the records that wait without blocking (see
   [input_waiting]). *)
module Client_flow = struct
  type t =
    { tcp : flow
    ; fd : Eio_unix.Fd.t option (* non-blocking *)
    ; mutable state : Tls.Engine.state
    ; input : Cstruct.t
    ; mutable plain : string
    ; mutable plain_off : int
    ; mutable close_notify_received : bool
    ; mutable tcp_eof : bool
    ; mutable close_notify_sent : bool
    ; output : string Queue.t
    ; mutable writing : bool
    ; written : Eio.Condition.t
    ; mutable close_delimited : bool
    }

  (* Records go out in the order the engine made them, from one fiber at a time.
     A fiber whose records another fiber is writing waits for it. *)
  let rec flush t =
    if not (Queue.is_empty t.output)
    then
      if t.writing
      then (
        Eio.Condition.await_no_mutex t.written;
        flush t)
      else (
        t.writing <- true;
        Fun.protect
          ~finally:(fun () ->
            t.writing <- false;
            Eio.Condition.broadcast t.written)
          (fun () ->
             while not (Queue.is_empty t.output) do
               Eio.Flow.copy_string (Queue.pop t.output) t.tcp
             done))

  let send t data =
    Queue.push data t.output;
    flush t

  let send_ignoring_errors t data =
    try send t data with Eio.Cancel.Cancelled _ as exn -> raise exn | _ -> ()

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
        | `Alert alert -> Tls_eio.Tls_alert alert
        | failure -> Tls_eio.Tls_failure failure)

  let pull t =
    match Eio.Flow.single_read t.tcp t.input with
    | n -> feed t (Cstruct.to_string t.input ~off:0 ~len:n)
    | exception (End_of_file | Eio.Io (Eio.Net.E (Connection_reset _), _)) ->
      t.tcp_eof <- true

  let ended t = t.close_notify_received || t.tcp_eof

  let handshake ~fd tcp state ~init =
    let t =
      { tcp
      ; fd
      ; state
      ; input = Cstruct.create 0x4000
      ; plain = ""
      ; plain_off = 0
      ; close_notify_received = false
      ; tcp_eof = false
      ; close_notify_sent = false
      ; output = Queue.create ()
      ; writing = false
      ; written = Eio.Condition.create ()
      ; close_delimited = false
      }
    in
    send t init;
    while Tls.Engine.handshake_in_progress t.state do
      if ended t then raise End_of_file;
      pull t
    done;
    t

  (* An idle connection: without blocking, read the TCP bytes that wait and give
     them to the TLS engine. Response bytes other than CR and LF, a close_notify
     or the end of TCP mean that the connection must not be reused (undici
     GHSA-35p6-xmwp-9g52). Decrypted bytes stay for the next read; a session
     ticket or a key update is not input. Without a non-blocking descriptor
     ([eio_linux]) only the records that reads returned are looked at. *)
  let input_waiting t () =
    let rec response_byte i =
      i < String.length t.plain
      &&
      match t.plain.[i] with
      | '\r' | '\n' -> response_byte (i + 1)
      | _ -> true
    in
    let buf = Bytes.create 0x4000 in
    let rec poll fd =
      if not (ended t)
      then
        match Unix.read fd buf 0 (Bytes.length buf) with
        | 0 -> t.tcp_eof <- true
        | n ->
          feed t (Bytes.sub_string buf 0 n);
          poll fd
    in
    match
      Option.iter (fun fd -> Eio_unix.Fd.use fd ~if_closed:ignore poll) t.fd
    with
    | () | (exception Unix.Unix_error ((EAGAIN | EWOULDBLOCK | EINTR), _, _)) ->
      ended t || response_byte t.plain_off
    | exception (Eio.Cancel.Cancelled _ as exn) -> raise exn
    | exception _ -> true

  let read_methods = []

  let rec single_read t buf =
    let available = String.length t.plain - t.plain_off in
    if available > 0
    then (
      let n = min (Cstruct.length buf) available in
      Cstruct.blit_from_string t.plain t.plain_off buf 0 n;
      t.plain_off <- t.plain_off + n;
      n)
    else if t.close_notify_received
    then raise End_of_file
    else if t.tcp_eof
    then raise (if t.close_delimited then Truncated else End_of_file)
    else (
      pull t;
      single_read t buf)

  (* One record of up to 16 KiB from the leading buffers. *)
  let single_write t bufs =
    if t.close_notify_sent then raise End_of_file;
    let record = Buffer.create (min record_size (Cstruct.lenv bufs)) in
    let rec fill = function
      | [] -> ()
      | buf :: rest ->
        let n = min (Cstruct.length buf) (record_size - Buffer.length record) in
        Buffer.add_string record (Cstruct.to_string buf ~off:0 ~len:n);
        if Buffer.length record < record_size then fill rest
    in
    fill bufs;
    match
      Tls.Engine.send_application_data t.state [ Buffer.contents record ]
    with
    | None -> invalid_arg "Ache_eio_tls: the TLS session is not ready"
    | Some (state, data) ->
      t.state <- state;
      send t data;
      Buffer.length record

  let copy t ~src = Eio.Flow.Pi.simple_copy ~single_write t ~src

  (* [`All] stops the connection at once, maybe because a write is stuck: it
     sends close_notify only if no write is in progress. *)
  let shutdown t command =
    match command with
    | `Receive -> ()
    | (`Send | `All) when t.close_notify_sent -> ()
    | `All when t.writing || not (Queue.is_empty t.output) ->
      t.close_notify_sent <- true
    | `Send | `All ->
      t.close_notify_sent <- true;
      let state, data = Tls.Engine.send_close_notify t.state in
      t.state <- state;
      send t data

  let close t = Eio.Resource.close t.tcp
end

module Server_flow = struct
  type t =
    { tls : Tls_eio.t
    ; tcp : flow
    ; scratch : Cstruct.t
    }

  let read_methods = []
  let single_read t buf = Eio.Flow.single_read t.tls buf
  let single_write t bufs = single_write t.tls t.scratch bufs
  let copy t ~src = Eio.Flow.Pi.simple_copy ~single_write t ~src

  let shutdown t = function
    | `Receive -> Eio.Flow.shutdown t.tcp `Receive
    | (`Send | `All) as command -> Eio.Flow.shutdown t.tls command

  let close t = Eio.Resource.close t.tls
end

let two_way
      (type t)
      (module Tls_flow : Eio.Flow.Pi.TWO_WAY with type t = t)
      close
  =
  Eio.Resource.handler
    (Eio.Resource.bindings (Eio.Flow.Pi.two_way (module Tls_flow))
    @ [ H (Eio.Resource.Close, close) ])

let client_handler = two_way (module Client_flow) Client_flow.close
let server_handler = two_way (module Server_flow) Server_flow.close

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

type peer =
  [ `Host of [ `host ] Domain_name.t
  | `Ip of Ipaddr.t
  | `Insecure_no_name_check
  ]

module Client = struct
  type t =
    { client : Ache_eio.Client.t
    ; flow : Client_flow.t
    }

  let non_blocking fd =
    match Eio_unix.Fd.is_blocking fd with
    | blocking -> not blocking
    | exception _ -> false

  let create_connection ?config ~sw ~tls ~(peer : peer) flow =
    let config_tls =
      match peer with
      | `Host host -> Tls.Config.peer tls host
      | `Ip ip -> Tls.Config.ip tls ip
      | `Insecure_no_name_check -> tls
    in
    let fd =
      match Eio_unix.Resource.fd_opt flow with
      | Some fd when non_blocking fd -> Some fd
      | Some _ | None -> None
    in
    let state, init = Tls.Engine.client config_tls in
    let tls_flow = Client_flow.handshake ~fd (flow :> flow) state ~init in
    let resource : flow = Eio.Resource.T (tls_flow, client_handler) in
    { client =
        Ache_eio.Client.create_connection
          ?config
          ~input_waiting:(Client_flow.input_waiting tls_flow)
          ~sw
          resource
    ; flow = tls_flow
    }

  let request
        t
        ?flush_headers_immediately
        request
        ~error_handler
        ~response_handler
    =
    Ache_eio.Client.request
      t.client
      ?flush_headers_immediately
      request
      ~error_handler
      ~response_handler:(fun response body ->
        if close_delimited request response then t.flow.close_delimited <- true;
        response_handler response body)

  let shutdown t = Ache_eio.Client.shutdown t.client
  let is_closed t = Ache_eio.Client.is_closed t.client
  let upgrade t impl = Ache_eio.Client.upgrade t.client impl
end

module Server = struct
  let create_connection_handler
        ?config
        ?clock
        ?(idle_timeout = Some 60.)
        ?(head_timeout = Some 30.)
        ?close_timeout
        ?body_timeout
        ?write_timeout
        ?date
        ?stop
        ?grace
        ~tls
        ~request_handler
        ~error_handler
    =
    let handler =
      Ache_eio.Server.create_connection_handler
        ?config
        ?clock
        ~idle_timeout
        ~head_timeout
        ?close_timeout
        ?body_timeout
        ?write_timeout
        ?date
        ?stop
        ?grace
        ~request_handler
        ~error_handler
    in
    let limit =
      match idle_timeout, head_timeout with
      | None, None -> None
      | Some seconds, None | None, Some seconds -> Some seconds
      | Some idle, Some head -> Some (Float.min idle head)
    in
    fun tcp client_addr ->
      let handshake () = Tls_eio.server_of_flow tls tcp in
      let handshake () =
        match clock, limit with
        | Some clock, Some seconds ->
          Eio.Time.with_timeout_exn clock seconds handshake
        | None, Some seconds ->
          Eio.Fiber.first
            (fun () ->
               Eio_unix.sleep seconds;
               raise Eio.Time.Timeout)
            handshake
        | _, None -> handshake ()
      in
      let flow =
        match stop with
        | None -> Some (handshake ())
        | Some stop when Eio.Promise.is_resolved stop -> None
        | Some stop ->
          Eio.Fiber.first
            (fun () -> Some (handshake ()))
            (fun () ->
               Eio.Promise.await stop;
               None)
      in
      Option.iter
        (fun flow ->
           handler
             (Eio.Resource.T
                ( { Server_flow.tls = flow; tcp :> flow; scratch = scratch () }
                , server_handler )
              : flow)
             client_addr)
        flow
end
