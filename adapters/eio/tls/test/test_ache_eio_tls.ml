open Ache

let certs = Ache_test_certs.generate ()
let server_config = Ache_test_certs.server_config certs
let client_config = Ache_test_certs.client_config certs
let localhost = Domain_name.(host_exn (of_string_exn "localhost"))
let server_errors : Server_connection.error list ref = ref []
let partial_body_cut = ref false

module Echo = struct
  type t =
    { mutable pending : string
    ; on_read : t -> string -> unit
    ; mutable closed : bool
    ; mutable reader_k : (unit -> unit) option
    ; mutable writer_k : (unit -> unit) option
    }

  let create ?(pending = "") on_read =
    { pending; on_read; closed = false; reader_k = None; writer_k = None }

  let wake k = match k with None -> () | Some k -> k ()

  let wakeup_writer t =
    let k = t.writer_k in
    t.writer_k <- None;
    wake k

  let send t s =
    t.pending <- t.pending ^ s;
    wakeup_writer t

  let shutdown t =
    t.closed <- true;
    let k = t.reader_k in
    t.reader_k <- None;
    wake k;
    wakeup_writer t

  let next_read_operation t = if t.closed then `Close else `Read

  let read t bs ~off ~len =
    if len > 0 then t.on_read t (Ache.Bigstring.substring bs ~off ~len);
    len

  let read_eof t bs ~off ~len =
    let n = read t bs ~off ~len in
    shutdown t;
    n

  let yield_reader t k = t.reader_k <- Some k

  let next_write_operation t =
    let len = String.length t.pending in
    if len > 0
    then
      `Write
        [ { IOVec.buffer = Ache.Bigstring.of_string t.pending ~off:0 ~len
          ; off = 0
          ; len
          }
        ]
    else if t.closed
    then `Close 0
    else `Yield

  let report_write_result t = function
    | `Ok n -> t.pending <- String.sub t.pending n (String.length t.pending - n)
    | `Closed ->
      t.pending <- "";
      shutdown t

  let yield_writer t k = t.writer_k <- Some k
  let report_exn t _ = shutdown t
  let is_closed t = t.closed && t.pending = ""
end

let pending_upgrade : Request_descriptor.t option ref = ref None
let pending_body : Request_descriptor.t option ref = ref None
let pending_body_failed = ref false
let stream_closed : unit Eio.Promise.u option ref = ref None

(* A chunked response of many small buffers, each its own iovec. *)
let stream_piece = String.make 64 'x'
let stream_pieces = 1024

let request_handler (_ : Eio.Net.Sockaddr.stream) reqd =
  let request = Request_descriptor.request reqd in
  let body = Request_descriptor.request_body reqd in
  match request.meth, request.target with
  | `POST, "/echo" ->
    let headers =
      Headers.of_list
        [ ( "content-length"
          , Option.get (Headers.get request.headers "content-length") )
        ]
    in
    let response_body =
      Request_descriptor.respond_with_streaming
        reqd
        (Response.create ~headers `OK)
    in
    let rec on_read buffer ~off ~len =
      Body.Writer.write_string
        response_body
        (Ache.Bigstring.substring buffer ~off ~len);
      Body.Writer.flush response_body (fun _ ->
        Body.Reader.schedule_read body ~on_eof ~on_error ~on_read)
    and on_eof () = Body.Writer.close response_body
    and on_error () = Body.Writer.close response_body in
    Body.Reader.schedule_read body ~on_eof ~on_error ~on_read
  | `POST, "/partial" ->
    let rec on_read _ ~off:_ ~len:_ =
      Body.Reader.schedule_read body ~on_eof ~on_error ~on_read
    and on_eof () = Alcotest.fail "a cut body completed"
    and on_error () = partial_body_cut := true in
    Body.Reader.schedule_read body ~on_eof ~on_error ~on_read
  | `GET, "/upgrade" ->
    Body.Reader.close body;
    Request_descriptor.respond_with_upgrade
      reqd
      (Headers.of_list [ "connection", "upgrade"; "upgrade", "echo" ])
      (Runtime.make (module Echo) (Echo.create (fun t s -> Echo.send t s)))
  | `GET, "/upgrade-later" -> pending_upgrade := Some reqd
  | `GET, "/ws-echo" ->
    let rec echo ws =
      match Ache_eio.Websocket.receive ws with
      | Some (Text message) ->
        Ache_eio.Websocket.send_text ws message;
        echo ws
      | Some (Binary message) ->
        Ache_eio.Websocket.send_binary ws message;
        echo ws
      | None -> ()
    in
    (match Ache_eio.Websocket.upgrade reqd echo with
    | Ok () -> ()
    | Error _ -> Alcotest.fail "handshake rejected")
  | `POST, "/respond-later" ->
    pending_body := Some reqd;
    let rec on_read _ ~off:_ ~len:_ =
      Body.Reader.schedule_read body ~on_eof ~on_error ~on_read
    and on_eof () = ()
    and on_error () = pending_body_failed := true in
    Body.Reader.schedule_read body ~on_eof ~on_error ~on_read
  | `GET, "/stream-forever" ->
    Body.Reader.close body;
    let response_body =
      Request_descriptor.respond_with_streaming reqd (Response.create `OK)
    in
    let chunk = String.make 65536 'x' in
    let rec loop () =
      Body.Writer.write_string response_body chunk;
      Body.Writer.flush response_body (function
        | `Written -> loop ()
        | `Closed ->
          Option.iter (fun u -> Eio.Promise.resolve u ()) !stream_closed)
    in
    loop ()
  | `GET, "/raise" -> failwith "handler failed"
  | `GET, "/stream" ->
    Body.Reader.close body;
    let response_body =
      Request_descriptor.respond_with_streaming
        reqd
        (Response.create
           ~headers:(Headers.of_list [ "transfer-encoding", "chunked" ])
           `OK)
    in
    let piece =
      Ache.Bigstring.of_string
        ~off:0
        ~len:(String.length stream_piece)
        stream_piece
    in
    for _ = 1 to stream_pieces do
      Body.Writer.schedule_bigstring response_body piece
    done;
    Body.Writer.close response_body
  | _, target ->
    Body.Reader.close body;
    Request_descriptor.respond_with_string reqd (Response.create `OK) target

let error_handler (_ : Eio.Net.Sockaddr.stream) ?request:_ error start_response =
  server_errors := error :: !server_errors;
  Body.Writer.close (start_response Headers.empty)

let open_fds () = Array.length (Sys.readdir "/dev/fd")

module Eof_flow = struct
  type t =
    { flow : Eio.Flow.two_way_ty Eio.Resource.t
    ; eof : bool ref
    }

  let read_methods = []

  let single_read t buf =
    match Eio.Flow.single_read t.flow buf with
    | n -> n
    | exception End_of_file ->
      t.eof := true;
      raise End_of_file

  let single_write t bufs = Eio.Flow.single_write t.flow bufs
  let copy t ~src = Eio.Flow.Pi.simple_copy ~single_write t ~src
  let shutdown t command = Eio.Flow.shutdown t.flow command
  let close (_ : t) = ()

  let handler =
    Eio.Resource.handler
      [ H
          ( Eio.Flow.Pi.Source
          , (module struct
              type nonrec t = t

              let read_methods = read_methods
              let single_read = single_read
            end) )
      ; H
          ( Eio.Flow.Pi.Sink
          , (module struct
              type nonrec t = t

              let single_write = single_write
              let copy = copy
            end) )
      ; H
          ( Eio.Flow.Pi.Shutdown
          , (module struct
              type nonrec t = t

              let shutdown = shutdown
            end) )
      ; H (Eio.Resource.Close, close)
      ]

  let wrap flow =
    let eof = ref false in
    ( (Eio.Resource.T
         ({ flow :> Eio.Flow.two_way_ty Eio.Resource.t; eof }, handler)
       : [ Eio.Flow.two_way_ty | Eio.Resource.close_ty ] Eio.Resource.t)
    , eof )
end

type raw =
  { tls : Tls_eio.t
  ; tcp_eof : bool ref
  }

let raw_client ?(config = client_config) socket =
  let flow, tcp_eof = Eof_flow.wrap socket in
  { tls = Tls_eio.client_of_flow config ~host:localhost flow; tcp_eof }

let raw_server socket =
  let flow, tcp_eof = Eof_flow.wrap socket in
  { tls = Tls_eio.server_of_flow server_config flow; tcp_eof }

let raw_read_until_eof raw =
  let output =
    Eio.Buf_read.parse_exn Eio.Buf_read.take_all ~max_size:1_000_000 raw.tls
  in
  output, if !(raw.tcp_eof) then `Tcp_eof else `Close_notify

let raw_read_head raw =
  let reader = Eio.Buf_read.of_flow raw.tls ~max_size:65536 in
  let rec go () = if Eio.Buf_read.line reader = "" then () else go () in
  go ()

let run f =
  Eio_main.run (fun env ->
    Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 10.0 (fun () ->
      Eio.Switch.run (fun sw -> f env sw)))

let handler_errors : exn list ref = ref []

let with_listener env sw handler f =
  handler_errors := [];
  let listening =
    Eio.Net.listen
      ~sw
      ~backlog:16
      ~reuse_addr:true
      (Eio.Stdenv.net env)
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0))
  in
  let handlers_done = ref 0 in
  let connected = ref 0 in
  Eio.Fiber.fork_daemon ~sw (fun () ->
    let rec accept () =
      Eio.Net.accept_fork
        ~sw
        listening
        ~on_error:(fun exn -> handler_errors := exn :: !handler_errors)
        (fun socket addr ->
           Fun.protect
             ~finally:(fun () -> incr handlers_done)
             (fun () -> handler socket addr));
      accept ()
    in
    accept ());
  let connect sw =
    incr connected;
    Eio.Net.connect ~sw (Eio.Stdenv.net env) (Eio.Net.listening_addr listening)
  in
  f connect;
  let rec wait n =
    if !handlers_done < !connected && n > 0
    then (
      Eio.Time.sleep (Eio.Stdenv.clock env) 0.01;
      wait (n - 1))
  in
  wait 500;
  Alcotest.(check int) "connection handlers returned" !connected !handlers_done

let with_server
      ?(timeouts = false)
      ?idle_timeout
      ?head_timeout
      ?body_timeout
      ?write_timeout
      f
  =
  server_errors := [];
  partial_body_cut := false;
  let fds = open_fds () in
  run (fun env sw ->
    let clock = if timeouts then Some (Eio.Stdenv.clock env) else None in
    with_listener
      env
      sw
      (Ache_eio_tls.Server.create_connection_handler
         ~date:false
         ?clock
         ~idle_timeout
         ~head_timeout
         ~body_timeout
         ~write_timeout
         ~tls:server_config
         ~request_handler
         ~error_handler)
      (f env));
  Alcotest.(check int) "no file descriptor leaked" fds (open_fds ())

let check_no_handler_error () =
  Alcotest.(check (list string))
    "no exception from the connection handler"
    []
    (List.map Printexc.to_string !handler_errors)

let read_body body =
  let buf = Stdlib.Buffer.create 1024 in
  let p, u = Eio.Promise.create () in
  let rec on_read bs ~off ~len =
    Stdlib.Buffer.add_string buf (Ache.Bigstring.substring bs ~off ~len);
    Body.Reader.schedule_read body ~on_eof ~on_error ~on_read
  and on_eof () = Eio.Promise.resolve u (Ok (Stdlib.Buffer.contents buf))
  and on_error () =
    Eio.Promise.resolve u (Error (Stdlib.Buffer.contents buf))
  in
  Body.Reader.schedule_read body ~on_eof ~on_error ~on_read;
  p

let request_with send ?(errors = ref []) ?(body = "") request =
  let p, u = Eio.Promise.create () in
  let request_body =
    send
      request
      ~error_handler:(fun error -> errors := error :: !errors)
      ~response_handler:(fun response response_body ->
        Eio.Promise.resolve u (response, read_body response_body))
  in
  Body.Writer.write_string request_body body;
  Body.Writer.close request_body;
  let response, body = Eio.Promise.await p in
  response, Eio.Promise.await body

let request ?errors client ?body request =
  request_with
    (fun request ~error_handler ~response_handler ->
       Ache_eio_tls.Client.request
         client
         request
         ~error_handler
         ~response_handler)
    ?errors
    ?body
    request

let get target =
  Request.create ~headers:(Headers.of_list [ "host", "localhost" ]) `GET target

let tls_client ~sw connect =
  Ache_eio_tls.Client.create_connection
    ~sw
    ~tls:client_config
    ~peer:(`Host localhost)
    (connect sw)

let plain_tls_client ~sw connect =
  Ache_eio.Client.create_connection
    ~sw
    (Tls_eio.client_of_flow client_config ~host:localhost (connect sw))

(* A TLS client that decrypts the bytes from the server one record at a time, to
   count the records that carry response bytes. *)
module Records = struct
  type t =
    { mutable state : Tls.Engine.state
    ; mutable pending : string
    ; plain : Stdlib.Buffer.t
    ; mutable data_records : int
    ; mutable closed : bool
    }

  let create state =
    { state
    ; pending = ""
    ; plain = Stdlib.Buffer.create 4096
    ; data_records = 0
    ; closed = false
    }

  let rec take t replies =
    let available = String.length t.pending in
    let size =
      if available < 5
      then max_int
      else 5 + ((Char.code t.pending.[3] lsl 8) lor Char.code t.pending.[4])
    in
    if available < size
    then List.rev replies
    else
      let record = String.sub t.pending 0 size in
      t.pending <- String.sub t.pending size (available - size);
      match Tls.Engine.handle_tls t.state record with
      | Ok (state, eof, `Response reply, `Data data) ->
        t.state <- state;
        if Option.is_some eof then t.closed <- true;
        (match data with
        | Some data when data <> "" ->
          t.data_records <- t.data_records + 1;
          Stdlib.Buffer.add_string t.plain data
        | _ -> ());
        take t (Option.fold ~none:replies ~some:(fun r -> r :: replies) reply)
      | Error (failure, _) ->
        Alcotest.failf "TLS client: %s" (Tls.Engine.string_of_failure failure)

  (* The replies to send back. *)
  let feed t bytes =
    t.pending <- t.pending ^ bytes;
    take t []

  let send t s =
    match Tls.Engine.send_application_data t.state [ s ] with
    | Some (state, data) ->
      t.state <- state;
      data
    | None -> Alcotest.fail "TLS client not ready"
end

let stream_request =
  "GET /stream HTTP/1.1\r\nhost: localhost\r\nconnection: close\r\n\r\n"

let check_stream_records (records : Records.t) =
  let plain = Stdlib.Buffer.contents records.plain in
  let body =
    String.concat "" (List.init stream_pieces (fun _ -> stream_piece))
  in
  Alcotest.(check bool)
    "chunked body"
    true
    (String.ends_with
       ~suffix:
         (Printf.sprintf "%x\r\n%s\r\n0\r\n\r\n" (String.length body) body)
       plain);
  (* One record for each 16 KiB, and one more in case the head goes in a write
     of its own. Without coalescing it is one for each iovec. *)
  let limit = ((String.length plain + 0x3fff) / 0x4000) + 1 in
  if records.data_records > limit
  then
    Alcotest.failf
      "%d application-data records for %d bytes, expected at most %d"
      records.data_records
      (String.length plain)
      limit

let test_stream_records () =
  with_server (fun _ connect ->
    Eio.Switch.run (fun sw ->
      let socket = connect sw in
      let state, init =
        Tls.Engine.client (Tls.Config.peer client_config localhost)
      in
      let records = Records.create state in
      Eio.Flow.copy_string init socket;
      let buf = Cstruct.create 0x4000 in
      let rec pump ~reply until =
        if not (until ())
        then
          match Eio.Flow.single_read socket buf with
          | exception End_of_file -> ()
          | n ->
            let replies = Records.feed records (Cstruct.to_string buf ~len:n) in
            if reply
            then List.iter (fun s -> Eio.Flow.copy_string s socket) replies;
            pump ~reply until
      in
      pump ~reply:true (fun () ->
        not (Tls.Engine.handshake_in_progress records.state));
      Eio.Flow.copy_string (Records.send records stream_request) socket;
      pump ~reply:false (fun () -> records.closed);
      check_stream_records records));
  check_no_handler_error ()

let body_result = Alcotest.(result string string)

let test_get () =
  with_server (fun _ connect ->
    Eio.Switch.run (fun sw ->
      let client = tls_client ~sw connect in
      let response, body = request client (get "/hello") in
      Alcotest.(check int) "status" 200 (Status.to_code response.status);
      Alcotest.check body_result "body" (Ok "/hello") body;
      Eio.Promise.await (Ache_eio_tls.Client.shutdown client)));
  check_no_handler_error ()

let test_post_echo () =
  with_server (fun _ connect ->
    Eio.Switch.run (fun sw ->
      let client = tls_client ~sw connect in
      let payload = String.init 200_000 (fun i -> Char.chr (97 + (i mod 26))) in
      let headers =
        Headers.of_list
          [ "host", "localhost"
          ; "content-length", string_of_int (String.length payload)
          ]
      in
      let response, body =
        request client ~body:payload (Request.create ~headers `POST "/echo")
      in
      Alcotest.(check int) "status" 200 (Status.to_code response.status);
      Alcotest.check body_result "body" (Ok payload) body;
      Eio.Promise.await (Ache_eio_tls.Client.shutdown client)))

let test_client_pipelined () =
  with_server (fun _ connect ->
    Eio.Switch.run (fun sw ->
      let client = tls_client ~sw connect in
      let (_, a), (_, b) =
        Eio.Fiber.pair
          (fun () -> request client (get "/a"))
          (fun () -> request client (get "/b"))
      in
      Alcotest.check body_result "first" (Ok "/a") a;
      Alcotest.check body_result "second" (Ok "/b") b;
      Eio.Promise.await (Ache_eio_tls.Client.shutdown client)))

let test_pipelined_connection_close () =
  with_server (fun _ connect ->
    Eio.Switch.run (fun sw ->
      let raw = raw_client (connect sw) in
      Eio.Flow.copy_string
        "GET /a HTTP/1.1\r\n\
         host: localhost\r\n\
         \r\n\
         GET /b HTTP/1.1\r\n\
         host: localhost\r\n\
         connection: close\r\n\
         \r\n"
        raw.tls;
      let output, ending = raw_read_until_eof raw in
      Alcotest.(check string)
        "responses"
        "HTTP/1.1 200 OK\r\n\
         content-length: 2\r\n\
         \r\n\
         /aHTTP/1.1 200 OK\r\n\
         content-length: 2\r\n\
         connection: close\r\n\
         \r\n\
         /b"
        output;
      Alcotest.(check bool)
        "server sent close_notify"
        true
        (ending = `Close_notify);
      Eio.Flow.shutdown raw.tls `Send));
  check_no_handler_error ()

let test_client_sends_close_notify () =
  let server_saw = ref None in
  run (fun env sw ->
    with_listener
      env
      sw
      (fun socket _ ->
         let raw = raw_server socket in
         raw_read_head raw;
         Eio.Flow.copy_string
           "HTTP/1.1 200 OK\r\ncontent-length: 2\r\n\r\nok"
           raw.tls;
         server_saw := Some (raw_read_until_eof raw);
         Eio.Flow.shutdown raw.tls `Send)
      (fun connect ->
         Eio.Switch.run (fun sw ->
           let client = tls_client ~sw connect in
           let _, body = request client (get "/") in
           Alcotest.check body_result "body" (Ok "ok") body;
           Eio.Promise.await (Ache_eio_tls.Client.shutdown client))));
  Alcotest.(check bool)
    "client sent close_notify and nothing else"
    true
    (!server_saw = Some ("", `Close_notify))

let test_cut_request_body () =
  with_server (fun env connect ->
    Eio.Switch.run (fun sw ->
      let socket = connect sw in
      let raw = raw_client socket in
      Eio.Flow.copy_string
        "POST /partial HTTP/1.1\r\n\
         host: localhost\r\n\
         content-length: 1000\r\n\
         \r\n\
         0123456789"
        raw.tls;
      Eio.Time.sleep (Eio.Stdenv.clock env) 0.05;
      Eio.Flow.shutdown socket `Send));
  Alcotest.(check bool) "body reported cut" true !partial_body_cut;
  check_no_handler_error ()

let with_raw_server ~response ~close_notify client_side =
  run (fun env sw ->
    with_listener
      env
      sw
      (fun socket _ ->
         let raw = raw_server socket in
         raw_read_head raw;
         Eio.Flow.copy_string response raw.tls;
         if close_notify then Eio.Flow.shutdown raw.tls `Send;
         Eio.Flow.shutdown socket `All)
      client_side)

let response_then_close ~response ~close_notify () =
  let errors = ref [] in
  let result = ref (Error "no response") in
  let responded = ref false in
  with_raw_server ~response ~close_notify (fun connect ->
    Eio.Switch.run (fun sw ->
      let client = tls_client ~sw connect in
      let p, u = Eio.Promise.create () in
      let request_body =
        Ache_eio_tls.Client.request
          client
          (get "/")
          ~error_handler:(fun error ->
            errors := error :: !errors;
            if not !responded then Eio.Promise.resolve u ())
          ~response_handler:(fun _ body ->
            responded := true;
            Eio.Fiber.fork ~sw (fun () ->
              result := Eio.Promise.await (read_body body);
              Eio.Promise.resolve u ()))
      in
      Body.Writer.close request_body;
      Eio.Promise.await p;
      Eio.Promise.await (Ache_eio_tls.Client.shutdown client)));
  !result, !errors

let close_delimited = "HTTP/1.1 200 OK\r\nconnection: close\r\n\r\npartial"

let test_truncated_response () =
  let body, errors =
    response_then_close ~response:close_delimited ~close_notify:false ()
  in
  Alcotest.check body_result "body cut" (Error "partial") body;
  Alcotest.(check bool)
    "error handler got Truncated"
    true
    (match errors with [ `Exn Ache_eio_tls.Truncated ] -> true | _ -> false)

let test_close_notify_response () =
  let body, errors =
    response_then_close ~response:close_delimited ~close_notify:true ()
  in
  Alcotest.check body_result "body complete" (Ok "partial") body;
  Alcotest.(check int) "no error" 0 (List.length errors)

let test_fixed_length_without_close_notify () =
  let body, errors =
    response_then_close
      ~response:"HTTP/1.1 200 OK\r\ncontent-length: 8\r\n\r\ncomplete"
      ~close_notify:false
      ()
  in
  Alcotest.check body_result "body complete" (Ok "complete") body;
  Alcotest.(check int) "no error" 0 (List.length errors)

let test_chunked_without_close_notify () =
  let body, errors =
    response_then_close
      ~response:
        "HTTP/1.1 200 OK\r\n\
         transfer-encoding: chunked\r\n\
         \r\n\
         8\r\n\
         complete\r\n\
         0\r\n\
         \r\n"
      ~close_notify:false
      ()
  in
  Alcotest.check body_result "body complete" (Ok "complete") body;
  Alcotest.(check int) "no error" 0 (List.length errors)

(* tls-eio 2.1.5 raises End_of_file at the end of TCP with or without
   close_notify, so a client on a plain Tls_eio flow cannot see the
   truncation. *)
let test_plain_tls_eio_flow_misses_truncation () =
  let errors = ref [] in
  let result = ref (Error "no response") in
  with_raw_server ~response:close_delimited ~close_notify:false (fun connect ->
    Eio.Switch.run (fun sw ->
      let client = plain_tls_client ~sw connect in
      let _, body =
        request_with
          (fun request ~error_handler ~response_handler ->
             Ache_eio.Client.request
               client
               request
               ~error_handler
               ~response_handler)
          ~errors
          (get "/")
      in
      result := body;
      Eio.Promise.await (Ache_eio.Client.shutdown client)));
  Alcotest.check body_result "body reported complete" (Ok "partial") !result;
  Alcotest.(check int) "no error" 0 (List.length !errors)

let test_no_response_without_close_notify () =
  let body, errors = response_then_close ~response:"" ~close_notify:false () in
  Alcotest.check body_result "no response" (Error "no response") body;
  Alcotest.(check bool)
    "Connection_closed"
    true
    (match errors with [ `Connection_closed ] -> true | _ -> false)

let test_timeout_check_timing () =
  let raises f =
    match f () with () -> false | exception Invalid_argument _ -> true
  in
  Eio_main.run (fun env ->
    Eio.Switch.run (fun sw ->
      let flow, _peer = Eio_unix.Net.socketpair_stream ~sw () in
      let bound =
        Ache_eio_tls.Server.create_connection_handler
          ~date:false
          ~head_timeout:(Some (-1.))
          ~tls:server_config
          ~request_handler
          ~error_handler
      in
      Alcotest.(check bool)
        "bound: raises on a flow"
        true
        (raises (fun () -> bound flow (`Unix "socketpair")));
      Alcotest.(check bool)
        "every optional argument given: raises at once"
        true
        (raises (fun () ->
           ignore
             (Ache_eio_tls.Server.create_connection_handler
                ~date:false
                ~config:Ache.Config.default
                ~clock:(Eio.Stdenv.clock env)
                ~idle_timeout:(Some 1.)
                ~head_timeout:(Some (-1.))
                ~body_timeout:None
                ~write_timeout:None
                ~stop:(Eio.Promise.create_resolved ())
                ~grace:1.
                ~tls:server_config
                ~request_handler
                ~error_handler
              : _ -> _ -> unit)))))

let test_untrusted_certificate () =
  let other_ca = Ache_test_certs.generate () in
  with_server (fun _ connect ->
    Eio.Switch.run (fun sw ->
      match
        Ache_eio_tls.Client.create_connection
          ~sw
          ~tls:(Ache_test_certs.client_config other_ca)
          ~peer:(`Host localhost)
          (connect sw)
      with
      | _ -> Alcotest.fail "handshake succeeded"
      | exception Tls_eio.Tls_failure (`Error (`AuthenticationFailure _)) -> ()));
  Alcotest.(check int)
    "handshake error given to on_error"
    1
    (List.length !handler_errors)

let send_plain_request sw connect =
  let socket = connect sw in
  Eio.Flow.copy_string "GET / HTTP/1.1\r\nhost: localhost\r\n\r\n" socket;
  socket

let test_plain_text_client_closes () =
  with_server (fun _ connect ->
    Eio.Switch.run (fun sw ->
      let socket = send_plain_request sw connect in
      Eio.Flow.shutdown socket `Send))

let drain socket =
  try
    let (_ : string) =
      Eio.Buf_read.parse_exn Eio.Buf_read.take_all ~max_size:65536 socket
    in
    ()
  with
  | Eio.Io _ -> ()

let test_plain_text_client_waits () =
  with_server ~timeouts:true ~idle_timeout:0.2 (fun env connect ->
    Eio.Switch.run (fun sw ->
      let socket = send_plain_request sw connect in
      let since = Eio.Time.now (Eio.Stdenv.clock env) in
      drain socket;
      let elapsed = Eio.Time.now (Eio.Stdenv.clock env) -. since in
      Alcotest.(check bool) "closed by the handshake limit" true (elapsed < 2.0)));
  Alcotest.(check bool)
    "Timeout given to on_error"
    true
    (List.mem Eio.Time.Timeout !handler_errors)

let test_handshake_timeout () =
  with_server ~timeouts:true ~idle_timeout:0.2 (fun env connect ->
    Eio.Switch.run (fun sw ->
      let socket = connect sw in
      let since = Eio.Time.now (Eio.Stdenv.clock env) in
      drain socket;
      let elapsed = Eio.Time.now (Eio.Stdenv.clock env) -. since in
      Alcotest.(check bool) "closed by the handshake limit" true (elapsed < 2.0)))

let test_idle_timeout () =
  with_server ~timeouts:true ~idle_timeout:0.2 (fun _ connect ->
    Eio.Switch.run (fun sw ->
      let raw = raw_client (connect sw) in
      let output, ending = raw_read_until_eof raw in
      Alcotest.(check string) "no response" "" output;
      Alcotest.(check bool) "close_notify" true (ending = `Close_notify);
      Eio.Flow.shutdown raw.tls `Send));
  check_no_handler_error ()

let test_head_timeout () =
  with_server ~timeouts:true ~head_timeout:0.2 (fun _ connect ->
    Eio.Switch.run (fun sw ->
      let raw = raw_client (connect sw) in
      Eio.Flow.copy_string "GET /a HTTP/1.1\r\n" raw.tls;
      let output, ending = raw_read_until_eof raw in
      Alcotest.(check bool)
        "408"
        true
        (String.starts_with ~prefix:"HTTP/1.1 408 Request Timeout\r\n" output);
      Alcotest.(check bool) "close_notify" true (ending = `Close_notify);
      Eio.Flow.shutdown raw.tls `Send));
  Alcotest.(check int) "one error" 1 (List.length !server_errors);
  check_no_handler_error ()

let test_head_timeout_then_data () =
  with_server ~timeouts:true ~head_timeout:0.2 (fun _ connect ->
    Eio.Switch.run (fun sw ->
      let raw = raw_client (connect sw) in
      let reader = Eio.Buf_read.of_flow raw.tls ~max_size:65536 in
      Eio.Flow.copy_string "GET /a HTTP/1.1\r\n" raw.tls;
      Alcotest.(check string)
        "status line"
        "HTTP/1.1 408 Request Timeout"
        (Eio.Buf_read.line reader);
      Eio.Flow.copy_string "host: localhost\r\n\r\n" raw.tls;
      Eio.Flow.shutdown raw.tls `Send;
      let (_ : string) = Eio.Buf_read.take_all reader in
      Alcotest.(check bool) "close_notify" false !(raw.tcp_eof)));
  Alcotest.(check int) "one error" 1 (List.length !server_errors);
  check_no_handler_error ()

let respond_while_body_read_waits ~timeouts () =
  pending_body := None;
  pending_body_failed := false;
  with_server ~timeouts ~idle_timeout:0.2 ~body_timeout:0.2 (fun env connect ->
    Eio.Switch.run (fun sw ->
      let socket = connect sw in
      let raw = raw_client socket in
      Eio.Flow.copy_string
        "POST /respond-later HTTP/1.1\r\n\
         host: localhost\r\n\
         content-length: 100000\r\n\
         \r\n\
         abc"
        raw.tls;
      let rec respond () =
        match !pending_body with
        | Some reqd ->
          Eio.Time.sleep (Eio.Stdenv.clock env) 0.05;
          Request_descriptor.respond_with_string
            reqd
            (Response.create `OK)
            "late"
        | None ->
          Eio.Time.sleep (Eio.Stdenv.clock env) 0.001;
          respond ()
      in
      respond ();
      let output, ending = raw_read_until_eof raw in
      Alcotest.(check string)
        "response"
        "HTTP/1.1 200 OK\r\ncontent-length: 4\r\n\r\nlate"
        output;
      Alcotest.(check bool) "close_notify" true (ending = `Close_notify);
      Alcotest.(check bool)
        "server closes before the client sends close_notify"
        true
        (match
           Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 2.0 (fun () ->
             Eio.Flow.single_read socket (Cstruct.create 1))
         with
        | (_ : int) -> false
        | exception End_of_file -> true);
      Eio.Flow.shutdown raw.tls `Send));
  Alcotest.(check bool) "request body failed" true !pending_body_failed;
  check_no_handler_error ()

let test_handler_exception () =
  with_server (fun _ connect ->
    Eio.Switch.run (fun sw ->
      let client = tls_client ~sw connect in
      let response, _ = request client (get "/raise") in
      Alcotest.(check int) "status" 500 (Status.to_code response.status);
      Eio.Promise.await (Ache_eio_tls.Client.shutdown client)));
  check_no_handler_error ()

let test_reset_mid_head () =
  with_server (fun _ connect ->
    Eio.Switch.run (fun sw ->
      let socket = connect sw in
      let raw = raw_client socket in
      Eio.Flow.copy_string "GET /a HTTP/1.1\r\nHo" raw.tls;
      Eio.Net.setsockopt socket Eio.Net.Sockopt.SO_LINGER (Some 0);
      Eio.Flow.close socket));
  check_no_handler_error ()

let test_cancel_connection_fiber () =
  server_errors := [];
  run (fun env sw ->
    let cancel, resolve_cancel = Eio.Promise.create () in
    let returned = ref false in
    with_listener
      env
      sw
      (fun socket addr ->
         Eio.Fiber.first
           (fun () ->
              Ache_eio_tls.Server.create_connection_handler
                ~date:false
                ~tls:server_config
                ~request_handler
                ~error_handler
                socket
                addr)
           (fun () -> Eio.Promise.await cancel);
         returned := true)
      (fun connect ->
         Eio.Switch.run (fun sw ->
           let raw = raw_client (connect sw) in
           Eio.Flow.copy_string "GET /a HTTP/1.1\r\nHo" raw.tls;
           Eio.Time.sleep (Eio.Stdenv.clock env) 0.05;
           Eio.Promise.resolve resolve_cancel ();
           Eio.Time.sleep (Eio.Stdenv.clock env) 0.05;
           Alcotest.(check bool) "handler returned" true !returned)));
  Alcotest.(check int) "no error reported" 0 (List.length !server_errors);
  check_no_handler_error ()

let upgrade_headers =
  Headers.of_list
    [ "host", "localhost"
    ; "connection", "upgrade"
    ; "upgrade", "echo"
    ; "content-length", "0"
    ]

let test_upgrade () =
  with_server (fun _ connect ->
    Eio.Switch.run (fun sw ->
      let client = tls_client ~sw connect in
      let upgraded, resolve_upgraded = Eio.Promise.create () in
      let request_body =
        Ache_eio_tls.Client.request
          client
          (Request.create ~headers:upgrade_headers `GET "/upgrade")
          ~error_handler:(fun _ -> Alcotest.fail "request failed")
          ~response_handler:(fun response _body ->
            Eio.Promise.resolve resolve_upgraded response)
      in
      Body.Writer.close request_body;
      let response = Eio.Promise.await upgraded in
      Alcotest.(check int) "status" 101 (Status.to_code response.status);
      let message = "hello over echo" in
      let echoed, resolve_echoed = Eio.Promise.create () in
      let received = Stdlib.Buffer.create 16 in
      let echo_client =
        Echo.create ~pending:message (fun t s ->
          Stdlib.Buffer.add_string received s;
          if Stdlib.Buffer.length received >= String.length message
          then (
            Echo.shutdown t;
            Eio.Promise.resolve resolve_echoed (Stdlib.Buffer.contents received)))
      in
      Ache_eio_tls.Client.upgrade
        client
        (Runtime.make (module Echo) echo_client);
      Alcotest.(check string) "echoed" message (Eio.Promise.await echoed);
      Eio.Promise.await (Ache_eio_tls.Client.shutdown client)));
  check_no_handler_error ()

let test_upgrade_bytes_after_head () =
  with_server (fun _ connect ->
    Eio.Switch.run (fun sw ->
      let raw = raw_client (connect sw) in
      Eio.Flow.copy_string
        "GET /upgrade HTTP/1.1\r\n\
         host: localhost\r\n\
         connection: upgrade\r\n\
         upgrade: echo\r\n\
         \r\n\
         ping"
        raw.tls;
      let expected =
        "HTTP/1.1 101 Switching Protocols\r\n\
         connection: upgrade\r\n\
         upgrade: echo\r\n\
         \r\n\
         ping"
      in
      let reader = Eio.Buf_read.of_flow raw.tls ~max_size:65536 in
      Alcotest.(check string)
        "101 then echoed bytes"
        expected
        (Eio.Buf_read.take (String.length expected) reader);
      Eio.Flow.copy_string "pong" raw.tls;
      Alcotest.(check string)
        "later bytes echoed"
        "pong"
        (Eio.Buf_read.take 4 reader);
      Eio.Flow.shutdown raw.tls `Send))

let test_upgrade_with_read_pending () =
  with_server (fun env connect ->
    Eio.Switch.run (fun sw ->
      let clock = Eio.Stdenv.clock env in
      let raw = raw_client (connect sw) in
      Eio.Flow.copy_string
        "GET /upgrade-later HTTP/1.1\r\n\
         host: localhost\r\n\
         connection: upgrade\r\n\
         upgrade: echo\r\n\
         \r\n"
        raw.tls;
      Eio.Time.sleep clock 0.05;
      Eio.Flow.copy_string "ping" raw.tls;
      Eio.Time.sleep clock 0.05;
      Request_descriptor.respond_with_upgrade
        (Option.get !pending_upgrade)
        (Headers.of_list [ "connection", "upgrade"; "upgrade", "echo" ])
        (Runtime.make (module Echo) (Echo.create (fun t s -> Echo.send t s)));
      let expected =
        "HTTP/1.1 101 Switching Protocols\r\n\
         connection: upgrade\r\n\
         upgrade: echo\r\n\
         \r\n\
         ping"
      in
      let reader = Eio.Buf_read.of_flow raw.tls ~max_size:65536 in
      Alcotest.(check string)
        "101 then the bytes sent before it"
        expected
        (Eio.Buf_read.take (String.length expected) reader);
      Eio.Flow.shutdown raw.tls `Send));
  check_no_handler_error ()

let test_idle_timeout_then_request () =
  with_server ~timeouts:true ~idle_timeout:0.2 (fun env connect ->
    Eio.Switch.run (fun sw ->
      let raw = raw_client (connect sw) in
      Eio.Time.sleep (Eio.Stdenv.clock env) 0.3;
      Eio.Flow.copy_string "GET /a HTTP/1.1\r\nhost: localhost\r\n\r\n" raw.tls;
      let output, ending = raw_read_until_eof raw in
      Alcotest.(check string) "no response" "" output;
      Alcotest.(check bool) "close_notify" true (ending = `Close_notify);
      Eio.Flow.shutdown raw.tls `Send));
  Alcotest.(check int) "no error" 0 (List.length !server_errors);
  check_no_handler_error ()

let test_body_between_timed_reads () =
  with_server
    ~timeouts:true
    ~idle_timeout:0.3
    ~head_timeout:0.3
    (fun env connect ->
       Eio.Switch.run (fun sw ->
         let raw = raw_client (connect sw) in
         let reader = Eio.Buf_read.of_flow raw.tls ~max_size:65536 in
         Eio.Flow.copy_string
           "POST /echo HTTP/1.1\r\n\
            host: localhost\r\n\
            content-length: 6\r\n\
            \r\n\
            abc"
           raw.tls;
         Eio.Time.sleep (Eio.Stdenv.clock env) 0.05;
         Eio.Flow.copy_string "def" raw.tls;
         let echoed = "HTTP/1.1 200 OK\r\ncontent-length: 6\r\n\r\nabcdef" in
         Alcotest.(check string)
           "echo response"
           echoed
           (Eio.Buf_read.take (String.length echoed) reader);
         Eio.Flow.copy_string
           "GET /a HTTP/1.1\r\nhost: localhost\r\n\r\n"
           raw.tls;
         let rest = Eio.Buf_read.take_all reader in
         Alcotest.(check bool)
           "second response"
           true
           (String.starts_with ~prefix:"HTTP/1.1 200 OK\r\n" rest
           && String.ends_with ~suffix:"/a" rest);
         Alcotest.(check bool) "close_notify" false !(raw.tcp_eof);
         Eio.Flow.shutdown raw.tls `Send));
  Alcotest.(check int) "no error" 0 (List.length !server_errors);
  check_no_handler_error ()

let test_client_shutdown_with_pending_read () =
  let server_saw = ref None in
  let errors = ref [] in
  run (fun env sw ->
    with_listener
      env
      sw
      (fun socket _ ->
         let raw = raw_server socket in
         raw_read_head raw;
         Eio.Flow.copy_string "HTTP/1.1 200 OK\r\ncontent-le" raw.tls;
         server_saw := Some (raw_read_until_eof raw);
         Eio.Flow.shutdown raw.tls `Send)
      (fun connect ->
         Eio.Switch.run (fun sw ->
           let client = tls_client ~sw connect in
           let request_body =
             Ache_eio_tls.Client.request
               client
               (get "/")
               ~error_handler:(fun error -> errors := error :: !errors)
               ~response_handler:(fun _ _ ->
                 Alcotest.fail "unexpected response")
           in
           Body.Writer.close request_body;
           Eio.Time.sleep (Eio.Stdenv.clock env) 0.1;
           Eio.Promise.await (Ache_eio_tls.Client.shutdown client))));
  Alcotest.(check bool)
    "client sent close_notify"
    true
    (!server_saw = Some ("", `Close_notify));
  Alcotest.(check int) "one error" 1 (List.length !errors);
  check_no_handler_error ()

let handshake_with peer =
  let result = ref None in
  with_server (fun _ connect ->
    Eio.Switch.run (fun sw ->
      match
        Ache_eio_tls.Client.create_connection
          ~sw
          ~tls:client_config
          ~peer
          (connect sw)
      with
      | client ->
        result := Some (Ok ());
        Eio.Promise.await (Ache_eio_tls.Client.shutdown client)
      | exception exn -> result := Some (Error exn)));
  Option.get !result

let check_accepted name peer =
  match handshake_with peer with
  | Ok () -> ()
  | Error exn ->
    Alcotest.failf "%s: rejected with %s" name (Printexc.to_string exn)

let check_rejected name peer =
  match handshake_with peer with
  | Error (Tls_eio.Tls_failure (`Error (`AuthenticationFailure _))) -> ()
  | Error exn ->
    Alcotest.failf "%s: unexpected %s" name (Printexc.to_string exn)
  | Ok () -> Alcotest.failf "%s: handshake accepted" name

let test_peer_host () =
  check_accepted "localhost" (`Host localhost);
  check_rejected
    "example.com"
    (`Host Domain_name.(host_exn (of_string_exn "example.com")))

let test_peer_ip () =
  check_accepted "127.0.0.1" (`Ip (Ipaddr.of_string_exn "127.0.0.1"));
  check_rejected "127.0.0.2" (`Ip (Ipaddr.of_string_exn "127.0.0.2"))

let test_peer_insecure () = check_accepted "no name" `Insecure_no_name_check

let stalled_handshake ?idle_timeout ?head_timeout () =
  let elapsed = ref 0. in
  with_server ~timeouts:true ?idle_timeout ?head_timeout (fun env connect ->
    Eio.Switch.run (fun sw ->
      let socket = connect sw in
      let since = Eio.Time.now (Eio.Stdenv.clock env) in
      drain socket;
      elapsed := Eio.Time.now (Eio.Stdenv.clock env) -. since));
  !elapsed

let check_bounded name elapsed =
  if elapsed > 1.0 then Alcotest.failf "%s: closed after %.3f s" name elapsed

let test_handshake_head_timeout () =
  check_bounded "head_timeout" (stalled_handshake ~head_timeout:0.2 ())

let test_handshake_smaller_timeout () =
  check_bounded
    "head_timeout smaller"
    (stalled_handshake ~idle_timeout:5.0 ~head_timeout:0.2 ());
  check_bounded
    "idle_timeout smaller"
    (stalled_handshake ~idle_timeout:0.2 ~head_timeout:5.0 ())

let test_handshake_no_timeout () =
  with_server (fun env connect ->
    Eio.Switch.run (fun sw ->
      let socket = connect sw in
      let state =
        Eio.Fiber.first
          (fun () ->
             drain socket;
             `Closed)
          (fun () ->
             Eio.Time.sleep (Eio.Stdenv.clock env) 0.5;
             `Open)
      in
      Alcotest.(check bool) "still open after 0.5 s" true (state = `Open);
      Eio.Flow.shutdown socket `Send))

let ws_frame opcode payload =
  let mask = "\x01\x02\x03\x04" in
  let len = String.length payload in
  assert (len < 126);
  String.make 1 (Char.chr (0x80 lor opcode))
  ^ String.make 1 (Char.chr (0x80 lor len))
  ^ mask
  ^ String.mapi
      (fun i c -> Char.chr (Char.code c lxor Char.code mask.[i land 3]))
      payload

let test_websocket_echo () =
  with_server (fun _ connect ->
    Eio.Switch.run (fun sw ->
      let raw = raw_client (connect sw) in
      Eio.Flow.copy_string
        "GET /ws-echo HTTP/1.1\r\n\
         host: localhost\r\n\
         upgrade: websocket\r\n\
         connection: Upgrade\r\n\
         sec-websocket-key: dGhlIHNhbXBsZSBub25jZQ==\r\n\
         sec-websocket-version: 13\r\n\
         \r\n"
        raw.tls;
      let reader = Eio.Buf_read.of_flow raw.tls ~max_size:65536 in
      let rec skip_head () =
        if Eio.Buf_read.line reader <> "" then skip_head ()
      in
      Alcotest.(check string)
        "101"
        "HTTP/1.1 101 Switching Protocols"
        (Eio.Buf_read.line reader);
      skip_head ();
      Eio.Flow.copy_string (ws_frame 0x1 "hello") raw.tls;
      Alcotest.(check string)
        "echo"
        "\x81\x05hello"
        (Eio.Buf_read.take 7 reader);
      Eio.Flow.copy_string (ws_frame 0x8 "\x03\xe8") raw.tls;
      Alcotest.(check string)
        "close echoed"
        "\x88\x02\x03\xe8"
        (Eio.Buf_read.take 4 reader);
      Eio.Flow.close raw.tls));
  check_no_handler_error ()

let with_stop_server f =
  server_errors := [];
  pending_body := None;
  let fds = open_fds () in
  run (fun env sw ->
    let clock = Eio.Stdenv.clock env in
    let stop, resolve_stop = Eio.Promise.create () in
    with_listener
      env
      sw
      (Ache_eio_tls.Server.create_connection_handler
         ~date:false
         ~clock
         ~stop
         ~grace:1.0
         ~tls:server_config
         ~request_handler
         ~error_handler)
      (f clock (fun () ->
         Eio.Promise.resolve resolve_stop ();
         for _ = 1 to 3 do
           Eio.Fiber.yield ()
         done)));
  Alcotest.(check int) "no file descriptor leaked" fds (open_fds ())

let test_stop_drains () =
  with_stop_server (fun clock stop connect ->
    Eio.Switch.run (fun sw ->
      let raw = raw_client (connect sw) in
      Eio.Flow.copy_string
        "POST /respond-later HTTP/1.1\r\n\
         host: localhost\r\n\
         content-length: 0\r\n\
         \r\n"
        raw.tls;
      let rec reqd () =
        match !pending_body with
        | Some reqd -> reqd
        | None ->
          Eio.Time.sleep clock 0.001;
          reqd ()
      in
      let reqd = reqd () in
      stop ();
      Request_descriptor.respond_with_string
        reqd
        (Response.create
           ~headers:(Headers.of_list [ "content-length", "4" ])
           `OK)
        "done";
      let output, ending = raw_read_until_eof raw in
      Alcotest.(check string)
        "response"
        "HTTP/1.1 200 OK\r\ncontent-length: 4\r\nconnection: close\r\n\r\ndone"
        output;
      Alcotest.(check bool) "close_notify" true (ending = `Close_notify)));
  check_no_handler_error ()

let test_stop_during_handshake () =
  with_stop_server (fun clock stop connect ->
    Eio.Switch.run (fun sw ->
      let tcp = connect sw in
      Eio.Time.sleep clock 0.02;
      stop ();
      let output =
        Eio.Time.with_timeout_exn clock 0.5 (fun () ->
          Eio.Buf_read.parse_exn Eio.Buf_read.take_all ~max_size:65536 tcp)
      in
      Alcotest.(check string) "nothing sent" "" output));
  check_no_handler_error ()

let test_write_timeout () =
  let closed, resolve_closed = Eio.Promise.create () in
  stream_closed := Some resolve_closed;
  with_server ~write_timeout:0.3 (fun env connect ->
    Eio.Switch.run (fun sw ->
      let clock = Eio.Stdenv.clock env in
      let raw = raw_client (connect sw) in
      let start = Eio.Time.now clock in
      Eio.Flow.copy_string
        "GET /stream-forever HTTP/1.1\r\nhost: localhost\r\n\r\n"
        raw.tls;
      Eio.Promise.await closed;
      let elapsed = Eio.Time.now clock -. start in
      if elapsed > 1.5 then Alcotest.failf "closed after %.3f s" elapsed));
  stream_closed := None;
  check_no_handler_error ()

let () =
  (* On OCaml 5.5, the first Eio_main.run of a process opens a pipe pair that
     stays open: run it once so the descriptor counts of with_server do not
     include it. *)
  Eio_main.run ignore;
  Alcotest.run
    "ache-eio-tls"
    [ ( "tls"
      , [ Alcotest.test_case "GET" `Quick test_get
        ; Alcotest.test_case
            "a streamed response: one TLS record for each 16 KiB"
            `Quick
            test_stream_records
        ; Alcotest.test_case "POST echo of 200 kB" `Quick test_post_echo
        ; Alcotest.test_case "client pipelines" `Quick test_client_pipelined
        ; Alcotest.test_case
            "pipelined requests and connection: close"
            `Quick
            test_pipelined_connection_close
        ; Alcotest.test_case
            "client sends close_notify"
            `Quick
            test_client_sends_close_notify
        ; Alcotest.test_case
            "request body cut without close_notify"
            `Quick
            test_cut_request_body
        ; Alcotest.test_case
            "close-delimited response cut without close_notify"
            `Quick
            test_truncated_response
        ; Alcotest.test_case
            "close-delimited response ended by close_notify"
            `Quick
            test_close_notify_response
        ; Alcotest.test_case
            "Content-Length response, no close_notify"
            `Quick
            test_fixed_length_without_close_notify
        ; Alcotest.test_case
            "chunked response, no close_notify"
            `Quick
            test_chunked_without_close_notify
        ; Alcotest.test_case
            "no response and no close_notify is Connection_closed"
            `Quick
            test_no_response_without_close_notify
        ; Alcotest.test_case
            "a plain Tls_eio flow misses the truncation"
            `Quick
            test_plain_tls_eio_flow_misses_truncation
        ; Alcotest.test_case
            "when the timeout arguments are checked"
            `Quick
            test_timeout_check_timing
        ; Alcotest.test_case
            "untrusted certificate"
            `Quick
            test_untrusted_certificate
        ; Alcotest.test_case
            "plain-text client that closes"
            `Quick
            test_plain_text_client_closes
        ; Alcotest.test_case
            "plain-text client that waits"
            `Quick
            test_plain_text_client_waits
        ; Alcotest.test_case
            "handshake bounded by idle_timeout"
            `Quick
            test_handshake_timeout
        ; Alcotest.test_case
            "handshake bounded by head_timeout"
            `Quick
            test_handshake_head_timeout
        ; Alcotest.test_case
            "handshake bounded by the smaller timeout"
            `Quick
            test_handshake_smaller_timeout
        ; Alcotest.test_case
            "handshake without a timeout waits for the client"
            `Quick
            test_handshake_no_timeout
        ; Alcotest.test_case "peer `Host" `Quick test_peer_host
        ; Alcotest.test_case "peer `Ip" `Quick test_peer_ip
        ; Alcotest.test_case
            "peer `Insecure_no_name_check"
            `Quick
            test_peer_insecure
        ; Alcotest.test_case "idle timeout" `Quick test_idle_timeout
        ; Alcotest.test_case "head timeout" `Quick test_head_timeout
        ; Alcotest.test_case
            "input after a head timeout"
            `Quick
            test_head_timeout_then_data
        ; Alcotest.test_case "handler exception" `Quick test_handler_exception
        ; Alcotest.test_case "peer reset mid-head" `Quick test_reset_mid_head
        ; Alcotest.test_case
            "cancelling the connection fiber"
            `Quick
            test_cancel_connection_fiber
        ; Alcotest.test_case "client upgrade" `Quick test_upgrade
        ; Alcotest.test_case
            "Ache_eio.Websocket echo over TLS"
            `Quick
            test_websocket_echo
        ; Alcotest.test_case
            "server upgrade with bytes after the head"
            `Quick
            test_upgrade_bytes_after_head
        ; Alcotest.test_case
            "server upgrade with bytes sent before the 101"
            `Quick
            test_upgrade_with_read_pending
        ; Alcotest.test_case
            "request after the idle timeout"
            `Quick
            test_idle_timeout_then_request
        ; Alcotest.test_case
            "response while a body read waits: body_timeout, system clock"
            `Quick
            (respond_while_body_read_waits ~timeouts:false)
        ; Alcotest.test_case
            "response while a body read waits: body_timeout, clock"
            `Quick
            (respond_while_body_read_waits ~timeouts:true)
        ; Alcotest.test_case
            "a body read between timed reads, then the idle timeout"
            `Quick
            test_body_between_timed_reads
        ; Alcotest.test_case
            "Client.shutdown with a read pending"
            `Quick
            test_client_shutdown_with_pending_read
        ] )
    ; ( "progress"
      , [ Alcotest.test_case
            "a client that stops reading is closed after write_timeout"
            `Quick
            test_write_timeout
        ] )
    ; ( "stop"
      , [ Alcotest.test_case
            "a request in flight finishes with connection: close"
            `Quick
            test_stop_drains
        ; Alcotest.test_case
            "a handshake in progress is closed"
            `Quick
            test_stop_during_handshake
        ] )
    ]
