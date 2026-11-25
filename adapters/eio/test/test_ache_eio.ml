open Ache

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

let echo_server () =
  Runtime.make (module Echo) (Echo.create (fun t s -> Echo.send t s))

let server_errors : Server_connection.error list ref = ref []
let partial_body_cut = ref false
let stream_flush_closed : unit Eio.Promise.u option ref = ref None
let requests_seen = ref 0
let on_ws_closed : (int option -> unit) ref = ref ignore
let pending_upgrade : Request_descriptor.t option ref = ref None
let pending_response : Request_descriptor.t option ref = ref None
let pending_body_failed = ref false
let pending_body_bytes = Stdlib.Buffer.create 16
let later_reqd : Request_descriptor.t option ref = ref None
let big_size = 16 * 1024 * 1024

exception Respond_later

let late_error_response : (unit -> unit) option ref = ref None

let request_handler (_ : Eio.Net.Sockaddr.stream) reqd =
  incr requests_seen;
  let request = Request_descriptor.request reqd in
  let body = Request_descriptor.request_body reqd in
  match request.meth, request.target with
  | `GET, "/hello" ->
    Body.Reader.close body;
    Request_descriptor.respond_with_string
      reqd
      (Response.create
         ~headers:(Headers.of_list [ "content-length", "11" ])
         `OK)
      "hello world"
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
    and on_error () = failwith "request body cut short" in
    Body.Reader.schedule_read body ~on_eof ~on_error ~on_read
  | `GET, "/upgrade" ->
    Body.Reader.close body;
    Request_descriptor.respond_with_upgrade
      reqd
      (Headers.of_list [ "connection", "upgrade"; "upgrade", "echo" ])
      (echo_server ())
  | `GET, "/ws-flood" ->
    (match
       Websocket.upgrade reqd (fun ws ->
         let chunk = String.make 4096 'x' in
         let rec loop () =
           Websocket.send_text ws chunk;
           Websocket.flush ws (function `Written -> loop () | `Closed -> ())
         in
         loop ();
         { on_message = ignore; on_close = (fun code -> !on_ws_closed code) })
     with
    | Ok () -> ()
    | Error _ -> Alcotest.fail "handshake rejected")
  | `GET, "/stream" ->
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
          Option.iter (fun u -> Eio.Promise.resolve u ()) !stream_flush_closed)
    in
    loop ()
  | `GET, "/upgrade-later" -> pending_upgrade := Some reqd
  | `CONNECT, _ ->
    Request_descriptor.respond_with_upgrade reqd Headers.empty (echo_server ())
  | `POST, "/early-close" ->
    Request_descriptor.respond_with_string
      reqd
      (Response.create ~headers:(Headers.of_list [ "connection", "close" ]) `OK)
      "no"
  | `POST, "/early-413" ->
    Request_descriptor.respond_with_string
      reqd
      (Response.create `Content_too_large)
      "no"
  | `POST, "/respond-later" ->
    let rec on_read buffer ~off ~len =
      Stdlib.Buffer.add_string
        pending_body_bytes
        (Ache.Bigstring.substring buffer ~off ~len);
      Body.Reader.schedule_read body ~on_eof ~on_error ~on_read
    and on_eof () = Alcotest.fail "a cut body completed"
    and on_error () = pending_body_failed := true in
    Body.Reader.schedule_read body ~on_eof ~on_error ~on_read;
    pending_response := Some reqd
  | `POST, "/respond-later-raise" ->
    let rec on_read _ ~off:_ ~len:_ =
      Body.Reader.schedule_read body ~on_eof ~on_error ~on_read
    and on_eof () = Alcotest.fail "a cut body completed"
    and on_error () = failwith "body on_error" in
    Body.Reader.schedule_read body ~on_eof ~on_error ~on_read;
    pending_response := Some reqd
  | `POST, "/respond-later-then-body" ->
    let rec on_read _ ~off:_ ~len:_ =
      Body.Reader.schedule_read body ~on_eof ~on_error ~on_read
    and on_eof () = ()
    and on_error () = pending_body_failed := true in
    Body.Reader.schedule_read body ~on_eof ~on_error ~on_read;
    pending_response := Some reqd
  | `GET, "/later" ->
    Body.Reader.close body;
    later_reqd := Some reqd
  | `GET, "/big" ->
    Body.Reader.close body;
    let response_body =
      Request_descriptor.respond_with_streaming
        reqd
        (Response.create
           ~headers:
             (Headers.of_list [ "content-length", string_of_int big_size ])
           `OK)
    in
    let chunk = String.make (1024 * 1024) 'x' in
    let rec loop n =
      if n = 0
      then Body.Writer.close response_body
      else (
        Body.Writer.write_string response_body chunk;
        Body.Writer.flush response_body (function
          | `Written -> loop (n - 1)
          | `Closed -> ()))
    in
    loop (big_size / String.length chunk)
  | `GET, "/raise" -> failwith "handler failed"
  | `GET, "/raise-later" -> raise Respond_later
  | `POST, "/partial" ->
    let rec on_read _ ~off:_ ~len:_ =
      Body.Reader.schedule_read body ~on_eof ~on_error ~on_read
    and on_eof () = Alcotest.fail "a cut body completed"
    and on_error () = partial_body_cut := true in
    Body.Reader.schedule_read body ~on_eof ~on_error ~on_read
  | _, target ->
    Body.Reader.close body;
    Request_descriptor.respond_with_string
      reqd
      (Response.create
         ~headers:
           (Headers.of_list
              [ "content-length", string_of_int (String.length target) ])
         `OK)
      target

let error_handler (_ : Eio.Net.Sockaddr.stream) ?request:_ error start_response =
  server_errors := error :: !server_errors;
  match error with
  | `Exn Respond_later ->
    late_error_response :=
      Some (fun () -> Body.Writer.close (start_response Headers.empty))
  | _ -> Body.Writer.close (start_response Headers.empty)

let connections_done : unit Eio.Stream.t = Eio.Stream.create max_int
let real_sleep : (float -> unit) ref = ref (fun (_ : float) -> ())

let with_server
      ?(flow = fun socket -> (socket :> Eio.Flow.two_way_ty Eio.Resource.t))
      ?clock
      ?idle_timeout
      ?head_timeout
      f
  =
  server_errors := [];
  requests_seen := 0;
  Eio_main.run (fun env ->
    while Option.is_some (Eio.Stream.take_nonblocking connections_done) do
      ()
    done;
    real_sleep := Eio.Time.sleep (Eio.Stdenv.clock env);
    Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 10.0 (fun () ->
      Eio.Switch.run (fun sw ->
        let net = Eio.Stdenv.net env in
        let listening =
          Eio.Net.listen
            ~sw
            ~backlog:16
            ~reuse_addr:true
            net
            (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0))
        in
        Eio.Fiber.fork_daemon ~sw (fun () ->
          let rec accept () =
            Eio.Net.accept_fork
              ~sw
              listening
              ~on_error:raise
              (fun socket addr ->
                 Ache_eio.Server.create_connection_handler
                   ?clock
                   ?idle_timeout
                   ?head_timeout
                   ~request_handler
                   ~error_handler
                   (flow socket)
                   addr;
                 Eio.Stream.add connections_done ());
            accept ()
          in
          accept ());
        f ~sw ~net (Eio.Net.listening_addr listening))))

let read_body body =
  let buf = Stdlib.Buffer.create 1024 in
  let p, u = Eio.Promise.create () in
  let rec on_read bs ~off ~len =
    Stdlib.Buffer.add_string buf (Ache.Bigstring.substring bs ~off ~len);
    Body.Reader.schedule_read body ~on_eof ~on_error ~on_read
  and on_eof () = Eio.Promise.resolve u (Stdlib.Buffer.contents buf)
  and on_error () = Eio.Promise.resolve u "<response body cut short>" in
  Body.Reader.schedule_read body ~on_eof ~on_error ~on_read;
  p

let client_error_handler error =
  match error with
  | `Malformed_response msg -> Alcotest.failf "malformed response: %s" msg
  | `Invalid_response_body_length _ -> Alcotest.fail "invalid body length"
  | `Connection_closed -> Alcotest.fail "connection closed"
  | `Exn exn -> raise exn

let request client ?(body = "") request =
  let p, u = Eio.Promise.create () in
  let request_body =
    Ache_eio.Client.request
      client
      request
      ~error_handler:client_error_handler
      ~response_handler:(fun response response_body ->
        Eio.Promise.resolve u (response, read_body response_body))
  in
  Body.Writer.write_string request_body body;
  Body.Writer.close request_body;
  let response, body = Eio.Promise.await p in
  response, Eio.Promise.await body

let test_get () =
  with_server (fun ~sw ~net addr ->
    let client =
      Ache_eio.Client.create_connection ~sw (Eio.Net.connect ~sw net addr)
    in
    let response, body =
      request
        client
        (Request.create
           ~headers:(Headers.of_list [ "host", "localhost" ])
           `GET
           "/hello")
    in
    Alcotest.(check int) "status" 200 (Status.to_code response.status);
    Alcotest.(check string) "body" "hello world" body;
    Eio.Promise.await (Ache_eio.Client.shutdown client))

let test_post_echo () =
  with_server (fun ~sw ~net addr ->
    let client =
      Ache_eio.Client.create_connection ~sw (Eio.Net.connect ~sw net addr)
    in
    let payload =
      String.init
        ((10 * Config.default.read_buffer_size) + 7)
        (fun i -> Char.chr (97 + (i mod 26)))
    in
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
    Alcotest.(check int) "length" (String.length payload) (String.length body);
    Alcotest.(check bool) "body" true (String.equal payload body);
    Eio.Promise.await (Ache_eio.Client.shutdown client))

let test_pipelined () =
  with_server (fun ~sw ~net addr ->
    let socket = Eio.Net.connect ~sw net addr in
    Eio.Flow.copy_string
      "GET /a HTTP/1.1\r\n\
       host: localhost\r\n\
       \r\n\
       GET /b HTTP/1.1\r\n\
       host: localhost\r\n\
       connection: close\r\n\
       \r\n"
      socket;
    let output =
      Eio.Buf_read.parse_exn Eio.Buf_read.take_all ~max_size:65536 socket
    in
    Eio.Flow.close socket;
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
      output)

let test_many_requests_bounded_stack () =
  let requests = 20_000 in
  let response = "HTTP/1.1 200 OK\r\ncontent-length: 2\r\n\r\n/a" in
  let old = Gc.get () in
  Fun.protect
    ~finally:(fun () -> Gc.set old)
    (fun () ->
       Gc.set { old with stack_limit = 16_384 };
       with_server (fun ~sw ~net addr ->
         let socket = Eio.Net.connect ~sw net addr in
         let reader = Eio.Buf_read.of_flow socket ~max_size:65536 in
         let received = ref 0 in
         (try
            for _ = 1 to requests do
              Eio.Flow.copy_string
                "GET /a HTTP/1.1\r\nhost: localhost\r\n\r\n"
                socket;
              if Eio.Buf_read.take (String.length response) reader = response
              then incr received
            done
          with
         | End_of_file -> ());
         Eio.Flow.close socket;
         Eio.Stream.take connections_done;
         Alcotest.(check int) "responses" requests !received;
         Alcotest.(check int) "server errors" 0 (List.length !server_errors)))

let test_connect_tunnel () =
  with_server (fun ~sw ~net addr ->
    let client =
      Ache_eio.Client.create_connection ~sw (Eio.Net.connect ~sw net addr)
    in
    let answered, resolve_answered = Eio.Promise.create () in
    let request_body =
      Ache_eio.Client.request
        client
        (Request.create
           ~headers:(Headers.of_list [ "host", "localhost:443" ])
           `CONNECT
           "localhost:443")
        ~error_handler:client_error_handler
        ~response_handler:(fun response _body ->
          Eio.Promise.resolve resolve_answered response)
    in
    Body.Writer.close request_body;
    let response = Eio.Promise.await answered in
    Alcotest.(check int) "status" 200 (Status.to_code response.status);
    let message = "hello through the tunnel" in
    let echoed, resolve_echoed = Eio.Promise.create () in
    let received = Stdlib.Buffer.create 16 in
    let tunnel_client =
      Echo.create ~pending:message (fun t s ->
        Stdlib.Buffer.add_string received s;
        if Stdlib.Buffer.length received >= String.length message
        then (
          Echo.shutdown t;
          Eio.Promise.resolve resolve_echoed (Stdlib.Buffer.contents received)))
    in
    Ache_eio.Client.upgrade client (Runtime.make (module Echo) tunnel_client);
    Alcotest.(check string) "echoed" message (Eio.Promise.await echoed);
    Eio.Promise.await (Ache_eio.Client.shutdown client))

let test_connect_tunnel_bytes_after_head () =
  with_server (fun ~sw ~net addr ->
    let socket = Eio.Net.connect ~sw net addr in
    let reader = Eio.Buf_read.of_flow socket ~max_size:65536 in
    Eio.Flow.copy_string
      "CONNECT localhost:443 HTTP/1.1\r\nhost: localhost:443\r\n\r\nping"
      socket;
    let expected = "HTTP/1.1 200 OK\r\n\r\nping" in
    Alcotest.(check string)
      "200, then the bytes sent with the head come back"
      expected
      (Eio.Buf_read.take (String.length expected) reader);
    Eio.Flow.copy_string "pong" socket;
    Alcotest.(check string) "later bytes" "pong" (Eio.Buf_read.take 4 reader);
    Eio.Flow.close socket;
    Eio.Stream.take connections_done)

let test_upgrade () =
  with_server (fun ~sw ~net addr ->
    let client =
      Ache_eio.Client.create_connection ~sw (Eio.Net.connect ~sw net addr)
    in
    let upgraded, resolve_upgraded = Eio.Promise.create () in
    let request_body =
      Ache_eio.Client.request
        client
        (Request.create
           ~headers:
             (Headers.of_list
                [ "host", "localhost"
                ; "connection", "upgrade"
                ; "upgrade", "echo"
                ; "content-length", "0"
                ])
           `GET
           "/upgrade")
        ~error_handler:client_error_handler
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
    Ache_eio.Client.upgrade client (Runtime.make (module Echo) echo_client);
    Alcotest.(check string) "echoed" message (Eio.Promise.await echoed);
    Eio.Promise.await (Ache_eio.Client.shutdown client))

let test_upgrade_incomplete_token_reaches_new_protocol () =
  with_server (fun ~sw ~net addr ->
    let socket = Eio.Net.connect ~sw net addr in
    Eio.Flow.copy_string
      "GET /upgrade HTTP/1.1\r\n\
       host: localhost\r\n\
       connection: upgrade\r\n\
       upgrade: echo\r\n\
       \r\n\
       ping"
      socket;
    let expected =
      "HTTP/1.1 101 Switching Protocols\r\n\
       connection: upgrade\r\n\
       upgrade: echo\r\n\
       \r\n\
       ping"
    in
    let reader = Eio.Buf_read.of_flow socket ~max_size:4096 in
    Alcotest.(check string)
      "101 then echoed bytes"
      expected
      (Eio.Buf_read.take (String.length expected) reader);
    Eio.Flow.close socket)

module Failing_flow = struct
  type t =
    { inner : Eio.Flow.two_way_ty Eio.Resource.t
    ; fail_read : bool
    ; fail_write : bool
    }

  let read_methods = []

  let single_read t buf =
    if t.fail_read
    then failwith "read failed"
    else Eio.Flow.single_read t.inner buf

  let single_write t bufs =
    if t.fail_write
    then failwith "write failed"
    else Eio.Flow.single_write t.inner bufs

  let copy t ~src = Eio.Flow.Pi.simple_copy ~single_write t ~src
  let shutdowns : Eio.Flow.shutdown_command list ref = ref []
  let failing_shutdowns : Eio.Flow.shutdown_command list ref = ref []

  let shutdown t cmd =
    shutdowns := cmd :: !shutdowns;
    if List.mem cmd !failing_shutdowns
    then failwith "shutdown failed"
    else Eio.Flow.shutdown t.inner cmd

  let wrap ~fail_read ~fail_write socket =
    Eio.Resource.T
      ( { inner = (socket :> Eio.Flow.two_way_ty Eio.Resource.t)
        ; fail_read
        ; fail_write
        }
      , Eio.Flow.Pi.two_way
          (module struct
            type nonrec t = t

            let read_methods = read_methods
            let single_read = single_read
            let single_write = single_write
            let copy = copy
            let shutdown = shutdown
          end) )
end

let check_shutdowns () =
  Alcotest.(check (list string))
    "each direction shut down once"
    [ "receive"; "send" ]
    (List.sort
       compare
       (List.concat_map
          (function
            | `Receive -> [ "receive" ]
            | `Send -> [ "send" ]
            | `All -> [ "receive"; "send" ])
          !Failing_flow.shutdowns))

let get_until_closed ~sw ~net addr =
  let socket = Eio.Net.connect ~sw net addr in
  Eio.Flow.copy_string "GET /hello HTTP/1.1\r\nhost: localhost\r\n\r\n" socket;
  (match
     Eio.Buf_read.parse_exn Eio.Buf_read.take_all ~max_size:65536 socket
   with
  | (_ : string) -> ()
  | exception Eio.Io (Eio.Net.E (Connection_reset _), _) -> ());
  Eio.Flow.close socket

let test_read_failure () =
  Failing_flow.shutdowns := [];
  with_server
    ~flow:(Failing_flow.wrap ~fail_read:true ~fail_write:false)
    (fun ~sw ~net addr ->
       get_until_closed ~sw ~net addr;
       Eio.Stream.take connections_done);
  check_shutdowns ()

let test_write_failure () =
  Failing_flow.shutdowns := [];
  with_server
    ~flow:(Failing_flow.wrap ~fail_read:false ~fail_write:true)
    (fun ~sw ~net addr ->
       get_until_closed ~sw ~net addr;
       Eio.Stream.take connections_done);
  check_shutdowns ()

let test_eof_mid_body () =
  partial_body_cut := false;
  with_server (fun ~sw ~net addr ->
    let socket = Eio.Net.connect ~sw net addr in
    Eio.Flow.copy_string
      "POST /partial HTTP/1.1\r\n\
       host: localhost\r\n\
       content-length: 100\r\n\
       \r\n\
       only ten b"
      socket;
    Eio.Flow.shutdown socket `Send;
    let (_ : string) =
      Eio.Buf_read.parse_exn Eio.Buf_read.take_all ~max_size:65536 socket
    in
    Eio.Flow.close socket);
  Alcotest.(check bool) "request body cut" true !partial_body_cut;
  match !server_errors with
  | [ `Bad_request ] -> ()
  | _ ->
    Alcotest.failf
      "expected one Bad_request, got %d errors"
      (List.length !server_errors)

let ws_flood_request =
  "GET /ws-flood HTTP/1.1\r\n\
   host: localhost\r\n\
   upgrade: websocket\r\n\
   connection: Upgrade\r\n\
   sec-websocket-key: dGhlIHNhbXBsZSBub25jZQ==\r\n\
   sec-websocket-version: 13\r\n\
   \r\n"

let ws_101 =
  "HTTP/1.1 101 Switching Protocols\r\n\
   upgrade: websocket\r\n\
   connection: Upgrade\r\n\
   sec-websocket-accept: s3pPLMBiTxaQ9kYGzzhZRbK+xOo=\r\n\
   \r\n"

let test_websocket_peer_reset () =
  let closed, resolve_closed = Eio.Promise.create () in
  on_ws_closed := Eio.Promise.resolve resolve_closed;
  with_server (fun ~sw ~net addr ->
    let socket = Eio.Net.connect ~sw net addr in
    Eio.Flow.copy_string ws_flood_request socket;
    let reader = Eio.Buf_read.of_flow socket ~max_size:65536 in
    Alcotest.(check string)
      "101"
      ws_101
      (Eio.Buf_read.take (String.length ws_101) reader);
    Alcotest.(check string)
      "server is sending"
      "\x81\x7e\x10\x00"
      (Eio.Buf_read.take 4 reader);
    Eio.Net.setsockopt socket Eio.Net.Sockopt.SO_LINGER (Some 0);
    let reset_at = Unix.gettimeofday () in
    Eio.Flow.close socket;
    Eio.Fiber.first
      (fun () ->
         Alcotest.(check (option int))
           "on_close without a close frame"
           None
           (Eio.Promise.await closed);
         Eio.Stream.take connections_done)
      (fun () ->
         !real_sleep 2.0;
         Alcotest.fail "still running 2 s after the reset");
    let elapsed = Unix.gettimeofday () -. reset_at in
    Alcotest.(check bool)
      (Printf.sprintf "ended within 1 s (%.3f s)" elapsed)
      true
      (elapsed < 1.0));
  on_ws_closed := ignore

let test_flush_after_peer_closed () =
  let flushed, resolve_flushed = Eio.Promise.create () in
  stream_flush_closed := Some resolve_flushed;
  Failing_flow.shutdowns := [];
  with_server
    ~flow:(Failing_flow.wrap ~fail_read:false ~fail_write:false)
    (fun ~sw ~net addr ->
       let socket = Eio.Net.connect ~sw net addr in
       Eio.Flow.copy_string
         "GET /stream HTTP/1.1\r\nhost: localhost\r\n\r\n"
         socket;
       let reader = Eio.Buf_read.of_flow socket ~max_size:65536 in
       let rec skip_head () =
         if Eio.Buf_read.line reader <> "" then skip_head ()
       in
       Alcotest.(check string)
         "status line"
         "HTTP/1.1 200 OK"
         (Eio.Buf_read.line reader);
       skip_head ();
       Eio.Net.setsockopt socket Eio.Net.Sockopt.SO_LINGER (Some 0);
       Eio.Flow.close socket;
       Eio.Promise.await flushed;
       Eio.Stream.take connections_done);
  stream_flush_closed := None;
  check_shutdowns ()

let rec advance clock =
  if not (Eio_mock.Clock.try_advance clock)
  then (
    !real_sleep 0.001;
    advance clock)

let read_all socket =
  Eio.Buf_read.parse_exn Eio.Buf_read.take_all ~max_size:65536 socket

let get_hello = "GET /hello HTTP/1.1\r\nhost: localhost\r\n\r\n"
let hello_response = "HTTP/1.1 200 OK\r\ncontent-length: 11\r\n\r\nhello world"

let hello_response_close =
  "HTTP/1.1 200 OK\r\n\
   content-length: 11\r\n\
   connection: close\r\n\
   \r\n\
   hello world"

let counting_flow = Failing_flow.wrap ~fail_read:false ~fail_write:false

let test_idle_timeout () =
  let clock = Eio_mock.Clock.make () in
  Failing_flow.shutdowns := [];
  with_server ~flow:counting_flow ~clock ~idle_timeout:0.1 (fun ~sw ~net addr ->
    let socket = Eio.Net.connect ~sw net addr in
    advance clock;
    Alcotest.(check (float 0.))
      "fired at the idle timeout"
      0.1
      (Eio.Time.now clock);
    Alcotest.(check string) "nothing written" "" (read_all socket);
    advance clock;
    Alcotest.(check (float 0.))
      "stops waiting for the peer to close"
      0.2
      (Eio.Time.now clock);
    Eio.Stream.take connections_done;
    Eio.Flow.close socket);
  Alcotest.(check int) "no error" 0 (List.length !server_errors);
  check_shutdowns ()

let test_idle_timeout_between_requests () =
  let clock = Eio_mock.Clock.make () in
  with_server ~clock ~idle_timeout:0.1 (fun ~sw ~net addr ->
    let socket = Eio.Net.connect ~sw net addr in
    let reader = Eio.Buf_read.of_flow socket ~max_size:65536 in
    Eio.Flow.copy_string get_hello socket;
    Alcotest.(check string)
      "first response"
      hello_response
      (Eio.Buf_read.take (String.length hello_response) reader);
    Eio.Flow.copy_string get_hello socket;
    Alcotest.(check string)
      "second response"
      hello_response
      (Eio.Buf_read.take (String.length hello_response) reader);
    advance clock;
    Alcotest.(check (float 0.))
      "fired at the idle timeout"
      0.1
      (Eio.Time.now clock);
    Alcotest.(check bool) "closed" true (Eio.Buf_read.at_end_of_input reader);
    Eio.Flow.close socket;
    Eio.Stream.take connections_done);
  Alcotest.(check int) "two requests" 2 !requests_seen

let head_timeout () =
  let clock = Eio_mock.Clock.make () in
  let output = ref "" in
  Failing_flow.shutdowns := [];
  with_server ~flow:counting_flow ~clock ~head_timeout:0.1 (fun ~sw ~net addr ->
    let socket = Eio.Net.connect ~sw net addr in
    Eio.Flow.copy_string "GET /hello HTTP/1.1\r\nHo" socket;
    advance clock;
    Alcotest.(check (float 0.))
      "fired at the head timeout"
      0.1
      (Eio.Time.now clock);
    output := read_all socket;
    Eio.Flow.close socket;
    Eio.Stream.take connections_done);
  Alcotest.(check int) "request handler not called" 0 !requests_seen;
  check_shutdowns ();
  !output

let status_line output =
  match String.index_opt output '\r' with
  | Some i -> String.sub output 0 i
  | None -> output

let test_head_timeout () =
  let output = head_timeout () in
  Alcotest.(check int) "one error" 1 (List.length !server_errors);
  Alcotest.(check bool)
    "4xx status line"
    true
    (String.starts_with ~prefix:"HTTP/1.1 4" output)

let test_head_timeout_408 () =
  Alcotest.(check string)
    "status line"
    "HTTP/1.1 408 Request Timeout"
    (status_line (head_timeout ()))

let test_head_deadline () =
  let clock = Eio_mock.Clock.make () in
  with_server ~clock ~head_timeout:0.1 (fun ~sw ~net addr ->
    let socket = Eio.Net.connect ~sw net addr in
    Eio.Flow.copy_string "GET /hello HTTP/1.1\r\n" socket;
    !real_sleep 0.05;
    Eio_mock.Clock.set_time clock 0.05;
    Eio.Flow.copy_string "a: b\r\n" socket;
    !real_sleep 0.05;
    advance clock;
    Alcotest.(check (float 0.))
      "fired at the head timeout after the first bytes"
      0.1
      (Eio.Time.now clock);
    Alcotest.(check bool)
      "4xx status line"
      true
      (String.starts_with ~prefix:"HTTP/1.1 4" (read_all socket));
    Eio.Flow.close socket;
    Eio.Stream.take connections_done)

let test_slow_body_not_cut () =
  let clock = Eio_mock.Clock.make () in
  with_server ~clock ~idle_timeout:0.1 ~head_timeout:0.1 (fun ~sw ~net addr ->
    let socket = Eio.Net.connect ~sw net addr in
    let reader = Eio.Buf_read.of_flow socket ~max_size:65536 in
    Eio.Flow.copy_string
      "POST /echo HTTP/1.1\r\n\
       host: localhost\r\n\
       content-length: 10\r\n\
       \r\n\
       hello"
      socket;
    let head = "HTTP/1.1 200 OK\r\ncontent-length: 10\r\n\r\nhello" in
    Alcotest.(check string)
      "first half echoed"
      head
      (Eio.Buf_read.take (String.length head) reader);
    Alcotest.(check bool)
      "no timer during the body"
      false
      (Eio_mock.Clock.try_advance clock);
    Eio_mock.Clock.set_time clock 100.;
    Eio.Flow.copy_string "world" socket;
    Alcotest.(check string) "second half" "world" (Eio.Buf_read.take 5 reader);
    Eio.Flow.close socket;
    Eio.Stream.take connections_done);
  Alcotest.(check int) "no error" 0 (List.length !server_errors)

let unread_body_request target =
  Printf.sprintf
    "POST %s HTTP/1.1\r\nhost: localhost\r\ncontent-length: 100000\r\n\r\nabc"
    target

let early_close_response =
  "HTTP/1.1 200 OK\r\nconnection: close\r\ncontent-length: 2\r\n\r\nno"

let early_413_response =
  "HTTP/1.1 413 Content Too Large\r\ncontent-length: 2\r\n\r\nno"

let test_unread_body_without_idle_timeout () =
  List.iter
    (fun (target, response) ->
       with_server (fun ~sw ~net addr ->
         let socket = Eio.Net.connect ~sw net addr in
         Eio.Flow.copy_string (unread_body_request target) socket;
         Alcotest.(check string)
           (target ^ ": response, then the connection closes")
           response
           (read_all socket);
         Eio.Stream.take connections_done;
         Eio.Flow.close socket))
    [ "/early-close", early_close_response; "/early-413", early_413_response ]

(* Decision 0010: the server drains at most 256 KiB of an unread body. *)
let large_413_head =
  "POST /early-413 HTTP/1.1\r\n\
   host: localhost\r\n\
   content-length: 10000000\r\n\
   \r\n"

let large_413_response =
  "HTTP/1.1 413 Content Too Large\r\n\
   content-length: 2\r\n\
   connection: close\r\n\
   \r\n\
   no"

let test_unread_body_drain_limit () =
  let clock = Eio_mock.Clock.make () in
  with_server ~clock ~idle_timeout:0.1 (fun ~sw ~net addr ->
    let socket = Eio.Net.connect ~sw net addr in
    let reader = Eio.Buf_read.of_flow socket ~max_size:65536 in
    Eio.Flow.copy_string large_413_head socket;
    Alcotest.(check string)
      "413 closes"
      large_413_response
      (Eio.Buf_read.take (String.length large_413_response) reader);
    (try Eio.Flow.copy_string (String.make 0x50000 'x') socket with
    | Eio.Io _ -> ());
    Alcotest.(check bool)
      "closed before the idle timeout"
      true
      (try Eio.Buf_read.at_end_of_input reader with Eio.Io _ -> true);
    Alcotest.(check (float 0.)) "no time passed" 0. (Eio.Time.now clock);
    Eio.Flow.close socket;
    Eio.Stream.take connections_done)

let test_unread_body_idle_timeout () =
  let clock = Eio_mock.Clock.make () in
  with_server ~clock ~idle_timeout:0.1 (fun ~sw ~net addr ->
    let socket = Eio.Net.connect ~sw net addr in
    let reader = Eio.Buf_read.of_flow socket ~max_size:65536 in
    Eio.Flow.copy_string (unread_body_request "/early-413") socket;
    Alcotest.(check string)
      "response"
      early_413_response
      (Eio.Buf_read.take (String.length early_413_response) reader);
    advance clock;
    Alcotest.(check (float 0.))
      "closed at the idle timeout"
      0.1
      (Eio.Time.now clock);
    Alcotest.(check bool) "closed" true (Eio.Buf_read.at_end_of_input reader);
    Eio.Flow.close socket;
    Eio.Stream.take connections_done)

let respond_while_body_read_pending ?clock ?idle_timeout () =
  pending_response := None;
  pending_body_failed := false;
  with_server ?clock ?idle_timeout (fun ~sw ~net addr ->
    let socket = Eio.Net.connect ~sw net addr in
    let reader = Eio.Buf_read.of_flow socket ~max_size:65536 in
    Eio.Flow.copy_string (unread_body_request "/respond-later") socket;
    let rec respond () =
      match !pending_response with
      | Some reqd ->
        Request_descriptor.respond_with_string reqd (Response.create `OK) "late"
      | None ->
        !real_sleep 0.001;
        respond ()
    in
    respond ();
    let response = "HTTP/1.1 200 OK\r\ncontent-length: 4\r\n\r\nlate" in
    Alcotest.(check string)
      "response"
      response
      (Eio.Buf_read.take (String.length response) reader);
    Option.iter
      (fun clock ->
         advance clock;
         Alcotest.(check (float 0.))
           "closed at the idle timeout"
           0.1
           (Eio.Time.now clock))
      clock;
    Alcotest.(check bool) "closed" true (Eio.Buf_read.at_end_of_input reader);
    Eio.Flow.close socket;
    Eio.Stream.take connections_done);
  Alcotest.(check bool) "request body failed" true !pending_body_failed

let test_respond_while_body_read_pending () = respond_while_body_read_pending ()

let respond_when_pending () =
  let rec respond () =
    match !pending_response with
    | Some reqd ->
      pending_response := None;
      !real_sleep 0.001;
      Request_descriptor.respond_with_string reqd (Response.create `OK) "late"
    | None ->
      !real_sleep 0.001;
      respond ()
  in
  respond ()

let late_response = "HTTP/1.1 200 OK\r\ncontent-length: 4\r\n\r\nlate"

let body_on_error_raises ?clock ?idle_timeout () =
  pending_response := None;
  with_server ?clock ?idle_timeout (fun ~sw ~net addr ->
    let socket = Eio.Net.connect ~sw net addr in
    let reader = Eio.Buf_read.of_flow socket ~max_size:65536 in
    Eio.Flow.copy_string (unread_body_request "/respond-later-raise") socket;
    respond_when_pending ();
    Alcotest.(check string)
      "response"
      late_response
      (Eio.Buf_read.take (String.length late_response) reader);
    Option.iter advance clock;
    Alcotest.(check bool) "closed" true (Eio.Buf_read.at_end_of_input reader);
    Eio.Flow.close socket;
    Eio.Stream.take connections_done)

let test_body_on_error_raises () = body_on_error_raises ()

let test_body_on_error_raises_idle_timeout () =
  body_on_error_raises ~clock:(Eio_mock.Clock.make ()) ~idle_timeout:0.1 ()

let live_words () =
  Gc.full_major ();
  (Gc.stat ()).live_words

let test_body_read_stops_bounded () =
  let requests = 200 in
  let growth = ref 0 in
  pending_response := None;
  pending_body_failed := false;
  with_server
    ~clock:(Eio_mock.Clock.make ())
    ~idle_timeout:30.
    (fun ~sw ~net addr ->
       let socket = Eio.Net.connect ~sw net addr in
       Eio.Net.setsockopt socket Eio.Net.Sockopt.TCP_NODELAY true;
       let reader = Eio.Buf_read.of_flow socket ~max_size:65536 in
       let exchange () =
         Eio.Flow.copy_string
           "POST /respond-later-then-body HTTP/1.1\r\n\
            host: localhost\r\n\
            content-length: 6\r\n\
            \r\n\
            abc"
           socket;
         respond_when_pending ();
         Alcotest.(check string)
           "response"
           late_response
           (Eio.Buf_read.take (String.length late_response) reader);
         Eio.Flow.copy_string "def" socket
       in
       for _ = 1 to requests / 10 do
         exchange ()
       done;
       let before = live_words () in
       for _ = 1 to requests do
         exchange ()
       done;
       growth := live_words () - before;
       Eio.Flow.close socket;
       Eio.Stream.take connections_done);
  Alcotest.(check bool) "no body failed" false !pending_body_failed;
  if !growth > requests
  then Alcotest.failf "heap grew by %d words over %d requests" !growth requests

let test_respond_while_body_read_pending_idle_timeout () =
  respond_while_body_read_pending
    ~clock:(Eio_mock.Clock.make ())
    ~idle_timeout:0.1
    ()

module Gated_flow = struct
  type t = { inner : Eio.Flow.two_way_ty Eio.Resource.t }

  let bytes_read = ref 0
  let gate : (int * unit Eio.Promise.t * string) option ref = ref None
  let read_methods = []

  let single_read t buf =
    match !gate with
    | Some (after, released, data) when !bytes_read >= after ->
      gate := None;
      Eio.Promise.await released;
      Cstruct.blit_from_string data 0 buf 0 (String.length data);
      String.length data
    | _ ->
      let n = Eio.Flow.single_read t.inner buf in
      bytes_read := !bytes_read + n;
      n

  let single_write t bufs = Eio.Flow.single_write t.inner bufs
  let copy t ~src = Eio.Flow.Pi.simple_copy ~single_write t ~src
  let shutdown t cmd = Eio.Flow.shutdown t.inner cmd

  let wrap socket =
    Eio.Resource.T
      ( { inner = (socket :> Eio.Flow.two_way_ty Eio.Resource.t) }
      , Eio.Flow.Pi.two_way
          (module struct
            type nonrec t = t

            let read_methods = read_methods
            let single_read = single_read
            let single_write = single_write
            let copy = copy
            let shutdown = shutdown
          end) )
end

let test_body_bytes_at_the_deadline () =
  let clock = Eio_mock.Clock.make () in
  let request = unread_body_request "/respond-later" in
  let released, release = Eio.Promise.create () in
  pending_response := None;
  pending_body_failed := false;
  Stdlib.Buffer.clear pending_body_bytes;
  Gated_flow.bytes_read := 0;
  Gated_flow.gate := Some (String.length request, released, "def");
  with_server
    ~flow:Gated_flow.wrap
    ~clock
    ~idle_timeout:0.1
    (fun ~sw ~net addr ->
       let socket = Eio.Net.connect ~sw net addr in
       let reader = Eio.Buf_read.of_flow socket ~max_size:65536 in
       Eio.Flow.copy_string request socket;
       let rec respond () =
         match !pending_response with
         | Some reqd ->
           !real_sleep 0.05;
           Request_descriptor.respond_with_string
             reqd
             (Response.create `OK)
             "late"
         | None ->
           !real_sleep 0.001;
           respond ()
       in
       respond ();
       let response = "HTTP/1.1 200 OK\r\ncontent-length: 4\r\n\r\nlate" in
       Alcotest.(check string)
         "response"
         response
         (Eio.Buf_read.take (String.length response) reader);
       Eio_mock.Clock.set_time clock 0.1;
       Eio.Promise.resolve release ();
       Alcotest.(check bool) "closed" true (Eio.Buf_read.at_end_of_input reader);
       Eio.Flow.close socket;
       Eio.Stream.take connections_done);
  Alcotest.(check string)
    "body bytes before the deadline"
    "abc"
    (Stdlib.Buffer.contents pending_body_bytes);
  Alcotest.(check bool) "request body failed" true !pending_body_failed

let test_no_timeout_configured () =
  with_server (fun ~sw ~net addr ->
    let socket = Eio.Net.connect ~sw net addr in
    let reader = Eio.Buf_read.of_flow socket ~max_size:65536 in
    Eio.Flow.copy_string get_hello socket;
    Alcotest.(check string)
      "first response"
      hello_response
      (Eio.Buf_read.take (String.length hello_response) reader);
    !real_sleep 0.2;
    Eio.Flow.copy_string get_hello socket;
    Alcotest.(check string)
      "second response"
      hello_response
      (Eio.Buf_read.take (String.length hello_response) reader);
    Eio.Flow.close socket;
    Eio.Stream.take connections_done)

let test_timeout_needs_clock () =
  Eio_mock.Backend.run (fun () ->
    match
      Ache_eio.Server.create_connection_handler
        ~idle_timeout:1.
        ~request_handler
        ~error_handler
        (Eio_mock.Flow.make "flow")
        (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0))
    with
    | () -> Alcotest.fail "expected Invalid_argument"
    | exception Invalid_argument _ -> ())

let test_timeout_check_timing () =
  let addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, 0) in
  let raises f =
    match f () with () -> false | exception Invalid_argument _ -> true
  in
  Eio_mock.Backend.run (fun () ->
    let flow = Eio_mock.Flow.make "flow" in
    let bound =
      Ache_eio.Server.create_connection_handler
        ~idle_timeout:(-1.)
        ~request_handler
        ~error_handler
    in
    Alcotest.(check bool)
      "bound: raises on a flow"
      true
      (raises (fun () -> bound flow addr));
    let take (h : Eio_mock.Flow.t -> Eio.Net.Sockaddr.stream -> unit) = h in
    let inline = ref None in
    Alcotest.(check bool)
      "inline: no raise when passed"
      false
      (raises (fun () ->
         inline :=
           Some
             (take
                (Ache_eio.Server.create_connection_handler
                   ~idle_timeout:(-1.)
                   ~request_handler
                   ~error_handler))));
    Alcotest.(check bool)
      "inline: raises on a flow"
      true
      (raises (fun () -> Option.get !inline flow addr));
    Alcotest.(check bool)
      "every optional argument given: raises at once"
      true
      (raises (fun () ->
         ignore
           (Ache_eio.Server.create_connection_handler
              ~config:Ache.Config.default
              ~clock:(Eio_mock.Clock.make ())
              ~idle_timeout:(-1.)
              ~head_timeout:1.
              ~request_handler
              ~error_handler
            : _ -> _ -> unit))))

let test_cancel_is_not_an_error () =
  server_errors := [];
  Eio_main.run (fun _env ->
    Eio.Switch.run (fun sw ->
      let server, client = Eio_unix.Net.socketpair_stream ~sw () in
      Eio.Flow.copy_string "GET /hello HTTP/1.1\r\nHo" client;
      Eio.Fiber.first
        (fun () ->
           Ache_eio.Server.create_connection_handler
             ~request_handler
             ~error_handler
             server
             (`Unix "socketpair"))
        (fun () -> Eio.Fiber.yield ())));
  Alcotest.(check int) "no error reported" 0 (List.length !server_errors)

let test_close_with_trailing_bytes () =
  Failing_flow.shutdowns := [];
  with_server
    ~flow:(Failing_flow.wrap ~fail_read:false ~fail_write:false)
    (fun ~sw ~net addr ->
       let socket = Eio.Net.connect ~sw net addr in
       Eio.Flow.copy_string
         "GET /hello HTTP/1.1\r\nhost: localhost\r\nconnection: close\r\n\r\n"
         socket;
       let reader = Eio.Buf_read.of_flow socket ~max_size:65536 in
       Alcotest.(check string)
         "response"
         hello_response_close
         (Eio.Buf_read.take (String.length hello_response_close) reader);
       Alcotest.(check bool) "closed" true (Eio.Buf_read.at_end_of_input reader);
       Eio.Flow.copy_string "more bytes" socket;
       Eio.Flow.close socket;
       Eio.Stream.take connections_done);
  Alcotest.(check int) "no error" 0 (List.length !server_errors);
  check_shutdowns ()

let test_handler_exception () =
  Failing_flow.shutdowns := [];
  with_server ~flow:counting_flow (fun ~sw ~net addr ->
    let socket = Eio.Net.connect ~sw net addr in
    Eio.Flow.copy_string "GET /raise HTTP/1.1\r\nhost: localhost\r\n\r\n" socket;
    Alcotest.(check string)
      "status line"
      "HTTP/1.1 500 Internal Server Error"
      (status_line (read_all socket));
    Eio.Flow.close socket;
    Eio.Stream.take connections_done);
  check_shutdowns ()

let rec respond_late () =
  match !late_error_response with
  | Some respond ->
    late_error_response := None;
    respond ()
  | None ->
    !real_sleep 0.01;
    respond_late ()

let test_late_error_response () =
  late_error_response := None;
  with_server (fun ~sw ~net addr ->
    let socket = Eio.Net.connect ~sw net addr in
    Eio.Flow.copy_string
      "GET /raise-later HTTP/1.1\r\nhost: localhost\r\n\r\n"
      socket;
    !real_sleep 0.05;
    respond_late ();
    Alcotest.(check string)
      "status line"
      "HTTP/1.1 500 Internal Server Error"
      (status_line (read_all socket));
    Eio.Flow.close socket;
    Eio.Stream.take connections_done)

let test_run_server () =
  Eio_main.run (fun env ->
    Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 10.0 (fun () ->
      Eio.Switch.run (fun sw ->
        let net = Eio.Stdenv.net env in
        let listening =
          Eio.Net.listen
            ~sw
            ~backlog:16
            ~reuse_addr:true
            net
            (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0))
        in
        let stop, resolve_stop = Eio.Promise.create () in
        Eio.Fiber.both
          (fun () ->
             Eio.Net.run_server
               ~stop
               ~on_error:raise
               listening
               (Ache_eio.Server.create_connection_handler
                  ~request_handler
                  ~error_handler))
          (fun () ->
             let socket =
               Eio.Net.connect ~sw net (Eio.Net.listening_addr listening)
             in
             Eio.Flow.copy_string
               "GET /hello HTTP/1.1\r\n\
                host: localhost\r\n\
                connection: close\r\n\
                \r\n"
               socket;
             Alcotest.(check string)
               "response"
               hello_response_close
               (read_all socket);
             Eio.Flow.close socket;
             Eio.Promise.resolve resolve_stop ()))))

let test_pipelined_head_deadline () =
  let clock = Eio_mock.Clock.make () in
  with_server ~clock ~head_timeout:0.5 (fun ~sw ~net addr ->
    let socket = Eio.Net.connect ~sw net addr in
    let reader = Eio.Buf_read.of_flow socket ~max_size:65536 in
    Eio.Flow.copy_string "GET /a HTTP/1.1\r\n" socket;
    !real_sleep 0.05;
    Eio_mock.Clock.set_time clock 0.4;
    Eio.Flow.copy_string "host: x\r\n\r\nGET /b HTTP/1.1\r\n" socket;
    let first = "HTTP/1.1 200 OK\r\ncontent-length: 2\r\n\r\n/a" in
    Alcotest.(check string)
      "first response"
      first
      (Eio.Buf_read.take (String.length first) reader);
    advance clock;
    Alcotest.(check (float 0.))
      "second head times out 0.5 s after its own first byte"
      0.9
      (Eio.Time.now clock);
    Alcotest.(check string)
      "second response"
      "HTTP/1.1 408 Request Timeout"
      (status_line (Eio.Buf_read.take_all reader));
    Eio.Flow.close socket;
    Eio.Stream.take connections_done)

let close_without_reading_eof ?clock ?head_timeout () =
  with_server ?clock ?head_timeout (fun ~sw ~net addr ->
    let socket = Eio.Net.connect ~sw net addr in
    let reader = Eio.Buf_read.of_flow socket ~max_size:65536 in
    Eio.Flow.copy_string
      "GET /hello HTTP/1.1\r\nhost: localhost\r\nconnection: close\r\n\r\n"
      socket;
    Alcotest.(check string)
      "response"
      hello_response_close
      (Eio.Buf_read.take (String.length hello_response_close) reader);
    Option.iter
      (fun clock ->
         advance clock;
         Alcotest.(check (float 0.))
           "linger bound"
           Ache_eio.Server.linger_without_idle_timeout
           (Eio.Time.now clock))
      clock;
    Eio.Stream.take connections_done;
    Eio.Flow.close socket)

let test_no_linger_without_clock () = close_without_reading_eof ()

let test_linger_bounded_without_idle_timeout () =
  close_without_reading_eof ~clock:(Eio_mock.Clock.make ()) ~head_timeout:5. ()

let with_failing_shutdown cmd f =
  Failing_flow.failing_shutdowns := [ cmd ];
  Fun.protect ~finally:(fun () -> Failing_flow.failing_shutdowns := []) f

let test_failing_shutdown cmd () =
  with_failing_shutdown cmd (fun () ->
    with_server ~flow:counting_flow (fun ~sw ~net addr ->
      let socket = Eio.Net.connect ~sw net addr in
      Eio.Flow.copy_string
        "GET /hello HTTP/1.1\r\nhost: localhost\r\nconnection: close\r\n\r\n"
        socket;
      Alcotest.(check string) "response" hello_response_close (read_all socket);
      Eio.Flow.close socket;
      Eio.Stream.take connections_done))

let test_failing_shutdown_all () =
  with_failing_shutdown `All (fun () ->
    with_server
      ~flow:(Failing_flow.wrap ~fail_read:true ~fail_write:false)
      (fun ~sw ~net addr ->
         get_until_closed ~sw ~net addr;
         Eio.Stream.take connections_done))

module Raising = struct
  type t = { mutable sent : bool }

  let next_read_operation _ = `Read
  let read _ _ ~off:_ ~len:_ = failwith "read"
  let read_eof _ _ ~off:_ ~len:_ = failwith "read_eof"
  let yield_reader _ _ = ()

  let next_write_operation t =
    if t.sent
    then `Yield
    else
      `Write
        [ { IOVec.buffer = Ache.Bigstring.of_string "ping" ~off:0 ~len:4
          ; off = 0
          ; len = 4
          }
        ]

  let report_write_result t _ = t.sent <- true
  let yield_writer _ _ = ()
  let report_exn _ _ = failwith "report_exn"
  let is_closed _ = false
  let shutdown _ = ()
end

let test_report_exn_raises () =
  with_server (fun ~sw ~net addr ->
    let client =
      Ache_eio.Client.create_connection ~sw (Eio.Net.connect ~sw net addr)
    in
    let upgraded, resolve_upgraded = Eio.Promise.create () in
    let request_body =
      Ache_eio.Client.request
        client
        (Request.create
           ~headers:
             (Headers.of_list
                [ "host", "localhost"
                ; "connection", "upgrade"
                ; "upgrade", "echo"
                ; "content-length", "0"
                ])
           `GET
           "/upgrade")
        ~error_handler:client_error_handler
        ~response_handler:(fun response _body ->
          Eio.Promise.resolve resolve_upgraded response)
    in
    Body.Writer.close request_body;
    let (_ : Response.t) = Eio.Promise.await upgraded in
    Ache_eio.Client.upgrade
      client
      (Runtime.make (module Raising) { Raising.sent = false });
    Eio.Promise.await (Ache_eio.Client.shutdown client))

module Poison_flow = struct
  type t = { inner : Eio.Flow.two_way_ty Eio.Resource.t }

  let bytes_read = ref 0
  let cancelled_reads = ref 0
  let ops_after_cancel = ref 0
  let send_shutdowns = ref 0

  let reset () =
    bytes_read := 0;
    cancelled_reads := 0;
    ops_after_cancel := 0;
    send_shutdowns := 0

  let check_poisoned () =
    if !cancelled_reads > 0
    then (
      incr ops_after_cancel;
      failwith "operation after a cancelled read")

  let read_methods = []

  let single_read t buf =
    check_poisoned ();
    match Eio.Flow.single_read t.inner buf with
    | n ->
      bytes_read := !bytes_read + n;
      n
    | exception (Eio.Cancel.Cancelled _ as exn) ->
      incr cancelled_reads;
      raise exn

  let single_write t bufs =
    check_poisoned ();
    Eio.Flow.single_write t.inner bufs

  let copy t ~src = Eio.Flow.Pi.simple_copy ~single_write t ~src
  let shutdown_delay : (unit -> unit) ref = ref ignore

  let shutdown t cmd =
    !shutdown_delay ();
    check_poisoned ();
    (match cmd with `Send | `All -> incr send_shutdowns | `Receive -> ());
    Eio.Flow.shutdown t.inner cmd

  let wrap socket =
    Eio.Resource.T
      ( { inner = (socket :> Eio.Flow.two_way_ty Eio.Resource.t) }
      , Eio.Flow.Pi.two_way
          (module struct
            type nonrec t = t

            let read_methods = read_methods
            let single_read = single_read
            let single_write = single_write
            let copy = copy
            let shutdown = shutdown
          end) )

  let check ~bytes_sent =
    Alcotest.(check int) "every byte read once" bytes_sent !bytes_read;
    Alcotest.(check int) "no read cancelled" 0 !cancelled_reads;
    Alcotest.(check int)
      "no operation after a cancelled read"
      0
      !ops_after_cancel
end

let read_head reader =
  let status = Eio.Buf_read.line reader in
  let rec skip () = if Eio.Buf_read.line reader <> "" then skip () in
  skip ();
  status

let test_head_timeout_then_data () =
  let clock = Eio_mock.Clock.make () in
  Poison_flow.reset ();
  let first = "GET /hello HTTP/1.1\r\nHo"
  and late = "st: localhost\r\n\r\nGET /hello HTTP/1.1\r\n\r\n" in
  with_server
    ~flow:Poison_flow.wrap
    ~clock
    ~head_timeout:0.1
    (fun ~sw ~net addr ->
       let socket = Eio.Net.connect ~sw net addr in
       let reader = Eio.Buf_read.of_flow socket ~max_size:65536 in
       Eio.Flow.copy_string first socket;
       advance clock;
       Alcotest.(check string)
         "status line"
         "HTTP/1.1 408 Request Timeout"
         (read_head reader);
       Eio.Flow.copy_string late socket;
       Eio.Flow.shutdown socket `Send;
       Alcotest.(check bool)
         "nothing more"
         true
         (Eio.Buf_read.at_end_of_input reader);
       Eio.Stream.take connections_done;
       Eio.Flow.close socket);
  Alcotest.(check int) "no request handled" 0 !requests_seen;
  Alcotest.(check int) "one error" 1 (List.length !server_errors);
  Poison_flow.check ~bytes_sent:(String.length first + String.length late)

let test_idle_timeout_then_request () =
  let clock = Eio_mock.Clock.make () in
  Poison_flow.reset ();
  with_server
    ~flow:Poison_flow.wrap
    ~clock
    ~idle_timeout:0.1
    (fun ~sw ~net addr ->
       let socket = Eio.Net.connect ~sw net addr in
       advance clock;
       !real_sleep 0.05;
       Eio.Flow.copy_string get_hello socket;
       Eio.Flow.shutdown socket `Send;
       Alcotest.(check string) "no response" "" (read_all socket);
       Eio.Stream.take connections_done;
       Eio.Flow.close socket);
  Alcotest.(check int) "no request handled" 0 !requests_seen;
  Alcotest.(check int) "no error" 0 (List.length !server_errors);
  Poison_flow.check ~bytes_sent:(String.length get_hello)

let test_client_shutdown_with_pending_read () =
  Poison_flow.reset ();
  let errors = ref [] in
  Eio_main.run (fun env ->
    Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 5.0 @@ fun () ->
    Eio.Switch.run (fun sw ->
      let client_end, server_end = Eio_unix.Net.socketpair_stream ~sw () in
      let client =
        Ache_eio.Client.create_connection ~sw (Poison_flow.wrap client_end)
      in
      let request_body =
        Ache_eio.Client.request
          client
          (Request.create
             ~headers:(Headers.of_list [ "host", "localhost" ])
             `GET
             "/")
          ~error_handler:(fun error -> errors := error :: !errors)
          ~response_handler:(fun _ _ -> Alcotest.fail "unexpected response")
      in
      Body.Writer.close request_body;
      let reader = Eio.Buf_read.of_flow server_end ~max_size:65536 in
      let (_ : string) = read_head reader in
      let partial = "HTTP/1.1 200 OK\r\ncontent-le" in
      Eio.Flow.copy_string partial server_end;
      Eio.Time.sleep (Eio.Stdenv.clock env) 0.05;
      (Poison_flow.shutdown_delay :=
         fun () -> Eio.Time.sleep (Eio.Stdenv.clock env) 0.05);
      Fun.protect
        ~finally:(fun () -> Poison_flow.shutdown_delay := ignore)
        (fun () -> Eio.Promise.await (Ache_eio.Client.shutdown client));
      Alcotest.(check bool)
        "peer sees the end of input"
        true
        (Eio.Buf_read.at_end_of_input reader);
      Alcotest.(check int)
        "bytes read"
        (String.length partial)
        !Poison_flow.bytes_read;
      Alcotest.(check int) "Send shutdown" 1 !Poison_flow.send_shutdowns;
      Alcotest.(check int)
        "no operation after a cancelled read"
        0
        !Poison_flow.ops_after_cancel));
  Alcotest.(check int) "one error" 1 (List.length !errors)

let test_upgrade_with_read_pending () =
  with_server (fun ~sw ~net addr ->
    let socket = Eio.Net.connect ~sw net addr in
    Eio.Flow.copy_string
      "GET /upgrade-later HTTP/1.1\r\n\
       host: localhost\r\n\
       connection: upgrade\r\n\
       upgrade: echo\r\n\
       \r\n"
      socket;
    !real_sleep 0.05;
    Eio.Flow.copy_string "ping" socket;
    !real_sleep 0.05;
    Request_descriptor.respond_with_upgrade
      (Option.get !pending_upgrade)
      (Headers.of_list [ "connection", "upgrade"; "upgrade", "echo" ])
      (echo_server ());
    let expected =
      "HTTP/1.1 101 Switching Protocols\r\n\
       connection: upgrade\r\n\
       upgrade: echo\r\n\
       \r\n\
       ping"
    in
    let reader = Eio.Buf_read.of_flow socket ~max_size:65536 in
    Alcotest.(check string)
      "101 then the bytes sent before it"
      expected
      (Eio.Buf_read.take (String.length expected) reader);
    Eio.Flow.shutdown socket `Send;
    Eio.Stream.take connections_done;
    Eio.Flow.close socket)

let domain_handler (_ : Eio.Net.Sockaddr.stream) reqd =
  let body = string_of_int (Stdlib.Domain.self () :> int) in
  Request_descriptor.respond_with_string
    reqd
    (Response.create
       ~headers:
         (Headers.of_list
            [ "content-length", string_of_int (String.length body) ])
       `OK)
    body

let kernel_spreads_connections =
  match
    In_channel.with_open_text "/proc/sys/kernel/ostype" In_channel.input_all
  with
  | ostype -> String.trim ostype = "Linux"
  | exception Sys_error _ -> false

let check_domain_response seen response =
  let body =
    match String.split_on_char '\n' response |> List.rev with
    | body :: _ -> body
    | [] -> ""
  in
  Alcotest.(check string)
    "response"
    (Printf.sprintf
       "HTTP/1.1 200 OK\r\ncontent-length: %d\r\nconnection: close\r\n\r\n%s"
       (String.length body)
       body)
    response;
  Hashtbl.replace seen (int_of_string body) ()

let get_close = "GET / HTTP/1.1\r\nhost: localhost\r\nconnection: close\r\n\r\n"

let test_server_run_domains () =
  Eio_main.run (fun env ->
    let net = Eio.Stdenv.net env in
    let port =
      Eio.Switch.run (fun sw ->
        match
          Eio.Net.listening_addr
            (Eio.Net.listen
               ~sw
               ~backlog:1
               net
               (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0)))
        with
        | `Tcp (_, port) -> port
        | `Unix _ -> assert false)
    in
    let addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, port) in
    Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 10.0 (fun () ->
      Eio.Fiber.first
        (fun () ->
           Ache_eio.Server.run
             ~domains:2
             ~on_error:raise
             ~net
             ~domain_mgr:(Eio.Stdenv.domain_mgr env)
             ~request_handler:domain_handler
             ~error_handler
             addr)
        (fun () ->
           let seen = Hashtbl.create 2 in
           let rec get () =
             match
               Eio.Switch.run (fun sw ->
                 let socket = Eio.Net.connect ~sw net addr in
                 Eio.Flow.copy_string get_close socket;
                 read_all socket)
             with
             | response -> response
             | exception Eio.Io (Eio.Net.E (Connection_failure (Refused _)), _)
               ->
               Eio.Time.sleep (Eio.Stdenv.clock env) 0.01;
               get ()
           in
           let batch () =
             Eio.Fiber.List.iter
               (fun () -> check_domain_response seen (get ()))
               (List.init 16 (fun _ -> ()))
           in
           batch ();
           if kernel_spreads_connections
           then (
             while Hashtbl.length seen < 2 do
               batch ()
             done;
             Alcotest.(check int) "domains that served" 2 (Hashtbl.length seen)))))

let accepted_socket_nodelay ~server_port ~client_port =
  (* Unix.file_descr is int on Unix: otherlibs/unix/unix.ml. *)
  let fd_of_int : int -> Unix.file_descr = Obj.magic in
  let port = function
    | Unix.ADDR_INET (_, port) -> Some port
    | ADDR_UNIX _ -> None
  in
  let rec find n =
    if n = 1024
    then Alcotest.fail "no accepted socket"
    else
      let fd = fd_of_int n in
      match port (Unix.getsockname fd), port (Unix.getpeername fd) with
      | Some local, Some peer when local = server_port && peer = client_port ->
        Unix.getsockopt fd TCP_NODELAY
      | _ | (exception Unix.Unix_error _) -> find (n + 1)
  in
  find 0

let nodelay_response nodelay =
  let body = string_of_bool nodelay in
  Printf.sprintf
    "HTTP/1.1 200 OK\r\ncontent-length: %d\r\nconnection: close\r\n\r\n%s"
    (String.length body)
    body

let test_server_run_tcp_nodelay ?tcp_nodelay expected () =
  Eio_main.run (fun env ->
    let net = Eio.Stdenv.net env in
    let port =
      Eio.Switch.run (fun sw ->
        match
          Eio.Net.listening_addr
            (Eio.Net.listen
               ~sw
               ~backlog:1
               net
               (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0)))
        with
        | `Tcp (_, port) -> port
        | `Unix _ -> assert false)
    in
    let addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, port) in
    let request_handler client reqd =
      let client_port =
        match client with `Tcp (_, port) -> port | `Unix _ -> assert false
      in
      let body =
        string_of_bool (accepted_socket_nodelay ~server_port:port ~client_port)
      in
      Request_descriptor.respond_with_string
        reqd
        (Response.create
           ~headers:
             (Headers.of_list
                [ "content-length", string_of_int (String.length body) ])
           `OK)
        body
    in
    Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 10.0 (fun () ->
      Eio.Fiber.first
        (fun () ->
           Ache_eio.Server.run
             ~domains:1
             ?tcp_nodelay
             ~on_error:raise
             ~net
             ~domain_mgr:(Eio.Stdenv.domain_mgr env)
             ~request_handler
             ~error_handler
             addr)
        (fun () ->
           let rec get () =
             match
               Eio.Switch.run (fun sw ->
                 let socket = Eio.Net.connect ~sw net addr in
                 Eio.Flow.copy_string get_close socket;
                 read_all socket)
             with
             | response -> response
             | exception Eio.Io (Eio.Net.E (Connection_failure (Refused _)), _)
               ->
               Eio.Time.sleep (Eio.Stdenv.clock env) 0.01;
               get ()
           in
           Alcotest.(check string)
             "response"
             (nodelay_response expected)
             (get ()))))

let half_closed_request request =
  let clock = Eio_mock.Clock.make () in
  later_reqd := None;
  let output = ref "" in
  with_server ~clock ~idle_timeout:5. (fun ~sw ~net addr ->
    let socket = Eio.Net.connect ~sw net addr in
    Eio.Flow.copy_string request socket;
    Eio.Flow.shutdown socket `Send;
    let rec respond () =
      match !later_reqd with
      | None ->
        !real_sleep 0.01;
        respond ()
      | Some reqd ->
        !real_sleep 0.1;
        (try
           Request_descriptor.respond_with_string
             reqd
             (Response.create
                ~headers:(Headers.of_list [ "content-length", "5" ])
                `OK)
             "later"
         with
        | exn -> output := "respond raised " ^ Printexc.to_string exn)
    in
    respond ();
    if !output = "" then output := read_all socket;
    Eio.Stream.take connections_done;
    Eio.Flow.close socket);
  Alcotest.(check int) "no error" 0 (List.length !server_errors);
  !output

let test_half_close_close_request () =
  Alcotest.(check string)
    "response"
    "HTTP/1.1 200 OK\r\ncontent-length: 5\r\nconnection: close\r\n\r\nlater"
    (half_closed_request
       "GET /later HTTP/1.1\r\nhost: localhost\r\nconnection: close\r\n\r\n")

let test_half_close_http_1_0 () =
  Alcotest.(check string)
    "status line"
    "HTTP/1.1 200 OK"
    (status_line (half_closed_request "GET /later HTTP/1.0\r\n\r\n"))

let test_half_close_large_response () =
  let clock = Eio_mock.Clock.make () in
  with_server ~clock ~idle_timeout:5. (fun ~sw ~net addr ->
    let socket = Eio.Net.connect ~sw net addr in
    Eio.Flow.copy_string
      "GET /big HTTP/1.1\r\nhost: localhost\r\nconnection: close\r\n\r\n"
      socket;
    Eio.Flow.shutdown socket `Send;
    !real_sleep 0.2;
    let buf = Cstruct.create 65536 in
    let rec drain n =
      match Eio.Flow.single_read socket buf with
      | k -> drain (n + k)
      | exception End_of_file -> n
    in
    let head =
      Printf.sprintf
        "HTTP/1.1 200 OK\r\ncontent-length: %d\r\nconnection: close\r\n\r\n"
        big_size
    in
    Alcotest.(check int)
      "bytes received"
      (String.length head + big_size)
      (drain 0);
    Eio.Stream.take connections_done;
    Eio.Flow.close socket);
  Alcotest.(check int) "no error" 0 (List.length !server_errors)

let test_linger_deadline_not_renewed () =
  let clock = Eio_mock.Clock.make () in
  with_server ~clock ~idle_timeout:0.5 (fun ~sw ~net addr ->
    let socket = Eio.Net.connect ~sw net addr in
    Eio.Flow.copy_string
      "GET /hello HTTP/1.1\r\nhost: localhost\r\nconnection: close\r\n\r\n"
      socket;
    Alcotest.(check string) "response" hello_response_close (read_all socket);
    let rec trickle i =
      if
        i <= 10 && Option.is_none (Eio.Stream.take_nonblocking connections_done)
      then (
        Eio.Flow.copy_string "x" socket;
        !real_sleep 0.05;
        Eio_mock.Clock.set_time clock (0.2 *. float_of_int i);
        !real_sleep 0.05;
        trickle (i + 1))
      else i
    in
    let steps = trickle 1 in
    Alcotest.(check bool)
      "linger ended while the peer kept sending"
      true
      (steps <= 10);
    Alcotest.(check bool)
      "linger lasted until idle_timeout"
      true
      (Eio.Time.now clock >= 0.5);
    Eio.Flow.close socket)

let test_server_run_survives_accept_errors () =
  Eio_main.run (fun env ->
    let net = Eio.Stdenv.net env in
    let clock = Eio.Stdenv.clock env in
    let port =
      Eio.Switch.run (fun sw ->
        match
          Eio.Net.listening_addr
            (Eio.Net.listen
               ~sw
               ~backlog:1
               net
               (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0)))
        with
        | `Tcp (_, port) -> port
        | `Unix _ -> assert false)
    in
    let addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, port) in
    Eio.Time.with_timeout_exn clock 30.0 (fun () ->
      Eio.Switch.run (fun sw ->
        let out_r, out_w = Eio_unix.pipe sw in
        let child =
          Eio.Process.spawn
            ~sw
            (Eio.Stdenv.process_mgr env)
            ~stdout:out_w
            [ "/bin/sh"
            ; "-c"
            ; Printf.sprintf
                "ulimit -n 48 && exec ./accept_error_server.exe %d"
                port
            ]
        in
        Eio.Flow.close out_w;
        let output = Eio.Buf_read.of_flow out_r ~max_size:1_000_000 in
        let rec connect () =
          match Eio.Net.connect ~sw net addr with
          | socket -> socket
          | exception Eio.Io (Eio.Net.E (Connection_failure (Refused _)), _) ->
            Eio.Time.sleep clock 0.01;
            connect ()
        in
        let get = "GET / HTTP/1.1\r\nhost: localhost\r\n\r\n"
        and ok = "HTTP/1.1 200 OK\r\ncontent-length: 2\r\n\r\nok" in
        let first = connect () in
        let first_reader = Eio.Buf_read.of_flow first ~max_size:65536 in
        Eio.Flow.copy_string get first;
        Alcotest.(check string)
          "first response"
          ok
          (Eio.Buf_read.take (String.length ok) first_reader);
        let flood =
          List.filter_map
            (fun () ->
               match Eio.Net.connect ~sw net addr with
               | socket -> Some socket
               | exception Eio.Io _ -> None)
            (List.init 96 (fun _ -> ()))
        in
        let line = Eio.Buf_read.line output in
        if not (String.starts_with ~prefix:"on_error" line)
        then Alcotest.failf "server output: %s" line;
        Eio.Flow.copy_string get first;
        Alcotest.(check string)
          "live connection still served"
          ok
          (match Eio.Buf_read.take (String.length ok) first_reader with
          | response -> response
          | exception End_of_file -> "connection cut");
        List.iter Eio.Flow.close flood;
        Eio.Flow.close first;
        let socket = connect () in
        Eio.Flow.copy_string
          "GET / HTTP/1.1\r\nhost: localhost\r\nconnection: close\r\n\r\n"
          socket;
        Alcotest.(check string)
          "new connection served"
          "HTTP/1.1 200 OK\r\ncontent-length: 2\r\nconnection: close\r\n\r\nok"
          (read_all socket);
        Eio.Process.signal child Sys.sigkill;
        ignore (Eio.Process.await child : Eio.Process.exit_status))))

exception Error_handler_failed

let test_server_run_error_handler_exception () =
  Eio_main.run (fun env ->
    let net = Eio.Stdenv.net env in
    let clock = Eio.Stdenv.clock env in
    let port =
      Eio.Switch.run (fun sw ->
        match
          Eio.Net.listening_addr
            (Eio.Net.listen
               ~sw
               ~backlog:1
               net
               (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0)))
        with
        | `Tcp (_, port) -> port
        | `Unix _ -> assert false)
    in
    let addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, port) in
    let errors = Eio.Stream.create max_int in
    let request_handler _ _ = failwith "request handler failed" in
    let error_handler _ ?request:_ error start_response =
      match error with
      | `Exn _ -> raise Error_handler_failed
      | #Status.standard -> Body.Writer.close (start_response Headers.empty)
    in
    Eio.Time.with_timeout_exn clock 10.0 (fun () ->
      Eio.Fiber.first
        (fun () ->
           Ache_eio.Server.run
             ~domains:1
             ~on_error:(Eio.Stream.add errors)
             ~net
             ~domain_mgr:(Eio.Stdenv.domain_mgr env)
             ~request_handler
             ~error_handler
             addr)
        (fun () ->
           let rec connect sw =
             match Eio.Net.connect ~sw net addr with
             | socket -> socket
             | exception Eio.Io (Eio.Net.E (Connection_failure (Refused _)), _)
               ->
               Eio.Time.sleep clock 0.01;
               connect sw
           in
           Eio.Switch.run (fun sw ->
             let socket = connect sw in
             Eio.Flow.copy_string "GET / HTTP/1.1\r\nhost: x\r\n" socket;
             Eio.Net.setsockopt socket Eio.Net.Sockopt.SO_LINGER (Some 0);
             Eio.Flow.close socket);
           Eio.Time.sleep clock 0.1;
           Eio.Switch.run (fun sw ->
             let socket = connect sw in
             Eio.Flow.copy_string get_close socket;
             let (_ : string) = read_all socket in
             ());
           Alcotest.(check string)
             "on_error gets the error_handler exception"
             (Printexc.to_string Error_handler_failed)
             (Printexc.to_string
                (Eio.Time.with_timeout_exn clock 5.0 (fun () ->
                   Eio.Stream.take errors)));
           Eio.Time.sleep clock 0.1;
           Alcotest.(check int)
             "no other error, none for the reset"
             0
             (Eio.Stream.length errors))))

(* Client: response bytes on an idle connection (undici GHSA-35p6-xmwp-9g52).
   The server runs in another domain with blocking sockets, so that the client
   domain can block while the stray bytes arrive. For each of [strays], it
   accepts a connection, answers the first request, sends the stray bytes 50 ms
   later, and answers a second request if one comes within 1 s. The domain
   returns the second requests. *)
let stray_server strays =
  let listening = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.setsockopt listening Unix.SO_REUSEADDR true;
  Unix.bind listening (Unix.ADDR_INET (Unix.inet_addr_loopback, 0));
  Unix.listen listening 1;
  let port =
    match Unix.getsockname listening with
    | Unix.ADDR_INET (_, port) -> port
    | Unix.ADDR_UNIX _ -> assert false
  in
  let serve stray =
    let socket, _ = Unix.accept listening in
    let write s =
      try ignore (Unix.write_substring socket s 0 (String.length s) : int) with
      | Unix.Unix_error _ -> ()
    in
    let buf = Bytes.create 4096 in
    let rec read_head acc =
      let rec has_end i =
        i + 4 <= String.length acc
        && (String.sub acc i 4 = "\r\n\r\n" || has_end (i + 1))
      in
      if has_end 0
      then acc
      else
        match Unix.read socket buf 0 (Bytes.length buf) with
        | 0 -> acc
        | n -> read_head (acc ^ Bytes.sub_string buf 0 n)
        | exception Unix.Unix_error _ -> acc
    in
    ignore (read_head "" : string);
    write "HTTP/1.1 200 OK\r\nContent-Length: 2\r\n\r\nok";
    Unix.sleepf 0.05;
    write stray;
    Unix.setsockopt_float socket Unix.SO_RCVTIMEO 1.0;
    let second = read_head "" in
    if second <> ""
    then write "HTTP/1.1 200 OK\r\nContent-Length: 4\r\n\r\nreal";
    Unix.close socket;
    second
  in
  ( port
  , Domain.spawn (fun () ->
      Fun.protect
        ~finally:(fun () -> Unix.close listening)
        (fun () -> List.map serve strays)) )

let stray_cases =
  [ "a response", "HTTP/1.1 200 OK\r\nContent-Length: 4\r\n\r\nevil"
  ; "part of a head", "HTTP/1.1 200 OK\r\n"
  ]

let stray_outcome client =
  let result, resolve = Eio.Promise.create () in
  let finish r = ignore (Eio.Promise.try_resolve resolve r : bool) in
  let writer =
    Ache_eio.Client.request
      client
      (Ache.Request.create
         ~headers:(Ache.Headers.of_list [ "host", "localhost" ])
         `GET
         "/")
      ~error_handler:(function
        | `Connection_closed -> finish "Connection_closed"
        | `Malformed_response _ -> finish "Malformed_response"
        | `Invalid_response_body_length _ ->
          finish "Invalid_response_body_length"
        | `Exn exn -> finish (Printexc.to_string exn))
      ~response_handler:(fun response reader ->
        let body = Stdlib.Buffer.create 16 in
        let rec loop () =
          Ache.Body.Reader.schedule_read
            reader
            ~on_error:(fun () -> finish "body error")
            ~on_eof:(fun () ->
              finish
                (Printf.sprintf
                   "%d %s"
                   (Ache.Status.to_code response.status)
                   (Stdlib.Buffer.contents body)))
            ~on_read:(fun bs ~off ~len ->
              Stdlib.Buffer.add_string
                body
                (Ache.Bigstring.substring bs ~off ~len);
              loop ())
        in
        loop ())
  in
  Ache.Body.Writer.close writer;
  Eio.Promise.await result

let test_stray_bytes_on_idle_connection () =
  let port, server = stray_server (List.map snd stray_cases) in
  List.iter
    (fun (name, _) ->
       Eio_main.run (fun env ->
         Eio.Switch.run (fun sw ->
           let socket =
             Eio.Net.connect
               ~sw
               (Eio.Stdenv.net env)
               (`Tcp (Eio.Net.Ipaddr.V4.loopback, port))
           in
           let client = Ache_eio.Client.create_connection ~sw socket in
           Alcotest.(check string)
             (name ^ ": first")
             "200 ok"
             (stray_outcome client);
           (* The stray bytes arrive while this domain does not run the read
              loop. *)
           Unix.sleepf 0.2;
           Alcotest.(check string)
             (name ^ ": second")
             "Connection_closed"
             (stray_outcome client);
           Eio.Promise.await (Ache_eio.Client.shutdown client)));
       ())
    stray_cases;
  Alcotest.(check (list string))
    "second requests not sent"
    (List.map (fun _ -> "") stray_cases)
    (Domain.join server)

let () =
  Alcotest.run
    "ache-eio loopback"
    [ ( "loopback"
      , [ "GET", `Quick, test_get
        ; ( "client: response bytes on an idle connection are not a response"
          , `Quick
          , test_stray_bytes_on_idle_connection )
        ; "POST echo larger than the read buffer", `Quick, test_post_echo
        ; "two pipelined requests", `Quick, test_pipelined
        ; ( "many keep-alive requests on one connection keep the stack bounded"
          , `Quick
          , test_many_requests_bounded_stack )
        ; "101 upgrade to an echo protocol", `Quick, test_upgrade
        ; ( "CONNECT tunnel through respond_with_upgrade and Client.upgrade"
          , `Quick
          , test_connect_tunnel )
        ; ( "CONNECT tunnel gets the bytes sent with the head"
          , `Quick
          , test_connect_tunnel_bytes_after_head )
        ; ( "an incomplete token sent with the upgrade request reaches the new \
             protocol"
          , `Quick
          , test_upgrade_incomplete_token_reaches_new_protocol )
        ; "read failure ends the connection", `Quick, test_read_failure
        ; "write failure ends the connection", `Quick, test_write_failure
        ; "peer closes mid-body", `Quick, test_eof_mid_body
        ; ( "flush calls back with `Closed after the peer closes"
          , `Quick
          , test_flush_after_peer_closed )
        ; ( "WebSocket server sending to a reset peer ends the connection"
          , `Quick
          , test_websocket_peer_reset )
        ; ( "run_server takes the partially applied handler"
          , `Quick
          , test_run_server )
        ; ( "Server.run with 2 domains serves, from both on Linux"
          , `Quick
          , test_server_run_domains )
        ; ( "Server.run keeps serving after accept errors"
          , `Quick
          , test_server_run_survives_accept_errors )
        ; ( "Server.run gives an error_handler exception to on_error"
          , `Quick
          , test_server_run_error_handler_exception )
        ; ( "a half-closed Connection: close request answered later"
          , `Quick
          , test_half_close_close_request )
        ; ( "a half-closed HTTP/1.0 request answered later"
          , `Quick
          , test_half_close_http_1_0 )
        ; ( "a half-closed request gets all of a 16 MiB streamed response"
          , `Quick
          , test_half_close_large_response )
        ; ( "Server.run: TCP_NODELAY off by default"
          , `Quick
          , test_server_run_tcp_nodelay false )
        ; ( "Server.run ~tcp_nodelay:true"
          , `Quick
          , test_server_run_tcp_nodelay ~tcp_nodelay:true true )
        ; ( "normal close with trailing bytes shuts each direction once"
          , `Quick
          , test_close_with_trailing_bytes )
        ; "request handler exception", `Quick, test_handler_exception
        ; "late error response", `Quick, test_late_error_response
        ; ( "cancelling the connection fiber is not an error"
          , `Quick
          , test_cancel_is_not_an_error )
        ; ( "without a clock the handler returns while the peer stays open"
          , `Quick
          , test_no_linger_without_clock )
        ; ( "without idle_timeout lingering is bounded"
          , `Quick
          , test_linger_bounded_without_idle_timeout )
        ; ( "a failing Send shutdown ends the connection"
          , `Quick
          , test_failing_shutdown `Send )
        ; ( "a failing Receive shutdown ends the connection"
          , `Quick
          , test_failing_shutdown `Receive )
        ; ( "a failing All shutdown ends the connection"
          , `Quick
          , test_failing_shutdown_all )
        ; ( "an exception from report_exn ends the connection"
          , `Quick
          , test_report_exn_raises )
        ] )
    ; ( "timeouts"
      , [ "idle connection is closed", `Quick, test_idle_timeout
        ; ( "connection idle between requests is closed"
          , `Quick
          , test_idle_timeout_between_requests )
        ; "incomplete head is rejected", `Quick, test_head_timeout
        ; ( "incomplete head gets 408 Request Timeout"
          , `Quick
          , test_head_timeout_408 )
        ; ( "head timeout counts from the first byte of the head"
          , `Quick
          , test_head_deadline )
        ; "slow request body is not cut", `Quick, test_slow_body_not_cut
        ; "no timeout configured", `Quick, test_no_timeout_configured
        ; ( "unread body after the response, no idle_timeout: closes"
          , `Quick
          , test_unread_body_without_idle_timeout )
        ; ( "unread body after the response: idle_timeout"
          , `Quick
          , test_unread_body_idle_timeout )
        ; ( "unread body past 256 KiB closes the connection"
          , `Quick
          , test_unread_body_drain_limit )
        ; ( "response while a body read waits, no idle_timeout: closes"
          , `Quick
          , test_respond_while_body_read_pending )
        ; ( "response while a body read waits: idle_timeout"
          , `Quick
          , test_respond_while_body_read_pending_idle_timeout )
        ; ( "response while a body read waits, body on_error raises"
          , `Quick
          , test_body_on_error_raises )
        ; ( "response while a body read waits, body on_error raises: \
             idle_timeout"
          , `Quick
          , test_body_on_error_raises_idle_timeout )
        ; ( "body read stops end with their read"
          , `Quick
          , test_body_read_stops_bounded )
        ; ( "body bytes that arrive at the idle timeout are not delivered"
          , `Quick
          , test_body_bytes_at_the_deadline )
        ; "a timeout needs a clock", `Quick, test_timeout_needs_clock
        ; ( "when the timeout arguments are checked"
          , `Quick
          , test_timeout_check_timing )
        ; ( "input during linger does not extend it"
          , `Quick
          , test_linger_deadline_not_renewed )
        ; ( "a pipelined head gets its own deadline"
          , `Quick
          , test_pipelined_head_deadline )
        ; ( "input after a head timeout is read once, no read cancelled"
          , `Quick
          , test_head_timeout_then_data )
        ; ( "a request after the idle timeout is read once, no read cancelled"
          , `Quick
          , test_idle_timeout_then_request )
        ; ( "Client.shutdown cancels no read before its Send shutdown"
          , `Quick
          , test_client_shutdown_with_pending_read )
        ; ( "bytes sent before a delayed 101 reach the new protocol"
          , `Quick
          , test_upgrade_with_read_pending )
        ] )
    ]
