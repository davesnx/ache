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

let resolve computation value =
  ignore (Miou.Computation.try_return computation value : bool)

let await computation =
  match Miou.Computation.await computation with
  | Ok value -> value
  | Error (exn, bt) -> Printexc.raise_with_backtrace exn bt

let server_errors : Server_connection.error list ref = ref []
let partial_body_cut = ref false
let stream_flush_closed : unit Miou.Computation.t option ref = ref None
let requests_seen = ref 0
let on_ws_closed : (int option -> unit) ref = ref ignore
let pending_upgrade : Reqd.t option ref = ref None

exception Respond_later

let late_error_response : (unit -> unit) option ref = ref None

let request_handler (_ : Unix.sockaddr) reqd =
  incr requests_seen;
  let request = Reqd.request reqd in
  let body = Reqd.request_body reqd in
  match request.meth, request.target with
  | `GET, "/hello" ->
    Body.Reader.close body;
    Reqd.respond_with_string
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
      Reqd.respond_with_streaming reqd (Response.create ~headers `OK)
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
    Reqd.respond_with_upgrade
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
      Reqd.respond_with_streaming reqd (Response.create `OK)
    in
    let chunk = String.make 65536 'x' in
    let rec loop () =
      Body.Writer.write_string response_body chunk;
      Body.Writer.flush response_body (function
        | `Written -> loop ()
        | `Closed -> Option.iter (fun c -> resolve c ()) !stream_flush_closed)
    in
    loop ()
  | `GET, "/upgrade-later" -> pending_upgrade := Some reqd
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
    Reqd.respond_with_string
      reqd
      (Response.create
         ~headers:
           (Headers.of_list
              [ "content-length", string_of_int (String.length target) ])
         `OK)
      target

let error_handler (_ : Unix.sockaddr) ?request:_ error start_response =
  server_errors := error :: !server_errors;
  match error with
  | `Exn Respond_later ->
    late_error_response :=
      Some (fun () -> Body.Writer.close (start_response Headers.empty))
  | _ -> Body.Writer.close (start_response Headers.empty)

let connections_done = ref 0
let connections_taken = ref 0

let rec wait_connection_done () =
  if !connections_done > !connections_taken
  then incr connections_taken
  else (
    Miou_unix.sleep 0.001;
    wait_connection_done ())

let with_timeout seconds f =
  let body = Miou.async f
  and timer =
    Miou.async (fun () ->
      Miou_unix.sleep seconds;
      Alcotest.fail "test timed out")
  in
  match Miou.await_first [ body; timer ] with
  | Ok value -> value
  | Error exn -> raise exn

let with_server ?idle_timeout ?head_timeout f =
  server_errors := [];
  requests_seen := 0;
  connections_done := 0;
  connections_taken := 0;
  Miou_unix.run ~domains:0 (fun () ->
    let listening = Miou_unix.tcpv4 () in
    Miou_unix.bind_and_listen
      ~backlog:16
      listening
      (Unix.ADDR_INET (Unix.inet_addr_loopback, 0));
    let addr = Unix.getsockname (Miou_unix.to_file_descr listening) in
    let server =
      Miou.async (fun () ->
        let orphans = Miou.orphans () in
        let rec accept () =
          let socket, peer = Miou_unix.accept listening in
          ignore
            (Miou.async ~orphans (fun () ->
               Fun.protect
                 ~finally:(fun () -> Miou_unix.close socket)
                 (fun () ->
                    Ache_miou.Server.create_connection_handler
                      ?idle_timeout
                      ?head_timeout
                      ~request_handler
                      ~error_handler
                      socket
                      peer);
               incr connections_done)
             : unit Miou.t);
          accept ()
        in
        accept ())
    in
    Fun.protect
      ~finally:(fun () ->
        Miou.cancel server;
        Miou_unix.close listening)
      (fun () -> with_timeout 10.0 (fun () -> f addr)))

let connect addr =
  let socket = Miou_unix.tcpv4 () in
  Miou_unix.connect socket addr;
  socket

let send socket s = Miou_unix.write socket s

let shutdown_send socket =
  Unix.shutdown (Miou_unix.to_file_descr socket) Unix.SHUTDOWN_SEND

let read_all socket =
  let out = Stdlib.Buffer.create 1024 in
  let buf = Bytes.create 4096 in
  let rec loop () =
    match Miou_unix.read socket buf with
    | 0 -> ()
    | n ->
      Stdlib.Buffer.add_subbytes out buf 0 n;
      loop ()
    | exception Unix.Unix_error (ECONNRESET, _, _) -> ()
  in
  loop ();
  Stdlib.Buffer.contents out

let take socket n =
  let buf = Bytes.create n in
  Miou_unix.really_read socket buf;
  Bytes.to_string buf

let at_end_of_input socket =
  match Miou_unix.read socket (Bytes.create 1) with
  | 0 -> true
  | _ -> false
  | exception Unix.Unix_error (ECONNRESET, _, _) -> true

let read_line socket =
  let out = Stdlib.Buffer.create 64 in
  let byte = Bytes.create 1 in
  let rec loop () =
    Miou_unix.really_read socket byte;
    match Bytes.get byte 0 with
    | '\n' -> ()
    | '\r' -> loop ()
    | c ->
      Stdlib.Buffer.add_char out c;
      loop ()
  in
  loop ();
  Stdlib.Buffer.contents out

let read_head socket =
  let status = read_line socket in
  let rec skip () = if read_line socket <> "" then skip () in
  skip ();
  status

let read_body body =
  let buf = Stdlib.Buffer.create 1024 in
  let result = Miou.Computation.create () in
  let rec on_read bs ~off ~len =
    Stdlib.Buffer.add_string buf (Ache.Bigstring.substring bs ~off ~len);
    Body.Reader.schedule_read body ~on_eof ~on_error ~on_read
  and on_eof () = resolve result (Stdlib.Buffer.contents buf)
  and on_error () = resolve result "<response body cut short>" in
  Body.Reader.schedule_read body ~on_eof ~on_error ~on_read;
  result

let client_error_handler error =
  match error with
  | `Malformed_response msg -> Alcotest.failf "malformed response: %s" msg
  | `Invalid_response_body_length _ -> Alcotest.fail "invalid body length"
  | `Connection_closed -> Alcotest.fail "connection closed"
  | `Exn exn -> raise exn

let request client ?(body = "") request =
  let result = Miou.Computation.create () in
  let request_body =
    Ache_miou.Client.request
      client
      request
      ~error_handler:client_error_handler
      ~response_handler:(fun response response_body ->
        resolve result (response, read_body response_body))
  in
  Body.Writer.write_string request_body body;
  Body.Writer.close request_body;
  let response, body = await result in
  response, await body

let get_hello = "GET /hello HTTP/1.1\r\nhost: localhost\r\n\r\n"
let hello_response = "HTTP/1.1 200 OK\r\ncontent-length: 11\r\n\r\nhello world"

let hello_response_close =
  "HTTP/1.1 200 OK\r\n\
   content-length: 11\r\n\
   connection: close\r\n\
   \r\n\
   hello world"

let status_line output =
  match String.index_opt output '\r' with
  | Some i -> String.sub output 0 i
  | None -> output

let check_elapsed name ~from ~min ~max =
  let elapsed = Unix.gettimeofday () -. from in
  if elapsed < min || elapsed > max
  then Alcotest.failf "%s: %.3f s, expected %.3f to %.3f s" name elapsed min max

let test_get () =
  with_server (fun addr ->
    let socket = connect addr in
    let client = Ache_miou.Client.create_connection socket in
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
    Ache_miou.Client.shutdown client;
    Alcotest.(check bool) "closed" true (Ache_miou.Client.is_closed client);
    Miou_unix.close socket)

let test_post_echo () =
  with_server (fun addr ->
    let socket = connect addr in
    let client = Ache_miou.Client.create_connection socket in
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
    Ache_miou.Client.shutdown client;
    Miou_unix.close socket)

let test_pipelined () =
  with_server (fun addr ->
    let socket = connect addr in
    send
      socket
      "GET /a HTTP/1.1\r\n\
       host: localhost\r\n\
       \r\n\
       GET /b HTTP/1.1\r\n\
       host: localhost\r\n\
       connection: close\r\n\
       \r\n";
    let output = read_all socket in
    Miou_unix.close socket;
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

let test_upgrade () =
  with_server (fun addr ->
    let socket = connect addr in
    let client = Ache_miou.Client.create_connection socket in
    let upgraded = Miou.Computation.create () in
    let request_body =
      Ache_miou.Client.request
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
        ~response_handler:(fun response _body -> resolve upgraded response)
    in
    Body.Writer.close request_body;
    let response = await upgraded in
    Alcotest.(check int) "status" 101 (Status.to_code response.status);
    let message = "hello over echo" in
    let echoed = Miou.Computation.create () in
    let received = Stdlib.Buffer.create 16 in
    let echo_client =
      Echo.create ~pending:message (fun t s ->
        Stdlib.Buffer.add_string received s;
        if Stdlib.Buffer.length received >= String.length message
        then (
          Echo.shutdown t;
          resolve echoed (Stdlib.Buffer.contents received)))
    in
    Ache_miou.Client.upgrade client (Runtime.make (module Echo) echo_client);
    Alcotest.(check string) "echoed" message (await echoed);
    Ache_miou.Client.shutdown client;
    Miou_unix.close socket)

let test_upgrade_incomplete_token_reaches_new_protocol () =
  with_server (fun addr ->
    let socket = connect addr in
    send
      socket
      "GET /upgrade HTTP/1.1\r\n\
       host: localhost\r\n\
       connection: upgrade\r\n\
       upgrade: echo\r\n\
       \r\n\
       ping";
    let expected =
      "HTTP/1.1 101 Switching Protocols\r\n\
       connection: upgrade\r\n\
       upgrade: echo\r\n\
       \r\n\
       ping"
    in
    Alcotest.(check string)
      "101 then echoed bytes"
      expected
      (take socket (String.length expected));
    Miou_unix.close socket)

let test_eof_mid_body () =
  partial_body_cut := false;
  with_server (fun addr ->
    let socket = connect addr in
    send
      socket
      "POST /partial HTTP/1.1\r\n\
       host: localhost\r\n\
       content-length: 100\r\n\
       \r\n\
       only ten b";
    shutdown_send socket;
    let (_ : string) = read_all socket in
    Miou_unix.close socket);
  Alcotest.(check bool) "request body cut" true !partial_body_cut;
  match !server_errors with
  | [ `Bad_request ] -> ()
  | _ ->
    Alcotest.failf
      "expected one Bad_request, got %d errors"
      (List.length !server_errors)

let test_half_close_gets_response () =
  with_server (fun addr ->
    let socket = connect addr in
    send socket get_hello;
    shutdown_send socket;
    let output = read_all socket in
    Miou_unix.close socket;
    Alcotest.(check string)
      "response after the peer half-closed"
      hello_response
      output;
    wait_connection_done ());
  Alcotest.(check int) "no error" 0 (List.length !server_errors)

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
  let closed = Miou.Computation.create () in
  on_ws_closed := resolve closed;
  with_server (fun addr ->
    let socket = connect addr in
    send socket ws_flood_request;
    Alcotest.(check string) "101" ws_101 (take socket (String.length ws_101));
    Alcotest.(check string)
      "server is sending"
      "\x81\x7e\x10\x00"
      (take socket 4);
    Unix.setsockopt_optint
      (Miou_unix.to_file_descr socket)
      Unix.SO_LINGER
      (Some 0);
    let reset_at = Unix.gettimeofday () in
    Miou_unix.close socket;
    with_timeout 2.0 (fun () ->
      Alcotest.(check (option int))
        "on_close without a close frame"
        None
        (await closed);
      wait_connection_done ());
    let elapsed = Unix.gettimeofday () -. reset_at in
    Alcotest.(check bool)
      (Printf.sprintf "ended within 1 s (%.3f s)" elapsed)
      true
      (elapsed < 1.0));
  on_ws_closed := ignore

let test_flush_after_peer_closed () =
  let flushed = Miou.Computation.create () in
  stream_flush_closed := Some flushed;
  with_server (fun addr ->
    let socket = connect addr in
    send socket "GET /stream HTTP/1.1\r\nhost: localhost\r\n\r\n";
    Alcotest.(check string) "status line" "HTTP/1.1 200 OK" (read_head socket);
    Unix.setsockopt_optint
      (Miou_unix.to_file_descr socket)
      Unix.SO_LINGER
      (Some 0);
    Miou_unix.close socket;
    await flushed;
    wait_connection_done ());
  stream_flush_closed := None

let test_close_with_trailing_bytes () =
  with_server (fun addr ->
    let socket = connect addr in
    send
      socket
      "GET /hello HTTP/1.1\r\nhost: localhost\r\nconnection: close\r\n\r\n";
    Alcotest.(check string)
      "response"
      hello_response_close
      (take socket (String.length hello_response_close));
    Alcotest.(check bool) "closed" true (at_end_of_input socket);
    send socket "more bytes";
    Miou_unix.close socket;
    wait_connection_done ());
  Alcotest.(check int) "no error" 0 (List.length !server_errors)

let test_handler_exception () =
  with_server (fun addr ->
    let socket = connect addr in
    send socket "GET /raise HTTP/1.1\r\nhost: localhost\r\n\r\n";
    Alcotest.(check string)
      "status line"
      "HTTP/1.1 500 Internal Server Error"
      (status_line (read_all socket));
    Miou_unix.close socket;
    wait_connection_done ())

let rec respond_late () =
  match !late_error_response with
  | Some respond ->
    late_error_response := None;
    respond ()
  | None ->
    Miou_unix.sleep 0.01;
    respond_late ()

let test_late_error_response () =
  late_error_response := None;
  with_server (fun addr ->
    let socket = connect addr in
    send socket "GET /raise-later HTTP/1.1\r\nhost: localhost\r\n\r\n";
    Miou_unix.sleep 0.05;
    respond_late ();
    Alcotest.(check string)
      "status line"
      "HTTP/1.1 500 Internal Server Error"
      (status_line (read_all socket));
    Miou_unix.close socket;
    wait_connection_done ())

let test_cancel_is_not_an_error () =
  server_errors := [];
  Miou_unix.run ~domains:0 (fun () ->
    let server, client = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
    let server = Miou_unix.of_file_descr server
    and client = Miou_unix.of_file_descr client in
    send client "GET /hello HTTP/1.1\r\nHo";
    let handler =
      Miou.async (fun () ->
        Ache_miou.Server.create_connection_handler
          ~request_handler
          ~error_handler
          server
          (Unix.ADDR_UNIX "socketpair"))
    in
    Miou.yield ();
    Miou.cancel handler;
    (match Miou.await handler with
    | Error Miou.Cancelled -> ()
    | Ok () -> Alcotest.fail "the handler returned"
    | Error exn -> raise exn);
    Miou_unix.close server;
    Miou_unix.close client);
  Alcotest.(check int) "no error reported" 0 (List.length !server_errors)

let close_without_reading_eof ?idle_timeout ?head_timeout ~linger () =
  with_server ?idle_timeout ?head_timeout (fun addr ->
    let socket = connect addr in
    send
      socket
      "GET /hello HTTP/1.1\r\nhost: localhost\r\nconnection: close\r\n\r\n";
    Alcotest.(check string)
      "response"
      hello_response_close
      (take socket (String.length hello_response_close));
    let from = Unix.gettimeofday () in
    wait_connection_done ();
    check_elapsed "linger" ~from ~min:(linger -. 0.05) ~max:(linger +. 0.5);
    Miou_unix.close socket)

let test_no_linger_without_timeouts () = close_without_reading_eof ~linger:0. ()

let test_linger_bounded_by_idle_timeout () =
  close_without_reading_eof ~idle_timeout:0.3 ~linger:0.3 ()

let test_linger_bounded_without_idle_timeout () =
  close_without_reading_eof
    ~head_timeout:5.
    ~linger:Ache_miou.Server.linger_without_idle_timeout
    ()

let test_linger_ends_at_peer_close () =
  with_server ~idle_timeout:5. (fun addr ->
    let socket = connect addr in
    send
      socket
      "GET /hello HTTP/1.1\r\nhost: localhost\r\nconnection: close\r\n\r\n";
    Alcotest.(check string)
      "response"
      hello_response_close
      (take socket (String.length hello_response_close));
    let from = Unix.gettimeofday () in
    Miou_unix.close socket;
    wait_connection_done ();
    check_elapsed "returns at the peer close" ~from ~min:0. ~max:1.)

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

let live_words () =
  Gc.full_major ();
  (Gc.stat ()).live_words

let check_timers_bounded ?idle_timeout ?head_timeout ~requests send_request =
  let growth = ref 0 in
  with_server ?idle_timeout ?head_timeout (fun addr ->
    let earlier_live_sleeper = Miou.async (fun () -> Miou_unix.sleep 20.) in
    let socket = connect addr in
    Unix.setsockopt (Miou_unix.to_file_descr socket) Unix.TCP_NODELAY true;
    let get_hellos n =
      for _ = 1 to n do
        send_request socket;
        let response = take socket (String.length hello_response) in
        if response <> hello_response
        then Alcotest.failf "unexpected response %S" response
      done
    in
    get_hellos (requests / 10);
    let before = live_words () in
    get_hellos requests;
    growth := live_words () - before;
    Miou.cancel earlier_live_sleeper;
    Miou_unix.close socket);
  if !growth > requests
  then Alcotest.failf "heap grew by %d words over %d requests" !growth requests

let test_idle_timer_bounded () =
  check_timers_bounded ~idle_timeout:30. ~requests:10_000 (fun socket ->
    send socket get_hello)

let test_head_timer_bounded () =
  check_timers_bounded ~head_timeout:30. ~requests:500 (fun socket ->
    send socket "GET /hello HTTP/1.1\r\n";
    Miou_unix.sleep 0.001;
    send socket "host: localhost\r\n\r\n")

let check_connection_timers_bounded ~connections exchange =
  let growth = ref 0 in
  Miou_unix.run ~domains:0 (fun () ->
    let listening = Miou_unix.tcpv4 () in
    Miou_unix.bind_and_listen
      ~backlog:16
      listening
      (Unix.ADDR_INET (Unix.inet_addr_loopback, 0));
    let addr = Unix.getsockname (Miou_unix.to_file_descr listening) in
    let closed = ref 0 in
    let server =
      Miou.async (fun () ->
        let orphans = Miou.orphans () in
        let rec reap () =
          match Miou.care orphans with
          | Some (Some task) ->
            let (_ : (unit, exn) result) = Miou.await task in
            reap ()
          | Some None | None -> ()
        in
        let rec accept () =
          reap ();
          let socket, peer = Miou_unix.accept listening in
          ignore
            (Miou.async ~orphans (fun () ->
               Fun.protect
                 ~finally:(fun () ->
                   Miou_unix.close socket;
                   incr closed)
                 (fun () ->
                    Ache_miou.Server.create_connection_handler
                      ~idle_timeout:30.
                      ~head_timeout:30.
                      ~request_handler
                      ~error_handler
                      socket
                      peer))
             : unit Miou.t);
          accept ()
        in
        accept ())
    in
    Fun.protect
      ~finally:(fun () ->
        Miou.cancel server;
        Miou_unix.close listening)
      (fun () ->
         with_timeout 10.0 (fun () ->
           let rec short_live_sleepers () =
             Miou_unix.sleep 0.01;
             short_live_sleepers ()
           in
           let live = Miou.async short_live_sleepers in
           let opened = ref 0 in
           let rec all_closed () =
             if !closed < !opened
             then (
               Miou_unix.sleep 0.001;
               all_closed ())
           in
           let exchanges n =
             for _ = 1 to n do
               let socket = connect addr in
               incr opened;
               Miou_unix.sleep 0.001;
               exchange socket;
               Miou_unix.close socket
             done;
             all_closed ()
           in
           exchanges (connections / 10);
           let before = live_words () in
           exchanges connections;
           Miou_unix.sleep 0.6;
           growth := live_words () - before;
           Miou.cancel live)));
  if !growth > connections
  then
    Alcotest.failf
      "heap grew by %d words over %d connections"
      !growth
      connections

let test_closed_connection_timers_bounded () =
  check_connection_timers_bounded ~connections:1000 (fun socket ->
    send socket "GET /hello HTTP/1.1\r\n";
    Miou_unix.sleep 0.001;
    send socket "host: localhost\r\nconnection: close\r\n\r\n";
    Alcotest.(check string)
      "response"
      hello_response_close
      (take socket (String.length hello_response_close)))

let test_idle_connection_timers_bounded () =
  check_connection_timers_bounded ~connections:1000 (fun socket ->
    send socket get_hello;
    Alcotest.(check string)
      "response"
      hello_response
      (take socket (String.length hello_response)))

let test_report_exn_raises () =
  with_server (fun addr ->
    let socket = connect addr in
    let client = Ache_miou.Client.create_connection socket in
    let upgraded = Miou.Computation.create () in
    let request_body =
      Ache_miou.Client.request
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
        ~response_handler:(fun response _body -> resolve upgraded response)
    in
    Body.Writer.close request_body;
    let (_ : Response.t) = await upgraded in
    Ache_miou.Client.upgrade
      client
      (Runtime.make (module Raising) { Raising.sent = false });
    Ache_miou.Client.shutdown client;
    Miou_unix.close socket)

let test_idle_timeout () =
  with_server ~idle_timeout:0.3 (fun addr ->
    let from = Unix.gettimeofday () in
    let socket = connect addr in
    Alcotest.(check string) "nothing written" "" (read_all socket);
    check_elapsed "closed at the idle timeout" ~from ~min:0.25 ~max:0.6;
    wait_connection_done ();
    check_elapsed "stops waiting for the peer to close" ~from ~min:0.55 ~max:1.2;
    Miou_unix.close socket);
  Alcotest.(check int) "no error" 0 (List.length !server_errors)

let test_idle_timeout_after_cancelled_connection () =
  let client = ref None in
  with_server ~idle_timeout:0.3 (fun addr ->
    client := Some (connect addr);
    Miou_unix.sleep 0.05);
  Option.iter
    (fun socket -> Unix.close (Miou_unix.to_file_descr socket))
    !client;
  test_idle_timeout ()

let test_idle_timeout_between_requests () =
  with_server ~idle_timeout:0.3 (fun addr ->
    let socket = connect addr in
    send socket get_hello;
    Alcotest.(check string)
      "first response"
      hello_response
      (take socket (String.length hello_response));
    Miou_unix.sleep 0.2;
    send socket get_hello;
    Alcotest.(check string)
      "second response"
      hello_response
      (take socket (String.length hello_response));
    let from = Unix.gettimeofday () in
    Alcotest.(check bool) "closed" true (at_end_of_input socket);
    check_elapsed "closed at the idle timeout" ~from ~min:0.25 ~max:0.6;
    Miou_unix.close socket;
    wait_connection_done ());
  Alcotest.(check int) "two requests" 2 !requests_seen

let head_timeout () =
  let output = ref "" in
  with_server ~head_timeout:0.3 (fun addr ->
    let socket = connect addr in
    let from = Unix.gettimeofday () in
    send socket "GET /hello HTTP/1.1\r\nHo";
    output := read_all socket;
    check_elapsed "closed at the head timeout" ~from ~min:0.25 ~max:0.6;
    Miou_unix.close socket;
    wait_connection_done ());
  Alcotest.(check int) "request handler not called" 0 !requests_seen;
  !output

let test_head_timeout () =
  let output = head_timeout () in
  Alcotest.(check int) "one error" 1 (List.length !server_errors);
  Alcotest.(check string)
    "status line"
    "HTTP/1.1 408 Request Timeout"
    (status_line output)

let test_head_deadline () =
  with_server ~head_timeout:0.4 (fun addr ->
    let socket = connect addr in
    let from = Unix.gettimeofday () in
    send socket "GET /hello HTTP/1.1\r\n";
    Miou_unix.sleep 0.2;
    send socket "a: b\r\n";
    let output = read_all socket in
    check_elapsed
      "fired at the head timeout after the first bytes"
      ~from
      ~min:0.35
      ~max:0.55;
    Alcotest.(check string)
      "status line"
      "HTTP/1.1 408 Request Timeout"
      (status_line output);
    Miou_unix.close socket;
    wait_connection_done ())

let test_slow_body_not_cut () =
  with_server ~idle_timeout:0.1 ~head_timeout:0.1 (fun addr ->
    let socket = connect addr in
    send
      socket
      "POST /echo HTTP/1.1\r\n\
       host: localhost\r\n\
       content-length: 10\r\n\
       \r\n\
       hello";
    let head = "HTTP/1.1 200 OK\r\ncontent-length: 10\r\n\r\nhello" in
    Alcotest.(check string)
      "first half echoed"
      head
      (take socket (String.length head));
    Miou_unix.sleep 0.4;
    send socket "world";
    Alcotest.(check string) "second half" "world" (take socket 5);
    Miou_unix.close socket;
    wait_connection_done ());
  Alcotest.(check int) "no error" 0 (List.length !server_errors)

let test_no_timeout_configured () =
  with_server (fun addr ->
    let socket = connect addr in
    send socket get_hello;
    Alcotest.(check string)
      "first response"
      hello_response
      (take socket (String.length hello_response));
    Miou_unix.sleep 0.3;
    send socket get_hello;
    Alcotest.(check string)
      "second response"
      hello_response
      (take socket (String.length hello_response));
    Miou_unix.close socket;
    wait_connection_done ())

let test_timeout_check_timing () =
  let raises f =
    match f () with () -> false | exception Invalid_argument _ -> true
  in
  Miou_unix.run ~domains:0 (fun () ->
    let socket = Miou_unix.tcpv4 () in
    let addr = Unix.ADDR_INET (Unix.inet_addr_loopback, 0) in
    let bound =
      Ache_miou.Server.create_connection_handler
        ~idle_timeout:(-1.)
        ~request_handler
        ~error_handler
    in
    Alcotest.(check bool)
      "bound: raises on a socket"
      true
      (raises (fun () -> bound socket addr));
    Alcotest.(check bool)
      "zero head_timeout raises"
      true
      (raises (fun () ->
         Ache_miou.Server.create_connection_handler
           ~head_timeout:0.
           ~request_handler
           ~error_handler
           socket
           addr));
    let take (h : Miou_unix.file_descr -> Unix.sockaddr -> unit) = h in
    let inline = ref None in
    Alcotest.(check bool)
      "inline: no raise when passed"
      false
      (raises (fun () ->
         inline :=
           Some
             (take
                (Ache_miou.Server.create_connection_handler
                   ~idle_timeout:(-1.)
                   ~request_handler
                   ~error_handler))));
    Alcotest.(check bool)
      "inline: raises on a socket"
      true
      (raises (fun () -> Option.get !inline socket addr));
    Alcotest.(check bool)
      "every optional argument given: raises at once"
      true
      (raises (fun () ->
         ignore
           (Ache_miou.Server.create_connection_handler
              ~config:Ache.Config.default
              ~idle_timeout:(-1.)
              ~head_timeout:1.
              ~request_handler
              ~error_handler
            : _ -> _ -> unit)));
    Miou_unix.close socket)

let test_pipelined_head_deadline () =
  with_server ~head_timeout:0.5 (fun addr ->
    let socket = connect addr in
    send socket "GET /a HTTP/1.1\r\n";
    Miou_unix.sleep 0.3;
    let from = Unix.gettimeofday () in
    send socket "host: x\r\n\r\nGET /b HTTP/1.1\r\n";
    let first = "HTTP/1.1 200 OK\r\ncontent-length: 2\r\n\r\n/a" in
    Alcotest.(check string)
      "first response"
      first
      (take socket (String.length first));
    let output = read_all socket in
    check_elapsed
      "second head times out 0.5 s after its own first byte"
      ~from
      ~min:0.45
      ~max:0.8;
    Alcotest.(check string)
      "second response"
      "HTTP/1.1 408 Request Timeout"
      (status_line output);
    Miou_unix.close socket;
    wait_connection_done ())

let test_head_timeout_then_data () =
  let first = "GET /hello HTTP/1.1\r\nHo"
  and late = "st: localhost\r\n\r\nGET /hello HTTP/1.1\r\n\r\n" in
  with_server ~head_timeout:0.1 (fun addr ->
    let socket = connect addr in
    send socket first;
    Alcotest.(check string)
      "status line"
      "HTTP/1.1 408 Request Timeout"
      (read_head socket);
    send socket late;
    shutdown_send socket;
    Alcotest.(check bool) "nothing more" true (at_end_of_input socket);
    wait_connection_done ();
    Miou_unix.close socket);
  Alcotest.(check int) "no request handled" 0 !requests_seen;
  Alcotest.(check int) "one error" 1 (List.length !server_errors)

let test_idle_timeout_then_request () =
  with_server ~idle_timeout:0.1 (fun addr ->
    let socket = connect addr in
    Miou_unix.sleep 0.15;
    send socket get_hello;
    shutdown_send socket;
    Alcotest.(check string) "no response" "" (read_all socket);
    wait_connection_done ();
    Miou_unix.close socket);
  Alcotest.(check int) "no request handled" 0 !requests_seen;
  Alcotest.(check int) "no error" 0 (List.length !server_errors)

let test_client_shutdown_with_pending_read () =
  let errors = ref [] in
  Miou_unix.run ~domains:0 (fun () ->
    with_timeout 5.0 (fun () ->
      let client_end, server_end =
        Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0
      in
      let client_end = Miou_unix.of_file_descr client_end
      and server_end = Miou_unix.of_file_descr server_end in
      let client = Ache_miou.Client.create_connection client_end in
      let request_body =
        Ache_miou.Client.request
          client
          (Request.create
             ~headers:(Headers.of_list [ "host", "localhost" ])
             `GET
             "/")
          ~error_handler:(fun error -> errors := error :: !errors)
          ~response_handler:(fun _ _ -> Alcotest.fail "unexpected response")
      in
      Body.Writer.close request_body;
      let (_ : string) = read_head server_end in
      send server_end "HTTP/1.1 200 OK\r\ncontent-le";
      Miou_unix.sleep 0.05;
      Ache_miou.Client.shutdown client;
      Alcotest.(check bool)
        "peer sees the end of input"
        true
        (at_end_of_input server_end);
      Miou_unix.close client_end;
      Miou_unix.close server_end));
  Alcotest.(check int) "one error" 1 (List.length !errors)

let test_upgrade_with_read_pending () =
  with_server (fun addr ->
    let socket = connect addr in
    send
      socket
      "GET /upgrade-later HTTP/1.1\r\n\
       host: localhost\r\n\
       connection: upgrade\r\n\
       upgrade: echo\r\n\
       \r\n";
    Miou_unix.sleep 0.05;
    send socket "ping";
    Miou_unix.sleep 0.05;
    Reqd.respond_with_upgrade
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
    Alcotest.(check string)
      "101 then the bytes sent before it"
      expected
      (take socket (String.length expected));
    shutdown_send socket;
    wait_connection_done ();
    Miou_unix.close socket)

let domain_handler (_ : Unix.sockaddr) reqd =
  let body = string_of_int (Stdlib.Domain.self () :> int) in
  Reqd.respond_with_string
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

let rec connect_retrying addr =
  let socket = Miou_unix.tcpv4 () in
  match Miou_unix.connect socket addr with
  | () -> socket
  | exception Unix.Unix_error (ECONNREFUSED, _, _) ->
    Miou_unix.close socket;
    Miou_unix.sleep 0.01;
    connect_retrying addr

let test_server_run_domains () =
  server_errors := [];
  Miou_unix.run ~domains:1 (fun () ->
    let port =
      let socket = Miou_unix.tcpv4 () in
      Fun.protect
        ~finally:(fun () -> Miou_unix.close socket)
        (fun () ->
           Miou_unix.bind_and_listen
             socket
             (Unix.ADDR_INET (Unix.inet_addr_loopback, 0));
           match Unix.getsockname (Miou_unix.to_file_descr socket) with
           | ADDR_INET (_, port) -> port
           | ADDR_UNIX _ -> assert false)
    in
    let addr = Unix.ADDR_INET (Unix.inet_addr_loopback, port) in
    let server =
      Miou.async (fun () ->
        Ache_miou.Server.run
          ~domains:2
          ~on_error:raise
          ~request_handler:domain_handler
          ~error_handler
          addr)
    in
    Fun.protect
      ~finally:(fun () -> Miou.cancel server)
      (fun () ->
         with_timeout 10.0 (fun () ->
           let seen = Hashtbl.create 2 in
           let get () =
             let socket = connect_retrying addr in
             Fun.protect
               ~finally:(fun () -> Miou_unix.close socket)
               (fun () ->
                  send socket get_close;
                  read_all socket)
           in
           let batch () =
             List.iter
               (fun response ->
                  check_domain_response seen (Result.get_ok response))
               (Miou.await_all (List.init 16 (fun _ -> Miou.async get)))
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
  server_errors := [];
  Miou_unix.run ~domains:0 (fun () ->
    let port =
      let socket = Miou_unix.tcpv4 () in
      Fun.protect
        ~finally:(fun () -> Miou_unix.close socket)
        (fun () ->
           Miou_unix.bind_and_listen
             socket
             (Unix.ADDR_INET (Unix.inet_addr_loopback, 0));
           match Unix.getsockname (Miou_unix.to_file_descr socket) with
           | ADDR_INET (_, port) -> port
           | ADDR_UNIX _ -> assert false)
    in
    let addr = Unix.ADDR_INET (Unix.inet_addr_loopback, port) in
    let request_handler client reqd =
      let client_port =
        match client with
        | Unix.ADDR_INET (_, port) -> port
        | ADDR_UNIX _ -> assert false
      in
      let body =
        string_of_bool (accepted_socket_nodelay ~server_port:port ~client_port)
      in
      Reqd.respond_with_string
        reqd
        (Response.create
           ~headers:
             (Headers.of_list
                [ "content-length", string_of_int (String.length body) ])
           `OK)
        body
    in
    let server =
      Miou.async (fun () ->
        Ache_miou.Server.run
          ~domains:1
          ?tcp_nodelay
          ~on_error:raise
          ~request_handler
          ~error_handler
          addr)
    in
    Fun.protect
      ~finally:(fun () -> Miou.cancel server)
      (fun () ->
         with_timeout 10.0 (fun () ->
           let socket = connect_retrying addr in
           Fun.protect
             ~finally:(fun () -> Miou_unix.close socket)
             (fun () ->
                send socket get_close;
                Alcotest.(check string)
                  "response"
                  (nodelay_response expected)
                  (read_all socket)))))

let () =
  Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
  Alcotest.run
    "ache-miou loopback"
    [ ( "loopback"
      , [ "GET", `Quick, test_get
        ; "POST echo larger than the read buffer", `Quick, test_post_echo
        ; "two pipelined requests", `Quick, test_pipelined
        ; "101 upgrade to an echo protocol", `Quick, test_upgrade
        ; ( "an incomplete token sent with the upgrade request reaches the new \
             protocol"
          , `Quick
          , test_upgrade_incomplete_token_reaches_new_protocol )
        ; "peer closes mid-body", `Quick, test_eof_mid_body
        ; ( "a request followed by a half-close gets its response"
          , `Quick
          , test_half_close_gets_response )
        ; ( "flush calls back with `Closed after the peer closes"
          , `Quick
          , test_flush_after_peer_closed )
        ; ( "WebSocket server sending to a reset peer ends the connection"
          , `Quick
          , test_websocket_peer_reset )
        ; ( "normal close with trailing bytes"
          , `Quick
          , test_close_with_trailing_bytes )
        ; "request handler exception", `Quick, test_handler_exception
        ; "late error response", `Quick, test_late_error_response
        ; ( "cancelling the connection task is not an error"
          , `Quick
          , test_cancel_is_not_an_error )
        ; ( "without a timeout the handler returns while the peer stays open"
          , `Quick
          , test_no_linger_without_timeouts )
        ; ( "lingering is bounded by idle_timeout"
          , `Quick
          , test_linger_bounded_by_idle_timeout )
        ; ( "without idle_timeout lingering is bounded"
          , `Quick
          , test_linger_bounded_without_idle_timeout )
        ; ( "lingering ends when the peer closes"
          , `Quick
          , test_linger_ends_at_peer_close )
        ; ( "an exception from report_exn ends the connection"
          , `Quick
          , test_report_exn_raises )
        ; ( "Server.run with 2 domains serves, from both on Linux"
          , `Quick
          , test_server_run_domains )
        ; ( "Server.run: TCP_NODELAY off by default"
          , `Quick
          , test_server_run_tcp_nodelay false )
        ; ( "Server.run ~tcp_nodelay:true"
          , `Quick
          , test_server_run_tcp_nodelay ~tcp_nodelay:true true )
        ] )
    ; ( "timeouts"
      , [ "idle connection is closed", `Quick, test_idle_timeout
        ; ( "connection idle between requests is closed"
          , `Quick
          , test_idle_timeout_between_requests )
        ; "incomplete head gets 408 Request Timeout", `Quick, test_head_timeout
        ; ( "head timeout counts from the first byte of the head"
          , `Quick
          , test_head_deadline )
        ; "slow request body is not cut", `Quick, test_slow_body_not_cut
        ; "no timeout configured", `Quick, test_no_timeout_configured
        ; ( "when the timeout arguments are checked"
          , `Quick
          , test_timeout_check_timing )
        ; ( "a pipelined head gets its own deadline"
          , `Quick
          , test_pipelined_head_deadline )
        ; ( "input after a head timeout is not handled"
          , `Quick
          , test_head_timeout_then_data )
        ; ( "a request after the idle timeout is not handled"
          , `Quick
          , test_idle_timeout_then_request )
        ; ( "Client.shutdown with a read pending"
          , `Quick
          , test_client_shutdown_with_pending_read )
        ; ( "bytes sent before a delayed 101 reach the new protocol"
          , `Quick
          , test_upgrade_with_read_pending )
        ; ( "keep-alive requests do not pile up idle timers"
          , `Quick
          , test_idle_timer_bounded )
        ; ( "requests with split heads do not pile up head timers"
          , `Quick
          , test_head_timer_bounded )
        ; ( "idle timeout fires after a connection was cancelled"
          , `Quick
          , test_idle_timeout_after_cancelled_connection )
        ; ( "closed connections do not pile up timers"
          , `Quick
          , test_closed_connection_timers_bounded )
        ; ( "keep-alive connections closed while idle do not pile up timers"
          , `Quick
          , test_idle_connection_timers_bounded )
        ] )
    ]
