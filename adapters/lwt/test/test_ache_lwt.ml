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
let stream_flush_closed : unit Lwt.u option ref = ref None
let stream_start : unit Lwt.t ref = ref Lwt.return_unit

let stream response_body =
  let chunk = String.make 65536 'x' in
  let rec loop () =
    Body.Writer.write_string response_body chunk;
    Body.Writer.flush response_body (function
      | `Written -> loop ()
      | `Closed ->
        Option.iter (fun u -> Lwt.wakeup_later u ()) !stream_flush_closed)
  in
  loop ()

let requests_seen = ref 0
let ws_ended : (string, exn) result Lwt.u option ref = ref None
let ws_flood_sends = ref 0

let end_ws result =
  Option.iter (fun u -> Lwt.wakeup_later u result) !ws_ended;
  ws_ended := None

let ws_echo ws =
  let rec loop () =
    Lwt.bind (Ache_lwt.Websocket.receive ws) (function
      | Some "close-me" ->
        Lwt.bind (Ache_lwt.Websocket.close ws 1001) (fun () ->
          Lwt.map
            (fun next ->
               end_ws
                 (Ok
                    (match next with
                    | None -> "closed, receive None"
                    | Some m -> "received " ^ m)))
            (Ache_lwt.Websocket.receive ws))
      | Some message -> Lwt.bind (Ache_lwt.Websocket.send ws message) loop
      | None ->
        end_ws (Ok "receive None");
        Lwt.return_unit)
  in
  loop ()

let ws_flood ws =
  let chunk = String.make 4096 'x' in
  let rec loop () =
    incr ws_flood_sends;
    Lwt.bind (Ache_lwt.Websocket.send ws chunk) loop
  in
  Lwt.catch loop (fun exn ->
    Lwt.map
      (fun next ->
         end_ws
           (match next with
           | None -> Error exn
           | Some _ -> Ok "receive returned a message after the reset"))
      (Ache_lwt.Websocket.receive ws))

exception Respond_later

let late_error_response : (unit -> unit) option ref = ref None

let request_handler (_ : Unix.sockaddr) reqd =
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
  | `GET, ("/ws-echo" | "/ws-flood") ->
    (match Ache_lwt.Websocket.upgrade reqd with
    | Ok ws ->
      Lwt.async (fun () ->
        if request.target = "/ws-echo" then ws_echo ws else ws_flood ws)
    | Error _ ->
      Request_descriptor.respond_with_string
        reqd
        (Response.create
           ~headers:(Headers.of_list [ "content-length", "0" ])
           `Bad_request)
        "")
  | `GET, "/stream" ->
    Body.Reader.close body;
    stream
      (Request_descriptor.respond_with_streaming reqd (Response.create `OK))
  | `GET, "/stream-late" ->
    Body.Reader.close body;
    Lwt.async (fun () ->
      Lwt.map
        (fun () ->
           stream
             (Request_descriptor.respond_with_streaming
                reqd
                (Response.create `OK)))
        !stream_start)
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

let error_handler (_ : Unix.sockaddr) ?request:_ error start_response =
  server_errors := error :: !server_errors;
  match error with
  | `Exn Respond_later ->
    late_error_response :=
      Some (fun () -> Body.Writer.close (start_response Headers.empty))
  | _ -> Body.Writer.close (start_response Headers.empty)

open Lwt.Infix

let run f = Lwt_main.run (Lwt_unix.with_timeout 10.0 f)
let server_sockets : Lwt_unix.file_descr list ref = ref []

let with_server
      ?idle_timeout
      ?head_timeout
      ?(connection_handler =
        fun ~request_handler ~error_handler addr socket ->
          Ache_lwt.Server.create_connection_handler
            ?idle_timeout
            ?head_timeout
            ~request_handler
            ~error_handler
            addr
            socket)
      f
  =
  server_errors := [];
  requests_seen := 0;
  run (fun () ->
    let listening = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    Lwt_unix.setsockopt listening Unix.SO_REUSEADDR true;
    Lwt_unix.bind listening Unix.(ADDR_INET (inet_addr_loopback, 0))
    >>= fun () ->
    Lwt_unix.listen listening 16;
    let addr = Lwt_unix.getsockname listening in
    let connections = ref [] in
    server_sockets := [];
    let rec accept () =
      Lwt_unix.accept listening >>= fun (socket, client_addr) ->
      server_sockets := socket :: !server_sockets;
      connections :=
        connection_handler ~request_handler ~error_handler client_addr socket
        :: !connections;
      accept ()
    in
    let acceptor = accept () in
    let connect () =
      let socket = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
      Lwt_unix.connect socket addr >|= fun () -> socket
    in
    f connect >>= fun () ->
    Lwt.cancel acceptor;
    Lwt_unix.close listening >>= fun () -> Lwt.join !connections)

let read_body body =
  let buf = Stdlib.Buffer.create 1024 in
  let p, u = Lwt.wait () in
  let rec on_read bs ~off ~len =
    Stdlib.Buffer.add_string buf (Ache.Bigstring.substring bs ~off ~len);
    Body.Reader.schedule_read body ~on_eof ~on_error ~on_read
  and on_eof () = Lwt.wakeup_later u (Stdlib.Buffer.contents buf)
  and on_error () = Lwt.wakeup_later u "<response body cut short>" in
  Body.Reader.schedule_read body ~on_eof ~on_error ~on_read;
  p

let client_error_handler error =
  match error with
  | `Malformed_response msg -> Alcotest.failf "malformed response: %s" msg
  | `Invalid_response_body_length _ -> Alcotest.fail "invalid body length"
  | `Connection_closed -> Alcotest.fail "connection closed"
  | `Exn exn -> raise exn

let request client ?(body = "") request =
  let p, u = Lwt.wait () in
  let request_body =
    Ache_lwt.Client.request
      client
      request
      ~error_handler:client_error_handler
      ~response_handler:(fun response response_body ->
        Lwt.wakeup_later u (response, read_body response_body))
  in
  Body.Writer.write_string request_body body;
  Body.Writer.close request_body;
  p >>= fun (response, body) ->
  body >|= fun body -> response, body

let write_all socket s =
  let rec go off =
    if off = String.length s
    then Lwt.return_unit
    else
      Lwt_unix.write_string socket s off (String.length s - off) >>= fun n ->
      go (off + n)
  in
  go 0

let read_all socket =
  let buf = Stdlib.Buffer.create 1024 in
  let chunk = Bytes.create 4096 in
  let rec go () =
    Lwt_unix.read socket chunk 0 (Bytes.length chunk) >>= function
    | 0 -> Lwt.return (Stdlib.Buffer.contents buf)
    | n ->
      Stdlib.Buffer.add_subbytes buf chunk 0 n;
      go ()
  in
  go ()

let test_get () =
  with_server (fun connect ->
    connect () >>= Ache_lwt.Client.create_connection >>= fun client ->
    request
      client
      (Request.create
         ~headers:(Headers.of_list [ "host", "localhost" ])
         `GET
         "/hello")
    >>= fun (response, body) ->
    Alcotest.(check int) "status" 200 (Status.to_code response.status);
    Alcotest.(check string) "body" "hello world" body;
    Ache_lwt.Client.shutdown client)

let test_post_echo () =
  with_server (fun connect ->
    connect () >>= Ache_lwt.Client.create_connection >>= fun client ->
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
    request client ~body:payload (Request.create ~headers `POST "/echo")
    >>= fun (response, body) ->
    Alcotest.(check int) "status" 200 (Status.to_code response.status);
    Alcotest.(check int) "length" (String.length payload) (String.length body);
    Alcotest.(check bool) "body" true (String.equal payload body);
    Ache_lwt.Client.shutdown client)

let test_pipelined () =
  with_server (fun connect ->
    connect () >>= fun socket ->
    write_all
      socket
      "GET /a HTTP/1.1\r\n\
       host: localhost\r\n\
       \r\n\
       GET /b HTTP/1.1\r\n\
       host: localhost\r\n\
       connection: close\r\n\
       \r\n"
    >>= fun () ->
    read_all socket >>= fun output ->
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
    Lwt_unix.close socket)

let test_upgrade () =
  with_server (fun connect ->
    connect () >>= Ache_lwt.Client.create_connection >>= fun client ->
    let upgraded, resolve_upgraded = Lwt.wait () in
    let request_body =
      Ache_lwt.Client.request
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
          Lwt.wakeup_later resolve_upgraded response)
    in
    Body.Writer.close request_body;
    upgraded >>= fun response ->
    Alcotest.(check int) "status" 101 (Status.to_code response.status);
    let message = "hello over echo" in
    let echoed, resolve_echoed = Lwt.wait () in
    let received = Stdlib.Buffer.create 16 in
    let echo_client =
      Echo.create ~pending:message (fun t s ->
        Stdlib.Buffer.add_string received s;
        if Stdlib.Buffer.length received >= String.length message
        then (
          Echo.shutdown t;
          Lwt.wakeup_later resolve_echoed (Stdlib.Buffer.contents received)))
    in
    Ache_lwt.Client.upgrade client (Runtime.make (module Echo) echo_client);
    echoed >>= fun echoed ->
    Alcotest.(check string) "echoed" message echoed;
    Ache_lwt.Client.shutdown client)

let read_exactly socket n =
  let buf = Bytes.create n in
  let rec go off =
    if off = n
    then Lwt.return (Bytes.to_string buf)
    else
      Lwt_unix.read socket buf off (n - off) >>= function
      | 0 -> Lwt.return (Bytes.sub_string buf 0 off)
      | k -> go (off + k)
  in
  go 0

let test_upgrade_incomplete_token_reaches_new_protocol () =
  with_server (fun connect ->
    connect () >>= fun socket ->
    write_all
      socket
      "GET /upgrade HTTP/1.1\r\n\
       host: localhost\r\n\
       connection: upgrade\r\n\
       upgrade: echo\r\n\
       \r\n\
       ping"
    >>= fun () ->
    let expected =
      "HTTP/1.1 101 Switching Protocols\r\n\
       connection: upgrade\r\n\
       upgrade: echo\r\n\
       \r\n\
       ping"
    in
    read_exactly socket (String.length expected) >>= fun output ->
    Alcotest.(check string) "101 then echoed bytes" expected output;
    Lwt_unix.close socket)

let fail_read = ref false
let fail_write = ref false
let fail_shutdown = ref false
let closes = ref 0

module Test_io = struct
  type socket = Lwt_unix.file_descr
  type addr = Unix.sockaddr

  let read socket bigstring ~off ~len =
    if !fail_read
    then Lwt.fail (Failure "read failed")
    else
      Lwt_bytes.read socket bigstring off len >>= function
      | 0 -> Lwt.fail End_of_file
      | n -> Lwt.return n

  let writev socket iovecs =
    if !fail_write
    then Lwt.fail (Failure "writev failed")
    else
      let vectors = Lwt_unix.IO_vectors.create () in
      List.iter
        (fun { IOVec.buffer; off; len } ->
           Lwt_unix.IO_vectors.append_bigarray vectors buffer off len)
        iovecs;
      Lwt_unix.writev socket vectors >|= fun n -> `Ok n

  let shutdown_receive socket =
    if !fail_shutdown
    then failwith "shutdown failed"
    else
      try Lwt_unix.shutdown socket Unix.SHUTDOWN_RECEIVE with
      | Unix.Unix_error _ -> ()

  let close socket =
    incr closes;
    Lwt_unix.close socket >>= fun () ->
    if !fail_shutdown
    then Lwt.fail (Failure "close failed")
    else Lwt.return_unit
end

module Test_runtime = Ache_lwt.Make (Test_io)
module Test_server = Test_runtime.Server

let with_failing_io ~read ~write =
  fail_read := read;
  fail_write := write;
  closes := 0;
  with_server
    ~connection_handler:(fun ~request_handler ~error_handler addr socket ->
      Test_server.create_connection_handler
        ~request_handler
        ~error_handler
        addr
        socket)
    (fun connect ->
       connect () >>= fun socket ->
       write_all socket "GET /hello HTTP/1.1\r\nhost: localhost\r\n\r\n"
       >>= fun () ->
       Lwt.catch
         (fun () -> read_all socket >|= ignore)
         (function
           | Unix.Unix_error (Unix.ECONNRESET, _, _) -> Lwt.return_unit
           | exn -> Lwt.fail exn)
       >>= fun () -> Lwt_unix.close socket);
  Alcotest.(check int) "close called once" 1 !closes

let test_client_shutdown_closes_once () =
  fail_read := false;
  fail_write := false;
  closes := 0;
  with_server (fun connect ->
    connect () >>= Test_runtime.Client.create_connection >>= fun client ->
    let p, u = Lwt.wait () in
    let request_body =
      Test_runtime.Client.request
        client
        (Request.create
           ~headers:(Headers.of_list [ "host", "localhost" ])
           `GET
           "/hello")
        ~error_handler:client_error_handler
        ~response_handler:(fun response body ->
          Lwt.wakeup_later u (response, read_body body))
    in
    Body.Writer.close request_body;
    p >>= fun (_, body) ->
    body >>= fun body ->
    Alcotest.(check string) "body" "hello world" body;
    Test_runtime.Client.shutdown client >>= fun () -> Lwt_unix.sleep 0.05);
  Alcotest.(check int) "close called once" 1 !closes

let test_handler_exception () =
  fail_read := false;
  fail_write := false;
  closes := 0;
  with_server
    ~connection_handler:(fun ~request_handler ~error_handler addr socket ->
      Test_server.create_connection_handler
        ~request_handler
        ~error_handler
        addr
        socket)
    (fun connect ->
       connect () >>= fun socket ->
       write_all socket "GET /raise HTTP/1.1\r\nhost: localhost\r\n\r\n"
       >>= fun () ->
       read_all socket >>= fun output ->
       Alcotest.(check string)
         "status line"
         "HTTP/1.1 500 Internal Server Error"
         (String.sub output 0 (String.index output '\r'));
       Lwt_unix.close socket);
  Alcotest.(check int) "close called once" 1 !closes

let test_read_failure () = with_failing_io ~read:true ~write:false
let test_write_failure () = with_failing_io ~read:false ~write:true

let test_eof_mid_body () =
  partial_body_cut := false;
  with_server (fun connect ->
    connect () >>= fun socket ->
    write_all
      socket
      "POST /partial HTTP/1.1\r\n\
       host: localhost\r\n\
       content-length: 100\r\n\
       \r\n\
       only ten b"
    >>= fun () ->
    Lwt_unix.shutdown socket Unix.SHUTDOWN_SEND;
    read_all socket >>= fun (_ : string) -> Lwt_unix.close socket);
  Alcotest.(check bool) "request body cut" true !partial_body_cut;
  match !server_errors with
  | [ `Bad_request ] -> ()
  | _ ->
    Alcotest.failf
      "expected one Bad_request, got %d errors"
      (List.length !server_errors)

let check_server_sockets_closed () =
  Alcotest.(check bool)
    "server socket closed"
    true
    (List.for_all (fun s -> Lwt_unix.state s = Lwt_unix.Closed) !server_sockets)

let read_head socket =
  let buf = Stdlib.Buffer.create 1024 in
  let byte = Bytes.create 1 in
  let rec go () =
    let n = Stdlib.Buffer.length buf in
    if n >= 4 && Stdlib.Buffer.sub buf (n - 4) 4 = "\r\n\r\n"
    then Lwt.return (Stdlib.Buffer.contents buf)
    else
      Lwt_unix.read socket byte 0 1 >>= function
      | 0 -> Lwt.return (Stdlib.Buffer.contents buf)
      | _ ->
        Stdlib.Buffer.add_bytes buf byte;
        go ()
  in
  go ()

let test_flush_after_peer_closed () =
  let flushed, resolve_flushed = Lwt.wait () in
  stream_flush_closed := Some resolve_flushed;
  with_server (fun connect ->
    connect () >>= fun socket ->
    write_all socket "GET /stream HTTP/1.1\r\nhost: localhost\r\n\r\n"
    >>= fun () ->
    read_head socket >>= fun head ->
    Alcotest.(check string)
      "status line"
      "HTTP/1.1 200 OK\r\n"
      (String.sub head 0 17);
    Lwt_unix.setsockopt_optint socket Unix.SO_LINGER (Some 0);
    Lwt_unix.close socket >>= fun () -> flushed);
  stream_flush_closed := None;
  check_server_sockets_closed ()

let test_flush_after_peer_closed_epipe () =
  let flushed, resolve_flushed = Lwt.wait () in
  let start, resolve_start = Lwt.wait () in
  stream_flush_closed := Some resolve_flushed;
  stream_start := start;
  with_server (fun connect ->
    connect () >>= fun socket ->
    write_all socket "GET /stream-late HTTP/1.1\r\nhost: localhost\r\n\r\n"
    >>= fun () ->
    Lwt_unix.close socket >>= fun () ->
    Lwt_unix.sleep 0.05 >>= fun () ->
    Lwt.wakeup_later resolve_start ();
    flushed);
  stream_flush_closed := None;
  stream_start := Lwt.return_unit;
  check_server_sockets_closed ()

let ws_handshake target =
  Printf.sprintf
    "GET %s HTTP/1.1\r\n\
     host: localhost\r\n\
     upgrade: websocket\r\n\
     connection: Upgrade\r\n\
     sec-websocket-key: dGhlIHNhbXBsZSBub25jZQ==\r\n\
     sec-websocket-version: 13\r\n\
     \r\n"
    target

let ws_101 =
  "HTTP/1.1 101 Switching Protocols\r\n\
   upgrade: websocket\r\n\
   connection: Upgrade\r\n\
   sec-websocket-accept: s3pPLMBiTxaQ9kYGzzhZRbK+xOo=\r\n\
   \r\n"

let ws_frame ?(fin = true) opcode payload =
  let mask = "\x01\x02\x03\x04" in
  let len = String.length payload in
  assert (len < 126);
  String.make 1 (Char.chr ((if fin then 0x80 else 0) lor opcode))
  ^ String.make 1 (Char.chr (0x80 lor len))
  ^ mask
  ^ String.mapi
      (fun i c -> Char.chr (Char.code c lxor Char.code mask.[i land 3]))
      payload

let read_ws_frame socket =
  read_exactly socket 2 >>= fun head ->
  let len = Char.code head.[1] in
  assert (len < 126);
  read_exactly socket len >|= fun payload -> Char.code head.[0], payload

let wait_ws_end () =
  let ended, u = Lwt.wait () in
  ws_ended := Some u;
  ended

let test_websocket_echo () =
  let ended = wait_ws_end () in
  with_server (fun connect ->
    connect () >>= fun socket ->
    write_all socket (ws_handshake "/ws-echo") >>= fun () ->
    read_head socket >>= fun head ->
    Alcotest.(check string) "101" ws_101 head;
    write_all socket (ws_frame 0x1 "hello") >>= fun () ->
    read_ws_frame socket >>= fun first ->
    write_all
      socket
      (ws_frame ~fin:false 0x1 "w\xc3"
      ^ ws_frame 0x9 "ping"
      ^ ws_frame 0x0 "\xb6rld")
    >>= fun () ->
    read_ws_frame socket >>= fun second ->
    read_ws_frame socket >>= fun third ->
    Alcotest.(check (list (pair int string)))
      "echo, pong, echo of the fragmented message"
      [ 0x81, "hello"; 0x8a, "ping"; 0x81, "w\xc3\xb6rld" ]
      [ first; second; third ];
    write_all socket (ws_frame 0x8 "\x03\xe8") >>= fun () ->
    read_ws_frame socket >>= fun close ->
    Alcotest.(check (pair int string)) "close echoed" (0x88, "\x03\xe8") close;
    read_all socket >>= fun rest ->
    Alcotest.(check string) "then end of input" "" rest;
    ended >>= fun result ->
    Alcotest.(check (result string reject))
      "server loop"
      (Ok "receive None")
      result;
    Lwt_unix.close socket);
  check_server_sockets_closed ()

let test_websocket_server_close () =
  let ended = wait_ws_end () in
  with_server (fun connect ->
    connect () >>= fun socket ->
    write_all socket (ws_handshake "/ws-echo") >>= fun () ->
    read_head socket >>= fun head ->
    Alcotest.(check string) "101" ws_101 head;
    write_all socket (ws_frame 0x1 "close-me") >>= fun () ->
    read_ws_frame socket >>= fun close ->
    Alcotest.(check (pair int string)) "close 1001" (0x88, "\x03\xe9") close;
    ended >>= fun result ->
    Alcotest.(check (result string reject))
      "close resolved"
      (Ok "closed, receive None")
      result;
    write_all socket (ws_frame 0x8 "\x03\xe9") >>= fun () ->
    read_all socket >>= fun rest ->
    Alcotest.(check string) "then end of input" "" rest;
    Lwt_unix.close socket);
  check_server_sockets_closed ()

let test_websocket_receive_backpressure () =
  let ws = ref None in
  let server =
    Server_connection.create (fun reqd ->
      match Ache_lwt.Websocket.upgrade reqd with
      | Ok t -> ws := Some t
      | Error _ -> Alcotest.fail "handshake rejected")
  in
  let feed s =
    let len = String.length s in
    let n =
      Server_connection.read
        server
        (Ache.Bigstring.of_string ~off:0 ~len s)
        ~off:0
        ~len
    in
    Alcotest.(check int) "all consumed" len n
  in
  let rec drain () =
    match Server_connection.next_write_operation server with
    | `Write iovecs ->
      Server_connection.report_write_result
        server
        (`Ok (List.fold_left (fun acc iov -> acc + iov.IOVec.len) 0 iovecs));
      drain ()
    | `Yield | `Close _ -> ()
  in
  let read_operation () =
    match Server_connection.next_read_operation server with
    | `Read -> "`Read"
    | `Yield -> "`Yield"
    | `Close -> "`Close"
  in
  feed (ws_handshake "/ws");
  drain ();
  let ws = Option.get !ws in
  let fed = ref 0 in
  while read_operation () = "`Read" && !fed < 100 do
    feed (ws_frame 0x1 (string_of_int !fed));
    incr fed
  done;
  Alcotest.(check string)
    (Printf.sprintf "reader yields (after %d messages)" !fed)
    "`Yield"
    (read_operation ());
  Alcotest.(check int) "after 8 unconsumed messages" 8 !fed;
  let woken = ref false in
  Server_connection.yield_reader server (fun () -> woken := true);
  let first = Lwt_main.run (Ache_lwt.Websocket.receive ws) in
  Alcotest.(check (option string)) "first message" (Some "0") first;
  Alcotest.(check bool) "receive wakes the reader" true !woken;
  Alcotest.(check string) "reads again" "`Read" (read_operation ());
  feed (ws_frame 0x1 "8");
  Alcotest.(check (list (option string)))
    "the rest, in order"
    (List.init 8 (fun i -> Some (string_of_int (i + 1))))
    (Lwt_main.run
       (Lwt_list.map_s
          (fun () -> Ache_lwt.Websocket.receive ws)
          (List.init 8 ignore)))

let ws_sans_io () =
  let ws = ref None in
  let server =
    Server_connection.create (fun reqd ->
      match Ache_lwt.Websocket.upgrade reqd with
      | Ok t -> ws := Some t
      | Error _ -> Alcotest.fail "handshake rejected")
  in
  let feed s =
    let len = String.length s in
    let (_ : int) =
      Server_connection.read
        server
        (Ache.Bigstring.of_string ~off:0 ~len s)
        ~off:0
        ~len
    in
    ()
  in
  let rec drain () =
    match Server_connection.next_write_operation server with
    | `Write iovecs ->
      Server_connection.report_write_result
        server
        (`Ok (List.fold_left (fun acc iov -> acc + iov.IOVec.len) 0 iovecs));
      drain ()
    | `Yield | `Close _ -> ()
  in
  feed (ws_handshake "/ws");
  drain ();
  Option.get !ws, feed, drain

let receive_within seconds ws =
  Lwt.pick
    [ Ache_lwt.Websocket.receive ws
    ; (Lwt_unix.sleep seconds >|= fun () -> Some "<timeout>")
    ]

let test_websocket_cancelled_receive () =
  let ws, feed, (_ : unit -> unit) = ws_sans_io () in
  Alcotest.(check (option string))
    "first receive times out"
    (Some "<timeout>")
    (Lwt_main.run (receive_within 0.01 ws));
  feed (ws_frame 0x1 "hello");
  Alcotest.(check (option string))
    "the next receive gets the message"
    (Some "hello")
    (Lwt_main.run (receive_within 0.5 ws));
  let cancelled = Ache_lwt.Websocket.receive ws in
  Lwt.cancel cancelled;
  Alcotest.(check bool)
    "Lwt.cancel cancels receive"
    true
    (Lwt.state cancelled = Lwt.Fail Lwt.Canceled);
  feed (ws_frame 0x1 "again");
  Alcotest.(check (option string))
    "the receive after Lwt.cancel gets the message"
    (Some "again")
    (Lwt_main.run (receive_within 0.5 ws))

let test_websocket_cancelled_receive_then_close () =
  let ws, feed, drain = ws_sans_io () in
  Lwt.cancel (Ache_lwt.Websocket.receive ws);
  Alcotest.(check (option string))
    "receive times out"
    (Some "<timeout>")
    (Lwt_main.run (receive_within 0.01 ws));
  feed (ws_frame 0x8 "\x03\xe8");
  drain ();
  Alcotest.(check (option string))
    "a later receive gives None"
    None
    (Lwt_main.run (receive_within 0.5 ws))

let test_websocket_cancelled_receives_bounded () =
  let ws, feed, (_ : unit -> unit) = ws_sans_io () in
  let cancel_many n =
    for _ = 1 to n do
      Lwt.cancel (Ache_lwt.Websocket.receive ws)
    done
  in
  let live_words () =
    Gc.full_major ();
    (Gc.stat ()).live_words
  in
  cancel_many 1_000;
  let before = live_words () in
  cancel_many 100_000;
  let grown = live_words () - before in
  Alcotest.(check bool)
    (Printf.sprintf
       "100000 cancelled receives keep %d more live words (< 10000)"
       grown)
    true
    (grown < 10_000);
  feed (ws_frame 0x1 "after");
  Alcotest.(check (option string))
    "a receive still gets the next message"
    (Some "after")
    (Lwt_main.run (receive_within 0.5 ws))

let test_websocket_peer_reset () =
  let ended = wait_ws_end () in
  ws_flood_sends := 0;
  with_server (fun connect ->
    connect () >>= fun socket ->
    write_all socket (ws_handshake "/ws-flood") >>= fun () ->
    read_head socket >>= fun head ->
    Alcotest.(check string) "101" ws_101 head;
    read_exactly socket 4 >>= fun first ->
    Alcotest.(check string) "server is sending" "\x81\x7e\x10\x00" first;
    Lwt_unix.setsockopt_optint socket Unix.SO_LINGER (Some 0);
    let reset_at = Unix.gettimeofday () in
    Lwt_unix.close socket >>= fun () ->
    Lwt.pick
      [ ended
      ; (Lwt_unix.sleep 2.0 >|= fun () -> Ok "still running 2 s after the reset")
      ]
    >|= fun result ->
    let elapsed = Unix.gettimeofday () -. reset_at in
    Alcotest.(check bool)
      "send failed with Closed, receive is None"
      true
      (result = Error Ache_lwt.Websocket.Closed);
    Alcotest.(check bool)
      (Printf.sprintf "ended within 1 s (%.3f s)" elapsed)
      true
      (elapsed < 1.0));
  check_server_sockets_closed ()

let get_hello = "GET /hello HTTP/1.1\r\nhost: localhost\r\n\r\n"
let hello_response = "HTTP/1.1 200 OK\r\ncontent-length: 11\r\n\r\nhello world"

let check_elapsed name ~since ~at_least =
  let elapsed = Unix.gettimeofday () -. since in
  if elapsed < at_least || elapsed > 2.0
  then Alcotest.failf "%s: closed after %.3f s" name elapsed

let test_idle_timeout () =
  with_server ~idle_timeout:0.1 (fun connect ->
    let since = Unix.gettimeofday () in
    connect () >>= fun socket ->
    read_all socket >>= fun output ->
    check_elapsed "idle" ~since ~at_least:0.1;
    Alcotest.(check string) "nothing written" "" output;
    Lwt_unix.close socket);
  Alcotest.(check int) "no error" 0 (List.length !server_errors);
  check_server_sockets_closed ()

let test_idle_timeout_between_requests () =
  with_server ~idle_timeout:0.3 (fun connect ->
    connect () >>= fun socket ->
    write_all socket get_hello >>= fun () ->
    read_exactly socket (String.length hello_response) >>= fun first ->
    Alcotest.(check string) "first response" hello_response first;
    Lwt_unix.sleep 0.2 >>= fun () ->
    write_all socket get_hello >>= fun () ->
    read_exactly socket (String.length hello_response) >>= fun second ->
    Alcotest.(check string) "second response" hello_response second;
    let since = Unix.gettimeofday () in
    read_all socket >>= fun rest ->
    check_elapsed "idle after a response" ~since ~at_least:0.25;
    Alcotest.(check string) "then closed" "" rest;
    Lwt_unix.close socket);
  Alcotest.(check int) "two requests" 2 !requests_seen

let head_timeout () =
  let output = ref "" in
  with_server ~head_timeout:0.1 (fun connect ->
    connect () >>= fun socket ->
    let since = Unix.gettimeofday () in
    write_all socket "GET /hello HTTP/1.1\r\nHo" >>= fun () ->
    read_all socket >>= fun s ->
    check_elapsed "head" ~since ~at_least:0.1;
    output := s;
    Lwt_unix.close socket);
  Alcotest.(check int) "request handler not called" 0 !requests_seen;
  check_server_sockets_closed ();
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

let rec respond_late () =
  match !late_error_response with
  | Some respond ->
    late_error_response := None;
    respond ();
    Lwt.return_unit
  | None -> Lwt_unix.sleep 0.01 >>= respond_late

let test_late_error_response () =
  late_error_response := None;
  with_server (fun connect ->
    connect () >>= fun socket ->
    write_all socket "GET /raise-later HTTP/1.1\r\nhost: localhost\r\n\r\n"
    >>= fun () ->
    Lwt_unix.sleep 0.05 >>= respond_late >>= fun () ->
    read_all socket >>= fun output ->
    Alcotest.(check string)
      "status line"
      "HTTP/1.1 500 Internal Server Error"
      (status_line output);
    Lwt_unix.close socket)

let test_head_deadline () =
  with_server ~head_timeout:0.4 (fun connect ->
    connect () >>= fun socket ->
    let since = Unix.gettimeofday () in
    write_all socket "GET /hello HTTP/1.1\r\n" >>= fun () ->
    Lwt_unix.sleep 0.2 >>= fun () ->
    write_all socket "a: b\r\n" >>= fun () ->
    read_all socket >>= fun output ->
    let elapsed = Unix.gettimeofday () -. since in
    if elapsed < 0.35 || elapsed > 0.55
    then Alcotest.failf "head rejected after %.3f s, not 0.4 s" elapsed;
    Alcotest.(check bool)
      "4xx status line"
      true
      (String.starts_with ~prefix:"HTTP/1.1 4" output);
    Lwt_unix.close socket)

let test_slow_body_not_cut () =
  with_server ~idle_timeout:0.1 ~head_timeout:0.1 (fun connect ->
    connect () >>= fun socket ->
    write_all
      socket
      "POST /echo HTTP/1.1\r\n\
       host: localhost\r\n\
       content-length: 10\r\n\
       \r\n\
       hello"
    >>= fun () ->
    let head = "HTTP/1.1 200 OK\r\ncontent-length: 10\r\n\r\nhello" in
    read_exactly socket (String.length head) >>= fun first ->
    Alcotest.(check string) "first half echoed" head first;
    Lwt_unix.sleep 0.3 >>= fun () ->
    write_all socket "world" >>= fun () ->
    read_exactly socket 5 >>= fun second ->
    Alcotest.(check string) "second half" "world" second;
    Lwt_unix.close socket);
  Alcotest.(check int) "no error" 0 (List.length !server_errors)

let test_no_timeout_configured () =
  with_server (fun connect ->
    connect () >>= fun socket ->
    write_all socket get_hello >>= fun () ->
    read_exactly socket (String.length hello_response) >>= fun first ->
    Alcotest.(check string) "first response" hello_response first;
    Lwt_unix.sleep 0.2 >>= fun () ->
    write_all socket get_hello >>= fun () ->
    read_exactly socket (String.length hello_response) >>= fun second ->
    Alcotest.(check string) "second response" hello_response second;
    Lwt_unix.close socket)

let test_timeout_must_be_positive () =
  let socket = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  (match
     Ache_lwt.Server.create_connection_handler
       ~idle_timeout:0.
       ~request_handler
       ~error_handler
       (Unix.ADDR_INET (Unix.inet_addr_loopback, 0))
       socket
   with
  | (_ : unit Lwt.t) -> Alcotest.fail "expected Invalid_argument"
  | exception Invalid_argument _ -> ());
  Lwt_main.run (Lwt_unix.close socket)

let test_timeout_check_timing () =
  let addr = Unix.ADDR_INET (Unix.inet_addr_loopback, 0) in
  let socket = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  let raises f =
    match f () with
    | (_ : unit Lwt.t) -> false
    | exception Invalid_argument _ -> true
  in
  let bound =
    Ache_lwt.Server.create_connection_handler
      ~config:Ache.Config.default
      ~idle_timeout:0.
      ~head_timeout:1.
      ~request_handler
      ~error_handler
  in
  Alcotest.(check bool)
    "bound, every option given: raises on a socket"
    true
    (raises (fun () -> bound addr socket));
  let take (h : Unix.sockaddr -> Lwt_unix.file_descr -> unit Lwt.t) = h in
  let inline =
    take
      (Ache_lwt.Server.create_connection_handler
         ~idle_timeout:0.
         ~request_handler
         ~error_handler)
  in
  Alcotest.(check bool)
    "inline: raises on a socket"
    true
    (raises (fun () -> inline addr socket));
  Lwt_main.run (Lwt_unix.close socket)

let contains s sub =
  let n = String.length sub in
  let rec go i =
    i + n <= String.length s && (String.sub s i n = sub || go (i + 1))
  in
  go 0

let test_pipelined_head_deadline () =
  with_server ~head_timeout:1.0 (fun connect ->
    connect () >>= fun socket ->
    write_all socket "GET /a HTTP/1.1\r\n" >>= fun () ->
    Lwt_unix.sleep 0.7 >>= fun () ->
    write_all socket "host: x\r\n\r\nGET /b HTTP/1.1\r\n" >>= fun () ->
    Lwt_unix.sleep 0.6 >>= fun () ->
    write_all socket "host: x\r\nconnection: close\r\n\r\n" >>= fun () ->
    read_all socket >>= fun output ->
    Alcotest.(check bool)
      "second response is not 408"
      false
      (contains output "408");
    Lwt_unix.close socket);
  Alcotest.(check int) "no error" 0 (List.length !server_errors);
  Alcotest.(check int) "both requests served" 2 !requests_seen

let test_head_timer_cancelled () =
  let before = Lwt_engine.timer_count () in
  with_server ~head_timeout:30. (fun connect ->
    List.init 5 Fun.id
    |> Lwt_list.iter_s (fun (_ : int) ->
      connect () >>= fun socket ->
      write_all socket "GET /hello HTTP/1.1\r\nHo" >>= fun () ->
      Lwt_unix.sleep 0.05 >>= fun () ->
      Lwt_unix.shutdown socket Unix.SHUTDOWN_SEND;
      read_all socket >>= fun (_ : string) -> Lwt_unix.close socket));
  let left = Lwt_engine.timer_count () - before in
  if left > 1 then Alcotest.failf "%d timers left after 5 connections" left

let live_words () =
  Gc.full_major ();
  (Gc.stat ()).live_words

let check_timers_bounded ?idle_timeout ?head_timeout ~requests send =
  let previous = Lwt_engine.get () in
  Lwt_engine.set ~destroy:false (new Lwt_engine.select);
  let growth = ref 0 in
  Fun.protect
    ~finally:(fun () -> Lwt_engine.set previous)
    (fun () ->
       with_server ?idle_timeout ?head_timeout (fun connect ->
         let earlier_live_sleeper = Lwt_unix.sleep 20. in
         connect () >>= fun socket ->
         Lwt_unix.setsockopt socket Unix.TCP_NODELAY true;
         let rec get_hellos n =
           if n = 0
           then Lwt.return_unit
           else
             send socket >>= fun () ->
             read_exactly socket (String.length hello_response)
             >>= fun response ->
             if response <> hello_response
             then Alcotest.failf "unexpected response %S" response;
             get_hellos (n - 1)
         in
         get_hellos (requests / 10) >>= fun () ->
         let before = live_words () in
         get_hellos requests >>= fun () ->
         growth := live_words () - before;
         Lwt.cancel earlier_live_sleeper;
         Lwt_unix.close socket));
  if !growth > requests
  then Alcotest.failf "heap grew by %d words over %d requests" !growth requests

let test_idle_timer_bounded () =
  check_timers_bounded ~idle_timeout:30. ~requests:10_000 (fun socket ->
    write_all socket get_hello)

let test_head_timer_bounded () =
  check_timers_bounded ~head_timeout:30. ~requests:500 (fun socket ->
    write_all socket "GET /hello HTTP/1.1\r\n" >>= fun () ->
    Lwt_unix.sleep 0.001 >>= fun () ->
    write_all socket "host: localhost\r\n\r\n")

let check_connection_timers_bounded ~connections exchange =
  let previous = Lwt_engine.get () in
  Lwt_engine.set ~destroy:false (new Lwt_engine.select);
  let growth = ref 0 in
  Fun.protect
    ~finally:(fun () -> Lwt_engine.set previous)
    (fun () ->
       run (fun () ->
         let listening = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
         Lwt_unix.setsockopt listening Unix.SO_REUSEADDR true;
         Lwt_unix.bind listening Unix.(ADDR_INET (inet_addr_loopback, 0))
         >>= fun () ->
         Lwt_unix.listen listening 16;
         let addr = Lwt_unix.getsockname listening in
         let open_connections = ref 0 in
         let rec accept () =
           Lwt_unix.accept listening >>= fun (socket, client_addr) ->
           incr open_connections;
           Lwt.async (fun () ->
             Ache_lwt.Server.create_connection_handler
               ~idle_timeout:30.
               ~head_timeout:30.
               ~request_handler
               ~error_handler
               client_addr
               socket
             >|= fun () -> decr open_connections);
           accept ()
         in
         let acceptor = accept () in
         let earlier_live_sleeper = Lwt_unix.sleep 20. in
         let rec all_closed () =
           if !open_connections = 0
           then Lwt.return_unit
           else Lwt_unix.sleep 0.001 >>= all_closed
         in
         let rec exchanges n =
           if n = 0
           then all_closed ()
           else
             let socket = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
             Lwt_unix.connect socket addr >>= fun () ->
             Lwt_unix.sleep 0.001 >>= fun () ->
             exchange socket >>= fun () ->
             Lwt_unix.close socket >>= fun () -> exchanges (n - 1)
         in
         exchanges (connections / 10) >>= fun () ->
         let before = live_words () in
         exchanges connections >>= fun () ->
         growth := live_words () - before;
         Lwt.cancel earlier_live_sleeper;
         Lwt.cancel acceptor;
         Lwt_unix.close listening));
  if !growth > connections
  then
    Alcotest.failf
      "heap grew by %d words over %d connections"
      !growth
      connections

let test_closed_connection_timers_bounded () =
  check_connection_timers_bounded ~connections:1000 (fun socket ->
    write_all socket "GET /hello HTTP/1.1\r\n" >>= fun () ->
    Lwt_unix.sleep 0.001 >>= fun () ->
    write_all socket "host: localhost\r\nconnection: close\r\n\r\n"
    >>= fun () ->
    read_all socket >>= fun output ->
    Alcotest.(check string)
      "response"
      "HTTP/1.1 200 OK\r\n\
       content-length: 11\r\n\
       connection: close\r\n\
       \r\n\
       hello world"
      output;
    Lwt.return_unit)

let test_idle_connection_timers_bounded () =
  check_connection_timers_bounded ~connections:1000 (fun socket ->
    write_all socket get_hello >>= fun () ->
    read_exactly socket (String.length hello_response) >|= fun response ->
    Alcotest.(check string) "response" hello_response response)

let test_failing_shutdown_and_close () =
  fail_read := false;
  fail_write := false;
  fail_shutdown := true;
  closes := 0;
  Fun.protect
    ~finally:(fun () -> fail_shutdown := false)
    (fun () ->
       with_server
         ~connection_handler:(fun ~request_handler ~error_handler addr socket ->
           Test_server.create_connection_handler
             ~request_handler
             ~error_handler
             addr
             socket)
         (fun connect ->
            connect () >>= fun socket ->
            write_all
              socket
              "GET /hello HTTP/1.1\r\n\
               host: localhost\r\n\
               connection: close\r\n\
               \r\n"
            >>= fun () ->
            read_all socket >>= fun output ->
            Alcotest.(check string)
              "status line"
              "HTTP/1.1 200 OK"
              (status_line output);
            Lwt_unix.close socket));
  Alcotest.(check int) "close called once" 1 !closes

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
  fail_read := false;
  fail_write := false;
  closes := 0;
  with_server (fun connect ->
    connect () >>= Test_runtime.Client.create_connection >>= fun client ->
    let upgraded, resolve_upgraded = Lwt.wait () in
    let request_body =
      Test_runtime.Client.request
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
          Lwt.wakeup_later resolve_upgraded response)
    in
    Body.Writer.close request_body;
    upgraded >>= fun (_ : Response.t) ->
    Test_runtime.Client.upgrade
      client
      (Runtime.make (module Raising) { Raising.sent = false });
    let rec until_closed () =
      if !closes > 0
      then Lwt.return_unit
      else Lwt_unix.sleep 0.01 >>= until_closed
    in
    until_closed ());
  Alcotest.(check int) "close called once" 1 !closes

let expired_after timer =
  let since = Unix.gettimeofday () in
  Ache_lwt.Timeout.expired timer >|= fun () -> Unix.gettimeofday () -. since

let check_between name ~min ~max elapsed =
  if elapsed < min || elapsed > max
  then Alcotest.failf "%s: %.3f s, expected %.3f to %.3f s" name elapsed min max

let test_timeout_expires () =
  run (fun () ->
    let timer = Ache_lwt.Timeout.create 0.1 in
    Ache_lwt.Timeout.set timer;
    expired_after timer >|= check_between "expired" ~min:0.1 ~max:0.5)

let test_timeout_set_moves_deadline () =
  run (fun () ->
    let timer = Ache_lwt.Timeout.create 0.2 in
    Ache_lwt.Timeout.set timer;
    let expired = expired_after timer in
    Lwt_unix.sleep 0.1 >>= fun () ->
    Ache_lwt.Timeout.set timer;
    expired >>= fun elapsed ->
    check_between "moved deadline" ~min:0.3 ~max:0.7 elapsed;
    Ache_lwt.Timeout.set timer;
    expired_after timer >|= check_between "set again" ~min:0.2 ~max:0.6)

let test_timeout_stop () =
  run (fun () ->
    let timer = Ache_lwt.Timeout.create 0.05 in
    Ache_lwt.Timeout.set timer;
    let expired = Ache_lwt.Timeout.expired timer in
    Ache_lwt.Timeout.stop timer;
    Ache_lwt.Timeout.stop timer;
    Lwt_unix.sleep 0.2 >>= fun () ->
    Alcotest.(check bool) "pending after stop" true (Lwt.is_sleeping expired);
    let since = Unix.gettimeofday () in
    Ache_lwt.Timeout.set timer;
    expired >|= fun () ->
    check_between
      "set after stop"
      ~min:0.05
      ~max:0.4
      (Unix.gettimeofday () -. since))

let test_timeout_lengths () =
  run (fun () ->
    let short = Ache_lwt.Timeout.create 0.05
    and long = Ache_lwt.Timeout.create 0.25 in
    Ache_lwt.Timeout.set long;
    Ache_lwt.Timeout.set short;
    let long_expired = expired_after long in
    expired_after short >>= fun elapsed ->
    check_between "short" ~min:0.05 ~max:0.2 elapsed;
    Alcotest.(check bool) "long pending" true (Lwt.is_sleeping long_expired);
    long_expired >|= check_between "long" ~min:0.25 ~max:0.6)

let test_timeout_cancelled_expired () =
  run (fun () ->
    let timer = Ache_lwt.Timeout.create 0.1 in
    Ache_lwt.Timeout.set timer;
    let since = Unix.gettimeofday () in
    Lwt.pick [ Ache_lwt.Timeout.expired timer; Lwt.return_unit ] >>= fun () ->
    Lwt.cancel (Ache_lwt.Timeout.expired timer);
    Ache_lwt.Timeout.expired timer >|= fun () ->
    check_between
      "after cancel"
      ~min:0.1
      ~max:0.5
      (Unix.gettimeofday () -. since))

let test_timeout_create_must_be_positive () =
  List.iter
    (fun seconds ->
       match Ache_lwt.Timeout.create seconds with
       | (_ : Ache_lwt.Timeout.t) -> Alcotest.failf "accepted %g" seconds
       | exception Invalid_argument _ -> ())
    [ 0.; -1.; Float.nan ]

let () =
  Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
  Alcotest.run
    "ache-lwt loopback"
    [ ( "loopback"
      , [ "GET", `Quick, test_get
        ; "POST echo larger than the read buffer", `Quick, test_post_echo
        ; "two pipelined requests", `Quick, test_pipelined
        ; "101 upgrade to an echo protocol", `Quick, test_upgrade
        ; ( "an incomplete token sent with the upgrade request reaches the new \
             protocol"
          , `Quick
          , test_upgrade_incomplete_token_reaches_new_protocol )
        ; "WebSocket echo and close", `Quick, test_websocket_echo
        ; "WebSocket server close", `Quick, test_websocket_server_close
        ; ( "WebSocket receive queue pauses reading"
          , `Quick
          , test_websocket_receive_backpressure )
        ; ( "WebSocket cancelled receive loses no message"
          , `Quick
          , test_websocket_cancelled_receive )
        ; ( "WebSocket cancelled receive, then close"
          , `Quick
          , test_websocket_cancelled_receive_then_close )
        ; ( "WebSocket cancelled receives do not pile up"
          , `Quick
          , test_websocket_cancelled_receives_bounded )
        ; ( "WebSocket server sending to a reset peer ends the connection"
          , `Quick
          , test_websocket_peer_reset )
        ; "read failure ends the connection", `Quick, test_read_failure
        ; "write failure ends the connection", `Quick, test_write_failure
        ; "request handler exception", `Quick, test_handler_exception
        ; "late error response", `Quick, test_late_error_response
        ; ( "client shutdown closes the socket once"
          , `Quick
          , test_client_shutdown_closes_once )
        ; "peer closes mid-body", `Quick, test_eof_mid_body
        ; ( "flush calls back with `Closed after the peer closes"
          , `Quick
          , test_flush_after_peer_closed )
        ; ( "flush calls back with `Closed after writing to a closed peer \
             (EPIPE)"
          , `Quick
          , test_flush_after_peer_closed_epipe )
        ; ( "failing shutdown_receive and close end the connection"
          , `Quick
          , test_failing_shutdown_and_close )
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
        ; "a timeout must be positive", `Quick, test_timeout_must_be_positive
        ; ( "when the timeout arguments are checked"
          , `Quick
          , test_timeout_check_timing )
        ; ( "a pipelined head gets its own deadline"
          , `Quick
          , test_pipelined_head_deadline )
        ; ( "the head timer ends with the connection"
          , `Quick
          , test_head_timer_cancelled )
        ; ( "keep-alive requests do not pile up idle timers"
          , `Quick
          , test_idle_timer_bounded )
        ; ( "requests with split heads do not pile up head timers"
          , `Quick
          , test_head_timer_bounded )
        ; ( "closed connections do not pile up timers"
          , `Quick
          , test_closed_connection_timers_bounded )
        ; ( "keep-alive connections closed while idle do not pile up timers"
          , `Quick
          , test_idle_connection_timers_bounded )
        ] )
    ; ( "Timeout"
      , [ "expires after its deadline", `Quick, test_timeout_expires
        ; "set moves the deadline", `Quick, test_timeout_set_moves_deadline
        ; "stop keeps it from expiring", `Quick, test_timeout_stop
        ; "two lengths expire in order", `Quick, test_timeout_lengths
        ; ( "cancelling expired does not change it"
          , `Quick
          , test_timeout_cancelled_expired )
        ; ( "create rejects a length that is not positive"
          , `Quick
          , test_timeout_create_must_be_positive )
        ] )
    ]
