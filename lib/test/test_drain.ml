open Ache

(* A synchronous driver: feed input while the connection asks for it, collect
   everything it writes. *)

let feed conn s =
  let len = String.length s in
  let bs = Bigstring.of_string s ~off:0 ~len in
  let rec go off =
    if off < len
    then
      match Server_connection.next_read_operation conn with
      | `Read ->
        let n = Server_connection.read conn bs ~off ~len:(len - off) in
        if n > 0 then go (off + n) else off
      | `Yield | `Close -> off
    else off
  in
  go 0

let output conn =
  let buf = Stdlib.Buffer.create 256 in
  let rec go () =
    match Server_connection.next_write_operation conn with
    | `Write iovecs ->
      let s = Helpers.Write_operation.iovecs_to_string iovecs in
      Stdlib.Buffer.add_string buf s;
      Server_connection.report_write_result conn (`Ok (String.length s));
      go ()
    | `Yield -> `Yield
    | `Close _ -> `Close
  in
  let state = go () in
  Stdlib.Buffer.contents buf, state

let read_op conn =
  match Server_connection.next_read_operation conn with
  | `Read -> "Read"
  | `Yield -> "Yield"
  | `Close -> "Close"

let get = "GET / HTTP/1.1\r\nHost: x\r\n\r\n"

let respond reqd =
  Body.Reader.close (Request_descriptor.request_body reqd);
  Request_descriptor.respond_with_string
    reqd
    (Response.create ~headers:(Headers.of_list [ "content-length", "2" ]) `OK)
    "ok"

let ok = "HTTP/1.1 200 OK\r\ncontent-length: 2\r\n\r\nok"

let ok_close =
  "HTTP/1.1 200 OK\r\ncontent-length: 2\r\nconnection: close\r\n\r\nok"

let check_string = Alcotest.(check string)

let check_state msg expected actual =
  Alcotest.(check string)
    msg
    expected
    (match actual with `Yield -> "Yield" | `Close -> "Close")

let test_drain_idle () =
  let conn = Server_connection.create respond in
  ignore (feed conn get : int);
  let out, _ = output conn in
  check_string "first response" ok out;
  check_string "reads" "Read" (read_op conn);
  Server_connection.drain conn;
  check_string "idle connection closes" "Close" (read_op conn);
  let out, state = output conn in
  check_string "nothing more written" "" out;
  check_state "writer closes" "Close" state;
  Alcotest.(check bool) "closed" true (Server_connection.is_closed conn)

let test_drain_before_any_request () =
  let conn = Server_connection.create respond in
  Server_connection.drain conn;
  check_string "closes" "Close" (read_op conn);
  Server_connection.drain conn;
  check_string "drain again does nothing" "Close" (read_op conn)

let test_drain_mid_head () =
  let handled = ref 0 in
  let conn =
    Server_connection.create (fun reqd ->
      incr handled;
      respond reqd)
  in
  ignore (feed conn "GET / HTTP/1.1\r\nHo" : int);
  Server_connection.drain conn;
  check_string "still reads the head" "Read" (read_op conn);
  ignore (feed conn ("st: x\r\n\r\n" ^ get) : int);
  let out, state = output conn in
  check_string "the started request gets a closing response" ok_close out;
  check_state "then closes" "Close" state;
  Alcotest.(check int) "one request served" 1 !handled;
  check_string "reader closed" "Close" (read_op conn)

let test_drain_mid_body () =
  let body = Stdlib.Buffer.create 16 in
  let reqd_ref = ref None in
  let conn =
    Server_connection.create (fun reqd ->
      reqd_ref := Some reqd;
      let request_body = Request_descriptor.request_body reqd in
      let rec read () =
        Body.Reader.schedule_read
          request_body
          ~on_eof:(fun () -> respond reqd)
          ~on_error:ignore
          ~on_read:(fun bs ~off ~len ->
            Stdlib.Buffer.add_string body (Bigstring.substring bs ~off ~len);
            read ())
      in
      read ())
  in
  ignore
    (feed conn "POST / HTTP/1.1\r\nHost: x\r\nContent-Length: 10\r\n\r\nhello"
     : int);
  Server_connection.drain conn;
  check_string "reads the rest of the body" "Read" (read_op conn);
  ignore (feed conn ("world" ^ get) : int);
  check_string "whole body" "helloworld" (Stdlib.Buffer.contents body);
  let out, state = output conn in
  check_string "closing response" ok_close out;
  check_state "closes" "Close" state

let test_drain_streaming_response () =
  let response_body = ref None in
  let conn =
    Server_connection.create (fun reqd ->
      Body.Reader.close (Request_descriptor.request_body reqd);
      response_body :=
        Some
          (Request_descriptor.respond_with_streaming
             ~flush_headers_immediately:true
             reqd
             (Response.create `OK)))
  in
  ignore (feed conn get : int);
  let out, state = output conn in
  check_string
    "chunked head"
    "HTTP/1.1 200 OK\r\ntransfer-encoding: chunked\r\n\r\n"
    out;
  check_state "waits for the body" "Yield" state;
  Server_connection.drain conn;
  let body = Option.get !response_body in
  Body.Writer.write_string body "abc";
  Body.Writer.close body;
  let out, state = output conn in
  check_string "body ends as framed" "3\r\nabc\r\n0\r\n\r\n" out;
  check_state "then closes" "Close" state;
  check_string "no new request" "Close" (read_op conn)

let test_drain_pipelined () =
  let handled = ref 0 in
  let pending = ref None in
  let conn =
    Server_connection.create (fun reqd ->
      incr handled;
      pending := Some reqd)
  in
  ignore (feed conn (get ^ get ^ get) : int);
  Alcotest.(check int) "first handler called" 1 !handled;
  Server_connection.drain conn;
  respond (Option.get !pending);
  let out, state = output conn in
  check_string "first response closes" ok_close out;
  check_state "closes" "Close" state;
  Alcotest.(check int) "queued requests are not served" 1 !handled

let test_drain_after_response_started () =
  let conn = Server_connection.create respond in
  ignore (feed conn get : int);
  Server_connection.drain conn;
  let out, state = output conn in
  check_string "response written before drain is not changed" ok out;
  check_state "then closes" "Close" state

let test_max_requests () =
  let config = { Config.default with max_requests_per_connection = Some 3 } in
  let handled = ref 0 in
  let conn =
    Server_connection.create ~config (fun reqd ->
      incr handled;
      respond reqd)
  in
  ignore (feed conn get : int);
  let out, _ = output conn in
  check_string "1st" ok out;
  ignore (feed conn get : int);
  let out, _ = output conn in
  check_string "2nd" ok out;
  ignore (feed conn (get ^ get) : int);
  let out, state = output conn in
  check_string "3rd closes" ok_close out;
  check_state "writer closes" "Close" state;
  Alcotest.(check int) "4th not served" 3 !handled;
  check_string "reader closed" "Close" (read_op conn)

let test_max_requests_one () =
  let config = { Config.default with max_requests_per_connection = Some 1 } in
  let conn = Server_connection.create ~config respond in
  ignore (feed conn get : int);
  let out, state = output conn in
  check_string "first closes" ok_close out;
  check_state "closes" "Close" state

let test_max_requests_invalid () =
  let config = { Config.default with max_requests_per_connection = Some 0 } in
  match Server_connection.create ~config respond with
  | _ -> Alcotest.fail "expected Invalid_argument"
  | exception Invalid_argument _ -> ()

let test_drain_websocket () =
  let c = Test_websocket.upgraded () in
  Server_connection.drain c.server;
  Test_websocket.check_output c (Test_websocket.close_frame_out 1001) `Close;
  Test_websocket.check_reader_closed c

let test_drain_before_101_written () =
  let c = Test_websocket.connect () in
  Server_connection.drain c.server;
  Test_websocket.check_output
    c
    (Test_websocket.response_101 ^ Test_websocket.close_frame_out 1001)
    `Close

let ok_ka hint =
  "HTTP/1.1 200 OK\r\ncontent-length: 2\r\nKeep-Alive: " ^ hint ^ "\r\n\r\nok"

let test_keep_alive_hint () =
  let conn = Server_connection.create ~keep_alive_timeout:5 respond in
  ignore (feed conn get : int);
  let out, _ = output conn in
  check_string "hint" (ok_ka "timeout=5") out;
  let conn = Server_connection.create respond in
  ignore (feed conn get : int);
  let out, _ = output conn in
  check_string "no hint without keep_alive_timeout" ok out

let test_keep_alive_max () =
  let config = { Config.default with max_requests_per_connection = Some 3 } in
  let conn = Server_connection.create ~config ~keep_alive_timeout:5 respond in
  ignore (feed conn get : int);
  let out, _ = output conn in
  check_string "1st" (ok_ka "timeout=5, max=2") out;
  ignore (feed conn get : int);
  let out, _ = output conn in
  check_string "2nd" (ok_ka "timeout=5, max=1") out;
  ignore (feed conn get : int);
  let out, state = output conn in
  check_string "3rd closes, no hint" ok_close out;
  check_state "closes" "Close" state

let test_keep_alive_not_on_close () =
  let conn = Server_connection.create ~keep_alive_timeout:5 respond in
  ignore
    (feed conn "GET / HTTP/1.1\r\nHost: x\r\nConnection: close\r\n\r\n" : int);
  let out, _ = output conn in
  check_string "closing response has no hint" ok_close out;
  let conn = Server_connection.create ~keep_alive_timeout:5 respond in
  ignore (feed conn "GET / HTTP/1.0\r\n\r\n" : int);
  let out, _ = output conn in
  check_string "HTTP/1.0 response that closes has no hint" ok_close out;
  let conn = Server_connection.create ~keep_alive_timeout:5 respond in
  ignore (feed conn "GET / HTTP/1.0\r\nConnection: keep-alive\r\n\r\n" : int);
  let out, _ = output conn in
  check_string
    "HTTP/1.0 keep-alive gets the hint"
    "HTTP/1.1 200 OK\r\n\
     content-length: 2\r\n\
     connection: keep-alive\r\n\
     Keep-Alive: timeout=5\r\n\
     \r\n\
     ok"
    out;
  let conn = Server_connection.create ~keep_alive_timeout:5 respond in
  Server_connection.drain conn;
  ignore (feed conn get : int);
  let out, _ = output conn in
  check_string "drained connection: nothing served" "" out

let test_keep_alive_app_and_date () =
  let conn =
    Server_connection.create ~keep_alive_timeout:5 (fun reqd ->
      Body.Reader.close (Request_descriptor.request_body reqd);
      Request_descriptor.respond_with_string
        reqd
        (Response.create
           ~headers:
             (Headers.of_list
                [ "content-length", "2"; "keep-alive", "timeout=1" ])
           `OK)
        "ok")
  in
  ignore (feed conn get : int);
  let out, _ = output conn in
  check_string
    "the application's hint is kept"
    "HTTP/1.1 200 OK\r\ncontent-length: 2\r\nkeep-alive: timeout=1\r\n\r\nok"
    out;
  let conn =
    Server_connection.create
      ~date:(fun () -> "Sun, 06 Nov 1994 08:49:37 GMT")
      ~keep_alive_timeout:5
      respond
  in
  ignore (feed conn get : int);
  let out, _ = output conn in
  check_string
    "Date then Keep-Alive"
    "HTTP/1.1 200 OK\r\n\
     content-length: 2\r\n\
     Date: Sun, 06 Nov 1994 08:49:37 GMT\r\n\
     Keep-Alive: timeout=5\r\n\
     \r\n\
     ok"
    out;
  match Server_connection.create ~keep_alive_timeout:0 respond with
  | _ -> Alcotest.fail "expected Invalid_argument"
  | exception Invalid_argument _ -> ()

(* Client: the hint of the last response. *)

let client_exchange ?(close = false) headers =
  let client = Client_connection.create () in
  let body =
    Client_connection.request
      client
      (Request.create
         ~headers:
           (Headers.of_list
              ((if close then [ "connection", "close" ] else [])
              @ [ "host", "x" ]))
         `GET
         "/")
      ~error_handler:(fun _ -> ())
      ~response_handler:(fun _ body ->
        Body.Reader.schedule_read
          body
          ~on_eof:ignore
          ~on_error:ignore
          ~on_read:(fun _ ~off:_ ~len:_ -> ()))
  in
  Body.Writer.close body;
  let rec write () =
    match Client_connection.next_write_operation client with
    | `Write iovecs ->
      Client_connection.report_write_result client (`Ok (IOVec.lengthv iovecs));
      write ()
    | `Yield | `Close _ -> ()
  in
  write ();
  let during = Client_connection.reuse_timeout client in
  let response =
    "HTTP/1.1 200 OK\r\ncontent-length: 2\r\n" ^ headers ^ "\r\nok"
  in
  let len = String.length response in
  let bs = Bigstring.of_string response ~off:0 ~len in
  let rec read off =
    if off < len
    then
      match Client_connection.next_read_operation client with
      | `Read ->
        read (off + Client_connection.read client bs ~off ~len:(len - off))
      | `Yield | `Close -> ()
  in
  read 0;
  ignore
    (Client_connection.next_read_operation client : [ `Read | `Yield | `Close ]);
  write ();
  during, Client_connection.reuse_timeout client

let check_reuse msg expected actual =
  Alcotest.(check (option (float 1e-9))) msg expected actual

let test_client_reuse_timeout () =
  let during, after = client_exchange "Keep-Alive: timeout=5, max=99\r\n" in
  check_reuse "none while the request is in progress" None during;
  check_reuse "hint minus 2 s" (Some 3.) after;
  check_reuse
    "at least 0"
    (Some 0.)
    (snd (client_exchange "Keep-Alive: timeout=1\r\n"));
  check_reuse
    "capped at 600 s"
    (Some 598.)
    (snd (client_exchange "Keep-Alive: max=5, timeout=100000\r\n"));
  check_reuse
    "bad values ignored"
    None
    (snd (client_exchange "Keep-Alive: timeout=abc, timeout=-1\r\n"));
  check_reuse
    "a later valid timeout is used"
    (Some 8.)
    (snd
       (client_exchange "Keep-Alive: timeout=x\r\nkeep-alive: TIMEOUT = 10\r\n"));
  check_reuse "no hint" None (snd (client_exchange ""));
  check_reuse
    "closing response"
    None
    (snd (client_exchange "Connection: close\r\nKeep-Alive: timeout=5\r\n"))

let tests =
  [ "drain: idle connection closes", `Quick, test_drain_idle
  ; "drain: before any request", `Quick, test_drain_before_any_request
  ; "drain: head partly read", `Quick, test_drain_mid_head
  ; "drain: body partly read", `Quick, test_drain_mid_body
  ; "drain: streaming response", `Quick, test_drain_streaming_response
  ; "drain: pipelined requests not served", `Quick, test_drain_pipelined
  ; "drain: response already written", `Quick, test_drain_after_response_started
  ; "drain: WebSocket sends 1001", `Quick, test_drain_websocket
  ; "drain: before the 101 is written", `Quick, test_drain_before_101_written
  ; "max_requests_per_connection", `Quick, test_max_requests
  ; "Keep-Alive hint", `Quick, test_keep_alive_hint
  ; "Keep-Alive max counts down", `Quick, test_keep_alive_max
  ; ( "no Keep-Alive hint on a closing response"
    , `Quick
    , test_keep_alive_not_on_close )
  ; ( "Keep-Alive from the application, with Date"
    , `Quick
    , test_keep_alive_app_and_date )
  ; "client: reuse_timeout from Keep-Alive", `Quick, test_client_reuse_timeout
  ; "max_requests_per_connection = 1", `Quick, test_max_requests_one
  ; "max_requests_per_connection < 1", `Quick, test_max_requests_invalid
  ]
