open Ache
open Rfc

let host = Headers.of_list [ "host", "example.com" ]
let client_get target = Request.create ~headers:host `GET target
let client_head target = Request.create ~headers:host `HEAD target

let start_line s =
  match Wire.split_head s with
  | Some (line, _, _) -> line
  | None -> Alcotest.failf "no complete head in %S" s

let starts_with ~prefix s =
  String.length s >= String.length prefix
  && String.sub s 0 (String.length prefix) = prefix

let check_prefix msg prefix s =
  if not (starts_with ~prefix s)
  then Alcotest.failf "%s: expected %S to start with %S" msg s prefix

let ok_handler ex = Server.respond ~body:"ok" `OK ex

let check_refused_version version =
  let s = Server.run ~handler:ok_handler (req ~version "GET" "/") in
  Alcotest.(check int) "request handler not called" 0 (Server.handler_calls s);
  match Server.statuses s with
  | [ 400 ] | [ 505 ] -> ()
  | codes ->
    Alcotest.failf
      "expected 400 or 505, got [%s] in %S"
      (String.concat ";" (List.map string_of_int codes))
      (Server.output s)

let by_target f ex = f ex.Server.request.Request.target ex

let check_no_body_then_next ~status s =
  let out = Server.output s in
  if contains out "hello"
  then Alcotest.failf "body bytes sent after a %d head: %S" status out;
  check_status "no-content response then the next response" [ status; 200 ] s;
  let rs, rest = Wire.parse_responses out in
  Alcotest.(check string) "nothing after the second response" "" rest;
  Alcotest.(check string) "second body" "ok" (List.nth rs 1).Wire.body;
  check_open "connection kept open" s

let no_body_server_test ~status ~streaming =
  let first =
    if streaming
    then (
      fun ex ->
        let w =
          Reqd.respond_with_streaming
            ex.Server.reqd
            (Response.create (Status.of_code status))
        in
        Body.Writer.write_string w "hello";
        Body.Writer.close w)
    else
      fun ex ->
        Reqd.respond_with_string
          ex.Server.reqd
          (Response.create (Status.of_code status))
          "hello"
  in
  let handler = by_target (function "/a" -> first | _ -> ok_handler) in
  let s = Server.run ~handler (get "/a" ^ get "/b") in
  check_no_body_then_next ~status s

let pipelined_client requests =
  let c = Client.create () in
  let exs = List.map (fun r -> Client.request c r) requests in
  c, exs

let check_client_ex msg ~status ~body ex =
  Alcotest.(check (list string)) (msg ^ ": no error") [] ex.Client.errors;
  Alcotest.check int_list (msg ^ ": status") [ status ] (Client.statuses ex);
  Alcotest.(check string) (msg ^ ": body") body ex.body;
  Alcotest.(check bool) (msg ^ ": body complete") true ex.body_eof

let next_ok = "HTTP/1.1 200 OK\r\ncontent-length: 2\r\n\r\nok"

let client_no_body_test ~first_request ~first_response ~status =
  let c, exs = pipelined_client [ first_request; client_get "/b" ] in
  Client.feed c (first_response ^ next_ok);
  match exs with
  | [ a; b ] ->
    check_client_ex "first response" ~status ~body:"" a;
    check_client_ex "second response" ~status:200 ~body:"ok" b
  | _ -> assert false

let version_tests =
  [ test
      "§6.2 server SHOULD: response to an HTTP/1.1 request is HTTP/1.1"
      (fun () ->
         let s = Server.run ~handler:ok_handler (get "/") in
         check_prefix
           "status line"
           "HTTP/1.1 200 "
           (start_line (Server.output s)))
  ; test
      "§6.2 server SHOULD: response to an HTTP/1.0 request is HTTP/1.1"
      (fun () ->
         let s =
           Server.run ~handler:ok_handler (req ~version:"HTTP/1.0" "GET" "/")
         in
         check_prefix
           "status line"
           "HTTP/1.1 200 "
           (start_line (Server.output s)))
  ; test
      "§6.2 server SHOULD: response to an HTTP/1.2 request is HTTP/1.1"
      (fun () ->
         let s =
           Server.run ~handler:ok_handler (req ~version:"HTTP/1.2" "GET" "/")
         in
         check_prefix
           "status line"
           "HTTP/1.1 200 "
           (start_line (Server.output s)))
  ; test "§6.2 server MUST NOT: library error response is HTTP/1.1" (fun () ->
      let s =
        Server.run
          (req
             ~headers:[ "Host", "example.com"; "Content-Length", "-1" ]
             "POST"
             "/")
      in
      check_status "error response" [ 400 ] s;
      check_prefix "status line" "HTTP/1.1 400 " (start_line (Server.output s)))
  ; test
      "§6.2 both SHOULD: HTTP/1.2 request is processed as HTTP/1.1"
      (fun () ->
         let s =
           Server.run
             ~handler:ok_handler
             (req ~version:"HTTP/1.2" "GET" "/a" ^ get "/b")
         in
         Alcotest.(check int) "both requests handled" 2 (Server.handler_calls s);
         check_status "two responses" [ 200; 200 ] s;
         check_open "connection persistent as in HTTP/1.1" s)
  ; test
      "§6.2 client SHOULD: HTTP/1.2 response is processed as HTTP/1.1"
      (fun () ->
         let c, exs = pipelined_client [ client_get "/a"; client_get "/b" ] in
         let r = "HTTP/1.2 200 OK\r\ncontent-length: 2\r\n\r\nok" in
         Client.feed c (r ^ r);
         List.iter (check_client_ex "response" ~status:200 ~body:"ok") exs)
  ; test "§6.2 client SHOULD: request is written as HTTP/1.1" (fun () ->
      let c = Client.create () in
      let _ = Client.request c (client_get "/") in
      check_prefix "request line" "GET / HTTP/1.1\r\n" (Client.written c))
  ; test "§2.5 server DEF: version is one digit, a period, one digit" (fun () ->
      List.iter
        (fun version ->
           check_bad_request
             ~msg:version
             (Server.run ~handler:ok_handler (req ~version "GET" "/")))
        [ "HTTP/1.10"; "HTTP/11.1"; "HTTP/1"; "HTTP/1." ])
  ; test
      "§2.5 server DEF: HTTP/2.0 request is not processed as HTTP/1.x"
      (fun () -> check_refused_version "HTTP/2.0")
  ; test
      "§2.5 server DEF: HTTP/0.9 request is not processed as HTTP/1.x"
      (fun () -> check_refused_version "HTTP/0.9")
  ; test
      "§2.5 server DEF: HTTP/9.9 request is not processed as HTTP/1.x"
      (fun () -> check_refused_version "HTTP/9.9")
  ; test
      "§2.5 client DEF: HTTP/2.0 response is not processed as HTTP/1.x"
      (fun () ->
         let c = Client.create () in
         let ex = Client.request c (client_get "/") in
         Client.feed c "HTTP/2.0 200 OK\r\ncontent-length: 2\r\n\r\nok";
         Alcotest.check int_list "no response delivered" [] (Client.statuses ex);
         Alcotest.(check bool) "error reported" true (ex.errors <> []))
  ]

let retain_request_tests =
  [ test
      "§6 client MUST: HEAD response with Content-Length ends after the head"
      (fun () ->
         client_no_body_test
           ~first_request:(client_head "/a")
           ~first_response:"HTTP/1.1 200 OK\r\ncontent-length: 5\r\n\r\n"
           ~status:200)
  ; test
      "§6 client MUST: HEAD response with chunked framing ends after the head"
      (fun () ->
         client_no_body_test
           ~first_request:(client_head "/a")
           ~first_response:
             "HTTP/1.1 200 OK\r\ntransfer-encoding: chunked\r\n\r\n"
           ~status:200)
  ]

let content_tests =
  [ test
      "§6.4.1 server DEF: no body bytes in a 204 from respond_with_string"
      (fun () -> no_body_server_test ~status:204 ~streaming:false)
  ; test
      "§6.4.1 server DEF: no body bytes in a 304 from respond_with_string"
      (fun () -> no_body_server_test ~status:304 ~streaming:false)
  ; test "§6.4.1 server DEF: no body bytes in a streamed 204" (fun () ->
      no_body_server_test ~status:204 ~streaming:true)
  ; test "§6.4.1 server DEF: no body bytes in a streamed 304" (fun () ->
      no_body_server_test ~status:304 ~streaming:true)
  ; test "§6.4.1 server DEF: no body bytes after a 100 head" (fun () ->
      let handler ex =
        match
          Reqd.respond_with_string
            ex.Server.reqd
            (Response.create `Continue)
            "hello"
        with
        | () -> ()
        | exception (Invalid_argument _ | Failure _) -> ()
      in
      let s = Server.run ~handler (get "/") in
      let out = Server.output s in
      if contains out "hello"
      then Alcotest.failf "body bytes sent after a 100 head: %S" out)
  ; test "§6.4.1 server DEF: a 200 response includes its content" (fun () ->
      let s =
        Server.run ~handler:(Server.respond ~body:"hello" `OK) (get "/")
      in
      Alcotest.(check string) "body" "hello" (Server.response s).Wire.body)
  ; test "§6.4.1 client DEF: 204 response ends after the head" (fun () ->
      client_no_body_test
        ~first_request:(client_get "/a")
        ~first_response:"HTTP/1.1 204 No Content\r\n\r\n"
        ~status:204)
  ; test
      "§6.4.1 client DEF: 304 response with Content-Length ends after the head"
      (fun () ->
         client_no_body_test
           ~first_request:(client_get "/a")
           ~first_response:
             "HTTP/1.1 304 Not Modified\r\ncontent-length: 5\r\n\r\n"
           ~status:304)
  ; test "§6.4.1 client DEF: 100 response ends after the head" (fun () ->
      let c = Client.create () in
      let ex = Client.request c (client_get "/") in
      Client.feed c ("HTTP/1.1 100 Continue\r\n\r\n" ^ next_ok);
      check_client_ex "final response" ~status:200 ~body:"ok" ex)
  ]

let trailer_tests =
  [ test
      "§6.5.1 server MUST NOT: trailer not merged into request headers"
      (fun () ->
         let s =
           Server.run
             (req
                ~headers:
                  [ "Host", "example.com"; "Transfer-Encoding", "chunked" ]
                ~body:"3\r\nabc\r\n0\r\nX-Trailer: 1\r\n\r\n"
                "POST"
                "/")
         in
         check_status "echo response" [ 200 ] s;
         Alcotest.(check string) "body" "abc" (Server.response s).Wire.body;
         match s.exchanges with
         | [ ex ] ->
           Alcotest.(check bool) "body complete" true ex.body_complete;
           Alcotest.(check (option string))
             "trailer not in headers"
             None
             (Headers.get ex.request.headers "x-trailer")
         | _ -> Alcotest.fail "expected one request")
  ; test
      "§6.5.1 client MUST NOT: trailer not merged into response headers"
      (fun () ->
         let c = Client.create () in
         let ex = Client.request c (client_get "/") in
         Client.feed
           c
           "HTTP/1.1 200 OK\r\n\
            transfer-encoding: chunked\r\n\
            \r\n\
            3\r\n\
            abc\r\n\
            0\r\n\
            X-Trailer: 1\r\n\
            \r\n";
         check_client_ex "response" ~status:200 ~body:"abc" ex;
         match ex.responses with
         | [ r ] ->
           Alcotest.(check (option string))
             "trailer not in headers"
             None
             (Headers.get r.headers "x-trailer")
         | _ -> Alcotest.fail "expected one response")
  ]

let date_tests =
  [ test "§6.6.1 server MUST NOT: library adds no Date field" (fun () ->
      let check_no_date msg s =
        if contains (String.lowercase_ascii (Server.output s)) "date:"
        then Alcotest.failf "%s: Date field written: %S" msg (Server.output s)
      in
      let ok = Server.run ~handler:(Server.respond ~body:"" `OK) (get "/") in
      check_status "application response" [ 200 ] ok;
      check_no_date "application response" ok;
      let error =
        Server.run
          (req
             ~headers:[ "Host", "example.com"; "Content-Length", "-1" ]
             "POST"
             "/")
      in
      check_status "error response" [ 400 ] error;
      check_no_date "error response" error)
  ]

let tests =
  version_tests
  @ retain_request_tests
  @ content_tests
  @ trailer_tests
  @ date_tests
