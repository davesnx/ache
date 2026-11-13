open Ache
open Rfc

let host = "Host", "example.com"
let te v = "Transfer-Encoding", v
let cl v = "Content-Length", v
let next = get "/next"

let post ?(version = "HTTP/1.1") headers body =
  req ~version ~headers:(host :: headers) ~body "POST" "/"

let bodies s =
  List.map (fun (ex : Server.exchange) -> ex.body) s.Server.exchanges

let run_split ?handler input =
  let s = Server.create ?handler () in
  String.iter (fun c -> Server.feed s (String.make 1 c)) input;
  s

let read_only ex = Server.read_body ex ~on_complete:ignore
let by_target f ex = f ex.Server.request.Request.target ex
let codes l = String.concat "; " (List.map string_of_int l)

let check_echoed msg ~body s =
  check_status (msg ^ ": statuses") [ 200; 200 ] s;
  Alcotest.(check (list string)) (msg ^ ": bodies") [ body; "" ] (bodies s);
  Alcotest.(check (list string))
    (msg ^ ": targets")
    [ "/"; "/next" ]
    (targets s);
  (match Wire.parse_responses (Server.output s) with
  | r :: _, _ -> Alcotest.(check string) (msg ^ ": echoed") body r.Wire.body
  | [], _ -> Alcotest.failf "%s: no response" msg);
  check_open (msg ^ ": connection open") s

let accept_body ?(headers = [ te "chunked" ]) chunks body =
  let input = post headers chunks ^ next in
  check_echoed (String.escaped chunks) ~body (Server.run input);
  check_echoed (String.escaped chunks ^ " byte by byte") ~body (run_split input)

let check_body_rejected msg s =
  check_status (msg ^ ": 400") [ 400 ] s;
  check_not_dispatched "/next" s;
  check_closed (msg ^ ": connection closed") s

let reject_chunked chunks =
  let input = post [ te "chunked" ] chunks ^ next in
  let msg = String.escaped chunks in
  check_body_rejected msg (Server.run ~handler:read_only input);
  check_body_rejected
    (msg ^ " byte by byte")
    (run_split ~handler:read_only input)

let check_head_rejected ?(allowed = [ 400 ]) msg s =
  (match Server.statuses s with
  | [ c ] when List.mem c allowed -> ()
  | l ->
    Alcotest.failf
      "%s: expected one of [%s], got [%s] in %S"
      msg
      (codes allowed)
      (codes l)
      (Server.output s));
  Alcotest.(check int) (msg ^ ": handler not called") 0 (Server.handler_calls s);
  check_not_dispatched "/next" s;
  check_closed (msg ^ ": connection closed") s

let client_get target =
  Request.create
    ~headers:(Headers.of_list [ "host", "example.com" ])
    `GET
    target

let client_response ?(meth = `GET) raw =
  let c = Client.create () in
  let ex =
    Client.request
      c
      (Request.create
         ~headers:(Headers.of_list [ "host", "example.com" ])
         meth
         "/")
  in
  Client.feed c raw;
  c, ex

let chunked_head = "HTTP/1.1 200 OK\r\ntransfer-encoding: chunked\r\n\r\n"
let next_ok = "HTTP/1.1 200 OK\r\ncontent-length: 2\r\n\r\nok"

let check_client_ok msg ~body ex =
  Alcotest.(check (list string)) (msg ^ ": no error") [] ex.Client.errors;
  Alcotest.check int_list (msg ^ ": status") [ 200 ] (Client.statuses ex);
  Alcotest.(check string) (msg ^ ": body") body ex.body;
  Alcotest.(check bool) (msg ^ ": body complete") true ex.body_eof

let client_accept chunks body =
  let msg = String.escaped chunks in
  let _, ex = client_response (chunked_head ^ chunks) in
  check_client_ok msg ~body ex;
  let c, ex = client_response "" in
  String.iter
    (fun ch -> Client.feed c (String.make 1 ch))
    (chunked_head ^ chunks);
  check_client_ok (msg ^ " byte by byte") ~body ex

let check_client_error msg c ex =
  Alcotest.(check bool) (msg ^ ": error reported") true (ex.Client.errors <> []);
  Alcotest.(check bool) (msg ^ ": connection closed") true (Client.is_closed c)

let client_reject chunks =
  let msg = String.escaped chunks in
  let c, ex = client_response (chunked_head ^ chunks ^ next_ok) in
  check_client_error msg c ex

let pipelined requests raw =
  let c = Client.create () in
  let exs = List.map (fun r -> Client.request c r) requests in
  Client.feed c raw;
  c, exs

let head_of s =
  match Wire.split_head s with
  | Some (start_line, headers, rest) ->
    { Wire.start_line; headers; body = rest }
  | None -> Alcotest.failf "no complete head in %S" s

let stream ?(headers = []) ?(writes = []) status ex =
  let w =
    Reqd.respond_with_streaming
      ex.Server.reqd
      (Response.create ~headers:(Headers.of_list headers) status)
  in
  List.iter (Body.Writer.write_string w) writes;
  Body.Writer.close w

let count_sub s sub =
  let n = String.length sub in
  let rec go i acc =
    match Wire.index_from s i sub with
    | None -> acc
    | Some j -> go (j + n) (acc + 1)
  in
  go 0 0

let check_no_te msg s =
  if contains (String.lowercase_ascii (Server.output s)) "transfer-encoding"
  then Alcotest.failf "%s: Transfer-Encoding written: %S" msg (Server.output s)

let ok_next handler =
  by_target (function "/next" -> Server.respond ~body:"ok" `OK | _ -> handler)

let check_closes_after_one msg s =
  (match Server.statuses s with
  | [ _ ] -> ()
  | l -> Alcotest.failf "%s: expected one response, got [%s]" msg (codes l));
  check_closed (msg ^ ": connection closed") s

let request_framing_tests =
  [ test
      "§6.3 server DEF: Transfer-Encoding chunked frames the body by chunks"
      (fun () ->
         accept_body "3\r\nabc\r\n0\r\n\r\n" "abc";
         accept_body "1\r\na\r\n2\r\nbc\r\n0\r\n\r\n" "abc")
  ; test
      "§7 server DEF: transfer-coding name chunked is case-insensitive"
      (fun () ->
         List.iter
           (fun v ->
              accept_body ~headers:[ te v ] "3\r\nabc\r\n0\r\n\r\n" "abc")
           [ "Chunked"; "CHUNKED"; "cHuNkEd" ])
  ; test
      "§6.3 server MUST: 400 then close when chunked is not the final coding"
      (fun () ->
         List.iter
           (fun values ->
              let s =
                Server.run
                  (post (List.map te values) "3\r\nabc\r\n0\r\n\r\n" ^ next)
              in
              check_head_rejected
                ~allowed:[ 400; 501 ]
                (String.concat " | " values)
                s)
           [ [ "gzip" ]
           ; [ "identity" ]
           ; [ "chunked, gzip" ]
           ; [ "chunked"; "gzip" ]
           ; [ "chunked, chunked, identity" ]
           ])
  ; test
      "§6.1 server SHOULD: unknown coding before chunked gets 400 or 501"
      (fun () ->
         List.iter
           (fun values ->
              let s =
                Server.run
                  (post (List.map te values) "3\r\nabc\r\n0\r\n\r\n" ^ next)
              in
              check_head_rejected
                ~allowed:[ 400; 501 ]
                (String.concat " | " values)
                s)
           [ [ "gzip, chunked" ]; [ "gzip"; "chunked" ] ])
  ; test "§7.1 server SHOULD: chunked with parameters is an error" (fun () ->
      List.iter
        (fun v ->
           let s = Server.run (post [ te v ] "3\r\nabc\r\n0\r\n\r\n" ^ next) in
           check_head_rejected ~allowed:[ 400; 501 ] v s)
        [ "chunked;foo=bar"; "chunked; q=1" ])
  ; test
      "§6.1 server MUST: close after a request with both Content-Length and \
       Transfer-Encoding"
      (fun () ->
         List.iter
           (fun headers ->
              let s =
                Server.run (post headers "3\r\nabc\r\n0\r\n\r\n" ^ next)
              in
              (match Server.statuses s with
              | [ 400 ] ->
                Alcotest.(check int)
                  "handler not called"
                  0
                  (Server.handler_calls s)
              | [ 200 ] ->
                Alcotest.(check (list string))
                  "body read by Transfer-Encoding"
                  [ "abc" ]
                  (bodies s)
              | l ->
                Alcotest.failf "expected [400] or [200], got [%s]" (codes l));
              check_not_dispatched "/next" s;
              check_closed "connection closed" s)
           [ [ cl "3"; te "chunked" ]
           ; [ te "chunked"; cl "3" ]
           ; [ cl "0"; te "chunked" ]
           ; [ cl "100"; te "chunked" ]
           ])
  ; test
      "§6.1 server MUST: HTTP/1.0 request with Transfer-Encoding closes the \
       connection"
      (fun () ->
         List.iter
           (fun extra ->
              let s =
                Server.run
                  (req
                     ~version:"HTTP/1.0"
                     ~headers:
                       ([ host; "Connection", "keep-alive"; te "chunked" ]
                       @ extra)
                     ~body:"3\r\nabc\r\n0\r\n\r\n"
                     "POST"
                     "/"
                  ^ req
                      ~version:"HTTP/1.0"
                      ~headers:[ host; "Connection", "keep-alive" ]
                      "GET"
                      "/next")
              in
              (match Server.statuses s with
              | [ 400 ] ->
                Alcotest.(check int)
                  "handler not called"
                  0
                  (Server.handler_calls s)
              | [ 200 ] ->
                Alcotest.(check (list string))
                  "body read by Transfer-Encoding"
                  [ "abc" ]
                  (bodies s)
              | l ->
                Alcotest.failf "expected [400] or [200], got [%s]" (codes l));
              check_not_dispatched "/next" s;
              check_closed "connection closed" s)
           [ []; [ cl "3" ] ])
  ; test
      "§6.3 server MUST: 400 then close for an invalid Content-Length"
      (fun () ->
         List.iter
           (fun v ->
              check_head_rejected v (Server.run (post [ cl v ] "abc" ^ next)))
           [ "abc"; "-1"; ""; "1 2"; "3, 4"; "3,4"; "3;"; "3 3" ])
  ; test
      "§6.3 server MUST: 400 then close for a Content-Length that is not \
       1*DIGIT"
      (fun () ->
         List.iter
           (fun v ->
              check_head_rejected v (Server.run (post [ cl v ] "abc" ^ next)))
           [ "0x3"; "+3"; "0b11" ])
  ; test
      "§6.3 server MAY: Content-Length 3,3 is rejected or read as 3"
      (fun () ->
         let s = Server.run (post [ cl "3,3" ] "abc" ^ next) in
         match Server.statuses s with
         | [ 200; 200 ] -> check_echoed "collapsed" ~body:"abc" s
         | _ -> check_head_rejected "rejected" s)
  ; test
      "§6.3 server DEF: Content-Length frames exactly that many octets"
      (fun () ->
         accept_body ~headers:[ cl "3" ] "abc" "abc";
         accept_body ~headers:[ cl "0" ] "" "";
         accept_body ~headers:[ cl "003" ] "abc" "abc")
  ; test
      "§6.3 server DEF: request without Content-Length or Transfer-Encoding \
       has no body"
      (fun () ->
         accept_body ~headers:[] "" "";
         let s = Server.run (get "/" ^ next) in
         check_status "two responses" [ 200; 200 ] s;
         Alcotest.(check (list string)) "targets" [ "/"; "/next" ] (targets s))
  ; test
      "§6 server DEF: a GET with Transfer-Encoding chunked has a body"
      (fun () ->
         let s =
           Server.run
             (req
                ~headers:[ host; te "chunked" ]
                ~body:"3\r\nabc\r\n0\r\n\r\n"
                "GET"
                "/"
             ^ next)
         in
         check_echoed "GET" ~body:"abc" s)
  ]

let chunk_accept_cases =
  [ "0\r\n\r\n", ""
  ; "000\r\n\r\n", ""
  ; "a\r\n0123456789\r\n0\r\n\r\n", "0123456789"
  ; "A\r\n0123456789\r\n0\r\n\r\n", "0123456789"
  ; "0003\r\nabc\r\n0\r\n\r\n", "abc"
  ; "000000000000003\r\nabc\r\n0\r\n\r\n", "abc"
  ; "0000000000000003\r\nabc\r\n0\r\n\r\n", "abc"
  ]

let ext_accept_cases =
  [ "3;a\r\nabc\r\n0\r\n\r\n"
  ; "3;a=b\r\nabc\r\n0\r\n\r\n"
  ; "3;a=\"q\\\"x\"\r\nabc\r\n0\r\n\r\n"
  ; "3;a=\"\"\r\nabc\r\n0\r\n\r\n"
  ; "3 ; a = b\r\nabc\r\n0\r\n\r\n"
  ; "3\t;\ta\t=\tb\r\nabc\r\n0\r\n\r\n"
  ; "3;a;b=c\r\nabc\r\n0\r\n\r\n"
  ; "3\r\nabc\r\n0;a=b\r\n\r\n"
  ]

let trailer_accept_cases =
  [ "3\r\nabc\r\n0\r\nX-T: 1\r\n\r\n"
  ; "3\r\nabc\r\n0\r\nX-A: 1\r\nX-B: 2\r\nX-A: 3\r\n\r\n"
  ; "3\r\nabc\r\n0\r\nX-T:\r\n\r\n"
  ]

let size_reject_cases =
  [ "g\r\nabc\r\n0\r\n\r\n"
  ; "\r\nabc\r\n0\r\n\r\n"
  ; " 3\r\nabc\r\n0\r\n\r\n"
  ; "3 \r\nabc\r\n0\r\n\r\n"
  ; "-3\r\nabc\r\n0\r\n\r\n"
  ; "0x3\r\nabc\r\n0\r\n\r\n"
  ; "+3\r\nabc\r\n0\r\n\r\n"
  ]

let large_reject_cases =
  [ "1000000000000000\r\nabc\r\n0\r\n\r\n"
  ; "ffffffffffffffff\r\nabc\r\n0\r\n\r\n"
  ; "10000000000000003\r\nabc\r\n0\r\n\r\n"
  ]

let ext_reject_cases =
  [ "3;\r\nabc\r\n0\r\n\r\n"
  ; "3;=b\r\nabc\r\n0\r\n\r\n"
  ; "3;a=\r\nabc\r\n0\r\n\r\n"
  ; "3;a=\"x\r\nabc\r\n0\r\n\r\n"
  ; "3;a b\r\nabc\r\n0\r\n\r\n"
  ; "3x\r\nabc\r\n0\r\n\r\n"
  ]

let data_reject_cases =
  [ "3\r\nabcd\r\n0\r\n\r\n"; "3\r\nab\r\n0\r\n\r\n"; "3\r\nabc0\r\n\r\n" ]

let bare_lf_cases =
  [ "3\nabc\r\n0\r\n\r\n"
  ; "3\r\nabc\n0\r\n\r\n"
  ; "3\r\nabc\r\n0\n\r\n"
  ; "3\r\nabc\r\n0\r\n\n"
  ; "3\r\nabc\r\n0\r\nX-T: 1\n\r\n"
  ; "3\nabc\n0\n\n"
  ]

let trailer_reject_cases =
  [ "3\r\nabc\r\n0\r\nnocolon\r\n\r\n"; "3\r\nabc\r\n0\r\nX T: 1\r\n\r\n" ]

let chunked_tests =
  [ test "§7.1 server MUST: chunk sizes in hex with leading zeros" (fun () ->
      List.iter (fun (c, b) -> accept_body c b) chunk_accept_cases)
  ; test "§7.1 client MUST: chunk sizes in hex with leading zeros" (fun () ->
      List.iter (fun (c, b) -> client_accept c b) chunk_accept_cases)
  ; test "§7.1 server DEF: 400 then close for an invalid chunk-size" (fun () ->
      List.iter reject_chunked size_reject_cases)
  ; test "§7.1 client DEF: error and close for an invalid chunk-size" (fun () ->
      List.iter client_reject size_reject_cases)
  ; test
      "§7.1 server MUST: chunk-size too large for 64 bits is not read as small"
      (fun () -> List.iter reject_chunked large_reject_cases)
  ; test
      "§7.1 client MUST: chunk-size too large for 64 bits is not read as small"
      (fun () -> List.iter client_reject large_reject_cases)
  ; test
      "§7.1.1 server MUST: unrecognized chunk extensions are ignored"
      (fun () -> List.iter (fun c -> accept_body c "abc") ext_accept_cases)
  ; test
      "§7.1.1 client MUST: unrecognized chunk extensions are ignored"
      (fun () -> List.iter (fun c -> client_accept c "abc") ext_accept_cases)
  ; test "§7.1.1 server DEF: 400 then close for an invalid chunk-ext" (fun () ->
      List.iter reject_chunked ext_reject_cases)
  ; test
      "§7.1.1 client DEF: error and close for an invalid chunk-ext"
      (fun () -> List.iter client_reject ext_reject_cases)
  ; test
      "§7.1 server DEF: 400 then close when chunk-data is not followed by CRLF"
      (fun () -> List.iter reject_chunked data_reject_cases)
  ; test
      "§7.1 client DEF: error and close when chunk-data is not followed by CRLF"
      (fun () -> List.iter client_reject data_reject_cases)
  ; test
      "§7.1 server DEF: 400 then close for a bare LF in the chunked body"
      (fun () -> List.iter reject_chunked bare_lf_cases)
  ; test
      "§7.1 client DEF: error and close for a bare LF in the chunked body"
      (fun () -> List.iter client_reject bare_lf_cases)
  ; test "§7.1.2 server MAY: trailer fields are read and discarded" (fun () ->
      List.iter
        (fun c ->
           accept_body c "abc";
           let s = Server.run (post [ te "chunked" ] c) in
           match Server.requests s with
           | [ r ] ->
             List.iter
               (fun n ->
                  Alcotest.(check (option string))
                    (n ^ " not in headers")
                    None
                    (Headers.get r.headers n))
               [ "x-t"; "x-a"; "x-b" ]
           | _ -> Alcotest.fail "expected one request")
        trailer_accept_cases)
  ; test "§7.1.2 client MAY: trailer fields are read and discarded" (fun () ->
      List.iter
        (fun c ->
           client_accept c "abc";
           let _, ex = client_response (chunked_head ^ c) in
           match ex.responses with
           | [ r ] ->
             List.iter
               (fun n ->
                  Alcotest.(check (option string))
                    (n ^ " not in headers")
                    None
                    (Headers.get r.headers n))
               [ "x-t"; "x-a"; "x-b" ]
           | _ -> Alcotest.fail "expected one response")
        trailer_accept_cases)
  ; test
      "§7.1.2 server DEF: 400 then close for an invalid trailer field"
      (fun () -> List.iter reject_chunked trailer_reject_cases)
  ; test
      "§7.1.2 client DEF: error and close for an invalid trailer field"
      (fun () -> List.iter client_reject trailer_reject_cases)
  ; test
      "§7.1 client DEF: chunked response followed by a pipelined response"
      (fun () ->
         let c, exs =
           pipelined
             [ client_get "/a"; client_get "/b" ]
             (chunked_head ^ "3\r\nabc\r\n0\r\nX-T: 1\r\n\r\n" ^ next_ok)
         in
         Alcotest.(check bool) "open" false (Client.is_closed c);
         match exs with
         | [ a; b ] ->
           check_client_ok "first" ~body:"abc" a;
           check_client_ok "second" ~body:"ok" b
         | _ -> assert false)
  ]

let server_response_tests =
  [ test
      "§6.1 server MUST NOT: app chunked Transfer-Encoding is applied once"
      (fun () ->
         let s =
           Server.run
             ~handler:
               (stream
                  ~headers:[ "transfer-encoding", "chunked" ]
                  ~writes:[ "hello" ]
                  `OK)
             (get "/")
         in
         let out = String.lowercase_ascii (Server.output s) in
         Alcotest.(check int)
           "one transfer-encoding field"
           1
           (count_sub out "transfer-encoding");
         Alcotest.(check string)
           "decoded body"
           "hello"
           (Server.response s).Wire.body;
         check_open "connection open" s)
  ; test
      "§6.1 server MUST: response with a non-chunked final coding closes the \
       connection"
      (fun () ->
         let s =
           Server.run
             ~handler:
               (ok_next
                  (stream
                     ~headers:[ "transfer-encoding", "gzip" ]
                     ~writes:[ "xyz" ]
                     `OK))
             (get "/" ^ next)
         in
         check_closes_after_one "gzip" s)
  ; test
      "§6.1 server MUST NOT: library adds no Transfer-Encoding to 1xx or 204"
      (fun () ->
         check_no_te "100" (Server.run ~handler:Server.send_continue (get "/"));
         check_no_te
           "204 respond_with_string"
           (Server.run ~handler:(Server.respond_raw `No_content) (get "/"));
         check_no_te
           "204 respond_with_streaming"
           (Server.run ~handler:(stream `No_content) (get "/")))
  ; test
      "§6.1 server MUST NOT: no Transfer-Encoding in a streamed response to \
       HTTP/1.0"
      (fun () ->
         let s =
           Server.run
             ~handler:(stream ~writes:[ "hello" ] `OK)
             (req ~version:"HTTP/1.0" "GET" "/")
         in
         check_no_te "HTTP/1.0" s;
         Alcotest.(check string) "body" "hello" (head_of (Server.output s)).body;
         check_closed "connection closed" s)
  ; test
      "§6.1 server MUST NOT: streamed response to an HTTP/1.0 keep-alive \
       request is close-delimited"
      (fun () ->
         let ka = [ host; "Connection", "keep-alive" ] in
         let s =
           Server.run
             ~handler:(ok_next (stream ~writes:[ "hello" ] `OK))
             (req ~version:"HTTP/1.0" ~headers:ka "GET" "/"
             ^ req ~version:"HTTP/1.0" ~headers:ka "GET" "/next")
         in
         check_no_te "HTTP/1.0" s;
         check_closes_after_one "keep-alive" s)
  ; test
      "§6.1 server MAY: Transfer-Encoding from the app is kept on HEAD and 304 \
       without body bytes"
      (fun () ->
         List.iter
           (fun (label, meth, status) ->
              let s =
                Server.run
                  ~handler:
                    (stream ~headers:[ "transfer-encoding", "chunked" ] status)
                  (req meth "/")
              in
              let m = head_of (Server.output s) in
              Alcotest.(check (option string))
                (label ^ ": transfer-encoding")
                (Some "chunked")
                (Wire.header m "transfer-encoding");
              Alcotest.(check string) (label ^ ": no body bytes") "" m.body)
           [ "HEAD", "HEAD", `OK; "304", "GET", `Not_modified ])
  ; test
      "§6.2 server MUST NOT: library adds no Content-Length when the app sets \
       Transfer-Encoding"
      (fun () ->
         let check label s =
           let m = head_of (Server.output s) in
           Alcotest.(check (option string))
             (label ^ ": content-length")
             None
             (Wire.header m "content-length");
           Alcotest.(check string)
             (label ^ ": body")
             "abc"
             (Server.response s).Wire.body
         in
         let rejected = ref false in
         let s =
           Server.run
             ~handler:(fun ex ->
               match
                 Server.respond_raw
                   ~headers:[ "transfer-encoding", "chunked" ]
                   ~body:"3\r\nabc\r\n0\r\n\r\n"
                   `OK
                   ex
               with
               | () -> ()
               | exception Invalid_argument _ -> rejected := true)
             (get "/")
         in
         Alcotest.(check bool)
           "respond_with_string with Transfer-Encoding is rejected"
           true
           !rejected;
         Alcotest.(check string) "nothing written" "" (Server.output s);
         check
           "respond_with_streaming"
           (Server.run
              ~handler:
                (stream
                   ~headers:[ "transfer-encoding", "chunked" ]
                   ~writes:[ "abc" ]
                   `OK)
              (get "/")))
  ; test
      "§6.3 server SHOULD: streamed response without a length is chunked or \
       closes the connection"
      (fun () ->
         let s =
           Server.run
             ~handler:(ok_next (stream ~writes:[ "hello" ] `OK))
             (get "/" ^ next)
         in
         let m = head_of (Server.output s) in
         match Wire.header m "transfer-encoding" with
         | Some "chunked" -> ()
         | _ -> check_closes_after_one "close-delimited" s)
  ]

let client_framing_tests =
  [ test
      "§6.3 client DEF: 204 and 304 with Transfer-Encoding chunked end after \
       the head"
      (fun () ->
         List.iter
           (fun (status, line) ->
              let _, exs =
                pipelined
                  [ client_get "/a"; client_get "/b" ]
                  (line ^ "\r\ntransfer-encoding: chunked\r\n\r\n" ^ next_ok)
              in
              match exs with
              | [ a; b ] ->
                Alcotest.(check (list string)) "no error" [] a.errors;
                Alcotest.check
                  int_list
                  "first status"
                  [ status ]
                  (Client.statuses a);
                Alcotest.(check string) "first body" "" a.body;
                Alcotest.(check bool) "first complete" true a.body_eof;
                check_client_ok "second" ~body:"ok" b
              | _ -> assert false)
           [ 204, "HTTP/1.1 204 No Content"; 304, "HTTP/1.1 304 Not Modified" ])
  ; test
      "§6.3 client DEF: HEAD response with Content-Length and \
       Transfer-Encoding ends after the head"
      (fun () ->
         let _, exs =
           pipelined
             [ Request.create
                 ~headers:(Headers.of_list [ "host", "example.com" ])
                 `HEAD
                 "/a"
             ; client_get "/b"
             ]
             ("HTTP/1.1 200 OK\r\n\
               content-length: 5\r\n\
               transfer-encoding: chunked\r\n\
               \r\n"
             ^ next_ok)
         in
         match exs with
         | [ a; b ] ->
           Alcotest.(check string) "first body" "" a.body;
           Alcotest.(check bool) "first complete" true a.body_eof;
           check_client_ok "second" ~body:"ok" b
         | _ -> assert false)
  ; test
      "§6.3 client MUST: Transfer-Encoding in a 2xx to CONNECT is ignored"
      (fun () ->
         let c, ex =
           client_response
             ~meth:`CONNECT
             "HTTP/1.1 200 OK\r\n\
              transfer-encoding: chunked\r\n\
              \r\n\
              zz not chunked"
         in
         Alcotest.check int_list "one 200" [ 200 ] (Client.statuses ex);
         Alcotest.(check (list string)) "no error" [] ex.errors;
         Client.feed c "\r\nmore tunnel bytes\r\n";
         Alcotest.(check (list string)) "still no error" [] ex.errors;
         Alcotest.(check bool)
           "reader not closed"
           false
           (Client.read_op c = `Close))
  ; test
      "§6.3 client DEF: Transfer-Encoding overrides Content-Length"
      (fun () ->
         let _, ex =
           client_response
             "HTTP/1.1 200 OK\r\n\
              content-length: 2\r\n\
              transfer-encoding: chunked\r\n\
              \r\n\
              3\r\n\
              abc\r\n\
              0\r\n\
              \r\n"
         in
         check_client_ok "TE wins" ~body:"abc" ex)
  ; test
      "§6.3 client MUST: connection not reused after a response with both \
       Content-Length and Transfer-Encoding"
      (fun () ->
         let c, ex =
           client_response
             "HTTP/1.1 200 OK\r\n\
              content-length: 2\r\n\
              transfer-encoding: chunked\r\n\
              \r\n\
              3\r\n\
              abc\r\n\
              0\r\n\
              \r\n"
         in
         check_client_ok "TE wins" ~body:"abc" ex;
         Alcotest.(check bool) "connection closed" true (Client.is_closed c))
  ; test "§6.3 client DEF: chunked is matched case-insensitively" (fun () ->
      List.iter
        (fun v ->
           let _, ex =
             client_response
               ("HTTP/1.1 200 OK\r\ntransfer-encoding: "
               ^ v
               ^ "\r\n\r\n3\r\nabc\r\n0\r\n\r\n")
           in
           check_client_ok v ~body:"abc" ex)
        [ "Chunked"; "CHUNKED" ])
  ; test
      "§6.3 client DEF: response with a non-chunked final coding is read until \
       close"
      (fun () ->
         List.iter
           (fun (label, fields, body) ->
              let c, ex =
                client_response ("HTTP/1.1 200 OK\r\n" ^ fields ^ "\r\n" ^ body)
              in
              Alcotest.(check bool)
                (label ^ ": not complete before EOF")
                false
                ex.body_eof;
              Client.feed c "more";
              Client.close_input c;
              check_client_ok label ~body:(body ^ "more") ex;
              Alcotest.(check bool)
                (label ^ ": closed")
                true
                (Client.is_closed c))
           [ "gzip", "transfer-encoding: gzip\r\n", "abc"
           ; ( "chunked, gzip"
             , "transfer-encoding: chunked, gzip\r\n"
             , "3\r\nabc\r\n0\r\n\r\n" )
           ; ( "chunked then gzip lines"
             , "transfer-encoding: chunked\r\ntransfer-encoding: gzip\r\n"
             , "3\r\nabc\r\n0\r\n\r\n" )
           ])
  ; test
      "§6.3 client MUST: close the connection after an invalid Content-Length"
      (fun () ->
         List.iter
           (fun v ->
              let c, ex =
                client_response
                  ("HTTP/1.1 200 OK\r\ncontent-length: "
                  ^ v
                  ^ "\r\n\r\nok"
                  ^ next_ok)
              in
              Alcotest.check
                int_list
                (v ^ ": no response")
                []
                (Client.statuses ex);
              Alcotest.(check (list string))
                (v ^ ": error")
                [ "Invalid_response_body_length" ]
                ex.errors;
              Alcotest.(check bool) (v ^ ": closed") true (Client.is_closed c))
           [ "abc"; "-1"; ""; "1 2"; "2, 3"; "2,3"; "2;" ])
  ; test
      "§6.3 client DEF: Content-Length frames exactly that many octets"
      (fun () ->
         let c, exs =
           pipelined
             [ client_get "/a"; client_get "/b" ]
             ("HTTP/1.1 200 OK\r\ncontent-length: 3\r\n\r\nabc" ^ next_ok)
         in
         (match exs with
         | [ a; b ] ->
           check_client_ok "first" ~body:"abc" a;
           check_client_ok "second" ~body:"ok" b
         | _ -> assert false);
         Alcotest.(check bool) "open" false (Client.is_closed c))
  ; test
      "§6.3 client DEF: response without a length is read until close"
      (fun () ->
         let c, ex = client_response "HTTP/1.1 200 OK\r\n\r\nabc" in
         Alcotest.(check bool) "not complete before EOF" false ex.body_eof;
         Client.feed c "def";
         Client.close_input c;
         check_client_ok "close-delimited" ~body:"abcdef" ex;
         Alcotest.(check bool) "closed" true (Client.is_closed c))
  ; test
      "§6.1 client MUST: HTTP/1.0 response with Transfer-Encoding closes the \
       connection"
      (fun () ->
         List.iter
           (fun extra ->
              let c, _ =
                client_response
                  ("HTTP/1.0 200 OK\r\nconnection: keep-alive\r\n"
                  ^ extra
                  ^ "transfer-encoding: chunked\r\n\r\n3\r\nabc\r\n0\r\n\r\n")
              in
              Alcotest.(check bool) "closed" true (Client.is_closed c))
           [ ""; "content-length: 3\r\n" ])
  ]

let client_request_tests =
  let written_body c =
    match Wire.split_head (Client.written c) with
    | Some (_, headers, rest) -> { Wire.start_line = ""; headers; body = rest }
    | None -> Alcotest.failf "no head in %S" (Client.written c)
  in
  let send fields =
    let c = Client.create () in
    let ex =
      Client.request
        ~body:"abc"
        c
        (Request.create
           ~headers:(Headers.of_list (("host", "example.com") :: fields))
           `POST
           "/")
    in
    c, ex
  in
  [ test
      "§6.3 client MUST: request content is sent with Content-Length or chunked"
      (fun () ->
         let c, _ = send [ "content-length", "3" ] in
         Alcotest.(check string)
           "content-length body"
           "abc"
           (written_body c).body;
         let c, _ = send [ "transfer-encoding", "chunked" ] in
         Alcotest.(check string)
           "chunked body"
           "3\r\nabc\r\n0\r\n\r\n"
           (written_body c).body;
         let c, _ = send [] in
         Alcotest.(check string)
           "no framing: no body bytes"
           ""
           (written_body c).body)
  ; test
      "§6.2 client MUST NOT: library adds no Content-Length to a chunked \
       request"
      (fun () ->
         let c, _ = send [ "transfer-encoding", "chunked" ] in
         Alcotest.(check (option string))
           "content-length"
           None
           (Wire.header (written_body c) "content-length"))
  ; test
      "§6.1 client MUST: request with a non-chunked final coding is not sent \
       unframed"
      (fun () ->
         match send [ "transfer-encoding", "gzip" ] with
         | exception (Failure _ | Invalid_argument _) -> ()
         | c, _ ->
           let m = written_body c in
           (match Wire.header m "transfer-encoding" with
           | Some v when String.ends_with ~suffix:"chunked" v -> ()
           | v ->
             Alcotest.failf
               "transfer-encoding %s written in %S"
               (Option.value ~default:"none" v)
               (Client.written c)))
  ]

let incomplete_tests =
  [ test
      "§8 server MAY: EOF in the head closes without calling the handler"
      (fun () ->
         List.iter
           (fun input ->
              let s = Server.run ~eof:true input in
              (match Server.statuses s with
              | [] | [ 400 ] -> ()
              | l ->
                Alcotest.failf "expected no response or 400, got [%s]" (codes l));
              Alcotest.(check int)
                "handler not called"
                0
                (Server.handler_calls s);
              check_closed "connection closed" s)
           [ "GET / HTTP/1.1\r\nHost: a\r\n"
           ; "GET / HTTP/1.1\r\nHost: a\r\n\r"
           ; "GET / HT"
           ])
  ; test
      "§6.3 server MUST: EOF before Content-Length octets closes without a \
       success response"
      (fun () ->
         let s =
           Server.run ~handler:read_only ~eof:true (post [ cl "5" ] "ab")
         in
         if List.mem 200 (Server.statuses s)
         then Alcotest.failf "200 for an incomplete body: %S" (Server.output s);
         check_closed "connection closed" s)
  ; test
      "§8 server MAY: EOF in a chunked body closes without a success response"
      (fun () ->
         List.iter
           (fun chunks ->
              let s =
                Server.run
                  ~handler:read_only
                  ~eof:true
                  (post [ te "chunked" ] chunks)
              in
              if List.mem 200 (Server.statuses s)
              then
                Alcotest.failf
                  "200 for an incomplete body: %S"
                  (Server.output s);
              check_closed (String.escaped chunks ^ ": connection closed") s)
           [ "3\r\nab"
           ; "3\r\nabc\r\n"
           ; "3\r\nabc\r\n0\r\n"
           ; "3\r\nabc\r\n0\r\nX-T: 1\r\n"
           ; "3"
           ])
  ; test
      "§8 server MUST: an incomplete request body is not reported as complete"
      (fun () ->
         List.iter
           (fun (headers, body) ->
              let s =
                Server.run ~handler:read_only ~eof:true (post headers body)
              in
              match s.Server.exchanges with
              | [ ex ] ->
                Alcotest.(check bool)
                  (String.escaped body ^ ": body not complete")
                  false
                  ex.body_complete
              | _ -> Alcotest.fail "expected one request")
           [ [ cl "5" ], "ab"; [ te "chunked" ], "3\r\nabc\r\n" ])
  ; test "§8 client MUST: EOF in the head is reported as an error" (fun () ->
      let c, ex = client_response "HTTP/1.1 200 OK\r\ncontent-le" in
      Client.close_input c;
      Alcotest.check int_list "no response" [] (Client.statuses ex);
      check_client_error "head" c ex)
  ; test
      "§6.3 client MUST: EOF before Content-Length octets is reported as \
       incomplete"
      (fun () ->
         let c, ex =
           client_response "HTTP/1.1 200 OK\r\ncontent-length: 5\r\n\r\nhel"
         in
         Client.close_input c;
         Alcotest.(check string) "partial body" "hel" ex.body;
         check_client_error "content-length" c ex)
  ; test
      "§8 client MUST: EOF in a chunked body is reported as incomplete"
      (fun () ->
         List.iter
           (fun chunks ->
              let c, ex = client_response (chunked_head ^ chunks) in
              Client.close_input c;
              check_client_error (String.escaped chunks) c ex)
           [ "3\r\nab"
           ; "3\r\nabc\r\n"
           ; "3\r\nabc\r\n0\r\n"
           ; "3\r\nabc\r\n0\r\nX-T: 1\r\n"
           ; "3"
           ])
  ]

let tests =
  request_framing_tests
  @ chunked_tests
  @ server_response_tests
  @ client_framing_tests
  @ client_request_tests
  @ incomplete_tests
