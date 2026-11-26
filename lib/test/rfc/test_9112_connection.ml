open Ache
open Rfc

let smuggled = "GET /smuggled HTTP/1.1\r\nHost: a\r\n\r\n"
let chunk data = Printf.sprintf "%x\r\n%s\r\n" (String.length data) data
let last_chunk = "0\r\n\r\n"
let len s = string_of_int (String.length s)
let post ?version headers body = req ?version ~headers ~body "POST" "/"
let strings = Alcotest.(list string)

let check_targets msg expected s =
  Alcotest.check strings msg expected (targets s)

let server_responses s = fst (Wire.parse_responses (Server.output s))
let server_bodies s = List.map (fun m -> m.Wire.body) (server_responses s)

let first_response_connection s =
  match server_responses s with
  | r :: _ -> Wire.header r "connection"
  | [] -> Alcotest.failf "no response in %S" (Server.output s)

let check_close_option msg s =
  Alcotest.(check (option string))
    msg
    (Some "close")
    (Option.map String.lowercase_ascii (first_response_connection s))

let respond_target ex = Server.respond ~body:ex.Server.request.target `OK ex
let respond_unread ex = Server.respond `OK ex

let ok_response ?(headers = "") body =
  Printf.sprintf
    "HTTP/1.1 200 OK\r\n%sContent-Length: %d\r\n\r\n%s"
    headers
    (String.length body)
    body

let client_send
      ?(headers = [ "Host", "a" ])
      ?body
      ?close_body
      ?(meth = `GET)
      c
      target
  =
  Client.request
    ?body
    ?close_body
    c
    (Request.create ~headers:(Headers.of_list headers) meth target)

let check_client_errors msg expected (ex : Client.exchange) =
  Alcotest.(check bool) msg expected (ex.errors <> [])

let check_client_closed msg c =
  Alcotest.(check bool) msg true (Client.is_closed c)

let check_client_statuses msg expected ex =
  Alcotest.check int_list msg expected (Client.statuses ex)

let no_exn msg f =
  match f () with
  | () -> ()
  | exception exn -> Alcotest.failf "%s: raised %s" msg (Printexc.to_string exn)

type outcome =
  | Rejected of int list
  | Body_rejected
  | Served of
      { statuses : int list
      ; closed : bool option
      }

let outcome_to_string = function
  | Rejected codes ->
    Printf.sprintf
      "rejected with one of [%s] before the handler, closed"
      (String.concat "; " (List.map string_of_int codes))
  | Body_rejected -> "handler called for /, then 400 and closed"
  | Served { statuses; closed } ->
    Printf.sprintf
      "handler called for / only, statuses [%s]%s"
      (String.concat "; " (List.map string_of_int statuses))
      (match closed with
      | None -> ""
      | Some true -> ", closed"
      | Some false -> ", open")

let outcome_holds s = function
  | Rejected allowed ->
    (match Server.statuses s with
      | [ code ] -> List.mem code allowed
      | _ -> false)
    && Server.handler_calls s = 0
    && Server.is_closed s
  | Body_rejected ->
    Server.statuses s = [ 400 ] && targets s = [ "/" ] && Server.is_closed s
  | Served { statuses; closed } ->
    Server.statuses s = statuses
    && targets s = [ "/" ]
    && Option.fold ~none:true ~some:(Bool.equal (Server.is_closed s)) closed

let check_smuggled_not_dispatched s =
  if List.mem "/smuggled" (targets s)
  then
    Alcotest.failf
      "/smuggled reached the request handler; requests: [%s]"
      (String.concat
         "; "
         (List.map
            (fun (r : Request.t) ->
               Printf.sprintf "%S %s" (Method.to_string r.meth) r.target)
            (Server.requests s)))

let echo_unless_error ex =
  Server.read_body ex ~on_complete:(fun body ->
    if Option.is_none (Request_descriptor.error_code ex.Server.reqd)
    then Server.respond ~body `OK ex)

let smuggling_case (name, input, outcomes) =
  let run () =
    let s = Server.run ~handler:echo_unless_error input in
    check_smuggled_not_dispatched s;
    if not (List.exists (outcome_holds s) outcomes)
    then
      Alcotest.failf
        "observed statuses [%s], targets [%s], closed %b; expected one of: %s"
        (String.concat "; " (List.map string_of_int (Server.statuses s)))
        (String.concat "; " (targets s))
        (Server.is_closed s)
        (String.concat " | " (List.map outcome_to_string outcomes))
  in
  name, run

let rejected = [ Rejected [ 400 ] ]

let served_then_closed =
  [ Rejected [ 400 ]; Served { statuses = [ 200 ]; closed = Some true } ]

let rejected_or_served =
  [ Rejected [ 400 ]; Served { statuses = [ 200 ]; closed = None } ]

let rejected_or_next_rejected =
  [ Rejected [ 400 ]; Served { statuses = [ 200; 400 ]; closed = Some true } ]

let cl_te_body = last_chunk ^ smuggled
let hidden_chunked = chunk smuggled ^ last_chunk
let raw_post head body = "POST / HTTP/1.1\r\n" ^ head ^ "\r\n" ^ body

let smuggling_cases =
  [ ( "Content-Length then Transfer-Encoding: chunked"
    , post
        [ "Host", "a"
        ; "Content-Length", len cl_te_body
        ; "Transfer-Encoding", "chunked"
        ]
        cl_te_body
    , served_then_closed )
  ; ( "Transfer-Encoding: chunked then Content-Length"
    , post
        [ "Host", "a"
        ; "Transfer-Encoding", "chunked"
        ; "Content-Length", len cl_te_body
        ]
        cl_te_body
    , served_then_closed )
  ; ( "chunked body with a short Content-Length that exposes a request"
    , post
        [ "Host", "a"
        ; ( "Content-Length"
          , len (Printf.sprintf "%x\r\n" (String.length smuggled)) )
        ; "Transfer-Encoding", "chunked"
        ]
        hidden_chunked
    , served_then_closed )
  ; ( "two Content-Length lines with different values"
    , post
        [ "Host", "a"
        ; "Content-Length", "3"
        ; "Content-Length", len ("abc" ^ smuggled)
        ]
        ("abc" ^ smuggled)
    , rejected )
  ; ( "Content-Length list 3, 30"
    , post [ "Host", "a"; "Content-Length", "3, 30" ] ("abc" ^ smuggled)
    , rejected )
  ; ( "Content-Length +3"
    , post [ "Host", "a"; "Content-Length", "+3" ] ("abc" ^ smuggled)
    , rejected )
  ; ( "Content-Length 0x3"
    , post [ "Host", "a"; "Content-Length", "0x3" ] ("abc" ^ smuggled)
    , rejected )
  ; ( "Content-Length 1_0"
    , post [ "Host", "a"; "Content-Length", "1_0" ] ("abcdefghij" ^ smuggled)
    , rejected )
  ; ( "Transfer-Encoding: chunked on two field lines"
    , post
        [ "Host", "a"
        ; "Transfer-Encoding", "chunked"
        ; "Transfer-Encoding", "chunked"
        ]
        hidden_chunked
    , [ Rejected [ 400; 501 ]; Served { statuses = [ 200 ]; closed = None } ] )
  ; ( "Transfer-Encoding: chunked, identity"
    , post [ "Host", "a"; "Transfer-Encoding", "chunked, identity" ] cl_te_body
    , rejected )
  ; ( "Transfer-Encoding: xchunked"
    , post [ "Host", "a"; "Transfer-Encoding", "xchunked" ] cl_te_body
    , [ Rejected [ 400; 501 ] ] )
  ; ( "Transfer-Encoding value on an obs-fold line"
    , raw_post "Host: a\r\nTransfer-Encoding:\r\n chunked\r\n" hidden_chunked
    , rejected_or_served )
  ; ( "Transfer-Encoding: chunked in HTTP/1.0 with keep-alive"
    , post
        ~version:"HTTP/1.0"
        [ "Host", "a"
        ; "Connection", "keep-alive"
        ; "Transfer-Encoding", "chunked"
        ]
        cl_te_body
    , served_then_closed )
  ; ( "whitespace between Transfer-Encoding and the colon"
    , raw_post "Host: a\r\nTransfer-Encoding : chunked\r\n" cl_te_body
    , rejected )
  ; ( "Transfer_Encoding with an underscore"
    , post [ "Host", "a"; "Transfer_Encoding", "chunked" ] cl_te_body
    , rejected_or_next_rejected )
  ; ( "bare CR before Transfer-Encoding in a field value"
    , raw_post "Host: a\r\nX: y\rTransfer-Encoding: chunked\r\n" cl_te_body
    , rejected_or_next_rejected )
  ; ( "bare LF before Transfer-Encoding in a field value"
    , raw_post "Host: a\r\nX: y\nTransfer-Encoding: chunked\r\n" hidden_chunked
    , [ Rejected [ 400 ]
      ; Served { statuses = [ 200 ]; closed = None }
      ; Served { statuses = [ 200; 400 ]; closed = Some true }
      ] )
  ; ( "chunk size 0x3"
    , post
        [ "Host", "a"; "Transfer-Encoding", "chunked" ]
        ("0x3\r\nabc\r\n" ^ last_chunk ^ smuggled)
    , [ Body_rejected ] )
  ; ( "chunk size with a trailing space"
    , post
        [ "Host", "a"; "Transfer-Encoding", "chunked" ]
        ("3 \r\nabc\r\n" ^ last_chunk ^ smuggled)
    , [ Body_rejected ] )
  ; ( "chunk size ffffffffffffffff3"
    , post
        [ "Host", "a"; "Transfer-Encoding", "chunked" ]
        ("ffffffffffffffff3\r\nabc\r\n" ^ last_chunk ^ smuggled)
    , [ Body_rejected ] )
  ; ( "CRLF inside a quoted chunk extension"
    , post
        [ "Host", "a"; "Transfer-Encoding", "chunked" ]
        ("3;x=\"a\r\nb\"\r\nabc\r\n" ^ last_chunk ^ smuggled)
    , [ Body_rejected ] )
  ; ( "bare LF line endings in the head"
    , "POST / HTTP/1.1\nHost: a\nTransfer-Encoding: chunked\n\n"
      ^ hidden_chunked
    , rejected_or_served )
  ; ( "bare LF line endings in the chunk lines"
    , post
        [ "Host", "a"; "Transfer-Encoding", "chunked" ]
        (Printf.sprintf "%x\n%s\n0\n\n" (String.length smuggled) smuggled)
    , [ Body_rejected; Served { statuses = [ 200 ]; closed = None } ] )
  ; ( "whitespace-preceded Transfer-Encoding line before the first field"
    , "POST / HTTP/1.1\r\n Transfer-Encoding: chunked\r\nHost: a\r\n\r\n"
      ^ cl_te_body
    , rejected_or_next_rejected )
  ; ( "GET with a Content-Length body that holds a request"
    , req
        ~headers:[ "Host", "a"; "Content-Length", len smuggled ]
        ~body:smuggled
        "GET"
        "/"
    , [ Served { statuses = [ 200 ]; closed = None } ] )
  ]

let smuggling_tests =
  List.map
    (fun case ->
       let name, run = smuggling_case case in
       test ("§11.2 server DEF: no smuggling: " ^ name) run)
    smuggling_cases

let pending = ref []
let hold ex = pending := !pending @ [ ex ]

let release s =
  match !pending with
  | ex :: rest ->
    pending := rest;
    respond_target ex;
    Server.pump s
  | [] -> Alcotest.fail "no held request"

let server_tests =
  [ test "§9.3 server DEF: HTTP/1.1 without Connection persists" (fun () ->
      let s = Server.run (get "/1" ^ get "/2") in
      check_status "both answered" [ 200; 200 ] s;
      check_targets "both dispatched" [ "/1"; "/2" ] s;
      check_open "connection open" s)
  ; test
      "§9.3 server DEF: HTTP/1.0 without Connection closes after the first \
       response"
      (fun () ->
         let s =
           Server.run (req ~version:"HTTP/1.0" ~headers:[] "GET" "/1" ^ get "/2")
         in
         check_status "first answered" [ 200 ] s;
         check_not_dispatched "/2" s;
         check_closed "connection closed" s)
  ; test
      "§9.3 server DEF: HTTP/1.0 with Connection: keep-alive persists"
      (fun () ->
         let s =
           Server.run
             (req
                ~version:"HTTP/1.0"
                ~headers:[ "Connection", "keep-alive" ]
                "GET"
                "/1"
             ^ get "/2")
         in
         check_status "both answered" [ 200; 200 ] s;
         check_targets "both dispatched" [ "/1"; "/2" ] s;
         check_open "connection open" s)
  ; test
      "§9.6 server MUST: request Connection: close ends the connection and \
       later requests are not processed"
      (fun () ->
         let s =
           Server.run
             (get ~headers:[ "Host", "a"; "Connection", "close" ] "/1"
             ^ get "/2")
         in
         check_status "one response" [ 200 ] s;
         check_targets "only the first dispatched" [ "/1" ] s;
         check_closed "connection closed" s)
  ; test
      "§9.6 server MUST: app response Connection: close ends the connection \
       and later requests are not processed"
      (fun () ->
         let s =
           Server.run
             ~handler:(Server.respond ~headers:[ "Connection", "close" ] `OK)
             (get "/1" ^ get "/2")
         in
         check_status "one response" [ 200 ] s;
         check_targets "only the first dispatched" [ "/1" ] s;
         check_closed "connection closed" s)
  ; test
      "§9.6 server SHOULD: close option in the response to a request with \
       Connection: close"
      (fun () ->
         let s =
           Server.run (get ~headers:[ "Host", "a"; "Connection", "close" ] "/")
         in
         check_closed "connection closed" s;
         check_close_option "Connection: close in the response" s)
  ; test
      "§9.6 server SHOULD: close option in the response to an HTTP/1.0 request"
      (fun () ->
         let s = Server.run (req ~version:"HTTP/1.0" ~headers:[] "GET" "/") in
         check_closed "connection closed" s;
         check_close_option "Connection: close in the response" s)
  ; test "§9.6 server SHOULD: close option in a 400 response" (fun () ->
      let s = Server.run "GET / HTTP/1.1\r\nHost: a\r\nBad Header\r\n\r\n" in
      check_bad_request s;
      check_close_option "Connection: close in the 400" s)
  ; test
      "§9.6 server MUST NOT: no request processed after a 400 on the connection"
      (fun () ->
         let s =
           Server.run
             ("GET / HTTP/1.1\r\nHost: a\r\nBad Header\r\n\r\n" ^ get "/next")
         in
         check_rejected s;
         check_not_dispatched "/next" s)
  ; test
      "§9.5 server SHOULD: connection stays open after a normal HTTP/1.1 \
       response"
      (fun () ->
         let s = Server.run (get "/") in
         check_status "answered" [ 200 ] s;
         Alcotest.(check (option string))
           "no Connection field in the response"
           None
           (first_response_connection s);
         check_open "connection open" s)
  ; test
      "§9.3.2 server MUST: three pipelined requests answered in order"
      (fun () ->
         let s =
           Server.run ~handler:respond_target (get "/1" ^ get "/2" ^ get "/3")
         in
         check_status "three answers" [ 200; 200; 200 ] s;
         Alcotest.check
           strings
           "responses in request order"
           [ "/1"; "/2"; "/3" ]
           (server_bodies s))
  ; test
      "§9.3.2 server MUST: next pipelined request reaches the app only after \
       the previous response"
      (fun () ->
         pending := [];
         let s = Server.create ~handler:hold () in
         Server.feed s (get "/1" ^ get "/2" ^ get "/3");
         check_targets "only the head dispatched" [ "/1" ] s;
         Alcotest.(check string) "nothing written" "" (Server.output s);
         release s;
         check_targets
           "second dispatched after the first answer"
           [ "/1"; "/2" ]
           s;
         release s;
         release s;
         Alcotest.check
           strings
           "responses in request order"
           [ "/1"; "/2"; "/3" ]
           (server_bodies s);
         check_open "connection open" s)
  ; test
      "§9.3.2 server MUST: pipelined requests with Content-Length and chunked \
       bodies answered in order"
      (fun () ->
         let s =
           Server.run
             (post [ "Host", "a"; "Content-Length", "3" ] "abc"
             ^ post
                 [ "Host", "a"; "Transfer-Encoding", "chunked" ]
                 (chunk "def" ^ last_chunk)
             ^ get "/3")
         in
         check_status "three answers" [ 200; 200; 200 ] s;
         Alcotest.check
           strings
           "echoed bodies in request order"
           [ "abc"; "def"; "" ]
           (server_bodies s);
         check_open "connection open" s)
  ]

let check_unread_body input =
  let s = Server.run ~handler:respond_unread input in
  check_smuggled_not_dispatched s;
  match Server.statuses s, targets s, Server.is_closed s with
  | [ 200; 200 ], [ "/"; "/next" ], _ | [ 200 ], [ "/" ], true -> ()
  | codes, ts, closed ->
    Alcotest.failf
      "observed statuses [%s], targets [%s], closed %b; expected the body \
       skipped and /next served, or one response then close"
      (String.concat "; " (List.map string_of_int codes))
      (String.concat "; " ts)
      closed

let eof_tests =
  [ test
      "§9.3 server MUST: unread Content-Length body is skipped or the \
       connection closed"
      (fun () ->
         check_unread_body
           (post [ "Host", "a"; "Content-Length", len smuggled ] smuggled
           ^ get "/next"))
  ; test
      "§9.3 server MUST: unread chunked body is skipped or the connection \
       closed"
      (fun () ->
         check_unread_body
           (post [ "Host", "a"; "Transfer-Encoding", "chunked" ] hidden_chunked
           ^ get "/next"))
  ; test
      "§9.5 server SHOULD: EOF between requests closes the connection"
      (fun () ->
         let s = Server.run ~eof:true (get "/1") in
         check_status "answered" [ 200 ] s;
         check_closed "connection closed" s)
  ; test
      "§9.5 server SHOULD: response still written after EOF, then close"
      (fun () ->
         pending := [];
         let s = Server.create ~handler:hold () in
         Server.feed s (get "/1");
         Server.close_input s;
         check_targets "dispatched" [ "/1" ] s;
         release s;
         check_status "answered after EOF" [ 200 ] s;
         check_closed "connection closed" s)
  ; test
      "§9.5 client SHOULD: EOF with an outstanding request reports an error \
       and closes"
      (fun () ->
         let c = Client.create () in
         let ex = client_send c "/" in
         Client.close_input c;
         check_client_errors "error reported" true ex;
         check_client_statuses "no response" [] ex;
         check_client_closed "client closed" c)
  ]

let client_tests =
  [ test
      "§9.2 client MUST: pipelined responses matched to requests in order"
      (fun () ->
         let c =
           Client.create
             ~config:{ Config.default with max_pipelined_requests = 16 }
             ()
         in
         let ex1 = client_send c "/1" in
         let ex2 = client_send c "/2" in
         Alcotest.(check string)
           "both requests written before any response"
           "GET /1 HTTP/1.1\r\n\
            Host: a\r\n\
            \r\n\
            GET /2 HTTP/1.1\r\n\
            Host: a\r\n\
            \r\n"
           (Client.written c);
         Client.feed
           c
           (ok_response "r1"
           ^ "HTTP/1.1 201 Created\r\nContent-Length: 2\r\n\r\nr2");
         check_client_statuses "first response" [ 200 ] ex1;
         Alcotest.(check string) "first body" "r1" ex1.body;
         check_client_statuses "second response" [ 201 ] ex2;
         Alcotest.(check string) "second body" "r2" ex2.body)
  ; test
      "§9.3.2 client SHOULD NOT: no request pipelined after a POST before its \
       response"
      (fun () ->
         let c = Client.create () in
         let ex1 =
           client_send
             ~meth:`POST
             ~headers:[ "Host", "a"; "Content-Length", "3" ]
             ~body:"abc"
             c
             "/1"
         in
         let ex2 = client_send c "/2" in
         Alcotest.(check string)
           "only the POST written"
           "POST /1 HTTP/1.1\r\nHost: a\r\nContent-Length: 3\r\n\r\nabc"
           (Client.take_written c);
         Client.feed c (ok_response "r1");
         check_client_statuses "POST answered" [ 200 ] ex1;
         Alcotest.(check string)
           "GET written after the POST response"
           "GET /2 HTTP/1.1\r\nHost: a\r\n\r\n"
           (Client.written c);
         Client.feed c (ok_response "r2");
         check_client_statuses "GET answered" [ 200 ] ex2)
  ; test
      "§9.3 client MUST: whole response body read before the next response"
      (fun () ->
         let c = Client.create () in
         let ex1 = client_send c "/1" in
         let ex2 = client_send c "/2" in
         let inner = "HTTP/1.1 500 X\r\nContent-Length: 0\r\n\r\n" in
         let head =
           Printf.sprintf
             "HTTP/1.1 200 OK\r\nContent-Length: %d\r\n\r\n"
             (String.length inner)
         in
         Client.feed c (head ^ String.sub inner 0 10);
         check_client_statuses "second request not answered yet" [] ex2;
         Client.feed c (String.sub inner 10 (String.length inner - 10));
         Alcotest.(check string) "first body is the inner bytes" inner ex1.body;
         check_client_statuses "inner bytes are not a response" [] ex2;
         Client.feed c (ok_response "r2");
         check_client_statuses "second response" [ 200 ] ex2;
         Alcotest.(check string) "second body" "r2" ex2.body)
  ; test
      "§9.2 client MUST NOT: data with no outstanding request is not a response"
      (fun () ->
         let c = Client.create () in
         no_exn "feeding an unsolicited response" (fun () ->
           Client.feed c (ok_response "x"));
         check_client_closed "client closed" c)
  ; test
      "§6.3 client MUST NOT: a second response after the last one is not \
       delivered"
      (fun () ->
         let c = Client.create () in
         let ex = client_send c "/" in
         no_exn "feeding two responses for one request" (fun () ->
           Client.feed c (ok_response "r1" ^ ok_response "r2"));
         check_client_statuses "one response" [ 200 ] ex;
         Alcotest.(check string) "first body only" "r1" ex.body;
         check_client_errors "no error on the first exchange" false ex;
         check_client_closed "client closed" c)
  ; test "§6.3 client MAY: CRLF after the last response is discarded" (fun () ->
      let c = Client.create () in
      let ex = client_send c "/" in
      no_exn "feeding a response and CRLF" (fun () ->
        Client.feed c (ok_response "r1" ^ "\r\n"));
      check_client_statuses "one response" [ 200 ] ex;
      Alcotest.(check string) "body" "r1" ex.body;
      check_client_errors "no error on the exchange" false ex)
  ; test
      "§9.6 client MUST: response Connection: close stops further requests"
      (fun () ->
         let c = Client.create () in
         let ex1 = client_send c "/1" in
         let ex2 = client_send c "/2" in
         ignore (Client.take_written c);
         Client.feed c (ok_response ~headers:"Connection: close\r\n" "ok");
         check_client_statuses "first answered" [ 200 ] ex1;
         Alcotest.(check string) "first body" "ok" ex1.body;
         check_client_statuses "second not answered" [] ex2;
         Alcotest.(check string) "nothing written after" "" (Client.written c);
         check_client_closed "client closed" c)
  ; test
      "§9.6 client MUST: no request written after a response head with \
       Connection: close"
      (fun () ->
         let c = Client.create () in
         let ex1 = client_send c "/1" in
         ignore (Client.take_written c);
         Client.feed
           c
           "HTTP/1.1 200 OK\r\nConnection: close\r\nContent-Length: 2\r\n\r\no";
         let _ex2 = client_send c "/2" in
         Alcotest.(check string)
           "nothing written during the response"
           ""
           (Client.written c);
         Client.feed c "k";
         Alcotest.(check string) "first body" "ok" ex1.body;
         Alcotest.(check string) "nothing written after" "" (Client.written c);
         check_client_closed "client closed" c)
  ; test
      "§9.3.2 client SHOULD: unanswered pipelined request reported when the \
       connection closes"
      (fun () ->
         let c =
           Client.create
             ~config:{ Config.default with max_pipelined_requests = 16 }
             ()
         in
         let _ex1 = client_send c "/1" in
         let ex2 = client_send c "/2" in
         Alcotest.(check string)
           "both requests pipelined"
           "GET /1 HTTP/1.1\r\n\
            Host: a\r\n\
            \r\n\
            GET /2 HTTP/1.1\r\n\
            Host: a\r\n\
            \r\n"
           (Client.written c);
         Client.feed c (ok_response ~headers:"Connection: close\r\n" "ok");
         check_client_closed "client closed" c;
         check_client_statuses "second not answered" [] ex2;
         Alcotest.(check (list string))
           "second request gets Connection_closed"
           [ "Connection_closed" ]
           ex2.errors)
  ; test
      "§9.6 client MUST NOT: no request written after one with Connection: \
       close"
      (fun () ->
         let c = Client.create () in
         let ex1 =
           client_send ~headers:[ "Host", "a"; "Connection", "close" ] c "/1"
         in
         let _ex2 = client_send c "/2" in
         Alcotest.(check string)
           "only the first request written"
           "GET /1 HTTP/1.1\r\nHost: a\r\nConnection: close\r\n\r\n"
           (Client.take_written c);
         Client.feed c (ok_response "ok");
         check_client_statuses "first answered" [ 200 ] ex1;
         Alcotest.(check string) "nothing written after" "" (Client.written c);
         check_client_closed "client closed" c)
  ; test
      "§9.3 client MAY: no reuse after an HTTP/1.0 response without keep-alive"
      (fun () ->
         let c = Client.create () in
         let ex1 = client_send c "/1" in
         let ex2 = client_send c "/2" in
         ignore (Client.take_written c);
         Client.feed c "HTTP/1.0 200 OK\r\nContent-Length: 2\r\n\r\nok";
         check_client_statuses "first answered" [ 200 ] ex1;
         check_client_statuses "second not answered" [] ex2;
         Alcotest.(check string) "nothing written after" "" (Client.written c);
         check_client_closed "client closed" c)
  ; test
      "§9.5 client SHOULD: stop sending the body after an error response with \
       Connection: close"
      (fun () ->
         let c = Client.create () in
         let ex =
           client_send
             ~meth:`POST
             ~headers:[ "Host", "a"; "Content-Length", "6" ]
             ~body:"abc"
             ~close_body:false
             c
             "/upload"
         in
         ignore (Client.take_written c);
         Client.feed
           c
           "HTTP/1.1 413 Content Too Large\r\n\
            Connection: close\r\n\
            Content-Length: 0\r\n\
            \r\n";
         check_client_statuses "early response seen" [ 413 ] ex;
         no_exn "writing the rest of the body" (fun () ->
           Body.Writer.write_string ex.writer "def";
           Body.Writer.close ex.writer;
           Client.pump c);
         Alcotest.(check string)
           "rest of the body not written"
           ""
           (Client.written c);
         check_client_closed "client closed" c)
  ]

let splitting_header =
  "x\r\n\
   Content-Length: 0\r\n\
   \r\n\
   HTTP/1.1 200 OK\r\n\
   Content-Length: 4\r\n\
   \r\n\
   evil"

let integrity_tests =
  [ test
      "§11.1 server DEF: app field value cannot split the response"
      (fun () ->
         let raised = ref false in
         let handler ex =
           match
             Server.respond
               ~headers:[ "X-A", splitting_header ]
               ~body:"ok"
               `OK
               ex
           with
           | () -> ()
           | exception Invalid_argument _ ->
             raised := true;
             Server.respond ~body:"" `Internal_server_error ex
         in
         let s = Server.run ~handler (get "/") in
         Alcotest.(check bool)
           "Invalid_argument at the application's call"
           true
           !raised;
         match Wire.parse_responses (Server.output s) with
         | [ r ], "" ->
           Alcotest.(check int) "only the fallback 500" 500 (Wire.status r)
         | rs, rest ->
           Alcotest.failf
             "a client parses %d responses (unparsed %S) from %S"
             (List.length rs)
             rest
             (Server.output s)
         | exception _ ->
           Alcotest.failf "the output is not one response: %S" (Server.output s))
  ; test
      "§11.3 client DEF: truncated Content-Length body at EOF is an error"
      (fun () ->
         let c = Client.create () in
         let ex = client_send c "/" in
         Client.feed c "HTTP/1.1 200 OK\r\nContent-Length: 10\r\n\r\nabc";
         Client.close_input c;
         Alcotest.(check string) "bytes received" "abc" ex.body;
         check_client_errors "error reported" true ex)
  ; test
      "§11.3 client DEF: truncated chunked body at EOF is an error"
      (fun () ->
         let c = Client.create () in
         let ex = client_send c "/" in
         Client.feed
           c
           "HTTP/1.1 200 OK\r\nTransfer-Encoding: chunked\r\n\r\n5\r\nab";
         Client.close_input c;
         Alcotest.(check string) "bytes received" "ab" ex.body;
         check_client_errors "error reported" true ex)
  ; test "§11.3 client DEF: close-delimited body complete at EOF" (fun () ->
      let c = Client.create () in
      let ex = client_send c "/" in
      Client.feed c "HTTP/1.1 200 OK\r\n\r\nabc";
      Client.close_input c;
      Alcotest.(check string) "body" "abc" ex.body;
      Alcotest.(check bool) "body EOF" true ex.body_eof;
      check_client_errors "no error" false ex)
  ; test
      "§11.2 server DEF: Content-Length with OWS frames the body exactly"
      (fun () ->
         let s =
           Server.run
             (raw_post "Host: a\r\nContent-Length:   3  \r\n" "abc"
             ^ get "/next")
         in
         check_status "both answered" [ 200; 200 ] s;
         check_targets "both dispatched" [ "/"; "/next" ] s;
         Alcotest.check strings "bodies" [ "abc"; "" ] (server_bodies s))
  ]

let timeout_action s =
  Server_connection.read_timeout s.Server.driver.Server.Driver.conn

let timeout_tests =
  [ test
      "§9.5 server SHOULD: idle timeout closes gracefully and writes nothing"
      (fun () ->
         let s = Server.run (get "/") in
         check_status "answered" [ 200 ] s;
         ignore (Server.take_output s);
         (match timeout_action s with
         | Some (`Idle, close) -> close ()
         | Some ((`Head | `Body | `Close), _) ->
           Alcotest.fail "head timeout between requests"
         | None -> Alcotest.fail "no timeout between requests");
         Server.pump s;
         Alcotest.(check string) "nothing written" "" (Server.output s);
         check_closed "connection closed" s)
  ; test
      "§9.5 server SHOULD: head timeout answers 408 and closes without the \
       handler"
      (fun () ->
         let s = Server.run "GET / HTTP/1.1\r\nHo" in
         (match timeout_action s with
         | Some (`Head, reject) -> reject ()
         | Some ((`Idle | `Body | `Close), _) ->
           Alcotest.fail "idle timeout inside a head"
         | None -> Alcotest.fail "no timeout inside a head");
         Server.pump s;
         check_rejected ~allowed:[ 408 ] s)
  ; test
      "§9.5 server SHOULD: body timeout while a body is read answers 408 and \
       closes"
      (fun () ->
         let s =
           Server.run
             ~handler:(fun ex -> Server.read_body ex ~on_complete:ignore)
             "POST / HTTP/1.1\r\nHost: a\r\nContent-Length: 5\r\n\r\nab"
         in
         (match timeout_action s with
         | Some (`Body, expire) -> expire ()
         | Some ((`Idle | `Head | `Close), _) ->
           Alcotest.fail "idle or head timeout inside a body"
         | None -> Alcotest.fail "no timeout while a body is read");
         Server.pump s;
         check_status "408 after the handler" [ 408 ] s;
         check_closed "connection closed" s)
  ]

let tests =
  server_tests
  @ eof_tests
  @ timeout_tests
  @ client_tests
  @ integrity_tests
  @ smuggling_tests
