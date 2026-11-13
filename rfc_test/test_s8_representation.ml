open Ache
open Rfc

let host = Headers.of_list [ "host", "example.com" ]
let client_get target = Request.create ~headers:host `GET target

let post_cl ?(body = "abc") values =
  req
    ~headers:
      (("Host", "example.com") :: List.map (fun v -> "Content-Length", v) values)
    ~body
    "POST"
    "/"

let check_echo msg body s =
  check_status msg [ 200 ] s;
  Alcotest.(check string) (msg ^ ": body") body (Server.response s).Wire.body

let reject_cl value () =
  check_bad_request ~msg:value (Server.run (post_cl [ value ]))

let client_response raw =
  let c = Client.create () in
  let ex = Client.request c (client_get "/") in
  Client.feed c raw;
  ex

let cl_response ?(body = "ok") values =
  "HTTP/1.1 200 OK\r\n"
  ^ String.concat
      ""
      (List.map (fun v -> "content-length: " ^ v ^ "\r\n") values)
  ^ "\r\n"
  ^ body

let check_client_rejects msg ex =
  Alcotest.check
    int_list
    (msg ^ ": no response delivered")
    []
    (Client.statuses ex);
  Alcotest.(check bool) (msg ^ ": error reported") true (ex.Client.errors <> [])

let client_reject_cl value () =
  check_client_rejects value (client_response (cl_response [ value ]))

let check_large_server value =
  let s = Server.run (post_cl [ value ]) in
  match Server.statuses s with
  | [] ->
    (match s.exchanges with
    | [ ex ] ->
      Alcotest.(check bool)
        (value ^ ": body not complete")
        false
        ex.body_complete
    | _ -> Alcotest.failf "%s: no response and no request" value)
  | [ 400 ] | [ 413 ] ->
    Alcotest.(check int)
      (value ^ ": handler not called")
      0
      (Server.handler_calls s)
  | codes ->
    Alcotest.failf
      "%s: expected no response, 400 or 413, got [%s]"
      value
      (String.concat ";" (List.map string_of_int codes))

let check_large_client value =
  let ex = client_response (cl_response [ value ]) in
  Alcotest.(check bool) (value ^ ": body not complete") false ex.Client.body_eof

let head_of s =
  match Wire.split_head s with
  | Some (start_line, headers, rest) ->
    { Wire.start_line; headers; body = rest }
  | None -> Alcotest.failf "no complete head in %S" s

let check_no_cl msg s =
  let m = head_of (Server.output s) in
  Alcotest.(check (option string)) msg None (Wire.header m "content-length")

let streaming_empty status ex =
  let w = Reqd.respond_with_streaming ex.Server.reqd (Response.create status) in
  Body.Writer.close w

let syntax_tests =
  [ test "§8.6 server DEF: Content-Length 3 frames a 3 octet body" (fun () ->
      check_echo "plain" "abc" (Server.run (post_cl [ "3" ]));
      check_echo "with OWS" "abc" (Server.run (post_cl [ " 3 " ])))
  ; test "§8.6 server DEF: 400 for Content-Length 0x3" (reject_cl "0x3")
  ; test "§8.6 server DEF: 400 for Content-Length +3" (reject_cl "+3")
  ; test "§8.6 server DEF: 400 for Content-Length 1_0" (reject_cl "1_0")
  ; test "§8.6 server DEF: 400 for Content-Length 0o3" (reject_cl "0o3")
  ; test
      "§8.6 server DEF: 400 for negative, empty or non-digit Content-Length"
      (fun () ->
         List.iter
           (fun v -> reject_cl v ())
           [ "-1"; ""; "3 4"; "3a"; "abc"; "3.0" ])
  ; test "§8.6 server DEF: 400 for differing Content-Length values" (fun () ->
      check_bad_request
        ~msg:"two field lines"
        (Server.run (post_cl [ "3"; "4" ]));
      check_bad_request ~msg:"one list" (Server.run (post_cl [ "3, 4" ])))
  ; test
      "§8.6 client DEF: error for Content-Length 0x2"
      (client_reject_cl "0x2")
  ; test "§8.6 client DEF: error for Content-Length +2" (client_reject_cl "+2")
  ; test
      "§8.6 client DEF: error for negative or non-digit Content-Length"
      (fun () ->
         List.iter (fun v -> client_reject_cl v ()) [ "-1"; "2 3"; "x" ])
  ; test "§8.6 client DEF: error for differing Content-Length values" (fun () ->
      check_client_rejects
        "two field lines"
        (client_response (cl_response [ "2"; "3" ]));
      check_client_rejects "one list" (client_response (cl_response [ "2, 3" ])))
  ]

let large_tests =
  [ test
      "§8.6 server MUST: Content-Length over 64 bits is not read as small"
      (fun () ->
         List.iter
           check_large_server
           [ "99999999999999999999"
           ; "18446744073709551619"
           ; "9223372036854775808"
           ])
  ; test
      "§8.6 server MUST: Content-Length 9223372036854775807 is not misread"
      (fun () -> check_large_server "9223372036854775807")
  ; test
      "§8.6 client MUST: Content-Length over 64 bits is not read as small"
      (fun () ->
         List.iter
           check_large_client
           [ "99999999999999999999"
           ; "18446744073709551618"
           ; "9223372036854775808"
           ])
  ]

let duplicate_tests =
  [ test
      "§8.6 server MAY: Content-Length 3, 3 is rejected or read as 3"
      (fun () ->
         List.iter
           (fun values ->
              let s = Server.run (post_cl values) in
              match Server.statuses s with
              | [ 200 ] -> check_echo "collapsed" "abc" s
              | _ -> check_bad_request ~msg:"rejected" s)
           [ [ "3, 3" ]; [ "3"; "3" ] ])
  ; test
      "§8.6 client MAY: Content-Length 2, 2 is rejected or read as 2"
      (fun () ->
         List.iter
           (fun values ->
              let ex = client_response (cl_response values) in
              match Client.statuses ex with
              | [ 200 ] ->
                Alcotest.(check string) "collapsed: body" "ok" ex.body;
                Alcotest.(check bool)
                  "collapsed: body complete"
                  true
                  ex.body_eof
              | _ -> check_client_rejects "rejected" ex)
           [ [ "2, 2" ]; [ "2"; "2" ] ])
  ]

let generation_tests =
  [ test
      "§8.6 server MUST NOT: library adds no Content-Length to 1xx"
      (fun () ->
         check_no_cl
           "100 head"
           (Server.run ~handler:(Server.respond_raw `Continue) (get "/")))
  ; test
      "§8.6 server MUST NOT: library adds no Content-Length to 204"
      (fun () ->
         check_no_cl
           "respond_with_string"
           (Server.run ~handler:(Server.respond_raw `No_content) (get "/"));
         check_no_cl
           "respond_with_streaming"
           (Server.run ~handler:(streaming_empty `No_content) (get "/")))
  ; test
      "§8.6 server MUST NOT: library adds no Content-Length to 2xx to CONNECT"
      (fun () ->
         let connect =
           req
             ~headers:[ "Host", "example.com:443" ]
             "CONNECT"
             "example.com:443"
         in
         check_no_cl
           "respond_with_string"
           (Server.run ~handler:(Server.respond_raw `OK) connect);
         check_no_cl
           "respond_with_streaming"
           (Server.run ~handler:(streaming_empty `OK) connect))
  ; test
      "§8.6 server MAY: library keeps the HEAD response Content-Length"
      (fun () ->
         let s =
           Server.run
             ~handler:
               (Server.respond_raw ~headers:[ "content-length", "5" ] `OK)
             (req "HEAD" "/")
         in
         let m = Server.response ~methods:[ `HEAD ] s in
         Alcotest.(check (option string))
           "content-length"
           (Some "5")
           (Wire.header m "content-length");
         Alcotest.(check string) "no body" "" m.body)
  ; test
      "§8.6 server MAY: library keeps the 304 response Content-Length"
      (fun () ->
         let s =
           Server.run
             ~handler:
               (Server.respond_raw
                  ~headers:[ "content-length", "5" ]
                  `Not_modified)
             (get "/")
         in
         let m = Server.response s in
         Alcotest.(check (option string))
           "content-length"
           (Some "5")
           (Wire.header m "content-length");
         Alcotest.(check string) "no body" "" m.body)
  ; known_failure
      ~ticket:"07"
      "§8.6 server SHOULD: respond_with_string adds Content-Length"
      (fun () ->
         let s =
           Server.run ~handler:(Server.respond_raw ~body:"hello" `OK) (get "/")
         in
         let m = head_of (Server.output s) in
         Alcotest.(check (option string))
           "content-length"
           (Some "5")
           (Wire.header m "content-length");
         Alcotest.(check string) "body" "hello" m.body)
  ; test
      "§8.6 client SHOULD NOT: no Content-Length on a GET without content"
      (fun () ->
         let c = Client.create () in
         let _ = Client.request c (client_get "/") in
         let w = String.lowercase_ascii (Client.written c) in
         Alcotest.(check bool)
           "request written"
           true
           (contains w "get / http/1.1\r\n");
         if contains w "content-length"
         then Alcotest.failf "Content-Length written: %S" (Client.written c))
  ]

let tests = syntax_tests @ large_tests @ duplicate_tests @ generation_tests
