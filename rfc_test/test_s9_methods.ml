open Ache
open Rfc

let meth = Alcotest.of_pp Method.pp_hum
let ignore_request (_ : Server.exchange) = ()

let only_request s =
  match Server.requests s with
  | [ r ] -> r
  | rs -> Alcotest.failf "expected one request, got %d" (List.length rs)

let standard : (string * Method.standard) list =
  [ "GET", `GET
  ; "HEAD", `HEAD
  ; "POST", `POST
  ; "PUT", `PUT
  ; "DELETE", `DELETE
  ; "CONNECT", `CONNECT
  ; "OPTIONS", `OPTIONS
  ; "TRACE", `TRACE
  ]

let registered = List.map (fun (name, m) -> name, (m :> Method.t)) standard
let target_for = function "CONNECT" -> "example.com:443" | _ -> "/"

let head_only s =
  match Wire.split_head (Server.output s) with
  | None -> Alcotest.failf "no response head in %S" (Server.output s)
  | Some (_, _, rest) ->
    Alcotest.(check string) "no bytes after the response head" "" rest

let tests =
  [ test "§9.1 both DEF: method token is case-sensitive" (fun () ->
      Alcotest.check meth "GET" `GET (Method.of_string "GET");
      Alcotest.check meth "get" (`Other "get") (Method.of_string "get");
      Alcotest.check meth "Get" (`Other "Get") (Method.of_string "Get");
      Alcotest.check meth "head" (`Other "head") (Method.of_string "head");
      let s = Server.run ~handler:ignore_request (req "get" "/") in
      Alcotest.check
        meth
        "handler sees get"
        (`Other "get")
        (only_request s).meth;
      let s = Server.run ~handler:ignore_request (req "GET" "/") in
      Alcotest.check meth "handler sees GET" `GET (only_request s).meth)
  ; test
      "§9.1 both DEF: registered methods round-trip of_string and to_string"
      (fun () ->
         List.iter
           (fun (name, m) ->
              Alcotest.check meth name m (Method.of_string name);
              Alcotest.(check string) name name (Method.to_string m))
           registered)
  ; test
      "§9.1 server DEF: each registered method reaches the handler"
      (fun () ->
         List.iter
           (fun (name, m) ->
              let s =
                Server.run ~handler:ignore_request (req name (target_for name))
              in
              Alcotest.check meth name m (only_request s).meth)
           registered)
  ; test
      "§9.1 server DEF: extension method tokens reach the handler as Other"
      (fun () ->
         List.iter
           (fun name ->
              let s = Server.run ~handler:ignore_request (req name "/") in
              Alcotest.check meth name (`Other name) (only_request s).meth;
              Alcotest.(check string)
                (name ^ " to_string")
                name
                (Method.to_string (`Other name)))
           [ "PATCH"; "M-SEARCH"; "PROPFIND"; "X_Custom.1~!" ])
  ; test "§9.1 server DEF: method with a non-tchar byte gets 400" (fun () ->
      List.iter
        (fun name -> check_bad_request ~msg:name (Server.run (req name "/")))
        [ "GE(T"; "GE\"T"; "GE,T"; "GE/T"; "GE:T"; "G\x7fET" ])
  ; test "§9.1 server MUST: GET and HEAD are supported" (fun () ->
      let s =
        Server.run
          ~handler:(Server.respond ~body:"hello" `OK)
          (get "/" ^ req "HEAD" "/")
      in
      Alcotest.(check (list meth))
        "methods seen"
        [ `GET; `HEAD ]
        (List.map (fun (r : Request.t) -> r.meth) (Server.requests s));
      check_status ~methods:[ `GET; `HEAD ] "200 to both" [ 200; 200 ] s;
      check_open "connection open" s)
  ; test "§9.2.1 both DEF: safe methods are GET HEAD OPTIONS TRACE" (fun () ->
      List.iter
        (fun (name, m) ->
           let expected = List.mem name [ "GET"; "HEAD"; "OPTIONS"; "TRACE" ] in
           Alcotest.(check bool) name expected (Method.is_safe m))
        standard)
  ; test
      "§9.2.2 both DEF: idempotent methods are PUT DELETE and the safe methods"
      (fun () ->
         List.iter
           (fun (name, m) ->
              let expected =
                List.mem
                  name
                  [ "GET"; "HEAD"; "OPTIONS"; "TRACE"; "PUT"; "DELETE" ]
              in
              Alcotest.(check bool) name expected (Method.is_idempotent m))
           standard)
  ; test "§9.2.3 both DEF: cacheable methods are GET HEAD POST" (fun () ->
      List.iter
        (fun (name, m) ->
           let expected = List.mem name [ "GET"; "HEAD"; "POST" ] in
           Alcotest.(check bool) name expected (Method.is_cacheable m))
        standard)
  ; test
      "§9.2.2 client SHOULD NOT: no automatic retry of a POST after EOF"
      (fun () ->
         let c = Client.create () in
         let ex =
           Client.request
             ~body:"abc"
             c
             (Request.create
                ~headers:
                  (Headers.of_list
                     [ "host", "example.com"; "content-length", "3" ])
                `POST
                "/")
         in
         let first = Client.take_written c in
         Alcotest.(check bool)
           "request written"
           true
           (contains first "POST / HTTP/1.1");
         Client.close_input c;
         Alcotest.(check (list string))
           "error reported"
           [ "Malformed_response" ]
           ex.errors;
         Alcotest.(check string) "nothing written again" "" (Client.written c))
  ; test
      "§9.3.1 server DEF: content in a GET request is framed by its length"
      (fun () ->
         let s =
           Server.run
             (req
                ~headers:[ "Host", "a"; "Content-Length", "5" ]
                ~body:"hello"
                "GET"
                "/"
             ^ get "/")
         in
         check_status "two responses" [ 200; 200 ] s;
         Alcotest.(check (list string))
           "bodies seen"
           [ "hello"; "" ]
           (List.map (fun (ex : Server.exchange) -> ex.body) s.exchanges))
  ; test
      "§9.3.2 server DEF: content in a HEAD request is framed by its length"
      (fun () ->
         let s =
           Server.run
             ~handler:(Server.respond_after_body `OK)
             (req
                ~headers:[ "Host", "a"; "Content-Length", "5" ]
                ~body:"hello"
                "HEAD"
                "/"
             ^ get "/")
         in
         check_status ~methods:[ `HEAD; `GET ] "two responses" [ 200; 200 ] s;
         Alcotest.(check (list string))
           "bodies seen"
           [ "hello"; "" ]
           (List.map (fun (ex : Server.exchange) -> ex.body) s.exchanges))
  ; test "§9.3.5 server DEF: content in a DELETE request is framed" (fun () ->
      let s =
        Server.run
          (req
             ~headers:[ "Host", "a"; "Transfer-Encoding", "chunked" ]
             ~body:"5\r\nhello\r\n0\r\n\r\n"
             "DELETE"
             "/"
          ^ get "/")
      in
      check_status "two responses" [ 200; 200 ] s;
      Alcotest.(check (list string))
        "bodies seen"
        [ "hello"; "" ]
        (List.map (fun (ex : Server.exchange) -> ex.body) s.exchanges))
  ; test
      "§9.3.2 server SHOULD: HEAD response keeps the app Content-Length"
      (fun () ->
         let s =
           Server.run
             ~handler:
               (Server.respond_raw ~headers:[ "content-length", "5" ] `OK)
             (req "HEAD" "/")
         in
         let r = Server.response ~methods:[ `HEAD ] s in
         Alcotest.(check (option string))
           "content-length"
           (Some "5")
           (Wire.header r "content-length");
         head_only s;
         check_open "connection open" s)
  ; known_failure
      ~ticket:"07"
      "§9.3.2 server MUST NOT: no content in a HEAD respond_with_string"
      (fun () ->
         let s =
           Server.run
             ~handler:(Server.respond ~body:"hello" `OK)
             (req "HEAD" "/")
         in
         head_only s)
  ; known_failure
      ~ticket:"07"
      "§9.3.2 server MUST NOT: no content in a streamed HEAD response"
      (fun () ->
         let flushed = ref None in
         let s =
           Server.run
             ~handler:(fun ex ->
               let body =
                 Reqd.respond_with_streaming
                   ex.reqd
                   (Response.create
                      ~headers:(Headers.of_list [ "content-length", "5" ])
                      `OK)
               in
               Body.Writer.write_string body "hello";
               Body.Writer.flush body (fun r -> flushed := Some r);
               Body.Writer.close body)
             (req "HEAD" "/")
         in
         Alcotest.(check bool) "flush answered" true (Option.is_some !flushed);
         head_only s;
         check_open "connection open" s)
  ; known_failure
      ~ticket:"07"
      "§9.3.2 server MUST NOT: no content in an error response to HEAD"
      (fun () ->
         let module D = Server.D in
         let d =
           D.make (Server_connection.create (fun _ -> Alcotest.fail "handler"))
         in
         D.feed
           d
           (req ~headers:[ "Host", "a"; "Content-Length", "abc" ] "HEAD" "/");
         match Wire.split_head (D.output d) with
         | None -> Alcotest.failf "no response in %S" (D.output d)
         | Some (line, _, rest) ->
           Alcotest.(check string) "status" "HTTP/1.1 400 Bad Request" line;
           Alcotest.(check string) "no content" "" rest)
  ; known_failure
      ~ticket:"07"
      "§9.3.2 server MUST NOT: pipelined GET response follows the HEAD head"
      (fun () ->
         let s =
           Server.run
             ~handler:(Server.respond ~body:"hello" `OK)
             (req "HEAD" "/" ^ get "/")
         in
         let rs, rest =
           Wire.parse_responses ~methods:[ `HEAD; `GET ] (Server.output s)
         in
         Alcotest.(check (list string))
           "status lines"
           [ "HTTP/1.1 200 OK"; "HTTP/1.1 200 OK" ]
           (List.map (fun (r : Wire.message) -> r.start_line) rs);
         Alcotest.(check string) "nothing left over" "" rest;
         Alcotest.(check string) "GET body" "hello" (List.nth rs 1).Wire.body)
  ; test
      "§9.3.6 server MUST: CONNECT with a valid port reaches the handler"
      (fun () ->
         List.iter
           (fun target ->
              let s =
                Server.run
                  ~handler:ignore_request
                  (req ~headers:[ "Host", target ] "CONNECT" target)
              in
              let r = only_request s in
              Alcotest.check meth target `CONNECT r.meth;
              Alcotest.(check string) "target" target r.target)
           [ "example.com:443"; "example.com:80"; "[::1]:8080" ])
  ; test
      "§9.3.6 server MUST: 400 for CONNECT with an empty or invalid port"
      (fun () ->
         List.iter
           (fun target ->
              check_bad_request
                ~msg:target
                (Server.run
                   ~handler:ignore_request
                   (req ~headers:[ "Host", "example.com" ] "CONNECT" target)))
           [ "example.com:"
           ; "example.com"
           ; "example.com:http"
           ; "example.com:-1"
           ; "example.com:4 43"
           ])
  ; test
      "§9.3.6 server MUST NOT: no length fields added to a 2xx to CONNECT"
      (fun () ->
         let streamed =
           Server.run
             ~handler:(fun ex ->
               let body =
                 Reqd.respond_with_streaming
                   ~flush_headers_immediately:true
                   ex.reqd
                   (Response.create `OK)
               in
               Body.Writer.write_string body "tunnel";
               Body.Writer.flush body ignore)
             (req "CONNECT" "example.com:443")
         in
         let fixed =
           Server.run
             ~handler:(Server.respond_raw `OK)
             (req "CONNECT" "example.com:443")
         in
         List.iter
           (fun (label, s) ->
              match Wire.split_head (Server.output s) with
              | None ->
                Alcotest.failf "%s: no head in %S" label (Server.output s)
              | Some (line, headers, _) ->
                Alcotest.(check string) label "HTTP/1.1 200 OK" line;
                let m = { Wire.start_line = line; headers; body = "" } in
                Alcotest.(check bool)
                  (label ^ " content-length")
                  false
                  (Wire.has_header m "content-length");
                Alcotest.(check bool)
                  (label ^ " transfer-encoding")
                  false
                  (Wire.has_header m "transfer-encoding"))
           [ "streamed", streamed; "respond_with_string", fixed ];
         match Wire.split_head (Server.output streamed) with
         | Some (_, _, rest) ->
           Alcotest.(check string) "tunnel bytes sent raw" "tunnel" rest
         | None -> ())
  ; test
      "§9.3.6 client MUST: Content-Length in a 2xx to CONNECT is ignored"
      (fun () ->
         let c = Client.create () in
         let ex =
           Client.request
             c
             (Request.create
                ~headers:(Headers.of_list [ "host", "example.com:443" ])
                `CONNECT
                "example.com:443")
         in
         Client.feed
           c
           "HTTP/1.1 200 OK\r\nContent-Length: 5\r\n\r\nhello world, tunnel";
         Alcotest.(check int_list) "one 200" [ 200 ] (Client.statuses ex);
         Alcotest.(check (list string)) "no error" [] ex.errors;
         Alcotest.(check bool)
           "no 5-byte body ended by Content-Length"
           false
           ex.body_eof;
         Client.feed c "more tunnel bytes";
         Alcotest.(check (list string)) "still no error" [] ex.errors;
         Alcotest.(check bool)
           "reader not closed"
           false
           (Client.read_op c = `Close))
  ]
