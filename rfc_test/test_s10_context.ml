open Ache
open Rfc

let expect_head ?(version = "HTTP/1.1") ?(expect = "100-continue") target =
  req
    ~version
    ~headers:[ "Host", "example.com"; "Expect", expect; "Content-Length", "5" ]
    "POST"
    target

let continue_then_final ex =
  Server.send_continue ex;
  Server.read_body ex ~on_complete:(fun body ->
    Server.respond ~body:("got " ^ body) `OK ex)

let tests =
  [ test
      "§10.1.1 server MUST: final status can be sent before the content"
      (fun () ->
         let s =
           Server.run ~handler:(Server.respond `Unauthorized) (expect_head "/")
         in
         Alcotest.(check int) "handler called" 1 (Server.handler_calls s);
         check_status "immediate 401" [ 401 ] s)
  ; test
      "§10.1.1 server MUST NOT: 100 Continue is sent before the content"
      (fun () ->
         let s = Server.run ~handler:continue_then_final (expect_head "/") in
         Alcotest.(check string)
           "only the 100 head"
           "HTTP/1.1 100 Continue\r\n\r\n"
           (Server.output s))
  ; known_failure
      ~ticket:"08"
      "§10.1.1 server MUST: final response follows the 100"
      (fun () ->
         let s = Server.create ~handler:continue_then_final () in
         Server.feed s (expect_head "/");
         Server.feed s "hello";
         let out = Server.output s in
         check_status "100 then 200" [ 100; 200 ] s;
         Alcotest.(check bool)
           "200 head right after the 100 head"
           true
           (String.starts_with
              ~prefix:"HTTP/1.1 100 Continue\r\n\r\nHTTP/1.1 200 OK\r\n"
              out);
         Alcotest.(check string)
           "final body"
           "got hello"
           (List.nth (fst (Wire.parse_responses out)) 1).Wire.body)
  ; test
      "§10.1.1 server DEF: 100-CONTINUE in any case gets the same handling"
      (fun () ->
         List.iter
           (fun expect ->
              let s =
                Server.run
                  ~handler:continue_then_final
                  (expect_head ~expect "/")
              in
              Alcotest.(check string)
                expect
                "HTTP/1.1 100 Continue\r\n\r\n"
                (Server.output s))
           [ "100-CONTINUE"; "100-Continue"; "100-continue" ])
  ; test "§10.1.1 server MUST: HTTP/1.0 request ignores 100-continue" (fun () ->
      let s = Server.run (expect_head ~version:"HTTP/1.0" "/" ^ "hello") in
      check_status "only the final 200" [ 200 ] s;
      Alcotest.(check bool)
        "no 100 written"
        false
        (contains (Server.output s) "100 Continue");
      Alcotest.(check string)
        "body echoed"
        "hello"
        (Server.response s).Wire.body)
  ; test
      "§10.1.1 server SHOULD: early response with Connection close closes"
      (fun () ->
         let s =
           Server.run
             ~handler:
               (Server.respond
                  ~headers:[ "Connection", "close" ]
                  `Expectation_failed)
             (expect_head "/")
         in
         check_status "417" [ 417 ] s;
         Alcotest.check write_op "writer closed" `Close (Server.write_op s))
  ; test
      "§10.1.1 server SHOULD: early 401, then content and next request read"
      (fun () ->
         let s =
           Server.create
             ~handler:(fun ex ->
               if ex.request.target = "/a"
               then Server.respond `Unauthorized ex
               else Server.echo ex)
             ()
         in
         Server.feed s (expect_head "/a");
         check_status "immediate 401" [ 401 ] s;
         Server.feed s ("hello" ^ get "/b");
         check_status "401 then 200" [ 401; 200 ] s;
         Alcotest.(check (list string))
           "targets"
           [ "/a"; "/b" ]
           (List.map (fun (r : Request.t) -> r.target) (Server.requests s));
         check_open "connection open" s)
  ; test "§10.2.4 server SHOULD NOT: library adds no Server field" (fun () ->
      let app = Server.run ~handler:(Server.respond ~body:"x" `OK) (get "/") in
      let module D = Server.D in
      let d = D.make (Server_connection.create (fun _ -> ())) in
      D.feed d (req ~headers:[ "Host", "a"; "Content-Length", "x" ] "GET" "/");
      List.iter
        (fun (label, out) ->
           match Wire.split_head out with
           | None -> Alcotest.failf "%s: no response in %S" label out
           | Some (start_line, headers, _) ->
             Alcotest.(check bool)
               label
               false
               (Wire.has_header
                  { Wire.start_line; headers; body = "" }
                  "server"))
        [ "application 200", Server.output app; "library 400", D.output d ])
  ]
