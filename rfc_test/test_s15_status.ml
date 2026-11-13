open Ache
open Rfc

let get_request () =
  Request.create ~headers:(Headers.of_list [ "host", "example.com" ]) `GET "/"

let client_exchange response =
  let c = Client.create () in
  let ex = Client.request c (get_request ()) in
  Client.feed c response;
  c, ex

let first_line s =
  match Wire.index_from s 0 "\r\n" with Some i -> String.sub s 0 i | None -> s

let class_of (s : Status.t) =
  List.filter_map
    (fun (c, p) -> if p s then Some c else None)
    [ 1, Status.is_informational
    ; 2, Status.is_successful
    ; 3, Status.is_redirection
    ; 4, Status.is_client_error
    ; 5, Status.is_server_error
    ]

let check_registered codes =
  List.iter
    (fun (n, phrase) ->
       let label = string_of_int n in
       let s = Status.of_code n in
       Alcotest.(check int) (label ^ " to_code") n (Status.to_code s);
       Alcotest.(check string) (label ^ " to_string") label (Status.to_string s);
       Alcotest.(check int_list) (label ^ " class") [ n / 100 ] (class_of s);
       Alcotest.(check string)
         (label ^ " reason")
         phrase
         (Response.create s).reason;
       let server = Server.run ~handler:(Server.respond_raw s) (get "/") in
       Alcotest.(check string)
         (label ^ " status line")
         (Printf.sprintf "HTTP/1.1 %d %s" n phrase)
         (first_line (Server.output server)))
    codes

let no_content_after_head s =
  match Wire.split_head (Server.output s) with
  | None -> Alcotest.failf "no response head in %S" (Server.output s)
  | Some (_, _, rest) ->
    Alcotest.(check string) "no bytes after the response head" "" rest

let tests =
  [ test
      "§15 both DEF: codes 100..599 are classed by their first digit"
      (fun () ->
         for n = 100 to 599 do
           Alcotest.(check int_list)
             (string_of_int n)
             [ n / 100 ]
             (class_of (Status.of_code n))
         done)
  ; test
      "§15 client MUST: unrecognized 299 371 471 599 keep their class"
      (fun () ->
         List.iter
           (fun n ->
              let _, ex =
                client_exchange
                  (Printf.sprintf
                     "HTTP/1.1 %d Odd\r\nContent-Length: 2\r\n\r\nok"
                     n)
              in
              Alcotest.(check int_list) "status" [ n ] (Client.statuses ex);
              Alcotest.(check (list string)) "no error" [] ex.errors;
              Alcotest.(check string) "body" "ok" ex.body;
              match ex.responses with
              | [ r ] ->
                Alcotest.(check int_list)
                  (string_of_int n ^ " class")
                  [ n / 100 ]
                  (class_of r.status)
              | _ -> Alcotest.fail "one response")
           [ 299; 371; 471; 599 ])
  ; known_failure
      ~ticket:"08"
      "§15 client MUST: unrecognized 199 is interim before the final"
      (fun () ->
         let _, ex =
           client_exchange
             "HTTP/1.1 199 Odd\r\n\
              \r\n\
              HTTP/1.1 200 OK\r\n\
              Content-Length: 2\r\n\
              \r\n\
              ok"
         in
         Alcotest.(check int_list) "final status" [ 200 ] (Client.statuses ex);
         Alcotest.(check (list string)) "no error" [] ex.errors;
         Alcotest.(check string) "body" "ok" ex.body)
  ; test
      "§15 client SHOULD: response with status 600 is delivered with its body"
      (fun () ->
         let _, ex =
           client_exchange "HTTP/1.1 600 Odd\r\nContent-Length: 2\r\n\r\nok"
         in
         Alcotest.(check int_list) "status" [ 600 ] (Client.statuses ex);
         Alcotest.(check (list string)) "no error" [] ex.errors;
         Alcotest.(check string) "body" "ok" ex.body)
  ; known_failure
      ~ticket:"new"
      "§15 client SHOULD: received status 600 is classed as a server error"
      (fun () ->
         let _, ex =
           client_exchange "HTTP/1.1 600 Odd\r\nContent-Length: 2\r\n\r\nok"
         in
         match ex.responses with
         | [ r ] ->
           Alcotest.(check bool)
             "is_server_error"
             true
             (Status.is_server_error r.status)
         | _ -> Alcotest.fail "one response")
  ; known_failure
      ~ticket:"new"
      "§15 client SHOULD: status 099 gives a response or an error"
      (fun () ->
         let _, ex =
           client_exchange "HTTP/1.1 099 Odd\r\nContent-Length: 2\r\n\r\nok"
         in
         Alcotest.(check int)
           "one response or one error"
           1
           (List.length ex.responses + List.length ex.errors))
  ; test
      "§15 client DEF: four-digit status 1000 is a malformed response"
      (fun () ->
         let _, ex =
           client_exchange "HTTP/1.1 1000 Odd\r\nContent-Length: 2\r\n\r\nok"
         in
         Alcotest.(check (list string))
           "error"
           [ "Malformed_response" ]
           ex.errors;
         Alcotest.(check int) "no response" 0 (List.length ex.responses))
  ; known_failure
      ~ticket:"new"
      "§15 client DEF: two-digit status 20 is a malformed response"
      (fun () ->
         let _, ex =
           client_exchange "HTTP/1.1 20 Odd\r\nContent-Length: 2\r\n\r\nok"
         in
         Alcotest.(check (list string))
           "error"
           [ "Malformed_response" ]
           ex.errors;
         Alcotest.(check int) "no response" 0 (List.length ex.responses))
  ; test "§15.2 both DEF: registered 1xx codes and reason phrases" (fun () ->
      check_registered [ 100, "Continue"; 101, "Switching Protocols" ])
  ; test "§15.3 both DEF: registered 2xx codes and reason phrases" (fun () ->
      check_registered
        [ 200, "OK"
        ; 201, "Created"
        ; 202, "Accepted"
        ; 203, "Non-Authoritative Information"
        ; 204, "No Content"
        ; 205, "Reset Content"
        ; 206, "Partial Content"
        ])
  ; test "§15.4 both DEF: registered 3xx codes and reason phrases" (fun () ->
      check_registered
        [ 300, "Multiple Choices"
        ; 301, "Moved Permanently"
        ; 302, "Found"
        ; 303, "See Other"
        ; 304, "Not Modified"
        ; 305, "Use Proxy"
        ; 307, "Temporary Redirect"
        ])
  ; known_failure
      ~ticket:"09"
      "§15.4 both DEF: 308 Permanent Redirect"
      (fun () -> check_registered [ 308, "Permanent Redirect" ])
  ; test "§15.5 both DEF: registered 4xx codes and reason phrases" (fun () ->
      check_registered
        [ 400, "Bad Request"
        ; 401, "Unauthorized"
        ; 402, "Payment Required"
        ; 403, "Forbidden"
        ; 404, "Not Found"
        ; 405, "Method Not Allowed"
        ; 406, "Not Acceptable"
        ; 407, "Proxy Authentication Required"
        ; 408, "Request Timeout"
        ; 409, "Conflict"
        ; 410, "Gone"
        ; 411, "Length Required"
        ; 412, "Precondition Failed"
        ; 414, "URI Too Long"
        ; 415, "Unsupported Media Type"
        ; 416, "Range Not Satisfiable"
        ; 417, "Expectation Failed"
        ; 426, "Upgrade Required"
        ])
  ; known_failure
      ~ticket:"09"
      "§15.5 both DEF: 413 Content Too Large"
      (fun () -> check_registered [ 413, "Content Too Large" ])
  ; known_failure
      ~ticket:"09"
      "§15.5 both DEF: 421 Misdirected Request"
      (fun () -> check_registered [ 421, "Misdirected Request" ])
  ; known_failure
      ~ticket:"09"
      "§15.5 both DEF: 422 Unprocessable Content"
      (fun () -> check_registered [ 422, "Unprocessable Content" ])
  ; test "§15.6 both DEF: registered 5xx codes and reason phrases" (fun () ->
      check_registered
        [ 500, "Internal Server Error"
        ; 501, "Not Implemented"
        ; 502, "Bad Gateway"
        ; 503, "Service Unavailable"
        ; 504, "Gateway Timeout"
        ; 505, "HTTP Version Not Supported"
        ])
  ; known_failure
      ~ticket:"08"
      "§15.2 server MUST NOT: no 1xx response to an HTTP/1.0 client"
      (fun () ->
         let s =
           Server.run
             ~handler:(fun ex ->
               Server.send_continue ex;
               Server.respond_after_body `OK ex)
             (req
                ~version:"HTTP/1.0"
                ~headers:
                  [ "Host", "a"
                  ; "Expect", "100-continue"
                  ; "Content-Length", "5"
                  ]
                ~body:"hello"
                "POST"
                "/")
         in
         check_status "only the final 200" [ 200 ] s)
  ; known_failure
      ~ticket:"08"
      "§15.2 client MUST: 100 Continue before the final response is parsed"
      (fun () ->
         let _, ex =
           client_exchange
             "HTTP/1.1 100 Continue\r\n\
              \r\n\
              HTTP/1.1 200 OK\r\n\
              Content-Length: 2\r\n\
              \r\n\
              ok"
         in
         Alcotest.(check int_list) "final status" [ 200 ] (Client.statuses ex);
         Alcotest.(check (list string)) "no error" [] ex.errors;
         Alcotest.(check string) "body" "ok" ex.body)
  ; known_failure
      ~ticket:"08"
      "§15.2 client MUST: 103 and 100 before the final response are parsed"
      (fun () ->
         let _, ex =
           client_exchange
             "HTTP/1.1 103 Early Hints\r\n\
              Link: </a>\r\n\
              \r\n\
              HTTP/1.1 100 Continue\r\n\
              \r\n\
              HTTP/1.1 200 OK\r\n\
              Content-Length: 2\r\n\
              \r\n\
              ok"
         in
         Alcotest.(check int_list) "final status" [ 200 ] (Client.statuses ex);
         Alcotest.(check (list string)) "no error" [] ex.errors;
         Alcotest.(check string) "body" "ok" ex.body)
  ; known_failure
      ~ticket:"07"
      "§15.3.5 server DEF: a 204 response ends after its header section"
      (fun () ->
         no_content_after_head
           (Server.run
              ~handler:(Server.respond_raw ~body:"hello" `No_content)
              (get "/")))
  ; known_failure
      ~ticket:"07"
      "§15.4.5 server DEF: a 304 response ends after its header section"
      (fun () ->
         no_content_after_head
           (Server.run
              ~handler:(fun ex ->
                let body =
                  Reqd.respond_with_streaming
                    ex.reqd
                    (Response.create `Not_modified)
                in
                Body.Writer.write_string body "hello";
                Body.Writer.close body)
              (get "/")))
  ; test "§15.5 server SHOULD: library 400 carries an explanation" (fun () ->
      let module D = Server.D in
      let d = D.make (Server_connection.create (fun _ -> ())) in
      D.feed d (req ~headers:[ "Host", "a"; "Content-Length", "x" ] "GET" "/");
      let r = Wire.response (D.output d) in
      Alcotest.(check string) "status" "HTTP/1.1 400 Bad Request" r.start_line;
      Alcotest.(check bool) "explanation" true (String.length r.body > 0))
  ; test "§15.6 server SHOULD: library 500 carries an explanation" (fun () ->
      let module D = Server.D in
      let conn = Server_connection.create (fun _ -> ()) in
      let d = D.make conn in
      D.feed d (get "/");
      Server_connection.report_exn conn (Failure "boom");
      D.pump d;
      let r = Wire.response (D.output d) in
      Alcotest.(check string)
        "status"
        "HTTP/1.1 500 Internal Server Error"
        r.start_line;
      Alcotest.(check bool) "explanation" true (String.length r.body > 0))
  ]
