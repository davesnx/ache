open Ache

let fields = Alcotest.(list (pair string string))

let contains s sub =
  let n = String.length sub in
  let rec go i =
    i + n <= String.length s && (String.sub s i n = sub || go (i + 1))
  in
  go 0

let check_contains msg s sub =
  if not (contains s sub) then Alcotest.failf "%s: %S not in %S" msg sub s

let check_lacks msg s sub =
  if contains s sub then Alcotest.failf "%s: %S in %S" msg sub s

(* Every way to cut [input] in two, and one byte at a time. *)
let splits input =
  let n = String.length input in
  List.init (n + 1) (fun i ->
    ( Printf.sprintf "split at %d" i
    , [ String.sub input 0 i; String.sub input i (n - i) ] ))
  @ [ "byte by byte", List.init n (fun i -> String.make 1 input.[i]) ]

let chunked_post ?(headers = "") body =
  "POST / HTTP/1.1\r\nHost: a\r\nTransfer-Encoding: chunked\r\n"
  ^ headers
  ^ "\r\n"
  ^ body

let received_trailers =
  "3\r\n\
   abc\r\n\
   0\r\n\
   X-A: 1\r\n\
   Content-Length: 9\r\n\
   x-b: 2\r\n\
   HOST: evil\r\n\
   Trailer: x-a\r\n\
   X-A: 3\r\n\
   te: trailers\r\n\
   \r\n"

let expected_trailers = [ "X-A", "1"; "x-b", "2"; "X-A", "3" ]

type result =
  | Eof of (string * string) list
  | Error

let ending =
  Alcotest.testable
    (fun fmt -> function
       | Eof l ->
         Format.fprintf
           fmt
           "Eof [%s]"
           (String.concat "; " (List.map (fun (n, v) -> n ^ ": " ^ v) l))
       | Error -> Format.pp_print_string fmt "Error")
    ( = )

let read_body ~on_end body =
  let rec next () =
    Body.Reader.schedule_read
      body
      ~on_eof:(fun () ->
        on_end (Eof (Headers.to_list (Body.Reader.trailers body))))
      ~on_error:(fun () -> on_end Error)
      ~on_read:(fun _ ~off:_ ~len:_ ->
        Alcotest.(check fields)
          "no trailers before the end"
          []
          (Headers.to_list (Body.Reader.trailers body));
        next ())
  in
  next ()

let ok =
  Response.create ~headers:(Headers.of_list [ "content-length", "0" ]) `OK

let server_reading results reqd =
  read_body (Request_descriptor.request_body reqd) ~on_end:(fun r ->
    results := r :: !results;
    match r with
    | Eof _ -> Request_descriptor.respond_with_string reqd ok ""
    | Error -> ())

let serve_pieces ?config handler pieces =
  let t = Testing.create ?config handler in
  List.iter (Testing.feed t) pieces;
  Testing.output t

let test_server_receives () =
  List.iter
    (fun (name, pieces) ->
       let results = ref [] in
       let out = serve_pieces (server_reading results) pieces in
       Alcotest.(check (list ending)) name [ Eof expected_trailers ] !results;
       check_contains name out "HTTP/1.1 200 OK\r\n")
    (splits (chunked_post received_trailers))

let test_server_no_trailers () =
  let results = ref [] in
  ignore
    (serve_pieces
       (server_reading results)
       [ chunked_post "3\r\nabc\r\n0\r\n\r\n" ]
     : string);
  Alcotest.(check (list ending)) "empty" [ Eof [] ] !results

(* Client side: feed [pieces] as the response to one GET. *)
let client_output c =
  let b = Buffer.create 256 in
  let rec go () =
    match Client_connection.next_write_operation c with
    | `Write iovecs ->
      let n =
        List.fold_left
          (fun n { IOVec.buffer; off; len } ->
             Buffer.add_string b (Bigstring.substring buffer ~off ~len);
             n + len)
          0
          iovecs
      in
      Client_connection.report_write_result c (`Ok n);
      go ()
    | `Yield | `Close _ -> ()
  in
  go ();
  Buffer.contents b

let client_feed c pending =
  let rec go s =
    let len = String.length s in
    if len = 0
    then s
    else
      match Client_connection.next_read_operation c with
      | `Read ->
        let n =
          Client_connection.read
            c
            (Bigstring.of_string s ~off:0 ~len)
            ~off:0
            ~len
        in
        if n = 0 then s else go (String.sub s n (len - n))
      | `Yield | `Close -> s
  in
  go pending

let client_feed_pieces c pieces =
  ignore
    (List.fold_left
       (fun pending piece -> client_feed c (pending ^ piece))
       ""
       pieces
     : string)

let get = Request.create ~headers:(Headers.of_list [ "host", "a" ]) `GET "/"

let client_reading ?config ?(request = get) results errors =
  let c = Client_connection.create ?config () in
  let body =
    Client_connection.request
      c
      request
      ~error_handler:(fun _ -> incr errors)
      ~response_handler:(fun _ body ->
        read_body body ~on_end:(fun r -> results := r :: !results))
  in
  c, body

let chunked_response body =
  "HTTP/1.1 200 OK\r\nTransfer-Encoding: chunked\r\n\r\n" ^ body

let test_client_receives () =
  List.iter
    (fun (name, pieces) ->
       let results = ref [] in
       let errors = ref 0 in
       let c, body = client_reading results errors in
       Body.Writer.close body;
       ignore (client_output c : string);
       client_feed_pieces c pieces;
       Alcotest.(check (list ending)) name [ Eof expected_trailers ] !results;
       Alcotest.(check int) (name ^ ": errors") 0 !errors)
    (splits (chunked_response received_trailers))

(* Sending. *)
let streaming ?(status = `OK) ?(headers = []) ~trailers reqd =
  let body =
    Request_descriptor.respond_with_streaming
      reqd
      (Response.create ~headers:(Headers.of_list headers) status)
  in
  Body.Writer.write_string body "hi";
  Body.Writer.close ~trailers body

let te_request ?(meth = "GET") ?(version = "1.1") te =
  Printf.sprintf
    "%s / HTTP/%s\r\nHost: a\r\n%s\r\n"
    meth
    version
    (match te with None -> "" | Some v -> "TE: " ^ v ^ "\r\n")

let grpc_trailers =
  Headers.of_list [ "grpc-status", "0"; "grpc-message", "all good" ]

let sent = "0\r\ngrpc-status: 0\r\ngrpc-message: all good\r\n\r\n"

let test_te_gating () =
  List.iter
    (fun (te, expected) ->
       let out =
         Testing.serve (streaming ~trailers:grpc_trailers) (te_request te)
       in
       let name = Option.value te ~default:"no TE" in
       check_contains name out "transfer-encoding: chunked\r\n";
       if expected
       then check_contains name out ("2\r\nhi\r\n" ^ sent)
       else (
         check_contains name out "2\r\nhi\r\n0\r\n\r\n";
         check_lacks name out "grpc-status"))
    [ Some "trailers", true
    ; Some "trailers, deflate", true
    ; Some "deflate,trailers", true
    ; Some "TRAILERS", true
    ; Some "Trailers ; ", false
    ; Some " deflate , Trailers ", true
    ; Some "trailersx", false
    ; Some "deflate", false
    ; Some "", false
    ; None, false
    ]

let test_te_multiple_lines () =
  let out =
    Testing.serve
      (streaming ~trailers:grpc_trailers)
      "GET / HTTP/1.1\r\nHost: a\r\nTE: deflate\r\nte: trailers\r\n\r\n"
  in
  check_contains "two TE lines" out sent

let expect_invalid msg f =
  match f () with
  | () -> Alcotest.failf "%s: no Invalid_argument" msg
  | exception Invalid_argument _ -> ()

let forbidden =
  [ "authorization"
  ; "cache-control"
  ; "connection"
  ; "content-encoding"
  ; "content-length"
  ; "content-range"
  ; "content-type"
  ; "expect"
  ; "host"
  ; "keep-alive"
  ; "max-forwards"
  ; "pragma"
  ; "proxy-authenticate"
  ; "proxy-authorization"
  ; "proxy-connection"
  ; "range"
  ; "realm"
  ; "te"
  ; "trailer"
  ; "transfer-encoding"
  ; "www-authenticate"
  ]

let invalid_trailers =
  List.concat_map
    (fun name ->
       [ [ name, "x" ]
       ; [ String.uppercase_ascii name, "x" ]
       ; [ "x-ok", "1"; String.capitalize_ascii name, "x" ]
       ])
    forbidden
  @ [ [ "bad name", "x" ]; [ "", "x" ]; [ "x-a", "a\r\nb" ]; [ "x-a", "\000" ] ]

(* Each invalid close raises and leaves the body open; a later plain close still
   ends it. *)
let server_rejects ?(te = Some "trailers") ?(headers = []) () =
  let raised = ref 0 in
  let handler reqd =
    let body =
      Request_descriptor.respond_with_streaming
        reqd
        (Response.create ~headers:(Headers.of_list headers) `OK)
    in
    List.iter
      (fun trailers ->
         expect_invalid
           (Headers.to_string (Headers.of_list trailers))
           (fun () ->
              Body.Writer.close ~trailers:(Headers.of_list trailers) body);
         incr raised;
         Alcotest.(check bool) "still open" false (Body.Writer.is_closed body))
      invalid_trailers;
    Body.Writer.write_string body "hi";
    Body.Writer.close body
  in
  let out = Testing.serve handler (te_request te) in
  Alcotest.(check int) "raised" (List.length invalid_trailers) !raised;
  out

let test_send_forbidden () =
  let out = server_rejects () in
  check_contains "chunked body ends" out "2\r\nhi\r\n0\r\n\r\n";
  let out = server_rejects ~te:None () in
  check_contains "without TE" out "2\r\nhi\r\n0\r\n\r\n"

let test_send_content_length () =
  let handler reqd =
    let body =
      Request_descriptor.respond_with_streaming
        reqd
        (Response.create
           ~headers:(Headers.of_list [ "content-length", "2" ])
           `OK)
    in
    Body.Writer.write_string body "hi";
    expect_invalid "content-length" (fun () ->
      Body.Writer.close ~trailers:grpc_trailers body);
    Alcotest.(check bool) "still open" false (Body.Writer.is_closed body);
    Body.Writer.close ~trailers:Headers.empty body
  in
  let out = Testing.serve handler (te_request (Some "trailers")) in
  check_contains "body" out "content-length: 2\r\n\r\nhi";
  check_lacks "no trailers" out "grpc"

let test_send_dropped () =
  List.iter
    (fun (name, request, status) ->
       let out =
         Testing.serve (streaming ~status ~trailers:grpc_trailers) request
       in
       check_lacks name out "grpc";
       check_lacks name out "\r\n0\r\n")
    [ "HEAD", te_request ~meth:"HEAD" (Some "trailers"), `OK
    ; "204", te_request (Some "trailers"), `No_content
    ; "304", te_request (Some "trailers"), `Not_modified
    ; "close-delimited", te_request ~version:"1.0" (Some "trailers"), `OK
    ];
  let out =
    Testing.serve
      (fun reqd ->
         let body =
           Request_descriptor.respond_with_streaming reqd (Response.create `OK)
         in
         expect_invalid "invalid on HEAD" (fun () ->
           Body.Writer.close
             ~trailers:(Headers.of_list [ "content-length", "1" ])
             body);
         Body.Writer.close body)
      (te_request ~meth:"HEAD" (Some "trailers"))
  in
  check_contains "HEAD answered" out "HTTP/1.1 200 OK\r\n"

let test_close_twice () =
  let out =
    Testing.serve
      (fun reqd ->
         let body =
           Request_descriptor.respond_with_streaming reqd (Response.create `OK)
         in
         Body.Writer.close ~trailers:grpc_trailers body;
         Body.Writer.close
           ~trailers:(Headers.of_list [ "x-second", "1"; "host", "x" ])
           body)
      (te_request (Some "trailers"))
  in
  check_contains "first trailers" out ("\r\n" ^ sent);
  check_lacks "second trailers" out "x-second"

let test_client_sends () =
  let request =
    Request.create
      ~headers:(Headers.of_list [ "host", "a"; "transfer-encoding", "chunked" ])
      `POST
      "/"
  in
  let results = ref [] in
  let errors = ref 0 in
  let c, body = client_reading ~request results errors in
  Body.Writer.write_string body "abc";
  List.iter
    (fun trailers ->
       expect_invalid "client" (fun () ->
         Body.Writer.close ~trailers:(Headers.of_list trailers) body))
    invalid_trailers;
  Body.Writer.close ~trailers:grpc_trailers body;
  let out = client_output c in
  check_contains "client trailers" out ("3\r\nabc\r\n" ^ sent);
  let request =
    Request.create
      ~headers:(Headers.of_list [ "host", "a"; "content-length", "3" ])
      `POST
      "/"
  in
  let c, body = client_reading ~request results errors in
  Body.Writer.write_string body "abc";
  expect_invalid "client content-length" (fun () ->
    Body.Writer.close ~trailers:grpc_trailers body);
  Body.Writer.close body;
  check_lacks "no trailers" (client_output c) "grpc";
  let c, body = client_reading results errors in
  Body.Writer.close ~trailers:grpc_trailers body;
  check_lacks "no request body" (client_output c) "grpc"

(* The trailer section has max_header_size bytes, across reads. *)
let config = { Config.default with max_header_size = 256 }

(* One field and the final CRLF, [total] bytes in all. *)
let trailer_section total = "x: " ^ String.make (total - 7) 'v' ^ "\r\n\r\n"

let test_server_too_large () =
  List.iter
    (fun (name, pieces) ->
       let results = ref [] in
       let out = serve_pieces ~config (server_reading results) pieces in
       Alcotest.(check int) name 1 (List.length !results);
       check_contains name out "HTTP/1.1 200 OK\r\n")
    (splits (chunked_post ("3\r\nabc\r\n0\r\n" ^ trailer_section 256)));
  List.iter
    (fun (name, pieces) ->
       let results = ref [] in
       let out = serve_pieces ~config (server_reading results) pieces in
       Alcotest.(check (list ending)) name [ Error ] !results;
       check_contains name out "HTTP/1.1 431 ")
    (splits (chunked_post ("3\r\nabc\r\n0\r\n" ^ trailer_section 257)))

let test_client_too_large () =
  List.iter
    (fun (total, expected, error_count) ->
       List.iter
         (fun (name, pieces) ->
            let name = Printf.sprintf "%d bytes, %s" total name in
            let results = ref [] in
            let errors = ref 0 in
            let c, body = client_reading ~config results errors in
            Body.Writer.close body;
            ignore (client_output c : string);
            client_feed_pieces c pieces;
            Alcotest.(check (list ending)) name expected !results;
            Alcotest.(check int) (name ^ ": errors") error_count !errors)
         (splits
            (chunked_response ("3\r\nabc\r\n0\r\n" ^ trailer_section total))))
    [ 256, [ Eof [ "x", String.make 249 'v' ] ], 0; 257, [ Error ], 1 ]

(* gRPC over HTTP/1.1: the status comes in a trailer. *)
let grpc_handler reqd =
  let request_body = Request_descriptor.request_body reqd in
  read_body request_body ~on_end:(fun _ ->
    let body =
      Request_descriptor.respond_with_streaming
        reqd
        (Response.create
           ~headers:(Headers.of_list [ "content-type", "application/grpc" ])
           `OK)
    in
    Body.Writer.write_string body "\000\000\000\000\002hi";
    Body.Writer.close
      ~trailers:
        (Headers.of_list [ "grpc-status", "0"; "grpc-message", "all good" ])
      body)

let test_grpc () =
  let request =
    Request.create
      ~headers:
        (Headers.of_list
           [ "host", "a"
           ; "content-type", "application/grpc"
           ; "te", "trailers"
           ; "transfer-encoding", "chunked"
           ])
      `POST
      "/helloworld.Greeter/SayHello"
  in
  let c = Client_connection.create () in
  let status = ref None in
  let errors = ref 0 in
  let body =
    Client_connection.request
      c
      request
      ~error_handler:(fun _ -> incr errors)
      ~response_handler:(fun _ body ->
        let rec next () =
          Body.Reader.schedule_read
            body
            ~on_eof:(fun () ->
              status := Headers.get (Body.Reader.trailers body) "grpc-status")
            ~on_error:ignore
            ~on_read:(fun _ ~off:_ ~len:_ -> next ())
        in
        next ())
  in
  Body.Writer.write_string body "\000\000\000\000\002hi";
  Body.Writer.close ~trailers:(Headers.of_list [ "x-request-id", "7" ]) body;
  let request_bytes = client_output c in
  check_contains "request trailer" request_bytes "0\r\nx-request-id: 7\r\n\r\n";
  let response_bytes = ref "" in
  List.iter
    (fun (name, pieces) ->
       let out = serve_pieces grpc_handler pieces in
       check_contains name out "content-type: application/grpc\r\n";
       check_contains name out "0\r\ngrpc-status: 0\r\n";
       response_bytes := out)
    (splits request_bytes);
  List.iter
    (fun (name, pieces) ->
       let c = Client_connection.create () in
       let status = ref None in
       ignore
         (Client_connection.request
            c
            request
            ~error_handler:(fun _ -> incr errors)
            ~response_handler:(fun _ body ->
              read_body body ~on_end:(function
                | Eof trailers ->
                  status := List.assoc_opt "grpc-status" trailers
                | Error -> ())));
       ignore (client_output c : string);
       client_feed_pieces c pieces;
       Alcotest.(check (option string)) name (Some "0") !status)
    (splits !response_bytes);
  client_feed_pieces c [ !response_bytes ];
  Alcotest.(check (option string)) "grpc-status" (Some "0") !status;
  Alcotest.(check int) "errors" 0 !errors

let tests =
  [ "server receives trailers at every split", `Quick, test_server_receives
  ; "server body without trailers", `Quick, test_server_no_trailers
  ; "client receives trailers at every split", `Quick, test_client_receives
  ; "server sends trailers only with TE: trailers", `Quick, test_te_gating
  ; "TE over several field lines", `Quick, test_te_multiple_lines
  ; "invalid and forbidden trailers raise", `Quick, test_send_forbidden
  ; "trailers on a content-length body raise", `Quick, test_send_content_length
  ; "trailers dropped without content or length", `Quick, test_send_dropped
  ; "second close drops its trailers", `Quick, test_close_twice
  ; "client sends trailers", `Quick, test_client_sends
  ; ( "server: trailer section over max_header_size"
    , `Quick
    , test_server_too_large )
  ; ( "client: trailer section over max_header_size"
    , `Quick
    , test_client_too_large )
  ; "gRPC status in a trailer", `Quick, test_grpc
  ]
