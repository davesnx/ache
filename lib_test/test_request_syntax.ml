open Ache

type exchange =
  { request : Request.t
  ; mutable body : string
  }

type served =
  { output : string
  ; exchanges : exchange list
  ; errors : (Server_connection.error * Request.t option) list
  ; read_closed : bool
  ; write_closed : bool
  }

type mode =
  | Immediate
  | Deferred
  | Streaming

let serve ?config ?(chunk = max_int) ?(mode = Immediate) ?(eof = false) input =
  let exchanges = ref [] in
  let errors = ref [] in
  let later = ref [] in
  let ok reqd =
    match (Reqd.request reqd).meth with
    | `CONNECT -> Response.create `OK
    | _ ->
      Response.create ~headers:(Headers.of_list [ "content-length", "0" ]) `OK
  in
  let respond reqd =
    match mode with
    | Immediate -> Reqd.respond_with_string reqd (ok reqd) ""
    | Deferred ->
      later := (fun () -> Reqd.respond_with_string reqd (ok reqd) "") :: !later
    | Streaming ->
      let body =
        Reqd.respond_with_streaming
          ~flush_headers_immediately:true
          reqd
          (Response.create `OK)
      in
      later := (fun () -> Body.Writer.close body) :: !later
  in
  let request_handler reqd =
    let ex = { request = Reqd.request reqd; body = "" } in
    exchanges := ex :: !exchanges;
    let body = Reqd.request_body reqd in
    let rec loop () =
      Body.Reader.schedule_read
        body
        ~on_eof:(fun () ->
          match Reqd.error_code reqd with None -> respond reqd | Some _ -> ())
        ~on_read:(fun bs ~off ~len ->
          ex.body <- ex.body ^ Bigstringaf.substring bs ~off ~len;
          loop ())
    in
    loop ()
  in
  let error_handler ?request error respond =
    errors := (error, request) :: !errors;
    let body = respond Headers.empty in
    Body.Writer.write_string body "error";
    Body.Writer.close body
  in
  let conn = Server_connection.create ?config ~error_handler request_handler in
  let pending = ref input in
  let eof_sent = ref false in
  let output = Buffer.create 256 in
  let read_closed = ref false in
  let write_closed = ref false in
  let reader_parked = ref false in
  let writer_parked = ref false in
  let stalled = ref false in
  let woken = ref false in
  let read_step () =
    match Server_connection.next_read_operation conn with
    | `Read ->
      let len = min chunk (String.length !pending) in
      if len = 0 && eof && not !eof_sent
      then (
        eof_sent := true;
        ignore
          (Server_connection.read_eof conn Bigstringaf.empty ~off:0 ~len:0
           : int);
        true)
      else
        len > 0
        &&
        let bs = Bigstringaf.of_string !pending ~off:0 ~len in
        let n = Server_connection.read conn bs ~off:0 ~len in
        pending := String.sub !pending n (String.length !pending - n);
        let progress = n > 0 || not !stalled in
        stalled := n = 0;
        progress
    | `Yield ->
      if not !reader_parked
      then (
        reader_parked := true;
        Server_connection.yield_reader conn (fun () ->
          reader_parked := false;
          woken := true));
      false
    | `Close ->
      read_closed := true;
      false
  in
  let write_step () =
    match Server_connection.next_write_operation conn with
    | `Write iovecs ->
      let s = Helpers.Write_operation.iovecs_to_string iovecs in
      Buffer.add_string output s;
      Server_connection.report_write_result conn (`Ok (String.length s));
      stalled := false;
      true
    | `Yield ->
      if not !writer_parked
      then (
        writer_parked := true;
        Server_connection.yield_writer conn (fun () ->
          writer_parked := false;
          woken := true));
      false
    | `Close _ ->
      write_closed := true;
      false
  in
  let rec pump () =
    woken := false;
    let r = read_step () in
    let w = write_step () in
    if r || w || !woken then pump ()
  in
  let rec run () =
    pump ();
    match List.rev !later with
    | [] -> ()
    | actions ->
      later := [];
      List.iter (fun f -> f ()) actions;
      run ()
  in
  run ();
  { output = Buffer.contents output
  ; exchanges = List.rev !exchanges
  ; errors = List.rev !errors
  ; read_closed = !read_closed
  ; write_closed = !write_closed
  }

let status_line output =
  match String.index_opt output '\r' with
  | Some i -> String.sub output 0 i
  | None -> output

let statuses output =
  let rec go i acc =
    match String.index_from_opt output i 'H' with
    | None -> List.rev acc
    | Some j ->
      if j + 12 <= String.length output && String.sub output j 9 = "HTTP/1.1 "
      then go (j + 12) (int_of_string (String.sub output (j + 9) 3) :: acc)
      else go (j + 1) acc
  in
  go 0 []

let smuggled = "GET /smuggled HTTP/1.1\r\nHost: a\r\n\r\n"

let check_rejected ?config ?(chunk = max_int) ~status input =
  let msg = Grammar_corpus.summary (Printf.sprintf "%S" input) in
  let s = serve ?config ~chunk (input ^ smuggled) in
  Alcotest.(check (list int))
    (msg ^ ": statuses")
    [ status ]
    (statuses s.output);
  Alcotest.(check int)
    (msg ^ ": request handler not called")
    0
    (List.length s.exchanges);
  Alcotest.(check bool) (msg ^ ": reader closed") true s.read_closed;
  Alcotest.(check bool) (msg ^ ": writer closed") true s.write_closed

let first_requests =
  [ "GET /first HTTP/1.1\r\nHost: a\r\n\r\n"
  ; "POST /first HTTP/1.1\r\nHost: a\r\nContent-Length: 5\r\n\r\nhello"
  ]

let check_rejected_after_first ?config ~chunk ~mode ~status first input =
  let msg =
    Printf.sprintf
      "%S then %s (chunk %d, %s)"
      first
      (Grammar_corpus.summary (Printf.sprintf "%S" input))
      chunk
      (match mode with
      | Immediate -> "immediate"
      | Deferred -> "deferred"
      | Streaming -> "streaming")
  in
  let s = serve ?config ~chunk ~mode (first ^ input ^ smuggled) in
  Alcotest.(check (list int))
    (msg ^ ": statuses")
    [ 200; status ]
    (statuses s.output);
  Alcotest.(check (list string))
    (msg ^ ": request handler called for the first request only")
    [ "/first" ]
    (List.map (fun ex -> ex.request.target) s.exchanges);
  Alcotest.(check bool) (msg ^ ": reader closed") true s.read_closed;
  Alcotest.(check bool) (msg ^ ": writer closed") true s.write_closed

let check_rejected_both ?config ~status input =
  List.iter
    (fun chunk ->
       check_rejected ?config ~chunk ~status input;
       List.iter
         (fun first ->
            List.iter
              (fun mode ->
                 check_rejected_after_first
                   ?config
                   ~chunk
                   ~mode
                   ~status
                   first
                   input)
              [ Immediate; Deferred; Streaming ])
         first_requests)
    [ max_int; 1 ]

let only_exchange s =
  match s.exchanges with
  | [ ex ] -> ex
  | exs ->
    Alcotest.failf
      "expected one request, got %d; output %S"
      (List.length exs)
      s.output

let accepted ?config ?chunk input =
  let s = serve ?config ?chunk input in
  Alcotest.(check string)
    (Printf.sprintf "%S: status" input)
    "HTTP/1.1 200 OK"
    (status_line s.output);
  only_exchange s

let header_list (r : Request.t) = Headers.to_list r.headers
let pairs = Alcotest.(list (pair string string))

let request_line_rejected () =
  List.iter
    (fun chunk ->
       let s = serve ~chunk "GET / \r\n\r\n" in
       Alcotest.(check (list int))
         "no version and no further input"
         [ 400 ]
         (statuses s.output);
       Alcotest.(check bool) "reader closed" true s.read_closed)
    [ 1; max_int ];
  List.iter
    (fun (status, input) -> check_rejected_both ~status input)
    [ 400, "GET / \r\n\r\n"
    ; 400, "GET /\r\n\r\n"
    ; 505, "GET / HTTP/9.9\r\nHost: a\r\n\r\n"
    ; 505, "GET / HTTP/2.0\r\nHost: a\r\n\r\n"
    ; 505, "GET / HTTP/0.9\r\nHost: a\r\n\r\n"
    ; 400, " / HTTP/1.1\r\nHost: a\r\n\r\n"
    ; 400, "GET  HTTP/1.1\r\nHost: a\r\n\r\n"
    ; 400, "GET / HTTP/1.1\nHost: a\n\n"
    ; 400, "GE(T / HTTP/1.1\r\nHost: a\r\n\r\n"
    ; 400, "GET /\001 HTTP/1.1\r\nHost: a\r\n\r\n"
    ; 400, "GET /\127 HTTP/1.1\r\nHost: a\r\n\r\n"
    ]

let request_line_accepted () =
  List.iter
    (fun chunk ->
       let ex = accepted ~chunk "GET / HTTP/1.2\r\nHost: a\r\n\r\n" in
       Alcotest.(check string)
         "version"
         "HTTP/1.2"
         (Version.to_string ex.request.version))
    [ 1; max_int ]

let field_syntax_rejected () =
  List.iter
    (fun field ->
       check_rejected_both
         ~status:400
         ("GET / HTTP/1.1\r\nHost: a\r\n" ^ field ^ "\r\n"))
    [ "X-Invalid[]: t\r\n"
    ; ": t\r\n"
    ; "X\128: t\r\n"
    ; "Host : a\r\n"
    ; "X\t: a\r\n"
    ; "X: a\000b\r\n"
    ; "X: a\nb\r\n"
    ; "X: a\rb\r\n"
    ; "X: a\007b\r\n"
    ; "X: a\127b\r\n"
    ; "X: a\r\n b\r\n"
    ; "X: a\r\n\tb\r\n"
    ]

let field_syntax_accepted () =
  let ex = accepted "GET / HTTP/1.1\r\nHost: a\r\nX: caf\xc3\xa9\r\n\r\n" in
  Alcotest.check
    pairs
    "obs-text"
    [ "Host", "a"; "X", "caf\xc3\xa9" ]
    (header_list ex.request);
  let ex = accepted "GET / HTTP/1.1\r\nHost: a\r\nX:\t a b \t\r\n\r\n" in
  Alcotest.check
    pairs
    "OWS trimmed"
    [ "Host", "a"; "X", "a b" ]
    (header_list ex.request)

let host_rejected () =
  List.iter
    (fun input ->
       check_rejected_both ~status:400 input;
       let s = serve input in
       match s.errors with
       | [ (`Bad_request, Some r) ] ->
         Alcotest.(check string) "error handler gets the request" "/" r.target
       | _ -> Alcotest.failf "%S: expected one Bad_request with request" input)
    [ "GET / HTTP/1.1\r\n\r\n"
    ; "GET / HTTP/1.2\r\n\r\n"
    ; "GET / HTTP/1.1\r\nHost: a\r\nHost: b\r\n\r\n"
    ; "GET / HTTP/1.1\r\nHost: a\r\nHost: a\r\n\r\n"
    ; "GET / HTTP/1.0\r\nHost: a\r\nhost: a\r\n\r\n"
    ; "GET / HTTP/1.1\r\nHost: a b\r\n\r\n"
    ; "GET / HTTP/1.1\r\nHost: a/b\r\n\r\n"
    ; "GET / HTTP/1.1\r\nHost: u@a\r\n\r\n"
    ; "GET / HTTP/1.1\r\nHost: a?b\r\n\r\n"
    ; "GET / HTTP/1.1\r\nHost: a#b\r\n\r\n"
    ; "GET / HTTP/1.1\r\nHost: a:b:c\r\n\r\n"
    ; "GET / HTTP/1.1\r\nHost: a:x\r\n\r\n"
    ; "GET / HTTP/1.1\r\nHost: [::1\r\n\r\n"
    ; "GET / HTTP/1.1\r\nHost: [::1]:8080:1\r\n\r\n"
    ]

let host_accepted () =
  ignore (accepted "GET / HTTP/1.0\r\n\r\n" : exchange);
  ignore (accepted "GET http://a/ HTTP/1.1\r\nHost: a\r\n\r\n" : exchange);
  ignore (accepted "GET / HTTP/1.1\r\nHost: [::1]:8080\r\n\r\n" : exchange);
  ignore (accepted "GET / HTTP/1.1\r\nHost: \r\n\r\n" : exchange);
  ignore (accepted "GET / HTTP/1.1\r\nHost: a:\r\n\r\n" : exchange);
  ignore (accepted "GET / HTTP/1.1\r\nHost: [::1]\r\n\r\n" : exchange);
  ignore (accepted "GET / HTTP/1.1\r\nHost: 127.0.0.1:80\r\n\r\n" : exchange)

let client_request_with_two_hosts () =
  let conn = Client_connection.create () in
  let body =
    Client_connection.request
      conn
      ~flush_headers_immediately:true
      (Request.create
         ~headers:(Headers.of_list [ "Host", "a"; "Host", "b" ])
         `GET
         "/")
      ~error_handler:(fun _ -> ())
      ~response_handler:(fun _ _ -> ())
  in
  Body.Writer.close body;
  match Client_connection.next_write_operation conn with
  | `Write iovecs ->
    Alcotest.(check string)
      "request written"
      "GET / HTTP/1.1\r\nHost: a\r\nHost: b\r\n\r\n"
      (Helpers.Write_operation.iovecs_to_string iovecs)
  | _ -> Alcotest.fail "expected a write"

let connect_rejected () =
  List.iter
    (fun target ->
       check_rejected_both
         ~status:400
         ("CONNECT " ^ target ^ " HTTP/1.1\r\nHost: example.com\r\n\r\n"))
    [ "example.com:"
    ; "example.com"
    ; "example.com:http"
    ; "example.com:-1"
    ; "example.com:65536"
    ; "example.com:99999999999999999999"
    ; ":443"
    ; "/x"
    ; "u@example.com:443"
    ; "::::443"
    ; "a:b:443"
    ; "[::1:443"
    ; "[zz]:443"
    ; "[]:443"
    ; "[::1]x:443"
    ]

let connect_accepted () =
  List.iter
    (fun target ->
       let ex =
         accepted
           ("CONNECT " ^ target ^ " HTTP/1.1\r\nHost: " ^ target ^ "\r\n\r\n")
       in
       Alcotest.(check string) "target" target ex.request.target)
    [ "example.com:443"; "example.com:0"; "example.com:65535"; "[::1]:8080" ]

let framing_rejected () =
  List.iter
    (fun (status, input) -> check_rejected_both ~status input)
    [ ( 400
      , "POST / HTTP/1.1\r\n\
         Host: a\r\n\
         Content-Length: 3\r\n\
         Transfer-Encoding: chunked\r\n\
         \r\n\
         3\r\n\
         abc\r\n\
         0\r\n\
         \r\n" )
    ; 400, "POST / HTTP/1.1\r\nHost: a\r\nContent-Length: 0x3\r\n\r\nabc"
    ; 400, "POST / HTTP/1.1\r\nHost: a\r\nContent-Length: +3\r\n\r\nabc"
    ; 400, "POST / HTTP/1.1\r\nHost: a\r\nContent-Length: 1_0\r\n\r\nabc"
    ; 400, "POST / HTTP/1.1\r\nHost: a\r\nContent-Length: 3 3\r\n\r\nabc"
    ; ( 400
      , "POST / HTTP/1.1\r\n\
         Host: a\r\n\
         Content-Length: 3\r\n\
         Content-Length: 4\r\n\
         \r\n\
         abcd" )
    ; 400, "POST / HTTP/1.1\r\nHost: a\r\nContent-Length: 3, 4\r\n\r\nabcd"
    ; 400, "POST / HTTP/1.1\r\nHost: a\r\nContent-Length: 3,\r\n\r\nabc"
    ; ( 400
      , "POST / HTTP/1.1\r\n\
         Host: a\r\n\
         Content-Length: 99999999999999999999\r\n\
         \r\n" )
    ; ( 400
      , "POST / HTTP/1.1\r\n\
         Host: a\r\n\
         Content-Length: 9223372036854775808\r\n\
         \r\n" )
    ; ( 400
      , "POST / HTTP/1.0\r\n\
         Transfer-Encoding: chunked\r\n\
         \r\n\
         3\r\n\
         abc\r\n\
         0\r\n\
         \r\n" )
    ; ( 400
      , "POST / HTTP/1.1\r\nHost: a\r\nTransfer-Encoding: chunked, gzip\r\n\r\n"
      )
    ; 400, "POST / HTTP/1.1\r\nHost: a\r\nTransfer-Encoding: gzip\r\n\r\n"
    ; ( 400
      , "POST / HTTP/1.1\r\n\
         Host: a\r\n\
         Transfer-Encoding: chunked, chunked\r\n\
         \r\n" )
    ; ( 400
      , "POST / HTTP/1.1\r\nHost: a\r\nTransfer-Encoding: chunked;x=1\r\n\r\n" )
    ; ( 501
      , "POST / HTTP/1.1\r\n\
         Host: a\r\n\
         Transfer-Encoding: gzip\r\n\
         Transfer-Encoding: chunked\r\n\
         \r\n" )
    ; ( 501
      , "POST / HTTP/1.1\r\nHost: a\r\nTransfer-Encoding: gzip, chunked\r\n\r\n"
      )
    ]

let framing_accepted () =
  List.iter
    (fun (input, body) ->
       let ex = accepted input in
       Alcotest.(check string) (Printf.sprintf "%S: body" input) body ex.body)
    [ "POST / HTTP/1.1\r\nHost: a\r\nContent-Length: 3, 3\r\n\r\nabc", "abc"
    ; ( "POST / HTTP/1.1\r\n\
         Host: a\r\n\
         Content-Length: 3\r\n\
         Content-Length: 3\r\n\
         \r\n\
         abc"
      , "abc" )
    ; "POST / HTTP/1.1\r\nHost: a\r\nContent-Length: 003\r\n\r\nabc", "abc"
    ; ( "POST / HTTP/1.1\r\n\
         Host: a\r\n\
         Transfer-Encoding: Chunked\r\n\
         \r\n\
         3\r\n\
         abc\r\n\
         0\r\n\
         \r\n"
      , "abc" )
    ; ( "POST / HTTP/1.1\r\n\
         Host: a\r\n\
         Transfer-Encoding: ,chunked\r\n\
         \r\n\
         3\r\n\
         abc\r\n\
         0\r\n\
         \r\n"
      , "abc" )
    ]

let chunked_head =
  "POST / HTTP/1.1\r\nHost: a\r\nTransfer-Encoding: chunked\r\n\r\n"

let body_framing_rejected () =
  List.iter
    (fun (eof, input) ->
       List.iter
         (fun chunk ->
            let msg = Printf.sprintf "%S (chunk %d)" input chunk in
            let s = serve ~chunk ~eof input in
            Alcotest.(check (list int))
              (msg ^ ": statuses")
              [ 400 ]
              (statuses s.output);
            Alcotest.(check (list string))
              (msg ^ ": request handler called once")
              [ "/" ]
              (List.map (fun ex -> ex.request.target) s.exchanges);
            Alcotest.(check bool) (msg ^ ": reader closed") true s.read_closed;
            Alcotest.(check bool) (msg ^ ": writer closed") true s.write_closed)
         [ max_int; 1 ])
    [ false, chunked_head ^ "5\nhello\r\n0\r\n\r\n" ^ smuggled
    ; false, chunked_head ^ "5\r\nhelloXX0\r\n\r\n" ^ smuggled
    ; false, chunked_head ^ "5;a=\"x\r\ny\"\r\nhello\r\n0\r\n\r\n" ^ smuggled
    ; true, chunked_head ^ "100\r\n" ^ smuggled
    ]

let line_limits () =
  let config = { Config.default with max_header_size = 1024 } in
  List.iter
    (fun (status, input) -> check_rejected_both ~config ~status input)
    [ 501, String.make 2000 'M' ^ " / HTTP/1.1\r\nHost: a\r\n\r\n"
    ; 501, String.make 1024 'M'
    ; 414, "GET /" ^ String.make 2000 'a' ^ " HTTP/1.1\r\nHost: a\r\n\r\n"
    ; 414, "GET /" ^ String.make 1100 'a'
    ; ( 431
      , "GET / HTTP/1.1\r\nHost: a\r\nX: " ^ String.make 2000 'v' ^ "\r\n\r\n" )
    ; 431, "GET / HTTP/1.1\r\n" ^ String.make 2000 'X'
    ];
  ignore
    (accepted
       ~config
       ("GET /" ^ String.make 900 'a' ^ " HTTP/1.1\r\nHost: a\r\n\r\n")
     : exchange)

let target_form_rejected () =
  List.iter
    (fun input -> check_rejected_both ~status:400 input)
    [ "GET * HTTP/1.1\r\nHost: a\r\n\r\n"
    ; "POST * HTTP/1.1\r\nHost: a\r\n\r\n"
    ; "GET 127.0.0.1:80 HTTP/1.1\r\nHost: a\r\n\r\n"
    ; "POST [::1]:8080 HTTP/1.1\r\nHost: a\r\n\r\n"
    ; "OPTIONS 127.0.0.1:80 HTTP/1.1\r\nHost: a\r\n\r\n"
    ; "GET a/b HTTP/1.1\r\nHost: a\r\n\r\n"
    ; "GET :80 HTTP/1.1\r\nHost: a\r\n\r\n"
    ; "GET 1http://a/ HTTP/1.1\r\nHost: a\r\n\r\n"
    ; "GET ?x HTTP/1.1\r\nHost: a\r\n\r\n"
    ]

let target_form_accepted () =
  List.iter
    (fun (meth, target) ->
       let ex =
         accepted (meth ^ " " ^ target ^ " HTTP/1.1\r\nHost: a\r\n\r\n")
       in
       Alcotest.(check string) "target" target ex.request.target)
    [ "OPTIONS", "*"
    ; "OPTIONS", "/"
    ; "GET", "/a?b"
    ; "GET", "http://a/b"
    ; "GET", "HTTPS://a"
    ; "POST", "urn:x-y.z+1:a"
    ]

let content_length_shared () =
  let response cl =
    Response.create ~headers:(Headers.of_list [ "content-length", cl ]) `OK
  in
  List.iter
    (fun cl ->
       match Response.body_length ~request_method:`GET (response cl) with
       | `Error `Internal_server_error -> ()
       | len -> Alcotest.failf "%S: %a" cl Response.Body_length.pp_hum len)
    [ "+5"; "0x5"; "0_5"; "5 5"; "5, 6"; "-1"; ""; "0o5" ];
  List.iter
    (fun (cl, n) ->
       match Response.body_length ~request_method:`GET (response cl) with
       | `Fixed m -> Alcotest.(check int64) cl n m
       | len -> Alcotest.failf "%S: %a" cl Response.Body_length.pp_hum len)
    [ "5", 5L; "5, 5", 5L; "05", 5L ]

let connection_close_option () =
  List.iter
    (fun connection ->
       let s =
         serve
           ("GET / HTTP/1.1\r\nHost: a\r\nConnection: "
           ^ connection
           ^ "\r\n\r\n"
           ^ smuggled)
       in
       Alcotest.(check (list int)) connection [ 200 ] (statuses s.output);
       Alcotest.(check int) "one request" 1 (List.length s.exchanges);
       Alcotest.(check bool) "reader closed" true s.read_closed;
       Alcotest.(check bool) "writer closed" true s.write_closed)
    [ "Close"; "CLOSE"; "keep-alive, close"; "close, upgrade"; ", close" ];
  let s =
    serve
      ("GET / HTTP/1.1\r\n\
        Host: a\r\n\
        Connection: close\r\n\
        Connection: upgrade\r\n\
        \r\n"
      ^ smuggled)
  in
  Alcotest.(check int) "two lines: one request" 1 (List.length s.exchanges);
  List.iter
    (fun chunk ->
       let s =
         serve
           ~chunk
           "GET / HTTP/1.1\r\n\
            Host: a\r\n\
            Connection: close\r\n\
            \r\n\
            GET / HTTP/9.1\r\n\
            Host: a\r\n\
            \r\n"
       in
       Alcotest.(check (list int))
         "rejected request after close is not answered"
         [ 200 ]
         (statuses s.output);
       Alcotest.(check bool)
         "closed after close"
         true
         (s.read_closed && s.write_closed))
    [ max_int; 1 ];
  let s =
    serve ("GET / HTTP/1.0\r\nConnection: Keep-Alive\r\n\r\n" ^ smuggled)
  in
  Alcotest.(check int)
    "HTTP/1.0 Keep-Alive: two requests"
    2
    (List.length s.exchanges)

let smuggling () =
  List.iter
    (fun input ->
       List.iter
         (fun chunk ->
            let s = serve ~chunk (input ^ smuggled) in
            List.iter
              (fun ex ->
                 if ex.request.target = "/smuggled"
                 then Alcotest.failf "%S: /smuggled reached the handler" input)
              s.exchanges;
            Alcotest.(check bool)
              (Printf.sprintf "%S: connection closed" input)
              true
              (s.read_closed && s.write_closed))
         [ 1; max_int ])
    [ "POST / HTTP/1.1\r\nHost: a\r\nContent-Length: +5\r\n\r\nhello"
    ; "POST / HTTP/1.1\r\nHost: a\r\nContent-Length: 0x5\r\n\r\nhello"
    ; "POST / HTTP/1.1\r\nHost: a\r\nContent-Length: 0_5\r\n\r\nhello"
    ; "POST / HTTP/1.1\r\nHost: a\r\nContent-Length:  5 5\r\n\r\nhello"
    ; "POST / HTTP/1.1\r\nHost: a\r\nContent-Length: 5, 6\r\n\r\nhello"
    ; "POST / HTTP/1.1\r\n\
       Host: a\r\n\
       Content-Length: 5\r\n\
       Transfer-Encoding: chunked\r\n\
       \r\n\
       0\r\n\
       \r\n"
    ; "POST / HTTP/1.1\r\n\
       Host: a\r\n\
       Transfer-Encoding: chunked, identity\r\n\
       \r\n\
       0\r\n\
       \r\n"
    ; "POST / HTTP/1.1\r\n\
       Host: a\r\n\
       Transfer-Encoding: xchunked\r\n\
       \r\n\
       0\r\n\
       \r\n"
    ; "POST / HTTP/1.1\r\nHost: a\r\nX-A\r\nB: 1\r\n\r\n"
    ; "POST / HTTP/1.1\r\nHost: a\r\nX: a\nContent-Length: 5\r\n\r\nhello"
    ; "POST / HTTP/1.1\r\nHost: a\r\nX: a\rContent-Length: 5\r\n\r\nhello"
    ; "POST / HTTP/1.1\r\nHost: a\r\nX: a\000b\r\nContent-Length: 0\r\n\r\n"
    ; "POST / HTTP/1.1\r\nHost: a\r\nContent-Length : 5\r\n\r\nhello"
    ; "POST / HTTP/1.1\r\nHost: a\r\nX: a\r\n Content-Length: 5\r\n\r\nhello"
    ; "POST / HTTP/1.0\r\n\
       Transfer-Encoding: chunked\r\n\
       Content-Length: 5\r\n\
       \r\n\
       hello"
    ]

let tests =
  [ "request line rejected", `Quick, request_line_rejected
  ; "request line accepted", `Quick, request_line_accepted
  ; "field syntax rejected", `Quick, field_syntax_rejected
  ; "field syntax accepted", `Quick, field_syntax_accepted
  ; "host rejected", `Quick, host_rejected
  ; "host accepted", `Quick, host_accepted
  ; "client request with two hosts", `Quick, client_request_with_two_hosts
  ; "connect rejected", `Quick, connect_rejected
  ; "connect accepted", `Quick, connect_accepted
  ; "framing rejected", `Quick, framing_rejected
  ; "framing accepted", `Quick, framing_accepted
  ; "body framing rejected", `Quick, body_framing_rejected
  ; "request line limits", `Quick, line_limits
  ; "target form rejected", `Quick, target_form_rejected
  ; "target form accepted", `Quick, target_form_accepted
  ; "content-length shared", `Quick, content_length_shared
  ; "connection close option", `Quick, connection_close_option
  ; "smuggling", `Quick, smuggling
  ]
