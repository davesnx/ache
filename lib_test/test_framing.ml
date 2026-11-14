open Ache

type server =
  { conn : Server_connection.t
  ; out : Buffer.t
  ; mutable pending : string
  ; mutable read_op : [ `Read | `Yield | `Close ]
  ; mutable write_op : [ `Write | `Yield | `Close ]
  }

let rec pump s =
  let read_progress =
    match Server_connection.next_read_operation s.conn with
    | `Read ->
      s.read_op <- `Read;
      let len = String.length s.pending in
      len > 0
      &&
      let bs = Bigstringaf.of_string s.pending ~off:0 ~len in
      let n = Server_connection.read s.conn bs ~off:0 ~len in
      s.pending <- String.sub s.pending n (len - n);
      n > 0
    | (`Yield | `Close) as op ->
      s.read_op <- op;
      false
  in
  let write_progress =
    match Server_connection.next_write_operation s.conn with
    | `Write iovecs ->
      s.write_op <- `Write;
      let n =
        List.fold_left
          (fun n { IOVec.buffer; off; len } ->
             Buffer.add_string s.out (Bigstringaf.substring buffer ~off ~len);
             n + len)
          0
          iovecs
      in
      Server_connection.report_write_result s.conn (`Ok n);
      true
    | `Yield ->
      s.write_op <- `Yield;
      false
    | `Close _ ->
      s.write_op <- `Close;
      false
  in
  if read_progress || write_progress then pump s

let feed s input =
  s.pending <- s.pending ^ input;
  pump s

let server ?(error_handler = fun ?request:_ _ _ -> ()) handler input =
  let s =
    { conn = Server_connection.create ~error_handler handler
    ; out = Buffer.create 256
    ; pending = ""
    ; read_op = `Read
    ; write_op = `Yield
    }
  in
  feed s input;
  s

let req ?(version = "HTTP/1.1") ?(headers = []) meth =
  Printf.sprintf
    "%s %s %s\r\nHost: a\r\n%s\r\n"
    meth
    (if meth = "CONNECT" then "a:443" else "/")
    version
    (String.concat "" (List.map (fun (n, v) -> n ^ ": " ^ v ^ "\r\n") headers))

let response ?(headers = []) status =
  Response.create ~headers:(Headers.of_list headers) status

let with_string ?headers status body reqd =
  Reqd.respond_with_string reqd (response ?headers status) body

let with_bigstring ?headers status body reqd =
  Reqd.respond_with_bigstring
    reqd
    (response ?headers status)
    (Bigstringaf.of_string body ~off:0 ~len:(String.length body))

let flushed = ref []

let streaming ?headers status body reqd =
  let w = Reqd.respond_with_streaming reqd (response ?headers status) in
  Body.Writer.write_string w body;
  Body.Writer.flush w (fun r -> flushed := r :: !flushed);
  Body.Writer.close w

let state s =
  match s.read_op, s.write_op with
  | `Close, `Close -> "closed"
  | `Read, _ -> "open"
  | _ -> "other"

(* (request, handler, exact bytes written, connection state after) *)
let cases =
  [ ( "HEAD, string, app CL"
    , req "HEAD"
    , with_string ~headers:[ "content-length", "5" ] `OK "hello"
    , "HTTP/1.1 200 OK\r\ncontent-length: 5\r\n\r\n"
    , "open" )
  ; ( "HEAD, string, no CL"
    , req "HEAD"
    , with_string `OK "hello"
    , "HTTP/1.1 200 OK\r\n\r\n"
    , "open" )
  ; ( "HEAD, bigstring, app CL"
    , req "HEAD"
    , with_bigstring ~headers:[ "content-length", "5" ] `OK "hello"
    , "HTTP/1.1 200 OK\r\ncontent-length: 5\r\n\r\n"
    , "open" )
  ; ( "HEAD, streaming, app CL"
    , req "HEAD"
    , streaming ~headers:[ "content-length", "5" ] `OK "hello"
    , "HTTP/1.1 200 OK\r\ncontent-length: 5\r\n\r\n"
    , "open" )
  ; ( "HEAD, streaming, no CL"
    , req "HEAD"
    , streaming `OK "hello"
    , "HTTP/1.1 200 OK\r\n\r\n"
    , "open" )
  ; ( "GET 204, string"
    , req "GET"
    , with_string `No_content "hello"
    , "HTTP/1.1 204 No Content\r\n\r\n"
    , "open" )
  ; ( "GET 204, streaming"
    , req "GET"
    , streaming `No_content "hello"
    , "HTTP/1.1 204 No Content\r\n\r\n"
    , "open" )
  ; ( "GET 304, string, app CL"
    , req "GET"
    , with_string ~headers:[ "content-length", "5" ] `Not_modified "hello"
    , "HTTP/1.1 304 Not Modified\r\ncontent-length: 5\r\n\r\n"
    , "open" )
  ; ( "GET 304, streaming"
    , req "GET"
    , streaming `Not_modified "hello"
    , "HTTP/1.1 304 Not Modified\r\n\r\n"
    , "open" )
  ; ( "GET 200, string, no CL"
    , req "GET"
    , with_string `OK "hello"
    , "HTTP/1.1 200 OK\r\ncontent-length: 5\r\n\r\nhello"
    , "open" )
  ; ( "GET 200, bigstring, no CL"
    , req "GET"
    , with_bigstring `OK "hello"
    , "HTTP/1.1 200 OK\r\ncontent-length: 5\r\n\r\nhello"
    , "open" )
  ; ( "GET 200, string, app CL"
    , req "GET"
    , with_string ~headers:[ "content-length", "5" ] `OK "hello"
    , "HTTP/1.1 200 OK\r\ncontent-length: 5\r\n\r\nhello"
    , "open" )
  ; ( "GET 200, string, empty, no CL"
    , req "GET"
    , with_string `OK ""
    , "HTTP/1.1 200 OK\r\ncontent-length: 0\r\n\r\n"
    , "open" )
  ; ( "HTTP/1.0 GET 200, string, no CL"
    , req ~version:"HTTP/1.0" "GET"
    , with_string `OK "hello"
    , "HTTP/1.1 200 OK\r\ncontent-length: 5\r\nconnection: close\r\n\r\nhello"
    , "closed" )
  ; ( "GET 200, string, request Connection: close"
    , req ~headers:[ "Connection", "close" ] "GET"
    , with_string `OK "hello"
    , "HTTP/1.1 200 OK\r\ncontent-length: 5\r\nconnection: close\r\n\r\nhello"
    , "closed" )
  ; ( "HEAD 200, string, app CL, request Connection: close"
    , req ~headers:[ "Connection", "close" ] "HEAD"
    , with_string ~headers:[ "content-length", "5" ] `OK "hello"
    , "HTTP/1.1 200 OK\r\ncontent-length: 5\r\nconnection: close\r\n\r\n"
    , "closed" )
  ; ( "GET 204, string, request Connection: close"
    , req ~headers:[ "Connection", "close" ] "GET"
    , with_string `No_content ""
    , "HTTP/1.1 204 No Content\r\nconnection: close\r\n\r\n"
    , "closed" )
  ; ( "GET 200, string, app Connection: keep-alive, Close"
    , req "GET"
    , with_string ~headers:[ "connection", "keep-alive, Close" ] `OK "hello"
    , "HTTP/1.1 200 OK\r\n\
       connection: keep-alive, Close\r\n\
       content-length: 5\r\n\
       \r\n\
       hello"
    , "closed" )
  ; ( "GET 200, string, app Connection: x-a, request Connection: close"
    , req ~headers:[ "Connection", "close" ] "GET"
    , with_string ~headers:[ "connection", "x-a" ] `OK "hello"
    , "HTTP/1.1 200 OK\r\n\
       connection: x-a\r\n\
       content-length: 5\r\n\
       connection: close\r\n\
       \r\n\
       hello"
    , "closed" )
  ; ( "HTTP/1.0 GET 200, string, app Connection: close"
    , req ~version:"HTTP/1.0" "GET"
    , with_string ~headers:[ "connection", "close" ] `OK "hello"
    , "HTTP/1.1 200 OK\r\nconnection: close\r\ncontent-length: 5\r\n\r\nhello"
    , "closed" )
  ; ( "GET 200, streaming, app CL"
    , req "GET"
    , streaming ~headers:[ "content-length", "5" ] `OK "hello"
    , "HTTP/1.1 200 OK\r\ncontent-length: 5\r\n\r\nhello"
    , "open" )
  ; ( "GET 200, streaming, no CL, HTTP/1.1"
    , req "GET"
    , streaming `OK "hello"
    , "HTTP/1.1 200 OK\r\n\
       transfer-encoding: chunked\r\n\
       \r\n\
       5\r\n\
       hello\r\n\
       0\r\n\
       \r\n"
    , "open" )
  ; ( "GET 200, streaming, no CL, HTTP/1.0"
    , req ~version:"HTTP/1.0" "GET"
    , streaming `OK "hello"
    , "HTTP/1.1 200 OK\r\nconnection: close\r\n\r\nhello"
    , "closed" )
  ; ( "GET 200, streaming, no CL, request Connection: close"
    , req ~headers:[ "Connection", "close" ] "GET"
    , streaming `OK "hello"
    , "HTTP/1.1 200 OK\r\nconnection: close\r\n\r\nhello"
    , "closed" )
  ; ( "GET 200, streaming, no CL, app Connection: close"
    , req "GET"
    , streaming ~headers:[ "connection", "close" ] `OK "hello"
    , "HTTP/1.1 200 OK\r\nconnection: close\r\n\r\nhello"
    , "closed" )
  ; ( "GET 200, streaming, no CL, HTTP/1.0 keep-alive"
    , req ~version:"HTTP/1.0" ~headers:[ "Connection", "keep-alive" ] "GET"
    , streaming `OK "hello"
    , "HTTP/1.1 200 OK\r\nconnection: close\r\n\r\nhello"
    , "closed" )
  ; ( "GET 200, streaming, gzip transfer coding"
    , req "GET"
    , streaming ~headers:[ "transfer-encoding", "gzip" ] `OK "hello"
    , "HTTP/1.1 200 OK\r\n\
       transfer-encoding: gzip\r\n\
       connection: close\r\n\
       \r\n\
       hello"
    , "closed" )
  ; ( "CONNECT 200, string"
    , req "CONNECT"
    , with_string `OK "tunnel"
    , "HTTP/1.1 200 OK\r\n\r\ntunnel"
    , "other" )
  ; ( "CONNECT 200, streaming"
    , req "CONNECT"
    , streaming `OK "tunnel"
    , "HTTP/1.1 200 OK\r\n\r\ntunnel"
    , "other" )
  ; ( "CONNECT 404, string"
    , req "CONNECT"
    , with_string `Not_found "no"
    , "HTTP/1.1 404 Not Found\r\ncontent-length: 2\r\n\r\nno"
    , "open" )
  ; ( "CONNECT 403, streaming"
    , req "CONNECT"
    , streaming ~headers:[ "content-length", "2" ] `Forbidden "no"
    , "HTTP/1.1 403 Forbidden\r\ncontent-length: 2\r\n\r\nno"
    , "open" )
  ; ( "HTTP/1.0 GET 200, streaming, app chunked"
    , req ~version:"HTTP/1.0" "GET"
    , streaming ~headers:[ "transfer-encoding", "chunked" ] `OK "hello"
    , "HTTP/1.1 200 OK\r\nconnection: close\r\n\r\nhello"
    , "closed" )
  ; ( "HTTP/1.0 keep-alive GET 200, streaming, app chunked"
    , req ~version:"HTTP/1.0" ~headers:[ "Connection", "keep-alive" ] "GET"
    , streaming ~headers:[ "transfer-encoding", "chunked" ] `OK "hello"
    , "HTTP/1.1 200 OK\r\nconnection: close\r\n\r\nhello"
    , "closed" )
  ; ( "HTTP/1.0 keep-alive GET 200, string"
    , req ~version:"HTTP/1.0" ~headers:[ "Connection", "keep-alive" ] "GET"
    , with_string `OK "hello"
    , "HTTP/1.1 200 OK\r\n\
       content-length: 5\r\n\
       connection: keep-alive\r\n\
       \r\n\
       hello"
    , "open" )
  ; ( "HTTP/1.0 keep-alive GET 200, string, app Connection"
    , req ~version:"HTTP/1.0" ~headers:[ "Connection", "keep-alive" ] "GET"
    , with_string ~headers:[ "connection", "Keep-Alive" ] `OK "hello"
    , "HTTP/1.1 200 OK\r\n\
       connection: Keep-Alive\r\n\
       content-length: 5\r\n\
       \r\n\
       hello"
    , "open" )
  ; ( "GET 200, streaming, gzip then chunked"
    , req "GET"
    , streaming ~headers:[ "transfer-encoding", "gzip, chunked" ] `OK "hello"
    , "HTTP/1.1 200 OK\r\n\
       transfer-encoding: gzip, chunked\r\n\
       \r\n\
       5\r\n\
       hello\r\n\
       0\r\n\
       \r\n"
    , "open" )
  ; ( "GET 200, streaming, chunked then gzip"
    , req "GET"
    , streaming ~headers:[ "transfer-encoding", "chunked, gzip" ] `OK "hello"
    , "HTTP/1.1 200 OK\r\n\
       transfer-encoding: chunked, gzip\r\n\
       connection: close\r\n\
       \r\n\
       hello"
    , "closed" )
  ; ( "HEAD 200, streaming, app chunked"
    , req "HEAD"
    , streaming ~headers:[ "transfer-encoding", "chunked" ] `OK "hello"
    , "HTTP/1.1 200 OK\r\ntransfer-encoding: chunked\r\n\r\n"
    , "open" )
  ]

let each rows f =
  let failed =
    List.filter
      (fun row ->
         match f row with
         | () -> false
         | exception exn ->
           print_endline (Printexc.to_string exn);
           true)
      rows
  in
  if failed <> []
  then
    Alcotest.failf
      "%d of %d rows failed"
      (List.length failed)
      (List.length rows)

let test_table () =
  each cases (fun (label, input, handler, expected, expected_state) ->
    flushed := [];
    let s = server handler input in
    Alcotest.(check string) (label ^ ": bytes") expected (Buffer.contents s.out);
    Alcotest.(check string) (label ^ ": connection") expected_state (state s);
    List.iter
      (fun r ->
         Alcotest.(check bool)
           (label ^ ": flush answered `Written")
           true
           (r = `Written))
      !flushed)

let test_length_mismatch () =
  let raised = ref None in
  let handler reqd =
    match with_string ~headers:[ "content-length", "3" ] `OK "hello" reqd with
    | () -> ()
    | exception Invalid_argument msg -> raised := Some msg
  in
  let s = server handler (req "GET") in
  Alcotest.(check bool) "Invalid_argument" true (Option.is_some !raised);
  Alcotest.(check string) "nothing written" "" (Buffer.contents s.out)

let test_head_error_response () =
  let error_handler ?request:_ _ respond =
    let body = respond Headers.empty in
    Body.Writer.write_string body "error";
    Body.Writer.close body
  in
  let s =
    server
      ~error_handler
      (fun _ -> Alcotest.fail "handler called")
      (req ~headers:[ "Content-Length", "abc" ] "HEAD")
  in
  Alcotest.(check string)
    "head only"
    "HTTP/1.1 400 Bad Request\r\nconnection: close\r\n\r\n"
    (Buffer.contents s.out)

let test_pipelined_head_get () =
  let s =
    server
      (with_string ~headers:[ "content-length", "5" ] `OK "hello")
      (req "HEAD" ^ req "GET")
  in
  Alcotest.(check string)
    "GET head right after the HEAD head"
    "HTTP/1.1 200 OK\r\n\
     content-length: 5\r\n\
     \r\n\
     HTTP/1.1 200 OK\r\n\
     content-length: 5\r\n\
     \r\n\
     hello"
    (Buffer.contents s.out)

let tests =
  [ "server framing table", `Quick, test_table
  ; "Content-Length differs from the string", `Quick, test_length_mismatch
  ; "error response to HEAD has no body", `Quick, test_head_error_response
  ; "pipelined HEAD then GET", `Quick, test_pipelined_head_get
  ]

type client_result =
  { mutable statuses : int list
  ; mutable body : string
  ; mutable eof : bool
  ; mutable errors : int
  ; mutable read_op : [ `Read | `Yield | `Close ]
  }

let client ?(meth = `GET) input =
  let conn = Client_connection.create () in
  let r =
    { statuses = []; body = ""; eof = false; errors = 0; read_op = `Read }
  in
  let response_handler (response : Response.t) body =
    r.statuses <- r.statuses @ [ Status.to_code response.status ];
    let rec loop () =
      Body.Reader.schedule_read
        ~on_error:(Helpers.body_must_complete __LOC__)
        body
        ~on_eof:(fun () -> r.eof <- true)
        ~on_read:(fun bs ~off ~len ->
          r.body <- r.body ^ Bigstringaf.substring bs ~off ~len;
          loop ())
    in
    loop ()
  in
  let body =
    Client_connection.request
      conn
      (Request.create ~headers:(Headers.of_list [ "host", "a" ]) meth "/")
      ~error_handler:(fun _ -> r.errors <- r.errors + 1)
      ~response_handler
  in
  Body.Writer.close body;
  let rec drain () =
    match Client_connection.next_write_operation conn with
    | `Write iovecs ->
      Client_connection.report_write_result conn (`Ok (IOVec.lengthv iovecs));
      drain ()
    | `Yield | `Close _ -> ()
  in
  drain ();
  let rec feed input =
    match Client_connection.next_read_operation conn with
    | `Read when String.length input > 0 ->
      let len = String.length input in
      let bs = Bigstringaf.of_string input ~off:0 ~len in
      let n = Client_connection.read conn bs ~off:0 ~len in
      if n > 0 then feed (String.sub input n (len - n))
    | _ -> ()
  in
  feed input;
  r.read_op <- Client_connection.next_read_operation conn;
  r

let client_cases =
  [ ( "HEAD 200 with CL"
    , `HEAD
    , "HTTP/1.1 200 OK\r\ncontent-length: 5\r\n\r\n"
    , [ 200 ]
    , ""
    , true )
  ; ( "GET 204 with CL"
    , `GET
    , "HTTP/1.1 204 No Content\r\ncontent-length: 5\r\n\r\n"
    , [ 204 ]
    , ""
    , true )
  ; ( "GET 304 with chunked"
    , `GET
    , "HTTP/1.1 304 Not Modified\r\ntransfer-encoding: chunked\r\n\r\n"
    , [ 304 ]
    , ""
    , true )
  ; ( "100 then 200"
    , `GET
    , "HTTP/1.1 100 Continue\r\n\
       \r\n\
       HTTP/1.1 200 OK\r\n\
       content-length: 2\r\n\
       \r\n\
       ok"
    , [ 200 ]
    , "ok"
    , true )
  ; ( "103 with fields then 200"
    , `GET
    , "HTTP/1.1 103 Early Hints\r\n\
       Link: </a>\r\n\
       \r\n\
       HTTP/1.1 200 OK\r\n\
       content-length: 2\r\n\
       \r\n\
       ok"
    , [ 200 ]
    , "ok"
    , true )
  ; ( "102, 100, 199 then 204"
    , `POST
    , "HTTP/1.1 102 Processing\r\n\
       \r\n\
       HTTP/1.1 100 Continue\r\n\
       \r\n\
       HTTP/1.1 199 Odd\r\n\
       \r\n\
       HTTP/1.1 204 No Content\r\n\
       \r\n"
    , [ 204 ]
    , ""
    , true )
  ; ( "101 is final"
    , `GET
    , "HTTP/1.1 101 Switching Protocols\r\nupgrade: x\r\n\r\n"
    , [ 101 ]
    , ""
    , true )
  ; ( "CONNECT 200 with CL is a tunnel"
    , `CONNECT
    , "HTTP/1.1 200 OK\r\ncontent-length: 2\r\n\r\ntunnel bytes"
    , [ 200 ]
    , ""
    , false )
  ]

let test_client_table () =
  List.iter
    (fun (label, meth, input, statuses, body, eof) ->
       let r = client ~meth input in
       Alcotest.(check (list int)) (label ^ ": statuses") statuses r.statuses;
       Alcotest.(check string) (label ^ ": body") body r.body;
       Alcotest.(check bool) (label ^ ": body ended") eof r.eof;
       Alcotest.(check int) (label ^ ": errors") 0 r.errors)
    client_cases

let tests = tests @ [ "client framing table", `Quick, test_client_table ]

let expect_continue ?(version = "HTTP/1.1") () =
  req
    ~version
    ~headers:[ "Expect", "100-continue"; "Content-Length", "5" ]
    "POST"

let informational_then_final interim reqd =
  List.iter
    (fun status ->
       Reqd.respond_with_informational reqd (Response.create status))
    interim;
  let body = Reqd.request_body reqd in
  let received = Buffer.create 5 in
  let rec loop () =
    Body.Reader.schedule_read
      ~on_error:(Helpers.body_must_complete __LOC__)
      body
      ~on_eof:(fun () ->
        with_string `OK ("got " ^ Buffer.contents received) reqd)
      ~on_read:(fun bs ~off ~len ->
        Buffer.add_string received (Bigstringaf.substring bs ~off ~len);
        loop ())
  in
  loop ()

let test_continue () =
  let s =
    server (informational_then_final [ `Continue ]) (expect_continue ())
  in
  Alcotest.(check string)
    "100 before the body"
    "HTTP/1.1 100 Continue\r\n\r\n"
    (Buffer.contents s.out);
  feed s "hello";
  Alcotest.(check string)
    "final response after the body"
    "HTTP/1.1 100 Continue\r\n\
     \r\n\
     HTTP/1.1 200 OK\r\n\
     content-length: 9\r\n\
     \r\n\
     got hello"
    (Buffer.contents s.out);
  Alcotest.(check string) "connection open" "open" (state s)

let test_continue_http_1_0 () =
  let s =
    server
      (informational_then_final [ `Continue ])
      (expect_continue ~version:"HTTP/1.0" () ^ "hello")
  in
  Alcotest.(check string)
    "only the final response"
    "HTTP/1.1 200 OK\r\ncontent-length: 9\r\nconnection: close\r\n\r\ngot hello"
    (Buffer.contents s.out)

let test_several_informational () =
  let s =
    server
      (informational_then_final [ `Early_hints; `Continue ])
      (expect_continue () ^ "hello")
  in
  Alcotest.(check string)
    "103, 100, 200"
    "HTTP/1.1 103 Early Hints\r\n\
     \r\n\
     HTTP/1.1 100 Continue\r\n\
     \r\n\
     HTTP/1.1 200 OK\r\n\
     content-length: 9\r\n\
     \r\n\
     got hello"
    (Buffer.contents s.out)

let test_informational_misuse () =
  let only_raises label f =
    let outcome = ref "no exception" in
    let s =
      server
        (fun reqd ->
           match f reqd with
           | () -> ()
           | exception Invalid_argument _ -> outcome := "Invalid_argument")
        (req "GET")
    in
    Alcotest.(check string) (label ^ " raises") "Invalid_argument" !outcome;
    Alcotest.(check string)
      (label ^ " writes nothing")
      ""
      (Buffer.contents s.out)
  in
  only_raises "respond_with_string 100" (with_string `Continue "");
  only_raises "respond_with_bigstring 103" (with_bigstring `Early_hints "");
  only_raises "respond_with_streaming 100" (fun reqd ->
    ignore (Reqd.respond_with_streaming reqd (response `Continue)));
  only_raises "respond_with_string 199" (with_string (`Code 199) "");
  only_raises "respond_with_informational 200" (fun reqd ->
    Reqd.respond_with_informational reqd (response `OK));
  only_raises "respond_with_informational 101" (fun reqd ->
    Reqd.respond_with_informational reqd (response `Switching_protocols))

let tests =
  tests
  @ [ "100 Continue, then the final response", `Quick, test_continue
    ; "no 1xx to HTTP/1.0", `Quick, test_continue_http_1_0
    ; ( "103 and 100 before the final response"
      , `Quick
      , test_several_informational )
    ; "1xx misuse raises Invalid_argument", `Quick, test_informational_misuse
    ]

let test_pipelined_after_no_body () =
  List.iter
    (fun (label, first, first_status, first_head) ->
       flushed := [];
       let s =
         server
           (fun reqd ->
              match (Reqd.request reqd).meth with
              | `GET when !flushed = [] && first = "GET" ->
                streaming first_status "hello" reqd
              | `HEAD -> streaming first_status "hello" reqd
              | _ ->
                with_string ~headers:[ "content-length", "5" ] `OK "hello" reqd)
           (req first ^ req "GET")
       in
       Alcotest.(check string)
         (label ^ ": bytes")
         (first_head ^ "HTTP/1.1 200 OK\r\ncontent-length: 5\r\n\r\nhello")
         (Buffer.contents s.out);
       Alcotest.(check bool)
         (label ^ ": flush answered `Written")
         true
         (!flushed = [ `Written ]);
       Alcotest.(check string) (label ^ ": connection") "open" (state s))
    [ "HEAD", "HEAD", `OK, "HTTP/1.1 200 OK\r\n\r\n"
    ; "204", "GET", `No_content, "HTTP/1.1 204 No Content\r\n\r\n"
    ; "304", "GET", `Not_modified, "HTTP/1.1 304 Not Modified\r\n\r\n"
    ]

let test_connect_refused_then_next () =
  let s =
    server
      (fun reqd ->
         match (Reqd.request reqd).meth with
         | `CONNECT -> with_string `Forbidden "no" reqd
         | _ -> with_string `OK "ok" reqd)
      (req "CONNECT" ^ req "GET")
  in
  Alcotest.(check string)
    "403 then the GET response"
    "HTTP/1.1 403 Forbidden\r\n\
     content-length: 2\r\n\
     \r\n\
     noHTTP/1.1 200 OK\r\n\
     content-length: 2\r\n\
     \r\n\
     ok"
    (Buffer.contents s.out);
  Alcotest.(check string) "connection" "open" (state s)

let test_client_cl_and_te () =
  let chunked = "\r\n2\r\nok\r\n0\r\n\r\n" in
  let only_te =
    client ("HTTP/1.1 200 OK\r\ntransfer-encoding: chunked\r\n" ^ chunked)
  in
  let both =
    client
      ("HTTP/1.1 200 OK\r\ncontent-length: 5\r\ntransfer-encoding: chunked\r\n"
      ^ chunked)
  in
  List.iter
    (fun (label, r, read_op) ->
       Alcotest.(check string) (label ^ ": body") "ok" r.body;
       Alcotest.(check bool) (label ^ ": body ended") true r.eof;
       Alcotest.(check int) (label ^ ": errors") 0 r.errors;
       Alcotest.(check bool)
         (label ^ ": next read operation")
         true
         (r.read_op = read_op))
    [ "chunked only, connection kept", only_te, `Read
    ; "Content-Length and chunked, connection closed", both, `Close
    ]

let tests =
  tests
  @ [ ( "no-body response, then a pipelined GET"
      , `Quick
      , test_pipelined_after_no_body )
    ; ( "CONNECT refused, then a pipelined GET"
      , `Quick
      , test_connect_refused_then_next )
    ; "client: Content-Length with chunked", `Quick, test_client_cl_and_te
    ]

let test_client_cl_and_te_pipelined () =
  let run first_headers =
    let conn = Client_connection.create () in
    let written = Buffer.create 128 in
    let statuses = ref [] in
    let send target =
      let body =
        Client_connection.request
          conn
          (Request.create
             ~headers:(Headers.of_list [ "host", "a" ])
             `GET
             target)
          ~error_handler:(fun _ -> ())
          ~response_handler:(fun response body ->
            statuses := (target, Status.to_code response.status) :: !statuses;
            let rec loop () =
              Body.Reader.schedule_read
                ~on_error:(Helpers.body_must_complete __LOC__)
                body
                ~on_eof:ignore
                ~on_read:(fun _ ~off:_ ~len:_ -> loop ())
            in
            loop ())
      in
      Body.Writer.close body
    in
    send "/1";
    send "/2";
    let rec pump input =
      let wrote =
        match Client_connection.next_write_operation conn with
        | `Write iovecs ->
          List.iter
            (fun { IOVec.buffer; off; len } ->
               Buffer.add_string
                 written
                 (Bigstringaf.substring buffer ~off ~len))
            iovecs;
          Client_connection.report_write_result
            conn
            (`Ok (IOVec.lengthv iovecs));
          true
        | `Yield | `Close _ -> false
      in
      let input =
        match Client_connection.next_read_operation conn with
        | `Read when String.length input > 0 ->
          let len = String.length input in
          let bs = Bigstringaf.of_string input ~off:0 ~len in
          let n = Client_connection.read conn bs ~off:0 ~len in
          String.sub input n (len - n)
        | _ -> input
      in
      if wrote || (String.length input > 0 && input <> "")
      then
        if wrote
        then pump input
        else
          match Client_connection.next_read_operation conn with
          | `Read -> pump input
          | _ -> ()
    in
    pump
      ("HTTP/1.1 200 OK\r\n"
      ^ first_headers
      ^ "transfer-encoding: chunked\r\n\r\n2\r\nok\r\n0\r\n\r\n"
      ^ "HTTP/1.1 200 OK\r\ncontent-length: 0\r\n\r\n");
    List.rev !statuses, Client_connection.is_closed conn
  in
  let statuses, closed = run "" in
  Alcotest.(check (list (pair string int)))
    "chunked only: both answered"
    [ "/1", 200; "/2", 200 ]
    statuses;
  Alcotest.(check bool) "chunked only: open" false closed;
  let statuses, closed = run "content-length: 5\r\n" in
  Alcotest.(check (list (pair string int)))
    "CL and chunked: only the first answered"
    [ "/1", 200 ]
    statuses;
  Alcotest.(check bool) "CL and chunked: closed" true closed;
  let statuses, closed = run "connection: close\r\n" in
  Alcotest.(check (list (pair string int)))
    "Connection: close: only the first answered"
    [ "/1", 200 ]
    statuses;
  Alcotest.(check bool) "Connection: close: closed" true closed

let tests =
  tests
  @ [ ( "client: Content-Length with chunked, pipelined"
      , `Quick
      , test_client_cl_and_te_pipelined )
    ]

let ok_after reqd = with_string `OK "ok" reqd

let rejected =
  [ ( "string, app chunked"
    , req "GET"
    , with_string ~headers:[ "transfer-encoding", "chunked" ] `OK "hello" )
  ; ( "bigstring, app chunked"
    , req "GET"
    , with_bigstring ~headers:[ "transfer-encoding", "chunked" ] `OK "hello" )
  ; ( "string, app gzip"
    , req "GET"
    , with_string ~headers:[ "transfer-encoding", "gzip" ] `OK "hello" )
  ; ( "string, CL and TE"
    , req "GET"
    , with_string
        ~headers:[ "content-length", "5"; "transfer-encoding", "chunked" ]
        `OK
        "hello" )
  ; ( "streaming, CL and TE"
    , req "GET"
    , streaming
        ~headers:[ "content-length", "5"; "transfer-encoding", "chunked" ]
        `OK
        "hello" )
  ; ( "HEAD streaming, CL and TE"
    , req "HEAD"
    , streaming
        ~headers:[ "content-length", "5"; "transfer-encoding", "chunked" ]
        `OK
        "hello" )
  ; ( "204 string, app CL"
    , req "GET"
    , with_string ~headers:[ "content-length", "5" ] `No_content "" )
  ; ( "204 streaming, app TE"
    , req "GET"
    , streaming ~headers:[ "transfer-encoding", "chunked" ] `No_content "" )
  ; ( "CONNECT 200 string, app CL"
    , req "CONNECT"
    , with_string ~headers:[ "content-length", "6" ] `OK "tunnel" )
  ; ( "CONNECT 200 streaming, app TE"
    , req "CONNECT"
    , streaming ~headers:[ "transfer-encoding", "chunked" ] `OK "tunnel" )
  ; ( "string, invalid CL"
    , req "GET"
    , with_string ~headers:[ "content-length", "abc" ] `OK "ok" )
  ; ( "streaming, invalid CL"
    , req "GET"
    , streaming ~headers:[ "content-length", "-1" ] `OK "ok" )
  ; ( "streaming, two different CL"
    , req "GET"
    , streaming
        ~headers:[ "content-length", "1"; "content-length", "2" ]
        `OK
        "ok" )
  ; ( "103 informational, app CL"
    , req "GET"
    , fun reqd ->
        Reqd.respond_with_informational
          reqd
          (response ~headers:[ "content-length", "0" ] `Early_hints) )
  ]

let test_rejected () =
  each rejected (fun (label, input, handler) ->
    let outcome = ref "no exception" in
    let s =
      server
        (fun reqd ->
           match handler reqd with
           | () -> ()
           | exception Invalid_argument _ ->
             outcome := "Invalid_argument";
             if (Reqd.request reqd).meth <> `CONNECT then ok_after reqd)
        input
    in
    Alcotest.(check string)
      (Printf.sprintf "%s: raises (wrote %S)" label (Buffer.contents s.out))
      "Invalid_argument"
      !outcome;
    if not (String.starts_with ~prefix:"CONNECT" input)
    then
      Alcotest.(check string)
        (label ^ ": nothing written, reqd still usable")
        (if String.starts_with ~prefix:"HEAD" input
         then "HTTP/1.1 200 OK\r\n\r\n"
         else "HTTP/1.1 200 OK\r\ncontent-length: 2\r\n\r\nok")
        (Buffer.contents s.out)
    else
      Alcotest.(check string)
        (label ^ ": nothing written")
        ""
        (Buffer.contents s.out))

let test_invalid_content_length_message () =
  let raised = ref "" in
  let (_ : server) =
    server
      (fun reqd ->
         match
           Reqd.respond_with_string
             reqd
             (response ~headers:[ "content-length", "abc" ] `OK)
             "ok"
         with
         | () -> ()
         | exception Invalid_argument msg -> raised := msg)
      (req "GET")
  in
  Alcotest.(check string)
    "message"
    {|ache.Reqd.respond_with_string: invalid content-length "abc"|}
    !raised

let test_streaming_overflow () =
  let s =
    server
      (fun reqd ->
         match (Reqd.request reqd).target, (Reqd.request reqd).meth with
         | _, `POST ->
           let w =
             Reqd.respond_with_streaming
               reqd
               (response ~headers:[ "content-length", "3" ] `OK)
           in
           (match Body.Writer.write_string w "hello" with
           | () -> Alcotest.fail "write over the declared length accepted"
           | exception Invalid_argument _ -> ());
           Body.Writer.write_string w "hel";
           Body.Writer.close w
         | _ -> ok_after reqd)
      (req ~headers:[ "Content-Length", "0" ] "POST" ^ req "GET")
  in
  Alcotest.(check string)
    "3 bytes, then the next response"
    "HTTP/1.1 200 OK\r\n\
     content-length: 3\r\n\
     \r\n\
     helHTTP/1.1 200 OK\r\n\
     content-length: 2\r\n\
     \r\n\
     ok"
    (Buffer.contents s.out);
  Alcotest.(check string) "connection" "open" (state s)

let test_streaming_short () =
  let s =
    server
      (fun reqd ->
         match (Reqd.request reqd).meth with
         | `POST ->
           streaming ~headers:[ "content-length", "9" ] `OK "hello" reqd
         | _ -> ok_after reqd)
      (req ~headers:[ "Content-Length", "0" ] "POST" ^ req "GET")
  in
  Alcotest.(check string)
    "5 of 9 bytes, then close"
    "HTTP/1.1 200 OK\r\ncontent-length: 9\r\n\r\nhello"
    (Buffer.contents s.out);
  Alcotest.(check string) "connection" "closed" (state s)

let tests =
  tests
  @ [ "application framing fields rejected", `Quick, test_rejected
    ; ( "invalid content-length names the field"
      , `Quick
      , test_invalid_content_length_message )
    ; "streaming write over Content-Length", `Quick, test_streaming_overflow
    ; "streaming body short of Content-Length", `Quick, test_streaming_short
    ]
