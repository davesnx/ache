open Ache

type ex =
  { target : string
  ; writer : Body.Writer.t
  ; mutable statuses : int list
  ; mutable body : string
  ; mutable body_eof : bool
  ; mutable body_error : bool
  ; mutable errors : string list
  }

type conn =
  { c : Client_connection.t
  ; written : Buffer.t
  ; mutable pending : string
  ; mutable consumed : int
  ; mutable eof_sent : bool
  }

let error_to_string : Client_connection.error -> string = function
  | `Malformed_response _ -> "Malformed_response"
  | `Invalid_response_body_length _ -> "Invalid_response_body_length"
  | `Connection_closed -> "Connection_closed"
  | `Exn exn -> "Exn " ^ Printexc.to_string exn

let create () =
  { c = Client_connection.create ()
  ; written = Buffer.create 256
  ; pending = ""
  ; consumed = 0
  ; eof_sent = false
  }

let rec pump ?(eof = false) t =
  let wrote =
    match Client_connection.next_write_operation t.c with
    | `Write iovecs ->
      List.iter
        (fun { IOVec.buffer; off; len } ->
           Buffer.add_string
             t.written
             (Ache.Bigstring.substring buffer ~off ~len))
        iovecs;
      Client_connection.report_write_result t.c (`Ok (IOVec.lengthv iovecs));
      true
    | `Yield | `Close _ -> false
  in
  let read =
    match Client_connection.next_read_operation t.c with
    | `Read when eof && not t.eof_sent ->
      t.eof_sent <- true;
      let len = String.length t.pending in
      let bs = Ache.Bigstring.of_string t.pending ~off:0 ~len in
      let n = Client_connection.read_eof t.c bs ~off:0 ~len in
      t.consumed <- t.consumed + n;
      t.pending <- String.sub t.pending n (len - n);
      true
    | `Read when String.length t.pending > 0 ->
      let len = String.length t.pending in
      let bs = Ache.Bigstring.of_string t.pending ~off:0 ~len in
      let n = Client_connection.read t.c bs ~off:0 ~len in
      t.consumed <- t.consumed + n;
      t.pending <- String.sub t.pending n (len - n);
      n > 0
    | _ -> false
  in
  if wrote || read then pump ~eof t

let send
      ?(meth = `GET)
      ?(headers = [ "host", "a" ])
      ?body
      ?(close_body = true)
      t
      target
  =
  let rec ex =
    lazy
      { target
      ; writer =
          Client_connection.request
            t.c
            ~flush_headers_immediately:true
            (Request.create ~headers:(Headers.of_list headers) meth target)
            ~error_handler:(fun error ->
              let ex = Lazy.force ex in
              ex.errors <- ex.errors @ [ error_to_string error ])
            ~response_handler:(fun response reader ->
              let ex = Lazy.force ex in
              ex.statuses <- ex.statuses @ [ Status.to_code response.status ];
              let rec loop () =
                Body.Reader.schedule_read
                  reader
                  ~on_error:(fun () -> ex.body_error <- true)
                  ~on_eof:(fun () -> ex.body_eof <- true)
                  ~on_read:(fun bs ~off ~len ->
                    ex.body <- ex.body ^ Ache.Bigstring.substring bs ~off ~len;
                    loop ())
              in
              loop ())
      ; statuses = []
      ; body = ""
      ; body_eof = false
      ; body_error = false
      ; errors = []
      }
  in
  let ex = Lazy.force ex in
  Option.iter (Body.Writer.write_string ex.writer) body;
  if close_body then Body.Writer.close ex.writer;
  pump t;
  ex

let feed ~bytewise t input =
  if bytewise
  then
    String.iter
      (fun c ->
         t.pending <- t.pending ^ String.make 1 c;
         pump t)
      input
  else (
    t.pending <- t.pending ^ input;
    pump t)

let close_input t = pump ~eof:true t

let take_written t =
  let s = Buffer.contents t.written in
  Buffer.clear t.written;
  s

let closed t = Client_connection.is_closed t.c

let ok ?(headers = "") body =
  Printf.sprintf
    "HTTP/1.1 200 OK\r\n%sContent-Length: %d\r\n\r\n%s"
    headers
    (String.length body)
    body

let modes = [ "one read", false; "one byte per read", true ]

let both name f =
  ( name
  , `Quick
  , fun () ->
      List.iter
        (fun (mode, bytewise) ->
           try f ~bytewise with
           | exn -> Alcotest.failf "[%s] %s" mode (Printexc.to_string exn))
        modes )

let check_ex label ?(statuses = []) ?(body = "") ?(errors = []) ex =
  Alcotest.(check (list int)) (label ^ ": statuses") statuses ex.statuses;
  Alcotest.(check string) (label ^ ": body") body ex.body;
  Alcotest.(check (list string)) (label ^ ": errors") errors ex.errors

let check_closed label t = Alcotest.(check bool) label true (closed t)

let unsolicited ~bytewise =
  let t = create () in
  feed ~bytewise t (ok "x");
  check_closed "closed" t

let second_response ~bytewise =
  let t = create () in
  let ex = send t "/" in
  feed ~bytewise t (ok "r1" ^ ok "r2");
  check_ex "first" ~statuses:[ 200 ] ~body:"r1" ex;
  check_closed "closed" t

let crlf_between ~bytewise =
  let t = create () in
  let ex1 = send t "/1" in
  let ex2 = send t "/2" in
  feed ~bytewise t ("\r\n" ^ ok "r1" ^ "\r\n\r\n" ^ ok "r2" ^ "\r\n");
  check_ex "first" ~statuses:[ 200 ] ~body:"r1" ex1;
  check_ex "second" ~statuses:[ 200 ] ~body:"r2" ex2;
  Alcotest.(check bool) "open" false (closed t)

let no_pipelining_after_post ~bytewise =
  let t = create () in
  let ex1 =
    send
      ~meth:`POST
      ~headers:[ "host", "a"; "content-length", "3" ]
      ~body:"abc"
      t
      "/1"
  in
  let ex2 = send t "/2" in
  let ex3 = send t "/3" in
  Alcotest.(check string)
    "only the POST written"
    "POST /1 HTTP/1.1\r\nhost: a\r\ncontent-length: 3\r\n\r\nabc"
    (take_written t);
  feed ~bytewise t (ok "r1");
  check_ex "POST" ~statuses:[ 200 ] ~body:"r1" ex1;
  Alcotest.(check string)
    "both GETs written after the POST response"
    "GET /2 HTTP/1.1\r\nhost: a\r\n\r\nGET /3 HTTP/1.1\r\nhost: a\r\n\r\n"
    (take_written t);
  feed ~bytewise t (ok "r2" ^ ok "r3");
  check_ex "GET 2" ~statuses:[ 200 ] ~body:"r2" ex2;
  check_ex "GET 3" ~statuses:[ 200 ] ~body:"r3" ex3

let early_close_stops_body ~bytewise =
  let t = create () in
  let ex =
    send
      ~meth:`POST
      ~headers:[ "host", "a"; "content-length", "6" ]
      ~body:"abc"
      ~close_body:false
      t
      "/"
  in
  ignore (take_written t);
  feed
    ~bytewise
    t
    "HTTP/1.1 413 Content Too Large\r\n\
     Connection: close\r\n\
     Content-Length: 0\r\n\
     \r\n";
  Body.Writer.write_string ex.writer "def";
  Body.Writer.close ex.writer;
  pump t;
  check_ex "POST" ~statuses:[ 413 ] ex;
  Alcotest.(check string) "rest of the body not written" "" (take_written t);
  check_closed "closed" t

let invalid_request_then_valid ~bytewise =
  let t = create () in
  (match send ~headers:[ "host", "a"; "x-a", "a\r\nb: 1" ] t "/bad" with
  | _ -> Alcotest.fail "invalid request accepted"
  | exception Invalid_argument _ -> ());
  let ex = send t "/good" in
  Alcotest.(check string)
    "valid request written"
    "GET /good HTTP/1.1\r\nhost: a\r\n\r\n"
    (take_written t);
  feed ~bytewise t (ok "ok");
  check_ex "valid request" ~statuses:[ 200 ] ~body:"ok" ex

let http_1_0_with_transfer_encoding ~bytewise =
  List.iter
    (fun extra ->
       let t = create () in
       let ex1 = send t "/1" in
       let ex2 = send t "/2" in
       feed
         ~bytewise
         t
         ("HTTP/1.0 200 OK\r\nConnection: keep-alive\r\n"
         ^ extra
         ^ "Transfer-Encoding: chunked\r\n\r\n3\r\nabc\r\n0\r\n\r\n");
       check_ex "first" ~statuses:[ 200 ] ~body:"abc" ex1;
       check_ex "second" ~errors:[ "Connection_closed" ] ex2;
       check_closed "closed" t)
    [ ""; "Content-Length: 3\r\n" ]

let pending_after_close_option ~bytewise =
  let t = create () in
  let ex1 = send t "/1" in
  let ex2 = send t "/2" in
  let ex3 = send t "/3" in
  feed ~bytewise t (ok ~headers:"Connection: close\r\n" "ok");
  check_ex "first" ~statuses:[ 200 ] ~body:"ok" ex1;
  check_ex "second" ~errors:[ "Connection_closed" ] ex2;
  check_ex "third" ~errors:[ "Connection_closed" ] ex3;
  check_closed "closed" t;
  close_input t;
  check_ex "second, after EOF" ~errors:[ "Connection_closed" ] ex2

let eof_cases =
  [ "no response", "", [], "", [ "Connection_closed" ]
  ; "EOF after one response", ok "r1", [ 200 ], "r1", []
  ; ( "EOF inside a head"
    , "HTTP/1.1 200 OK\r\nContent-Le"
    , []
    , ""
    , [ "Malformed_response" ] )
  ; ( "EOF inside a fixed body"
    , "HTTP/1.1 200 OK\r\nContent-Length: 5\r\n\r\nab"
    , [ 200 ]
    , "ab"
    , [ "Malformed_response" ] )
  ; ( "EOF inside a chunked body"
    , "HTTP/1.1 200 OK\r\nTransfer-Encoding: chunked\r\n\r\n5\r\nab"
    , [ 200 ]
    , "ab"
    , [ "Malformed_response" ] )
  ; ( "close-delimited body ends at EOF"
    , "HTTP/1.1 200 OK\r\n\r\nabc"
    , [ 200 ]
    , "abc"
    , [] )
  ]

let pending_at_eof ~bytewise =
  List.iter
    (fun (label, input, statuses, body, errors) ->
       let t = create () in
       let ex1 = send t "/1" in
       let ex2 = send t "/2" in
       feed ~bytewise t input;
       close_input t;
       check_ex (label ^ ", first") ~statuses ~body ~errors ex1;
       check_ex (label ^ ", second") ~errors:[ "Connection_closed" ] ex2;
       check_closed (label ^ ": closed") t)
    eof_cases

let pending_at_shutdown ~bytewise:_ =
  let t = create () in
  let ex1 = send t "/1" in
  let ex2 = send t "/2" in
  Client_connection.shutdown t.c;
  pump t;
  check_ex "first" ~errors:[ "Connection_closed" ] ex1;
  check_ex "second" ~errors:[ "Connection_closed" ] ex2;
  check_closed "closed" t

let pending_after_malformed ~bytewise =
  let t = create () in
  let ex1 = send t "/1" in
  let ex2 = send t "/2" in
  feed ~bytewise t "HTTP/1.1 abc\r\n\r\n";
  check_ex "first" ~errors:[ "Malformed_response" ] ex1;
  check_ex "second" ~errors:[ "Connection_closed" ] ex2;
  check_closed "closed" t

let obs_fold ~bytewise =
  let t = create () in
  let ex = send t "/" in
  feed
    ~bytewise
    t
    "HTTP/1.1 200 OK\r\nX-A: a\r\n  b\r\n\tc\r\nContent-Length: 2\r\n\r\nok";
  check_ex "folded response" ~statuses:[ 200 ] ~body:"ok" ex

let obs_fold_value () =
  match
    Ache_private.Parse.Grammar.response
      "HTTP/1.1 200 OK\r\nX-A: a\r\n  b\r\n\tc\r\nX-B: d\r\n\r\n"
  with
  | Ok response ->
    Alcotest.(check (list (pair string string)))
      "fields"
      [ "X-A", "a b c"; "X-B", "d" ]
      (Headers.to_list response.headers)
  | Error e -> Alcotest.failf "rejected: %s" e

let obs_fold_request_rejected () =
  match
    Ache_private.Parse.Grammar.request
      "GET / HTTP/1.1\r\nHost: a\r\nX-A: a\r\n b\r\n\r\n"
  with
  | Ok _ -> Alcotest.fail "obs-fold accepted in a request"
  | Error _ -> ()

let upgrade_cases =
  [ ( "101"
    , `GET
    , [ "host", "a"; "connection", "upgrade"; "upgrade", "x" ]
    , "HTTP/1.1 101 Switching Protocols\r\n\
       Connection: upgrade\r\n\
       Upgrade: x\r\n\
       \r\n"
    , 101 )
  ; ( "2xx to CONNECT"
    , `CONNECT
    , [ "host", "a:443" ]
    , "HTTP/1.1 200 OK\r\nContent-Length: 5\r\n\r\n"
    , 200 )
  ]

let stops_after_upgrade ~bytewise =
  List.iter
    (fun (label, meth, headers, head, status) ->
       List.iter
         (fun trailing ->
            let t = create () in
            let target = if meth = `CONNECT then "a:443" else "/" in
            let ex = send ~meth ~headers t target in
            feed ~bytewise t (head ^ trailing);
            Alcotest.(check (list int))
              (label ^ ": status")
              [ status ]
              ex.statuses;
            Alcotest.(check (list string)) (label ^ ": errors") [] ex.errors;
            Alcotest.(check int)
              (Printf.sprintf "%s + %S: consumed" label trailing)
              (String.length head)
              t.consumed;
            Alcotest.(check string) (label ^ ": left") trailing t.pending;
            Alcotest.(check string) (label ^ ": body") "" ex.body)
         [ "ping"; "a b c\r\n"; "PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n"; ok "x" ])
    upgrade_cases

let body_end_cases =
  [ ( "fixed body cut by EOF"
    , "HTTP/1.1 200 OK\r\nContent-Length: 5\r\n\r\nhel"
    , `Eof
    , "hel"
    , false )
  ; ( "chunked body cut by EOF"
    , "HTTP/1.1 200 OK\r\nTransfer-Encoding: chunked\r\n\r\n3\r\nhel\r\n"
    , `Eof
    , "hel"
    , false )
  ; ( "chunked body with a framing error"
    , "HTTP/1.1 200 OK\r\nTransfer-Encoding: chunked\r\n\r\n3\r\nhelX"
    , `None
    , "hel"
    , false )
  ; ( "fixed body cut by shutdown"
    , "HTTP/1.1 200 OK\r\nContent-Length: 5\r\n\r\nhel"
    , `Shutdown
    , "hel"
    , false )
  ; ( "close-delimited body ends at EOF"
    , "HTTP/1.1 200 OK\r\n\r\nhello"
    , `Eof
    , "hello"
    , true )
  ]

let response_body_end ~bytewise =
  List.iter
    (fun (label, input, ending, body, complete) ->
       let t = create () in
       let ex = send t "/" in
       feed ~bytewise t input;
       (match ending with
       | `Eof -> close_input t
       | `Shutdown ->
         Client_connection.shutdown t.c;
         pump t
       | `None ->
         ignore
           (Client_connection.next_read_operation t.c
            : [ `Read | `Yield | `Close ]));
       Alcotest.(check string) (label ^ ": body") body ex.body;
       Alcotest.(check bool) (label ^ ": on_eof") complete ex.body_eof;
       Alcotest.(check bool) (label ^ ": on_error") (not complete) ex.body_error)
    body_end_cases

let switching_cases =
  [ ( "Upgrade GET"
    , `GET
    , "/up"
    , [ "host", "a"; "upgrade", "x"; "connection", "upgrade" ]
    , "HTTP/1.1 101 Switching Protocols\r\n\
       Upgrade: x\r\n\
       Connection: upgrade\r\n\
       \r\n"
    , 101 )
  ; ( "CONNECT"
    , `CONNECT
    , "a:443"
    , [ "host", "a:443" ]
    , "HTTP/1.1 200 OK\r\n\r\n"
    , 200 )
  ]

let written_has t target =
  let s = Buffer.contents t.written in
  let n = String.length target in
  let rec go i =
    i + n <= String.length s && (String.sub s i n = target || go (i + 1))
  in
  go 0

let no_pipelining_behind_switch ~bytewise =
  List.iter
    (fun (label, meth, target, headers, head, status) ->
       let t = create () in
       let first = send ~meth ~headers t target in
       let second = send t "/second" in
       Alcotest.(check bool)
         (label ^ ": second not written before the response")
         false
         (written_has t "/second");
       feed ~bytewise t head;
       Client_connection.shutdown t.c;
       pump t;
       check_ex (label ^ ", first") ~statuses:[ status ] first;
       check_ex (label ^ ", second") ~errors:[ "Connection_closed" ] second;
       Alcotest.(check bool)
         (label ^ ": second never written")
         false
         (written_has t "/second"))
    switching_cases;
  let t = create () in
  let first =
    send
      ~headers:[ "host", "a"; "upgrade", "x"; "connection", "upgrade" ]
      t
      "/up"
  in
  let second = send t "/second" in
  feed ~bytewise t (ok "no");
  check_ex "Upgrade refused, first" ~statuses:[ 200 ] ~body:"no" first;
  Alcotest.(check bool)
    "Upgrade refused: second written"
    true
    (written_has t "/second");
  feed ~bytewise t (ok "r2");
  check_ex "Upgrade refused, second" ~statuses:[ 200 ] ~body:"r2" second

let report_exn_stages ~bytewise =
  List.iter
    (fun (label, input, statuses, body, errors, body_error) ->
       let t = create () in
       let ex =
         if label = "after 101"
         then
           send
             ~headers:[ "host", "a"; "upgrade", "x"; "connection", "upgrade" ]
             t
             "/"
         else send t "/"
       in
       feed ~bytewise t input;
       Client_connection.report_exn t.c Exit;
       pump t;
       check_ex label ~statuses ~body ~errors ex;
       Alcotest.(check bool)
         (label ^ ": body on_error")
         body_error
         ex.body_error)
    [ "before the response", "", [], "", [ "Exn Stdlib.Exit" ], false
    ; ( "mid-body"
      , "HTTP/1.1 200 OK\r\nContent-Length: 5\r\n\r\nhe"
      , [ 200 ]
      , "he"
      , [ "Exn Stdlib.Exit" ]
      , true )
    ; "after a complete response", ok "done", [ 200 ], "done", [], false
    ; ( "after 101"
      , "HTTP/1.1 101 Switching Protocols\r\n\
         Upgrade: x\r\n\
         Connection: upgrade\r\n\
         \r\n"
      , [ 101 ]
      , ""
      , []
      , false )
    ]

let request_body_flush_on_close ~bytewise:_ =
  List.iter
    (fun (label, close) ->
       let t = create () in
       let ex =
         send
           ~meth:`POST
           ~headers:[ "host", "a"; "content-length", "10" ]
           ~close_body:false
           t
           "/post"
       in
       ignore (take_written t : string);
       let results = ref [] in
       Body.Writer.write_string ex.writer "abc";
       Body.Writer.flush ex.writer (fun r -> results := r :: !results);
       close t;
       pump t;
       Alcotest.(check (list string))
         (label ^ ": flush answered once")
         [ "Closed" ]
         (List.map
            (function `Written -> "Written" | `Closed -> "Closed")
            !results))
    [ ("shutdown", fun t -> Client_connection.shutdown t.c)
    ; ("report_exn", fun t -> Client_connection.report_exn t.c Exit)
    ; ( "write fails"
      , fun t ->
          match Client_connection.next_write_operation t.c with
          | `Write _ -> Client_connection.report_write_result t.c `Closed
          | `Yield | `Close _ -> Alcotest.fail "expected a write" )
    ]

let request_after_peer_closed ~bytewise =
  let t = create () in
  let first = send t "/1" in
  feed ~bytewise t (ok "r1");
  close_input t;
  check_ex "first" ~statuses:[ 200 ] ~body:"r1" first;
  let errors = ref [] in
  match
    Client_connection.request
      t.c
      (Request.create ~headers:(Headers.of_list [ "host", "a" ]) `GET "/2")
      ~error_handler:(fun e -> errors := error_to_string e :: !errors)
      ~response_handler:(fun _ _ -> Alcotest.fail "response handler called")
  with
  | (_ : Body.Writer.t) ->
    Alcotest.(check (list string)) "second" [ "Connection_closed" ] !errors
  | exception exn -> Alcotest.failf "request raised %s" (Printexc.to_string exn)

let write_closed_after_101 ~bytewise =
  let t = create () in
  let ex =
    send ~headers:[ "host", "a"; "upgrade", "x"; "connection", "upgrade" ] t "/"
  in
  feed
    ~bytewise
    t
    "HTTP/1.1 101 Switching Protocols\r\n\
     Upgrade: x\r\n\
     Connection: upgrade\r\n\
     \r\n";
  Client_connection.report_write_result t.c `Closed;
  (match Client_connection.next_write_operation t.c with
  | `Yield ->
    (match Client_connection.yield_writer t.c ignore with
    | () -> ()
    | exception exn ->
      Alcotest.failf "yield_writer raised %s" (Printexc.to_string exn))
  | `Write _ | `Close _ -> ());
  check_ex "upgrade" ~statuses:[ 101 ] ex

let response_body_endings ~bytewise =
  List.iter
    (fun (case, input, finish, expected) ->
       let t = create () in
       let events = ref [] in
       let body = ref None in
       let rec record b =
         Body.Reader.schedule_read
           b
           ~on_eof:(fun () -> events := "eof" :: !events)
           ~on_error:(fun () -> events := "error" :: !events)
           ~on_read:(fun _ ~off:_ ~len:_ ->
             events := "read" :: !events;
             record b)
       in
       let request_body =
         Client_connection.request
           t.c
           ~flush_headers_immediately:true
           (Request.create ~headers:(Headers.of_list [ "host", "a" ]) `GET "/")
           ~error_handler:(fun _ -> ())
           ~response_handler:(fun _ b ->
             body := Some b;
             record b)
       in
       Body.Writer.close request_body;
       pump t;
       feed ~bytewise t input;
       finish t (Option.get !body);
       let ends = List.filter (fun e -> e <> "read") !events in
       Alcotest.(check (list string)) (case ^ ": one ending") [ expected ] ends;
       Alcotest.(check string)
         (case ^ ": ending is last")
         expected
         (List.hd !events);
       let before = List.length !events in
       record (Option.get !body);
       Client_connection.shutdown t.c;
       close_input t;
       Alcotest.(check int)
         (case ^ ": nothing after the ending")
         before
         (List.length !events))
    [ ( "complete"
      , "HTTP/1.1 200 OK\r\nContent-Length: 3\r\n\r\nabc"
      , (fun t _ -> close_input t)
      , "eof" )
    ; ( "close-delimited, complete at EOF"
      , "HTTP/1.1 200 OK\r\n\r\nabc"
      , (fun t _ -> close_input t)
      , "eof" )
    ; ( "cut by EOF"
      , "HTTP/1.1 200 OK\r\nContent-Length: 5\r\n\r\nab"
      , (fun t _ -> close_input t)
      , "error" )
    ; ( "framing error"
      , "HTTP/1.1 200 OK\r\nTransfer-Encoding: chunked\r\n\r\n3\r\nabcX"
      , (fun t _ ->
          ignore
            (Client_connection.next_read_operation t.c
             : [ `Read | `Yield | `Close ]))
      , "error" )
    ; ( "closed by the application"
      , "HTTP/1.1 200 OK\r\nContent-Length: 5\r\n\r\nab"
      , (fun t b ->
          Body.Reader.close b;
          feed ~bytewise t "cde")
      , "eof" )
    ; ( "connection shutdown"
      , "HTTP/1.1 200 OK\r\nContent-Length: 5\r\n\r\nab"
      , (fun t _ -> Client_connection.shutdown t.c)
      , "error" )
    ]

let request_after_write_closed_while_reading ~bytewise =
  let t = create () in
  let first = send t "/1" in
  feed ~bytewise t "HTTP/1.1 200 OK\r\nContent-Length: 4\r\n\r\nr1";
  Client_connection.report_write_result t.c `Closed;
  let errors = ref [] in
  let (_ : Body.Writer.t) =
    Client_connection.request
      t.c
      (Request.create ~headers:(Headers.of_list [ "host", "a" ]) `GET "/2")
      ~error_handler:(fun e -> errors := error_to_string e :: !errors)
      ~response_handler:(fun _ _ -> Alcotest.fail "response handler called")
  in
  Alcotest.(check (list string)) "no error before the reader stops" [] !errors;
  feed ~bytewise t "r1";
  Alcotest.(check (list string))
    "error once the first response is complete"
    [ "Connection_closed" ]
    !errors;
  close_input t;
  check_ex "first" ~statuses:[ 200 ] ~body:"r1r1" first;
  Alcotest.(check (list string))
    "second: one outcome"
    [ "Connection_closed" ]
    !errors

let tests =
  [ both "response body cut short calls on_error, not on_eof" response_body_end
  ; both "unsolicited response is an error, not an exception" unsolicited
  ; both "second response after the last one closes" second_response
  ; both "CRLF around responses is discarded" crlf_between
  ; both "no pipelining behind a POST" no_pipelining_after_post
  ; both "early Connection: close stops the request body" early_close_stops_body
  ; both "invalid request raises, next request works" invalid_request_then_valid
  ; both
      "HTTP/1.0 response with Transfer-Encoding closes"
      http_1_0_with_transfer_encoding
  ; both "pending requests after Connection: close" pending_after_close_option
  ; both "pending requests at EOF" pending_at_eof
  ; both "pending requests at shutdown" pending_at_shutdown
  ; both "pending requests after a malformed response" pending_after_malformed
  ; both "obs-fold in a response" obs_fold
  ; "obs-fold value", `Quick, obs_fold_value
  ; "obs-fold in a request is rejected", `Quick, obs_fold_request_rejected
  ; both "reader stops after 101 and 2xx to CONNECT" stops_after_upgrade
  ; both
      "no pipelining behind an Upgrade or CONNECT request"
      no_pipelining_behind_switch
  ; both "report_exn at each stage" report_exn_stages
  ; both
      "request body flush answered when the connection closes"
      request_body_flush_on_close
  ; both "request after the peer closed" request_after_peer_closed
  ; both
      "request after the write side closed, earlier response still read"
      request_after_write_closed_while_reading
  ; both "response body: one final ending" response_body_endings
  ; both "write side closed after a 101" write_closed_after_101
  ]
