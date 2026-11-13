open Ache

let iovecs_to_string = Helpers.Write_operation.iovecs_to_string

let error_to_string = function
  | `Bad_request -> "Bad_request"
  | `Not_implemented -> "Not_implemented"
  | `Http_version_not_supported -> "Http_version_not_supported"
  | `Uri_too_long -> "Uri_too_long"
  | `Bad_gateway -> "Bad_gateway"
  | `Internal_server_error -> "Internal_server_error"
  | `Request_header_fields_too_large -> "Request_header_fields_too_large"
  | `Request_timeout -> "Request_timeout"
  | `Exn exn -> Printexc.to_string exn

let request_summary (r : Request.t) = Grammar_corpus.request_to_string r
let response_summary (r : Response.t) = Grammar_corpus.response_to_string r

let read_body body ~on_done =
  let buf = Buffer.create 64 in
  let rec on_read bs ~off ~len =
    Buffer.add_string buf (Bigstringaf.substring bs ~off ~len);
    Body.Reader.schedule_read body ~on_error ~on_read ~on_eof
  and on_eof () = on_done (Buffer.contents buf)
  and on_error () = on_done ("incomplete: " ^ Buffer.contents buf) in
  Body.Reader.schedule_read body ~on_error ~on_read ~on_eof

module Feeder = struct
  type t =
    { mutable pending : string
    ; mutable consumed : int
    ; next_read_operation : unit -> [ `Read | `Yield | `Close ]
    ; read : Bigstringaf.t -> off:int -> len:int -> int
    ; read_eof : Bigstringaf.t -> off:int -> len:int -> int
    ; drain : unit -> unit
    }

  let offer t f =
    let len = String.length t.pending in
    let bs = Bigstringaf.of_string t.pending ~off:0 ~len in
    let n = f bs ~off:0 ~len in
    if n < 0 || n > len then Alcotest.failf "read consumed %d of %d bytes" n len;
    t.consumed <- t.consumed + n;
    t.pending <- String.sub t.pending n (len - n);
    n

  let rec pump t =
    match t.next_read_operation () with
    | `Read when t.pending <> "" ->
      if offer t t.read > 0 then pump t else t.drain ()
    | `Yield ->
      t.drain ();
      if t.next_read_operation () = `Read && t.pending <> "" then pump t
    | `Read | `Close -> t.drain ()

  let feed t chunk =
    t.pending <- t.pending ^ chunk;
    pump t

  let eof t =
    pump t;
    match t.next_read_operation () with
    | `Read ->
      ignore (offer t t.read_eof : int);
      pump t
    | `Yield | `Close -> ()
end

type server =
  { conn : Server_connection.t
  ; feeder : Feeder.t
  ; output : Buffer.t
  ; mutable requests : (string * string) list
  ; mutable errors : string list
  }

let server ?config () =
  let output = Buffer.create 64 in
  let rec s =
    lazy
      (let request_handler reqd =
         read_body (Reqd.request_body reqd) ~on_done:(fun body ->
           let s = Lazy.force s in
           s.requests <-
             (request_summary (Reqd.request reqd), body) :: s.requests;
           match Reqd.error_code reqd, Reqd.response reqd with
           | None, None ->
             Reqd.respond_with_string reqd (Response.create `OK) ""
           | _ -> ())
       in
       let error_handler ?request:_ error start_response =
         let s = Lazy.force s in
         s.errors <- error_to_string error :: s.errors;
         Body.Writer.close (start_response Headers.empty)
       in
       let conn =
         Server_connection.create ?config ~error_handler request_handler
       in
       let rec drain () =
         match Server_connection.next_write_operation conn with
         | `Write iovecs ->
           let out = iovecs_to_string iovecs in
           Buffer.add_string output out;
           Server_connection.report_write_result conn (`Ok (String.length out));
           drain ()
         | `Yield | `Close _ -> ()
       in
       let feeder =
         { Feeder.pending = ""
         ; consumed = 0
         ; next_read_operation =
             (fun () -> Server_connection.next_read_operation conn)
         ; read = Server_connection.read conn
         ; read_eof = Server_connection.read_eof conn
         ; drain
         }
       in
       { conn; feeder; output; requests = []; errors = [] })
  in
  Lazy.force s

type client =
  { cconn : Client_connection.t
  ; cfeeder : Feeder.t
  ; mutable responses : (string * string) list
  ; mutable cerrors : string list
  }

let client ?config ?(meth = `GET) () =
  let cconn = Client_connection.create ?config () in
  let rec drain () =
    match Client_connection.next_write_operation cconn with
    | `Write iovecs ->
      Client_connection.report_write_result cconn (`Ok (IOVec.lengthv iovecs));
      drain ()
    | `Yield | `Close _ -> ()
  in
  let cfeeder =
    { Feeder.pending = ""
    ; consumed = 0
    ; next_read_operation =
        (fun () -> Client_connection.next_read_operation cconn)
    ; read = Client_connection.read cconn
    ; read_eof = Client_connection.read_eof cconn
    ; drain
    }
  in
  let c = { cconn; cfeeder; responses = []; cerrors = [] } in
  let body =
    Client_connection.request
      cconn
      (Request.create meth "/")
      ~error_handler:(fun error ->
        let e =
          match error with
          | `Malformed_response msg -> "Malformed_response " ^ msg
          | `Invalid_response_body_length _ -> "Invalid_response_body_length"
          | `Connection_closed -> "Connection_closed"
          | `Exn exn -> Printexc.to_string exn
        in
        c.cerrors <- e :: c.cerrors)
      ~response_handler:(fun response body ->
        read_body body ~on_done:(fun data ->
          c.responses <- (response_summary response, data) :: c.responses))
  in
  Body.Writer.close body;
  drain ();
  c

let every_byte s = List.init (String.length s) (fun i -> String.make 1 s.[i])

let two_way s =
  List.init
    (String.length s + 1)
    (fun i -> [ String.sub s 0 i; String.sub s i (String.length s - i) ])

let sized n s =
  let rec go i acc =
    if i >= String.length s
    then List.rev acc
    else
      let len = min n (String.length s - i) in
      go (i + len) (String.sub s i len :: acc)
  in
  go 0 []

let all_splits s = ([ s ] :: every_byte s :: two_way s : string list list)
let request_body = Alcotest.(pair string string)

let run_server ?config ?(eof = false) chunks =
  let s = server ?config () in
  List.iter (Feeder.feed s.feeder) chunks;
  if eof then Feeder.eof s.feeder;
  s

let check_server ?config ?eof ~expect input =
  List.iteri
    (fun i chunks ->
       let s = run_server ?config ?eof chunks in
       let msg = Printf.sprintf "split %d of %S" i input in
       Alcotest.(check (list request_body)) msg expect (List.rev s.requests);
       Alcotest.(check (list string)) (msg ^ ": errors") [] s.errors;
       Alcotest.(check int)
         (msg ^ ": consumed")
         (String.length input)
         s.feeder.consumed)
    (all_splits input)

let run_client ?config ?meth ?(eof = false) chunks =
  let c = client ?config ?meth () in
  List.iter (Feeder.feed c.cfeeder) chunks;
  if eof then Feeder.eof c.cfeeder;
  c

let check_client ?eof ~expect input =
  List.iteri
    (fun i chunks ->
       let c = run_client ?eof chunks in
       let msg = Printf.sprintf "split %d of %S" i input in
       Alcotest.(check (list request_body)) msg expect (List.rev c.responses);
       Alcotest.(check (list string)) (msg ^ ": errors") [] c.cerrors;
       Alcotest.(check int)
         (msg ^ ": consumed")
         (String.length input)
         c.cfeeder.consumed)
    (all_splits input)

let get = "GET /a HTTP/1.1\r\nHost: example.com\r\nAccept: */*\r\n\r\n"
let get_summary = {|"GET" "/a" HTTP/1.1 ["Host":"example.com";"Accept":"*/*"]|}
let test_get () = check_server ~expect:[ get_summary, "" ] get

let test_post_fixed () =
  check_server
    ~expect:
      [ ( {|"POST" "/p" HTTP/1.1 ["Host":"a";"Content-Length":"11"]|}
        , "hello world" )
      ]
    "POST /p HTTP/1.1\r\nHost: a\r\nContent-Length: 11\r\n\r\nhello world"

let chunked_post =
  "POST /c HTTP/1.1\r\n\
   Host: a\r\n\
   Transfer-Encoding: chunked\r\n\
   \r\n\
   5;name=value;flag\r\n\
   hello\r\n\
   6 ; q=\"a \\\"b\\\" c\"\r\n\
  \ world\r\n\
   0;last\r\n\
   Expires: never\r\n\
   X-Checksum: abc\r\n\
   \r\n"

let test_post_chunked () =
  check_server
    ~expect:
      [ ( {|"POST" "/c" HTTP/1.1 ["Host":"a";"Transfer-Encoding":"chunked"]|}
        , "hello world" )
      ]
    chunked_post

let test_pipelined () =
  check_server
    ~expect:
      [ get_summary, ""
      ; {|"POST" "/p" HTTP/1.1 ["Host":"a";"Content-Length":"3"]|}, "abc"
      ]
    (get ^ "POST /p HTTP/1.1\r\nHost: a\r\nContent-Length: 3\r\n\r\nabc")

let test_pipelined_incomplete_head_consumed () =
  let s = server () in
  let input = get ^ "GET /b" in
  let len = String.length input in
  let consumed =
    Server_connection.read
      s.conn
      (Bigstringaf.of_string input ~off:0 ~len)
      ~off:0
      ~len
  in
  Alcotest.(check int) "consumed" len consumed;
  Feeder.feed s.feeder " HTTP/1.1\r\nHost: a\r\n\r\n";
  Alcotest.(check (list request_body))
    "both requests"
    [ get_summary, ""; {|"GET" "/b" HTTP/1.1 ["Host":"a"]|}, "" ]
    (List.rev s.requests)

let test_response_fixed () =
  check_client
    ~expect:[ {|HTTP/1.1 200 "OK" ["Content-Length":"5"]|}, "hello" ]
    "HTTP/1.1 200 OK\r\nContent-Length: 5\r\n\r\nhello"

let test_response_chunked () =
  check_client
    ~expect:
      [ {|HTTP/1.1 200 "OK" ["Transfer-Encoding":"chunked"]|}, "hello world" ]
    "HTTP/1.1 200 OK\r\n\
     Transfer-Encoding: chunked\r\n\
     \r\n\
     5;a=b\r\n\
     hello\r\n\
     6\r\n\
    \ world\r\n\
     0\r\n\
     X-Trailer: 1\r\n\
     \r\n"

let test_response_close_delimited () =
  check_client
    ~eof:true
    ~expect:[ {|HTTP/1.1 200 "OK" []|}, "until the end" ]
    "HTTP/1.1 200 OK\r\n\r\nuntil the end"

let test_crlf_across_reads () =
  let head = "GET / HTTP/1.1\r\nHost: a\r\n\r\n" in
  let expect = [ {|"GET" "/" HTTP/1.1 ["Host":"a"]|}, "" ] in
  List.iter
    (fun cut ->
       let s =
         run_server
           [ String.sub head 0 cut
           ; String.sub head cut (String.length head - cut)
           ]
       in
       Alcotest.(check (list request_body))
         (Printf.sprintf "cut at %d" cut)
         expect
         s.requests)
    (List.filter
       (fun i -> head.[i] = '\r' || head.[i] = '\n')
       (List.init (String.length head) Fun.id))

let head_of_size n =
  let base = "GET / HTTP/1.1\r\nHost: a\r\nX: \r\n\r\n" in
  let pad = n - String.length base in
  "GET / HTTP/1.1\r\nHost: a\r\nX: " ^ String.make pad 'v' ^ "\r\n\r\n"

let test_head_limit () =
  let config = { Config.default with max_header_size = 100 } in
  let at = head_of_size 100 in
  Alcotest.(check int) "head size" 100 (String.length at);
  let s = run_server ~config [ at ] in
  Alcotest.(check int)
    "head at the limit is accepted"
    1
    (List.length s.requests);
  Alcotest.(check (list string)) "no error" [] s.errors;
  List.iter
    (fun chunks ->
       let s = run_server ~config chunks in
       Alcotest.(check (list request_body)) "no request" [] s.requests;
       Alcotest.(check (list string))
         "431"
         [ "Request_header_fields_too_large" ]
         s.errors;
       Alcotest.(check string)
         "status line"
         "HTTP/1.1 431"
         (String.sub (Buffer.contents s.output) 0 12))
    (all_splits (head_of_size 101))

let test_head_limit_client () =
  let config = { Config.default with max_header_size = 64 } in
  let c =
    run_client
      ~config
      [ "HTTP/1.1 200 OK\r\nX: " ^ String.make 64 'v' ^ "\r\n\r\n" ]
  in
  Alcotest.(check (list string))
    "client error"
    [ "Malformed_response header fields too large" ]
    c.cerrors

let test_chunk_line_limit () =
  let config = { Config.default with max_header_size = 64 } in
  let head =
    "POST / HTTP/1.1\r\nHost: a\r\nTransfer-Encoding: chunked\r\n\r\n"
  in
  let request line_size =
    head ^ "5;" ^ String.make (line_size - 4) 'x' ^ "\r\nhello\r\n0\r\n\r\n"
  in
  List.iter
    (fun chunks ->
       let s = run_server ~config chunks in
       Alcotest.(check (list string)) "line at the limit" [] s.errors;
       Alcotest.(check (list request_body))
         "body"
         [ ( {|"POST" "/" HTTP/1.1 ["Host":"a";"Transfer-Encoding":"chunked"]|}
           , "hello" )
         ]
         s.requests)
    (all_splits (request 64));
  List.iter
    (fun chunks ->
       let s = run_server ~config chunks in
       Alcotest.(check (list string))
         "line over the limit"
         [ "Request_header_fields_too_large" ]
         s.errors)
    (all_splits (request 65))

let test_large_header_small_reads () =
  let value = String.make (6 * 1024) 'v' in
  let input = "GET / HTTP/1.1\r\nHost: a\r\nX-Big: " ^ value ^ "\r\n\r\n" in
  let s = server () in
  List.iter
    (fun chunk ->
       Feeder.feed s.feeder chunk;
       Alcotest.(check string) "each read is consumed" "" s.feeder.pending)
    (sized 4096 input);
  Alcotest.(check (list request_body))
    "6 KiB header through 4096-byte reads"
    [ Printf.sprintf {|"GET" "/" HTTP/1.1 ["Host":"a";"X-Big":%S]|} value, "" ]
    s.requests;
  Alcotest.(check int) "consumed" (String.length input) s.feeder.consumed

let test_chunk_size_overflow () =
  let module G = Ache_private.Parse.Grammar in
  Alcotest.(check (result int64 string))
    "15 hex digits"
    (Ok 0xfffffffffffffffL)
    (G.chunk_size "fffffffffffffff\r\n");
  Alcotest.(check (result int64 string))
    "16 hex digits"
    (Error {|chunk-size too long: "ffffffffffffffff"|})
    (G.chunk_size "ffffffffffffffff\r\n");
  let s =
    run_server
      [ "POST / HTTP/1.1\r\n\
         Host: a\r\n\
         Transfer-Encoding: chunked\r\n\
         \r\n\
         7fffffffffffffff\r\n\
         hello\r\n\
         0\r\n\
         \r\n"
      ]
  in
  Alcotest.(check (list string)) "400" [ "Bad_request" ] s.errors;
  Alcotest.(check (list request_body))
    "no body bytes"
    [ ( {|"POST" "/" HTTP/1.1 ["Host":"a";"Transfer-Encoding":"chunked"]|}
      , "incomplete: " )
    ]
    s.requests

let chunked_head =
  "POST / HTTP/1.1\r\nHost: a\r\nTransfer-Encoding: chunked\r\n\r\n"

let chunked_summary =
  {|"POST" "/" HTTP/1.1 ["Host":"a";"Transfer-Encoding":"chunked"]|}

let test_chunk_size_leading_zeros () =
  check_server
    ~expect:[ chunked_summary, "hello" ]
    (chunked_head
    ^ "0000000000000005\r\nhello\r\n"
    ^ String.make 20 '0'
    ^ "\r\n\r\n");
  check_server
    ~expect:[ chunked_summary, String.make 16 'x' ]
    (chunked_head
    ^ "0000000000000000010\r\n"
    ^ String.make 16 'x'
    ^ "\r\n0\r\n\r\n");
  List.iter
    (fun size ->
       List.iter
         (fun chunks ->
            let s = run_server chunks in
            Alcotest.(check (list string)) size [ "Bad_request" ] s.errors;
            Alcotest.(check (list request_body))
              (size ^ ": no body bytes")
              [ chunked_summary, "incomplete: " ]
              s.requests)
         (all_splits (chunked_head ^ size ^ "\r\nhello\r\n0\r\n\r\n")))
    [ "10000000000000000"; "7fffffffffffffff"; "ffffffffffffffff" ]

let test_eof_states () =
  let chunked =
    "POST / HTTP/1.1\r\nHost: a\r\nTransfer-Encoding: chunked\r\n\r\n"
  in
  List.iter
    (fun (name, input) ->
       List.iter
         (fun chunks ->
            let s = run_server ~eof:true chunks in
            Alcotest.(check (list string)) name [ "Bad_request" ] s.errors)
         (all_splits input))
    [ "mid-head", "GET / HTTP/1.1\r\nHost: a\r\n"
    ; ( "mid-fixed-body"
      , "POST / HTTP/1.1\r\nHost: a\r\nContent-Length: 10\r\n\r\nhello" )
    ; "mid-chunk-size", chunked ^ "5;ext"
    ; "mid-chunk-data", chunked ^ "5\r\nhel"
    ; "mid-chunk-crlf", chunked ^ "5\r\nhello\r"
    ; "mid-trailers", chunked ^ "5\r\nhello\r\n0\r\nX-T: 1\r\n"
    ]

let test_eof_client () =
  List.iter
    (fun (name, input) ->
       let c = run_client ~eof:true [ input ] in
       Alcotest.(check bool) name true (c.cerrors <> []))
    [ "mid-head", "HTTP/1.1 200 OK\r\nX: 1\r\n"
    ; "mid-fixed-body", "HTTP/1.1 200 OK\r\nContent-Length: 10\r\n\r\nhello"
    ; ( "mid-chunk-data"
      , "HTTP/1.1 200 OK\r\nTransfer-Encoding: chunked\r\n\r\n5\r\nhe" )
    ]

let test_invalid_status_then_valid () =
  let c = run_client [ "HTTP/1.1 99 x\r\n\r\n" ] in
  Alcotest.(check (list string))
    "invalid status code"
    [ {|Malformed_response invalid status-code: "99"|} ]
    c.cerrors;
  Alcotest.(check (result string string))
    "grammar"
    (Error {|invalid status-code: "99"|})
    (Result.map
       response_summary
       (Ache_private.Parse.Grammar.response "HTTP/1.1 99 x\r\n\r\n"));
  check_client
    ~expect:[ {|HTTP/1.1 200 "OK" ["Content-Length":"2"]|}, "ok" ]
    "HTTP/1.1 200 OK\r\nContent-Length: 2\r\n\r\nok";
  check_server ~expect:[ get_summary, "" ] get

let test_interleaved_connections () =
  let a = server ()
  and b = server () in
  let ra = chunked_post
  and rb = get ^ get in
  for i = 0 to max (String.length ra) (String.length rb) - 1 do
    if i < String.length ra then Feeder.feed a.feeder (String.make 1 ra.[i]);
    Alcotest.(check bool)
      "string parse while another parse is suspended"
      true
      (Result.is_ok (Ache_private.Parse.Grammar.request get));
    if i < String.length rb then Feeder.feed b.feeder (String.make 1 rb.[i])
  done;
  Alcotest.(check (list request_body))
    "a"
    [ ( {|"POST" "/c" HTTP/1.1 ["Host":"a";"Transfer-Encoding":"chunked"]|}
      , "hello world" )
    ]
    a.requests;
  Alcotest.(check (list request_body))
    "b"
    [ get_summary, ""; get_summary, "" ]
    b.requests

let test_shutdown_suspended () =
  let a = server () in
  Feeder.feed a.feeder "GET / HTTP/1.1\r\nHo";
  Server_connection.shutdown a.conn;
  Alcotest.(check bool)
    "reader closed"
    true
    (Server_connection.next_read_operation a.conn = `Close);
  let b = server () in
  Feeder.feed
    b.feeder
    "POST / HTTP/1.1\r\nHost: a\r\nTransfer-Encoding: chunked\r\n\r\n5;e";
  Server_connection.shutdown b.conn;
  Alcotest.(check (list string)) "no error" [] (a.errors @ b.errors);
  check_server ~expect:[ get_summary, "" ] get

let body_bytes n =
  String.init n (fun i -> Char.chr (Char.code 'a' + (i * 7 mod 26)))

let chunked_of_sizes body sizes =
  let b = Buffer.create (String.length body * 2) in
  let rec go off = function
    | [] -> ()
    | size :: rest ->
      let size = min size (String.length body - off) in
      if size > 0
      then (
        Buffer.add_string b (Printf.sprintf "%x\r\n" size);
        Buffer.add_string b (String.sub body off size);
        Buffer.add_string b "\r\n";
        go (off + size) (if rest = [] then [ size ] else rest))
  in
  go 0 sizes;
  Buffer.add_string b "0\r\n\r\n";
  Buffer.contents b

let check_body_splits ~summary ~body input =
  List.iter
    (fun chunks ->
       let s = run_server chunks in
       Alcotest.(check (list string)) "errors" [] s.errors;
       Alcotest.(check (list request_body))
         (Printf.sprintf "%d reads" (List.length chunks))
         [ summary, body ]
         s.requests)
    [ [ input ]
    ; every_byte input
    ; sized 7 input
    ; sized 4096 input
    ; sized 4097 input
    ]

let test_large_body_every_split () =
  let body = body_bytes 10_000 in
  check_body_splits
    ~summary:{|"POST" "/" HTTP/1.1 ["Host":"a";"Content-Length":"10000"]|}
    ~body
    ("POST / HTTP/1.1\r\nHost: a\r\nContent-Length: 10000\r\n\r\n" ^ body);
  check_body_splits
    ~summary:chunked_summary
    ~body
    (chunked_head ^ chunked_of_sizes body [ 1; 2; 3; 1000; 5000; 17 ]);
  let body = body_bytes 300 in
  List.iter
    (fun (summary, input) ->
       List.iter
         (fun chunks ->
            let s = run_server chunks in
            Alcotest.(check (list request_body))
              "two reads"
              [ summary, body ]
              s.requests)
         (two_way input))
    [ ( {|"POST" "/" HTTP/1.1 ["Host":"a";"Content-Length":"300"]|}
      , "POST / HTTP/1.1\r\nHost: a\r\nContent-Length: 300\r\n\r\n" ^ body )
    ; chunked_summary, chunked_head ^ chunked_of_sizes body [ 1; 2; 3; 50 ]
    ]

let test_many_small_chunks () =
  let n = 100_000 in
  let input =
    chunked_head
    ^ String.concat "" (List.init n (fun _ -> "1\r\nx\r\n"))
    ^ "0\r\n\r\n"
  in
  let on_read_calls = ref 0 in
  let received = Buffer.create n in
  let eof = ref false in
  let handler reqd =
    let body = Reqd.request_body reqd in
    let rec on_read bs ~off ~len =
      incr on_read_calls;
      Buffer.add_string received (Bigstringaf.substring bs ~off ~len);
      Body.Reader.schedule_read
        ~on_error:(Helpers.body_must_complete __LOC__)
        body
        ~on_read
        ~on_eof
    and on_eof () =
      eof := true;
      Reqd.respond_with_string reqd (Response.create `OK) ""
    in
    Body.Reader.schedule_read
      ~on_error:(Helpers.body_must_complete __LOC__)
      body
      ~on_read
      ~on_eof
  in
  let conn = Server_connection.create handler in
  let bs = Bigstringaf.of_string input ~off:0 ~len:(String.length input) in
  let consumed =
    Server_connection.read conn bs ~off:0 ~len:(String.length input)
  in
  Alcotest.(check int) "consumed" (String.length input) consumed;
  Alcotest.(check bool) "eof" true !eof;
  Alcotest.(check string) "bytes" (String.make n 'x') (Buffer.contents received);
  Alcotest.(check int) "on_read calls" 1 !on_read_calls

let collect_body body =
  let buf = Buffer.create 16 in
  let done_ = ref false in
  let rec on_read bs ~off ~len =
    Buffer.add_string buf (Bigstringaf.substring bs ~off ~len);
    Body.Reader.schedule_read
      ~on_error:(Helpers.body_must_complete __LOC__)
      body
      ~on_read
      ~on_eof
  and on_eof () = done_ := true in
  Body.Reader.schedule_read
    ~on_error:(Helpers.body_must_complete __LOC__)
    body
    ~on_read
    ~on_eof;
  buf, done_

let test_pipelined_body_read_late () =
  let bodies = ref [] in
  let handler reqd =
    bodies := Reqd.request_body reqd :: !bodies;
    Reqd.respond_with_string reqd (Response.create `OK) ""
  in
  let conn = Server_connection.create handler in
  let input =
    "POST / HTTP/1.1\r\nHost: a\r\nContent-Length: 4\r\n\r\naaaa"
    ^ "POST / HTTP/1.1\r\n\
       Host: a\r\n\
       Transfer-Encoding: chunked\r\n\
       \r\n\
       4\r\n\
       bbbb\r\n\
       0\r\n\
       \r\n"
    ^ "POST / HTTP/1.1\r\nHost: a\r\nContent-Length: 4\r\n\r\ncccc"
  in
  let rec drain () =
    match Server_connection.next_write_operation conn with
    | `Write iovecs ->
      Server_connection.report_write_result conn (`Ok (IOVec.lengthv iovecs));
      drain ()
    | `Yield | `Close _ -> ()
  in
  let feeder =
    { Feeder.pending = ""
    ; consumed = 0
    ; next_read_operation =
        (fun () -> Server_connection.next_read_operation conn)
    ; read = Server_connection.read conn
    ; read_eof = Server_connection.read_eof conn
    ; drain
    }
  in
  Feeder.feed feeder input;
  Alcotest.(check int) "consumed" (String.length input) feeder.consumed;
  Alcotest.(check int) "three requests" 3 (List.length !bodies);
  let read = List.rev_map collect_body !bodies in
  List.iter2
    (fun expected (buf, done_) ->
       Alcotest.(check string) "body" expected (Buffer.contents buf);
       Alcotest.(check bool) "eof" true !done_)
    [ "aaaa"; "bbbb"; "cccc" ]
    read

let test_unread_body_backpressure () =
  let body = body_bytes 20_000 in
  let input =
    "POST / HTTP/1.1\r\nHost: a\r\nContent-Length: 20000\r\n\r\n" ^ body
  in
  let request_body = ref None in
  let conn =
    Server_connection.create (fun reqd ->
      request_body := Some (Reqd.request_body reqd))
  in
  let bs = Bigstringaf.of_string input ~off:0 ~len:(String.length input) in
  let fed = ref 0 in
  let read_some () =
    let len = min 4096 (String.length input - !fed) in
    fed := !fed + Server_connection.read conn bs ~off:!fed ~len
  in
  read_some ();
  Alcotest.(check bool)
    "reader yields while the body is unread"
    true
    (Server_connection.next_read_operation conn = `Yield);
  let held = ref [] in
  let received = Buffer.create 16 in
  let eof = ref false in
  let body_reader = Option.get !request_body in
  let on_read bs ~off ~len =
    held := (bs, off, len, Bigstringaf.substring bs ~off ~len) :: !held;
    Buffer.add_string received (Bigstringaf.substring bs ~off ~len)
  and on_eof () = eof := true in
  let rec loop rounds =
    if rounds > 100 then Alcotest.fail "body not read to the end";
    if not !eof
    then (
      Body.Reader.schedule_read
        ~on_error:(Helpers.body_must_complete __LOC__)
        body_reader
        ~on_read
        ~on_eof;
      (match Server_connection.next_read_operation conn with
      | `Read when !fed < String.length input -> read_some ()
      | _ -> ());
      loop (rounds + 1))
  in
  loop 0;
  Alcotest.(check string) "bytes" body (Buffer.contents received);
  List.iter
    (fun (bs, off, len, copy) ->
       Alcotest.(check bool)
         "delivered bytes are not overwritten"
         true
         (String.equal copy (Bigstringaf.substring bs ~off ~len)))
    !held

let tests =
  [ "GET at every split", `Quick, test_get
  ; "POST fixed body at every split", `Quick, test_post_fixed
  ; "POST chunked with extensions and trailers", `Quick, test_post_chunked
  ; "pipelined requests at every split", `Quick, test_pipelined
  ; ( "incomplete head after a message is consumed"
    , `Quick
    , test_pipelined_incomplete_head_consumed )
  ; "response with fixed body", `Quick, test_response_fixed
  ; "response with chunked body", `Quick, test_response_chunked
  ; "response with close-delimited body", `Quick, test_response_close_delimited
  ; "CRLF split across reads", `Quick, test_crlf_across_reads
  ; "head size limit", `Quick, test_head_limit
  ; "head size limit (client)", `Quick, test_head_limit_client
  ; "chunk-size line size limit", `Quick, test_chunk_line_limit
  ; "6 KiB header in 4096-byte reads", `Quick, test_large_header_small_reads
  ; "chunk-size overflow", `Quick, test_chunk_size_overflow
  ; "chunk-size with leading zeros", `Quick, test_chunk_size_leading_zeros
  ; "EOF in each state", `Quick, test_eof_states
  ; "EOF in each state (client)", `Quick, test_eof_client
  ; ( "invalid status code, then valid parses"
    , `Quick
    , test_invalid_status_then_valid )
  ; "interleaved connections", `Quick, test_interleaved_connections
  ; "shutdown with a suspended parse", `Quick, test_shutdown_suspended
  ; "10 KB bodies in many splits", `Quick, test_large_body_every_split
  ; "100,000 one-byte chunks in one read", `Quick, test_many_small_chunks
  ; "pipelined bodies read late", `Quick, test_pipelined_body_read_late
  ; "unread body: backpressure", `Quick, test_unread_body_backpressure
  ]
