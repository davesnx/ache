open Ache

let get = "GET / HTTP/1.1\r\nHost: a\r\n\r\n"

let request ?(headers = []) meth target =
  Request.create
    ~headers:(Headers.of_list (("host", "a") :: headers))
    meth
    target

let hello reqd =
  Request_descriptor.respond_with_string reqd (Response.create `OK) "hello"

let streaming ?(headers = []) reqd =
  let body =
    Request_descriptor.respond_with_streaming
      reqd
      (Response.create ~headers:(Headers.of_list headers) `OK)
  in
  Body.Writer.write_string body "hel";
  Body.Writer.write_string body "lo";
  Body.Writer.close body

let echo reqd =
  let request = Request_descriptor.request reqd in
  let reader = Request_descriptor.request_body reqd in
  let buffer = Buffer.create 16 in
  let rec next () =
    Body.Reader.schedule_read
      reader
      ~on_eof:(fun () ->
        Request_descriptor.respond_with_string
          reqd
          (Response.create `OK)
          (request.target ^ " " ^ Buffer.contents buffer))
      ~on_error:ignore
      ~on_read:(fun bs ~off ~len ->
        Buffer.add_string buffer (Bigstring.substring bs ~off ~len);
        next ())
  in
  next ()

let string_response () =
  Alcotest.(check string)
    "bytes"
    "HTTP/1.1 200 OK\r\ncontent-length: 5\r\n\r\nhello"
    (Testing.serve hello get)

let streaming_response () =
  Alcotest.(check string)
    "close-delimited for HTTP/1.0"
    "HTTP/1.1 200 OK\r\nconnection: close\r\n\r\nhello"
    (Testing.serve streaming "GET / HTTP/1.0\r\n\r\n");
  let response, body = Testing.serve_request streaming (request `GET "/") in
  Alcotest.(check int) "status" 200 (Status.to_code response.status);
  Alcotest.(check string) "body" "hello" body

let chunked_response () =
  Alcotest.(check string)
    "bytes"
    "HTTP/1.1 200 OK\r\n\
     transfer-encoding: chunked\r\n\
     \r\n\
     5\r\n\
     hello\r\n\
     0\r\n\
     \r\n"
    (Testing.serve streaming get);
  let response, body = Testing.serve_request streaming (request `GET "/") in
  Alcotest.(check (option string))
    "transfer-encoding"
    (Some "chunked")
    (Headers.get response.headers "transfer-encoding");
  Alcotest.(check string) "decoded body" "hello" body

let chunks_given_later () =
  let body = ref None in
  let t =
    Testing.create (fun reqd ->
      body :=
        Some
          (Request_descriptor.respond_with_streaming
             ~flush_headers_immediately:true
             reqd
             (Response.create `OK)))
  in
  Testing.feed t get;
  Alcotest.(check string)
    "head"
    "HTTP/1.1 200 OK\r\ntransfer-encoding: chunked\r\n\r\n"
    (Testing.output t);
  let body = Option.get !body in
  Body.Writer.write_string body "hel";
  Alcotest.(check string) "first chunk" "3\r\nhel\r\n" (Testing.output t);
  Body.Writer.write_string body "lo";
  Body.Writer.close body;
  Alcotest.(check string) "last chunk" "2\r\nlo\r\n0\r\n\r\n" (Testing.output t)

let pipelined () =
  let input =
    "POST /a HTTP/1.1\r\nHost: a\r\nContent-Length: 2\r\n\r\nxy"
    ^ "GET /b HTTP/1.1\r\nHost: a\r\n\r\n"
  in
  Alcotest.(check string)
    "two responses in order"
    ("HTTP/1.1 200 OK\r\ncontent-length: 5\r\n\r\n/a xy"
    ^ "HTTP/1.1 200 OK\r\ncontent-length: 3\r\n\r\n/b ")
    (Testing.serve echo input)

let handler_raises () =
  let output = Testing.serve (fun _ -> failwith "boom") get in
  Alcotest.(check bool)
    "500"
    true
    (String.starts_with ~prefix:"HTTP/1.1 500 Internal Server Error\r\n" output);
  let error = ref None in
  let error_handler ?request:_ e respond =
    error := Some e;
    Body.Writer.close (respond Headers.empty)
  in
  let response, _ =
    Testing.serve_request
      ~error_handler
      (fun _ -> failwith "boom")
      (request `GET "/")
  in
  Alcotest.(check int) "status" 500 (Status.to_code response.status);
  Alcotest.(check bool)
    "error handler got the exception"
    true
    (match !error with Some (`Exn (Failure _)) -> true | _ -> false)

let request_with_body () =
  let response, body =
    Testing.serve_request ~body:"payload" echo (request `POST "/p")
  in
  Alcotest.(check int) "status" 200 (Status.to_code response.status);
  Alcotest.(check string) "body" "/p payload" body;
  let _, body =
    Testing.serve_request
      ~body:"chunks"
      echo
      (request ~headers:[ "transfer-encoding", "chunked" ] `POST "/c")
  in
  Alcotest.(check string) "chunked request body" "/c chunks" body

let no_response () =
  Alcotest.check_raises
    "Failure"
    (Failure "Ache.Testing.serve_request: no response")
    (fun () -> ignore (Testing.serve_request ignore (request `GET "/")))

let later () =
  let pending = ref [] in
  let t = Testing.create (fun reqd -> pending := reqd :: !pending) in
  Testing.feed t get;
  Alcotest.(check string) "nothing yet" "" (Testing.output t);
  List.iter hello !pending;
  Alcotest.(check string)
    "response"
    "HTTP/1.1 200 OK\r\ncontent-length: 5\r\n\r\nhello"
    (Testing.output t);
  Alcotest.(check string) "read once" "" (Testing.output t);
  Alcotest.(check bool) "open" false (Testing.is_closed t);
  Testing.feed_eof t;
  Alcotest.(check string) "nothing more" "" (Testing.output t);
  Alcotest.(check bool) "closed" true (Testing.is_closed t)

module Stuck = struct
  type t = { mutable reads : int }

  let next_read_operation _ = `Read

  let read t _ ~off:_ ~len:_ =
    t.reads <- t.reads + 1;
    0

  let read_eof = read
  let yield_reader _ _ = ()
  let next_write_operation _ = `Yield
  let report_write_result _ _ = ()
  let yield_writer _ _ = ()
  let report_exn _ _ = ()
  let is_closed _ = false
  let shutdown _ = ()
end

let upgrade_that_never_reads () =
  let stuck = { Stuck.reads = 0 } in
  let output =
    Testing.serve
      (fun reqd ->
         Request_descriptor.respond_with_upgrade
           reqd
           (Headers.of_list [ "upgrade", "x"; "connection", "upgrade" ])
           (Runtime.make (module Stuck) stuck))
      "GET / HTTP/1.1\r\n\
       Host: a\r\n\
       Upgrade: x\r\n\
       Connection: upgrade\r\n\
       \r\n\
       rest"
  in
  Alcotest.(check string)
    "101"
    "HTTP/1.1 101 Switching Protocols\r\n\
     upgrade: x\r\n\
     connection: upgrade\r\n\
     \r\n"
    output;
  Alcotest.(check bool) "the engine was read" true (stuck.reads > 0)

let tests =
  [ "string response", `Quick, string_response
  ; "streaming response", `Quick, streaming_response
  ; "chunked response", `Quick, chunked_response
  ; "chunks given later", `Quick, chunks_given_later
  ; "pipelined requests", `Quick, pipelined
  ; "handler raises", `Quick, handler_raises
  ; "serve_request with a body", `Quick, request_with_body
  ; "serve_request without a response", `Quick, no_response
  ; "response given later", `Quick, later
  ; "upgrade that never reads", `Quick, upgrade_that_never_reads
  ]
