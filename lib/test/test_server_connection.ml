open Ache
open Helpers

let request_to_string ?body (r : Request.t) =
  let r =
    if
      Version.compare r.version Version.v1_1 >= 0
      && not (Headers.mem r.headers "host")
    then { r with headers = Headers.add r.headers "host" "example.com" }
    else r
  in
  request_to_string ?body r

let trace fmt = Format.ksprintf (Format.printf "%s\n%!") fmt

let request_error_pp_hum fmt = function
  | `Bad_request reason -> Format.fprintf fmt "Bad_request %S" reason
  | `Not_implemented -> Format.fprintf fmt "Not_implemented"
  | `Http_version_not_supported ->
    Format.fprintf fmt "Http_version_not_supported"
  | `Uri_too_long -> Format.fprintf fmt "Uri_too_long"
  | `Bad_gateway -> Format.fprintf fmt "Bad_gateway"
  | `Internal_server_error -> Format.fprintf fmt "Internal_server_error"
  | `Request_header_fields_too_large ->
    Format.fprintf fmt "Request_header_fields_too_large"
  | `Request_timeout -> Format.fprintf fmt "Request_timeout"
  | `Exn exn -> Format.fprintf fmt "Exn (%s)" (Printexc.to_string exn)

module Alcotest = struct
  include Alcotest

  let request_error = Alcotest.of_pp request_error_pp_hum

  let request =
    Alcotest.of_pp (fun fmt req ->
      Format.fprintf fmt "%s" (request_to_string req))
end

module Runtime : sig
  type t

  val create :
     ?config:Config.t
    -> ?error_handler:Server_connection.error_handler
    -> Server_connection.request_handler
    -> t

  val current_read_operation : t -> Read_operation.t
  val current_write_operation : t -> Write_operation.t
  val do_read : t -> (Server_connection.t -> 'a) -> 'a

  val do_write :
     t
    -> (Server_connection.t -> Ache.Bigstring.t IOVec.t list -> 'a)
    -> 'a

  val on_reader_unyield : t -> (unit -> unit) -> bool ref
  (** Returns a [ref] that is set to [true] after the callback was fired *)

  val on_writer_unyield : t -> (unit -> unit) -> bool ref
  val report_exn : t -> exn -> unit
  val with_server_connection : t -> (Server_connection.t -> 'a) -> 'a
  val shutdown : t -> unit
  val is_closed : t -> bool
end = struct
  open Server_connection

  type t =
    { server_connection : Server_connection.t
    ; mutable read_operation : [ `Initial | Read_operation.t ]
    ; mutable write_operation : [ `Initial | Write_operation.t ]
    ; read_loop : unit -> unit
    ; write_loop : unit -> unit
    ; mutable read_unyield_hook : (unit -> unit) option
    ; mutable write_unyield_hook : (unit -> unit) option
    }

  let rec read_step t =
    match next_read_operation t.server_connection with
    | `Read ->
      trace "reader: Read";
      t.read_operation <- `Read
    | `Yield ->
      trace "reader: Yield";
      t.read_operation <- `Yield;
      yield_reader t.server_connection (fun () ->
        trace "reader: Yield callback";
        read_step t;
        t.read_unyield_hook
        |> Option.iter (fun f ->
          t.read_unyield_hook <- None;
          f ()))
    | `Close ->
      trace "reader: Close";
      t.read_operation <- `Close

  let rec write_step t =
    match next_write_operation t.server_connection with
    | `Write xs ->
      trace "writer: Write";
      t.write_operation <- `Write xs
    | `Yield ->
      t.write_operation <- `Yield;
      trace "writer: Yield";
      yield_writer t.server_connection (fun () ->
        trace "writer: Yield callback";
        write_step t;
        t.write_unyield_hook
        |> Option.iter (fun f ->
          t.write_unyield_hook <- None;
          f ()))
    | `Close n ->
      trace "writer: Close";
      t.write_operation <- `Close n

  let create ?config ?error_handler request_handler =
    let request_handler r =
      trace "invoked: request_handler";
      request_handler r
    in
    let error_handler =
      Option.map
        (fun error_handler ?request ->
           trace "invoked: error_handler";
           error_handler ?request)
        error_handler
    in
    let rec t =
      lazy
        { server_connection = create ?config ?error_handler request_handler
        ; read_operation = `Initial
        ; write_operation = `Initial
        ; read_loop = (fun () -> read_step (Lazy.force_val t))
        ; write_loop = (fun () -> write_step (Lazy.force_val t))
        ; read_unyield_hook = None
        ; write_unyield_hook = None
        }
    in
    let t = Lazy.force_val t in
    t.read_loop ();
    t.write_loop ();
    t

  let current_read_operation t =
    match t.read_operation with
    | `Initial -> assert false
    | (`Read | `Yield | `Close) as op -> op

  let current_write_operation t =
    match t.write_operation with
    | `Initial -> assert false
    | (`Write _ | `Yield | `Close _) as op -> op

  let do_read t f =
    match current_read_operation t with
    | `Read ->
      trace "read: start";
      let res = f t.server_connection in
      trace "read: finished";
      t.read_loop ();
      res
    | (`Yield | `Close) as op ->
      Alcotest.failf
        "Read attempted during operation: %a"
        Read_operation.pp_hum
        op

  let with_server_connection t f = f t.server_connection

  let do_write t f =
    match current_write_operation t with
    | `Write bufs ->
      trace "write: start";
      let res = f t.server_connection bufs in
      trace "write: finished";
      t.write_loop ();
      res
    | (`Yield | `Close _) as op ->
      Alcotest.failf
        "Write attempted during operation: %a"
        Write_operation.pp_hum
        op

  let on_reader_unyield t f =
    let called = ref false in
    assert (Option.is_none t.read_unyield_hook);
    t.read_unyield_hook <-
      Some
        (fun () ->
          called := true;
          f ());
    called

  let on_writer_unyield t f =
    let called = ref false in
    assert (Option.is_none t.write_unyield_hook);
    t.write_unyield_hook <-
      Some
        (fun () ->
          called := true;
          f ());
    called

  let report_exn t = Server_connection.report_exn t.server_connection
  let shutdown t = Server_connection.shutdown t.server_connection
  let is_closed t = Server_connection.is_closed t.server_connection
end

open Runtime

let read ?(eof = false) t str ~off ~len =
  do_read t (fun conn ->
    if eof
    then Server_connection.read_eof conn str ~off ~len
    else Server_connection.read conn str ~off ~len)

let read_eof = read ~eof:true

let force_read_eof t str ~off ~len =
  with_server_connection t (fun conn ->
    Server_connection.read_eof conn str ~off ~len)

let feed_string ?eof t str =
  let len = String.length str in
  let input = Ache.Bigstring.of_string str ~off:0 ~len in
  read ?eof t input ~off:0 ~len

let read_string ?eof t str =
  let c = feed_string ?eof t str in
  Alcotest.(check int) "read consumes all input" (String.length str) c

let force_read t str ~off ~len =
  with_server_connection t (fun conn ->
    Server_connection.read conn str ~off ~len)

let force_feed_string t str =
  let len = String.length str in
  let input = Ache.Bigstring.of_string str ~off:0 ~len in
  force_read t input ~off:0 ~len

let force_read_string t str =
  let c = force_feed_string t str in
  Alcotest.(check int) "read consumes all input" (String.length str) c

let read_request ?eof t r =
  let request_string = request_to_string r in
  read_string ?eof t request_string

let reader_ready ?(msg = "Reader is ready") t =
  Alcotest.check read_operation msg `Read (current_read_operation t)

let reader_yielded t =
  Alcotest.check
    read_operation
    "Reader is in a yield state"
    `Yield
    (current_read_operation t)

let reader_closed ?(msg = "Reader is closed") t =
  Alcotest.check read_operation msg `Close (current_read_operation t)

let reader_errored = reader_closed ~msg:"Error shuts down the reader"

(* Checks that the [len] prefixes of expected and the write match, and returns
   the rest. *)
let write_partial_string ?(msg = "output written") t expected len =
  do_write t (fun conn bufs ->
    let actual = String.sub (Write_operation.iovecs_to_string bufs) 0 len in
    Alcotest.(check string) msg (String.sub expected 0 len) actual;
    Server_connection.report_write_result conn (`Ok len);
    String.sub expected len (String.length expected - len))

let write_string ?(msg = "output written") t expected =
  do_write t (fun conn bufs ->
    let len = String.length expected in
    let actual = Write_operation.iovecs_to_string bufs in
    Alcotest.(check string) msg expected actual;
    Server_connection.report_write_result conn (`Ok len))

let write_response ?(msg = "response written") ?body t r =
  let response_string = response_to_string ?body r in
  write_string ~msg t response_string

let write_eof t =
  do_write t (fun conn _ -> Server_connection.report_write_result conn `Closed)

let writer_ready t =
  let is_write =
    Alcotest.testable Write_operation.pp_hum (fun a b ->
      match a, b with `Write _, `Write _ -> true | _ -> false)
  in
  Alcotest.check
    is_write
    "Writer is ready"
    (`Write [])
    (current_write_operation t)

let writer_yielded t =
  Alcotest.check
    write_operation
    "Writer is in a yield state"
    `Yield
    (current_write_operation t)

let writer_closed ?(unread = 0) t =
  Alcotest.check
    write_operation
    "Writer is closed"
    (`Close unread)
    (current_write_operation t)

let connection_is_shutdown t =
  reader_closed t;
  writer_closed t

let connection_is_closed t =
  Alcotest.(check bool) "connection is closed" true (is_closed t)

let request_handler_with_body body reqd =
  Body.Reader.close (Request_descriptor.request_body reqd);
  Request_descriptor.respond_with_string reqd (Response.create `OK) body

let default_request_handler reqd = request_handler_with_body "" reqd

let echo_handler response reqd =
  let request_body = Request_descriptor.request_body reqd in
  let response_body = Request_descriptor.respond_with_streaming reqd response in
  let rec on_read buffer ~off ~len =
    Body.Writer.write_string
      response_body
      (Ache.Bigstring.substring ~off ~len buffer);
    Body.Writer.flush response_body (function
      | `Closed -> assert false
      | `Written ->
        Body.Reader.schedule_read
          ~on_error:(Helpers.body_must_complete __LOC__)
          request_body
          ~on_eof
          ~on_read)
  and on_eof () =
    print_endline "echo handler eof";
    Body.Writer.close response_body
  in
  Body.Reader.schedule_read
    ~on_error:(Helpers.body_must_complete __LOC__)
    request_body
    ~on_eof
    ~on_read

let streaming_handler ?(flush = false) ?(error = false) response writes reqd =
  let writes = ref writes in
  let request_body = Request_descriptor.request_body reqd in
  Body.Reader.close request_body;
  let body =
    Request_descriptor.respond_with_streaming
      ~flush_headers_immediately:flush
      reqd
      response
  in
  let rec write reason =
    match reason with
    | `Closed -> assert false
    | `Written ->
      (match !writes with
      | [] ->
        (match error with
        | false -> Body.Writer.close body
        | true -> Request_descriptor.report_exn reqd (Failure "exn"))
      | w :: ws ->
        Body.Writer.write_string body w;
        writes := ws;
        Body.Writer.flush body write)
  in
  write `Written

let synchronous_raise reqd =
  Request_descriptor.report_exn reqd (Failure "caught this exception")

let error_handler ?request:_ _error start_response =
  let resp_body = start_response Headers.empty in
  Body.Writer.write_string resp_body "got an error";
  Body.Writer.close resp_body

let test_initial_reader_state () =
  let t = create default_request_handler in
  reader_ready ~msg:"A new reader wants input" t

let test_reader_is_closed_after_eof () =
  let t = create default_request_handler in
  let c = read_eof t Ache.Bigstring.empty ~off:0 ~len:0 in
  Alcotest.(check int) "read_eof with no input returns 0" 0 c;
  connection_is_shutdown t;

  let t = create default_request_handler in
  let c = read t Ache.Bigstring.empty ~off:0 ~len:0 in
  Alcotest.(check int) "read with no input returns 0" 0 c;
  let c = read_eof t Ache.Bigstring.empty ~off:0 ~len:0 in
  Alcotest.(check int) "read_eof with no input returns 0" 0 c;
  connection_is_shutdown t

let test_commit_parse_after_every_header () =
  let t = create default_request_handler in
  let request_line = "GET / HTTP/1.1\r\n" in
  let single_header = "Links: /path/to/some/website\r\n" in
  let r =
    (* Each header is 30 bytes *)
    request_line
    ^ single_header
    ^ single_header
    ^ "host: a\r\nconnection: close\r\n\r\n"
  in
  let bs = Ache.Bigstring.of_string r ~off:0 ~len:(String.length r) in
  let c = read t bs ~off:0 ~len:30 in
  Alcotest.(check int) "an incomplete head consumes all input" 30 c;

  let c' = read t bs ~off:c ~len:(String.length single_header) in
  Alcotest.(check int)
    "parser can read a single header and commit"
    (String.length single_header)
    c';

  let c'' = read_eof t bs ~off:(c + c') ~len:(String.length r - (c + c')) in
  Alcotest.(check int)
    "read_eof with the rest of the input is accepted"
    (String.length r - (c + c'))
    c'';
  write_response
    t
    (Response.create ~headers:Headers.(encoding_fixed 0 @ connection_close) `OK);
  connection_is_shutdown t

let test_single_get () =
  (* Single GET *)
  let t = create default_request_handler in
  read_request t (Request.create `GET "/");
  write_response t (Response.create ~headers:(Headers.encoding_fixed 0) `OK);

  (* Single GET, close the connection *)
  let t = create default_request_handler in
  read_request t (Request.create `GET "/" ~headers:Headers.connection_close);
  write_response
    t
    (Response.create ~headers:Headers.(encoding_fixed 0 @ connection_close) `OK);
  connection_is_shutdown t;

  (* Single GET, with reponse body *)
  let response_body = "This is a test" in
  let t = create (request_handler_with_body response_body) in
  read_request t (Request.create `GET "/" ~headers:Headers.connection_close);
  write_response
    t
    ~body:response_body
    (Response.create
       ~headers:
         Headers.(
           encoding_fixed (String.length response_body) @ connection_close)
       `OK);
  connection_is_shutdown t

let test_asynchronous_response () =
  let response_body = "hello, world!" in
  let response_body_length = String.length response_body in
  let response =
    Response.create `OK ~headers:(Headers.encoding_fixed response_body_length)
  in
  let continue = ref (fun () -> ()) in
  let t =
    create (fun reqd ->
      continue :=
        fun () ->
          Body.Reader.close (Request_descriptor.request_body reqd);
          let data =
            Ache.Bigstring.of_string
              ~off:0
              ~len:response_body_length
              response_body
          in
          let size = Ache.Bigstring.length data in
          let response =
            Response.create `OK ~headers:(Headers.encoding_fixed size)
          in
          let response_body =
            Request_descriptor.respond_with_streaming reqd response
          in
          Body.Writer.write_bigstring response_body data;
          Body.Writer.close response_body)
  in
  read_request t (Request.create `GET "/");
  reader_yielded t;
  writer_yielded t;
  !continue ();
  write_response t ~body:response_body response;
  read_request t (Request.create `GET "/");
  reader_yielded t;
  writer_yielded t;
  !continue ();
  write_response t ~body:response_body response

let test_echo_post () =
  let request = Request.create `GET "/" ~headers:Headers.encoding_chunked in

  (* Echo a single chunk *)
  let response = Response.create `OK ~headers:Headers.encoding_chunked in
  let t = create (echo_handler response) in
  read_request t request;
  read_string t "e\r\nThis is a test";
  write_response t ~body:"e\r\nThis is a test\r\n" response;
  read_string t "\r\n0\r\n\r\n";
  write_string t "0\r\n\r\n";
  writer_yielded t;

  (* Echo two chunks *)
  let response = Response.create `OK ~headers:Headers.encoding_chunked in
  let t = create (echo_handler response) in
  read_request t request;
  read_string t "e\r\nThis is a test";
  write_response t ~body:"e\r\nThis is a test\r\n" response;
  read_string t "\r\n21\r\n... that involves multiple chunks";
  write_string t "21\r\n... that involves multiple chunks\r\n";
  read_string t "\r\n0\r\n\r\n";
  write_string t "0\r\n\r\n";
  writer_yielded t;

  (* Echo and close *)
  let response = Response.create `OK ~headers:Headers.connection_close in
  let t = create (echo_handler response) in
  read_request t request;
  read_string t "e\r\nThis is a test";
  write_response t ~body:"This is a test" response;
  read_string t "\r\n21\r\n... that involves multiple chunks";
  write_string t "... that involves multiple chunks";
  read_string t "\r\n0\r\n\r\n";
  connection_is_shutdown t

let test_streaming_response () =
  let request = Request.create `GET "/" in
  let response = Response.create ~headers:(Headers.encoding_fixed 12) `OK in

  let t = create (streaming_handler response [ "Hello "; "world!" ]) in
  read_request t request;
  write_response t ~body:"Hello " response;
  write_string t "world!";
  writer_yielded t

let test_asynchronous_streaming_response () =
  let request = Request.create `GET "/" ~headers:Headers.connection_close in
  let response = Response.create `OK in

  let body = ref None in
  let t =
    create (fun reqd ->
      body := Some (Request_descriptor.respond_with_streaming reqd response))
  in

  writer_yielded t;
  let writer_woken_up =
    on_writer_unyield t (fun () ->
      write_response
        t
        ~body:"Hello "
        (Response.create ~headers:Headers.connection_close `OK))
  in

  read_request t request;
  let body =
    match !body with None -> failwith "no body found" | Some body -> body
  in
  Body.Writer.write_string body "Hello ";
  Alcotest.(check bool) "Writer not woken up" false !writer_woken_up;
  Body.Writer.flush body ignore;
  Alcotest.(check bool) "Writer woken up" true !writer_woken_up;

  writer_yielded t;
  let writer_woken_up =
    on_writer_unyield t (fun () ->
      write_string t "world!";
      writer_closed t)
  in
  Body.Writer.write_string body "world!";
  Alcotest.(check bool) "Writer not woken up" false !writer_woken_up;
  Body.Writer.close body;
  Alcotest.(check bool) "Writer woken up" true !writer_woken_up

let test_asynchronous_streaming_response_with_immediate_flush () =
  let request = Request.create `GET "/" ~headers:Headers.connection_close in
  let response = Response.create `OK in

  let body = ref None in
  let t =
    create (fun reqd ->
      body :=
        Some
          (Request_descriptor.respond_with_streaming
             reqd
             response
             ~flush_headers_immediately:true))
  in
  writer_yielded t;
  let writer_woken_up =
    on_writer_unyield t (fun () ->
      write_response t (Response.create ~headers:Headers.connection_close `OK))
  in
  Alcotest.(check bool) "Writer not woken up" false !writer_woken_up;

  read_request t request;
  let body =
    match !body with None -> failwith "no body found" | Some body -> body
  in
  Alcotest.(check bool) "Writer woken up" true !writer_woken_up;

  writer_yielded t;
  let writer_woken_up = on_writer_unyield t (fun () -> writer_closed t) in
  Body.Writer.close body;
  Alcotest.(check bool) "Writer woken up" true !writer_woken_up

let test_empty_fixed_streaming_response () =
  let request = Request.create `GET "/" in
  let response = Response.create `OK ~headers:(Headers.encoding_fixed 0) in

  let t = create (streaming_handler response []) in
  read_request t request;
  write_response t response;
  writer_yielded t

let test_empty_chunked_streaming_response () =
  let request = Request.create `GET "/" in
  let response = Response.create `OK ~headers:Headers.encoding_chunked in

  let t = create (streaming_handler response []) in
  read_request t request;
  write_response t response ~body:"0\r\n\r\n";
  writer_yielded t

let test_multiple_get () =
  let t = create default_request_handler in
  read_request t (Request.create `GET "/");
  write_response t (Response.create ~headers:(Headers.encoding_fixed 0) `OK);
  read_request t (Request.create `GET "/");
  write_response t (Response.create ~headers:(Headers.encoding_fixed 0) `OK)

let test_asynchronous_streaming_response_flush_immediately () =
  let continue_response = ref (fun () -> ()) in
  let request = Request.create `GET "/" in
  let response = Response.create ~headers:(Headers.encoding_fixed 5) `OK in
  let request_handler reqd =
    let body =
      Request_descriptor.respond_with_streaming
        ~flush_headers_immediately:true
        reqd
        response
    in
    continue_response :=
      fun () ->
        Body.Writer.write_string body "hello";
        Body.Writer.close body
  in
  let t = create request_handler in
  read_request t request;
  write_response t response;
  writer_yielded t;
  let writer_woken_up = on_writer_unyield t ignore in
  !continue_response ();
  Alcotest.(check bool) "Writer woken up" true !writer_woken_up;
  write_string t "hello";
  writer_yielded t

(* Writer doesn't get a chance to yield before incoming request. *)
let test_asynchronous_streaming_response_writer_doesnt_yield () =
  let continue_response = ref (fun () -> ()) in
  let request = Request.create `GET "/" in
  let response = Response.create ~headers:(Headers.encoding_fixed 5) `OK in
  let request_handler reqd =
    continue_response :=
      fun () ->
        let body = Request_descriptor.respond_with_streaming reqd response in
        Body.Writer.write_string body "hello";
        Body.Writer.close body
  in
  let t = create request_handler in
  read_request t request;
  writer_yielded t;
  let writer_woken_up = on_writer_unyield t ignore in
  !continue_response ();
  write_response t ~body:"hello" response;
  Alcotest.(check bool) "Writer woken up" true !writer_woken_up;
  writer_yielded t

let test_connection_error () =
  let t = create ~error_handler (fun _ -> assert false) in
  writer_yielded t;
  let writer_woken_up = on_writer_unyield t ignore in
  report_exn t (Failure "connection failure");
  Alcotest.(check bool) "Writer woken up" true !writer_woken_up;
  write_response
    t
    ~msg:"Error response written"
    (Response.create ~headers:Headers.connection_close `Internal_server_error);
  write_string t "got an error"

let test_synchronous_error () =
  let t = create ~error_handler synchronous_raise in
  writer_yielded t;
  let writer_woken_up = on_writer_unyield t ignore in
  read_request t (Request.create `GET "/");
  reader_errored t;
  Alcotest.(check bool) "Writer woken up" true !writer_woken_up;
  write_response
    t
    ~msg:"Error response written"
    ~body:"got an error"
    (Response.create ~headers:Headers.connection_close `Internal_server_error)

let test_synchronous_error_asynchronous_handling () =
  let continue = ref (fun () -> ()) in
  let error_handler ?request error start_response =
    continue := fun () -> error_handler ?request error start_response
  in
  let t = create ~error_handler synchronous_raise in
  writer_yielded t;
  let writer_woken_up = on_writer_unyield t ignore in
  read_request t (Request.create `GET "/");
  reader_errored t;
  writer_yielded t;
  Alcotest.(check bool) "Writer not woken up" false !writer_woken_up;
  !continue ();
  Alcotest.(check bool) "Writer woken up" true !writer_woken_up;
  (* This shows up in two flushes because [Request_descriptor] creates error
     reposnses with [~flush_headers_immediately:true] *)
  write_response
    t
    ~msg:"Error response written"
    (Response.create ~headers:Headers.connection_close `Internal_server_error);
  write_string t "got an error"

let test_asynchronous_error () =
  let continue = ref (fun () -> ()) in
  let asynchronous_raise reqd = continue := fun () -> synchronous_raise reqd in
  let t = create ~error_handler asynchronous_raise in
  writer_yielded t;
  let writer_woken_up = on_writer_unyield t ignore in
  read_request t (Request.create `GET "/");
  Alcotest.(check bool) "Writer not woken up" false !writer_woken_up;
  writer_yielded t;
  reader_yielded t;
  !continue ();
  Alcotest.(check bool) "Writer woken up" true !writer_woken_up;
  (* This shows up in two flushes because [Request_descriptor] creates error
     reposnses with [~flush_headers_immediately:true] *)
  write_response
    t
    ~msg:"Error response written"
    (Response.create ~headers:Headers.connection_close `Internal_server_error);
  write_string t "got an error";
  reader_errored t;
  connection_is_shutdown t

let test_asynchronous_error_asynchronous_handling () =
  let continue_request = ref (fun () -> ()) in
  let asynchronous_raise reqd =
    continue_request := fun () -> synchronous_raise reqd
  in
  let continue_error = ref (fun () -> ()) in
  let error_handler ?request error start_response =
    continue_error := fun () -> error_handler ?request error start_response
  in
  let t = create ~error_handler asynchronous_raise in
  writer_yielded t;
  let writer_woken_up = on_writer_unyield t ignore in
  read_request t (Request.create `GET "/");
  Alcotest.(check bool) "Writer not woken up" false !writer_woken_up;
  writer_yielded t;
  reader_yielded t;
  !continue_request ();
  Alcotest.(check bool) "Writer not woken up" false !writer_woken_up;
  writer_yielded t;
  !continue_error ();
  Alcotest.(check bool) "Writer woken up" true !writer_woken_up;
  (* This shows up in two flushes because [Request_descriptor] creates error
     responses with [~flush_headers_immediately:true] *)
  write_response
    t
    ~msg:"Error response written"
    (Response.create ~headers:Headers.connection_close `Internal_server_error);
  write_string t "got an error";
  reader_errored t

let test_asynchronous_error_asynchronous_response_body () =
  let continue_request = ref (fun () -> ()) in
  let asynchronous_raise reqd =
    continue_request := fun () -> synchronous_raise reqd
  in
  let continue_error = ref (fun () -> ()) in
  let error_handler ?request:_ _error start_response =
    continue_error :=
      fun () ->
        let resp_body = start_response Headers.empty in
        continue_error :=
          fun () ->
            Body.Writer.write_string resp_body "got an error";
            Body.Writer.close resp_body
  in
  let t = create ~error_handler asynchronous_raise in
  writer_yielded t;
  let writer_woken_up = on_writer_unyield t ignore in
  read_request t (Request.create `GET "/");
  writer_yielded t;
  reader_yielded t;
  !continue_request ();
  writer_yielded t;
  !continue_error ();
  Alcotest.(check bool) "Writer woken up" true !writer_woken_up;
  write_response
    t
    ~msg:"Error response written"
    (Response.create ~headers:Headers.connection_close `Internal_server_error);
  writer_yielded t;
  let writer_woken_up = on_writer_unyield t ignore in
  !continue_error ();
  write_string t "got an error";
  Alcotest.(check bool) "Writer woken up" true !writer_woken_up;
  reader_errored t;
  connection_is_shutdown t

let test_error_while_parsing () =
  let continue_error = ref (fun () -> ()) in
  let error_handler ?request error start_response =
    continue_error := fun () -> error_handler ?request error start_response
  in
  let setup () =
    let t = create ~error_handler (fun _ -> assert false) in
    let n = feed_string t "GET / HTTP/1.1\r\n" in
    Alcotest.(check int) "read bytes" 16 n;
    reader_ready t;
    report_exn t (Failure "runtime error during parse");
    t
  in

  (* Handle before read *)
  let t = setup () in
  !continue_error ();
  write_response
    t
    ~msg:"Error response written"
    (Response.create ~headers:Headers.connection_close `Internal_server_error);
  write_string t "got an error";
  writer_closed t;
  (* XXX(dpatti): Runtime is in a read loop and must report something. I don't
     know if this could ever deadlock or if that's a runtime concern. *)
  reader_ready t;
  let n = feed_string t "Host: localhost\r\n" in
  (* NOTE(anmonteiro): we commit after parsing each single header, so this
     differs from the upstream test. *)
  Alcotest.(check int) "read bytes" 17 n;
  reader_closed t;

  (* Read before handle *)
  let t = setup () in
  reader_ready t;
  let n = feed_string t "Host: localhost\r\n" in
  Alcotest.(check int) "read bytes" 17 n;
  reader_closed t;
  !continue_error ();
  write_response
    t
    ~msg:"Error response written"
    (Response.create ~headers:Headers.connection_close `Internal_server_error);
  write_string t "got an error";
  writer_closed t

let test_error_before_read () =
  let request_handler _ = assert false in
  let invoked_error_handler = ref false in
  let error_handler ?request:_ _ _ = invoked_error_handler := true in
  let t = create ~error_handler request_handler in
  report_exn t (Failure "immediate runtime error");
  reader_ready t;
  writer_yielded t;
  (* XXX(dpatti): This seems wrong to me. Should we be sending responses when we
     haven't even read any bytes yet? Maybe too much of an edge case to
     worry. *)
  Alcotest.(check bool) "Error handler was invoked" true !invoked_error_handler

let test_error_left_unhandled () =
  let error_handler ?request:_ _ _ = () in
  let t = create ~error_handler (fun _ -> ()) in
  read_request t (Request.create `GET "/");
  report_exn t (Failure "runtime error");
  (* If the error handler is invoked and does not try to complete a response,
     the connection will hang. This is not necessarily desirable but rather a
     tradeoff to let the user respond asynchronously. *)
  reader_yielded t;
  writer_yielded t

let test_chunked_encoding () =
  let request_handler reqd =
    let response = Response.create `OK ~headers:Headers.encoding_chunked in
    let resp_body = Request_descriptor.respond_with_streaming reqd response in
    Body.Writer.write_string resp_body "First chunk";
    Body.Writer.flush resp_body (function
      | `Closed -> assert false
      | `Written ->
        Body.Writer.write_string resp_body "Second chunk";
        Body.Writer.close resp_body)
  in
  let t = create ~error_handler request_handler in
  writer_yielded t;
  read_request t (Request.create `GET "/");
  write_response
    t
    ~msg:"First chunk written"
    ~body:"b\r\nFirst chunk\r\n"
    (Response.create `OK ~headers:Headers.encoding_chunked);
  write_string t ~msg:"Second chunk" "c\r\nSecond chunk\r\n";
  write_string t ~msg:"Final chunk written" "0\r\n\r\n";
  reader_ready ~msg:"Keep-alive" t

let test_chunked_encoding_for_error () =
  let error_handler ?request error start_response =
    Alcotest.(check (option request)) "No parsed request" None request;
    Alcotest.(check request_error)
      "Request error"
      (`Bad_request "unexpected ' ' in request-line")
      error;
    let body = start_response Headers.encoding_chunked in
    Body.Writer.write_string body "Bad";
    Body.Writer.flush body (function
      | `Closed -> assert false
      | `Written ->
        Body.Writer.write_string body " request";
        Body.Writer.close body)
  in
  let t = create ~error_handler (fun _ -> assert false) in
  let c = feed_string t "  X\r\n\r\n" in
  Alcotest.(check int) "A failed head consumes the bytes it was given" 7 c;
  write_response
    t
    (Response.create
       `Bad_request
       ~headers:Headers.(encoding_chunked @ connection_close));
  write_string t "3\r\nBad\r\n";
  write_string t "8\r\n request\r\n";
  write_string t "0\r\n\r\n";
  connection_is_shutdown t

let test_blocked_write_on_chunked_encoding () =
  let request_handler reqd =
    let response = Response.create `OK ~headers:Headers.encoding_chunked in
    let resp_body = Request_descriptor.respond_with_streaming reqd response in
    Body.Writer.write_string resp_body "gets partially written";
    Body.Writer.flush resp_body ignore
    (* Response body never gets closed but for the purposes of the test, that's
     * OK. *)
  in
  let t = create ~error_handler request_handler in
  writer_yielded t;
  read_request t (Request.create `GET "/");
  let response_bytes =
    "HTTP/1.1 200 OK\r\n\
     transfer-encoding: chunked\r\n\
     \r\n\
     16\r\n\
     gets partially written\r\n"
  in
  let second_write =
    write_partial_string t ~msg:"first write" response_bytes 16
  in
  write_string t ~msg:"second write" second_write

module Upgraded = struct
  type t = { mutable driven : bool }

  let next_read_operation t =
    t.driven <- true;
    `Yield

  let read _ _ ~off:_ ~len:_ = 0
  let read_eof _ _ ~off:_ ~len:_ = 0
  let yield_reader _ _ = ()
  let next_write_operation _ = `Yield
  let report_write_result _ _ = ()
  let yield_writer _ _ = ()
  let report_exn _ _ = ()
  let is_closed _ = false
  let shutdown _ = ()
end

let upgrade_handler upgraded reqd =
  Request_descriptor.respond_with_upgrade
    reqd
    Headers.empty
    (Ache.Runtime.make (module Upgraded) upgraded)

let test_respond_with_upgrade () =
  let upgraded = { Upgraded.driven = false } in
  let t = create ~error_handler (upgrade_handler upgraded) in
  read_request t (Request.create `GET "/");
  Alcotest.(check bool) "Not upgraded before the response" false upgraded.driven;
  write_response
    ~msg:"Upgrade response written"
    t
    (Response.create `Switching_protocols);
  Alcotest.(check bool) "Upgraded protocol was called" true upgraded.driven;
  reader_yielded t

let test_upgrade_write_closed () =
  let upgraded = { Upgraded.driven = false } in
  let t = create ~error_handler (upgrade_handler upgraded) in
  read_request t (Request.create `GET "/");
  write_eof t;
  Alcotest.(check bool) "Upgraded protocol was not called" false upgraded.driven;
  connection_is_closed t;
  reader_closed t;
  writer_closed
    t
    ~unread:
      (String.length
         (response_to_string (Response.create `Switching_protocols)))

let test_unexpected_eof () =
  let t = create default_request_handler in
  read_request t (Request.create `GET "/");
  write_eof t;
  writer_closed t ~unread:38

let test_input_shrunk () =
  let continue_response = ref (fun () -> ()) in
  let error_handler ?request:_ _ = assert false in
  let request_handler reqd =
    Alcotest.(check (list (pair string string)))
      "got expected headers"
      [ "Host", "example.com"
      ; "Connection", "close"
      ; "Accept", "application/json, text/plain, */*"
      ; "Accept-Language", "en-US,en;q=0.5"
      ]
      (Headers.to_list (Request_descriptor.request reqd).headers);
    Body.Reader.close (Request_descriptor.request_body reqd);
    continue_response :=
      fun () ->
        Request_descriptor.respond_with_string reqd (Response.create `OK) ""
  in
  let t = create ~error_handler request_handler in
  reader_ready t;
  writer_yielded t;
  let writer_woken_up =
    on_writer_unyield t (fun () ->
      write_response
        t
        (Response.create
           ~headers:Headers.(encoding_fixed 0 @ connection_close)
           `OK))
  in
  let len = feed_string t "GET /v1/b HTTP/1.1\r\nH" in
  Alcotest.(check int) "partial read" 21 len;
  read_string
    t
    "ost: example.com\r\n\
     Connection: close\r\n\
     Accept: application/json, text/plain, */*\r\n\
     Accept-Language: en-US,en;q=0.5\r\n\
     \r\n";
  writer_yielded t;
  Alcotest.(check bool) "Writer not woken up" false !writer_woken_up;
  reader_closed t;
  !continue_response ();
  Alcotest.(check bool) "Writer woken up" true !writer_woken_up;
  writer_closed t

let malformed_request_string =
  "GET / HTTP/1.1\r\n\
   connection: close\r\n\
   X-Other-Header : shouldnt_have_space_before_colon\r\n\
   \r\n"

let eof_request_string =
  "GET / HTTP/1.1\r\nconnection: close\r\nX-Other-Header: EOF_after_this"

let basic_handler body reqd =
  let request_body = Request_descriptor.request_body reqd in
  Body.Reader.close request_body;
  Request_descriptor.respond_with_string reqd (Response.create `OK) body

let test_malformed conn =
  writer_yielded conn;
  let writer_woken_up = on_writer_unyield conn ignore in
  let len = String.length malformed_request_string in
  let input = Ache.Bigstring.of_string malformed_request_string ~off:0 ~len in
  let c = read conn input ~off:0 ~len in
  Alcotest.(check int) "a failed head consumes the bytes it was given" len c;
  reader_errored conn;
  Alcotest.(check bool) "Writer woken up" true !writer_woken_up

let test_malformed_request () =
  let t = create ~error_handler (basic_handler "") in
  test_malformed t;
  write_response
    t
    (Response.create ~headers:Headers.connection_close `Bad_request);
  write_string t "got an error"

let test_malformed_request_async () =
  let continue = ref (fun () -> ()) in
  let error_handler ?request:_ _error start_response =
    let resp_body = start_response Headers.empty in
    continue :=
      fun () ->
        Body.Writer.write_string resp_body "got an error";
        Body.Writer.close resp_body
  in
  let t = create ~error_handler (basic_handler "") in
  test_malformed t;
  !continue ();
  write_response
    t
    (Response.create ~headers:Headers.connection_close `Bad_request);
  write_string t "got an error"

let test_malformed_request_async_multiple_errors () =
  let continue = ref (fun () -> ()) in
  let error_handler ?request:_ _error start_response =
    let resp_body = start_response Headers.empty in
    continue :=
      fun () ->
        Body.Writer.write_string resp_body "got an error";
        Body.Writer.close resp_body
  in
  let t = create ~error_handler (basic_handler "") in
  test_malformed t;
  !continue ();
  let len = String.length malformed_request_string in
  let input = Ache.Bigstring.of_string malformed_request_string ~off:0 ~len in
  let c = force_read t input ~off:0 ~len in
  Alcotest.(check int) "read doesn't consume more input" 0 c;
  write_response
    t
    (Response.create ~headers:Headers.connection_close `Bad_request);
  write_string t "got an error"

let read_string_eof t str =
  let len = String.length str in
  let input = Ache.Bigstring.of_string str ~off:0 ~len in
  read_eof t input ~off:0 ~len

let test_malformed_request_eof () =
  let t = create ~error_handler (basic_handler "") in
  test_malformed t;
  write_response
    t
    (Response.create ~headers:Headers.connection_close `Bad_request);
  write_string t "got an error"

let streaming_error_handler
      ?(headers = Headers.empty)
      continue_error
      ?request:_
      _error
      start_response
  =
  let resp_body = start_response headers in
  continue_error :=
    fun () ->
      Body.Writer.write_string resp_body "got an error\n";
      Body.Writer.flush resp_body (function
        | `Closed -> assert false
        | `Written ->
          continue_error :=
            fun () ->
              Body.Writer.write_string resp_body "more output";
              Body.Writer.close resp_body)

let test_malformed_request_streaming_error_response () =
  let continue_error = ref (fun () -> ()) in
  let error_handler ?request error start_response =
    continue_error :=
      fun () ->
        streaming_error_handler continue_error ?request error start_response
  in
  let t = create ~error_handler (basic_handler "") in
  writer_yielded t;
  let writer_woken_up = on_writer_unyield t ignore in
  let c = read_string_eof t eof_request_string in
  Alcotest.(check int)
    "read consumes all input"
    (String.length eof_request_string)
    c;
  reader_errored t;
  !continue_error ();
  Alcotest.(check bool) "Writer woken up" true !writer_woken_up;
  write_response
    t
    (Response.create `Bad_request ~headers:Headers.connection_close);
  writer_yielded t;
  let writer_woken_up = on_writer_unyield t ignore in
  !continue_error ();
  write_string t ~msg:"First part of the response body written" "got an error\n";
  Alcotest.(check bool)
    "Writer woken up once more input is available"
    true
    !writer_woken_up;
  !continue_error ();
  write_string t ~msg:"Rest of the error response written" "more output";
  writer_closed t;
  connection_is_shutdown t

let chunked_error_handler continue_error ?request:_ _error start_response =
  let resp_body =
    start_response (Headers.of_list [ "transfer-encoding", "chunked" ])
  in
  Body.Writer.write_string resp_body "chunk 1\n";
  Body.Writer.flush resp_body (function
    | `Closed -> assert false
    | `Written ->
      continue_error :=
        fun () ->
          Body.Writer.write_string resp_body "chunk 2\n";
          Body.Writer.flush resp_body (function
            | `Closed -> assert false
            | `Written ->
              continue_error :=
                fun () ->
                  Body.Writer.write_string resp_body "chunk 3\n";
                  Body.Writer.close resp_body))

let test_malformed_request_chunked_error_response () =
  let continue_error = ref (fun () -> ()) in
  let error_handler ?request error start_response =
    continue_error :=
      fun () ->
        chunked_error_handler continue_error ?request error start_response
  in
  let t = create ~error_handler (basic_handler "") in
  writer_yielded t;
  let writer_woken_up = on_writer_unyield t ignore in
  let c = read_string_eof t eof_request_string in
  Alcotest.(check int)
    "read consumes all input"
    (String.length eof_request_string)
    c;
  reader_errored t;
  Alcotest.(check bool) "Writer hasn't woken up yet" false !writer_woken_up;
  !continue_error ();
  Alcotest.(check bool) "Writer woken up" true !writer_woken_up;
  write_response
    t
    ~msg:"First chunk written"
    (Response.create
       `Bad_request
       ~headers:
         (Headers.of_list
            [ "transfer-encoding", "chunked"; "connection", "close" ]));
  write_string t "8\r\nchunk 1\n\r\n";
  writer_yielded t;
  let writer_woken_up = on_writer_unyield t ignore in
  !continue_error ();
  write_string t ~msg:"Second chunk" "8\r\nchunk 2\n\r\n";
  !continue_error ();
  write_string t ~msg:"Second chunk" "8\r\nchunk 3\n\r\n";
  write_string t ~msg:"Final chunk written" "0\r\n\r\n";
  Alcotest.(check bool)
    "Writer woken up once more input is available"
    true
    !writer_woken_up;
  writer_closed t;
  connection_is_shutdown t

(* This may happen when writing an asynchronous error response on a broken *
   pipe. *)
let test_malformed_request_double_report_exn () =
  let continue_error = ref (fun () -> ()) in
  let error_handler ?request error start_response =
    continue_error :=
      fun () ->
        streaming_error_handler continue_error ?request error start_response
  in
  let t = create ~error_handler (basic_handler "") in
  writer_yielded t;
  let writer_woken_up = on_writer_unyield t ignore in
  let c = read_string_eof t eof_request_string in
  Alcotest.(check int)
    "read consumes all input"
    (String.length eof_request_string)
    c;
  reader_errored t;
  Alcotest.(check bool) "Writer hasn't woken up yet" false !writer_woken_up;
  !continue_error ();
  Alcotest.(check bool) "Writer woken up" true !writer_woken_up;
  write_eof t;
  report_exn t (Failure "broken pipe");
  writer_closed t ~unread:47;
  connection_is_closed t

let test_immediate_flush_empty_body () =
  let response = Response.create ~headers:(Headers.encoding_fixed 0) `OK in
  let request_handler reqd =
    let resp_body =
      Request_descriptor.respond_with_streaming
        ~flush_headers_immediately:true
        reqd
        response
    in
    Body.Writer.close resp_body
  in
  let t = create ~error_handler request_handler in
  reader_ready t;
  writer_yielded t;
  read_request t (Request.create `GET "/");
  write_response t response

let test_empty_body_no_immediate_flush () =
  let response = Response.create ~headers:(Headers.encoding_fixed 0) `OK in
  let request_handler reqd =
    let resp_body =
      Request_descriptor.respond_with_streaming
        ~flush_headers_immediately:false
        reqd
        response
    in
    Body.Writer.close resp_body
  in
  let t = create ~error_handler request_handler in
  reader_ready t;
  writer_yielded t;
  read_request t (Request.create `GET "/");
  write_response t ~body:"" response;
  writer_yielded t

let test_yield_before_starting_a_response () =
  let response = Response.create ~headers:(Headers.encoding_fixed 0) `OK in
  let continue_response = ref (fun () -> ()) in
  let request_handler reqd =
    Body.Reader.close (Request_descriptor.request_body reqd);
    continue_response :=
      fun () ->
        let resp_body =
          Request_descriptor.respond_with_streaming reqd response
        in
        Body.Writer.close resp_body
  in
  let t = create ~error_handler request_handler in
  reader_ready t;
  writer_yielded t;
  read_request t (Request.create `GET "/");
  reader_yielded t;
  let reader_woken_up = on_reader_unyield t ignore in
  Alcotest.(check bool) "Reader hasn't woken up yet" false !reader_woken_up;
  !continue_response ();
  write_response t ~body:"" response;
  writer_yielded t;
  Alcotest.(check bool) "Reader woken up" true !reader_woken_up

let test_respond_before_reading_entire_body () =
  let response = Response.create ~headers:(Headers.encoding_fixed 0) `OK in
  let continue_response = ref (fun () -> ()) in
  let request_handler reqd =
    (* Important that we never close the request body for this test. *)
    continue_response :=
      fun () ->
        let resp_body =
          Request_descriptor.respond_with_streaming reqd response
        in
        Body.Writer.close resp_body
  in
  let t = create ~error_handler request_handler in
  reader_ready t;
  writer_yielded t;
  read_request
    t
    (Request.create
       `GET
       "/"
       ~headers:(Headers.of_list [ "content-length", "2" ]));
  reader_yielded t;
  let reader_woken_up = on_reader_unyield t ignore in
  Alcotest.(check bool) "Reader hasn't woken up yet" false !reader_woken_up;
  !continue_response ();
  write_response t ~body:"" response;
  writer_yielded t;
  Alcotest.(check bool) "Reader woken up" true !reader_woken_up

let backpressure_request_handler continue_reading reqd =
  let request_body = Request_descriptor.request_body reqd in
  let rec on_read _buffer ~off:_ ~len:_ =
    continue_reading :=
      fun () ->
        Body.Reader.schedule_read
          ~on_error:(Helpers.body_must_complete __LOC__)
          request_body
          ~on_eof
          ~on_read
  and on_eof () =
    print_endline
      ("got eof" ^ string_of_bool (Body.Reader.is_closed request_body))
  in
  Body.Reader.schedule_read
    ~on_error:(Helpers.body_must_complete __LOC__)
    request_body
    ~on_eof
    ~on_read

let test_handling_backpressure_when_read_not_scheduled () =
  let continue_reading = ref (fun () -> ()) in
  let t =
    create ~error_handler (backpressure_request_handler continue_reading)
  in
  reader_ready t;
  writer_yielded t;
  let request =
    Request.create
      `GET
      ~headers:(Headers.of_list [ "content-length", "10" ])
      "/"
  in
  read_request t request;
  read_string t "five.";
  reader_yielded t;
  let reader_woken_up = on_reader_unyield t ignore in
  !continue_reading ();
  Alcotest.(check bool)
    "Reader wakes up if scheduling read"
    true
    !reader_woken_up;
  reader_ready
    ~msg:"Reader wants to read if there's a read scheduled in the body"
    t;
  writer_yielded t

let test_handling_backpressure_when_read_not_scheduled_early_yield () =
  let continue_reading = ref (fun () -> ()) in
  let t =
    create ~error_handler (backpressure_request_handler continue_reading)
  in
  reader_ready t;
  writer_yielded t;
  let request =
    Request.create
      `GET
      ~headers:(Headers.of_list [ "content-length", "10" ])
      "/"
  in
  read_request t request;
  let reader_woken_up = on_reader_unyield t ignore in
  read_string t "five.";
  reader_yielded t;
  !continue_reading ();
  Alcotest.(check bool)
    "Reader wakes up if scheduling read"
    true
    !reader_woken_up;
  reader_ready
    ~msg:"Reader wants to read if there's a read scheduled in the body"
    t;
  writer_yielded t

let test_input_shrunk_chunked () =
  let continue_response = ref (fun () -> ()) in
  let error_handler ?request:_ _ = assert false in
  let request_handler reqd =
    Alcotest.(check (list (pair string string)))
      "got expected headers"
      [ "Host", "example.com"; "Transfer-Encoding", "chunked" ]
      (Headers.to_list (Request_descriptor.request reqd).headers);
    Body.Reader.close (Request_descriptor.request_body reqd);
    continue_response :=
      fun () ->
        Request_descriptor.respond_with_string reqd (Response.create `OK) ""
  in
  let t = create ~error_handler request_handler in
  reader_ready t;
  writer_yielded t;
  (* never happens*)
  let _writer_woken_up =
    on_writer_unyield t (fun () -> write_response t (Response.create `OK))
  in
  let len = feed_string t "GET /v1/b HTTP/1.1\r\nH" in
  Alcotest.(check int) "partial read" 21 len;
  read_string t "ost: example.com\r\nTransfer-Encoding: chunked\r\n\r\n";

  let str = "5\r\ninput\r\n" in
  let len = String.length str in
  let input = Ache.Bigstring.of_string str ~off:0 ~len in
  let just_read = force_read t input ~off:0 ~len in
  Alcotest.(check int) "chunk and its CRLF read" len just_read;

  let just_read = force_read_eof t input ~off:len ~len:0 in
  Alcotest.(check int)
    "eof partial read, doesn't get terminating chunk"
    0
    just_read;

  writer_yielded t
(* TODO: test error handling. *)
(* reader_closed t;
  !continue_response ();
  writer_closed t; *)

let test_respond_before_reading_entire_body_chunked_eof () =
  let response = Response.create ~headers:(Headers.encoding_fixed 0) `OK in
  let continue_response = ref (fun () -> ()) in
  let request_handler reqd =
    (* Important that we never close the request body for this test. *)
    continue_response :=
      fun () ->
        let resp_body =
          Request_descriptor.respond_with_streaming reqd response
        in
        Body.Writer.close resp_body
  in
  let error_handler ?request:_ _error _start_response =
    Alcotest.fail
      "Expected error_handler not to be called because the response was \
       already sent"
  in
  let t = create ~error_handler request_handler in
  reader_ready t;
  writer_yielded t;
  read_request
    t
    (Request.create
       `GET
       "/"
       ~headers:(Headers.of_list [ "transfer-encoding", "chunked" ]));
  let reader_woken_up = on_reader_unyield t ignore in
  Alcotest.(check bool) "Reader hasn't woken up yet" false !reader_woken_up;
  let writer_woken_up = on_writer_unyield t ignore in
  !continue_response ();
  Alcotest.(check bool) "Writer woken up" true !writer_woken_up;
  write_response t ~body:"" response;
  writer_yielded t;
  Alcotest.(check bool) "Reader woken up" true !reader_woken_up;
  reader_ready t;

  let str = "5\r\ninput\r\n" in
  let len = String.length str in
  let input = Ache.Bigstring.of_string str ~off:0 ~len in
  let just_read = read_eof t input ~off:0 ~len in
  Alcotest.(check int)
    "malformed chunked encoding read completely"
    len
    just_read;

  writer_closed t;
  reader_errored t

let test_finish_response_after_read_eof () =
  let response = Response.create ~headers:(Headers.encoding_fixed 0) `OK in
  let continue_response = ref (fun () -> ()) in
  let request_handler reqd =
    (* Important that we never close the request body for this test. *)
    let resp_body = Request_descriptor.respond_with_streaming reqd response in
    continue_response := fun () -> Body.Writer.close resp_body
  in
  let error_handler ?request:_ _error _start_response =
    Alcotest.fail
      "Expected error_handler not to be called because the response was \
       already sent"
  in
  let t = create ~error_handler request_handler in
  reader_ready t;
  writer_yielded t;
  read_request
    t
    (Request.create
       `GET
       "/"
       ~headers:(Headers.of_list [ "transfer-encoding", "chunked" ]));
  let reader_woken_up = on_reader_unyield t ignore in
  Alcotest.(check bool) "Reader hasn't woken up yet" false !reader_woken_up;
  let writer_woken_up = on_writer_unyield t ignore in

  let str = "5\r\ninput\r\n" in
  let len = String.length str in
  let input = Ache.Bigstring.of_string str ~off:0 ~len in
  let just_read = force_read_eof t input ~off:0 ~len in
  Alcotest.(check int)
    "malformed chunked encoding read completely"
    len
    just_read;

  let (_ : Read_operation.t) =
    with_server_connection t (fun t -> Server_connection.next_read_operation t)
  in
  reader_errored t;
  Alcotest.(check bool) "Writer woken up" true !writer_woken_up;
  write_response t ~body:"" response;
  !continue_response ();
  writer_closed t;
  reader_closed t

let test_respond_before_reading_entire_body_no_error () =
  let response = Response.create ~headers:(Headers.encoding_fixed 0) `OK in
  let continue_response = ref (fun () -> ()) in
  let request_handler reqd =
    (* Important that we never close the request body for this test. *)
    continue_response :=
      fun () ->
        let resp_body =
          Request_descriptor.respond_with_streaming reqd response
        in
        Body.Writer.close resp_body
  in
  let error_handler ?request:_ _error _start_response = assert false in
  let t = create ~error_handler request_handler in
  reader_ready t;
  writer_yielded t;
  read_request
    t
    (Request.create
       `GET
       "/"
       ~headers:(Headers.of_list [ "content-length", "10" ]));
  let reader_woken_up = on_reader_unyield t ignore in
  Alcotest.(check bool) "Reader hasn't woken up yet" false !reader_woken_up;
  let writer_woken_up = on_writer_unyield t ignore in

  force_read_string t "data.";
  !continue_response ();
  Alcotest.(check bool) "Writer woken up" true !writer_woken_up;
  write_response t ~body:"" response;
  writer_yielded t;
  let writer_woken_up = on_writer_unyield t ignore in
  reader_ready t;
  (* Yield writer before feeding eof. * * Note: writer here is done. It yields
     before we feed more to the reader * to allow for it to complete. *)
  read_string t "final";

  (* Ready for the next request *)
  reader_ready t;
  Alcotest.(check bool)
    "Writer hasn't woken up yet, still yielding"
    false
    !writer_woken_up

let test_streaming_response_before_reading_entire_body_no_error () =
  let response = Response.create ~headers:(Headers.encoding_fixed 5) `OK in
  let continue_response = ref (fun () -> ()) in
  let request_handler reqd =
    (* Important that we never close the request body for this test. *)
    continue_response :=
      fun () ->
        let resp_body =
          Request_descriptor.respond_with_streaming reqd response
        in
        continue_response :=
          fun () ->
            Body.Writer.write_string resp_body "hello";
            Body.Writer.flush resp_body (function
              | `Closed -> assert false
              | `Written ->
                continue_response := fun () -> Body.Writer.close resp_body)
  in
  let error_handler ?request:_ _error _start_response = assert false in
  let t = create ~error_handler request_handler in
  reader_ready t;
  writer_yielded t;
  read_request
    t
    (Request.create
       `GET
       "/"
       ~headers:(Headers.of_list [ "content-length", "10" ]));
  let reader_woken_up = on_reader_unyield t ignore in
  Alcotest.(check bool) "Reader hasn't woken up yet" false !reader_woken_up;
  let writer_woken_up = on_writer_unyield t ignore in

  force_read_string t "data.";
  !continue_response ();
  Alcotest.(check bool) "Writer not woken up" false !writer_woken_up;

  !continue_response ();
  Alcotest.(check bool) "Writer woken up" true !writer_woken_up;
  write_response t ~body:"hello" response;

  writer_yielded t;
  let writer_woken_up = on_writer_unyield t ignore in
  !continue_response ();
  (* Important that the writer wakes up after closing it, so that it gets a *
     chance to close the request body, and thus advance the remaining request *
     body bytes to prepare the parser for the next request. *)
  Alcotest.(check bool) "Writer woken up" true !writer_woken_up;
  writer_yielded t;

  reader_ready t;
  read_string t "final";

  (* Ready for the next request *)
  reader_ready t

let test_failed_request_parse () =
  let error_handler_fired = ref false in
  let error_handler ?request error start_response =
    error_handler_fired := true;
    Alcotest.(check (option request)) "No parsed request" None request;
    Alcotest.(check request_error)
      "Request error"
      (`Bad_request "unexpected ' ' in field line")
      error;
    start_response Headers.empty |> Body.Writer.close
  in
  let request_handler _reqd = assert false in
  let t = create ~error_handler request_handler in
  reader_ready t;
  writer_yielded t;
  let writer_woken_up = on_writer_unyield t ignore in
  let len = feed_string t "GET /v1/b HTTP/1.1\r\nHost : example.com\r\n\r\n" in
  Alcotest.(check int) "a failed head consumes the bytes it was given" 42 len;
  reader_closed t;
  Alcotest.(check bool) "Error handler fired" true !error_handler_fired;
  Alcotest.(check bool) "Writer woken up" true !writer_woken_up;
  write_response
    t
    (Response.create ~headers:Headers.connection_close `Bad_request)

let test_bad_request () =
  (* A `Bad_request is returned in a number of cases surrounding
     transfer-encoding or content-length headers. *)
  let request =
    Request.create
      `GET
      "/"
      ~headers:
        (Headers.of_list [ "host", "example.com"; "content-length", "-1" ])
  in
  let error_handler_fired = ref false in
  let error_handler ?request:request' error start_response =
    error_handler_fired := true;
    Alcotest.(check (option request)) "Parsed request" (Some request) request';
    Alcotest.(check request_error)
      "Request error"
      (`Bad_request "invalid Content-Length")
      error;
    start_response Headers.empty |> Body.Writer.close
  in
  let request_handler _reqd = assert false in
  let t = create ~error_handler request_handler in
  reader_ready t;
  writer_yielded t;
  let writer_woken_up = on_writer_unyield t ignore in
  read_request t request;
  reader_closed t;
  Alcotest.(check bool) "Error handler fired" true !error_handler_fired;
  Alcotest.(check bool) "Writer woken up" true !writer_woken_up;
  write_response
    t
    (Response.create ~headers:Headers.connection_close `Bad_request)

let test_shutdown_hangs_request_body_read () =
  let got_eof = ref false in
  let got_error = ref false in
  let request_handler reqd =
    let request_body = Request_descriptor.request_body reqd in
    let rec on_read _buffer ~off:_ ~len:_ =
      Body.Reader.schedule_read request_body ~on_error ~on_eof ~on_read
    and on_eof () = got_eof := true
    and on_error () = got_error := true in
    Body.Reader.schedule_read request_body ~on_error ~on_eof ~on_read
  in
  let t = create ~error_handler request_handler in
  reader_ready t;
  writer_yielded t;
  let request =
    Request.create
      `GET
      ~headers:(Headers.of_list [ "content-length", "10" ])
      "/"
  in
  read_request t request;
  read_string t "five.";
  reader_ready
    ~msg:"Reader wants to read if there's a read scheduled in the body"
    t;
  shutdown t;
  Alcotest.(check bool)
    "EOF not delivered to an incomplete request body"
    false
    !got_eof;
  Alcotest.(check bool)
    "error delivered to the request body if the connection shuts down"
    true
    !got_error;
  do_read t ignore;
  connection_is_shutdown t

let cut_with_error_code reqd () =
  if Request_descriptor.error_code reqd = None
  then Alcotest.fail "request body cut without an error code"

let test_finish_response_after_read_eof_well_formed () =
  let response =
    Response.create ~headers:(Headers.of_list [ "content-length", "5" ]) `OK
  in
  let request_handler reqd =
    let request_body = Request_descriptor.request_body reqd in
    let rec on_read _buffer ~off:_ ~len:_ =
      Body.Reader.schedule_read
        ~on_error:(Helpers.body_must_complete __LOC__)
        request_body
        ~on_eof
        ~on_read
    and on_eof =
     fun () -> Request_descriptor.respond_with_string reqd response "hello"
    in
    Body.Reader.schedule_read
      ~on_error:(Helpers.body_must_complete __LOC__)
      request_body
      ~on_eof
      ~on_read
  in
  let error_handler ?request:_ _error _start_response =
    Alcotest.fail
      "Expected error_handler not to be called because the response was \
       already sent"
  in
  let t = create ~error_handler request_handler in
  reader_ready t;
  writer_yielded t;
  read_request
    t
    (Request.create
       `GET
       "/"
       ~headers:(Headers.of_list [ "content-length", "5" ]));
  let writer_woken_up = on_writer_unyield t ignore in

  let str = "hello" in
  let bs = Ache.Bigstring.of_string ~off:0 ~len:(String.length str) str in
  let just_read = read_eof t bs ~off:0 ~len:(String.length str) in
  Alcotest.(check int) "EOF read" (String.length str) just_read;
  reader_closed t;

  Alcotest.(check bool) "Writer woken up" true !writer_woken_up;
  write_response t ~body:"hello" response;
  connection_is_shutdown t

let test_finish_response_after_read_eof_malformed () =
  let error_handler_called = ref false in
  let response =
    Response.create ~headers:(Headers.of_list [ "content-length", "5" ]) `OK
  in
  let continue_response = ref (fun () -> ()) in
  let request_handler reqd =
    let request_body = Request_descriptor.request_body reqd in
    let rec on_read _buffer ~off:_ ~len:_ =
      Body.Reader.schedule_read
        ~on_error:(cut_with_error_code reqd)
        request_body
        ~on_eof
        ~on_read
    and on_eof =
     fun () ->
      continue_response :=
        fun () ->
          try Request_descriptor.respond_with_string reqd response "hello" with
          | exn -> Request_descriptor.report_exn reqd exn
    in
    Body.Reader.schedule_read
      ~on_error:(Helpers.body_must_complete __LOC__)
      request_body
      ~on_eof
      ~on_read
  in
  let error_handler ?request:_ _error start_response =
    error_handler_called := true;
    let body = start_response Headers.empty in
    Body.Writer.close body
  in
  let t = create ~error_handler request_handler in
  reader_ready t;
  writer_yielded t;
  read_request
    t
    (Request.create
       `GET
       "/"
       ~headers:(Headers.of_list [ "content-length", "10" ]));
  let writer_woken_up = on_writer_unyield t ignore in

  let str = "hello" in
  let bs = Ache.Bigstring.of_string ~off:0 ~len:(String.length str) str in
  let just_read = read_eof t bs ~off:0 ~len:(String.length str) in
  Alcotest.(check int) "EOF read" (String.length str) just_read;
  reader_closed t;
  Alcotest.(check bool) "Error handler was called" true !error_handler_called;

  !continue_response ();

  Alcotest.(check bool) "Writer woken up" true !writer_woken_up;
  write_response
    t
    ~body:""
    (Response.create ~headers:Headers.connection_close `Bad_request);
  writer_closed t;
  reader_closed t

let test_request_body_eof_response_not_sent () =
  let error_handler_called = ref false in
  let response =
    Response.create ~headers:(Headers.of_list [ "content-length", "5" ]) `OK
  in
  let request_handler reqd =
    let request_body = Request_descriptor.request_body reqd in
    let rec on_read _buffer ~off:_ ~len:_ =
      Body.Reader.schedule_read
        ~on_error:(cut_with_error_code reqd)
        request_body
        ~on_eof
        ~on_read
    and on_eof =
     fun () ->
      try Request_descriptor.respond_with_string reqd response "hello" with
      | exn -> Request_descriptor.report_exn reqd exn
    in
    Body.Reader.schedule_read
      ~on_error:(Helpers.body_must_complete __LOC__)
      request_body
      ~on_eof
      ~on_read
  in
  let error_handler ?request:_ _error start_response =
    error_handler_called := true;
    let body = start_response Headers.empty in
    Body.Writer.close body
  in
  let t = create ~error_handler request_handler in
  reader_ready t;
  writer_yielded t;
  read_request
    t
    (Request.create
       `GET
       "/"
       ~headers:(Headers.of_list [ "content-length", "10" ]));

  let str = "hello" in
  let bs = Ache.Bigstring.of_string ~off:0 ~len:(String.length str) str in
  let just_read = read_eof t bs ~off:0 ~len:(String.length str) in
  Alcotest.(check int) "EOF read" (String.length str) just_read;
  reader_closed t;
  Alcotest.(check bool) "Error handler was called" true !error_handler_called;

  write_response
    t
    ~body:""
    (Response.create ~headers:Headers.connection_close `Bad_request);
  writer_closed t;
  reader_closed t

let test_request_body_eof_response_not_sent_empty_eof () =
  let error_handler_called = ref false in
  let response =
    Response.create ~headers:(Headers.of_list [ "content-length", "5" ]) `OK
  in
  let request_handler reqd =
    let request_body = Request_descriptor.request_body reqd in
    let rec on_read _buffer ~off:_ ~len:_ =
      Body.Reader.schedule_read
        ~on_error:(cut_with_error_code reqd)
        request_body
        ~on_eof
        ~on_read
    and on_eof =
     fun () ->
      try Request_descriptor.respond_with_string reqd response "hello" with
      | exn -> Request_descriptor.report_exn reqd exn
    in
    Body.Reader.schedule_read
      ~on_error:(Helpers.body_must_complete __LOC__)
      request_body
      ~on_eof
      ~on_read
  in
  let error_handler ?request:_ _error start_response =
    error_handler_called := true;
    let body = start_response Headers.empty in
    Body.Writer.close body
  in
  let t = create ~error_handler request_handler in
  reader_ready t;
  writer_yielded t;
  read_request
    t
    (Request.create
       `GET
       "/"
       ~headers:(Headers.of_list [ "content-length", "10" ]));

  read_string t "hello";
  let just_read = read_eof t Ache.Bigstring.empty ~off:0 ~len:0 in
  Alcotest.(check int) "EOF read" 0 just_read;
  reader_closed t;
  Alcotest.(check bool) "Error handler was called" true !error_handler_called;

  write_response
    t
    ~body:""
    (Response.create ~headers:Headers.connection_close `Bad_request);
  writer_closed t;
  reader_closed t

let test_race_condition_writer_issues_yield_after_reader_eof () =
  let continue_response = ref (fun () -> ()) in
  let response =
    Response.create ~headers:(Headers.of_list [ "content-length", "10" ]) `OK
  in
  let response_handler reqd =
    let request_body = Request_descriptor.request_body reqd in
    Body.Reader.schedule_read
      ~on_error:(Helpers.body_must_complete __LOC__)
      request_body
      ~on_eof:ignore
      ~on_read:(fun _ ~off:_ ~len:_ ->
        Body.Reader.schedule_read
          ~on_error:(Helpers.body_must_complete __LOC__)
          request_body
          ~on_read:(fun _ ~off:_ ~len:_ -> ())
          ~on_eof:(fun () ->
            let resp_body =
              Request_descriptor.respond_with_streaming reqd response
            in
            Body.Writer.write_string resp_body (String.make 10 'a');
            Body.Writer.flush resp_body (function
              | `Closed -> assert false
              | `Written ->
                continue_response := fun () -> Body.Writer.close resp_body)))
  in
  let t = create ~error_handler response_handler in
  let request =
    Request.create ~headers:(Headers.of_list [ "content-length", "5" ]) `GET "/"
  in
  read_request t request;
  reader_ready t;
  read_string t "hello";
  write_response t ~body:(String.make 10 'a') response;
  reader_yielded t;
  let reader_woken_up =
    on_reader_unyield t (fun () ->
      ignore @@ read_eof t Ache.Bigstring.empty ~off:0 ~len:0;
      reader_closed t)
  in
  writer_yielded t;
  let writer_woken_up = on_writer_unyield t ignore in
  !continue_response ();
  Alcotest.(check bool) "Writer woken up" true !writer_woken_up;
  writer_closed t;
  Alcotest.(check bool) "Reader woken up" true !reader_woken_up;
  (* Also wakes up the reader *)
  connection_is_shutdown t

let test_multiple_requests_in_single_read () =
  let response = Response.create ~headers:(Headers.encoding_fixed 0) `OK in
  let t =
    create (fun reqd -> Request_descriptor.respond_with_string reqd response "")
  in
  let request = request_to_string (Request.create `GET "/") in
  read_string t (request ^ request ^ request);
  let response = response_to_string response in
  write_string
    ~msg:"one write holds every response"
    t
    (response ^ response ^ response);
  writer_yielded t;
  reader_ready t

let test_multiple_async_requests_in_single_read () =
  let response = Response.create ~headers:(Headers.encoding_fixed 0) `OK in
  let reqs_handled = ref 0 in
  let finish_handler = ref (fun () -> assert false) in
  let t =
    create (fun reqd ->
      reqs_handled := !reqs_handled + 1;
      finish_handler :=
        fun () -> Request_descriptor.respond_with_string reqd response "")
  in
  let reqs =
    request_to_string (Request.create `GET "/")
    ^ request_to_string (Request.create `GET "/")
  in
  read_string t reqs;
  reader_yielded t;
  writer_yielded t;
  Alcotest.(check int) "fired handler once" 1 !reqs_handled;
  !finish_handler ();
  write_response t response;
  Alcotest.(check int) "fired handler again" 2 !reqs_handled;
  !finish_handler ();
  write_response t response;
  reader_ready t

let test_errored_chunked_streaming_response () =
  let request = Request.create `GET "/" in
  let response =
    Response.create
      `OK
      ~headers:(Headers.of_list [ "Transfer-encoding", "chunked" ])
  in

  let t = create (streaming_handler ~error:true response []) in
  read_request t request;
  write_response t response;
  connection_is_shutdown t

let test_multiple_requests_in_single_read_with_close () =
  let response =
    Response.create `OK ~headers:Headers.(connection_close @ encoding_fixed 0)
  in
  let t =
    create (fun reqd -> Request_descriptor.respond_with_string reqd response "")
  in
  let request = request_to_string (Request.create `GET "/") in
  let consumed = feed_string t (request ^ request) in
  Alcotest.(check int)
    "only the first request is read"
    (String.length request)
    consumed;
  write_response t response;
  connection_is_shutdown t

let test_errored_content_length_streaming_response () =
  let request = Request.create `GET "/" in
  let response =
    Response.create `OK ~headers:(Headers.of_list [ "Content-Length", "10" ])
  in

  let t = create (streaming_handler ~error:true response [ "hello" ]) in
  read_request t request;
  write_response t response ~body:"hello";
  connection_is_shutdown t

let test_errored_chunked_streaming_response_async () =
  let continue = ref (fun () -> ()) in
  let request = Request.create `GET "/" in
  let response =
    Response.create
      `OK
      ~headers:(Headers.of_list [ "Transfer-encoding", "chunked" ])
  in

  let request_handler reqd =
    let request_body = Request_descriptor.request_body reqd in
    Body.Reader.close request_body;
    let body = Request_descriptor.respond_with_streaming reqd response in
    Body.Writer.write_string body "hello";
    Body.Writer.flush body (function
      | `Closed -> assert false
      | `Written ->
        continue := fun () -> Request_descriptor.report_exn reqd (Failure "heh"))
  in

  let t = create request_handler in
  read_request t request;
  write_response t response ~body:"5\r\nhello\r\n";
  let reader_woken_up = on_reader_unyield t ignore in
  let writer_woken_up = on_writer_unyield t ignore in
  !continue ();
  Alcotest.(check bool) "Reader woken up" true !reader_woken_up;
  Alcotest.(check bool) "Writer woken up" true !writer_woken_up;
  connection_is_shutdown t

let test_multiple_requests_in_single_read_with_eof () =
  let response = Response.create ~headers:(Headers.encoding_fixed 0) `OK in
  let t =
    create (fun reqd -> Request_descriptor.respond_with_string reqd response "")
  in
  let reqs =
    request_to_string (Request.create `GET "/")
    ^ request_to_string (Request.create `GET "/")
  in
  read_string t reqs ~eof:true;
  let response = response_to_string response in
  write_string ~msg:"one write holds both responses" t (response ^ response)

let test_parse_failure_after_checkpoint () =
  let error_queue = ref None in
  let error_handler ?request:_ error _start_response =
    Alcotest.(check (option reject)) "Error queue is empty" !error_queue None;
    error_queue := Some error
  in
  let request_handler _reqd = assert false in
  let t = create ~error_handler request_handler in
  reader_ready t;
  read_string t "GET index.html HTTP/1.1\r\n";
  let result = feed_string t " index.html HTTP/1.1\r\n\r\n" in
  Alcotest.(check int) "Bad header consumed" 24 result;
  reader_closed t;
  match !error_queue with
  | None -> Alcotest.fail "Expected error"
  | Some error ->
    Alcotest.(check request_error)
      "Error"
      (`Bad_request "unexpected ' ' in field line")
      error

let test_parse_failure_at_eof () =
  let error_queue = ref None in
  let continue = ref (fun () -> ()) in
  let error_handler ?request error start_response =
    Alcotest.(check (option reject)) "Error queue is empty" !error_queue None;
    Alcotest.(check (option reject)) "Request was not parsed" request None;
    error_queue := Some error;
    continue :=
      fun () ->
        let resp_body = start_response Headers.empty in
        Body.Writer.write_string resp_body "got an error";
        Body.Writer.close resp_body
  in
  let request_handler _reqd = assert false in
  let t = create ~error_handler request_handler in
  reader_ready t;
  read_string t "GET index.html HTTP/1.1\r\n";
  let result = feed_string ~eof:true t " index.html HTTP/1.1\r\n\r\n" in
  Alcotest.(check int) "Bad header consumed" 24 result;
  reader_closed t;
  (match !error_queue with
  | None -> Alcotest.fail "Expected error"
  | Some error ->
    Alcotest.(check request_error)
      "Error"
      (`Bad_request "unexpected ' ' in field line")
      error);
  !continue ();
  write_response
    t
    (Response.create ~headers:Headers.connection_close `Bad_request);
  write_string t "got an error";
  writer_closed t

let test_response_finished_before_body_read () =
  let response = Response.create `OK ~headers:(Headers.encoding_fixed 4) in
  let rev_body_chunks = ref [] in
  let request_handler reqd =
    let rec read_body () =
      Body.Reader.schedule_read
        ~on_error:(Helpers.body_must_complete __LOC__)
        (Request_descriptor.request_body reqd)
        ~on_read:(fun buf ~off ~len ->
          rev_body_chunks :=
            Ache.Bigstring.substring buf ~off ~len :: !rev_body_chunks;
          read_body ())
        ~on_eof:ignore
    in
    read_body ();
    Request_descriptor.respond_with_string reqd response "done"
  in
  let t = create request_handler in
  read_request t (Request.create `GET "/" ~headers:(Headers.encoding_fixed 12));
  write_response t response ~body:"done";
  (* Finish the request and send another *)
  read_string t "hello, ";
  read_string t "world";
  Alcotest.(check (list string))
    "received body"
    [ "world"; "hello, " ]
    !rev_body_chunks;
  read_request t (Request.create `GET "/");
  write_response t response ~body:"done"

let test_pipelined_requests_answer_before_reading_body () =
  let response = Response.create `OK ~headers:(Headers.encoding_fixed 0) in
  let request_handler reqd =
    let response =
      Response.create ~headers:(Headers.of_list [ "content-length", "0" ]) `OK
    in
    Request_descriptor.respond_with_string reqd response ""
  in
  let t = create request_handler in
  read_request t (Request.create `GET "/" ~headers:(Headers.encoding_fixed 5));
  write_response t response;
  (* Finish the request and send another *)
  read_string
    t
    "helloGET / HTTP/1.1\r\nhost: localhost\r\ncontent-length: 5\r\n\r\n";
  write_response t response

let test_body_flush_fairness () =
  let rev_body_chunks = ref [] in
  let continue = ref (fun () -> ()) in
  let t =
    create (fun reqd ->
      let request_body = Request_descriptor.request_body reqd in
      let rec on_read buffer ~off ~len =
        rev_body_chunks :=
          Ache.Bigstring.substring buffer ~off ~len :: !rev_body_chunks;
        continue :=
          fun () ->
            Body.Reader.schedule_read
              ~on_error:(Helpers.body_must_complete __LOC__)
              request_body
              ~on_eof
              ~on_read
      and on_eof () = print_endline "got eof" in
      continue :=
        fun () ->
          Body.Reader.schedule_read
            ~on_error:(Helpers.body_must_complete __LOC__)
            request_body
            ~on_eof
            ~on_read)
  in
  read_request t (Request.create `GET "/" ~headers:(Headers.encoding_fixed 20));
  reader_yielded t;
  writer_yielded t;
  let reader_woken_up = on_reader_unyield t ignore in
  !continue ();
  reader_ready t;
  Alcotest.(check bool) "reader woken up" true !reader_woken_up;
  read_string t "hello";
  !continue ();
  read_string t "hello";
  reader_yielded t;
  let reader_woken_up = on_reader_unyield t ignore in
  force_read_string t "hello";
  !continue ();
  Alcotest.(check bool) "reader woken up" true !reader_woken_up;
  Alcotest.(check int)
    "Request handler processed 3 chunks"
    3
    (List.length !rev_body_chunks)

let test_shutdown_in_request_handler () =
  let request = Request.create `GET "/" in
  let rec t = lazy (create (fun _ -> shutdown (Lazy.force t))) in
  let t = Lazy.force t in
  read_request t request;
  reader_closed t;
  writer_closed t

let test_shutdown_during_asynchronous_request () =
  let request = Request.create `GET "/" in
  let response = Response.create `OK in
  let continue = ref (fun () -> ()) in
  let t =
    create (fun reqd ->
      continue :=
        fun () -> Request_descriptor.respond_with_string reqd response "")
  in
  read_request t request;
  shutdown t;
  !continue ();
  reader_closed t;
  writer_closed t

let test_flush_response_before_shutdown () =
  let request = Request.create `GET "/" ~headers:(Headers.encoding_fixed 0) in
  let response = Response.create `OK ~headers:Headers.encoding_chunked in
  let continue = ref (fun () -> ()) in
  let request_handler reqd =
    let body =
      Request_descriptor.respond_with_streaming
        ~flush_headers_immediately:true
        reqd
        response
    in
    continue :=
      fun () ->
        Body.Writer.write_string body "hello world";
        Body.Writer.close body
  in
  let t = create request_handler in
  read_request t request;
  write_response t response;
  !continue ();
  shutdown t;

  write_string t "b\r\nhello world\r\n";
  connection_is_shutdown t

let test_schedule_read_with_data_available () =
  let response = Response.create ~headers:(Headers.encoding_fixed 0) `OK in
  let body = ref None in
  let continue = ref (fun () -> ()) in
  let request_handler reqd =
    body := Some (Request_descriptor.request_body reqd);
    continue :=
      fun () -> Request_descriptor.respond_with_string reqd response ""
  in
  let t = create request_handler in
  read_request t (Request.create `GET "/" ~headers:(Headers.encoding_fixed 6));

  let body = Option.get !body in
  let schedule_read expected =
    let did_read = ref false in
    Body.Reader.schedule_read
      ~on_error:(Helpers.body_must_complete __LOC__)
      body
      ~on_read:(fun buf ~off ~len ->
        let actual = Ache.Bigstring.substring buf ~off ~len in
        did_read := true;
        Alcotest.(check string) "Body" expected actual)
      ~on_eof:(fun () -> assert false);
    Alcotest.(check bool) "on_read called" true !did_read
  in

  (* We get some data on the connection, but not the full response yet. *)
  force_read_string t "Hello";
  (* Schedule a read when there is already data available. on_read should be
     called synchronously *)
  schedule_read "Hello";
  force_read_string t "!";
  schedule_read "!";
  (* Also works with eof *)
  Body.Reader.schedule_read
    ~on_error:(Helpers.body_must_complete __LOC__)
    body
    ~on_read:(fun _ ~off:_ ~len:_ -> Alcotest.fail "Expected eof")
    ~on_eof:(fun () -> !continue ());
  write_response t response

let test_eof_called_multiple_times () =
  let continue_reading = ref (fun () -> ()) in
  let eof_counter = ref 0 in
  let request_handler reqd =
    let request_body = Request_descriptor.request_body reqd in
    let rec on_read _buffer ~off:_ ~len:_ =
      continue_reading :=
        fun () ->
          Body.Reader.schedule_read
            ~on_error:(Helpers.body_must_complete __LOC__)
            request_body
            ~on_eof
            ~on_read
    and on_eof () =
      incr eof_counter;
      print_endline
        ("got eof" ^ string_of_bool (Body.Reader.is_closed request_body))
    in
    Body.Reader.schedule_read
      ~on_error:(Helpers.body_must_complete __LOC__)
      request_body
      ~on_eof
      ~on_read
  in
  let t = create ~error_handler request_handler in
  reader_ready t;
  writer_yielded t;
  let request =
    Request.create
      `GET
      ~headers:(Headers.of_list [ "content-length", "10" ])
      "/"
  in
  read_request t request;
  read_string t "five.";
  reader_yielded t;
  let reader_woken_up = on_reader_unyield t ignore in
  !continue_reading ();
  Alcotest.(check bool)
    "Reader wakes up if scheduling read"
    true
    !reader_woken_up;
  reader_ready
    ~msg:"Reader wants to read if there's a read scheduled in the body"
    t;
  read_string t "more.";
  reader_yielded t;
  let reader_woken_up = on_reader_unyield t !continue_reading in
  !continue_reading ();
  Alcotest.(check bool)
    "Reader wakes up if scheduling read"
    true
    !reader_woken_up;
  Alcotest.(check int) "`on_eof` only called once" 1 !eof_counter;
  writer_yielded t

let test_eof_called_for_empty_bodies () =
  let eof_counter = ref 0 in
  let request_handler reqd =
    let request_body = Request_descriptor.request_body reqd in
    let on_read _buffer ~off:_ ~len:_ = assert false
    and on_eof () = incr eof_counter in
    Body.Reader.schedule_read
      ~on_error:(Helpers.body_must_complete __LOC__)
      request_body
      ~on_eof
      ~on_read
  in
  let t = create ~error_handler request_handler in
  reader_ready t;
  writer_yielded t;
  let request =
    Request.create `GET ~headers:(Headers.of_list [ "content-length", "0" ]) "/"
  in
  read_request t request;
  reader_yielded t;
  Alcotest.(check int) "`on_eof` only called once" 1 !eof_counter;
  writer_yielded t;
  shutdown t;

  let t = create ~error_handler request_handler in
  reader_ready t;
  writer_yielded t;
  read_request t request;
  reader_yielded t;
  Alcotest.(check int) "`on_eof` only called once" 2 !eof_counter;
  writer_yielded t

let test_error_handler_chunked_response () =
  let continue_error = ref (fun () -> ()) in
  let error_handler ?request error start_response =
    continue_error :=
      fun () ->
        streaming_error_handler
          ~headers:(Headers.of_list [ "transfer-encoding", "chunked" ])
          continue_error
          ?request
          error
          start_response
  in
  let t = create ~error_handler (basic_handler "") in
  writer_yielded t;
  let writer_woken_up = on_writer_unyield t ignore in
  let c = read_string_eof t eof_request_string in
  Alcotest.(check int)
    "read consumes all input"
    (String.length eof_request_string)
    c;
  reader_errored t;
  Alcotest.(check bool) "Writer hasn't woken up yet" false !writer_woken_up;
  !continue_error ();
  Alcotest.(check bool) "Writer woken up" true !writer_woken_up;
  write_response
    t
    (Response.create
       ~headers:
         (Headers.of_list
            [ "transfer-encoding", "chunked"; "connection", "close" ])
       `Bad_request);
  !continue_error ();
  write_string t "d\r\ngot an error\n\r\n";
  report_exn t (Failure "broken pipe");
  writer_closed t;
  connection_is_closed t

let test_pipelined_requests_in_single_buffer_partial_body () =
  let response =
    Response.create ~headers:(Headers.of_list [ "content-length", "5" ]) `OK
  in
  let request_handler reqd =
    let request_body = Request_descriptor.request_body reqd in
    let response_body =
      Request_descriptor.respond_with_streaming reqd response
    in
    let rec on_read buffer ~off ~len =
      Body.Writer.write_bigstring response_body buffer ~off ~len;
      Body.Reader.schedule_read
        ~on_error:(Helpers.body_must_complete __LOC__)
        request_body
        ~on_eof
        ~on_read
    and on_eof () = Body.Writer.close response_body in
    Body.Reader.schedule_read
      ~on_error:(Helpers.body_must_complete __LOC__)
      (Request_descriptor.request_body reqd)
      ~on_eof
      ~on_read
  in
  let t = create request_handler in
  let req = Request.create `POST "/" ~headers:(Headers.encoding_fixed 5) in
  let reqs = request_to_string req ^ "hello" ^ request_to_string req in
  read_string t reqs;
  reader_yielded t;
  write_response t ~body:"hello" response;

  write_response t response;
  writer_yielded t;

  reader_ready t;
  read_string t "hello";
  write_string t "hello";
  reader_ready t

let test_multiple_pipelined_requests () =
  let response =
    Response.create ~headers:(Headers.of_list [ "content-length", "5" ]) `OK
  in
  let request_handler reqd =
    let request_body = Request_descriptor.request_body reqd in
    let response_body =
      Request_descriptor.respond_with_streaming reqd response
    in
    let rec on_read buffer ~off ~len =
      Body.Writer.write_bigstring response_body buffer ~off ~len;
      Body.Reader.schedule_read
        ~on_error:(Helpers.body_must_complete __LOC__)
        request_body
        ~on_eof
        ~on_read
    and on_eof () = Body.Writer.close response_body in
    Body.Reader.schedule_read
      ~on_error:(Helpers.body_must_complete __LOC__)
      (Request_descriptor.request_body reqd)
      ~on_eof
      ~on_read
  in
  let t = create request_handler in
  let req = Request.create `POST "/" ~headers:(Headers.encoding_fixed 5) in
  read_string t (request_to_string req ^ "hello");
  reader_yielded t;

  write_response t ~body:"hello" response;
  read_request t req;

  writer_yielded t;
  let writer_woken_up =
    on_writer_unyield t (fun () -> write_response t ~body:"hello" response)
  in
  reader_ready t;

  let reqs = "hello" ^ request_to_string req ^ "hello" in
  read_string t reqs;
  Alcotest.(check bool) "Writer woken up" true !writer_woken_up;
  reader_yielded t;
  write_response t ~body:"hello" response

let test_body_flush_after_bytes_in_the_wire () =
  let response =
    Response.create ~headers:(Headers.of_list [ "content-length", "5" ]) `OK
  in
  let callback_called = ref false in
  let callback = function
    | `Closed -> assert false
    | `Written -> callback_called := true
  in
  let request_handler ~flush_headers_immediately reqd =
    let response_body =
      Request_descriptor.respond_with_streaming
        ~flush_headers_immediately
        reqd
        response
    in
    Body.Writer.flush response_body callback
  in
  let t = create (request_handler ~flush_headers_immediately:true) in
  let req = Request.create `GET "/" in
  read_request t req;

  Alcotest.(check bool)
    "flush callback isn't called until the response has been written"
    false
    !callback_called;
  write_response t response;
  Alcotest.(check bool) "flush callback" true !callback_called;
  shutdown t;

  callback_called := false;
  let t = create (request_handler ~flush_headers_immediately:false) in
  let req = Request.create `GET "/" in
  read_request t req;

  Alcotest.(check bool)
    "flush callback isn't called until the response has been written"
    false
    !callback_called;
  write_response t response;
  Alcotest.(check bool) "flush callback" true !callback_called;
  shutdown t;
  connection_is_shutdown t

let test_input_consumed_before_closing_req_body () =
  let request = Request.create `GET "/" ~headers:Headers.encoding_chunked in
  let response = Response.create `OK ~headers:Headers.encoding_chunked in

  let t = create (echo_handler response) in
  read_request t request;
  read_string t "e\r\nThis is a test";
  write_response t ~body:"e\r\nThis is a test\r\n" response;
  read_string t "\r\n0\r\n";
  writer_yielded t;
  read_string t "\r\n";
  write_string t "0\r\n\r\n"

let test_no_request_dispatched_after_write_eof () =
  let request = Request.create `GET "/" ~headers:(Headers.encoding_fixed 0) in
  let reqd = ref None in
  let request_handler reqd' = reqd := Some reqd' in
  let t = create request_handler in
  let response = Response.create `OK ~headers:Headers.encoding_chunked in
  read_request t request;
  Request_descriptor.respond_with_streaming
    (Option.get !reqd)
    response
    ~flush_headers_immediately:true
  |> (ignore : Body.Writer.t -> unit);
  write_eof t;
  reqd := None;
  writer_closed ~unread:47 t;
  reader_closed t;
  let consumed = force_feed_string t (request_to_string request) in
  Alcotest.(check int) "later request not read" 0 consumed;
  Alcotest.(check bool) "request handler not fired" true (Option.is_none !reqd)

let test_no_request_dispatched_after_write_eof_before_send_response () =
  let request = Request.create `GET "/" ~headers:(Headers.encoding_fixed 0) in
  let first = ref None in
  let reqd = ref None in
  let request_handler reqd' = reqd := Some reqd' in
  let t = create request_handler in
  let response = Response.create `OK ~headers:Headers.encoding_chunked in
  read_request t request;

  let write_op = ref (current_write_operation t) in
  with_server_connection t (fun t ->
    Server_connection.report_write_result t `Closed;
    write_op := Server_connection.next_write_operation t);
  Alcotest.check write_operation "Writer is closed" (`Close 0) !write_op;
  first := !reqd;
  reqd := None;
  reader_closed t;
  let consumed = force_feed_string t (request_to_string request) in
  Alcotest.(check int) "later request not read" 0 consumed;
  Alcotest.(check bool) "request handler not fired" true (Option.is_none !reqd);
  let body =
    Request_descriptor.respond_with_streaming
      (Option.get !first)
      response
      ~flush_headers_immediately:true
  in
  Alcotest.(check bool)
    "body writer on a closed connection is closed"
    true
    (Body.Writer.is_closed body)

let test_write_response_after_read_eof () =
  let reqd = ref None in
  let t = create (fun _reqd -> reqd := Some _reqd) in
  let request_line = "GET / HTTP/1.1\r\n" in
  let single_header = "Links: /path/to/some/website\r\n" in
  let r =
    (* Each header is 30 bytes *)
    request_line
    ^ single_header
    ^ single_header
    ^ "host: a\r\nconnection: close\r\n\r\n"
  in
  let bs = Ache.Bigstring.of_string r ~off:0 ~len:(String.length r) in
  let c = read t bs ~off:0 ~len:30 in
  Alcotest.(check int) "an incomplete head consumes all input" 30 c;

  let c' = read t bs ~off:c ~len:(String.length single_header) in
  Alcotest.(check int)
    "parser can read a single header and commit"
    (String.length single_header)
    c';

  let c'' = read_eof t bs ~off:(c + c') ~len:(String.length r - (c + c')) in
  Alcotest.(check int)
    "read_eof with the rest of the input is accepted"
    (String.length r - (c + c'))
    c'';
  writer_yielded t;
  let body =
    Request_descriptor.respond_with_streaming
      ~flush_headers_immediately:true
      (Option.get !reqd)
      (Response.create `OK)
  in
  write_response t (Response.create ~headers:Headers.connection_close `OK);
  Body.Writer.close body;
  connection_is_shutdown t

let test_connect_method () =
  let upgraded = { Upgraded.driven = false } in
  let t = create ~error_handler (upgrade_handler upgraded) in
  read_request
    t
    (Request.create
       ~headers:(Headers.of_list [ "host", "example.com:80" ])
       `CONNECT
       "example.com:80");
  write_response ~msg:"200 written, no 101" t (Response.create `OK);
  Alcotest.(check bool) "Upgraded protocol was called" true upgraded.driven;
  reader_yielded t

let test_invalid_response_head_writes_nothing () =
  let output conn =
    let b = Buffer.create 64 in
    let rec go () =
      match Server_connection.next_write_operation conn with
      | `Write iovecs ->
        let s = Write_operation.iovecs_to_string iovecs in
        Buffer.add_string b s;
        Server_connection.report_write_result conn (`Ok (String.length s));
        go ()
      | `Yield | `Close _ -> ()
    in
    go ();
    Buffer.contents b
  in
  let bad =
    Response.create ~headers:(Headers.of_list [ "X-A", "a\r\nB: 1" ]) `OK
  in
  let request = "GET / HTTP/1.1\r\nHost: a\r\n\r\n" in
  let bs =
    Ache.Bigstring.of_string request ~off:0 ~len:(String.length request)
  in
  let raised = ref [] in
  let conn =
    Server_connection.create (fun reqd ->
      List.iter
        (fun respond ->
           match respond reqd with
           | () -> ()
           | exception Invalid_argument msg -> raised := msg :: !raised)
        [ (fun reqd -> Request_descriptor.respond_with_string reqd bad "body")
        ; (fun reqd ->
            Request_descriptor.respond_with_bigstring
              reqd
              bad
              (Ache.Bigstring.of_string "x" ~off:0 ~len:1))
        ; (fun reqd ->
            ignore
              (Request_descriptor.respond_with_streaming reqd bad
               : Body.Writer.t))
        ];
      Alcotest.(check bool)
        "no response started"
        true
        (Request_descriptor.response reqd = None);
      Request_descriptor.respond_with_string reqd (Response.create `OK) "")
  in
  ignore
    (Server_connection.read conn bs ~off:0 ~len:(String.length request) : int);
  Alcotest.(check (list string))
    "each respond_with_* raises"
    (List.init 3 (fun _ -> {|ache: invalid value for field "X-A"|}))
    !raised;
  Alcotest.(check string)
    "only the valid response is written"
    "HTTP/1.1 200 OK\r\ncontent-length: 0\r\n\r\n"
    (output conn);
  let error_output = ref [] in
  let conn =
    Server_connection.create
      ~error_handler:(fun ?request:_ error start_response ->
        error_output :=
          (match error with `Exn (Invalid_argument msg) -> msg | _ -> "other")
          :: !error_output;
        Body.Writer.close (start_response Headers.empty))
      (fun reqd -> Request_descriptor.respond_with_string reqd bad "")
  in
  (match Server_connection.read conn bs ~off:0 ~len:(String.length request) with
  | (_ : int) -> Alcotest.fail "expected the request handler to raise"
  | exception (Invalid_argument _ as exn) ->
    Server_connection.report_exn conn exn);
  Alcotest.(check (list string))
    "error handler sees the exception"
    [ {|ache: invalid value for field "X-A"|} ]
    !error_output;
  Alcotest.(check string)
    "only the error response is written"
    "HTTP/1.1 500 Internal Server Error\r\nconnection: close\r\n\r\n"
    (output conn)

module Raw = struct
  type t =
    { conn : Server_connection.t
    ; mutable pending : string
    ; out : Buffer.t
    }

  let create ?config ?(error_handler = error_handler) handler =
    { conn = Server_connection.create ?config ~error_handler handler
    ; pending = ""
    ; out = Buffer.create 256
    }

  let read_op t = Server_connection.next_read_operation t.conn

  let read t ~eof len =
    let bs = Ache.Bigstring.of_string t.pending ~off:0 ~len in
    let n =
      if eof
      then Server_connection.read_eof t.conn bs ~off:0 ~len
      else Server_connection.read t.conn bs ~off:0 ~len
    in
    t.pending <- String.sub t.pending n (String.length t.pending - n);
    n

  (* Offers the pending input in reads of [chunk] bytes, more when a read
     consumes nothing, while the reader asks for input. *)
  let feed ?(eof = false) ~chunk t s =
    t.pending <- t.pending ^ s;
    let eof = ref eof in
    let rec go size =
      let total = String.length t.pending in
      match read_op t with
      | `Read when !eof && size >= total ->
        eof := false;
        ignore (read t ~eof:true total : int);
        go chunk
      | `Read when total > 0 ->
        if read t ~eof:false (min size total) > 0
        then go chunk
        else if size < total
        then go (size + chunk)
        else ignore (read_op t)
      | `Read | `Yield | `Close -> ()
    in
    go chunk

  let write_op t =
    match Server_connection.next_write_operation t.conn with
    | `Write iovecs ->
      let s = Write_operation.iovecs_to_string iovecs in
      Buffer.add_string t.out s;
      Server_connection.report_write_result t.conn (`Ok (String.length s));
      `Write
    | `Yield -> `Yield
    | `Close _ -> `Close

  let rec write_all t = match write_op t with `Write -> write_all t | op -> op

  let take_output t =
    let s = Buffer.contents t.out in
    Buffer.clear t.out;
    s
end

let splits = [ "one read", max_int; "one byte per read", 1 ]

let test_eof_in_head_either_call_order () =
  List.iter
    (fun (split, chunk) ->
       List.iter
         (fun write_first ->
            List.iter
              (fun input ->
                 let label =
                   Printf.sprintf
                     "%S, %s, %s first"
                     input
                     split
                     (if write_first then "write" else "read")
                 in
                 let t =
                   Raw.create (fun _ -> Alcotest.fail "request handler called")
                 in
                 Raw.feed ~chunk t input;
                 Alcotest.check
                   read_operation
                   (label ^ ": reader waits")
                   `Read
                   (Raw.read_op t);
                 ignore (Raw.read t ~eof:true (String.length t.pending) : int);
                 if write_first then ignore (Raw.write_all t);
                 let read_op = Raw.read_op t in
                 let last = Raw.write_all t in
                 let output = Raw.take_output t in
                 Alcotest.(check bool)
                   (label ^ ": 400 written")
                   true
                   (String.starts_with ~prefix:"HTTP/1.1 400 " output);
                 Alcotest.(check bool)
                   (label ^ ": writer closed")
                   true
                   (last = `Close);
                 Alcotest.check
                   read_operation
                   (label ^ ": reader closed")
                   `Close
                   read_op)
              [ "GET / HT"
              ; "GET / HTTP/1.1\r\nHost: a\r\n"
              ; "GET / HTTP/1.1\r\nHost: a\r\n\r"
              ])
         [ true; false ])
    splits

type body_events =
  { mutable received : string
  ; mutable eof : bool
  ; mutable failed : int
  ; mutable reqd : Request_descriptor.t option
  }

let reading_handler events reqd =
  events.reqd <- Some reqd;
  let body = Request_descriptor.request_body reqd in
  let rec loop () =
    Body.Reader.schedule_read
      body
      ~on_error:(fun () -> events.failed <- events.failed + 1)
      ~on_eof:(fun () -> events.eof <- true)
      ~on_read:(fun bs ~off ~len ->
        events.received <-
          events.received ^ Ache.Bigstring.substring bs ~off ~len;
        loop ())
  in
  loop ()

let test_incomplete_request_body_not_complete () =
  List.iter
    (fun (split, chunk) ->
       List.iter
         (fun (headers, body, eof, received) ->
            let label = Printf.sprintf "%S, %s" body split in
            let events =
              { received = ""; eof = false; failed = 0; reqd = None }
            in
            let t = Raw.create (reading_handler events) in
            Raw.feed
              ~chunk
              t
              ("POST / HTTP/1.1\r\nHost: a\r\n" ^ headers ^ "\r\n" ^ body);
            if eof then Raw.feed ~eof:true ~chunk t "";
            ignore (Raw.write_all t);
            Alcotest.(check bool)
              (label ^ ": on_eof not called")
              false
              events.eof;
            Alcotest.(check int)
              (label ^ ": on_error called once")
              1
              events.failed;
            Alcotest.(check string)
              (label ^ ": bytes received")
              received
              events.received;
            Alcotest.(check bool)
              (label ^ ": error code")
              true
              (match Request_descriptor.error_code (Option.get events.reqd) with
              | Some (`Bad_request _) -> true
              | _ -> false))
         [ "content-length: 5\r\n", "ab", true, "ab"
         ; "transfer-encoding: chunked\r\n", "3\r\nab", true, "ab"
         ; "transfer-encoding: chunked\r\n", "3\r\nabc\r\n", true, "abc"
         ; "transfer-encoding: chunked\r\n", "3\r\nabc\r\n0\r\n", true, "abc"
         ; "transfer-encoding: chunked\r\n", "3\r\nabcX", false, "abc"
         ])
    splits

let test_complete_request_body_at_eof () =
  List.iter
    (fun (split, chunk) ->
       List.iter
         (fun (headers, body) ->
            let label = Printf.sprintf "%S, %s" body split in
            let events =
              { received = ""; eof = false; failed = 0; reqd = None }
            in
            let t = Raw.create (reading_handler events) in
            Raw.feed
              ~chunk
              t
              ("POST / HTTP/1.1\r\nHost: a\r\n" ^ headers ^ "\r\n" ^ body);
            Raw.feed ~eof:true ~chunk t "";
            Alcotest.(check bool) (label ^ ": on_eof called") true events.eof;
            Alcotest.(check int)
              (label ^ ": on_error not called")
              0
              events.failed;
            Alcotest.(check string)
              (label ^ ": bytes received")
              "abc"
              events.received)
         [ "content-length: 3\r\n", "abc"
         ; "transfer-encoding: chunked\r\n", "3\r\nabc\r\n0\r\n\r\n"
         ])
    splits

let test_unread_body_discarded_after_response () =
  let chunked size =
    let rec go n acc =
      if n = 0
      then String.concat "" (List.rev ("0\r\n\r\n" :: acc))
      else
        let k = min n 0x1000 in
        go (n - k) (Printf.sprintf "%x\r\n%s\r\n" k (String.make k 'x') :: acc)
    in
    "transfer-encoding: chunked\r\n", go size []
  in
  let fixed size =
    Printf.sprintf "content-length: %d\r\n" size, String.make size 'x'
  in
  List.iter
    (fun (split, chunk, size) ->
       List.iter
         (fun (framing, (headers, body)) ->
            let label = Printf.sprintf "%s, %d bytes, %s" framing size split in
            let bodies = ref [] in
            let t =
              Raw.create (fun reqd ->
                bodies := Request_descriptor.request_body reqd :: !bodies;
                Request_descriptor.respond_with_string
                  reqd
                  (Response.create `OK)
                  "")
            in
            Raw.feed
              ~chunk
              t
              ("POST / HTTP/1.1\r\nHost: a\r\n"
              ^ headers
              ^ "\r\n"
              ^ body
              ^ "GET / HTTP/1.1\r\nHost: a\r\n\r\n");
            ignore (Raw.write_all t);
            Alcotest.(check int)
              (label ^ ": both requests handled")
              2
              (List.length !bodies);
            Alcotest.(check string)
              (label ^ ": both answered")
              "HTTP/1.1 200 OK\r\n\
               content-length: 0\r\n\
               \r\n\
               HTTP/1.1 200 OK\r\n\
               content-length: 0\r\n\
               \r\n"
              (Raw.take_output t);
            let held = ref 0 in
            Body.Reader.schedule_read
              ~on_error:(Helpers.body_must_complete __LOC__)
              (List.nth !bodies 1)
              ~on_eof:ignore
              ~on_read:(fun _ ~off:_ ~len -> held := !held + len);
            if !held > min chunk size
            then
              Alcotest.failf
                "%s: %d body bytes held, more than one read of %d"
                label
                !held
                (min chunk size))
         [ "content-length", fixed size; "chunked", chunked size ])
    [ "one read", max_int, 0x100000
    ; "4096-byte reads", 0x1000, 0x100000
    ; "one byte per read", 1, 0x2000
    ]

let test_eof_in_discarded_body () =
  List.iter
    (fun (split, chunk) ->
       let t =
         Raw.create (fun reqd ->
           Request_descriptor.respond_with_string reqd (Response.create `OK) "")
       in
       Raw.feed
         ~chunk
         t
         "POST / HTTP/1.1\r\nHost: a\r\ncontent-length: 10\r\n\r\nhello";
       Raw.feed ~eof:true ~chunk t "";
       let write_op = Raw.write_all t in
       Alcotest.(check string)
         (split ^ ": only the response")
         "HTTP/1.1 200 OK\r\ncontent-length: 0\r\n\r\n"
         (Raw.take_output t);
       Alcotest.(check bool) (split ^ ": writer closed") true (write_op = `Close);
       Alcotest.check
         read_operation
         (split ^ ": reader closed")
         `Close
         (Raw.read_op t))
    splits

let test_error_handler_invalid_headers_closes () =
  let bad_error_handler ?request:_ _error start_response =
    Body.Writer.close (start_response (Headers.of_list [ "bad name", "x" ]))
  in
  List.iter
    (fun (split, chunk) ->
       List.iter
         (fun (case, input, handler) ->
            let label = case ^ ", " ^ split in
            let t = Raw.create ~error_handler:bad_error_handler handler in
            (match Raw.feed ~chunk t input with
            | () -> ()
            | exception exn ->
              Alcotest.failf "%s: raised %s" label (Printexc.to_string exn));
            Alcotest.check
              read_operation
              (label ^ ": reader closed")
              `Close
              (Raw.read_op t);
            let write_op = Raw.write_all t in
            Alcotest.(check string)
              (label ^ ": nothing written")
              ""
              (Raw.take_output t);
            Alcotest.(check bool)
              (label ^ ": writer closed")
              true
              (write_op = `Close))
         [ ( "parse error"
           , "GET / HTTP/1.1\r\nHost : a\r\n\r\n"
           , fun _ -> Alcotest.fail "request handler called" )
         ; ( "handler exception"
           , "GET / HTTP/1.1\r\nHost: a\r\n\r\n"
           , fun reqd -> Request_descriptor.report_exn reqd (Failure "handler")
           )
         ])
    splits

module Recorder = struct
  type t =
    { mutable got : string
    ; mutable eof : bool
    }

  let next_read_operation t = if t.eof then `Close else `Read

  let read t bs ~off ~len =
    t.got <- t.got ^ Ache.Bigstring.substring bs ~off ~len;
    len

  let read_eof t bs ~off ~len =
    t.eof <- true;
    read t bs ~off ~len

  let yield_reader _ _ = ()
  let next_write_operation _ = `Yield
  let report_write_result _ _ = ()
  let yield_writer _ _ = ()
  let report_exn _ _ = ()
  let is_closed _ = false
  let shutdown _ = ()
end

let switching_requests =
  [ ( "upgrade"
    , "GET / HTTP/1.1\r\n\
       Host: a\r\n\
       Connection: upgrade\r\n\
       Upgrade: echo\r\n\
       \r\n" )
  ; "CONNECT", "CONNECT a:443 HTTP/1.1\r\nHost: a:443\r\n\r\n"
  ]

let upgrade_response_for request =
  if String.starts_with ~prefix:"CONNECT" request
  then "HTTP/1.1 200 OK\r\n\r\n"
  else "HTTP/1.1 101 Switching Protocols\r\n\r\n"

let trailing_inputs =
  [ "ping"; "a b c\r\n"; "PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n" ]

let each_switching_case f =
  List.iter
    (fun (split, chunk) ->
       List.iter
         (fun (kind, request) ->
            List.iter
              (fun trailing ->
                 f
                   (Printf.sprintf "%s, %S, %s" kind trailing split)
                   ~chunk
                   request
                   trailing)
              trailing_inputs)
         switching_requests)
    splits

let test_bytes_after_upgrade_request_go_to_new_protocol () =
  each_switching_case (fun label ~chunk request trailing ->
    let recorder = { Recorder.got = ""; eof = false } in
    let calls = ref 0 in
    let t =
      Raw.create (fun reqd ->
        incr calls;
        Request_descriptor.respond_with_upgrade
          reqd
          Headers.empty
          (Ache.Runtime.make (module Recorder) recorder))
    in
    Raw.feed ~chunk t (request ^ trailing);
    Alcotest.(check string)
      (label ^ ": request consumed, rest kept")
      trailing
      t.pending;
    Alcotest.(check string)
      (label ^ ": nothing given before the 101")
      ""
      recorder.got;
    ignore (Raw.write_all t);
    Alcotest.(check string)
      (label ^ ": upgrade response written")
      (upgrade_response_for request)
      (Raw.take_output t);
    Raw.feed ~chunk t "";
    Alcotest.(check string)
      (label ^ ": new protocol gets the bytes")
      trailing
      recorder.got;
    Alcotest.(check int) (label ^ ": one request") 1 !calls)

let test_bytes_after_upgrade_request_parsed_after_other_response () =
  each_switching_case (fun label ~chunk request trailing ->
    let calls = ref 0 in
    let t =
      Raw.create (fun reqd ->
        incr calls;
        if !calls = 1
        then
          Request_descriptor.respond_with_string
            reqd
            (Response.create `Not_found)
            ""
        else Alcotest.failf "%s: trailing bytes dispatched" label)
    in
    Raw.feed ~chunk t (request ^ trailing);
    let first = Raw.take_output t in
    ignore (Raw.write_all t);
    Raw.feed ~chunk t "";
    ignore (Raw.write_all t);
    let output = first ^ Raw.take_output t in
    let response = "HTTP/1.1 404 Not Found\r\ncontent-length: 0\r\n\r\n" in
    Alcotest.(check bool)
      (label ^ ": 404 first")
      true
      (String.starts_with ~prefix:response output);
    let rest =
      String.sub
        output
        (String.length response)
        (String.length output - String.length response)
    in
    if trailing = "ping"
    then (
      Alcotest.(check string)
        (label ^ ": incomplete head consumed")
        ""
        t.pending;
      Alcotest.(check string) (label ^ ": nothing more written") "" rest)
    else
      Alcotest.(check bool)
        (label ^ ": trailing bytes rejected as HTTP")
        true
        (List.exists
           (fun prefix -> String.starts_with ~prefix rest)
           [ "HTTP/1.1 400 "; "HTTP/1.1 505 " ]))

let test_bytes_after_connect_not_read () =
  List.iter
    (fun (split, chunk) ->
       List.iter
         (fun trailing ->
            let label = Printf.sprintf "%S, %s" trailing split in
            let t =
              Raw.create (fun reqd ->
                Request_descriptor.respond_with_string
                  reqd
                  (Response.create `OK)
                  "")
            in
            Raw.feed
              ~chunk
              t
              ("CONNECT a:443 HTTP/1.1\r\nHost: a:443\r\n\r\n" ^ trailing);
            let write_op = Raw.write_all t in
            Raw.feed ~chunk t "";
            Alcotest.(check string)
              (label ^ ": only the 200")
              "HTTP/1.1 200 OK\r\n\r\n"
              (Raw.take_output t);
            Alcotest.(check string)
              (label ^ ": bytes not read")
              trailing
              t.pending;
            Alcotest.(check bool)
              (label ^ ": writer closed")
              true
              (write_op = `Close);
            Alcotest.check
              read_operation
              (label ^ ": reader closed")
              `Close
              (Raw.read_op t))
         trailing_inputs)
    splits

let test_respond_after_upgrade_before_flush () =
  let reqd = ref None in
  let t =
    Raw.create (fun r ->
      reqd := Some r;
      Request_descriptor.respond_with_upgrade
        r
        Headers.empty
        (Ache.Runtime.make (module Recorder) { Recorder.got = ""; eof = false }))
  in
  Raw.feed ~chunk:max_int t "GET / HTTP/1.1\r\nHost: a\r\n\r\n";
  let reqd = Option.get !reqd in
  Alcotest.(check (option int))
    "response is the 101"
    (Some 101)
    (Option.map
       (fun (r : Response.t) -> Status.to_code r.status)
       (Request_descriptor.response reqd));
  Alcotest.check_raises
    "second response raises"
    (Failure
       "ache.Request_descriptor.respond_with_string: response already started")
    (fun () ->
       Request_descriptor.respond_with_string reqd (Response.create `OK) "");
  ignore (Raw.write_all t);
  Alcotest.(check string)
    "only the 101"
    "HTTP/1.1 101 Switching Protocols\r\n\r\n"
    (Raw.take_output t)

let streaming_flush_case () =
  let results = ref [] in
  let t =
    Raw.create (fun reqd ->
      let body =
        Request_descriptor.respond_with_streaming reqd (Response.create `OK)
      in
      Body.Writer.write_string body "a";
      Body.Writer.flush body (fun r -> results := r :: !results))
  in
  Raw.feed ~chunk:max_int t "GET / HTTP/1.1\r\nHost: a\r\n\r\n";
  t, results

let flush_result =
  Alcotest.of_pp (fun fmt r ->
    Format.pp_print_string
      fmt
      (match r with `Written -> "Written" | `Closed -> "Closed"))

let test_flush_closed_when_write_fails () =
  List.iter
    (fun write_body_first ->
       let label =
         if write_body_first then "body in the write" else "body after the head"
       in
       let body = ref None in
       let results = ref [] in
       let t =
         Raw.create (fun reqd ->
           body :=
             Some
               (Request_descriptor.respond_with_streaming
                  ~flush_headers_immediately:true
                  reqd
                  (Response.create `OK)))
       in
       let write () =
         let body = Option.get !body in
         Body.Writer.write_string body "a";
         Body.Writer.flush body (fun r -> results := r :: !results)
       in
       Raw.feed ~chunk:max_int t "GET / HTTP/1.1\r\nHost: a\r\n\r\n";
       if write_body_first then write ();
       (match Server_connection.next_write_operation t.conn with
       | `Write _ ->
         if not write_body_first then write ();
         Server_connection.report_write_result t.conn `Closed
       | _ -> Alcotest.fail "expected a write");
       ignore (Raw.write_all t);
       ignore (Raw.read_op t);
       Alcotest.(check (list flush_result))
         (label ^ ": flush answered once")
         [ `Closed ]
         !results)
    [ true; false ]

let test_flush_closed_on_report_exn () =
  let t, results = streaming_flush_case () in
  Server_connection.report_exn t.conn (Failure "x");
  ignore (Raw.write_all t);
  ignore (Raw.read_op t);
  Alcotest.(check (list flush_result))
    "flush answered once"
    [ `Closed ]
    !results

let test_respond_on_closed_writer () =
  List.iter
    (fun (name, respond) ->
       let reqd = ref None in
       let t = Raw.create (fun r -> reqd := Some r) in
       Raw.feed ~chunk:max_int t "GET / HTTP/1.1\r\nHost: a\r\n\r\n";
       Server_connection.report_write_result t.conn `Closed;
       (match respond (Option.get !reqd) with
       | () -> ()
       | exception exn ->
         Alcotest.failf "%s raised %s" name (Printexc.to_string exn));
       Alcotest.(check bool)
         (name ^ ": response set")
         true
         (Option.is_some (Request_descriptor.response (Option.get !reqd)));
       Alcotest.(check bool)
         (name ^ ": writer closed")
         true
         (Raw.write_all t = `Close);
       Alcotest.(check string)
         (name ^ ": nothing written")
         ""
         (Raw.take_output t))
    [ ( "respond_with_string"
      , fun reqd ->
          Request_descriptor.respond_with_string
            reqd
            (Response.create `OK)
            "hello" )
    ; ( "respond_with_bigstring"
      , fun reqd ->
          Request_descriptor.respond_with_bigstring
            reqd
            (Response.create `OK)
            (Ache.Bigstring.of_string "hello" ~off:0 ~len:5) )
    ; ( "respond_with_streaming"
      , fun reqd ->
          let body =
            Request_descriptor.respond_with_streaming reqd (Response.create `OK)
          in
          Alcotest.(check bool)
            "body writer closed"
            true
            (Body.Writer.is_closed body);
          Body.Writer.write_string body "hello";
          let result = ref None in
          Body.Writer.flush body (fun r -> result := Some r);
          Alcotest.(check (option flush_result))
            "flush answered at once"
            (Some `Closed)
            !result;
          Body.Writer.close body )
    ]

let test_head_timeout_is_408 () =
  List.iter
    (fun (split, chunk) ->
       let errors = ref [] in
       let t =
         Raw.create
           ~error_handler:(fun ?request:_ error start_response ->
             errors := error :: !errors;
             Body.Writer.close (start_response Headers.empty))
           (fun _ -> Alcotest.fail "request handler called")
       in
       Raw.feed ~chunk t "GET / HTTP/1.1\r\nHo";
       (match Server_connection.read_timeout t.conn with
       | Some (`Head, reject) -> reject ()
       | Some ((`Idle | `Body), _) | None ->
         Alcotest.fail "expected a head timeout");
       Alcotest.(check (list request_error))
         (split ^ ": error")
         [ `Request_timeout ]
         !errors;
       Alcotest.check
         read_operation
         (split ^ ": reader closed")
         `Close
         (Raw.read_op t);
       Alcotest.(check bool)
         (split ^ ": writer closed")
         true
         (Raw.write_all t = `Close);
       Alcotest.(check string)
         (split ^ ": 408 written")
         "HTTP/1.1 408 Request Timeout\r\nconnection: close\r\n\r\n"
         (Raw.take_output t))
    splits

let test_upgrade_wins_over_connection_close () =
  List.iter
    (fun (split, chunk) ->
       List.iter
         (fun async ->
            let label = split ^ if async then ", async" else ", sync" in
            let recorder = { Recorder.got = ""; eof = false } in
            let respond reqd =
              Request_descriptor.respond_with_upgrade
                reqd
                (Headers.of_list [ "connection", "upgrade"; "upgrade", "x" ])
                (Ache.Runtime.make (module Recorder) recorder)
            in
            let pending = ref None in
            let t =
              Raw.create (fun reqd ->
                if async then pending := Some reqd else respond reqd)
            in
            Raw.feed
              ~chunk
              t
              "GET / HTTP/1.1\r\n\
               Host: a\r\n\
               Connection: close, upgrade\r\n\
               Upgrade: x\r\n\
               \r\n\
               ping";
            Alcotest.(check bool)
              (label ^ ": reader not closed before the 101")
              true
              (Raw.read_op t <> `Close);
            Option.iter respond !pending;
            ignore (Raw.write_all t);
            Alcotest.(check string)
              (label ^ ": 101 written")
              "HTTP/1.1 101 Switching Protocols\r\n\
               connection: upgrade\r\n\
               upgrade: x\r\n\
               \r\n"
              (Raw.take_output t);
            Raw.feed ~chunk t "";
            Alcotest.(check string)
              (label ^ ": new protocol gets the bytes")
              "ping"
              recorder.got)
         [ false; true ])
    splits

type switch_timing =
  | Before_eof
  | After_eof
  | Never

let test_switching_request_then_eof () =
  let forbidden =
    Response.create
      ~headers:(Headers.of_list [ "content-length", "0" ])
      `Forbidden
  in
  List.iter
    (fun (split, chunk) ->
       List.iter
         (fun (kind, request) ->
            List.iter
              (fun (timing, upgrade) ->
                 let label =
                   Printf.sprintf
                     "%s, %s, %s, %s"
                     kind
                     (match timing with
                     | Before_eof -> "responds before EOF"
                     | After_eof -> "responds after EOF"
                     | Never -> "never responds")
                     (if upgrade then "101" else "403")
                     split
                 in
                 let recorder = { Recorder.got = ""; eof = false } in
                 let respond reqd =
                   if upgrade
                   then
                     Request_descriptor.respond_with_upgrade
                       reqd
                       Headers.empty
                       (Ache.Runtime.make (module Recorder) recorder)
                   else Request_descriptor.respond_with_string reqd forbidden ""
                 in
                 let held = ref None in
                 let t =
                   Raw.create (fun reqd ->
                     if timing = Before_eof
                     then respond reqd
                     else held := Some reqd)
                 in
                 Raw.feed ~eof:true ~chunk t request;
                 if timing <> Before_eof
                 then
                   Alcotest.check
                     read_operation
                     (label ^ ": reader closed after EOF")
                     `Close
                     (Raw.read_op t);
                 if timing = After_eof then Option.iter respond !held;
                 ignore (Raw.write_all t);
                 let output = Raw.take_output t in
                 match timing, upgrade with
                 | Never, _ ->
                   Alcotest.(check string)
                     (label ^ ": nothing written")
                     ""
                     output
                 | _, true ->
                   Alcotest.(check string)
                     (label ^ ": upgrade response written")
                     (upgrade_response_for request)
                     output;
                   Alcotest.(check bool)
                     (label ^ ": new protocol sees EOF")
                     true
                     recorder.eof
                 | _, false ->
                   Alcotest.(check bool)
                     (label ^ ": 403 written")
                     true
                     (String.starts_with ~prefix:"HTTP/1.1 403 " output);
                   Alcotest.(check bool)
                     (label ^ ": closed")
                     true
                     (Raw.write_all t = `Close))
              [ Before_eof, false
              ; Before_eof, true
              ; After_eof, false
              ; After_eof, true
              ; Never, false
              ])
         switching_requests)
    splits

let test_after_write_closed () =
  let post n =
    Printf.sprintf "POST / HTTP/1.1\r\nHost: a\r\ncontent-length: %d\r\n\r\n" n
  in
  let get = "GET / HTTP/1.1\r\nHost: a\r\n\r\n" in
  List.iter
    (fun (split, chunk) ->
       let calls = ref 0 in
       let received = ref "" in
       let ended = ref "" in
       let reading_handler reqd =
         incr calls;
         let body = Request_descriptor.request_body reqd in
         let rec loop () =
           Body.Reader.schedule_read
             body
             ~on_eof:(fun () -> ended := "eof")
             ~on_error:(fun () -> ended := "error")
             ~on_read:(fun bs ~off ~len ->
               received := !received ^ Ache.Bigstring.substring bs ~off ~len;
               loop ())
         in
         loop ()
       in
       let t = Raw.create reading_handler in
       Raw.feed ~chunk t (post 10 ^ "hello");
       Server_connection.report_write_result t.conn `Closed;
       Raw.feed ~chunk t ("world" ^ get);
       Alcotest.(check string)
         (split ^ ": in-flight body complete")
         "helloworld"
         !received;
       Alcotest.(check string)
         (split ^ ": in-flight body ends with on_eof")
         "eof"
         !ended;
       Alcotest.(check int) (split ^ ": no new request dispatched") 1 !calls;
       Alcotest.check
         read_operation
         (split ^ ": reader closed")
         `Close
         (Raw.read_op t);
       calls := 0;
       let t = Raw.create (fun _ -> incr calls) in
       Raw.feed ~chunk t get;
       Server_connection.report_write_result t.conn `Closed;
       Raw.feed ~chunk t (get ^ get);
       Alcotest.(check int) (split ^ ": idle: nothing more dispatched") 1 !calls;
       Alcotest.check
         read_operation
         (split ^ ": idle: reader closed")
         `Close
         (Raw.read_op t))
    splits

let test_pipelined_request_not_dispatched_after_write_closed () =
  List.iter
    (fun (split, chunk) ->
       let calls = ref 0 in
       let t = Raw.create (fun _ -> incr calls) in
       let get = "GET / HTTP/1.1\r\nHost: a\r\n\r\n" in
       Raw.feed ~chunk t (get ^ get);
       Server_connection.report_write_result t.conn `Closed;
       ignore (Raw.write_all t);
       Raw.feed ~chunk t "";
       Alcotest.(check int) (split ^ ": second request not dispatched") 1 !calls;
       Alcotest.check
         read_operation
         (split ^ ": reader closed")
         `Close
         (Raw.read_op t))
    splits

let test_streaming_response_when_write_fails () =
  List.iter
    (fun (split, chunk) ->
       let calls = ref 0 in
       let body = ref None in
       let t =
         Raw.create (fun reqd ->
           incr calls;
           body :=
             Some
               (Request_descriptor.respond_with_streaming
                  ~flush_headers_immediately:true
                  reqd
                  (Response.create `OK)))
       in
       let get = "GET / HTTP/1.1\r\nHost: a\r\n\r\n" in
       Raw.feed ~chunk t get;
       ignore (Raw.write_all t);
       let body = Option.get !body in
       Body.Writer.write_string body "chunk";
       let results = ref [] in
       Body.Writer.flush body (fun r -> results := r :: !results);
       (match Server_connection.next_write_operation t.conn with
       | `Write _ -> Server_connection.report_write_result t.conn `Closed
       | `Yield | `Close _ -> Alcotest.fail "expected a write");
       Raw.feed ~chunk t (get ^ get);
       Alcotest.(check (list flush_result))
         (split ^ ": flush")
         [ `Closed ]
         !results;
       Alcotest.(check int) (split ^ ": handler not called again") 1 !calls;
       Alcotest.check
         read_operation
         (split ^ ": reader closed")
         `Close
         (Raw.read_op t))
    splits

let test_error_handler_respond_twice () =
  let second = ref None in
  let t =
    Raw.create
      ~error_handler:(fun ?request:_ _ respond ->
        Body.Writer.close (respond Headers.empty);
        match respond Headers.empty with
        | (_ : Body.Writer.t) -> second := Some "no exception"
        | exception Failure msg -> second := Some msg)
      (fun _ -> Alcotest.fail "request handler called")
  in
  Raw.feed ~chunk:max_int t "garbage garbage\r\n\r\n";
  ignore (Raw.write_all t);
  Alcotest.(check (option string))
    "second call raises"
    (Some "ache.Server_connection.error_handler: response already started")
    !second;
  Alcotest.(check string)
    "one response"
    "HTTP/1.1 400 Bad Request\r\nconnection: close\r\n\r\n"
    (Raw.take_output t)

let bad_request_inputs =
  [ ( "bare LF"
    , "GET / HTTP/1.1\nHost: a\r\n\r\n"
    , "unexpected '\\n' in request-line" )
  ; ( "obs-fold"
    , "GET / HTTP/1.1\r\nHost: a\r\nX: b\r\n c\r\n\r\n"
    , "unexpected ' ' in field line" )
  ; ( "bad Content-Length"
    , "POST / HTTP/1.1\r\nHost: a\r\nContent-Length: 1x\r\n\r\n"
    , "invalid Content-Length" )
  ; ( "bad target"
    , "GET index.html HTTP/1.1\r\nHost: a\r\n\r\n"
    , "request-target does not fit the method" )
  ]

let test_bad_request_reasons () =
  let reasons =
    List.map
      (fun (case, input, expected) ->
         let errors = ref [] in
         let t =
           Raw.create
             ~error_handler:(fun ?request:_ error respond ->
               errors := error :: !errors;
               Body.Writer.close (respond Headers.empty))
             (fun _ -> Alcotest.fail "request handler called")
         in
         Raw.feed ~chunk:max_int t input;
         ignore (Raw.write_all t);
         Alcotest.(check (list request_error))
           case
           [ `Bad_request expected ]
           !errors;
         expected)
      bad_request_inputs
  in
  Alcotest.(check int)
    "distinct reasons"
    (List.length reasons)
    (List.length (List.sort_uniq String.compare reasons))

let test_default_error_handler_hides_reason () =
  List.iter
    (fun (case, input, _) ->
       let t =
         { Raw.conn =
             Server_connection.create (fun _ ->
               Alcotest.fail "request handler called")
         ; pending = ""
         ; out = Buffer.create 256
         }
       in
       Raw.feed ~chunk:max_int t input;
       ignore (Raw.write_all t);
       Alcotest.(check string)
         case
         "HTTP/1.1 400 Bad Request\r\nconnection: close\r\n\r\n400"
         (Raw.take_output t))
    bad_request_inputs

let test_error_handler_flush_then_raise () =
  List.iter
    (fun (case, input, handler) ->
       let results = ref [] in
       let t =
         Raw.create
           ~error_handler:(fun ?request:_ _ respond ->
             let body = respond Headers.empty in
             Body.Writer.write_string body "x";
             Body.Writer.flush body (fun r -> results := r :: !results);
             raise Exit)
           handler
       in
       Raw.feed ~chunk:max_int t input;
       ignore (Raw.write_all t);
       ignore (Raw.read_op t);
       Alcotest.(check (list flush_result))
         (case ^ ": flush answered once")
         [ `Closed ]
         !results)
    [ ( "parse error"
      , "garbage garbage\r\n\r\n"
      , fun _ -> Alcotest.fail "request handler called" )
    ; ( "handler exception"
      , "GET / HTTP/1.1\r\nHost: a\r\n\r\n"
      , fun reqd -> Request_descriptor.report_exn reqd (Failure "handler") )
    ]

let test_report_exn_while_error_handler_pending () =
  let respond = ref None in
  let t =
    Raw.create
      ~error_handler:(fun ?request:_ _ r -> respond := Some r)
      (fun reqd -> Request_descriptor.report_exn reqd (Failure "handler"))
  in
  Raw.feed ~chunk:max_int t "GET / HTTP/1.1\r\nHost: a\r\n\r\n";
  (match Server_connection.report_exn t.conn (Failure "io") with
  | () -> ()
  | exception exn ->
    Alcotest.failf "report_exn raised %s" (Printexc.to_string exn));
  Alcotest.check read_operation "reader closed" `Close (Raw.read_op t);
  Alcotest.(check bool) "writer closed" true (Raw.write_all t = `Close);
  (match !respond with
  | Some r -> Body.Writer.close (r Headers.empty)
  | None -> Alcotest.fail "error handler not called");
  Alcotest.(check bool) "still closed" true (Raw.write_all t = `Close);
  Alcotest.(check string) "nothing written" "" (Raw.take_output t)

let test_tunnel_after_eof () =
  List.iter
    (fun (split, chunk) ->
       let held = ref None in
       let t = Raw.create (fun reqd -> held := Some reqd) in
       Raw.feed
         ~eof:true
         ~chunk
         t
         "CONNECT a:443 HTTP/1.1\r\nHost: a:443\r\n\r\n";
       let body =
         Request_descriptor.respond_with_streaming
           (Option.get !held)
           (Response.create `OK)
       in
       Body.Writer.write_string body "tunnel";
       Body.Writer.flush body ignore;
       ignore (Raw.write_all t);
       let op = Raw.read_op t in
       (if op = `Yield
        then
          match Server_connection.yield_reader t.conn ignore with
          | () -> ()
          | exception exn ->
            Alcotest.failf
              "%s: yield_reader raised %s"
              split
              (Printexc.to_string exn));
       Alcotest.check read_operation (split ^ ": reader closed") `Close op)
    splits

let record_body events body =
  let rec loop () =
    Body.Reader.schedule_read
      body
      ~on_eof:(fun () -> events := "eof" :: !events)
      ~on_error:(fun () -> events := "error" :: !events)
      ~on_read:(fun _ ~off:_ ~len:_ ->
        events := "read" :: !events;
        loop ())
  in
  loop ()

let check_one_ending label expected events =
  let ends = List.filter (fun e -> e <> "read") !events in
  Alcotest.(check (list string)) (label ^ ": one ending") [ expected ] ends;
  Alcotest.(check string)
    (label ^ ": ending is last")
    expected
    (List.hd !events)

let test_request_body_endings () =
  let post headers body =
    "POST / HTTP/1.1\r\nHost: a\r\n" ^ headers ^ "\r\n" ^ body
  in
  List.iter
    (fun (split, chunk) ->
       List.iter
         (fun (case, input, finish, expected) ->
            let label = case ^ ", " ^ split in
            let events = ref [] in
            let body = ref None in
            let t =
              Raw.create (fun reqd ->
                body := Some (Request_descriptor.request_body reqd);
                record_body events (Request_descriptor.request_body reqd))
            in
            Raw.feed ~chunk t input;
            finish t (Option.get !body);
            check_one_ending label expected events;
            let before = List.length !events in
            record_body events (Option.get !body);
            Server_connection.shutdown t.conn;
            Raw.feed ~eof:true ~chunk t "more";
            Alcotest.(check int)
              (label ^ ": nothing after the ending")
              before
              (List.length !events))
         [ ( "complete"
           , post "content-length: 3\r\n" "abc"
           , (fun t _ -> Raw.feed ~eof:true ~chunk t "")
           , "eof" )
         ; ( "cut by EOF"
           , post "content-length: 5\r\n" "ab"
           , (fun t _ -> Raw.feed ~eof:true ~chunk t "")
           , "error" )
         ; ( "framing error"
           , post "transfer-encoding: chunked\r\n" "3\r\nabcX"
           , (fun t _ -> ignore (Raw.read_op t))
           , "error" )
         ; ( "closed by the application"
           , post "content-length: 5\r\n" "ab"
           , (fun t body ->
               Body.Reader.close body;
               Raw.feed ~chunk t "cde")
           , "eof" )
         ; ( "connection shutdown"
           , post "content-length: 5\r\n" "ab"
           , (fun t _ -> Server_connection.shutdown t.conn)
           , "error" )
         ])
    splits

let test_upgrade_failure_messages () =
  let messages = ref [] in
  let attempt f =
    match f () with
    | () -> messages := "no exception" :: !messages
    | exception Failure msg -> messages := msg :: !messages
  in
  let impl () =
    Ache.Runtime.make (module Recorder) { Recorder.got = ""; eof = false }
  in
  let t =
    Raw.create
      ~error_handler:(fun ?request:_ _ _ -> ())
      (fun reqd ->
         match (Request_descriptor.request reqd).target with
         | "/twice" ->
           Request_descriptor.respond_with_upgrade reqd Headers.empty (impl ());
           attempt (fun () ->
             Request_descriptor.respond_with_upgrade
               reqd
               Headers.empty
               (impl ()))
         | _ ->
           Request_descriptor.report_exn reqd (Failure "x");
           attempt (fun () ->
             Request_descriptor.respond_with_upgrade
               reqd
               Headers.empty
               (impl ())))
  in
  Raw.feed ~chunk:max_int t "GET /error HTTP/1.1\r\nHost: a\r\n\r\n";
  let t2 =
    Raw.create (fun reqd ->
      Request_descriptor.respond_with_upgrade reqd Headers.empty (impl ());
      attempt (fun () ->
        Request_descriptor.respond_with_upgrade reqd Headers.empty (impl ())))
  in
  Raw.feed ~chunk:max_int t2 "GET /twice HTTP/1.1\r\nHost: a\r\n\r\n";
  Alcotest.(check (list string))
    "messages name respond_with_upgrade"
    [ "ache.Request_descriptor.respond_with_upgrade: invalid state, currently \
       handling error"
    ; "ache.Request_descriptor.respond_with_upgrade: response already started"
    ]
    (List.rev !messages)

let test_pipelined_output_backpressure () =
  let get = "GET / HTTP/1.1\r\nHost: a\r\n\r\n" in
  let body = String.make 0x10000 'x' in
  let count = 50 in
  List.iter
    (fun (split, chunk) ->
       let handled = ref 0 in
       let t =
         Raw.create (fun reqd ->
           incr handled;
           Request_descriptor.respond_with_string
             reqd
             (Response.create `OK)
             body)
       in
       for _ = 1 to count do
         Raw.feed ~chunk t get
       done;
       Alcotest.check
         read_operation
         (split ^ ": reader waits for the writer")
         `Yield
         (Raw.read_op t);
       if !handled > 2
       then
         Alcotest.failf
           "%s: %d requests answered while no byte was written"
           split
           !handled;
       let woken = ref false in
       Server_connection.yield_reader t.conn (fun () -> woken := true);
       let rec serve rounds =
         if rounds = 0 then Alcotest.failf "%s: requests not all served" split;
         ignore (Raw.write_all t);
         Raw.feed ~chunk t "";
         if !handled < count || Raw.write_op t = `Write then serve (rounds - 1)
       in
       serve (count * 4);
       Alcotest.(check bool) (split ^ ": writes wake the reader") true !woken;
       let response =
         response_to_string
           ~body
           (Response.create
              ~headers:(Headers.of_list [ "content-length", "65536" ])
              `OK)
       in
       Alcotest.(check int)
         (split ^ ": every response written once")
         (count * String.length response)
         (String.length (Raw.take_output t));
       Alcotest.check
         read_operation
         (split ^ ": reader waits for the next request")
         `Read
         (Raw.read_op t))
    splits

let test_connect_2xx_without_upgrade_closes () =
  let connect = "CONNECT a:443 HTTP/1.1\r\nHost: a:443\r\n\r\n" in
  List.iter
    (fun (split, chunk) ->
       List.iter
         (fun (case, handler, expected) ->
            List.iter
              (fun eof ->
                 let label =
                   Printf.sprintf
                     "%s, %s%s"
                     case
                     split
                     (if eof then ", then EOF" else "")
                 in
                 let t = Raw.create handler in
                 Raw.feed ~eof ~chunk t (connect ^ "tunnel bytes");
                 let write_op = Raw.write_all t in
                 Alcotest.(check string)
                   (label ^ ": response written")
                   expected
                   (Raw.take_output t);
                 Alcotest.(check bool)
                   (label ^ ": writer closed")
                   true
                   (write_op = `Close);
                 Alcotest.check
                   read_operation
                   (label ^ ": reader closed")
                   `Close
                   (Raw.read_op t);
                 Alcotest.(check bool)
                   (label ^ ": connection closed")
                   true
                   (Server_connection.is_closed t.conn))
              [ false; true ])
         [ ( "string"
           , (fun reqd ->
               Request_descriptor.respond_with_string
                 reqd
                 (Response.create `OK)
                 "hello")
           , "HTTP/1.1 200 OK\r\n\r\nhello" )
         ; ( "streaming"
           , (fun reqd ->
               let body =
                 Request_descriptor.respond_with_streaming
                   reqd
                   (Response.create `OK)
               in
               Body.Writer.write_string body "tunnel data";
               Body.Writer.close body)
           , "HTTP/1.1 200 OK\r\n\r\ntunnel data" )
         ])
    splits

let test_pipelined_handler_exception () =
  let get = "GET / HTTP/1.1\r\nHost: a\r\n\r\n" in
  List.iter
    (fun (split, chunk) ->
       let calls = ref 0 in
       let errors = ref [] in
       let t =
         Raw.create
           ~error_handler:(fun ?request error start_response ->
             errors :=
               (Option.map (fun r -> r.Request.target) request, error)
               :: !errors;
             Body.Writer.close (start_response Headers.empty))
           (fun reqd ->
              incr calls;
              if !calls = 2 then failwith "handler 2";
              Request_descriptor.respond_with_string
                reqd
                (Response.create `OK)
                "one")
       in
       (match Raw.feed ~chunk t (get ^ get) with
       | () -> ()
       | exception (Failure _ as exn) when chunk = 1 ->
         Server_connection.report_exn t.conn exn);
       let write_op =
         match Raw.write_all t with
         | op -> op
         | exception exn ->
           Alcotest.failf "%s: raised %s" split (Printexc.to_string exn)
       in
       Alcotest.(check int) (split ^ ": both handlers called") 2 !calls;
       Alcotest.(check (list (pair (option string) request_error)))
         (split ^ ": error reported on the second request")
         [ Some "/", `Exn (Failure "handler 2") ]
         !errors;
       Alcotest.(check string)
         (split ^ ": first response, then the error response")
         "HTTP/1.1 200 OK\r\n\
          content-length: 3\r\n\
          \r\n\
          oneHTTP/1.1 500 Internal Server Error\r\n\
          connection: close\r\n\
          \r\n"
         (Raw.take_output t);
       Alcotest.(check bool) (split ^ ": writer closed") true (write_op = `Close);
       Alcotest.check
         read_operation
         (split ^ ": reader closed")
         `Close
         (Raw.read_op t))
    splits

let post_head n =
  Printf.sprintf "POST / HTTP/1.1\r\nHost: a\r\ncontent-length: %d\r\n\r\n" n

let read_timeout_kind conn =
  match Server_connection.read_timeout conn with
  | None -> "none"
  | Some (`Idle, _) -> "idle"
  | Some (`Head, _) -> "head"
  | Some (`Body, _) -> "body"

let test_body_delivered_after_closing_response () =
  List.iter
    (fun (split, chunk) ->
       let events = ref [] in
       let received = ref "" in
       let t =
         Raw.create (fun reqd ->
           let body = Request_descriptor.request_body reqd in
           let rec loop () =
             Body.Reader.schedule_read
               body
               ~on_eof:(fun () -> events := "eof" :: !events)
               ~on_error:(fun () -> events := "error" :: !events)
               ~on_read:(fun bs ~off ~len ->
                 received := !received ^ Ache.Bigstring.substring bs ~off ~len;
                 loop ())
           in
           loop ();
           Request_descriptor.respond_with_string
             reqd
             (Response.create
                ~headers:(Headers.of_list [ "connection", "close" ])
                `OK)
             "bye")
       in
       Raw.feed ~chunk t (post_head 10 ^ "01234");
       Alcotest.(check bool)
         (split ^ ": writer closed")
         true
         (Raw.write_all t = `Close);
       Alcotest.(check string)
         (split ^ ": response written")
         "HTTP/1.1 200 OK\r\nconnection: close\r\ncontent-length: 3\r\n\r\nbye"
         (Raw.take_output t);
       Raw.feed ~chunk t "56789";
       Alcotest.(check string)
         (split ^ ": body delivered")
         "0123456789"
         !received;
       Alcotest.(check (list string)) (split ^ ": body ends") [ "eof" ] !events;
       Alcotest.check
         read_operation
         (split ^ ": reader closed")
         `Close
         (Raw.read_op t);
       Alcotest.(check bool)
         (split ^ ": connection closed")
         true
         (Server_connection.is_closed t.conn))
    splits

let test_unread_body_after_response_is_timed () =
  List.iter
    (fun (split, chunk) ->
       List.iter
         (fun (case, response, read_body) ->
            let label = case ^ ", " ^ split in
            let events = ref [] in
            let t =
              Raw.create (fun reqd ->
                if read_body
                then record_body events (Request_descriptor.request_body reqd);
                Request_descriptor.respond_with_string reqd response "no")
            in
            Raw.feed ~chunk t (post_head 100000 ^ "abc");
            Alcotest.(check string)
              (label ^ ": before the write")
              "body"
              (read_timeout_kind t.conn);
            ignore (Raw.write_all t);
            Alcotest.check
              read_operation
              (label ^ ": reader waits for the body")
              `Read
              (Raw.read_op t);
            (match Server_connection.read_timeout t.conn with
            | Some (`Body, close) -> close ()
            | _ ->
              Alcotest.failf
                "%s: expected a body timeout, got %s"
                label
                (read_timeout_kind t.conn));
            if read_body then check_one_ending label "error" events;
            Alcotest.check
              read_operation
              (label ^ ": reader closed")
              `Close
              (Raw.read_op t);
            Alcotest.(check bool)
              (label ^ ": writer closed")
              true
              (Raw.write_all t = `Close);
            Alcotest.(check bool)
              (label ^ ": connection closed")
              true
              (Server_connection.is_closed t.conn))
         [ ( "closing response"
           , Response.create
               ~headers:(Headers.of_list [ "connection", "close" ])
               `OK
           , false )
         ; "413, keep-alive", Response.create `Content_too_large, false
         ; "200, body being read", Response.create `OK, true
         ])
    splits

let test_body_timeout_only_after_response () =
  let t =
    Raw.create (fun reqd ->
      record_body (ref []) (Request_descriptor.request_body reqd))
  in
  Raw.feed ~chunk:max_int t (post_head 10 ^ "01234");
  Alcotest.(check string)
    "no timeout while the response is pending"
    "none"
    (read_timeout_kind t.conn)

let test_connect_upgrade_status () =
  let impl () =
    Ache.Runtime.make (module Recorder) { Recorder.got = ""; eof = false }
  in
  let raised = ref [] in
  let attempt f =
    match f () with
    | () -> ()
    | exception Invalid_argument msg -> raised := msg :: !raised
  in
  let t =
    Raw.create (fun reqd ->
      match (Request_descriptor.request reqd).meth with
      | `CONNECT ->
        attempt (fun () ->
          Request_descriptor.respond_with_upgrade
            reqd
            (Headers.of_list [ "content-length", "0" ])
            (impl ()));
        Request_descriptor.respond_with_upgrade
          ~status:`Created
          reqd
          Headers.empty
          (impl ())
      | _ ->
        attempt (fun () ->
          Request_descriptor.respond_with_upgrade
            ~status:`OK
            reqd
            Headers.empty
            (impl ()));
        Request_descriptor.respond_with_string reqd (Response.create `OK) "")
  in
  Raw.feed ~chunk:max_int t "GET / HTTP/1.1\r\nHost: a\r\n\r\n";
  ignore (Raw.write_all t);
  Raw.feed ~chunk:max_int t "CONNECT a:443 HTTP/1.1\r\nHost: a:443\r\n\r\n";
  ignore (Raw.write_all t);
  Alcotest.(check string)
    "200 to the GET, then 201 to the CONNECT"
    "HTTP/1.1 200 OK\r\ncontent-length: 0\r\n\r\nHTTP/1.1 201 Created\r\n\r\n"
    (Raw.take_output t);
  Alcotest.(check (list string))
    "invalid upgrades raise"
    [ "ache.Request_descriptor.respond_with_upgrade: ~status is for a CONNECT \
       request"
    ; "ache.Request_descriptor.respond_with_upgrade: content-length or \
       transfer-encoding in a 1xx, a 204, or a 2xx to CONNECT"
    ]
    (List.rev !raised)

let tests =
  [ "initial reader state", `Quick, test_initial_reader_state
  ; "shutdown reader closed", `Quick, test_reader_is_closed_after_eof
  ; ( "commit parse after every header line"
    , `Quick
    , test_commit_parse_after_every_header )
  ; "single GET", `Quick, test_single_get
  ; "multiple GETs", `Quick, test_multiple_get
  ; "asynchronous response", `Quick, test_asynchronous_response
  ; ( "asynchronous response, asynchronous body"
    , `Quick
    , test_asynchronous_streaming_response_flush_immediately )
  ; ( "asynchronous response, asynchronous body, writer doesn't yield"
    , `Quick
    , test_asynchronous_streaming_response_writer_doesnt_yield )
  ; "echo POST", `Quick, test_echo_post
  ; "streaming response", `Quick, test_streaming_response
  ; ( "asynchronous streaming response"
    , `Quick
    , test_asynchronous_streaming_response )
  ; ( "asynchronous streaming response, immediate flush"
    , `Quick
    , test_asynchronous_streaming_response_with_immediate_flush )
  ; ( "empty fixed streaming response"
    , `Quick
    , test_empty_fixed_streaming_response )
  ; ( "empty chunked streaming response"
    , `Quick
    , test_empty_chunked_streaming_response )
  ; "connection error", `Quick, test_connection_error
  ; "synchronous error, synchronous handling", `Quick, test_synchronous_error
  ; ( "synchronous error, asynchronous handling"
    , `Quick
    , test_synchronous_error_asynchronous_handling )
  ; "asynchronous error, synchronous handling", `Quick, test_asynchronous_error
  ; ( "asynchronous error, asynchronous handling"
    , `Quick
    , test_asynchronous_error_asynchronous_handling )
  ; ( "asynchronous error, asynchronous handling + asynchronous body"
    , `Quick
    , test_asynchronous_error_asynchronous_response_body )
  ; "error while parsing", `Quick, test_error_while_parsing
  ; "error before read", `Quick, test_error_before_read
  ; "error left unhandled", `Quick, test_error_left_unhandled
  ; "chunked encoding", `Quick, test_chunked_encoding
  ; "chunked encoding for error", `Quick, test_chunked_encoding_for_error
  ; ( "blocked write on chunked encoding"
    , `Quick
    , test_blocked_write_on_chunked_encoding )
  ; "respond with upgrade", `Quick, test_respond_with_upgrade
  ; "upgrade response write fails", `Quick, test_upgrade_write_closed
  ; "writer unexpected eof", `Quick, test_unexpected_eof
  ; "input shrunk", `Quick, test_input_shrunk
  ; "malformed request", `Quick, test_malformed_request
  ; "malformed request (async)", `Quick, test_malformed_request_async
  ; ( "multiple malformed requests?"
    , `Quick
    , test_malformed_request_async_multiple_errors )
  ; ( "malformed request, chunked error response"
    , `Quick
    , test_malformed_request_chunked_error_response )
  ; ( "malformed request, double report_exn"
    , `Quick
    , test_malformed_request_double_report_exn )
  ; "malformed request (EOF)", `Quick, test_malformed_request_eof
  ; ( "malformed request, streaming response"
    , `Quick
    , test_malformed_request_streaming_error_response )
  ; ( "`flush_headers_immediately` with empty body"
    , `Quick
    , test_immediate_flush_empty_body )
  ; ( "empty body with no immediate flush"
    , `Quick
    , test_empty_body_no_immediate_flush )
  ; ( "yield before starting a response"
    , `Quick
    , test_yield_before_starting_a_response )
  ; ( "respond before body has been read"
    , `Quick
    , test_respond_before_reading_entire_body )
  ; ( "test yield when read isn't scheduled"
    , `Quick
    , test_handling_backpressure_when_read_not_scheduled )
  ; ( "test yield when read isn't scheduled, reader yields early"
    , `Quick
    , test_handling_backpressure_when_read_not_scheduled_early_yield )
  ; "test partial input chunked body", `Quick, test_input_shrunk_chunked
  ; ( "respond before reading request body, then request body EOFs"
    , `Quick
    , test_respond_before_reading_entire_body_chunked_eof )
  ; ( "request body EOFs before closing response body, request body not closed"
    , `Quick
    , test_finish_response_after_read_eof )
  ; ( "respond before reading entire request body"
    , `Quick
    , test_respond_before_reading_entire_body_no_error )
  ; ( "respond before reading entire request body, streaming response"
    , `Quick
    , test_streaming_response_before_reading_entire_body_no_error )
  ; "failed request parse", `Quick, test_failed_request_parse
  ; "bad request", `Quick, test_bad_request
  ; ( "shutdown delivers eof to request bodies"
    , `Quick
    , test_shutdown_hangs_request_body_read )
  ; ( "request body eof, finish response immediately"
    , `Quick
    , test_finish_response_after_read_eof_well_formed )
  ; ( "request body eof, async response triggers error handler"
    , `Quick
    , test_finish_response_after_read_eof_malformed )
  ; ( "request body response not sent"
    , `Quick
    , test_request_body_eof_response_not_sent )
  ; ( "request body response not sent empty eof"
    , `Quick
    , test_request_body_eof_response_not_sent_empty_eof )
  ; ( "reader EOF race condition causes state machine to issue writer yield"
    , `Quick
    , test_race_condition_writer_issues_yield_after_reader_eof )
  ; ( "multiple requests in single read"
    , `Quick
    , test_multiple_requests_in_single_read )
  ; ( "multiple async requests in single read"
    , `Quick
    , test_multiple_async_requests_in_single_read )
  ; ( "chunked-encoding streaming error"
    , `Quick
    , test_errored_chunked_streaming_response )
  ; ( "content-length streaming error"
    , `Quick
    , test_errored_content_length_streaming_response )
  ; ( "chunked-encoding async streaming error"
    , `Quick
    , test_errored_chunked_streaming_response_async )
  ; ( "multiple requests with connection close"
    , `Quick
    , test_multiple_requests_in_single_read_with_close )
  ; ( "multiple requests with eof"
    , `Quick
    , test_multiple_requests_in_single_read_with_eof )
  ; ( "parse failure after checkpoint"
    , `Quick
    , test_parse_failure_after_checkpoint )
  ; "parse failure at eof", `Quick, test_parse_failure_at_eof
  ; ( "response finished before body read"
    , `Quick
    , test_response_finished_before_body_read )
  ; "pipelined", `Quick, test_pipelined_requests_answer_before_reading_body
  ; ( "body has a chance to flush before the next read operation"
    , `Quick
    , test_body_flush_fairness )
  ; "shutdown in request handler", `Quick, test_shutdown_in_request_handler
  ; ( "shutdown during asynchronous request"
    , `Quick
    , test_shutdown_during_asynchronous_request )
  ; ( "flush response before shutdown"
    , `Quick
    , test_flush_response_before_shutdown )
  ; ( "schedule read with data available"
    , `Quick
    , test_schedule_read_with_data_available )
  ; ( "read body eof called multiple times"
    , `Quick
    , test_eof_called_multiple_times )
  ; "eof called for empty bodies", `Quick, test_eof_called_for_empty_bodies
  ; ( "error handler with chunked transfer encoding"
    , `Quick
    , test_error_handler_chunked_response )
  ; ( "pipelined requests in single read"
    , `Quick
    , test_pipelined_requests_in_single_buffer_partial_body )
  ; "multiple pipelined requests", `Quick, test_multiple_pipelined_requests
  ; ( "Body.Writer.flush waits for bytes to have been written to the wire"
    , `Quick
    , test_body_flush_after_bytes_in_the_wire )
  ; ( "input consumed before closing request body"
    , `Quick
    , test_input_consumed_before_closing_req_body )
  ; ( "no request dispatched after write eof"
    , `Quick
    , test_no_request_dispatched_after_write_eof )
  ; ( "no request dispatched after write eof (before response sent)"
    , `Quick
    , test_no_request_dispatched_after_write_eof_before_send_response )
  ; ( "write response after reader EOF"
    , `Quick
    , test_write_response_after_read_eof )
  ; "CONNECT method", `Quick, test_connect_method
  ; ( "invalid response head writes nothing"
    , `Quick
    , test_invalid_response_head_writes_nothing )
  ; ( "EOF in the head: 400 in either call order"
    , `Quick
    , test_eof_in_head_either_call_order )
  ; ( "incomplete request body: on_error, not on_eof"
    , `Quick
    , test_incomplete_request_body_not_complete )
  ; ( "complete request body then EOF: on_eof"
    , `Quick
    , test_complete_request_body_at_eof )
  ; ( "unread request body discarded after the response"
    , `Quick
    , test_unread_body_discarded_after_response )
  ; "EOF in a discarded request body", `Quick, test_eof_in_discarded_body
  ; ( "error handler with invalid headers closes the connection"
    , `Quick
    , test_error_handler_invalid_headers_closes )
  ; ( "bytes after an upgrade request go to the new protocol"
    , `Quick
    , test_bytes_after_upgrade_request_go_to_new_protocol )
  ; ( "bytes after an upgrade request are HTTP after another response"
    , `Quick
    , test_bytes_after_upgrade_request_parsed_after_other_response )
  ; ( "bytes after a 2xx to CONNECT are not read; the connection closes"
    , `Quick
    , test_bytes_after_connect_not_read )
  ; ( "respond after an upgrade, before the 101 is written"
    , `Quick
    , test_respond_after_upgrade_before_flush )
  ; "head timeout answers 408", `Quick, test_head_timeout_is_408
  ; ( "upgrade or CONNECT request, then EOF"
    , `Quick
    , test_switching_request_then_eof )
  ; ( "after the write side closed: in-flight body, no new request"
    , `Quick
    , test_after_write_closed )
  ; ( "pipelined request not dispatched after the write side closed"
    , `Quick
    , test_pipelined_request_not_dispatched_after_write_closed )
  ; ( "streaming response when the write fails"
    , `Quick
    , test_streaming_response_when_write_fails )
  ; ( "error handler response function called twice"
    , `Quick
    , test_error_handler_respond_twice )
  ; ( "error handler flushes then raises"
    , `Quick
    , test_error_handler_flush_then_raise )
  ; "bad request reasons", `Quick, test_bad_request_reasons
  ; ( "default error handler does not send the reason"
    , `Quick
    , test_default_error_handler_hides_reason )
  ; ( "report_exn while the error handler has not responded"
    , `Quick
    , test_report_exn_while_error_handler_pending )
  ; "tunnel response after EOF", `Quick, test_tunnel_after_eof
  ; ( "respond_with_upgrade failures name it"
    , `Quick
    , test_upgrade_failure_messages )
  ; "request body: one final ending", `Quick, test_request_body_endings
  ; ( "upgrade wins over Connection: close"
    , `Quick
    , test_upgrade_wins_over_connection_close )
  ; ( "flush answered `Closed when the write fails"
    , `Quick
    , test_flush_closed_when_write_fails )
  ; ( "flush answered `Closed on report_exn"
    , `Quick
    , test_flush_closed_on_report_exn )
  ; "respond on a closed writer", `Quick, test_respond_on_closed_writer
  ; ( "pipelined requests wait for pending output"
    , `Quick
    , test_pipelined_output_backpressure )
  ; ( "respond_with_upgrade to CONNECT: status and checks"
    , `Quick
    , test_connect_upgrade_status )
  ; ( "2xx to CONNECT without upgrade closes"
    , `Quick
    , test_connect_2xx_without_upgrade_closes )
  ; "pipelined request handler raises", `Quick, test_pipelined_handler_exception
  ; ( "body delivered after a closing response"
    , `Quick
    , test_body_delivered_after_closing_response )
  ; ( "unread body after the response has a timeout"
    , `Quick
    , test_unread_body_after_response_is_timed )
  ; ( "no body timeout before the response"
    , `Quick
    , test_body_timeout_only_after_response )
  ]

(* https://httpwg.org/specs/rfc9110.html For example, a CONNECT request (Section
   9.3.6) or a request with the Upgrade header field (Section 7.8) can occur at
   any time, not just in the first message on a connection. todo(anmonteiro):
   add test for this *)
