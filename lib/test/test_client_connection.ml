open Ache
open Helpers
open Client_connection

let response_error_pp_hum fmt = function
  | `Malformed_response str -> Format.fprintf fmt "Malformed_response: %s" str
  | `Invalid_response_body_length resp ->
    Format.fprintf
      fmt
      "Invalid_response_body_length: %s"
      (response_to_string resp)
  | `Connection_closed -> Format.pp_print_string fmt "Connection_closed"
  | `Exn exn -> Format.fprintf fmt "Exn (%s)" (Printexc.to_string exn)

module Response = struct
  include Response

  let pp = pp_hum
  let equal x y = x = y
end

module Alcotest = struct
  include Alcotest

  let response_error = of_pp response_error_pp_hum
end

let feed_string t str =
  let len = String.length str in
  let input = Ache.Bigstring.of_string str ~off:0 ~len in
  read t input ~off:0 ~len

let read_string t str =
  let c = feed_string t str in
  Alcotest.(check int) "read consumes all input" (String.length str) c

let read_response t r =
  let response_string = response_to_string r in
  read_string t response_string

let reader_ready ?(msg = "Reader is ready") t =
  Alcotest.check
    read_operation
    msg
    `Read
    (next_read_operation t :> [ `Close | `Read | `Yield ])

let reader_yielded ?(msg = "Reader is in a yield state") t =
  Alcotest.check read_operation msg `Yield (next_read_operation t)

let reader_closed t =
  Alcotest.check
    read_operation
    "Reader is closed"
    `Close
    (next_read_operation t :> [ `Close | `Read | `Yield ])

let write_string ?(msg = "output written") t str =
  let len = String.length str in
  Alcotest.(check (option string))
    msg
    (Some str)
    (next_write_operation t |> Write_operation.to_write_as_string);
  report_write_result t (`Ok len)

let write_request ?(msg = "request written") t r =
  let request_string = request_to_string r in
  write_string ~msg t request_string

let writer_yielded t =
  Alcotest.check
    write_operation
    "Writer is in a yield state"
    `Yield
    (next_write_operation t)

let writer_closed t =
  Alcotest.check
    write_operation
    "Writer is closed"
    (`Close 0)
    (next_write_operation t)

let connection_is_shutdown t =
  Alcotest.(check bool) "connection is shutdown" true (is_closed t)

let default_response_handler
      ?(on_error = Helpers.body_must_complete __LOC__)
      expected_response
      response
      body
  =
  Alcotest.check
    (module Response)
    "expected response"
    expected_response
    response;
  let on_read _ ~off:_ ~len:_ = () in
  let on_eof () = () in
  Body.Reader.schedule_read body ~on_read ~on_eof ~on_error

let no_error_handler _ = assert false
let pipelining = { Config.default with max_pipelined_requests = 16 }

let test_commit_parse_after_every_header () =
  let request' = Request.create `GET "/" in
  let response =
    Response.create
      ~headers:
        (Headers.of_list
           [ "Links", "/path/to/some/website"
           ; "Links", "/path/to/some/website"
           ; "connection", "close"
           ])
      `OK
  in

  let t = create () in
  let body =
    request
      t
      request'
      ~flush_headers_immediately:true
      ~response_handler:(default_response_handler response)
      ~error_handler:no_error_handler
  in
  Body.Writer.close body;
  write_request t request';

  let response_line = "HTTP/1.1 200 OK\r\n" in
  let single_header = "Links: /path/to/some/website\r\n" in
  let r =
    (* Each header is 30 bytes *)
    response_line ^ single_header ^ single_header ^ "connection: close\r\n\r\n"
  in
  let bs = Ache.Bigstring.of_string r ~off:0 ~len:(String.length r) in
  let c = read t bs ~off:0 ~len:(String.length response_line + 15) in
  Alcotest.(check int)
    "an incomplete head consumes all input"
    (String.length response_line + 15)
    c;

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

  connection_is_shutdown t

let test_get () =
  let request' = Request.create `GET "/" in
  let response = Response.create `OK in

  (* Single GET *)
  let t = create () in
  let body =
    request
      t
      request'
      ~flush_headers_immediately:true
      ~response_handler:(default_response_handler response)
      ~error_handler:no_error_handler
  in
  Body.Writer.close body;
  write_request t request';
  read_response t response;

  (* Single GET, request closes the connection. *)
  let request_close =
    Request.create ~headers:(Headers.of_list [ "connection", "close" ]) `GET "/"
  in
  let t = create () in
  let body =
    request
      t
      request_close
      ~flush_headers_immediately:true
      ~response_handler:(default_response_handler response)
      ~error_handler:no_error_handler
  in
  Body.Writer.close body;
  write_request t request_close;
  writer_closed t;
  read_response t response;

  (* Single GET, response closes connection *)
  let response = Response.create `OK ~headers:Headers.connection_close in
  let t = create () in
  let body =
    request
      t
      request'
      ~flush_headers_immediately:true
      ~response_handler:(default_response_handler response)
      ~error_handler:no_error_handler
  in
  Body.Writer.close body;
  write_request t request';
  read_response t response;
  writer_closed t;
  let c = read_eof t Ache.Bigstring.empty ~off:0 ~len:0 in
  Alcotest.(check int) "read_eof with no input returns 0" 0 c;
  connection_is_shutdown t;

  (* Single GET, streaming body *)
  let response = Response.create `OK ~headers:Headers.encoding_chunked in
  let t = create () in
  let body =
    request
      t
      request'
      ~flush_headers_immediately:true
      ~response_handler:(default_response_handler response)
      ~error_handler:no_error_handler
  in
  Body.Writer.close body;
  write_request t request';
  read_response t response;
  read_string t "d\r\nHello, world!\r\n0\r\n\r\n"

let test_head () =
  let error_handler_called = ref false in
  let request' = Request.create `HEAD "/" in
  let response =
    Response.create ~headers:(Headers.of_list [ "content-length", "5" ]) `OK
  in

  let t = create () in
  let body =
    request
      t
      request'
      ~flush_headers_immediately:true
      ~response_handler:(default_response_handler response)
      ~error_handler:(fun _ -> error_handler_called := true)
  in
  Body.Writer.close body;
  write_request t request';
  read_response t response;
  let c = read_eof t Ache.Bigstring.empty ~off:0 ~len:0 in
  Alcotest.(check int) "read_eof with no input returns 0" 0 c;
  connection_is_shutdown t;
  Alcotest.(check bool) "error handler not called" false !error_handler_called

let test_get_last_close () =
  (* Multiple GET requests, the last one closes the connection *)
  let request' = Request.create `GET "/" in
  let response =
    Response.create ~headers:(Headers.of_list [ "content-length", "0" ]) `OK
  in
  let t = create () in
  let body =
    request
      t
      request'
      ~flush_headers_immediately:true
      ~response_handler:(default_response_handler response)
      ~error_handler:no_error_handler
  in
  Body.Writer.close body;
  write_request t request';
  read_response t response;

  let request'' =
    Request.create ~headers:(Headers.of_list [ "connection", "close" ]) `GET "/"
  in
  let body' =
    request
      t
      request''
      ~flush_headers_immediately:true
      ~response_handler:(default_response_handler response)
      ~error_handler:no_error_handler
  in
  Body.Writer.close body';
  write_request t request'';
  read_response t response;

  writer_closed t;
  let c = read_eof t Ache.Bigstring.empty ~off:0 ~len:0 in
  Alcotest.(check int) "read_eof with no input returns 0" 0 c;
  connection_is_shutdown t

let test_send_streaming_body () =
  let request' = Request.create `GET "/" ~headers:Headers.encoding_chunked in
  let response = Response.create `OK ~headers:Headers.encoding_chunked in
  let t = create () in
  let body =
    request
      t
      request'
      ~flush_headers_immediately:true
      ~response_handler:(default_response_handler response)
      ~error_handler:no_error_handler
  in
  write_request t request';
  read_response t response;
  Body.Writer.write_string body "hello";
  write_string t "5\r\nhello\r\n";
  Body.Writer.write_string body "world";
  Body.Writer.close body;
  write_string t "5\r\nworld\r\n0\r\n\r\n";
  writer_yielded t

let test_response_eof () =
  let request' = Request.create `GET "/" in
  let response = Response.create `OK in
  (* not actually writen to the channel *)

  let error_message = ref None in
  let t = create () in
  let body =
    request
      t
      request'
      ~flush_headers_immediately:true
      ~response_handler:(default_response_handler response)
      ~error_handler:(function
      | `Connection_closed -> error_message := Some "connection closed"
      | _ -> assert false)
  in
  Body.Writer.close body;
  write_request t request';
  reader_ready t;
  let c = read_eof t Ache.Bigstring.empty ~off:0 ~len:0 in
  Alcotest.(check int) "read_eof with no input returns 0" 0 c;
  connection_is_shutdown t;
  Alcotest.(check (option string))
    "connection closed before the response"
    (Some "connection closed")
    !error_message

let test_persistent_connection_requests () =
  let request' = Request.create `GET "/" in
  let response =
    Response.create ~headers:(Headers.of_list [ "content-length", "0" ]) `OK
  in
  let t = create () in
  let body =
    request
      t
      request'
      ~flush_headers_immediately:true
      ~response_handler:(default_response_handler response)
      ~error_handler:no_error_handler
  in
  Body.Writer.close body;
  write_request t request';
  read_response t response;
  writer_yielded t;
  reader_ready t;
  let body' =
    request
      t
      request'
      ~flush_headers_immediately:true
      ~response_handler:(default_response_handler response)
      ~error_handler:no_error_handler
  in
  Body.Writer.close body';
  write_request t request';
  read_response t response

let test_persistent_connection_requests_pipelining () =
  let request' = Request.create `GET "/" in
  let response =
    Response.create ~headers:(Headers.of_list [ "content-length", "0" ]) `OK
  in
  let t = create ~config:pipelining () in
  let body =
    request
      t
      request'
      ~flush_headers_immediately:true
      ~response_handler:(default_response_handler response)
      ~error_handler:no_error_handler
  in
  Body.Writer.close body;
  write_request t request';
  (* send the 2nd request without reading the response *)
  let response' =
    Response.create
      ~headers:(Headers.of_list [ "content-length", "0" ])
      `Not_found
  in
  let body' =
    request
      t
      request'
      ~flush_headers_immediately:true
      ~response_handler:(fun response body ->
        default_response_handler response' response body)
      ~error_handler:no_error_handler
  in
  Body.Writer.close body';
  write_request t request';
  read_response t response;
  read_response t response'

let test_persistent_connection_requests_pipelining_send_body () =
  let request' =
    Request.create ~headers:(Headers.of_list [ "content-length", "8" ]) `GET "/"
  in
  let response =
    Response.create ~headers:(Headers.of_list [ "content-length", "0" ]) `OK
  in
  let t = create ~config:pipelining () in
  let body =
    request
      t
      request'
      ~flush_headers_immediately:true
      ~response_handler:(default_response_handler response)
      ~error_handler:no_error_handler
  in
  write_request t request';
  (* send the 2nd request without reading the response *)
  let request'' = Request.create `GET "/" in
  let response' =
    Response.create
      ~headers:(Headers.of_list [ "content-length", "0" ])
      `Not_found
  in
  let body' =
    request
      t
      request''
      ~flush_headers_immediately:true
      ~response_handler:(fun response body ->
        default_response_handler response' response body)
      ~error_handler:no_error_handler
  in
  Body.Writer.close body';
  Body.Writer.write_string body "a string";
  Body.Writer.close body;
  write_string ~msg:"writes the body for the first request" t "a string";
  write_request t request'';
  read_response t response;
  read_response t response'

let test_persistent_connection_requests_body () =
  let request' = Request.create `GET "/" in
  let request'' = Request.create `GET "/second" in
  let response =
    Response.create ~headers:(Headers.of_list [ "content-length", "10" ]) `OK
  in
  let t = create () in
  let body =
    request
      t
      request'
      ~flush_headers_immediately:true
      ~response_handler:(default_response_handler response)
      ~error_handler:no_error_handler
  in
  Body.Writer.close body;
  write_request t request';
  let response' = Response.create `OK in
  read_response t response;
  read_string t "ten chars.";
  let body' =
    request
      t
      request''
      ~flush_headers_immediately:true
      ~response_handler:(default_response_handler response')
      ~error_handler:no_error_handler
  in
  Body.Writer.close body';
  write_request t request'';
  read_response t response'

let test_response_header_order () =
  let request' = Request.create `GET "/" in
  let headers = [ "a", "1"; "b", "2"; "c", "3" ] in
  let response = Response.create `OK ~headers:(Headers.of_list headers) in
  let received = ref None in
  let t = create () in
  let body =
    request
      t
      request'
      ~flush_headers_immediately:true
      ~response_handler:(fun response _ -> received := Some response)
      ~error_handler:no_error_handler
  in
  Body.Writer.close body;
  write_request t request';
  writer_yielded t;
  read_response t response;
  match !received with
  | None -> assert false
  | Some received ->
    Alcotest.(check (list (pair string string)))
      "headers are equal"
      headers
      (Headers.to_list received.headers)

let test_report_exn () =
  let request' = Request.create `GET "/" in
  let response = Response.create `OK in
  (* not actually writen to the channel *)

  let error_message = ref None in
  let t = create () in
  let body =
    request
      t
      request'
      ~flush_headers_immediately:true
      ~response_handler:(default_response_handler response)
      ~error_handler:(function
      | `Exn (Failure msg) -> error_message := Some msg
      | _ -> assert false)
  in
  Body.Writer.close body;
  write_request t request';
  writer_yielded t;
  reader_ready t;
  report_exn t (Failure "something went wrong");
  connection_is_shutdown t;
  Alcotest.(check (option string))
    "something went wrong"
    (Some "something went wrong")
    !error_message

let test_input_shrunk () =
  let request' = Request.create `GET "/" in
  let response = Response.create `OK in
  (* not actually writen to the channel *)

  let error_message = ref None in
  let t = create () in
  let body =
    request
      t
      request'
      ~flush_headers_immediately:true
      ~response_handler:(default_response_handler response)
      ~error_handler:(function
      | `Exn (Failure msg) -> error_message := Some msg
      | _ -> assert false)
  in
  Body.Writer.close body;
  write_request t request';
  writer_yielded t;
  reader_ready t;
  let c = feed_string t "HTTP/1.1 200 OK\r\nDate" in
  Alcotest.(check int) "read the incomplete head" 21 c;
  report_exn t (Failure "something went wrong");
  connection_is_shutdown t;
  Alcotest.(check (option string))
    "something went wrong"
    (Some "something went wrong")
    !error_message

let test_partial_input () =
  let request' = Request.create `GET "/" in
  let response_handler response response_body =
    Alcotest.(check (list (pair string string)))
      "got expected headers"
      [ "Connection", "close" ]
      (Headers.to_rev_list response.Response.headers);
    Body.Reader.close response_body
  in
  let t = create () in
  let body =
    request
      t
      request'
      ~flush_headers_immediately:true
      ~response_handler
      ~error_handler:no_error_handler
  in
  write_request t request';
  writer_yielded t;
  Body.Writer.close body;
  reader_ready t;
  let len = feed_string t "HTTP/1.1 200 OK\r\nC" in
  Alcotest.(check int) "partial read" 18 len;
  read_string t "onnection: close\r\n\r\n";
  let c = read_eof t Ache.Bigstring.empty ~off:0 ~len:0 in
  Alcotest.(check int) "read_eof with no input returns 0" 0 c;
  shutdown t;
  reader_closed t;
  writer_closed t

let test_empty_fixed_body () =
  let request' =
    Request.create ~headers:(Headers.of_list [ "Connection", "close" ]) `GET "/"
  in
  let response_handler response response_body =
    Alcotest.(check (list (pair string string)))
      "got expected headers"
      [ "Connection", "close" ]
      (Headers.to_rev_list response.Response.headers);
    Body.Reader.close response_body
  in
  let t = create () in
  let (_body : Body.Writer.t) =
    request
      t
      request'
      ~flush_headers_immediately:true
      ~response_handler
      ~error_handler:no_error_handler
  in
  write_request t request';
  writer_closed t;
  reader_ready t;
  read_response
    t
    (Response.create ~headers:(Headers.of_list [ "Connection", "close" ]) `OK);
  let c = read_eof t Ache.Bigstring.empty ~off:0 ~len:0 in
  Alcotest.(check int) "read_eof with no input returns 0" 0 c;
  reader_closed t

let test_fixed_body () =
  let request' =
    Request.create
      ~headers:
        (Headers.of_list [ "Connection", "close"; "Content-Length", "3" ])
      `GET
      "/"
  in
  let response_handler response response_body =
    Alcotest.(check (list (pair string string)))
      "got expected headers"
      [ "Connection", "close" ]
      (Headers.to_rev_list response.Response.headers);
    Body.Reader.close response_body
  in
  let t = create () in
  let body =
    request
      t
      request'
      ~flush_headers_immediately:true
      ~response_handler
      ~error_handler:no_error_handler
  in
  write_request t request';
  writer_yielded t;
  Body.Writer.write_string body "foo";
  write_string t "foo";
  writer_yielded t;
  Body.Writer.close body;
  reader_ready t;
  read_response
    t
    (Response.create ~headers:(Headers.of_list [ "Connection", "close" ]) `OK);
  let c = read_eof t Ache.Bigstring.empty ~off:0 ~len:0 in
  Alcotest.(check int) "read_eof with no input returns 0" 0 c;
  reader_closed t;
  writer_closed t

let test_fixed_body_persistent_connection () =
  let writer_woken_up = ref false in
  let request' =
    Request.create ~headers:(Headers.of_list [ "Content-Length", "2" ]) `GET "/"
  in
  let response_handler response response_body =
    Alcotest.(check (list (pair string string)))
      "got expected headers"
      []
      (Headers.to_rev_list response.Response.headers);
    Body.Reader.close response_body
  in
  let t = create () in
  let body =
    request
      t
      request'
      ~flush_headers_immediately:true
      ~response_handler
      ~error_handler:no_error_handler
  in
  write_request t request';
  writer_yielded t;
  yield_writer t (fun () -> writer_woken_up := true);
  Body.Writer.write_string body "hi";
  Body.Writer.close body;
  Alcotest.(check bool) "Writer woken up" true !writer_woken_up;
  write_string t "hi";
  writer_yielded t;
  writer_woken_up := false;
  yield_writer t (fun () -> writer_woken_up := true);
  Alcotest.(check bool)
    "Writer doesn't wake up immediately"
    false
    !writer_woken_up;
  reader_ready t;
  read_response t (Response.create ~headers:(Headers.of_list []) `OK);
  reader_ready t;
  Alcotest.(check bool) "Writer woken up" true !writer_woken_up;
  writer_yielded t

let test_empty_fixed_body_persistent_connection () =
  let request' =
    Request.create ~headers:(Headers.of_list [ "Content-Length", "0" ]) `GET "/"
  in
  let response_handler response response_body =
    Alcotest.(check (list (pair string string)))
      "got expected headers"
      []
      (Headers.to_rev_list response.Response.headers);
    Body.Reader.close response_body
  in
  let t = create () in
  let body =
    request
      t
      request'
      ~flush_headers_immediately:true
      ~response_handler
      ~error_handler:no_error_handler
  in
  write_request t request';
  writer_yielded t;
  Body.Writer.close body;
  reader_ready t;
  read_response t (Response.create ~headers:(Headers.of_list []) `OK);
  reader_ready t;
  writer_yielded t

let test_client_upgrade () =
  let writer_woken_up = ref false in
  let reader_woken_up = ref false in
  let request' =
    Request.create ~headers:(Headers.of_list [ "Content-Length", "0" ]) `GET "/"
  in
  let t = create () in
  let response = Response.create `Switching_protocols in
  let body =
    request
      t
      request'
      ~flush_headers_immediately:true
      ~response_handler:(default_response_handler response)
      ~error_handler:no_error_handler
  in
  write_request t request';
  writer_yielded t;
  Body.Writer.close body;
  reader_ready t;
  read_response t response;
  reader_yielded t;
  yield_reader t (fun () -> reader_woken_up := true);
  writer_yielded t;
  yield_writer t (fun () -> writer_woken_up := true);
  Alcotest.(check bool) "Reader hasn't woken up yet" false !reader_woken_up;
  Alcotest.(check bool) "Writer hasn't woken up yet" false !writer_woken_up;
  shutdown t;
  Alcotest.(check bool) "Reader woken up" true !reader_woken_up;
  Alcotest.(check bool) "Writer woken up" true !writer_woken_up;
  connection_is_shutdown t

let backpressure_response_handler
      continue_reading
      expected_response
      response
      body
  =
  Alcotest.check
    (module Response)
    "expected response"
    expected_response
    response;
  let rec on_read _buffer ~off:_ ~len:_ =
    continue_reading :=
      fun () ->
        Body.Reader.schedule_read
          ~on_error:(Helpers.body_must_complete __LOC__)
          body
          ~on_eof
          ~on_read
  and on_eof () = print_endline "got eof" in
  Body.Reader.schedule_read
    ~on_error:(Helpers.body_must_complete __LOC__)
    body
    ~on_eof
    ~on_read

let test_handling_backpressure_when_read_not_scheduled () =
  let writer_woken_up = ref false in
  let reader_woken_up = ref false in
  let continue_reading = ref (fun () -> ()) in
  let request' = Request.create `GET "/" in
  let response =
    Response.create ~headers:(Headers.of_list [ "content-length", "10" ]) `OK
  in
  let t = create () in
  let body =
    request
      t
      request'
      ~flush_headers_immediately:true
      ~response_handler:
        (backpressure_response_handler continue_reading response)
      ~error_handler:no_error_handler
  in
  write_request t request';
  writer_yielded t;
  Body.Writer.close body;
  reader_ready t;
  read_response t response;
  yield_writer t (fun () -> writer_woken_up := true);
  read_string t "five.";
  reader_yielded t;
  yield_reader t (fun () -> reader_woken_up := true);
  !continue_reading ();
  reader_ready
    ~msg:"Reader wants to read if there's a read scheduled in the body"
    t;
  Alcotest.(check bool)
    "Reader wakes up if scheduling read"
    true
    !reader_woken_up;
  Alcotest.(check bool) "Writer not woken up" false !writer_woken_up

let test_handling_backpressure_when_read_not_scheduled_early_yield () =
  let writer_woken_up = ref false in
  let reader_woken_up = ref false in
  let continue_reading = ref (fun () -> ()) in
  let request' = Request.create `GET "/" in
  let response =
    Response.create ~headers:(Headers.of_list [ "content-length", "10" ]) `OK
  in
  let t = create () in
  let body =
    request
      t
      request'
      ~flush_headers_immediately:true
      ~response_handler:
        (backpressure_response_handler continue_reading response)
      ~error_handler:no_error_handler
  in
  write_request t request';
  writer_yielded t;
  Body.Writer.close body;
  reader_ready t;
  read_response t response;
  yield_reader t (fun () -> reader_woken_up := true);
  yield_writer t (fun () -> writer_woken_up := true);
  read_string t "five.";
  reader_yielded t;
  !continue_reading ();
  reader_ready
    ~msg:"Reader wants to read if there's a read scheduled in the body"
    t;
  Alcotest.(check bool)
    "Reader wakes up if scheduling read"
    true
    !reader_woken_up;
  Alcotest.(check bool) "Writer not woken up" false !writer_woken_up

let test_eof_with_another_pipelined_request () =
  let reader_woken_up = ref false in
  let continue_reading = ref (fun () -> ()) in
  let request' = Request.create `GET "/" in
  let response =
    Response.create ~headers:(Headers.of_list [ "content-length", "10" ]) `OK
  in
  let t = create () in
  let response_handler continue_reading expected_response response body =
    Alcotest.check
      (module Response)
      "expected response"
      expected_response
      response;
    let on_read _buffer ~off:_ ~len:_ =
      continue_reading := fun () -> Body.Reader.close body
    and on_eof () = print_endline "got eof" in
    Body.Reader.schedule_read
      ~on_error:(Helpers.body_must_complete __LOC__)
      body
      ~on_eof
      ~on_read
  in
  let body =
    request
      t
      request'
      ~flush_headers_immediately:true
      ~response_handler:(response_handler continue_reading response)
      ~error_handler:no_error_handler
  in
  write_request t request';
  writer_yielded t;
  Body.Writer.close body;
  reader_ready t;
  read_response t response;
  yield_reader t (fun () -> reader_woken_up := true);
  yield_writer t ignore;
  read_string t "five.";
  reader_yielded t;
  !continue_reading ();
  reader_ready
    ~msg:"Reader wants to read if there's a read scheduled in the body"
    t;
  Alcotest.(check bool)
    "Reader wakes up if scheduling read"
    true
    !reader_woken_up;
  writer_yielded t;

  (* Pipeline the 2nd request. *)
  let expected_response = Response.create `OK in
  let error_handler_called = ref false in
  let body' =
    request
      t
      request'
      ~flush_headers_immediately:true
      ~response_handler:(fun response _ ->
        Alcotest.check
          (module Response)
          "expected response"
          expected_response
          response)
      ~error_handler:(fun _ -> error_handler_called := true)
  in
  Body.Writer.close body';
  write_request t request';
  writer_yielded t;

  let str = "rest." in
  let len = String.length str in
  let input = Ache.Bigstring.of_string str ~off:0 ~len in
  let just_read = read_eof t input ~off:0 ~len in
  Alcotest.(check int)
    "Rest of the response read completely, server EOF"
    len
    just_read;
  reader_closed t;

  Alcotest.(check bool)
    "Error handler called on the 2nd request"
    true
    !error_handler_called;
  connection_is_shutdown t

(* If we haven't finished receiving the full response, the error handler *
   should be called. *)
let test_eof_handler_response_body_not_closed () =
  let request' = Request.create `GET "/" in
  let response =
    Response.create ~headers:(Headers.of_list [ "content-length", "10" ]) `OK
  in
  let t = create () in
  let response_handler expected_response response body =
    Alcotest.check
      (module Response)
      "expected response"
      expected_response
      response;
    let on_read _buffer ~off:_ ~len:_ = () in
    let on_eof () = print_endline "got eof" in
    Body.Reader.schedule_read
      ~on_error:(Helpers.body_must_complete __LOC__)
      body
      ~on_eof
      ~on_read
  in
  let error_handler_called = ref false in
  let body =
    request
      t
      request'
      ~flush_headers_immediately:true
      ~response_handler:(response_handler response)
      ~error_handler:(fun _ -> error_handler_called := true)
  in
  write_request t request';
  writer_yielded t;
  Body.Writer.close body;
  reader_ready t;
  read_response t response;
  yield_writer t ignore;

  reader_ready
    ~msg:"Reader wants to read if there's a read scheduled in the body"
    t;

  let str = "rest." in
  let len = String.length str in
  let input = Ache.Bigstring.of_string str ~off:0 ~len in
  let just_read = read_eof t input ~off:0 ~len in
  Alcotest.(check int)
    "server EOF after delivering the first part of the response"
    len
    just_read;
  reader_closed t;

  Alcotest.(check bool) "Error handler called" true !error_handler_called;
  connection_is_shutdown t

let test_eof_handler_closed_response_body () =
  let continue_reading = ref (fun () -> ()) in
  let request' = Request.create `GET "/" in
  let response =
    Response.create ~headers:(Headers.of_list [ "content-length", "10" ]) `OK
  in
  let t = create () in
  let response_handler continue_reading expected_response response body =
    Alcotest.check
      (module Response)
      "expected response"
      expected_response
      response;
    let on_read _buffer ~off:_ ~len:_ =
      continue_reading := fun () -> Body.Reader.close body
    and on_eof () = print_endline "got eof" in
    Body.Reader.schedule_read
      ~on_error:(Helpers.body_must_complete __LOC__)
      body
      ~on_eof
      ~on_read
  in
  let error_handler_called = ref false in
  let body =
    request
      t
      request'
      ~flush_headers_immediately:true
      ~response_handler:(response_handler continue_reading response)
      ~error_handler:(fun _ -> error_handler_called := true)
  in
  write_request t request';
  writer_yielded t;
  Body.Writer.close body;
  reader_ready t;
  read_response t response;
  yield_writer t ignore;

  !continue_reading ();
  reader_ready
    ~msg:"Reader wants to read if there's a read scheduled in the body"
    t;

  let str = "rest." in
  let len = String.length str in
  let input = Ache.Bigstring.of_string str ~off:0 ~len in
  let just_read = read_eof t input ~off:0 ~len in
  Alcotest.(check int)
    "Rest of the response read completely, server EOF"
    len
    just_read;
  reader_closed t;

  Alcotest.(check bool) "Error handler called" true !error_handler_called;
  connection_is_shutdown t

let test_exception_closes_reader () =
  let reader_woken_up = ref false in
  let request' = Request.create `GET "/" in
  let error_handler_called = ref false in
  let response =
    Response.create
      ~headers:
        (Headers.of_list [ "connection", "close"; "content-length", "10" ])
      `OK
  in
  let t = create () in
  let body =
    request
      t
      request'
      ~flush_headers_immediately:true
      ~response_handler:(default_response_handler response)
      ~error_handler:(fun _ -> error_handler_called := true)
  in
  Body.Writer.close body;
  write_request t request';
  read_response t response;
  writer_closed t;
  read_string t "hello";
  reader_yielded t ~msg:"Reader yields if no read scheduled";
  yield_reader t (fun () -> reader_woken_up := true);
  report_exn t (Failure "something went wrong");
  Alcotest.(check bool) "Error handler called" true !error_handler_called;
  Alcotest.(check bool)
    "Reader wakes up if scheduling read"
    true
    !reader_woken_up;
  connection_is_shutdown t

let test_exception_closes_reader_persistent_connection () =
  let reader_woken_up = ref false in
  let writer_woken_up = ref false in
  let request' = Request.create `GET "/" in
  let error_handler_called = ref false in
  let response =
    Response.create ~headers:(Headers.of_list [ "content-length", "10" ]) `OK
  in
  let t = create () in
  let body =
    request
      t
      request'
      ~flush_headers_immediately:true
      ~response_handler:(default_response_handler response)
      ~error_handler:(fun _ -> error_handler_called := true)
  in
  Body.Writer.close body;
  write_request t request';
  read_response t response;
  writer_yielded t;
  yield_writer t (fun () -> writer_woken_up := true);
  read_string t "hello";
  reader_yielded t ~msg:"Reader yields if no read scheduled";
  yield_reader t (fun () -> reader_woken_up := true);
  report_exn t (Failure "something went wrong");
  Alcotest.(check bool) "Error handler called" true !error_handler_called;
  Alcotest.(check bool)
    "Reader wakes up if scheduling read"
    true
    !reader_woken_up;
  Alcotest.(check bool) "Writer woken up" true !writer_woken_up;
  connection_is_shutdown t

let test_exception_reading_response_body () =
  let request' = Request.create `GET "/" in
  let error_handler_called = ref false in
  let response =
    Response.create ~headers:(Headers.of_list [ "content-length", "10" ]) `OK
  in
  let t = create () in
  let response_handler expected_response response body =
    Alcotest.check
      (module Response)
      "expected response"
      expected_response
      response;
    let on_read _ ~off:_ ~len:_ = failwith "something went wrong" in
    let on_eof () = () in
    Body.Reader.schedule_read
      ~on_error:(Helpers.body_must_complete __LOC__)
      body
      ~on_read
      ~on_eof
  in
  let body =
    request
      t
      request'
      ~flush_headers_immediately:true
      ~response_handler:(response_handler response)
      ~error_handler:(fun _ -> error_handler_called := true)
  in
  Body.Writer.close body;
  write_request t request';
  read_response t response;
  read_string t "hello";
  Alcotest.(check bool) "Error handler called" true !error_handler_called;
  writer_closed t;
  reader_closed t;
  connection_is_shutdown t

let test_exception_reading_response_body_last_chunk () =
  let writer_woken_up = ref false in
  let request' = Request.create `GET "/" in
  let error_handler_called = ref false in
  let response =
    Response.create ~headers:(Headers.of_list [ "content-length", "10" ]) `OK
  in
  let t = create () in
  let response_handler expected_response response body =
    Alcotest.check
      (module Response)
      "expected response"
      expected_response
      response;
    let on_eof () = () in
    let on_read _ ~off:_ ~len:_ =
      Body.Reader.schedule_read
        ~on_error:(Helpers.body_must_complete __LOC__)
        body
        ~on_read:(fun _ ~off:_ ~len:_ ->
          report_exn t (Failure "something went wrong"))
        ~on_eof
    in
    Body.Reader.schedule_read
      ~on_error:(Helpers.body_must_complete __LOC__)
      body
      ~on_read
      ~on_eof
  in
  let body =
    request
      t
      request'
      ~flush_headers_immediately:true
      ~response_handler:(response_handler response)
      ~error_handler:(fun _ -> error_handler_called := true)
  in
  Body.Writer.close body;
  write_request t request';
  read_response t response;
  writer_yielded t;
  yield_writer t (fun () -> writer_woken_up := true);
  read_string t "hello";
  reader_ready t;
  read_string t "hello";
  Alcotest.(check bool) "Error handler called" true !error_handler_called;
  Alcotest.(check bool) "Writer woken up" true !writer_woken_up;
  writer_closed t;
  reader_closed t;
  connection_is_shutdown t

let test_async_exception_reading_response_body () =
  let reader_woken_up = ref false in
  let writer_woken_up = ref false in
  let continue_reading = ref (fun () -> ()) in
  let request' = Request.create `GET "/" in
  let error_handler_called = ref false in
  let response =
    Response.create ~headers:(Headers.of_list [ "content-length", "10" ]) `OK
  in
  let t = create () in
  let response_handler expected_response response body =
    Alcotest.check
      (module Response)
      "expected response"
      expected_response
      response;
    let on_read _ ~off:_ ~len:_ =
      continue_reading :=
        fun () -> report_exn t (Failure "something went wrong")
    in
    let on_eof () = () in
    Body.Reader.schedule_read
      ~on_error:(Helpers.body_must_complete __LOC__)
      body
      ~on_read
      ~on_eof
  in
  let body =
    request
      t
      request'
      ~flush_headers_immediately:true
      ~response_handler:(response_handler response)
      ~error_handler:(fun _ -> error_handler_called := true)
  in
  Body.Writer.close body;
  write_request t request';
  read_response t response;
  read_string t "hello";
  reader_yielded t;
  yield_reader t (fun () -> reader_woken_up := true);
  writer_yielded t;
  yield_writer t (fun () -> writer_woken_up := true);
  !continue_reading ();
  Alcotest.(check bool) "Error handler called" true !error_handler_called;
  Alcotest.(check bool)
    "Reader wakes up if scheduling read"
    true
    !reader_woken_up;
  Alcotest.(check bool) "Writer woken up" true !writer_woken_up;
  writer_closed t;
  reader_closed t;
  connection_is_shutdown t

let test_failed_response_parse () =
  let writer_woken_up = ref false in
  let request' = Request.create `GET "/" in

  let test response bytes_read expected_error =
    let error = ref None in
    let t = create () in
    let body =
      request
        t
        request'
        ~flush_headers_immediately:true
        ~response_handler:(fun _ _ -> assert false)
        ~error_handler:(fun e -> error := Some e)
    in
    Body.Writer.close body;
    write_request t request';
    writer_yielded t;
    yield_writer t (fun () -> writer_woken_up := true);
    reader_ready t;
    let len = feed_string t response in
    Alcotest.(check int) "bytes read" len bytes_read;
    reader_closed t;
    Alcotest.(check bool) "Writer woken up" true !writer_woken_up;
    connection_is_shutdown t;
    Alcotest.(check (option response_error))
      "Response error"
      (Some expected_error)
      !error
  in

  test
    "HTTP/1.1 200\rOK\r\n\r\n"
    19
    (`Malformed_response "unexpected '\\r' in status-line");

  let response = Response.create `OK ~headers:(Headers.encoding_fixed (-1)) in
  test (response_to_string response) 39 (`Invalid_response_body_length response)

let test_shutdown_hangs_response_body_read () =
  let got_eof = ref false in
  let got_error = ref false in
  let request' = Request.create `GET "/" in
  let response =
    Response.create ~headers:(Headers.of_list [ "content-length", "10" ]) `OK
  in
  let response_handler expected_response response response_body =
    Alcotest.check
      (module Response)
      "expected response"
      expected_response
      response;
    let rec on_read _buffer ~off:_ ~len:_ =
      Body.Reader.schedule_read response_body ~on_error ~on_eof ~on_read
    and on_eof () = got_eof := true
    and on_error () = got_error := true in
    Body.Reader.schedule_read response_body ~on_error ~on_eof ~on_read
  in
  let t = create () in
  let body =
    request
      t
      request'
      ~flush_headers_immediately:true
      ~response_handler:(response_handler response)
      ~error_handler:no_error_handler
  in
  reader_ready t;
  write_request t request';
  writer_yielded t;
  Body.Writer.close body;
  reader_ready t;
  read_response t response;
  read_string t "five.";
  reader_ready
    ~msg:"Reader wants to read if there's a read scheduled in the body"
    t;
  shutdown t;
  Alcotest.(check bool)
    "EOF not delivered to an incomplete response body"
    false
    !got_eof;
  Alcotest.(check bool)
    "error delivered to the response body if the connection shuts down"
    true
    !got_error;
  connection_is_shutdown t

let test_response_arrives_before_body_uploaded () =
  let reader_woken_up = ref false in
  let eof_delivered = ref false in
  let continue_response = ref (fun () -> ()) in
  let request' =
    Request.create
      `GET
      "/"
      ~headers:(Headers.of_list [ "content-length", "10" ])
  in
  let response =
    Response.create ~headers:(Headers.of_list [ "content-length", "10" ]) `OK
  in
  let t = create () in
  let response_handler continue_response expected_response response body =
    Alcotest.check
      (module Response)
      "expected response"
      expected_response
      response;
    let on_read _buffer ~off:_ ~len:_ = ()
    and on_eof () = eof_delivered := true in
    Body.Reader.schedule_read
      ~on_error:(Helpers.body_must_complete __LOC__)
      body
      ~on_eof
      ~on_read;
    continue_response :=
      fun () ->
        Body.Reader.schedule_read
          ~on_error:(Helpers.body_must_complete __LOC__)
          body
          ~on_eof
          ~on_read;
        continue_response :=
          fun () ->
            Body.Reader.schedule_read
              ~on_error:(Helpers.body_must_complete __LOC__)
              body
              ~on_eof
              ~on_read
  in
  let error_handler_called = ref false in
  let second_error_handler_called = ref false in
  let body =
    request
      t
      request'
      ~flush_headers_immediately:true
      ~response_handler:(response_handler continue_response response)
      ~error_handler:(fun _ -> error_handler_called := true)
  in
  write_request t request';
  writer_yielded t;
  Body.Writer.write_string body "hello";
  write_string t "hello";
  writer_yielded t;
  (* Never close the request body. *)
  reader_ready t;
  read_response t response;
  yield_writer t ignore;

  reader_ready
    ~msg:"Reader wants to read if there's a read scheduled in the body"
    t;
  let str = "hello" in
  let len = String.length str in
  let input = Ache.Bigstring.of_string str ~off:0 ~len in
  let just_read = read t input ~off:0 ~len in
  Alcotest.(check int) "reads the first chunk" len just_read;
  reader_yielded t;
  yield_reader t (fun () -> reader_woken_up := true);

  (* Sneak in a 2nd request. *)
  let expected_response = Response.create `OK in
  let body' =
    request
      t
      request'
      ~flush_headers_immediately:true
      ~response_handler:(fun response _ ->
        Alcotest.check
          (module Response)
          "expected response"
          expected_response
          response)
      ~error_handler:(fun _ -> second_error_handler_called := true)
  in
  Body.Writer.close body';
  writer_yielded t;

  !continue_response ();
  Alcotest.(check bool) "Reader woken up" true !reader_woken_up;
  reader_ready t;
  reader_woken_up := false;

  let str = "hello" in
  let len = String.length str in
  let input = Ache.Bigstring.of_string str ~off:0 ~len in
  let just_read = read_eof t input ~off:0 ~len in
  Alcotest.(check int)
    "server EOF after delivering the entire response"
    len
    just_read;
  reader_closed t;

  !continue_response ();
  Alcotest.(check bool) "EOF delivered to the response body" true !eof_delivered;

  Alcotest.(check bool)
    "First error handler not called"
    false
    !error_handler_called;
  Alcotest.(check bool)
    "2nd Error handler called"
    true
    !second_error_handler_called;
  connection_is_shutdown t

let test_race_condition_writer_issues_yield_after_reader_eof () =
  let reader_woken_up = ref false in
  let request' =
    Request.create ~headers:(Headers.of_list [ "content-length", "5" ]) `GET "/"
  in
  let response =
    Response.create ~headers:(Headers.of_list [ "content-length", "10" ]) `OK
  in
  let t = create () in
  let response_handler expected_response response _body =
    Alcotest.check
      (module Response)
      "expected response"
      expected_response
      response
  in
  let body =
    request
      t
      request'
      ~flush_headers_immediately:true
      ~response_handler:(response_handler response)
      ~error_handler:(fun _ -> ())
  in
  write_request t request';
  Body.Writer.write_string body "hello";
  Body.Writer.flush body ignore;
  write_string t "hello";
  reader_ready t;
  read_response t response;
  read_string t (String.make 5 'a');
  Body.Writer.close body;
  reader_yielded t;
  yield_reader t (fun () ->
    reader_woken_up := true;
    ignore @@ read_eof t Ache.Bigstring.empty ~off:0 ~len:0;
    reader_closed t);
  writer_closed t;
  Alcotest.(check bool) "Reader woken up" true !reader_woken_up;
  connection_is_shutdown t

let test_multiple_responses_in_single_read () =
  let res_handled = ref 0 in
  let request' =
    Request.create ~headers:(Headers.of_list [ "content-length", "0" ]) `GET "/"
  in
  let response =
    Response.create ~headers:(Headers.of_list [ "content-length", "0" ]) `OK
  in
  let t = create ~config:pipelining () in
  let response_handler _response _body = res_handled := !res_handled + 1 in
  let body =
    request
      t
      request'
      ~flush_headers_immediately:true
      ~response_handler
      ~error_handler:no_error_handler
  in
  write_request t request';
  writer_yielded t;
  Body.Writer.close body;
  reader_ready t;
  let body =
    request
      t
      request'
      ~flush_headers_immediately:true
      ~response_handler
      ~error_handler:no_error_handler
  in
  write_request t request';
  writer_yielded t;
  Body.Writer.close body;
  reader_ready t;

  let responses = response_to_string response ^ response_to_string response in
  read_string t responses;
  reader_ready t;
  Alcotest.(check int) "fired handler of both requests" 2 !res_handled

let test_chunked_error () =
  let error_handler_called = ref false in
  let request' =
    Request.create
      ~headers:(Headers.of_list [ "transfer-encoding", "chunked" ])
      `GET
      "/"
  in
  let t = create () in
  let response_handler _response _body = assert false in
  let body =
    request
      t
      request'
      ~flush_headers_immediately:true
      ~response_handler
      ~error_handler:(fun _ -> error_handler_called := true)
  in
  write_request t request';
  Body.Writer.write_string body "hello";
  write_string t "5\r\nhello\r\n";
  report_exn t (Failure "something went wrong");
  Alcotest.(check bool) "First error handler called" true !error_handler_called;
  Alcotest.(check bool)
    "request body is closed"
    true
    (Body.Writer.is_closed body);
  writer_closed t;
  connection_is_shutdown t;

  error_handler_called := false;
  let t = create () in
  let response =
    Response.create ~headers:(Headers.of_list [ "content-length", "10" ]) `OK
  in
  let body_cut = ref false in
  let body =
    request
      t
      request'
      ~flush_headers_immediately:true
      ~response_handler:
        (default_response_handler
           ~on_error:(fun () -> body_cut := true)
           response)
      ~error_handler:(fun _ -> error_handler_called := true)
  in
  write_request t request';
  Body.Writer.write_string body "hello";
  Body.Writer.flush body ignore;
  write_string t "5\r\nhello\r\n";
  read_response t response;
  report_exn t (Failure "something went wrong");
  Alcotest.(check bool) "First error handler called" true !error_handler_called;
  Alcotest.(check bool) "response body reported cut" true !body_cut;
  Alcotest.(check bool)
    "request body is closed"
    true
    (Body.Writer.is_closed body);
  writer_closed t;
  connection_is_shutdown t

let test_304_not_modified () =
  let error_handler_called = ref false in
  let request' = Request.create `GET "/" in
  let response =
    Response.create
      ~headers:(Headers.of_list [ "content-length", "5" ])
      `Not_modified
  in

  let t = create () in
  let body =
    request
      t
      request'
      ~flush_headers_immediately:true
      ~response_handler:(default_response_handler response)
      ~error_handler:(fun _ -> error_handler_called := true)
  in
  Body.Writer.close body;
  write_request t request';
  read_response t response;
  let c = read_eof t Ache.Bigstring.empty ~off:0 ~len:0 in
  Alcotest.(check int) "read_eof with no input returns 0" 0 c;
  connection_is_shutdown t;
  Alcotest.(check bool) "error handler not called" false !error_handler_called

let test_empty_content_length_body_closed () =
  let response = Response.create `OK in
  let request' = Request.create `GET "/" in
  let t = create () in
  let body =
    request
      t
      request'
      ~flush_headers_immediately:true
      ~response_handler:(default_response_handler response)
      ~error_handler:no_error_handler
  in
  Alcotest.(check bool)
    "request body length is 0"
    true
    (Request.body_length request' = `Fixed 0L);
  Alcotest.(check bool)
    "Request body starts out closed"
    true
    (Body.Writer.is_closed body);
  write_request t request';
  read_response t response;
  let c = read_eof t Ache.Bigstring.empty ~off:0 ~len:0 in
  Alcotest.(check int) "read_eof with no input returns 0" 0 c;
  connection_is_shutdown t

let test_schedule_read_with_data_available () =
  let request' = Request.create `GET "/" in
  let response = Response.create `OK ~headers:(Headers.encoding_fixed 6) in
  let t = create () in

  let body = ref None in
  let response_handler response' body' =
    body := Some body';
    Alcotest.check (module Response) "expected response" response response'
  in
  let req_body =
    request
      t
      request'
      ~flush_headers_immediately:true
      ~response_handler
      ~error_handler:no_error_handler
  in
  Body.Writer.close req_body;
  write_request t request';
  writer_yielded t;
  read_response t response;

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
  read_string t "Hello";
  reader_yielded t;

  (* Schedule a read when there is already data available. on_read should be
     called straight away, as who knows how long it'll be before more data
     arrives. *)
  schedule_read "Hello";
  read_string t "!";
  schedule_read "!";
  let did_eof = ref false in
  Body.Reader.schedule_read
    ~on_error:(Helpers.body_must_complete __LOC__)
    body
    ~on_read:(fun _ ~off:_ ~len:_ -> Alcotest.fail "Expected eof")
    ~on_eof:(fun () -> did_eof := true);
  Alcotest.(check bool) "on_eof called" true !did_eof

let test_dont_flush_headers_immediately () =
  let request' =
    Request.create `GET "/" ~headers:Headers.(of_list [ "content-length", "5" ])
  in

  let response = Response.create `OK ~headers:Headers.encoding_chunked in
  let t = create () in
  let body =
    request
      t
      request'
      ~flush_headers_immediately:false
      ~response_handler:(default_response_handler response)
      ~error_handler:no_error_handler
  in

  (* Writer yields when `~flush_headers_immediately` is false *)
  writer_yielded t;

  (* Write to the body *)
  Body.Writer.write_string body "Hello";
  Body.Writer.close body;

  (* Request line + body batched together. *)
  write_string t "GET / HTTP/1.1\r\ncontent-length: 5\r\n\r\nHello";

  read_response t response;
  read_string t "d\r\nHello, world!\r\n0\r\n\r\n"

let test_pipelining_no_immediate_flush () =
  let writer_woken_up = ref false in
  let request' = Request.create `GET "/" in
  let response =
    Response.create ~headers:(Headers.of_list [ "content-length", "0" ]) `OK
  in
  let t = create ~config:pipelining () in
  let body =
    request
      t
      ~flush_headers_immediately:false
      request'
      ~response_handler:(default_response_handler response)
      ~error_handler:no_error_handler
  in

  writer_yielded t;
  yield_writer t (fun () -> writer_woken_up := true);

  Body.Writer.close body;
  Alcotest.(check bool) "Writer woken up" true !writer_woken_up;
  writer_woken_up := false;

  write_request t request';

  (* send the 2nd request without reading the response *)
  let response' =
    Response.create
      ~headers:(Headers.of_list [ "content-length", "0" ])
      `Not_found
  in
  let body' =
    request
      t
      request'
      ~flush_headers_immediately:false
      ~response_handler:(fun response body ->
        default_response_handler response' response body)
      ~error_handler:no_error_handler
  in
  writer_yielded t;
  yield_writer t (fun () -> writer_woken_up := true);

  Body.Writer.close body';
  Alcotest.(check bool) "Writer woken up" true !writer_woken_up;
  writer_woken_up := false;

  write_request t request';
  read_response t response;
  read_response t response'

let test_flush_on_close () =
  let writer_woken_up = ref false in
  let request' = Request.create `GET "/" in
  let response =
    Response.create ~headers:(Headers.of_list [ "content-length", "0" ]) `OK
  in
  let t = create () in

  writer_yielded t;
  yield_writer t (fun () -> writer_woken_up := true);

  let body =
    request
      t
      ~flush_headers_immediately:false
      request'
      ~response_handler:(default_response_handler response)
      ~error_handler:no_error_handler
  in
  Alcotest.(check bool) "Writer woken up" true !writer_woken_up;

  Body.Writer.close body;

  write_request t request';
  read_response t response

let test_request_pipelining_async () =
  let request' =
    Request.create `GET "/" ~headers:(Headers.of_list [ "content-length", "5" ])
  in
  let response =
    Response.create ~headers:(Headers.of_list [ "content-length", "5" ]) `OK
  in
  let t = create ~config:pipelining () in
  let body =
    request
      t
      request'
      ~response_handler:(default_response_handler response)
      ~error_handler:no_error_handler
  in
  Body.Writer.write_string body "hello";
  Body.Writer.close body;
  let body' =
    request
      t
      request'
      ~response_handler:(fun response body ->
        default_response_handler response response body)
      ~error_handler:no_error_handler
  in
  Body.Writer.write_string body' "hello";
  Body.Writer.close body';

  let req_string = request_to_string request' ^ "hello" in
  write_string t req_string;
  write_string t req_string;

  read_response t response;
  read_string t "hello";
  read_response t response;
  read_string t "hello";
  shutdown t;
  connection_is_shutdown t

let test_request_pipelining_single_read () =
  let request' =
    Request.create `GET "/" ~headers:(Headers.of_list [ "content-length", "5" ])
  in
  let response =
    Response.create ~headers:(Headers.of_list [ "content-length", "5" ]) `OK
  in
  let handlers_called = ref 0 in
  let response_handler actual_response body =
    incr handlers_called;
    Alcotest.check
      (module Response)
      "expected response"
      response
      actual_response;
    let rec on_read _ ~off:_ ~len:_ =
      Body.Reader.schedule_read
        ~on_error:(Helpers.body_must_complete __LOC__)
        body
        ~on_read
        ~on_eof
    and on_eof () = () in
    Body.Reader.schedule_read
      ~on_error:(Helpers.body_must_complete __LOC__)
      body
      ~on_read
      ~on_eof
  in

  let t = create ~config:pipelining () in
  let body =
    request t request' ~response_handler ~error_handler:no_error_handler
  in
  Body.Writer.write_string body "hello";
  Body.Writer.close body;
  let body' =
    request t request' ~response_handler ~error_handler:no_error_handler
  in
  let req_string = request_to_string request' in
  write_string t (req_string ^ "hello");
  write_string t req_string;

  let resp_string = response_to_string response in
  read_string t (resp_string ^ "hello" ^ resp_string);
  Alcotest.(check int) "Both response handlers called" 2 !handlers_called;

  writer_yielded t;
  let writer_woken_up = ref false in
  yield_writer t (fun () ->
    writer_woken_up := true;
    write_string t "hello");

  Body.Writer.write_string body' "hello";
  Body.Writer.close body';
  Alcotest.(check bool) "Writer woken up" true !writer_woken_up

let test_flush_response_before_shutdown () =
  let request' = Request.create `GET "/" ~headers:Headers.encoding_chunked in
  let response_handler _response _body = assert false in
  let t = create () in
  let errors = ref [] in
  let body =
    request
      t
      request'
      ~flush_headers_immediately:true
      ~response_handler
      ~error_handler:(fun error -> errors := error :: !errors)
  in
  write_request t request';

  Body.Writer.write_string body "hello world";
  shutdown t;
  Alcotest.(check (list response_error))
    "unanswered request gets Connection_closed"
    [ `Connection_closed ]
    !errors;
  Alcotest.(check bool)
    "Shutting down closes the request body"
    true
    (Body.Writer.is_closed body);

  writer_closed t;
  connection_is_shutdown t

let test_report_exn_during_body_read () =
  let request' = Request.create `GET "/" in
  let response = Response.create `OK in

  let body_done = ref false in
  let body_failed = ref false in
  let error_message = ref None in
  let t = create () in
  let body =
    request
      t
      request'
      ~response_handler:(fun _ body ->
        let on_read _ ~off:_ ~len:_ = () in
        let on_eof () = body_done := true in
        let on_error () = body_failed := true in
        Body.Reader.schedule_read body ~on_error ~on_read ~on_eof)
      ~error_handler:(function
        | `Exn (Failure msg) ->
          Alcotest.(check bool) "body is not complete" false !body_done;
          error_message := Some msg
        | _ -> assert false)
  in
  Body.Writer.close body;
  write_request t request';
  writer_yielded t;
  reader_ready t;
  read_response t response;
  report_exn t (Failure "something went wrong");
  connection_is_shutdown t;
  Alcotest.(check (option string))
    "something went wrong"
    (Some "something went wrong")
    !error_message;
  Alcotest.(check bool) "body is not complete" false !body_done;
  Alcotest.(check bool) "body failed" true !body_failed

let test_can_read_response_after_write_eof () =
  let request' = Request.create `GET "/" ~headers:Headers.encoding_chunked in
  let resp = ref None in
  let response_handler response body =
    resp := Some response;
    Body.Reader.close body
  in
  let t = create () in
  let _body' =
    request
      t
      request'
      ~flush_headers_immediately:true
      ~response_handler
      ~error_handler:no_error_handler
  in
  write_request t request';

  let response = Response.create `OK ~headers:(Headers.encoding_fixed 0) in

  report_write_result t `Closed;
  writer_closed t;

  read_response t response;
  reader_ready t;
  Alcotest.(check bool)
    "response read and handler called"
    true
    (Option.is_some !resp)

let test_read_response_before_shutdown () =
  let request' = Request.create `GET "/" ~headers:Headers.encoding_chunked in
  let response_received = ref false in
  let response_handler _response _body = response_received := true in
  let t = create () in
  let body =
    request
      t
      request'
      ~flush_headers_immediately:true
      ~response_handler
      ~error_handler:no_error_handler
  in
  write_request t request';

  Body.Writer.write_string body "hello world";

  read_response t (Response.create ~headers:(Headers.encoding_fixed 0) `OK);
  Alcotest.(check bool) "Response handler called" true !response_received;

  shutdown t;
  Alcotest.(check bool)
    "Shutting down closes the request body"
    true
    (Body.Writer.is_closed body);

  writer_closed t;
  connection_is_shutdown t

let test_client_connect () =
  let writer_woken_up = ref false in
  let reader_woken_up = ref false in
  let request' =
    Request.create
      ~headers:(Headers.of_list [ "host", "example.com:80" ])
      `CONNECT
      "/"
  in
  let t = create () in
  let response = Response.create `OK in
  let body =
    request
      t
      request'
      ~flush_headers_immediately:true
      ~response_handler:(default_response_handler response)
      ~error_handler:no_error_handler
  in
  write_request t request';
  writer_yielded t;
  Body.Writer.close body;
  reader_ready t;
  read_response t response;
  reader_yielded t;
  yield_reader t (fun () -> reader_woken_up := true);
  writer_yielded t;
  yield_writer t (fun () -> writer_woken_up := true);
  Alcotest.(check bool) "Reader hasn't woken up yet" false !reader_woken_up;
  Alcotest.(check bool) "Writer hasn't woken up yet" false !writer_woken_up;
  shutdown t;
  Alcotest.(check bool) "Reader woken up" true !reader_woken_up;
  Alcotest.(check bool) "Writer woken up" true !writer_woken_up;
  connection_is_shutdown t

let test_invalid_request_head_writes_nothing () =
  let t = create () in
  Alcotest.check_raises
    "request raises"
    (Invalid_argument {|ache: invalid request target "/a HTTP/1.1\r\nX: y"|})
    (fun () ->
       ignore
         (request
            t
            ~flush_headers_immediately:true
            (Request.create `GET "/a HTTP/1.1\r\nX: y")
            ~response_handler:(fun _ _ -> ())
            ~error_handler:no_error_handler
          : Body.Writer.t));
  Alcotest.(check bool)
    "nothing written"
    true
    (match next_write_operation t with
    | `Write _ -> false
    | `Yield | `Close _ -> true)

(* Connection reuse rules: pipelining limit, 204/304 framing, request checks,
   unsolicited bytes. *)

type exchange =
  { mutable statuses : int list
  ; mutable body : string
  ; mutable errors : string list
  }

let error_name = function
  | `Malformed_response _ -> "Malformed_response"
  | `Invalid_response_body_length _ -> "Invalid_response_body_length"
  | `Connection_closed -> "Connection_closed"
  | `Exn _ -> "Exn"

let send ?(meth = `GET) ?(headers = []) ?body t target =
  let ex = { statuses = []; body = ""; errors = [] } in
  let writer =
    request
      t
      ~flush_headers_immediately:true
      (Request.create ~headers:(Headers.of_list headers) meth target)
      ~error_handler:(fun error ->
        ex.errors <- ex.errors @ [ error_name error ])
      ~response_handler:(fun response reader ->
        ex.statuses <- ex.statuses @ [ Status.to_code response.status ];
        let rec loop () =
          Body.Reader.schedule_read
            reader
            ~on_error:ignore
            ~on_eof:ignore
            ~on_read:(fun bs ~off ~len ->
              ex.body <- ex.body ^ Bigstring.substring bs ~off ~len;
              loop ())
        in
        loop ())
  in
  Option.iter (Body.Writer.write_string writer) body;
  Body.Writer.close writer;
  ex

(* Everything the connection writes now. *)
let rec drain_output t =
  match next_write_operation t with
  | `Write iovecs ->
    let s = Write_operation.iovecs_to_string iovecs in
    report_write_result t (`Ok (String.length s));
    s ^ drain_output t
  | `Yield | `Close _ -> ""

(* Reads [input], then runs the read side to its next stop. *)
let respond t input =
  if input <> ""
  then
    match next_read_operation t with
    | `Read -> read_string t input
    | `Yield | `Close -> Alcotest.fail "the connection does not read"
  else ();
  ignore (next_read_operation t : [ `Read | `Yield | `Close ])

let check_exchange msg ?(statuses = []) ?(body = "") ?(errors = []) ex =
  Alcotest.(check (list int)) (msg ^ ": statuses") statuses ex.statuses;
  Alcotest.(check string) (msg ^ ": body") body ex.body;
  Alcotest.(check (list string)) (msg ^ ": errors") errors ex.errors

let get target = Printf.sprintf "GET %s HTTP/1.1\r\n\r\n" target

let ok body =
  Printf.sprintf
    "HTTP/1.1 200 OK\r\ncontent-length: %d\r\n\r\n%s"
    (String.length body)
    body

let test_no_pipelining_by_default () =
  let t = create () in
  let a = send t "/a" in
  let b = send t "/b" in
  Alcotest.(check string) "only the first request" (get "/a") (drain_output t);
  respond t (ok "r1");
  check_exchange "first" ~statuses:[ 200 ] ~body:"r1" a;
  Alcotest.(check string)
    "second written after the first response"
    (get "/b")
    (drain_output t);
  respond t (ok "r2");
  check_exchange "second" ~statuses:[ 200 ] ~body:"r2" b

let test_pipelining_limit () =
  let config = { Config.default with max_pipelined_requests = 2 } in
  let t = create ~config () in
  let a = send t "/a" in
  let b = send t "/b" in
  let c = send t "/c" in
  Alcotest.(check string)
    "two requests wait for a response"
    (get "/a" ^ get "/b")
    (drain_output t);
  respond t (ok "r1");
  check_exchange "first" ~statuses:[ 200 ] ~body:"r1" a;
  Alcotest.(check string)
    "third after the first response"
    (get "/c")
    (drain_output t);
  respond t (ok "r2" ^ ok "r3");
  check_exchange "second" ~statuses:[ 200 ] ~body:"r2" b;
  check_exchange "third" ~statuses:[ 200 ] ~body:"r3" c

let test_pipelining_16_gets () =
  let t = create ~config:pipelining () in
  let targets = List.init 16 (Printf.sprintf "/%d") in
  let exs = List.map (send t) targets in
  Alcotest.(check string)
    "16 requests written before any response"
    (String.concat "" (List.map get targets))
    (drain_output t);
  respond t (String.concat "" (List.map ok targets));
  List.iter2
    (fun target ex -> check_exchange target ~statuses:[ 200 ] ~body:target ex)
    targets
    exs;
  Alcotest.(check bool) "open" false (is_closed t)

let test_pipelining_added_one_at_a_time () =
  let t = create ~config:pipelining () in
  let targets = [ "/1"; "/2"; "/3" ] in
  let exs =
    List.map
      (fun target ->
         let ex = send t target in
         Alcotest.(check string) target (get target) (drain_output t);
         ex)
      targets
  in
  respond t (String.concat "" (List.map ok targets));
  List.iter2
    (fun target ex -> check_exchange target ~statuses:[ 200 ] ~body:target ex)
    targets
    exs

let test_no_pipelining_of_non_idempotent () =
  let t = create ~config:pipelining () in
  let a = send t "/a" in
  let b =
    send ~meth:`POST ~headers:[ "content-length", "3" ] ~body:"abc" t "/b"
  in
  let c = send t "/c" in
  Alcotest.(check string)
    "the POST waits for the pending response"
    (get "/a")
    (drain_output t);
  respond t (ok "r1");
  check_exchange "GET" ~statuses:[ 200 ] ~body:"r1" a;
  Alcotest.(check string)
    "the POST, alone"
    "POST /b HTTP/1.1\r\ncontent-length: 3\r\n\r\nabc"
    (drain_output t);
  respond t (ok "r2");
  check_exchange "POST" ~statuses:[ 200 ] ~body:"r2" b;
  Alcotest.(check string) "then the GET" (get "/c") (drain_output t);
  respond t (ok "r3");
  check_exchange "last GET" ~statuses:[ 200 ] ~body:"r3" c

let test_no_pipelining_of_upgrade () =
  let t = create ~config:pipelining () in
  let a = send t "/a" in
  let _b =
    send ~headers:[ "connection", "upgrade"; "upgrade", "websocket" ] t "/b"
  in
  Alcotest.(check string)
    "the Upgrade request waits for the pending response"
    (get "/a")
    (drain_output t);
  respond t (ok "r1");
  check_exchange "GET" ~statuses:[ 200 ] ~body:"r1" a;
  Alcotest.(check string)
    "the Upgrade request after the response"
    "GET /b HTTP/1.1\r\nconnection: upgrade\r\nupgrade: websocket\r\n\r\n"
    (drain_output t)

let test_no_content_with_framing_closes () =
  let cases =
    [ "GET", "HTTP/1.1 204 No Content\r\ncontent-length: 5\r\n\r\n", true
    ; "GET", "HTTP/1.1 304 Not Modified\r\ncontent-length: 5\r\n\r\n", true
    ; ( "GET"
      , "HTTP/1.1 204 No Content\r\ntransfer-encoding: chunked\r\n\r\n"
      , true )
    ; ( "GET"
      , "HTTP/1.1 304 Not Modified\r\ntransfer-encoding: chunked\r\n\r\n"
      , true )
    ; "GET", "HTTP/1.1 204 No Content\r\ncontent-length: 0\r\n\r\n", false
    ; "GET", "HTTP/1.1 304 Not Modified\r\n\r\n", false
    ; "HEAD", "HTTP/1.1 304 Not Modified\r\ncontent-length: 5\r\n\r\n", false
    ]
  in
  List.iter
    (fun (meth, head, closes) ->
       let msg = String.escaped head in
       let t = create ~config:pipelining () in
       let a = send ~meth:(Method.of_string meth) t "/a" in
       let b = send t "/b" in
       ignore (drain_output t : string);
       respond t (head ^ ok "r2");
       Alcotest.(check (list int))
         (msg ^ ": first status")
         [ int_of_string (String.sub head 9 3) ]
         a.statuses;
       Alcotest.(check (list string)) (msg ^ ": first errors") [] a.errors;
       if closes
       then (
         check_exchange (msg ^ ": second") ~errors:[ "Connection_closed" ] b;
         Alcotest.(check bool) (msg ^ ": closed") true (is_closed t))
       else (
         check_exchange (msg ^ ": second") ~statuses:[ 200 ] ~body:"r2" b;
         Alcotest.(check bool) (msg ^ ": open") false (is_closed t)))
    cases

let test_request_framing_rejected () =
  let cases =
    [ ( [ "Host", "a"; "host", "b" ]
      , Invalid_argument "ache: more than one host field" )
    ; ( [ "content-length", "3"; "transfer-encoding", "chunked" ]
      , Invalid_argument
          "ache: content-length and transfer-encoding are both set" )
    ; ( [ "content-length", "3"; "content-length", "4" ]
      , Invalid_argument "ache: invalid content-length \"3, 4\"" )
    ; ( [ "content-length", "+3" ]
      , Invalid_argument "ache: invalid content-length \"+3\"" )
    ; ( [ "transfer-encoding", "gzip" ]
      , Invalid_argument "ache: transfer-encoding does not end with chunked" )
    ]
  in
  List.iter
    (fun (headers, exn) ->
       let t = create () in
       Alcotest.check_raises
         (String.concat ", " (List.map snd headers))
         exn
         (fun () -> ignore (send ~meth:`POST ~headers t "/" : exchange));
       Alcotest.(check string) "nothing written" "" (drain_output t);
       let ex = send t "/next" in
       Alcotest.(check string)
         "next request written"
         (get "/next")
         (drain_output t);
       respond t (ok "r");
       check_exchange "next request" ~statuses:[ 200 ] ~body:"r" ex)
    cases

let test_stray_bytes_when_idle () =
  List.iter
    (fun stray ->
       let msg = String.escaped stray in
       let t = create () in
       let a = send t "/a" in
       ignore (drain_output t : string);
       respond t (ok "r1");
       check_exchange (msg ^ ": first") ~statuses:[ 200 ] ~body:"r1" a;
       (* An unsolicited response is not read to its end. *)
       ignore (feed_string t stray : int);
       ignore (next_read_operation t : [ `Read | `Yield | `Close ]);
       Alcotest.(check bool) (msg ^ ": closed") true (is_closed t);
       let b = send t "/b" in
       Alcotest.(check string) (msg ^ ": nothing written") "" (drain_output t);
       check_exchange (msg ^ ": next request") ~errors:[ "Connection_closed" ] b)
    [ "HTTP/1.1 200 OK\r\ncontent-length: 4\r\n\r\nevil"
    ; "HTTP/1.1 200 OK\r\n"
    ; "H"
    ; "\r\nHTTP"
    ]

let test_crlf_when_idle () =
  let t = create () in
  let a = send t "/a" in
  ignore (drain_output t : string);
  respond t (ok "r1");
  check_exchange "first" ~statuses:[ 200 ] ~body:"r1" a;
  respond t "\r\n";
  Alcotest.(check bool) "open" false (is_closed t);
  let b = send t "/b" in
  Alcotest.(check string) "next request written" (get "/b") (drain_output t);
  respond t (ok "r2");
  check_exchange "next request" ~statuses:[ 200 ] ~body:"r2" b

(* Reads [chunks] one after the other, as the read side asks for them. *)
let read_chunks t chunks =
  let pending = ref "" in
  let rec pump () =
    ignore (drain_output t : string);
    match next_read_operation t with
    | `Read when !pending <> "" ->
      let n = feed_string t !pending in
      pending := String.sub !pending n (String.length !pending - n);
      if n > 0 then pump ()
    | `Read | `Yield | `Close -> ()
  in
  List.iter
    (fun chunk ->
       pending := !pending ^ chunk;
       pump ())
    chunks

let all_splits s =
  let len = String.length s in
  [ s ]
  :: List.init len (fun i -> String.make 1 s.[i])
  :: List.init (len + 1) (fun i ->
    [ String.sub s 0 i; String.sub s i (len - i) ])

let test_stray_bytes_after_response () =
  let config = { Config.default with max_pipelined_requests = 4 } in
  List.iter
    (fun (requests, input, errors) ->
       List.iter
         (fun chunks ->
            let msg =
              Printf.sprintf
                "%d requests, reads [%s]"
                requests
                (String.concat "," (List.map String.escaped chunks))
            in
            let t = create ~config () in
            let exchanges = List.init requests (fun _ -> send t "/") in
            read_chunks t chunks;
            Alcotest.(check (list (list string)))
              (msg ^ ": errors")
              errors
              (List.map (fun ex -> ex.errors) exchanges);
            Alcotest.(check (list int))
              (msg ^ ": first response")
              [ 200 ]
              (List.hd exchanges).statuses;
            Alcotest.(check bool) (msg ^ ": closed") true (is_closed t))
         (all_splits input))
    [ (* With no request waiting, the bytes are unsolicited. *)
      1, "HTTP/1.1 200 OK\r\nContent-Length: 0\r\n\r\n\n", [ [] ]
    ; ( 3
      , "HTTP/1.1 200 OK\r\nContent-Length: 0\r\n\r\n\n"
      , [ []; [ "Malformed_response" ]; [ "Connection_closed" ] ] )
    ; (* After a response that closes the connection, no request is answered. *)
      ( 3
      , "HTTP/1.0 200 OK\r\nContent-Length: 0\r\n\r\n\n"
      , [ []; [ "Connection_closed" ]; [ "Connection_closed" ] ] )
    ]

let reuse_tests =
  [ "no pipelining by default", `Quick, test_no_pipelining_by_default
  ; ( "max_pipelined_requests limits pending responses"
    , `Quick
    , test_pipelining_limit )
  ; ( "16 GETs pipeline with max_pipelined_requests = 16"
    , `Quick
    , test_pipelining_16_gets )
  ; ( "requests added one at a time pipeline"
    , `Quick
    , test_pipelining_added_one_at_a_time )
  ; ( "a POST is never written while a response is pending"
    , `Quick
    , test_no_pipelining_of_non_idempotent )
  ; ( "an Upgrade request is never written while a response is pending"
    , `Quick
    , test_no_pipelining_of_upgrade )
  ; ( "204 or 304 with a length or a coding closes the connection"
    , `Quick
    , test_no_content_with_framing_closes )
  ; ( "two Host fields or bad framing raise Invalid_argument"
    , `Quick
    , test_request_framing_rejected )
  ; ( "response bytes on an idle connection close it"
    , `Quick
    , test_stray_bytes_when_idle )
  ; ( "an empty line on an idle connection is ignored"
    , `Quick
    , test_crlf_when_idle )
  ; ( "bytes after a complete response, at every split"
    , `Quick
    , test_stray_bytes_after_response )
  ]

let tests =
  [ ( "commit parse after every header line"
    , `Quick
    , test_commit_parse_after_every_header )
  ; "GET", `Quick, test_get
  ; "HEAD", `Quick, test_head
  ; "send streaming body", `Quick, test_send_streaming_body
  ; "Response EOF", `Quick, test_response_eof
  ; "Response header order preserved", `Quick, test_response_header_order
  ; "report_exn", `Quick, test_report_exn
  ; "input_shrunk", `Quick, test_input_shrunk
  ; "multiple GET, last request closes connection", `Quick, test_get_last_close
  ; ( "Persistent connection, multiple GETs"
    , `Quick
    , test_persistent_connection_requests )
  ; ( "Persistent connection, request pipelining"
    , `Quick
    , test_persistent_connection_requests_pipelining )
  ; ( "Persistent connection, first request includes body"
    , `Quick
    , test_persistent_connection_requests_pipelining_send_body )
  ; ( "Persistent connections, read response body"
    , `Quick
    , test_persistent_connection_requests_body )
  ; "Partial input", `Quick, test_partial_input
  ; "Empty fixed body shuts down writer", `Quick, test_empty_fixed_body
  ; ( "Empty fixed body in a persistent connection doesn't shut down the writer"
    , `Quick
    , test_empty_fixed_body_persistent_connection )
  ; ( "Fixed body shuts down writer if connection is not persistent"
    , `Quick
    , test_fixed_body )
  ; ( "Fixed body doesn't shut down the writer if connection is persistent"
    , `Quick
    , test_fixed_body_persistent_connection )
  ; "Client support for upgrading a connection", `Quick, test_client_upgrade
  ; ( "test yield when read isn't scheduled"
    , `Quick
    , test_handling_backpressure_when_read_not_scheduled )
  ; ( "test yield when read isn't scheduled, reader yields early"
    , `Quick
    , test_handling_backpressure_when_read_not_scheduled_early_yield )
  ; ( "EOF while a request is in the pipeline"
    , `Quick
    , test_eof_with_another_pipelined_request )
  ; ( "EOF after handler response body not closed"
    , `Quick
    , test_eof_handler_response_body_not_closed )
  ; ( "EOF after handler closed response body"
    , `Quick
    , test_eof_handler_closed_response_body )
  ; ( "Exception closes the reader (on a non-persistent connection)"
    , `Quick
    , test_exception_closes_reader )
  ; ( "Exception closes the reader (on a persistent connection)"
    , `Quick
    , test_exception_closes_reader_persistent_connection )
  ; ( "Exception while reading the response body"
    , `Quick
    , test_exception_reading_response_body )
  ; ( "Exception while reading the response body"
    , `Quick
    , test_exception_reading_response_body_last_chunk )
  ; ( "Aynchronous exception while reading the response body"
    , `Quick
    , test_async_exception_reading_response_body )
  ; "failed response parse", `Quick, test_failed_response_parse
  ; ( "shutdown delivers eof to response bodies"
    , `Quick
    , test_shutdown_hangs_response_body_read )
  ; ( "full response arrives before uploading the entire request body, 2nd \
       request in the pipeline"
    , `Quick
    , test_response_arrives_before_body_uploaded )
  ; ( "reader EOF race condition causes state machine to issue writer yield"
    , `Quick
    , test_race_condition_writer_issues_yield_after_reader_eof )
  ; ( "multiple responses in single read"
    , `Quick
    , test_multiple_responses_in_single_read )
  ; "test chunked error", `Quick, test_chunked_error
  ; "304 Not Modified with Content-Length", `Quick, test_304_not_modified
  ; ( "body for a request with no payload starts out closed"
    , `Quick
    , test_empty_content_length_body_closed )
  ; ( "schedule read with data available"
    , `Quick
    , test_schedule_read_with_data_available )
  ; ( "don't flush headers immediately"
    , `Quick
    , test_dont_flush_headers_immediately )
  ; ( "pipelining + don't flush headers immediately"
    , `Quick
    , test_pipelining_no_immediate_flush )
  ; ( "flushing on close with ~set_headers_immediately:false"
    , `Quick
    , test_flush_on_close )
  ; ( "request pipelining flushes request body"
    , `Quick
    , test_request_pipelining_async )
  ; ( "request pipelining both responses in single buffer"
    , `Quick
    , test_request_pipelining_single_read )
  ; ( "shutting down closes request bodies"
    , `Quick
    , test_flush_response_before_shutdown )
  ; "shut down closes request body ", `Quick, test_read_response_before_shutdown
  ; "report exn during body read", `Quick, test_report_exn_during_body_read
  ; ( "read response after write eof"
    , `Quick
    , test_can_read_response_after_write_eof )
  ; "Client support for CONNECT", `Quick, test_client_connect
  ; ( "invalid request head writes nothing"
    , `Quick
    , test_invalid_request_head_writes_nothing )
  ]
  @ reuse_tests
