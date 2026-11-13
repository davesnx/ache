open Ache

module Echo = struct
  type t =
    { mutable pending : string
    ; on_read : t -> string -> unit
    ; mutable closed : bool
    ; mutable reader_k : (unit -> unit) option
    ; mutable writer_k : (unit -> unit) option
    }

  let create ?(pending = "") on_read =
    { pending; on_read; closed = false; reader_k = None; writer_k = None }

  let wake k = match k with None -> () | Some k -> k ()

  let wakeup_writer t =
    let k = t.writer_k in
    t.writer_k <- None;
    wake k

  let send t s =
    t.pending <- t.pending ^ s;
    wakeup_writer t

  let shutdown t =
    t.closed <- true;
    let k = t.reader_k in
    t.reader_k <- None;
    wake k;
    wakeup_writer t

  let next_read_operation t = if t.closed then `Close else `Read

  let read t bs ~off ~len =
    if len > 0 then t.on_read t (Bigstringaf.substring bs ~off ~len);
    len

  let read_eof t bs ~off ~len =
    let n = read t bs ~off ~len in
    shutdown t;
    n

  let yield_reader t k = t.reader_k <- Some k

  let next_write_operation t =
    let len = String.length t.pending in
    if len > 0
    then
      `Write
        [ { IOVec.buffer = Bigstringaf.of_string t.pending ~off:0 ~len
          ; off = 0
          ; len
          }
        ]
    else if t.closed
    then `Close 0
    else `Yield

  let report_write_result t = function
    | `Ok n -> t.pending <- String.sub t.pending n (String.length t.pending - n)
    | `Closed ->
      t.pending <- "";
      shutdown t

  let yield_writer t k = t.writer_k <- Some k
  let report_exn t _ = shutdown t
  let is_closed t = t.closed && t.pending = ""
end

let echo_server () =
  Runtime.make (module Echo) (Echo.create (fun t s -> Echo.send t s))

let server_errors : Server_connection.error list ref = ref []
let stream_flush_closed : unit Eio.Promise.u option ref = ref None
let requests_seen = ref 0

let request_handler (_ : Eio.Net.Sockaddr.stream) reqd =
  incr requests_seen;
  let request = Reqd.request reqd in
  let body = Reqd.request_body reqd in
  match request.meth, request.target with
  | `GET, "/hello" ->
    Body.Reader.close body;
    Reqd.respond_with_string
      reqd
      (Response.create
         ~headers:(Headers.of_list [ "content-length", "11" ])
         `OK)
      "hello world"
  | `POST, "/echo" ->
    let headers =
      Headers.of_list
        [ ( "content-length"
          , Option.get (Headers.get request.headers "content-length") )
        ]
    in
    let response_body =
      Reqd.respond_with_streaming reqd (Response.create ~headers `OK)
    in
    let rec on_read buffer ~off ~len =
      Body.Writer.write_string
        response_body
        (Bigstringaf.substring buffer ~off ~len);
      Body.Writer.flush response_body (fun _ ->
        Body.Reader.schedule_read body ~on_eof ~on_read)
    and on_eof () = Body.Writer.close response_body in
    Body.Reader.schedule_read body ~on_eof ~on_read
  | `GET, "/upgrade" ->
    Body.Reader.close body;
    Reqd.respond_with_upgrade
      reqd
      (Headers.of_list [ "connection", "upgrade"; "upgrade", "echo" ])
      (echo_server ())
  | `GET, "/stream" ->
    Body.Reader.close body;
    let response_body =
      Reqd.respond_with_streaming reqd (Response.create `OK)
    in
    let chunk = String.make 65536 'x' in
    let rec loop () =
      Body.Writer.write_string response_body chunk;
      Body.Writer.flush response_body (function
        | `Written -> loop ()
        | `Closed ->
          Option.iter (fun u -> Eio.Promise.resolve u ()) !stream_flush_closed)
    in
    loop ()
  | `GET, "/raise" -> failwith "handler failed"
  | `POST, "/partial" ->
    let rec on_read _ ~off:_ ~len:_ =
      Body.Reader.schedule_read body ~on_eof ~on_read
    and on_eof () = () in
    Body.Reader.schedule_read body ~on_eof ~on_read
  | _, target ->
    Body.Reader.close body;
    Reqd.respond_with_string
      reqd
      (Response.create
         ~headers:
           (Headers.of_list
              [ "content-length", string_of_int (String.length target) ])
         `OK)
      target

let error_handler (_ : Eio.Net.Sockaddr.stream) ?request:_ error start_response =
  server_errors := error :: !server_errors;
  Body.Writer.close (start_response Headers.empty)

let connections_done : unit Eio.Stream.t = Eio.Stream.create max_int
let real_sleep : (float -> unit) ref = ref (fun (_ : float) -> ())

let with_server
      ?(flow = fun socket -> (socket :> Eio.Flow.two_way_ty Eio.Resource.t))
      ?clock
      ?idle_timeout
      ?head_timeout
      f
  =
  server_errors := [];
  requests_seen := 0;
  Eio_main.run (fun env ->
    real_sleep := Eio.Time.sleep (Eio.Stdenv.clock env);
    Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 10.0 (fun () ->
      Eio.Switch.run (fun sw ->
        let net = Eio.Stdenv.net env in
        let listening =
          Eio.Net.listen
            ~sw
            ~backlog:16
            ~reuse_addr:true
            net
            (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0))
        in
        Eio.Fiber.fork_daemon ~sw (fun () ->
          let rec accept () =
            Eio.Net.accept_fork
              ~sw
              listening
              ~on_error:raise
              (fun socket addr ->
                 Ache_eio.Server.create_connection_handler
                   ?clock
                   ?idle_timeout
                   ?head_timeout
                   ~request_handler
                   ~error_handler
                   (flow socket)
                   addr;
                 Eio.Stream.add connections_done ());
            accept ()
          in
          accept ());
        f ~sw ~net (Eio.Net.listening_addr listening))))

let read_body body =
  let buf = Stdlib.Buffer.create 1024 in
  let p, u = Eio.Promise.create () in
  let rec on_read bs ~off ~len =
    Stdlib.Buffer.add_string buf (Bigstringaf.substring bs ~off ~len);
    Body.Reader.schedule_read body ~on_eof ~on_read
  and on_eof () = Eio.Promise.resolve u (Stdlib.Buffer.contents buf) in
  Body.Reader.schedule_read body ~on_eof ~on_read;
  p

let client_error_handler error =
  match error with
  | `Malformed_response msg -> Alcotest.failf "malformed response: %s" msg
  | `Invalid_response_body_length _ -> Alcotest.fail "invalid body length"
  | `Connection_closed -> Alcotest.fail "connection closed"
  | `Exn exn -> raise exn

let request client ?(body = "") request =
  let p, u = Eio.Promise.create () in
  let request_body =
    Ache_eio.Client.request
      client
      request
      ~error_handler:client_error_handler
      ~response_handler:(fun response response_body ->
        Eio.Promise.resolve u (response, read_body response_body))
  in
  Body.Writer.write_string request_body body;
  Body.Writer.close request_body;
  let response, body = Eio.Promise.await p in
  response, Eio.Promise.await body

let test_get () =
  with_server (fun ~sw ~net addr ->
    let client =
      Ache_eio.Client.create_connection ~sw (Eio.Net.connect ~sw net addr)
    in
    let response, body =
      request
        client
        (Request.create
           ~headers:(Headers.of_list [ "host", "localhost" ])
           `GET
           "/hello")
    in
    Alcotest.(check int) "status" 200 (Status.to_code response.status);
    Alcotest.(check string) "body" "hello world" body;
    Eio.Promise.await (Ache_eio.Client.shutdown client))

let test_post_echo () =
  with_server (fun ~sw ~net addr ->
    let client =
      Ache_eio.Client.create_connection ~sw (Eio.Net.connect ~sw net addr)
    in
    let payload =
      String.init
        ((10 * Config.default.read_buffer_size) + 7)
        (fun i -> Char.chr (97 + (i mod 26)))
    in
    let headers =
      Headers.of_list
        [ "host", "localhost"
        ; "content-length", string_of_int (String.length payload)
        ]
    in
    let response, body =
      request client ~body:payload (Request.create ~headers `POST "/echo")
    in
    Alcotest.(check int) "status" 200 (Status.to_code response.status);
    Alcotest.(check int) "length" (String.length payload) (String.length body);
    Alcotest.(check bool) "body" true (String.equal payload body);
    Eio.Promise.await (Ache_eio.Client.shutdown client))

let test_pipelined () =
  with_server (fun ~sw ~net addr ->
    let socket = Eio.Net.connect ~sw net addr in
    Eio.Flow.copy_string
      "GET /a HTTP/1.1\r\n\
       host: localhost\r\n\
       \r\n\
       GET /b HTTP/1.1\r\n\
       host: localhost\r\n\
       connection: close\r\n\
       \r\n"
      socket;
    let output =
      Eio.Buf_read.parse_exn Eio.Buf_read.take_all ~max_size:65536 socket
    in
    Eio.Flow.close socket;
    Alcotest.(check string)
      "responses"
      "HTTP/1.1 200 OK\r\n\
       content-length: 2\r\n\
       \r\n\
       /aHTTP/1.1 200 OK\r\n\
       content-length: 2\r\n\
       \r\n\
       /b"
      output)

let test_upgrade () =
  with_server (fun ~sw ~net addr ->
    let client =
      Ache_eio.Client.create_connection ~sw (Eio.Net.connect ~sw net addr)
    in
    let upgraded, resolve_upgraded = Eio.Promise.create () in
    let request_body =
      Ache_eio.Client.request
        client
        (Request.create
           ~headers:
             (Headers.of_list
                [ "host", "localhost"
                ; "connection", "upgrade"
                ; "upgrade", "echo"
                ; "content-length", "0"
                ])
           `GET
           "/upgrade")
        ~error_handler:client_error_handler
        ~response_handler:(fun response _body ->
          Eio.Promise.resolve resolve_upgraded response)
    in
    Body.Writer.close request_body;
    let response = Eio.Promise.await upgraded in
    Alcotest.(check int) "status" 101 (Status.to_code response.status);
    let message = "hello over echo" in
    let echoed, resolve_echoed = Eio.Promise.create () in
    let received = Stdlib.Buffer.create 16 in
    let echo_client =
      Echo.create ~pending:message (fun t s ->
        Stdlib.Buffer.add_string received s;
        if Stdlib.Buffer.length received >= String.length message
        then (
          Echo.shutdown t;
          Eio.Promise.resolve resolve_echoed (Stdlib.Buffer.contents received)))
    in
    Ache_eio.Client.upgrade client (Runtime.make (module Echo) echo_client);
    Alcotest.(check string) "echoed" message (Eio.Promise.await echoed);
    Eio.Promise.await (Ache_eio.Client.shutdown client))

let test_upgrade_incomplete_token_reaches_new_protocol () =
  with_server (fun ~sw ~net addr ->
    let socket = Eio.Net.connect ~sw net addr in
    Eio.Flow.copy_string
      "GET /upgrade HTTP/1.1\r\n\
       host: localhost\r\n\
       connection: upgrade\r\n\
       upgrade: echo\r\n\
       \r\n\
       ping"
      socket;
    let expected =
      "HTTP/1.1 101 Switching Protocols\r\n\
       connection: upgrade\r\n\
       upgrade: echo\r\n\
       \r\n\
       ping"
    in
    let reader = Eio.Buf_read.of_flow socket ~max_size:4096 in
    Alcotest.(check string)
      "101 then echoed bytes"
      expected
      (Eio.Buf_read.take (String.length expected) reader);
    Eio.Flow.close socket)

module Failing_flow = struct
  type t =
    { inner : Eio.Flow.two_way_ty Eio.Resource.t
    ; fail_read : bool
    ; fail_write : bool
    }

  let read_methods = []

  let single_read t buf =
    if t.fail_read
    then failwith "read failed"
    else Eio.Flow.single_read t.inner buf

  let single_write t bufs =
    if t.fail_write
    then failwith "write failed"
    else Eio.Flow.single_write t.inner bufs

  let copy t ~src = Eio.Flow.Pi.simple_copy ~single_write t ~src
  let shutdowns : Eio.Flow.shutdown_command list ref = ref []

  let shutdown t cmd =
    shutdowns := cmd :: !shutdowns;
    Eio.Flow.shutdown t.inner cmd

  let wrap ~fail_read ~fail_write socket =
    Eio.Resource.T
      ( { inner = (socket :> Eio.Flow.two_way_ty Eio.Resource.t)
        ; fail_read
        ; fail_write
        }
      , Eio.Flow.Pi.two_way
          (module struct
            type nonrec t = t

            let read_methods = read_methods
            let single_read = single_read
            let single_write = single_write
            let copy = copy
            let shutdown = shutdown
          end) )
end

let check_shutdowns () =
  Alcotest.(check (list string))
    "each direction shut down once"
    [ "receive"; "send" ]
    (List.sort
       compare
       (List.concat_map
          (function
            | `Receive -> [ "receive" ]
            | `Send -> [ "send" ]
            | `All -> [ "receive"; "send" ])
          !Failing_flow.shutdowns))

let get_until_closed ~sw ~net addr =
  let socket = Eio.Net.connect ~sw net addr in
  Eio.Flow.copy_string "GET /hello HTTP/1.1\r\nhost: localhost\r\n\r\n" socket;
  (match
     Eio.Buf_read.parse_exn Eio.Buf_read.take_all ~max_size:65536 socket
   with
  | (_ : string) -> ()
  | exception Eio.Io (Eio.Net.E (Connection_reset _), _) -> ());
  Eio.Flow.close socket

let test_read_failure () =
  Failing_flow.shutdowns := [];
  with_server
    ~flow:(Failing_flow.wrap ~fail_read:true ~fail_write:false)
    (fun ~sw ~net addr ->
       get_until_closed ~sw ~net addr;
       Eio.Stream.take connections_done);
  check_shutdowns ()

let test_write_failure () =
  Failing_flow.shutdowns := [];
  with_server
    ~flow:(Failing_flow.wrap ~fail_read:false ~fail_write:true)
    (fun ~sw ~net addr ->
       get_until_closed ~sw ~net addr;
       Eio.Stream.take connections_done);
  check_shutdowns ()

let test_eof_mid_body () =
  with_server (fun ~sw ~net addr ->
    let socket = Eio.Net.connect ~sw net addr in
    Eio.Flow.copy_string
      "POST /partial HTTP/1.1\r\n\
       host: localhost\r\n\
       content-length: 100\r\n\
       \r\n\
       only ten b"
      socket;
    Eio.Flow.shutdown socket `Send;
    let (_ : string) =
      Eio.Buf_read.parse_exn Eio.Buf_read.take_all ~max_size:65536 socket
    in
    Eio.Flow.close socket);
  match !server_errors with
  | [ `Bad_request ] -> ()
  | _ ->
    Alcotest.failf
      "expected one Bad_request, got %d errors"
      (List.length !server_errors)

let test_flush_after_peer_closed () =
  let flushed, resolve_flushed = Eio.Promise.create () in
  stream_flush_closed := Some resolve_flushed;
  Failing_flow.shutdowns := [];
  with_server
    ~flow:(Failing_flow.wrap ~fail_read:false ~fail_write:false)
    (fun ~sw ~net addr ->
       let socket = Eio.Net.connect ~sw net addr in
       Eio.Flow.copy_string
         "GET /stream HTTP/1.1\r\nhost: localhost\r\n\r\n"
         socket;
       let reader = Eio.Buf_read.of_flow socket ~max_size:65536 in
       let rec skip_head () =
         if Eio.Buf_read.line reader <> "" then skip_head ()
       in
       Alcotest.(check string)
         "status line"
         "HTTP/1.1 200 OK"
         (Eio.Buf_read.line reader);
       skip_head ();
       Eio.Net.setsockopt socket Eio.Net.Sockopt.SO_LINGER (Some 0);
       Eio.Flow.close socket;
       Eio.Promise.await flushed;
       Eio.Stream.take connections_done);
  stream_flush_closed := None;
  check_shutdowns ()

let known_failure ~ticket name f =
  ( Printf.sprintf "%s [known failure: ticket %s]" name ticket
  , `Quick
  , fun () ->
      match f () with
      | () ->
        Alcotest.failf
          "known failure now passes: remove the known_failure marker (ticket \
           %s) from \"%s\""
          ticket
          name
      | exception exn ->
        Printf.printf
          "KNOWN FAILURE (ticket %s) %s\n  observed: %s\n%!"
          ticket
          name
          (Printexc.to_string exn) )

let rec advance clock =
  if not (Eio_mock.Clock.try_advance clock)
  then (
    !real_sleep 0.001;
    advance clock)

let read_all socket =
  Eio.Buf_read.parse_exn Eio.Buf_read.take_all ~max_size:65536 socket

let get_hello = "GET /hello HTTP/1.1\r\nhost: localhost\r\n\r\n"
let hello_response = "HTTP/1.1 200 OK\r\ncontent-length: 11\r\n\r\nhello world"
let counting_flow = Failing_flow.wrap ~fail_read:false ~fail_write:false

let test_idle_timeout () =
  let clock = Eio_mock.Clock.make () in
  Failing_flow.shutdowns := [];
  with_server ~flow:counting_flow ~clock ~idle_timeout:0.1 (fun ~sw ~net addr ->
    let socket = Eio.Net.connect ~sw net addr in
    advance clock;
    Alcotest.(check (float 0.))
      "fired at the idle timeout"
      0.1
      (Eio.Time.now clock);
    Alcotest.(check string) "nothing written" "" (read_all socket);
    advance clock;
    Alcotest.(check (float 0.))
      "stops waiting for the peer to close"
      0.2
      (Eio.Time.now clock);
    Eio.Stream.take connections_done;
    Eio.Flow.close socket);
  Alcotest.(check int) "no error" 0 (List.length !server_errors);
  check_shutdowns ()

let test_idle_timeout_between_requests () =
  let clock = Eio_mock.Clock.make () in
  with_server ~clock ~idle_timeout:0.1 (fun ~sw ~net addr ->
    let socket = Eio.Net.connect ~sw net addr in
    let reader = Eio.Buf_read.of_flow socket ~max_size:65536 in
    Eio.Flow.copy_string get_hello socket;
    Alcotest.(check string)
      "first response"
      hello_response
      (Eio.Buf_read.take (String.length hello_response) reader);
    Eio.Flow.copy_string get_hello socket;
    Alcotest.(check string)
      "second response"
      hello_response
      (Eio.Buf_read.take (String.length hello_response) reader);
    advance clock;
    Alcotest.(check (float 0.))
      "fired at the idle timeout"
      0.1
      (Eio.Time.now clock);
    Alcotest.(check bool) "closed" true (Eio.Buf_read.at_end_of_input reader);
    Eio.Flow.close socket;
    Eio.Stream.take connections_done);
  Alcotest.(check int) "two requests" 2 !requests_seen

let head_timeout () =
  let clock = Eio_mock.Clock.make () in
  let output = ref "" in
  Failing_flow.shutdowns := [];
  with_server ~flow:counting_flow ~clock ~head_timeout:0.1 (fun ~sw ~net addr ->
    let socket = Eio.Net.connect ~sw net addr in
    Eio.Flow.copy_string "GET /hello HTTP/1.1\r\nHo" socket;
    advance clock;
    Alcotest.(check (float 0.))
      "fired at the head timeout"
      0.1
      (Eio.Time.now clock);
    output := read_all socket;
    Eio.Flow.close socket;
    Eio.Stream.take connections_done);
  Alcotest.(check int) "request handler not called" 0 !requests_seen;
  check_shutdowns ();
  !output

let status_line output =
  match String.index_opt output '\r' with
  | Some i -> String.sub output 0 i
  | None -> output

let test_head_timeout () =
  let output = head_timeout () in
  Alcotest.(check int) "one error" 1 (List.length !server_errors);
  Alcotest.(check bool)
    "4xx status line"
    true
    (String.starts_with ~prefix:"HTTP/1.1 4" output)

let test_head_timeout_408 () =
  Alcotest.(check string)
    "status line"
    "HTTP/1.1 408 Request Timeout"
    (status_line (head_timeout ()))

let test_head_deadline () =
  let clock = Eio_mock.Clock.make () in
  with_server ~clock ~head_timeout:0.1 (fun ~sw ~net addr ->
    let socket = Eio.Net.connect ~sw net addr in
    Eio.Flow.copy_string "GET /hello HTTP/1.1\r\n" socket;
    !real_sleep 0.05;
    Eio_mock.Clock.set_time clock 0.05;
    Eio.Flow.copy_string "a: b\r\n" socket;
    !real_sleep 0.05;
    advance clock;
    Alcotest.(check (float 0.))
      "fired at the head timeout after the first bytes"
      0.1
      (Eio.Time.now clock);
    Alcotest.(check bool)
      "4xx status line"
      true
      (String.starts_with ~prefix:"HTTP/1.1 4" (read_all socket));
    Eio.Flow.close socket;
    Eio.Stream.take connections_done)

let test_slow_body_not_cut () =
  let clock = Eio_mock.Clock.make () in
  with_server ~clock ~idle_timeout:0.1 ~head_timeout:0.1 (fun ~sw ~net addr ->
    let socket = Eio.Net.connect ~sw net addr in
    let reader = Eio.Buf_read.of_flow socket ~max_size:65536 in
    Eio.Flow.copy_string
      "POST /echo HTTP/1.1\r\n\
       host: localhost\r\n\
       content-length: 10\r\n\
       \r\n\
       hello"
      socket;
    let head = "HTTP/1.1 200 OK\r\ncontent-length: 10\r\n\r\nhello" in
    Alcotest.(check string)
      "first half echoed"
      head
      (Eio.Buf_read.take (String.length head) reader);
    Alcotest.(check bool)
      "no timer during the body"
      false
      (Eio_mock.Clock.try_advance clock);
    Eio_mock.Clock.set_time clock 100.;
    Eio.Flow.copy_string "world" socket;
    Alcotest.(check string) "second half" "world" (Eio.Buf_read.take 5 reader);
    Eio.Flow.close socket;
    Eio.Stream.take connections_done);
  Alcotest.(check int) "no error" 0 (List.length !server_errors)

let test_no_timeout_configured () =
  with_server (fun ~sw ~net addr ->
    let socket = Eio.Net.connect ~sw net addr in
    let reader = Eio.Buf_read.of_flow socket ~max_size:65536 in
    Eio.Flow.copy_string get_hello socket;
    Alcotest.(check string)
      "first response"
      hello_response
      (Eio.Buf_read.take (String.length hello_response) reader);
    !real_sleep 0.2;
    Eio.Flow.copy_string get_hello socket;
    Alcotest.(check string)
      "second response"
      hello_response
      (Eio.Buf_read.take (String.length hello_response) reader);
    Eio.Flow.close socket;
    Eio.Stream.take connections_done)

let test_timeout_needs_clock () =
  Eio_mock.Backend.run (fun () ->
    match
      Ache_eio.Server.create_connection_handler
        ~idle_timeout:1.
        ~request_handler
        ~error_handler
        (Eio_mock.Flow.make "flow")
        (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0))
    with
    | () -> Alcotest.fail "expected Invalid_argument"
    | exception Invalid_argument _ -> ())

let test_cancel_is_not_an_error () =
  server_errors := [];
  Eio_main.run (fun _env ->
    Eio.Switch.run (fun sw ->
      let server, client = Eio_unix.Net.socketpair_stream ~sw () in
      Eio.Flow.copy_string "GET /hello HTTP/1.1\r\nHo" client;
      Eio.Fiber.first
        (fun () ->
           Ache_eio.Server.create_connection_handler
             ~request_handler
             ~error_handler
             server
             (`Unix "socketpair"))
        (fun () -> Eio.Fiber.yield ())));
  Alcotest.(check int) "no error reported" 0 (List.length !server_errors)

let test_close_with_trailing_bytes () =
  Failing_flow.shutdowns := [];
  with_server
    ~flow:(Failing_flow.wrap ~fail_read:false ~fail_write:false)
    (fun ~sw ~net addr ->
       let socket = Eio.Net.connect ~sw net addr in
       Eio.Flow.copy_string
         "GET /hello HTTP/1.1\r\nhost: localhost\r\nconnection: close\r\n\r\n"
         socket;
       let reader = Eio.Buf_read.of_flow socket ~max_size:65536 in
       Alcotest.(check string)
         "response"
         hello_response
         (Eio.Buf_read.take (String.length hello_response) reader);
       Alcotest.(check bool) "closed" true (Eio.Buf_read.at_end_of_input reader);
       Eio.Flow.copy_string "more bytes" socket;
       Eio.Flow.close socket;
       Eio.Stream.take connections_done);
  Alcotest.(check int) "no error" 0 (List.length !server_errors);
  check_shutdowns ()

let test_handler_exception () =
  Failing_flow.shutdowns := [];
  with_server ~flow:counting_flow (fun ~sw ~net addr ->
    let socket = Eio.Net.connect ~sw net addr in
    Eio.Flow.copy_string "GET /raise HTTP/1.1\r\nhost: localhost\r\n\r\n" socket;
    Alcotest.(check string)
      "status line"
      "HTTP/1.1 500 Internal Server Error"
      (status_line (read_all socket));
    Eio.Flow.close socket;
    Eio.Stream.take connections_done);
  check_shutdowns ()

let test_run_server () =
  Eio_main.run (fun env ->
    Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 10.0 (fun () ->
      Eio.Switch.run (fun sw ->
        let net = Eio.Stdenv.net env in
        let listening =
          Eio.Net.listen
            ~sw
            ~backlog:16
            ~reuse_addr:true
            net
            (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0))
        in
        let stop, resolve_stop = Eio.Promise.create () in
        Eio.Fiber.both
          (fun () ->
             Eio.Net.run_server
               ~stop
               ~on_error:raise
               listening
               (Ache_eio.Server.create_connection_handler
                  ~request_handler
                  ~error_handler))
          (fun () ->
             let socket =
               Eio.Net.connect ~sw net (Eio.Net.listening_addr listening)
             in
             Eio.Flow.copy_string
               "GET /hello HTTP/1.1\r\n\
                host: localhost\r\n\
                connection: close\r\n\
                \r\n"
               socket;
             Alcotest.(check string)
               "response"
               "HTTP/1.1 200 OK\r\ncontent-length: 11\r\n\r\nhello world"
               (read_all socket);
             Eio.Flow.close socket;
             Eio.Promise.resolve resolve_stop ()))))

let () =
  Alcotest.run
    "ache-eio loopback"
    [ ( "loopback"
      , [ "GET", `Quick, test_get
        ; "POST echo larger than the read buffer", `Quick, test_post_echo
        ; "two pipelined requests", `Quick, test_pipelined
        ; "101 upgrade to an echo protocol", `Quick, test_upgrade
        ; ( "an incomplete token sent with the upgrade request reaches the new \
             protocol"
          , `Quick
          , test_upgrade_incomplete_token_reaches_new_protocol )
        ; "read failure ends the connection", `Quick, test_read_failure
        ; "write failure ends the connection", `Quick, test_write_failure
        ; "peer closes mid-body", `Quick, test_eof_mid_body
        ; ( "flush calls back with `Closed after the peer closes"
          , `Quick
          , test_flush_after_peer_closed )
        ; ( "run_server takes the partially applied handler"
          , `Quick
          , test_run_server )
        ; ( "normal close with trailing bytes shuts each direction once"
          , `Quick
          , test_close_with_trailing_bytes )
        ; "request handler exception", `Quick, test_handler_exception
        ; ( "cancelling the connection fiber is not an error"
          , `Quick
          , test_cancel_is_not_an_error )
        ] )
    ; ( "timeouts"
      , [ "idle connection is closed", `Quick, test_idle_timeout
        ; ( "connection idle between requests is closed"
          , `Quick
          , test_idle_timeout_between_requests )
        ; "incomplete head is rejected", `Quick, test_head_timeout
        ; known_failure
            ~ticket:"12"
            "incomplete head gets 408 Request Timeout"
            test_head_timeout_408
        ; ( "head timeout counts from the first byte of the head"
          , `Quick
          , test_head_deadline )
        ; "slow request body is not cut", `Quick, test_slow_body_not_cut
        ; "no timeout configured", `Quick, test_no_timeout_configured
        ; "a timeout needs a clock", `Quick, test_timeout_needs_clock
        ] )
    ]
