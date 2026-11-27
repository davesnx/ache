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
    if len > 0 then t.on_read t (Ache.Bigstring.substring bs ~off ~len);
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
        [ { IOVec.buffer = Ache.Bigstring.of_string t.pending ~off:0 ~len
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
let partial_body_cut = ref false
let stream_flush_closed : unit Eio.Promise.u option ref = ref None
let requests_seen = ref 0
let on_ws_closed : (int option -> unit) ref = ref ignore
let pending_upgrade : Request_descriptor.t option ref = ref None
let pending_response : Request_descriptor.t option ref = ref None
let pending_body_failed = ref false
let pending_body_bytes = Stdlib.Buffer.create 16
let later_reqd : Request_descriptor.t option ref = ref None
let big_size = 16 * 1024 * 1024

exception Respond_later

let late_error_response : (unit -> unit) option ref = ref None
let ws_reports : string Eio.Stream.t = Eio.Stream.create max_int
let callbacks_ws : Ache.Websocket.t option ref = ref None
let text_of = function Ache.Websocket.Text s -> s | Binary s -> "binary:" ^ s

let ws_echo ws =
  let module W = Ache_eio.Websocket in
  let rec loop () =
    match W.receive ws with
    | Some (Text "close-me") ->
      W.close ws 1001;
      Eio.Stream.add
        ws_reports
        (match W.receive ws, W.client_close ws with
        | None, None -> "closed, no client close"
        | None, Some { code; reason } ->
          Printf.sprintf "closed, client close %d %S" code reason
        | Some m, _ -> "received " ^ text_of m)
    | Some (Text "ping-me") ->
      W.ping ws "are you there";
      W.send_text ws "pong seen";
      loop ()
    | Some (Text "bye-reason") ->
      W.close ~reason:"done here" ws 4000;
      Eio.Stream.add ws_reports "closed with a reason"
    | Some (Text message) ->
      W.send_text ws message;
      loop ()
    | Some (Binary message) ->
      W.send_binary ws message;
      loop ()
    | None ->
      Eio.Stream.add
        ws_reports
        (match W.client_close ws with
        | None -> "receive None"
        | Some { code; reason } ->
          Printf.sprintf "receive None, client close %d %S" code reason)
  in
  loop ()

let ws_send_loop ws =
  let module W = Ache_eio.Websocket in
  let chunk = String.make 4096 'x' in
  let rec loop () =
    W.send_text ws chunk;
    loop ()
  in
  try loop () with
  | W.Closed ->
    Eio.Stream.add
      ws_reports
      (match W.receive ws with
      | None -> "send raised Closed, receive None"
      | Some _ -> "a message after the reset")

let request_handler (_ : Eio.Net.Sockaddr.stream) reqd =
  incr requests_seen;
  let request = Request_descriptor.request reqd in
  let body = Request_descriptor.request_body reqd in
  match request.meth, request.target with
  | `GET, "/hello" ->
    Body.Reader.close body;
    Request_descriptor.respond_with_string
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
      Request_descriptor.respond_with_streaming
        reqd
        (Response.create ~headers `OK)
    in
    let rec on_read buffer ~off ~len =
      Body.Writer.write_string
        response_body
        (Ache.Bigstring.substring buffer ~off ~len);
      Body.Writer.flush response_body (fun _ ->
        Body.Reader.schedule_read body ~on_eof ~on_error ~on_read)
    and on_eof () = Body.Writer.close response_body
    and on_error () = failwith "request body cut short" in
    Body.Reader.schedule_read body ~on_eof ~on_error ~on_read
  | `GET, "/upgrade" ->
    Body.Reader.close body;
    Request_descriptor.respond_with_upgrade
      reqd
      (Headers.of_list [ "connection", "upgrade"; "upgrade", "echo" ])
      (echo_server ())
  | `GET, "/ws-flood" ->
    (match
       Websocket.upgrade reqd (fun ws ->
         let chunk = String.make 4096 'x' in
         let rec loop () =
           Websocket.send_text ws chunk;
           Websocket.flush ws (function `Written -> loop () | `Closed -> ())
         in
         loop ();
         { on_message = ignore
         ; on_pong = ignore
         ; on_close =
             (fun close ->
               !on_ws_closed
                 (Option.map (fun { Websocket.code; _ } -> code) close))
         })
     with
    | Ok () -> ()
    | Error _ -> Alcotest.fail "handshake rejected")
  | `GET, "/ws-callbacks" ->
    (* The callback API of the core. *)
    (match
       Ache.Websocket.upgrade reqd (fun ws ->
         callbacks_ws := Some ws;
         { on_message =
             (function
               | Text "close-me" -> Ache.Websocket.close ws ~code:1000
               | Text _ | Binary _ -> ())
         ; on_pong = ignore
         ; on_close =
             (fun close ->
               Eio.Stream.add
                 ws_reports
                 (match close with
                 | None -> "on_close None"
                 | Some { code; _ } -> Printf.sprintf "on_close %d" code))
         })
     with
    | Ok () -> ()
    | Error _ -> Alcotest.fail "handshake rejected")
  | `GET, (("/ws-echo" | "/ws-send-loop") as target) ->
    let session = if target = "/ws-echo" then ws_echo else ws_send_loop in
    (match Ache_eio.Websocket.upgrade reqd session with
    | Ok () -> ()
    | Error _ -> Alcotest.fail "handshake rejected")
  | `GET, "/stream" ->
    Body.Reader.close body;
    let response_body =
      Request_descriptor.respond_with_streaming reqd (Response.create `OK)
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
  | `GET, "/upgrade-later" -> pending_upgrade := Some reqd
  | `CONNECT, _ ->
    Request_descriptor.respond_with_upgrade reqd Headers.empty (echo_server ())
  | `POST, "/early-close" ->
    Request_descriptor.respond_with_string
      reqd
      (Response.create ~headers:(Headers.of_list [ "connection", "close" ]) `OK)
      "no"
  | `POST, "/early-413" ->
    Request_descriptor.respond_with_string
      reqd
      (Response.create `Content_too_large)
      "no"
  | `POST, "/respond-later" ->
    let rec on_read buffer ~off ~len =
      Stdlib.Buffer.add_string
        pending_body_bytes
        (Ache.Bigstring.substring buffer ~off ~len);
      Body.Reader.schedule_read body ~on_eof ~on_error ~on_read
    and on_eof () = Alcotest.fail "a cut body completed"
    and on_error () = pending_body_failed := true in
    Body.Reader.schedule_read body ~on_eof ~on_error ~on_read;
    pending_response := Some reqd
  | `POST, "/respond-later-raise" ->
    let rec on_read _ ~off:_ ~len:_ =
      Body.Reader.schedule_read body ~on_eof ~on_error ~on_read
    and on_eof () = Alcotest.fail "a cut body completed"
    and on_error () = failwith "body on_error" in
    Body.Reader.schedule_read body ~on_eof ~on_error ~on_read;
    pending_response := Some reqd
  | `POST, "/respond-later-then-body" ->
    let rec on_read _ ~off:_ ~len:_ =
      Body.Reader.schedule_read body ~on_eof ~on_error ~on_read
    and on_eof () = ()
    and on_error () = pending_body_failed := true in
    Body.Reader.schedule_read body ~on_eof ~on_error ~on_read;
    pending_response := Some reqd
  | `GET, "/later" ->
    Body.Reader.close body;
    later_reqd := Some reqd
  | `GET, "/big" ->
    Body.Reader.close body;
    let response_body =
      Request_descriptor.respond_with_streaming
        reqd
        (Response.create
           ~headers:
             (Headers.of_list [ "content-length", string_of_int big_size ])
           `OK)
    in
    let chunk = String.make (1024 * 1024) 'x' in
    let rec loop n =
      if n = 0
      then Body.Writer.close response_body
      else (
        Body.Writer.write_string response_body chunk;
        Body.Writer.flush response_body (function
          | `Written -> loop (n - 1)
          | `Closed -> ()))
    in
    loop (big_size / String.length chunk)
  | `GET, "/raise" -> failwith "handler failed"
  | `GET, "/raise-later" -> raise Respond_later
  | `POST, "/partial" ->
    let rec on_read _ ~off:_ ~len:_ =
      Body.Reader.schedule_read body ~on_eof ~on_error ~on_read
    and on_eof () = Alcotest.fail "a cut body completed"
    and on_error () = partial_body_cut := true in
    Body.Reader.schedule_read body ~on_eof ~on_error ~on_read
  | _, target ->
    Body.Reader.close body;
    Request_descriptor.respond_with_string
      reqd
      (Response.create
         ~headers:
           (Headers.of_list
              [ "content-length", string_of_int (String.length target) ])
         `OK)
      target

let error_handler (_ : Eio.Net.Sockaddr.stream) ?request:_ error start_response =
  server_errors := error :: !server_errors;
  match error with
  | `Exn Respond_later ->
    late_error_response :=
      Some (fun () -> Body.Writer.close (start_response Headers.empty))
  | _ -> Body.Writer.close (start_response Headers.empty)

let connections_done : unit Eio.Stream.t = Eio.Stream.create max_int
let real_sleep : (float -> unit) ref = ref (fun (_ : float) -> ())
let real_clock : float Eio.Time.clock_ty Eio.Resource.t ref = ref (Obj.magic ())

let with_server
      ?(flow = fun socket -> (socket :> Eio.Flow.two_way_ty Eio.Resource.t))
      ?clock
      ?idle_timeout
      ?head_timeout
      ?close_timeout
      ?body_timeout
      ?write_timeout
      ?(date = false)
      f
  =
  server_errors := [];
  requests_seen := 0;
  Eio_main.run (fun env ->
    while Option.is_some (Eio.Stream.take_nonblocking connections_done) do
      ()
    done;
    real_sleep := Eio.Time.sleep (Eio.Stdenv.clock env);
    real_clock :=
      (Eio.Stdenv.clock env :> float Eio.Time.clock_ty Eio.Resource.t);
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
                   ~date
                   ?clock
                   ~idle_timeout
                   ~head_timeout
                   ~close_timeout
                   ~body_timeout
                   ~write_timeout
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
    Stdlib.Buffer.add_string buf (Ache.Bigstring.substring bs ~off ~len);
    Body.Reader.schedule_read body ~on_eof ~on_error ~on_read
  and on_eof () = Eio.Promise.resolve u (Stdlib.Buffer.contents buf)
  and on_error () = Eio.Promise.resolve u "<response body cut short>" in
  Body.Reader.schedule_read body ~on_eof ~on_error ~on_read;
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

(* [value] is an IMF-fixdate (RFC 9110 §5.6.7) within a few seconds of now. *)
let check_date_value value =
  let digits from len =
    String.for_all
      (function '0' .. '9' -> true | _ -> false)
      (String.sub value from len)
  in
  let one_of from names = List.mem (String.sub value from 3) names in
  let format_ok =
    String.length value = 29
    && one_of 0 [ "Mon"; "Tue"; "Wed"; "Thu"; "Fri"; "Sat"; "Sun" ]
    && String.sub value 3 2 = ", "
    && digits 5 2
    && value.[7] = ' '
    && one_of
         8
         [ "Jan"
         ; "Feb"
         ; "Mar"
         ; "Apr"
         ; "May"
         ; "Jun"
         ; "Jul"
         ; "Aug"
         ; "Sep"
         ; "Oct"
         ; "Nov"
         ; "Dec"
         ]
    && value.[11] = ' '
    && digits 12 4
    && value.[16] = ' '
    && digits 17 2
    && value.[19] = ':'
    && digits 20 2
    && value.[22] = ':'
    && digits 23 2
    && String.sub value 25 4 = " GMT"
  in
  if not format_ok then Alcotest.failf "Date %S is not an IMF-fixdate" value;
  let now = int_of_float (Unix.gettimeofday ()) in
  let near =
    List.init 11 (fun i ->
      Ache.Ache_private.Http_date.imf_fixdate (now - 5 + i))
  in
  if not (List.mem value near)
  then Alcotest.failf "Date %S is not within 5 s of now" value

let test_date_header () =
  List.iter
    (fun date ->
       with_server ~date (fun ~sw ~net addr ->
         let client =
           Ache_eio.Client.create_connection ~sw (Eio.Net.connect ~sw net addr)
         in
         let response, _ =
           request
             client
             (Request.create
                ~headers:(Headers.of_list [ "host", "localhost" ])
                `GET
                "/hello")
         in
         (match date, Headers.get_multi response.headers "date" with
         | true, [ value ] -> check_date_value value
         | false, [] -> ()
         | _, values ->
           Alcotest.failf
             "date:%b: Date fields %s"
             date
             (String.concat ", " values));
         Eio.Promise.await (Ache_eio.Client.shutdown client)))
    [ true; false ]

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
       connection: close\r\n\
       \r\n\
       /b"
      output)

let test_many_requests_bounded_stack () =
  let requests = 20_000 in
  let response = "HTTP/1.1 200 OK\r\ncontent-length: 2\r\n\r\n/a" in
  let old = Gc.get () in
  Fun.protect
    ~finally:(fun () -> Gc.set old)
    (fun () ->
       Gc.set { old with stack_limit = 16_384 };
       with_server (fun ~sw ~net addr ->
         let socket = Eio.Net.connect ~sw net addr in
         let reader = Eio.Buf_read.of_flow socket ~max_size:65536 in
         let received = ref 0 in
         (try
            for _ = 1 to requests do
              Eio.Flow.copy_string
                "GET /a HTTP/1.1\r\nhost: localhost\r\n\r\n"
                socket;
              if Eio.Buf_read.take (String.length response) reader = response
              then incr received
            done
          with
         | End_of_file -> ());
         Eio.Flow.close socket;
         Eio.Stream.take connections_done;
         Alcotest.(check int) "responses" requests !received;
         Alcotest.(check int) "server errors" 0 (List.length !server_errors)))

let test_connect_tunnel () =
  with_server (fun ~sw ~net addr ->
    let client =
      Ache_eio.Client.create_connection ~sw (Eio.Net.connect ~sw net addr)
    in
    let answered, resolve_answered = Eio.Promise.create () in
    let request_body =
      Ache_eio.Client.request
        client
        (Request.create
           ~headers:(Headers.of_list [ "host", "localhost:443" ])
           `CONNECT
           "localhost:443")
        ~error_handler:client_error_handler
        ~response_handler:(fun response _body ->
          Eio.Promise.resolve resolve_answered response)
    in
    Body.Writer.close request_body;
    let response = Eio.Promise.await answered in
    Alcotest.(check int) "status" 200 (Status.to_code response.status);
    let message = "hello through the tunnel" in
    let echoed, resolve_echoed = Eio.Promise.create () in
    let received = Stdlib.Buffer.create 16 in
    let tunnel_client =
      Echo.create ~pending:message (fun t s ->
        Stdlib.Buffer.add_string received s;
        if Stdlib.Buffer.length received >= String.length message
        then (
          Echo.shutdown t;
          Eio.Promise.resolve resolve_echoed (Stdlib.Buffer.contents received)))
    in
    Ache_eio.Client.upgrade client (Runtime.make (module Echo) tunnel_client);
    Alcotest.(check string) "echoed" message (Eio.Promise.await echoed);
    Eio.Promise.await (Ache_eio.Client.shutdown client))

let test_connect_tunnel_bytes_after_head () =
  with_server (fun ~sw ~net addr ->
    let socket = Eio.Net.connect ~sw net addr in
    let reader = Eio.Buf_read.of_flow socket ~max_size:65536 in
    Eio.Flow.copy_string
      "CONNECT localhost:443 HTTP/1.1\r\nhost: localhost:443\r\n\r\nping"
      socket;
    let expected = "HTTP/1.1 200 OK\r\n\r\nping" in
    Alcotest.(check string)
      "200, then the bytes sent with the head come back"
      expected
      (Eio.Buf_read.take (String.length expected) reader);
    Eio.Flow.copy_string "pong" socket;
    Alcotest.(check string) "later bytes" "pong" (Eio.Buf_read.take 4 reader);
    Eio.Flow.close socket;
    Eio.Stream.take connections_done)

(* A closed connection gives its read and write buffers to the pool of the
   domain, and the next connection, which takes them, answers from its own bytes
   only. A CONNECT tunnel's protocol received slices of the read buffer: that
   buffer stays out of the pool. *)
let test_buffer_pool () =
  let pooled () =
    ( Ache.Ache_private.Buffer_pool.pooled Config.default.read_buffer_size
    , Ache.Ache_private.Buffer_pool.pooled Config.default.response_buffer_size )
  in
  with_server (fun ~sw ~net addr ->
    let exchange request =
      let socket = Eio.Net.connect ~sw net addr in
      Eio.Flow.copy_string request socket;
      let response = Eio.Buf_read.(parse_exn take_all) socket ~max_size:65536 in
      Eio.Stream.take connections_done;
      Eio.Flow.close socket;
      response
    in
    let hello =
      "HTTP/1.1 200 OK\r\n\
       content-length: 11\r\n\
       connection: close\r\n\
       \r\n\
       hello world"
    in
    (* Fills most of the read buffer, and outgrows the write buffer: the encoder
       replaces it (decision 0022). *)
    let target = "/" ^ String.make 1500 'Z' in
    let long =
      Printf.sprintf
        "GET %s HTTP/1.1\r\n\
         host: localhost\r\n\
         x: %s\r\n\
         connection: close\r\n\
         \r\n"
        target
        (String.make 2000 'Z')
    in
    Alcotest.(check string)
      "a long exchange"
      (Printf.sprintf
         "HTTP/1.1 200 OK\r\n\
          content-length: 1501\r\n\
          connection: close\r\n\
          \r\n\
          %s"
         target)
      (exchange long);
    let read, write = pooled () in
    Alcotest.(check bool) "read buffer given back" true (read >= 1);
    Alcotest.(check bool) "write buffer given back" true (write >= 1);
    Alcotest.(check string)
      "a connection on the given back buffers"
      hello
      (exchange
         "GET /hello HTTP/1.1\r\nhost: localhost\r\nconnection: close\r\n\r\n");
    Alcotest.(check (pair int int))
      "buffers given back again"
      (read, write)
      (pooled ());
    let socket = Eio.Net.connect ~sw net addr in
    let reader = Eio.Buf_read.of_flow socket ~max_size:65536 in
    Eio.Flow.copy_string
      "CONNECT localhost:443 HTTP/1.1\r\nhost: localhost:443\r\n\r\nping"
      socket;
    let expected = "HTTP/1.1 200 OK\r\n\r\nping" in
    Alcotest.(check string)
      "tunnel"
      expected
      (Eio.Buf_read.take (String.length expected) reader);
    Eio.Flow.close socket;
    Eio.Stream.take connections_done;
    Alcotest.(check (pair int int))
      "a tunnel keeps its read buffer"
      (read - 1, write)
      (pooled ()))

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
  let failing_shutdowns : Eio.Flow.shutdown_command list ref = ref []

  let shutdown t cmd =
    shutdowns := cmd :: !shutdowns;
    if List.mem cmd !failing_shutdowns
    then failwith "shutdown failed"
    else Eio.Flow.shutdown t.inner cmd

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
  partial_body_cut := false;
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
  Alcotest.(check bool) "request body cut" true !partial_body_cut;
  match !server_errors with
  | [ `Bad_request _ ] -> ()
  | _ ->
    Alcotest.failf
      "expected one Bad_request, got %d errors"
      (List.length !server_errors)

let ws_flood_request =
  "GET /ws-flood HTTP/1.1\r\n\
   host: localhost\r\n\
   upgrade: websocket\r\n\
   connection: Upgrade\r\n\
   sec-websocket-key: dGhlIHNhbXBsZSBub25jZQ==\r\n\
   sec-websocket-version: 13\r\n\
   \r\n"

let ws_101 =
  "HTTP/1.1 101 Switching Protocols\r\n\
   upgrade: websocket\r\n\
   connection: Upgrade\r\n\
   sec-websocket-accept: s3pPLMBiTxaQ9kYGzzhZRbK+xOo=\r\n\
   \r\n"

let test_websocket_peer_reset () =
  let closed, resolve_closed = Eio.Promise.create () in
  on_ws_closed := Eio.Promise.resolve resolve_closed;
  with_server (fun ~sw ~net addr ->
    let socket = Eio.Net.connect ~sw net addr in
    Eio.Flow.copy_string ws_flood_request socket;
    let reader = Eio.Buf_read.of_flow socket ~max_size:65536 in
    Alcotest.(check string)
      "101"
      ws_101
      (Eio.Buf_read.take (String.length ws_101) reader);
    Alcotest.(check string)
      "server is sending"
      "\x81\x7e\x10\x00"
      (Eio.Buf_read.take 4 reader);
    Eio.Net.setsockopt socket Eio.Net.Sockopt.SO_LINGER (Some 0);
    let reset_at = Unix.gettimeofday () in
    Eio.Flow.close socket;
    Eio.Fiber.first
      (fun () ->
         Alcotest.(check (option int))
           "on_close without a close frame"
           None
           (Eio.Promise.await closed);
         Eio.Stream.take connections_done)
      (fun () ->
         !real_sleep 2.0;
         Alcotest.fail "still running 2 s after the reset");
    let elapsed = Unix.gettimeofday () -. reset_at in
    Alcotest.(check bool)
      (Printf.sprintf "ended within 1 s (%.3f s)" elapsed)
      true
      (elapsed < 1.0));
  on_ws_closed := ignore

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

let rec advance clock =
  if not (Eio_mock.Clock.try_advance clock)
  then (
    !real_sleep 0.001;
    advance clock)

(* The server's watchdog wakes at deadlines that a read or write has since
   moved: advance past them to [time]. *)
let rec advance_to clock time =
  if Eio.Time.now clock < time
  then (
    advance clock;
    advance_to clock time)

let read_all socket =
  Eio.Buf_read.parse_exn Eio.Buf_read.take_all ~max_size:65536 socket

let get_hello = "GET /hello HTTP/1.1\r\nhost: localhost\r\n\r\n"

(* A connection handler returns within [seconds] of [from]. *)
let check_done_within seconds ~from =
  match
    Eio.Fiber.first
      (fun () ->
         Eio.Stream.take connections_done;
         Some (Unix.gettimeofday () -. from))
      (fun () ->
         !real_sleep 2.0;
         None)
  with
  | Some elapsed ->
    Alcotest.(check bool)
      (Printf.sprintf "connection ended within %.3f s (%.3f s)" seconds elapsed)
      true
      (elapsed < seconds)
  | None -> Alcotest.fail "the connection did not end"

let hello_response = "HTTP/1.1 200 OK\r\ncontent-length: 11\r\n\r\nhello world"

let hello_response_close =
  "HTTP/1.1 200 OK\r\n\
   content-length: 11\r\n\
   connection: close\r\n\
   \r\n\
   hello world"

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
    (* No request is in flight: the connection does not linger (decision
       0023). *)
    Eio.Stream.take connections_done;
    Alcotest.(check (float 0.)) "no linger" 0.1 (Eio.Time.now clock);
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

(* The read that the head timeout ends still drops input, until the linger
   deadline. *)
let test_head_timeout_lingers () =
  let clock = Eio_mock.Clock.make () in
  Failing_flow.shutdowns := [];
  with_server
    ~flow:counting_flow
    ~clock
    ~head_timeout:0.1
    ~idle_timeout:0.5
    (fun ~sw ~net addr ->
       let socket = Eio.Net.connect ~sw net addr in
       let reader = Eio.Buf_read.of_flow socket ~max_size:65536 in
       Eio.Flow.copy_string "GET /hello HTTP/1.1\r\nHo" socket;
       !real_sleep 0.05;
       advance clock;
       Alcotest.(check (float 0.))
         "fired at the head timeout"
         0.1
         (Eio.Time.now clock);
       Alcotest.(check string)
         "status line"
         "HTTP/1.1 408 Request Timeout"
         (Eio.Buf_read.line reader);
       Eio.Flow.copy_string "st: localhost\r\n\r\n" socket;
       !real_sleep 0.05;
       advance clock;
       Alcotest.(check (float 0.))
         "lingers for idle_timeout after the 408"
         0.6
         (Eio.Time.now clock);
       ignore (Eio.Buf_read.take_all reader : string);
       Eio.Flow.close socket;
       Eio.Stream.take connections_done);
  Alcotest.(check int) "request handler not called" 0 !requests_seen;
  check_shutdowns ()

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
    (* The watchdog wakes once at the idle deadline of the first read. *)
    ignore (Eio_mock.Clock.try_advance clock : bool);
    !real_sleep 0.01;
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

let unread_body_request target =
  Printf.sprintf
    "POST %s HTTP/1.1\r\nhost: localhost\r\ncontent-length: 100000\r\n\r\nabc"
    target

let early_413_response =
  "HTTP/1.1 413 Content Too Large\r\ncontent-length: 2\r\n\r\nno"

let test_unread_body_without_body_timeout () =
  with_server (fun ~sw ~net addr ->
    let socket = Eio.Net.connect ~sw net addr in
    let reader = Eio.Buf_read.of_flow socket ~max_size:65536 in
    Eio.Flow.copy_string (unread_body_request "/early-413") socket;
    Alcotest.(check string)
      "response"
      early_413_response
      (Eio.Buf_read.take (String.length early_413_response) reader);
    !real_sleep 0.1;
    Eio.Flow.copy_string (String.make (100000 - 3) 'x') socket;
    Eio.Flow.copy_string get_hello socket;
    Alcotest.(check string)
      "the body is read, then the next request"
      hello_response
      (Eio.Buf_read.take (String.length hello_response) reader);
    Eio.Flow.close socket;
    Eio.Stream.take connections_done)

(* Decision 0010: the server drains at most 256 KiB of an unread body. *)
let large_413_head =
  "POST /early-413 HTTP/1.1\r\n\
   host: localhost\r\n\
   content-length: 10000000\r\n\
   \r\n"

let large_413_response =
  "HTTP/1.1 413 Content Too Large\r\n\
   content-length: 2\r\n\
   connection: close\r\n\
   \r\n\
   no"

let test_unread_body_drain_limit () =
  let clock = Eio_mock.Clock.make () in
  with_server ~clock ~idle_timeout:0.1 (fun ~sw ~net addr ->
    let socket = Eio.Net.connect ~sw net addr in
    let reader = Eio.Buf_read.of_flow socket ~max_size:65536 in
    Eio.Flow.copy_string large_413_head socket;
    Alcotest.(check string)
      "413 closes"
      large_413_response
      (Eio.Buf_read.take (String.length large_413_response) reader);
    (try Eio.Flow.copy_string (String.make 0x50000 'x') socket with
    | Eio.Io _ -> ());
    Alcotest.(check bool)
      "closed before the idle timeout"
      true
      (try Eio.Buf_read.at_end_of_input reader with Eio.Io _ -> true);
    Alcotest.(check (float 0.)) "no time passed" 0. (Eio.Time.now clock);
    Eio.Flow.close socket;
    Eio.Stream.take connections_done)

let test_unread_body_timeout () =
  let clock = Eio_mock.Clock.make () in
  with_server ~clock ~body_timeout:0.1 (fun ~sw ~net addr ->
    let socket = Eio.Net.connect ~sw net addr in
    let reader = Eio.Buf_read.of_flow socket ~max_size:65536 in
    Eio.Flow.copy_string (unread_body_request "/early-413") socket;
    Alcotest.(check string)
      "response"
      early_413_response
      (Eio.Buf_read.take (String.length early_413_response) reader);
    advance clock;
    Alcotest.(check (float 0.))
      "closed at the body timeout"
      0.1
      (Eio.Time.now clock);
    Alcotest.(check bool) "closed" true (Eio.Buf_read.at_end_of_input reader);
    Eio.Flow.close socket;
    Eio.Stream.take connections_done)

let respond_while_body_read_pending ?clock () =
  pending_response := None;
  pending_body_failed := false;
  with_server ?clock ~body_timeout:0.1 (fun ~sw ~net addr ->
    let socket = Eio.Net.connect ~sw net addr in
    let reader = Eio.Buf_read.of_flow socket ~max_size:65536 in
    Eio.Flow.copy_string (unread_body_request "/respond-later") socket;
    let rec respond () =
      match !pending_response with
      | Some reqd ->
        Request_descriptor.respond_with_string reqd (Response.create `OK) "late"
      | None ->
        !real_sleep 0.001;
        respond ()
    in
    respond ();
    let response = "HTTP/1.1 200 OK\r\ncontent-length: 4\r\n\r\nlate" in
    Alcotest.(check string)
      "response"
      response
      (Eio.Buf_read.take (String.length response) reader);
    Option.iter
      (fun clock ->
         advance clock;
         Alcotest.(check (float 0.))
           "closed at the body timeout"
           0.1
           (Eio.Time.now clock))
      clock;
    Alcotest.(check bool) "closed" true (Eio.Buf_read.at_end_of_input reader);
    Eio.Flow.close socket;
    Eio.Stream.take connections_done);
  Alcotest.(check bool) "request body failed" true !pending_body_failed

let test_respond_while_body_read_pending () = respond_while_body_read_pending ()

let respond_when_pending () =
  let rec respond () =
    match !pending_response with
    | Some reqd ->
      pending_response := None;
      !real_sleep 0.001;
      Request_descriptor.respond_with_string reqd (Response.create `OK) "late"
    | None ->
      !real_sleep 0.001;
      respond ()
  in
  respond ()

let late_response = "HTTP/1.1 200 OK\r\ncontent-length: 4\r\n\r\nlate"

let body_on_error_raises ?clock () =
  pending_response := None;
  with_server ?clock ~body_timeout:0.1 (fun ~sw ~net addr ->
    let socket = Eio.Net.connect ~sw net addr in
    let reader = Eio.Buf_read.of_flow socket ~max_size:65536 in
    Eio.Flow.copy_string (unread_body_request "/respond-later-raise") socket;
    respond_when_pending ();
    Alcotest.(check string)
      "response"
      late_response
      (Eio.Buf_read.take (String.length late_response) reader);
    Option.iter advance clock;
    Alcotest.(check bool) "closed" true (Eio.Buf_read.at_end_of_input reader);
    Eio.Flow.close socket;
    Eio.Stream.take connections_done)

let test_body_on_error_raises () = body_on_error_raises ()

let test_body_on_error_raises_idle_timeout () =
  body_on_error_raises ~clock:(Eio_mock.Clock.make ()) ()

let live_words () =
  Gc.full_major ();
  (Gc.stat ()).live_words

let test_body_read_stops_bounded () =
  let requests = 200 in
  let growth = ref 0 in
  pending_response := None;
  pending_body_failed := false;
  with_server
    ~clock:(Eio_mock.Clock.make ())
    ~body_timeout:30.
    (fun ~sw ~net addr ->
       let socket = Eio.Net.connect ~sw net addr in
       Eio.Net.setsockopt socket Eio.Net.Sockopt.TCP_NODELAY true;
       let reader = Eio.Buf_read.of_flow socket ~max_size:65536 in
       let exchange () =
         Eio.Flow.copy_string
           "POST /respond-later-then-body HTTP/1.1\r\n\
            host: localhost\r\n\
            content-length: 6\r\n\
            \r\n\
            abc"
           socket;
         respond_when_pending ();
         Alcotest.(check string)
           "response"
           late_response
           (Eio.Buf_read.take (String.length late_response) reader);
         Eio.Flow.copy_string "def" socket
       in
       for _ = 1 to requests / 10 do
         exchange ()
       done;
       let before = live_words () in
       for _ = 1 to requests do
         exchange ()
       done;
       growth := live_words () - before;
       Eio.Flow.close socket;
       Eio.Stream.take connections_done);
  Alcotest.(check bool) "no body failed" false !pending_body_failed;
  if !growth > requests
  then Alcotest.failf "heap grew by %d words over %d requests" !growth requests

let test_respond_while_body_read_pending_mock_clock () =
  respond_while_body_read_pending ~clock:(Eio_mock.Clock.make ()) ()

module Gated_flow = struct
  type t = { inner : Eio.Flow.two_way_ty Eio.Resource.t }

  let bytes_read = ref 0
  let gate : (int * unit Eio.Promise.t * string) option ref = ref None
  let read_methods = []

  let single_read t buf =
    match !gate with
    | Some (after, released, data) when !bytes_read >= after ->
      gate := None;
      Eio.Promise.await released;
      Cstruct.blit_from_string data 0 buf 0 (String.length data);
      String.length data
    | _ ->
      let n = Eio.Flow.single_read t.inner buf in
      bytes_read := !bytes_read + n;
      n

  let single_write t bufs = Eio.Flow.single_write t.inner bufs
  let copy t ~src = Eio.Flow.Pi.simple_copy ~single_write t ~src
  let shutdown t cmd = Eio.Flow.shutdown t.inner cmd

  let wrap socket =
    Eio.Resource.T
      ( { inner = (socket :> Eio.Flow.two_way_ty Eio.Resource.t) }
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

let test_body_bytes_at_the_deadline () =
  let clock = Eio_mock.Clock.make () in
  let request = unread_body_request "/respond-later" in
  let released, release = Eio.Promise.create () in
  pending_response := None;
  pending_body_failed := false;
  Stdlib.Buffer.clear pending_body_bytes;
  Gated_flow.bytes_read := 0;
  Gated_flow.gate := Some (String.length request, released, "def");
  with_server
    ~flow:Gated_flow.wrap
    ~clock
    ~body_timeout:0.1
    (fun ~sw ~net addr ->
       let socket = Eio.Net.connect ~sw net addr in
       let reader = Eio.Buf_read.of_flow socket ~max_size:65536 in
       Eio.Flow.copy_string request socket;
       let rec respond () =
         match !pending_response with
         | Some reqd ->
           !real_sleep 0.05;
           Request_descriptor.respond_with_string
             reqd
             (Response.create `OK)
             "late"
         | None ->
           !real_sleep 0.001;
           respond ()
       in
       respond ();
       let response = "HTTP/1.1 200 OK\r\ncontent-length: 4\r\n\r\nlate" in
       Alcotest.(check string)
         "response"
         response
         (Eio.Buf_read.take (String.length response) reader);
       Eio_mock.Clock.set_time clock 0.1;
       Eio.Promise.resolve release ();
       Alcotest.(check bool) "closed" true (Eio.Buf_read.at_end_of_input reader);
       Eio.Flow.close socket;
       Eio.Stream.take connections_done);
  Alcotest.(check string)
    "body bytes before the deadline"
    "abc"
    (Stdlib.Buffer.contents pending_body_bytes);
  Alcotest.(check bool) "request body failed" true !pending_body_failed

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

let test_default_timeouts () =
  let run ?idle_timeout input check =
    server_errors := [];
    pending_body_failed := false;
    Eio_main.run (fun env ->
      real_sleep := Eio.Time.sleep (Eio.Stdenv.clock env);
      Eio.Switch.run (fun sw ->
        let clock = Eio_mock.Clock.make () in
        let server, client = Eio_unix.Net.socketpair_stream ~sw () in
        Eio.Fiber.both
          (fun () ->
             Ache_eio.Server.create_connection_handler
               ~date:false
               ~clock
               ?idle_timeout
               ~request_handler
               ~error_handler
               server
               (`Unix "socketpair"))
          (fun () ->
             Eio.Flow.copy_string input client;
             !real_sleep 0.05;
             check clock client;
             Eio.Flow.close client)))
  in
  run "" (fun clock client ->
    advance clock;
    Alcotest.(check (float 0.))
      "idle: 1 s after the hint"
      61.
      (Eio.Time.now clock);
    Alcotest.(check string) "idle: closed" "" (read_all client));
  run "GET /hello HTTP/1.1\r\nHo" (fun clock client ->
    advance clock;
    Alcotest.(check (float 0.)) "head" 30. (Eio.Time.now clock);
    Alcotest.(check string)
      "head: 408"
      "HTTP/1.1 408 Request Timeout"
      (status_line (read_all client)));
  run
    "POST /respond-later HTTP/1.1\r\nhost: a\r\ncontent-length: 10\r\n\r\nabc"
    (fun clock client ->
       advance clock;
       Alcotest.(check (float 0.)) "body" 60. (Eio.Time.now clock);
       Alcotest.(check string)
         "body: 408"
         "HTTP/1.1 408 Request Timeout"
         (status_line (read_all client));
       Alcotest.(check bool) "body: on_error" true !pending_body_failed);
  run ~idle_timeout:None "" (fun clock _ ->
    Alcotest.(check bool)
      "~idle_timeout:None: no timer"
      false
      (Eio_mock.Clock.try_advance clock))

let test_timeout_check_timing () =
  let addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, 0) in
  let raises f =
    match f () with () -> false | exception Invalid_argument _ -> true
  in
  Eio_mock.Backend.run (fun () ->
    let flow = Eio_mock.Flow.make "flow" in
    let bound =
      Ache_eio.Server.create_connection_handler
        ~date:false
        ~idle_timeout:(Some (-1.))
        ~request_handler
        ~error_handler
    in
    Alcotest.(check bool)
      "bound: raises on a flow"
      true
      (raises (fun () -> bound flow addr));
    let take (h : Eio_mock.Flow.t -> Eio.Net.Sockaddr.stream -> unit) = h in
    let inline = ref None in
    Alcotest.(check bool)
      "inline: no raise when passed"
      false
      (raises (fun () ->
         inline :=
           Some
             (take
                (Ache_eio.Server.create_connection_handler
                   ~date:false
                   ~idle_timeout:(Some (-1.))
                   ~request_handler
                   ~error_handler))));
    Alcotest.(check bool)
      "inline: raises on a flow"
      true
      (raises (fun () -> Option.get !inline flow addr));
    Alcotest.(check bool)
      "every optional argument given: raises at once"
      true
      (raises (fun () ->
         ignore
           (Ache_eio.Server.create_connection_handler
              ~date:false
              ~config:Ache.Config.default
              ~clock:(Eio_mock.Clock.make ())
              ~idle_timeout:(Some (-1.))
              ~head_timeout:(Some 1.)
              ~close_timeout:(Some 1.)
              ~body_timeout:None
              ~write_timeout:None
              ~stop:(Eio.Promise.create_resolved ())
              ~grace:1.
              ~request_handler
              ~error_handler
            : _ -> _ -> unit)));
    Alcotest.(check bool)
      "close_timeout not positive: raises"
      true
      (raises (fun () ->
         Ache_eio.Server.create_connection_handler
           ~date:false
           ~close_timeout:(Some 0.)
           ~request_handler
           ~error_handler
           flow
           addr)))

let test_cancel_is_not_an_error () =
  server_errors := [];
  Eio_main.run (fun _env ->
    Eio.Switch.run (fun sw ->
      let server, client = Eio_unix.Net.socketpair_stream ~sw () in
      Eio.Flow.copy_string "GET /hello HTTP/1.1\r\nHo" client;
      Eio.Fiber.first
        (fun () ->
           Ache_eio.Server.create_connection_handler
             ~date:false
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
         hello_response_close
         (Eio.Buf_read.take (String.length hello_response_close) reader);
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

let rec respond_late () =
  match !late_error_response with
  | Some respond ->
    late_error_response := None;
    respond ()
  | None ->
    !real_sleep 0.01;
    respond_late ()

let test_late_error_response () =
  late_error_response := None;
  with_server (fun ~sw ~net addr ->
    let socket = Eio.Net.connect ~sw net addr in
    Eio.Flow.copy_string
      "GET /raise-later HTTP/1.1\r\nhost: localhost\r\n\r\n"
      socket;
    !real_sleep 0.05;
    respond_late ();
    Alcotest.(check string)
      "status line"
      "HTTP/1.1 500 Internal Server Error"
      (status_line (read_all socket));
    Eio.Flow.close socket;
    Eio.Stream.take connections_done)

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
                  ~date:false
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
               hello_response_close
               (read_all socket);
             Eio.Flow.close socket;
             Eio.Promise.resolve resolve_stop ()))))

let test_pipelined_head_deadline () =
  let clock = Eio_mock.Clock.make () in
  with_server ~clock ~head_timeout:0.5 (fun ~sw ~net addr ->
    let socket = Eio.Net.connect ~sw net addr in
    let reader = Eio.Buf_read.of_flow socket ~max_size:65536 in
    Eio.Flow.copy_string "GET /a HTTP/1.1\r\n" socket;
    !real_sleep 0.05;
    Eio_mock.Clock.set_time clock 0.4;
    Eio.Flow.copy_string "host: x\r\n\r\nGET /b HTTP/1.1\r\n" socket;
    let first = "HTTP/1.1 200 OK\r\ncontent-length: 2\r\n\r\n/a" in
    Alcotest.(check string)
      "first response"
      first
      (Eio.Buf_read.take (String.length first) reader);
    advance_to clock 0.9;
    Alcotest.(check (float 0.))
      "second head times out 0.5 s after its own first byte"
      0.9
      (Eio.Time.now clock);
    Alcotest.(check string)
      "second response"
      "HTTP/1.1 408 Request Timeout"
      (status_line (Eio.Buf_read.take_all reader));
    Eio.Flow.close socket;
    Eio.Stream.take connections_done)

(* The bytes after the closing request may be followed by more: the server
   lingers (decision 0023). *)
let close_without_reading_eof ?clock ?idle_timeout ?head_timeout () =
  with_server ?clock ?idle_timeout ?head_timeout (fun ~sw ~net addr ->
    let socket = Eio.Net.connect ~sw net addr in
    let reader = Eio.Buf_read.of_flow socket ~max_size:65536 in
    Eio.Flow.copy_string
      "GET /hello HTTP/1.1\r\n\
       host: localhost\r\n\
       connection: close\r\n\
       \r\n\
       GET /next"
      socket;
    Alcotest.(check string)
      "response"
      hello_response_close
      (Eio.Buf_read.take (String.length hello_response_close) reader);
    Option.iter
      (fun clock ->
         advance clock;
         Alcotest.(check (float 0.))
           "linger bound"
           Ache_eio.Server.linger_without_idle_timeout
           (Eio.Time.now clock))
      clock;
    Eio.Stream.take connections_done;
    Eio.Flow.close socket)

let test_linger_system_clock () = close_without_reading_eof ~idle_timeout:0.2 ()

(* A request read whole leaves nothing in flight: the server closes at once,
   although the client keeps its socket open (decision 0023). *)
let test_close_request_read_whole () =
  with_server ~idle_timeout:30. (fun ~sw ~net addr ->
    let socket = Eio.Net.connect ~sw net addr in
    let reader = Eio.Buf_read.of_flow socket ~max_size:65536 in
    let from = Unix.gettimeofday () in
    Eio.Flow.copy_string
      "GET /hello HTTP/1.1\r\nhost: localhost\r\nconnection: close\r\n\r\n"
      socket;
    Alcotest.(check string)
      "response"
      hello_response_close
      (Eio.Buf_read.take (String.length hello_response_close) reader);
    Alcotest.(check bool)
      "end of input"
      true
      (Eio.Buf_read.at_end_of_input reader);
    check_done_within 0.1 ~from;
    Eio.Flow.close socket)

let test_linger_bounded_without_idle_timeout () =
  close_without_reading_eof ~clock:(Eio_mock.Clock.make ()) ~head_timeout:5. ()

let with_failing_shutdown cmd f =
  Failing_flow.failing_shutdowns := [ cmd ];
  Fun.protect ~finally:(fun () -> Failing_flow.failing_shutdowns := []) f

let test_failing_shutdown cmd () =
  with_failing_shutdown cmd (fun () ->
    with_server ~flow:counting_flow (fun ~sw ~net addr ->
      let socket = Eio.Net.connect ~sw net addr in
      Eio.Flow.copy_string
        "GET /hello HTTP/1.1\r\nhost: localhost\r\nconnection: close\r\n\r\n"
        socket;
      Alcotest.(check string) "response" hello_response_close (read_all socket);
      Eio.Flow.close socket;
      Eio.Stream.take connections_done))

let test_failing_shutdown_all () =
  with_failing_shutdown `All (fun () ->
    with_server
      ~flow:(Failing_flow.wrap ~fail_read:true ~fail_write:false)
      (fun ~sw ~net addr ->
         get_until_closed ~sw ~net addr;
         Eio.Stream.take connections_done))

module Raising = struct
  type t = { mutable sent : bool }

  let next_read_operation _ = `Read
  let read _ _ ~off:_ ~len:_ = failwith "read"
  let read_eof _ _ ~off:_ ~len:_ = failwith "read_eof"
  let yield_reader _ _ = ()

  let next_write_operation t =
    if t.sent
    then `Yield
    else
      `Write
        [ { IOVec.buffer = Ache.Bigstring.of_string "ping" ~off:0 ~len:4
          ; off = 0
          ; len = 4
          }
        ]

  let report_write_result t _ = t.sent <- true
  let yield_writer _ _ = ()
  let report_exn _ _ = failwith "report_exn"
  let is_closed _ = false
  let shutdown _ = ()
end

let test_report_exn_raises () =
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
    let (_ : Response.t) = Eio.Promise.await upgraded in
    Ache_eio.Client.upgrade
      client
      (Runtime.make (module Raising) { Raising.sent = false });
    Eio.Promise.await (Ache_eio.Client.shutdown client))

module Poison_flow = struct
  type t = { inner : Eio.Flow.two_way_ty Eio.Resource.t }

  let bytes_read = ref 0
  let cancelled_reads = ref 0
  let ops_after_cancel = ref 0
  let send_shutdowns = ref 0

  let reset () =
    bytes_read := 0;
    cancelled_reads := 0;
    ops_after_cancel := 0;
    send_shutdowns := 0

  let check_poisoned () =
    if !cancelled_reads > 0
    then (
      incr ops_after_cancel;
      failwith "operation after a cancelled read")

  let read_methods = []

  let single_read t buf =
    check_poisoned ();
    match Eio.Flow.single_read t.inner buf with
    | n ->
      bytes_read := !bytes_read + n;
      n
    | exception (Eio.Cancel.Cancelled _ as exn) ->
      incr cancelled_reads;
      raise exn

  let single_write t bufs =
    check_poisoned ();
    Eio.Flow.single_write t.inner bufs

  let copy t ~src = Eio.Flow.Pi.simple_copy ~single_write t ~src
  let shutdown_delay : (unit -> unit) ref = ref ignore

  let shutdown t cmd =
    !shutdown_delay ();
    check_poisoned ();
    (match cmd with `Send | `All -> incr send_shutdowns | `Receive -> ());
    Eio.Flow.shutdown t.inner cmd

  let wrap socket =
    Eio.Resource.T
      ( { inner = (socket :> Eio.Flow.two_way_ty Eio.Resource.t) }
      , Eio.Flow.Pi.two_way
          (module struct
            type nonrec t = t

            let read_methods = read_methods
            let single_read = single_read
            let single_write = single_write
            let copy = copy
            let shutdown = shutdown
          end) )

  let check ~bytes_sent =
    Alcotest.(check int) "every byte read once" bytes_sent !bytes_read;
    Alcotest.(check int) "no read cancelled" 0 !cancelled_reads;
    Alcotest.(check int)
      "no operation after a cancelled read"
      0
      !ops_after_cancel
end

let read_head reader =
  let status = Eio.Buf_read.line reader in
  let rec skip () = if Eio.Buf_read.line reader <> "" then skip () in
  skip ();
  status

let test_head_timeout_then_data () =
  let clock = Eio_mock.Clock.make () in
  Poison_flow.reset ();
  let first = "GET /hello HTTP/1.1\r\nHo"
  and late = "st: localhost\r\n\r\nGET /hello HTTP/1.1\r\n\r\n" in
  with_server
    ~flow:Poison_flow.wrap
    ~clock
    ~head_timeout:0.1
    (fun ~sw ~net addr ->
       let socket = Eio.Net.connect ~sw net addr in
       let reader = Eio.Buf_read.of_flow socket ~max_size:65536 in
       Eio.Flow.copy_string first socket;
       advance clock;
       Alcotest.(check string)
         "status line"
         "HTTP/1.1 408 Request Timeout"
         (read_head reader);
       Eio.Flow.copy_string late socket;
       Eio.Flow.shutdown socket `Send;
       Alcotest.(check bool)
         "nothing more"
         true
         (Eio.Buf_read.at_end_of_input reader);
       Eio.Stream.take connections_done;
       Eio.Flow.close socket);
  Alcotest.(check int) "no request handled" 0 !requests_seen;
  Alcotest.(check int) "one error" 1 (List.length !server_errors);
  Poison_flow.check ~bytes_sent:(String.length first + String.length late)

let test_idle_timeout_then_request () =
  let clock = Eio_mock.Clock.make () in
  Poison_flow.reset ();
  with_server
    ~flow:Poison_flow.wrap
    ~clock
    ~idle_timeout:0.1
    (fun ~sw ~net addr ->
       let socket = Eio.Net.connect ~sw net addr in
       advance clock;
       !real_sleep 0.05;
       Eio.Flow.copy_string get_hello socket;
       Eio.Flow.shutdown socket `Send;
       Alcotest.(check string) "no response" "" (read_all socket);
       Eio.Stream.take connections_done;
       Eio.Flow.close socket);
  Alcotest.(check int) "no request handled" 0 !requests_seen;
  Alcotest.(check int) "no error" 0 (List.length !server_errors);
  (* The idle close does not linger (decision 0023): the receive shutdown ends
     the pending read before the request arrives, so nothing is read. *)
  Poison_flow.check ~bytes_sent:0

let test_client_shutdown_with_pending_read () =
  Poison_flow.reset ();
  let errors = ref [] in
  Eio_main.run (fun env ->
    Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 5.0 @@ fun () ->
    Eio.Switch.run (fun sw ->
      let client_end, server_end = Eio_unix.Net.socketpair_stream ~sw () in
      let client =
        Ache_eio.Client.create_connection ~sw (Poison_flow.wrap client_end)
      in
      let request_body =
        Ache_eio.Client.request
          client
          (Request.create
             ~headers:(Headers.of_list [ "host", "localhost" ])
             `GET
             "/")
          ~error_handler:(fun error -> errors := error :: !errors)
          ~response_handler:(fun _ _ -> Alcotest.fail "unexpected response")
      in
      Body.Writer.close request_body;
      let reader = Eio.Buf_read.of_flow server_end ~max_size:65536 in
      let (_ : string) = read_head reader in
      let partial = "HTTP/1.1 200 OK\r\ncontent-le" in
      Eio.Flow.copy_string partial server_end;
      Eio.Time.sleep (Eio.Stdenv.clock env) 0.05;
      (Poison_flow.shutdown_delay :=
         fun () -> Eio.Time.sleep (Eio.Stdenv.clock env) 0.05);
      Fun.protect
        ~finally:(fun () -> Poison_flow.shutdown_delay := ignore)
        (fun () -> Eio.Promise.await (Ache_eio.Client.shutdown client));
      Alcotest.(check bool)
        "peer sees the end of input"
        true
        (Eio.Buf_read.at_end_of_input reader);
      Alcotest.(check int)
        "bytes read"
        (String.length partial)
        !Poison_flow.bytes_read;
      Alcotest.(check int) "Send shutdown" 1 !Poison_flow.send_shutdowns;
      Alcotest.(check int)
        "no operation after a cancelled read"
        0
        !Poison_flow.ops_after_cancel));
  Alcotest.(check int) "one error" 1 (List.length !errors)

let test_upgrade_with_read_pending () =
  with_server (fun ~sw ~net addr ->
    let socket = Eio.Net.connect ~sw net addr in
    Eio.Flow.copy_string
      "GET /upgrade-later HTTP/1.1\r\n\
       host: localhost\r\n\
       connection: upgrade\r\n\
       upgrade: echo\r\n\
       \r\n"
      socket;
    !real_sleep 0.05;
    Eio.Flow.copy_string "ping" socket;
    !real_sleep 0.05;
    Request_descriptor.respond_with_upgrade
      (Option.get !pending_upgrade)
      (Headers.of_list [ "connection", "upgrade"; "upgrade", "echo" ])
      (echo_server ());
    let expected =
      "HTTP/1.1 101 Switching Protocols\r\n\
       connection: upgrade\r\n\
       upgrade: echo\r\n\
       \r\n\
       ping"
    in
    let reader = Eio.Buf_read.of_flow socket ~max_size:65536 in
    Alcotest.(check string)
      "101 then the bytes sent before it"
      expected
      (Eio.Buf_read.take (String.length expected) reader);
    Eio.Flow.shutdown socket `Send;
    Eio.Stream.take connections_done;
    Eio.Flow.close socket)

let domain_handler (_ : Eio.Net.Sockaddr.stream) reqd =
  let body = string_of_int (Stdlib.Domain.self () :> int) in
  Request_descriptor.respond_with_string
    reqd
    (Response.create
       ~headers:
         (Headers.of_list
            [ "content-length", string_of_int (String.length body) ])
       `OK)
    body

let kernel_spreads_connections =
  match
    In_channel.with_open_text "/proc/sys/kernel/ostype" In_channel.input_all
  with
  | ostype -> String.trim ostype = "Linux"
  | exception Sys_error _ -> false

let check_domain_response seen response =
  let body =
    match String.split_on_char '\n' response |> List.rev with
    | body :: _ -> body
    | [] -> ""
  in
  Alcotest.(check string)
    "response"
    (Printf.sprintf
       "HTTP/1.1 200 OK\r\ncontent-length: %d\r\nconnection: close\r\n\r\n%s"
       (String.length body)
       body)
    response;
  Hashtbl.replace seen (int_of_string body) ()

let get_close = "GET / HTTP/1.1\r\nhost: localhost\r\nconnection: close\r\n\r\n"

let test_server_run_domains () =
  Eio_main.run (fun env ->
    let net = Eio.Stdenv.net env in
    let port =
      Eio.Switch.run (fun sw ->
        match
          Eio.Net.listening_addr
            (Eio.Net.listen
               ~sw
               ~backlog:1
               net
               (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0)))
        with
        | `Tcp (_, port) -> port
        | `Unix _ -> assert false)
    in
    let addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, port) in
    Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 10.0 (fun () ->
      Eio.Fiber.first
        (fun () ->
           Ache_eio.Server.run
             ~date:false
             ~domains:2
             ~on_error:raise
             ~net
             ~domain_mgr:(Eio.Stdenv.domain_mgr env)
             ~request_handler:domain_handler
             ~error_handler
             addr)
        (fun () ->
           let seen = Hashtbl.create 2 in
           let rec get () =
             match
               Eio.Switch.run (fun sw ->
                 let socket = Eio.Net.connect ~sw net addr in
                 Eio.Flow.copy_string get_close socket;
                 read_all socket)
             with
             | response -> response
             | exception Eio.Io (Eio.Net.E (Connection_failure (Refused _)), _)
               ->
               Eio.Time.sleep (Eio.Stdenv.clock env) 0.01;
               get ()
           in
           let batch () =
             Eio.Fiber.List.iter
               (fun () -> check_domain_response seen (get ()))
               (List.init 16 (fun _ -> ()))
           in
           batch ();
           if kernel_spreads_connections
           then (
             while Hashtbl.length seen < 2 do
               batch ()
             done;
             Alcotest.(check int) "domains that served" 2 (Hashtbl.length seen)))))

let accepted_socket_nodelay ~server_port ~client_port =
  (* Unix.file_descr is int on Unix: otherlibs/unix/unix.ml. *)
  let fd_of_int : int -> Unix.file_descr = Obj.magic in
  let port = function
    | Unix.ADDR_INET (_, port) -> Some port
    | ADDR_UNIX _ -> None
  in
  let rec find n =
    if n = 1024
    then Alcotest.fail "no accepted socket"
    else
      let fd = fd_of_int n in
      match port (Unix.getsockname fd), port (Unix.getpeername fd) with
      | Some local, Some peer when local = server_port && peer = client_port ->
        Unix.getsockopt fd TCP_NODELAY
      | _ | (exception Unix.Unix_error _) -> find (n + 1)
  in
  find 0

let nodelay_response nodelay =
  let body = string_of_bool nodelay in
  Printf.sprintf
    "HTTP/1.1 200 OK\r\ncontent-length: %d\r\nconnection: close\r\n\r\n%s"
    (String.length body)
    body

let test_server_run_tcp_nodelay ?tcp_nodelay expected () =
  Eio_main.run (fun env ->
    let net = Eio.Stdenv.net env in
    let port =
      Eio.Switch.run (fun sw ->
        match
          Eio.Net.listening_addr
            (Eio.Net.listen
               ~sw
               ~backlog:1
               net
               (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0)))
        with
        | `Tcp (_, port) -> port
        | `Unix _ -> assert false)
    in
    let addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, port) in
    let request_handler client reqd =
      let client_port =
        match client with `Tcp (_, port) -> port | `Unix _ -> assert false
      in
      let body =
        string_of_bool (accepted_socket_nodelay ~server_port:port ~client_port)
      in
      Request_descriptor.respond_with_string
        reqd
        (Response.create
           ~headers:
             (Headers.of_list
                [ "content-length", string_of_int (String.length body) ])
           `OK)
        body
    in
    Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 10.0 (fun () ->
      Eio.Fiber.first
        (fun () ->
           Ache_eio.Server.run
             ~date:false
             ~domains:1
             ?tcp_nodelay
             ~on_error:raise
             ~net
             ~domain_mgr:(Eio.Stdenv.domain_mgr env)
             ~request_handler
             ~error_handler
             addr)
        (fun () ->
           let rec get () =
             match
               Eio.Switch.run (fun sw ->
                 let socket = Eio.Net.connect ~sw net addr in
                 Eio.Flow.copy_string get_close socket;
                 read_all socket)
             with
             | response -> response
             | exception Eio.Io (Eio.Net.E (Connection_failure (Refused _)), _)
               ->
               Eio.Time.sleep (Eio.Stdenv.clock env) 0.01;
               get ()
           in
           Alcotest.(check string)
             "response"
             (nodelay_response expected)
             (get ()))))

let half_closed_request request =
  let clock = Eio_mock.Clock.make () in
  later_reqd := None;
  let output = ref "" in
  with_server ~clock ~idle_timeout:5. (fun ~sw ~net addr ->
    let socket = Eio.Net.connect ~sw net addr in
    Eio.Flow.copy_string request socket;
    Eio.Flow.shutdown socket `Send;
    let rec respond () =
      match !later_reqd with
      | None ->
        !real_sleep 0.01;
        respond ()
      | Some reqd ->
        !real_sleep 0.1;
        (try
           Request_descriptor.respond_with_string
             reqd
             (Response.create
                ~headers:(Headers.of_list [ "content-length", "5" ])
                `OK)
             "later"
         with
        | exn -> output := "respond raised " ^ Printexc.to_string exn)
    in
    respond ();
    if !output = "" then output := read_all socket;
    Eio.Stream.take connections_done;
    Eio.Flow.close socket);
  Alcotest.(check int) "no error" 0 (List.length !server_errors);
  !output

let test_half_close_close_request () =
  Alcotest.(check string)
    "response"
    "HTTP/1.1 200 OK\r\ncontent-length: 5\r\nconnection: close\r\n\r\nlater"
    (half_closed_request
       "GET /later HTTP/1.1\r\nhost: localhost\r\nconnection: close\r\n\r\n")

let test_half_close_http_1_0 () =
  Alcotest.(check string)
    "status line"
    "HTTP/1.1 200 OK"
    (status_line (half_closed_request "GET /later HTTP/1.0\r\n\r\n"))

let test_half_close_large_response () =
  let clock = Eio_mock.Clock.make () in
  with_server ~clock ~idle_timeout:5. (fun ~sw ~net addr ->
    let socket = Eio.Net.connect ~sw net addr in
    Eio.Flow.copy_string
      "GET /big HTTP/1.1\r\nhost: localhost\r\nconnection: close\r\n\r\n"
      socket;
    Eio.Flow.shutdown socket `Send;
    !real_sleep 0.2;
    let buf = Cstruct.create 65536 in
    let rec drain n =
      match Eio.Flow.single_read socket buf with
      | k -> drain (n + k)
      | exception End_of_file -> n
    in
    let head =
      Printf.sprintf
        "HTTP/1.1 200 OK\r\ncontent-length: %d\r\nconnection: close\r\n\r\n"
        big_size
    in
    Alcotest.(check int)
      "bytes received"
      (String.length head + big_size)
      (drain 0);
    Eio.Stream.take connections_done;
    Eio.Flow.close socket);
  Alcotest.(check int) "no error" 0 (List.length !server_errors)

let test_linger_deadline_not_renewed () =
  let clock = Eio_mock.Clock.make () in
  with_server ~clock ~idle_timeout:0.5 (fun ~sw ~net addr ->
    let socket = Eio.Net.connect ~sw net addr in
    (* A byte after the request makes it linger (decision 0023). *)
    Eio.Flow.copy_string
      "GET /hello HTTP/1.1\r\nhost: localhost\r\nconnection: close\r\n\r\nx"
      socket;
    Alcotest.(check string) "response" hello_response_close (read_all socket);
    let rec trickle i =
      if
        i <= 10 && Option.is_none (Eio.Stream.take_nonblocking connections_done)
      then (
        Eio.Flow.copy_string "x" socket;
        !real_sleep 0.05;
        Eio_mock.Clock.set_time clock (0.2 *. float_of_int i);
        !real_sleep 0.05;
        trickle (i + 1))
      else i
    in
    let steps = trickle 1 in
    Alcotest.(check bool)
      "linger ended while the peer kept sending"
      true
      (steps <= 10);
    Alcotest.(check bool)
      "linger lasted until idle_timeout"
      true
      (Eio.Time.now clock >= 0.5);
    Eio.Flow.close socket)

let test_server_run_survives_accept_errors () =
  Eio_main.run (fun env ->
    let net = Eio.Stdenv.net env in
    let clock = Eio.Stdenv.clock env in
    let port =
      Eio.Switch.run (fun sw ->
        match
          Eio.Net.listening_addr
            (Eio.Net.listen
               ~sw
               ~backlog:1
               net
               (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0)))
        with
        | `Tcp (_, port) -> port
        | `Unix _ -> assert false)
    in
    let addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, port) in
    Eio.Time.with_timeout_exn clock 30.0 (fun () ->
      Eio.Switch.run (fun sw ->
        let out_r, out_w = Eio_unix.pipe sw in
        let child =
          Eio.Process.spawn
            ~sw
            (Eio.Stdenv.process_mgr env)
            ~stdout:out_w
            [ "/bin/sh"
            ; "-c"
            ; Printf.sprintf
                "ulimit -n 48 && exec ./accept_error_server.exe %d"
                port
            ]
        in
        Eio.Flow.close out_w;
        let output = Eio.Buf_read.of_flow out_r ~max_size:1_000_000 in
        let rec connect () =
          match Eio.Net.connect ~sw net addr with
          | socket -> socket
          | exception Eio.Io (Eio.Net.E (Connection_failure (Refused _)), _) ->
            Eio.Time.sleep clock 0.01;
            connect ()
        in
        let get = "GET / HTTP/1.1\r\nhost: localhost\r\n\r\n"
        and ok =
          "HTTP/1.1 200 OK\r\n\
           content-length: 2\r\n\
           Keep-Alive: timeout=60\r\n\
           \r\n\
           ok"
        in
        let first = connect () in
        let first_reader = Eio.Buf_read.of_flow first ~max_size:65536 in
        Eio.Flow.copy_string get first;
        Alcotest.(check string)
          "first response"
          ok
          (Eio.Buf_read.take (String.length ok) first_reader);
        let flood =
          List.filter_map
            (fun () ->
               match Eio.Net.connect ~sw net addr with
               | socket -> Some socket
               | exception Eio.Io _ -> None)
            (List.init 96 (fun _ -> ()))
        in
        let line = Eio.Buf_read.line output in
        if not (String.starts_with ~prefix:"on_error" line)
        then Alcotest.failf "server output: %s" line;
        Eio.Flow.copy_string get first;
        Alcotest.(check string)
          "live connection still served"
          ok
          (match Eio.Buf_read.take (String.length ok) first_reader with
          | response -> response
          | exception End_of_file -> "connection cut");
        List.iter Eio.Flow.close flood;
        Eio.Flow.close first;
        let socket = connect () in
        Eio.Flow.copy_string
          "GET / HTTP/1.1\r\nhost: localhost\r\nconnection: close\r\n\r\n"
          socket;
        Alcotest.(check string)
          "new connection served"
          "HTTP/1.1 200 OK\r\ncontent-length: 2\r\nconnection: close\r\n\r\nok"
          (read_all socket);
        Eio.Process.signal child Sys.sigkill;
        ignore (Eio.Process.await child : Eio.Process.exit_status))))

exception Error_handler_failed

let test_server_run_error_handler_exception () =
  Eio_main.run (fun env ->
    let net = Eio.Stdenv.net env in
    let clock = Eio.Stdenv.clock env in
    let port =
      Eio.Switch.run (fun sw ->
        match
          Eio.Net.listening_addr
            (Eio.Net.listen
               ~sw
               ~backlog:1
               net
               (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0)))
        with
        | `Tcp (_, port) -> port
        | `Unix _ -> assert false)
    in
    let addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, port) in
    let errors = Eio.Stream.create max_int in
    let request_handler _ _ = failwith "request handler failed" in
    let error_handler _ ?request:_ error start_response =
      match error with
      | `Exn _ -> raise Error_handler_failed
      | _ -> Body.Writer.close (start_response Headers.empty)
    in
    Eio.Time.with_timeout_exn clock 10.0 (fun () ->
      Eio.Fiber.first
        (fun () ->
           Ache_eio.Server.run
             ~date:false
             ~domains:1
             ~on_error:(Eio.Stream.add errors)
             ~net
             ~domain_mgr:(Eio.Stdenv.domain_mgr env)
             ~request_handler
             ~error_handler
             addr)
        (fun () ->
           let rec connect sw =
             match Eio.Net.connect ~sw net addr with
             | socket -> socket
             | exception Eio.Io (Eio.Net.E (Connection_failure (Refused _)), _)
               ->
               Eio.Time.sleep clock 0.01;
               connect sw
           in
           Eio.Switch.run (fun sw ->
             let socket = connect sw in
             Eio.Flow.copy_string "GET / HTTP/1.1\r\nhost: x\r\n" socket;
             Eio.Net.setsockopt socket Eio.Net.Sockopt.SO_LINGER (Some 0);
             Eio.Flow.close socket);
           Eio.Time.sleep clock 0.1;
           Eio.Switch.run (fun sw ->
             let socket = connect sw in
             Eio.Flow.copy_string get_close socket;
             let (_ : string) = read_all socket in
             ());
           Alcotest.(check string)
             "on_error gets the error_handler exception"
             (Printexc.to_string Error_handler_failed)
             (Printexc.to_string
                (Eio.Time.with_timeout_exn clock 5.0 (fun () ->
                   Eio.Stream.take errors)));
           Eio.Time.sleep clock 0.1;
           Alcotest.(check int)
             "no other error, none for the reset"
             0
             (Eio.Stream.length errors))))

let ws_handshake target =
  Printf.sprintf
    "GET %s HTTP/1.1\r\n\
     host: localhost\r\n\
     upgrade: websocket\r\n\
     connection: Upgrade\r\n\
     sec-websocket-key: dGhlIHNhbXBsZSBub25jZQ==\r\n\
     sec-websocket-version: 13\r\n\
     \r\n"
    target

let ws_frame ?(fin = true) opcode payload =
  let mask = "\x01\x02\x03\x04" in
  let len = String.length payload in
  assert (len < 126);
  String.make 1 (Char.chr ((if fin then 0x80 else 0) lor opcode))
  ^ String.make 1 (Char.chr (0x80 lor len))
  ^ mask
  ^ String.mapi
      (fun i c -> Char.chr (Char.code c lxor Char.code mask.[i land 3]))
      payload

let read_ws_frame reader =
  let head = Eio.Buf_read.take 2 reader in
  let len = Char.code head.[1] in
  assert (len < 126);
  Char.code head.[0], Eio.Buf_read.take len reader

let frame = Alcotest.(pair int string)

let drain_ws_reports () =
  while Option.is_some (Eio.Stream.take_nonblocking ws_reports) do
    ()
  done

let ws_connect ~sw ~net addr target =
  drain_ws_reports ();
  let socket = Eio.Net.connect ~sw net addr in
  Eio.Flow.copy_string (ws_handshake target) socket;
  let reader = Eio.Buf_read.of_flow socket ~max_size:65536 in
  Alcotest.(check string)
    "101"
    ws_101
    (Eio.Buf_read.take (String.length ws_101) reader);
  socket, reader

let test_websocket_echo () =
  with_server (fun ~sw ~net addr ->
    let socket, reader = ws_connect ~sw ~net addr "/ws-echo" in
    Eio.Flow.copy_string (ws_frame 0x1 "hello") socket;
    let first = read_ws_frame reader in
    Eio.Flow.copy_string
      (ws_frame ~fin:false 0x1 "w\xc3"
      ^ ws_frame 0x9 "ping"
      ^ ws_frame 0x0 "\xb6rld")
      socket;
    let second = read_ws_frame reader in
    let third = read_ws_frame reader in
    Eio.Flow.copy_string (ws_frame 0x2 "\x00\xff") socket;
    let fourth = read_ws_frame reader in
    Alcotest.(check (list frame))
      "echo, pong, echo of the fragmented message, binary echo"
      [ 0x81, "hello"; 0x8a, "ping"; 0x81, "w\xc3\xb6rld"; 0x82, "\x00\xff" ]
      [ first; second; third; fourth ];
    Eio.Flow.copy_string (ws_frame 0x8 "\x03\xe8bye") socket;
    Alcotest.check
      frame
      "close echoed"
      (0x88, "\x03\xe8")
      (read_ws_frame reader);
    Alcotest.(check string)
      "then end of input"
      ""
      (Eio.Buf_read.take_all reader);
    Alcotest.(check string)
      "session"
      "receive None, client close 1000 \"bye\""
      (Eio.Stream.take ws_reports);
    Eio.Flow.close socket;
    Eio.Stream.take connections_done)

let test_websocket_server_close () =
  with_server (fun ~sw ~net addr ->
    let socket, reader = ws_connect ~sw ~net addr "/ws-echo" in
    Eio.Flow.copy_string (ws_frame 0x1 "close-me") socket;
    Alcotest.check frame "close 1001" (0x88, "\x03\xe9") (read_ws_frame reader);
    (* The server waits for the client's close frame and drops the message
       before it. *)
    Eio.Flow.copy_string
      (ws_frame 0x1 "dropped" ^ ws_frame 0x8 "\x0f\xa0done")
      socket;
    Alcotest.(check string)
      "close returned with the client's close frame"
      "closed, client close 4000 \"done\""
      (Eio.Stream.take ws_reports);
    Alcotest.(check string)
      "then end of input"
      ""
      (Eio.Buf_read.take_all reader);
    Eio.Flow.close socket;
    Eio.Stream.take connections_done)

let test_websocket_close_timeout () =
  with_server ~close_timeout:0.2 (fun ~sw ~net addr ->
    let socket, reader = ws_connect ~sw ~net addr "/ws-echo" in
    Eio.Flow.copy_string (ws_frame 0x1 "close-me") socket;
    Alcotest.check frame "close 1001" (0x88, "\x03\xe9") (read_ws_frame reader);
    let closed_at = Unix.gettimeofday () in
    (* The client never answers, and sends a message every 50 ms: the messages
       are dropped and do not extend the close timeout. *)
    let rec send () =
      match Eio.Flow.copy_string (ws_frame 0x1 "dropped") socket with
      | () ->
        !real_sleep 0.05;
        send ()
      | exception (Eio.Io _ | End_of_file) -> Eio.Fiber.await_cancel ()
    in
    Eio.Fiber.first
      (fun () ->
         Alcotest.(check string)
           "then end of input"
           ""
           (Eio.Buf_read.take_all reader))
      send;
    let elapsed = Unix.gettimeofday () -. closed_at in
    Alcotest.(check bool)
      (Printf.sprintf "closed within 2.5 s (%.3f s)" elapsed)
      true
      (elapsed < 2.5);
    Alcotest.(check string)
      "close returned without the client's close frame"
      "closed, no client close"
      (Eio.Stream.take ws_reports);
    Eio.Flow.close socket;
    Eio.Stream.take connections_done)

(* A client that sends nothing after the server's close: the read that waits
   when the session closes starts again with the close timeout, with or without
   an idle timeout. *)
let close_timeout_silent ?idle_timeout () =
  with_server ?idle_timeout ~close_timeout:0.2 (fun ~sw ~net addr ->
    let socket, reader = ws_connect ~sw ~net addr "/ws-echo" in
    Eio.Flow.copy_string (ws_frame 0x1 "close-me") socket;
    Alcotest.check frame "close 1001" (0x88, "\x03\xe9") (read_ws_frame reader);
    let closed_at = Unix.gettimeofday () in
    Alcotest.(check string)
      "then end of input"
      ""
      (Eio.Buf_read.take_all reader);
    let elapsed = Unix.gettimeofday () -. closed_at in
    Alcotest.(check bool)
      (Printf.sprintf "closed within 2.5 s (%.3f s)" elapsed)
      true
      (elapsed < 2.5);
    Alcotest.(check string)
      "close returned without the client's close frame"
      "closed, no client close"
      (Eio.Stream.take ws_reports);
    Eio.Flow.close socket;
    Eio.Stream.take connections_done)

(* After the server's close frame, a ping from the client still gets a pong; the
   client then sends nothing and keeps its socket open, and the close timeout
   ends the connection. *)
let test_websocket_close_timeout_pong_then_silent () =
  with_server ~idle_timeout:30. ~close_timeout:0.2 (fun ~sw ~net addr ->
    let socket, reader = ws_connect ~sw ~net addr "/ws-echo" in
    Eio.Flow.copy_string (ws_frame 0x1 "close-me") socket;
    Alcotest.check frame "close 1001" (0x88, "\x03\xe9") (read_ws_frame reader);
    let closed_at = Unix.gettimeofday () in
    Eio.Flow.copy_string (ws_frame 0x9 "p") socket;
    Alcotest.check frame "pong after close" (0x8a, "p") (read_ws_frame reader);
    Alcotest.(check string)
      "then end of input"
      ""
      (Eio.Buf_read.take_all reader);
    let elapsed = Unix.gettimeofday () -. closed_at in
    Alcotest.(check bool)
      (Printf.sprintf "closed within 2.5 s (%.3f s)" elapsed)
      true
      (elapsed < 2.5);
    Alcotest.(check string)
      "close returned without the client's close frame"
      "closed, no client close"
      (Eio.Stream.take ws_reports);
    Eio.Flow.close socket;
    Eio.Stream.take connections_done)

(* The core's [close], called from another fiber while the read waits (a close
   from [on_message] runs inside a read). The read gets the close timeout,
   without an idle timeout. *)
let test_websocket_close_timeout_silent () = close_timeout_silent ()

let test_websocket_close_timeout_silent_idle () =
  close_timeout_silent ~idle_timeout:30. ()

(* After the close timeout, the connection does not linger: a client that keeps
   sending and never closes its socket does not keep it open. *)
let test_websocket_close_timeout_no_linger () =
  with_server ~idle_timeout:30. ~close_timeout:0.2 (fun ~sw ~net addr ->
    let socket, reader = ws_connect ~sw ~net addr "/ws-echo" in
    Eio.Flow.copy_string (ws_frame 0x1 "close-me") socket;
    Alcotest.check frame "close 1001" (0x88, "\x03\xe9") (read_ws_frame reader);
    let closed_at = Unix.gettimeofday () in
    let rec send () =
      (try Eio.Flow.copy_string (ws_frame 0x1 "dropped") socket with
      | Eio.Io _ | End_of_file -> ());
      !real_sleep 0.05;
      send ()
    in
    Eio.Fiber.first (fun () -> Eio.Stream.take connections_done) send;
    let elapsed = Unix.gettimeofday () -. closed_at in
    Alcotest.(check bool)
      (Printf.sprintf "handler returned within 2.5 s (%.3f s)" elapsed)
      true
      (elapsed < 2.5);
    Alcotest.(check string)
      "close returned without the client's close frame"
      "closed, no client close"
      (Eio.Stream.take ws_reports);
    Eio.Flow.close socket)

(* After a completed close handshake the server ends the TCP connection first
   (RFC 6455 7.1.1, decision 0019): the client, which keeps its socket open
   without sending FIN, sees the end of input within 100 ms. [~server_first]:
   the server sends the first close frame; otherwise the client does. *)
let test_websocket_close_handshake_fin ~server_first () =
  with_server ~idle_timeout:30. ~close_timeout:30. (fun ~sw ~net addr ->
    let socket, reader = ws_connect ~sw ~net addr "/ws-echo" in
    let from =
      if server_first
      then (
        Eio.Flow.copy_string (ws_frame 0x1 "close-me") socket;
        Alcotest.check
          frame
          "close 1001"
          (0x88, "\x03\xe9")
          (read_ws_frame reader);
        let from = Unix.gettimeofday () in
        Eio.Flow.copy_string (ws_frame 0x8 "\x03\xe8") socket;
        from)
      else
        let from = Unix.gettimeofday () in
        Eio.Flow.copy_string (ws_frame 0x8 "\x03\xe8bye") socket;
        Alcotest.check
          frame
          "close echoed"
          (0x88, "\x03\xe8")
          (read_ws_frame reader);
        from
    in
    Alcotest.(check bool)
      "end of input"
      true
      (Eio.Buf_read.at_end_of_input reader);
    let elapsed = Unix.gettimeofday () -. from in
    Alcotest.(check bool)
      (Printf.sprintf "end of input within 100 ms (%.3f s)" elapsed)
      true
      (elapsed < 0.1);
    (* Nothing may follow the client's close frame (decision 0024): the
       connection ends without waiting for the client's FIN. *)
    check_done_within 0.1 ~from;
    Eio.Flow.close socket;
    Alcotest.(check string)
      "session"
      (if server_first
       then "closed, client close 1000 \"\""
       else "receive None, client close 1000 \"bye\"")
      (Eio.Stream.take ws_reports))

let test_websocket_close_timeout_callbacks () =
  with_server ~close_timeout:0.2 (fun ~sw ~net addr ->
    callbacks_ws := None;
    let socket, reader = ws_connect ~sw ~net addr "/ws-callbacks" in
    Eio.Flow.copy_string (ws_frame 0x1 "hello") socket;
    (* The read loop has read the message and waits again. *)
    !real_sleep 0.1;
    Ache.Websocket.close (Option.get !callbacks_ws) ~code:1000;
    Alcotest.check frame "close 1000" (0x88, "\x03\xe8") (read_ws_frame reader);
    let closed_at = Unix.gettimeofday () in
    Alcotest.(check string)
      "then end of input"
      ""
      (Eio.Buf_read.take_all reader);
    let elapsed = Unix.gettimeofday () -. closed_at in
    Alcotest.(check bool)
      (Printf.sprintf "closed within 2.5 s (%.3f s)" elapsed)
      true
      (elapsed < 2.5);
    Alcotest.(check string)
      "on_close without the client's close frame"
      "on_close None"
      (Eio.Stream.take ws_reports);
    Eio.Flow.close socket;
    Eio.Stream.take connections_done)

(* The server's close frame starts the close timeout of the read that waits with
   the idle timeout: it ends at the close timeout, not at the idle deadline. *)
let test_websocket_close_timeout_rearms_read () =
  let clock = Eio_mock.Clock.make () in
  with_server ~clock ~idle_timeout:30. ~close_timeout:0.3 (fun ~sw ~net addr ->
    let socket, reader = ws_connect ~sw ~net addr "/ws-echo" in
    Eio.Flow.copy_string (ws_frame 0x1 "close-me") socket;
    Alcotest.check frame "close 1001" (0x88, "\x03\xe9") (read_ws_frame reader);
    !real_sleep 0.05;
    advance clock;
    Alcotest.(check (float 0.))
      "ended at the close timeout"
      0.3
      (Eio.Time.now clock);
    Alcotest.(check string)
      "then end of input"
      ""
      (Eio.Buf_read.take_all reader);
    Alcotest.(check string)
      "close returned without the client's close frame"
      "closed, no client close"
      (Eio.Stream.take ws_reports);
    Eio.Flow.close socket;
    Eio.Stream.take connections_done)

(* The idle deadline of the read that was waiting when the session called
   [close] passes: the server goes on reading the client's close frame. *)
let test_websocket_server_close_past_idle_deadline () =
  let clock = Eio_mock.Clock.make () in
  with_server ~clock ~idle_timeout:0.1 (fun ~sw ~net addr ->
    let socket, reader = ws_connect ~sw ~net addr "/ws-echo" in
    Eio.Flow.copy_string (ws_frame 0x1 "close-me") socket;
    Alcotest.check frame "close 1001" (0x88, "\x03\xe9") (read_ws_frame reader);
    !real_sleep 0.05;
    Eio_mock.Clock.set_time clock 0.2;
    !real_sleep 0.05;
    Eio.Flow.copy_string (ws_frame 0x8 "\x0f\xa0done") socket;
    Alcotest.(check string)
      "close returned with the client's close frame"
      "closed, client close 4000 \"done\""
      (Eio.Stream.take ws_reports);
    Eio.Flow.close socket;
    Eio.Stream.take connections_done)

let test_websocket_ping () =
  with_server (fun ~sw ~net addr ->
    let socket, reader = ws_connect ~sw ~net addr "/ws-echo" in
    Eio.Flow.copy_string (ws_frame 0x1 "ping-me") socket;
    Alcotest.check
      frame
      "server ping"
      (0x89, "are you there")
      (read_ws_frame reader);
    Eio.Flow.copy_string (ws_frame 0xA "are you there") socket;
    Alcotest.check
      frame
      "ping returned on the pong"
      (0x81, "pong seen")
      (read_ws_frame reader);
    Eio.Flow.copy_string (ws_frame 0x1 "bye-reason") socket;
    Alcotest.check
      frame
      "close with a reason"
      (0x88, "\x0f\xa0done here")
      (read_ws_frame reader);
    Eio.Flow.copy_string (ws_frame 0x8 "\x0f\xa0") socket;
    Alcotest.(check string)
      "close returned"
      "closed with a reason"
      (Eio.Stream.take ws_reports);
    Alcotest.(check string)
      "then end of input"
      ""
      (Eio.Buf_read.take_all reader);
    Eio.Flow.close socket;
    Eio.Stream.take connections_done)

let test_websocket_send_loop_peer_reset () =
  with_server (fun ~sw ~net addr ->
    let socket, reader = ws_connect ~sw ~net addr "/ws-send-loop" in
    Alcotest.(check string)
      "server is sending"
      "\x81\x7e\x10\x00"
      (Eio.Buf_read.take 4 reader);
    Eio.Net.setsockopt socket Eio.Net.Sockopt.SO_LINGER (Some 0);
    let reset_at = Unix.gettimeofday () in
    Eio.Flow.close socket;
    Eio.Fiber.first
      (fun () ->
         Alcotest.(check string)
           "session"
           "send raised Closed, receive None"
           (Eio.Stream.take ws_reports);
         Eio.Stream.take connections_done)
      (fun () ->
         !real_sleep 2.0;
         Alcotest.fail "still running 2 s after the reset");
    let elapsed = Unix.gettimeofday () -. reset_at in
    Alcotest.(check bool)
      (Printf.sprintf "ended within 1 s (%.3f s)" elapsed)
      true
      (elapsed < 1.0))

let with_ws_session ?(on_upgrade = ignore) ?idle_timeout session client =
  Eio_main.run (fun env ->
    let clock = Eio.Stdenv.clock env in
    Eio.Time.with_timeout_exn clock 10.0 (fun () ->
      Eio.Switch.run (fun sw ->
        let server, socket = Eio_unix.Net.socketpair_stream ~sw () in
        let handled, resolve_handled = Eio.Promise.create () in
        Eio.Fiber.fork ~sw (fun () ->
          Eio.Promise.resolve
            resolve_handled
            (match
               Ache_eio.Server.create_connection_handler
                 ~date:false
                 ?idle_timeout
                 ~request_handler:(fun _ reqd ->
                   match Ache_eio.Websocket.upgrade reqd (session clock) with
                   | Ok () -> on_upgrade ()
                   | Error _ -> Alcotest.fail "handshake rejected")
                 ~error_handler
                 server
                 (`Unix "ws")
             with
            | () -> Ok ()
            | exception exn -> Error (Printexc.to_string exn)));
        Eio.Flow.copy_string (ws_handshake "/ws") socket;
        let reader = Eio.Buf_read.of_flow socket ~max_size:65536 in
        Alcotest.(check string)
          "101"
          ws_101
          (Eio.Buf_read.take (String.length ws_101) reader);
        client clock socket reader;
        (* The server lingers until the peer closes. *)
        (try Eio.Flow.shutdown socket `Send with
        | Eio.Io _ -> ());
        Eio.Promise.await handled)))

let check_handled = Alcotest.(check (result unit string)) "handler" (Ok ())

let client_closes socket reader =
  Eio.Flow.copy_string (ws_frame 0x8 "\x03\xe8") socket;
  Alcotest.check frame "close echoed" (0x88, "\x03\xe8") (read_ws_frame reader);
  Alcotest.(check string) "then end of input" "" (Eio.Buf_read.take_all reader)

let receive_text ws = Option.map text_of (Ache_eio.Websocket.receive ws)

let test_websocket_ping_timeout () =
  let log = ref [] in
  let say s = log := s :: !log in
  let module W = Ache_eio.Websocket in
  let session clock ws =
    (match W.ping ws (String.make 126 'x') with
    | () -> say "126-byte ping sent"
    | exception Invalid_argument _ -> say "126-byte ping: Invalid_argument");
    (match W.close ws 1005 with
    | () -> say "close 1005 sent"
    | exception Invalid_argument _ -> say "close 1005: Invalid_argument");
    (match Eio.Time.with_timeout clock 0.05 (fun () -> Ok (W.ping ws "")) with
    | Ok () -> say "pong"
    | Error `Timeout -> say "no pong: timeout");
    W.ping ws "x";
    say "pong returns";
    (match W.ping ws "y" with
    | () -> say "pong"
    | exception W.Closed -> say "close: Closed");
    (match W.client_close ws with
    | Some { code; reason } ->
      say (Printf.sprintf "client close %d %S" code reason)
    | None -> say "no client close");
    (match W.ping ws "" with
    | () -> say "pong"
    | exception W.Closed -> say "ping after close: Closed");
    (match W.send_text ws "late" with
    | () -> say "sent"
    | exception W.Closed -> say "send after close: Closed");
    W.close ws 1000;
    say "close after close returns";
    say
      (match W.receive ws with None -> "receive None" | Some _ -> "a message")
  in
  check_handled
    (with_ws_session session (fun _ socket reader ->
       Alcotest.check frame "ping" (0x89, "") (read_ws_frame reader);
       Alcotest.check frame "ping x" (0x89, "x") (read_ws_frame reader);
       Eio.Flow.copy_string (ws_frame 0xA "x") socket;
       Alcotest.check frame "ping y" (0x89, "y") (read_ws_frame reader);
       client_closes socket reader));
  Alcotest.(check (list string))
    "session"
    [ "126-byte ping: Invalid_argument"
    ; "close 1005: Invalid_argument"
    ; "no pong: timeout"
    ; "pong returns"
    ; "close: Closed"
    ; "client close 1000 \"\""
    ; "ping after close: Closed"
    ; "send after close: Closed"
    ; "close after close returns"
    ; "receive None"
    ]
    (List.rev !log)

let readable socket =
  match Eio_unix.Resource.fd_opt socket with
  | None -> Alcotest.fail "no fd"
  | Some fd ->
    Eio_unix.Fd.use_exn "select" fd (fun fd ->
      match Unix.select [ fd ] [] [] 0. with [], _, _ -> false | _ -> true)

let test_websocket_receive_backpressure () =
  let go, resolve_go = Eio.Promise.create () in
  let received = ref [] in
  let session _ ws =
    Eio.Promise.await go;
    received := List.init 10 (fun _ -> receive_text ws);
    received := !received @ [ receive_text ws ]
  in
  check_handled
    (with_ws_session session (fun clock socket reader ->
       for i = 0 to 9 do
         Eio.Flow.copy_string (ws_frame 0x1 (string_of_int i)) socket
       done;
       Eio.Time.sleep clock 0.05;
       Eio.Flow.copy_string (ws_frame 0x9 "p") socket;
       Eio.Time.sleep clock 0.1;
       Alcotest.(check bool)
         "10 unread messages: reading paused, no pong"
         false
         (readable socket);
       Eio.Promise.resolve resolve_go ();
       Alcotest.check
         frame
         "receive resumes reading: pong"
         (0x8a, "p")
         (read_ws_frame reader);
       client_closes socket reader));
  Alcotest.(check (list (option string)))
    "the messages, in order, then None"
    (List.init 10 (fun i -> Some (string_of_int i)) @ [ None ])
    !received

let test_websocket_cancelled_receive () =
  let timed_out, resolve_timed_out = Eio.Promise.create () in
  let raced, resolve_raced = Eio.Promise.create () in
  let log = ref [] in
  let say s = log := s :: !log in
  let session clock ws =
    (match
       Eio.Time.with_timeout clock 0.01 (fun () -> Ok (receive_text ws))
     with
    | Ok _ -> say "a message"
    | Error `Timeout -> say "timeout");
    Eio.Promise.resolve resolve_timed_out ();
    say (Option.value (receive_text ws) ~default:"None");
    let first =
      Eio.Fiber.first (fun () -> receive_text ws) (fun () -> receive_text ws)
    in
    Eio.Promise.resolve resolve_raced ();
    say (Option.value first ~default:"None");
    say (Option.value (receive_text ws) ~default:"None");
    say (Option.value (receive_text ws) ~default:"None")
  in
  check_handled
    (with_ws_session session (fun _ socket reader ->
       Eio.Promise.await timed_out;
       Eio.Flow.copy_string (ws_frame 0x1 "hello") socket;
       Eio.Flow.copy_string (ws_frame 0x1 "a" ^ ws_frame 0x1 "b") socket;
       Eio.Promise.await raced;
       client_closes socket reader));
  Alcotest.(check (list string))
    "session"
    [ "timeout"; "hello"; "a"; "b"; "None" ]
    (List.rev !log)

let test_websocket_cancelled_receive_then_close () =
  let timed_out, resolve_timed_out = Eio.Promise.create () in
  let result = ref [] in
  let session clock ws =
    let r1 =
      match
        Eio.Time.with_timeout clock 0.01 (fun () -> Ok (receive_text ws))
      with
      | Ok _ -> "a message"
      | Error `Timeout -> "timeout"
    in
    Eio.Promise.resolve resolve_timed_out ();
    result := [ r1; Option.value (receive_text ws) ~default:"None" ]
  in
  check_handled
    (with_ws_session session (fun _ socket reader ->
       Eio.Promise.await timed_out;
       client_closes socket reader));
  Alcotest.(check (list string))
    "receive times out, then None"
    [ "timeout"; "None" ]
    !result

let test_websocket_cancelled_receives_bounded () =
  let cancelled, resolve_cancelled = Eio.Promise.create () in
  let grown = ref 0
  and after = ref None in
  let session _ ws =
    let cancel_many n =
      for _ = 1 to n do
        Eio.Fiber.first
          (fun () -> ignore (Ache_eio.Websocket.receive ws))
          Eio.Fiber.yield
      done
    in
    let live_words () =
      Gc.full_major ();
      (Gc.stat ()).live_words
    in
    cancel_many 1_000;
    let before = live_words () in
    cancel_many 20_000;
    grown := live_words () - before;
    Eio.Promise.resolve resolve_cancelled ();
    after := receive_text ws;
    ignore (Ache_eio.Websocket.receive ws)
  in
  check_handled
    (with_ws_session session (fun _ socket reader ->
       Eio.Promise.await cancelled;
       Eio.Flow.copy_string (ws_frame 0x1 "after") socket;
       client_closes socket reader));
  Alcotest.(check bool)
    (Printf.sprintf
       "20000 cancelled receives keep %d more live words (< 10000)"
       !grown)
    true
    (!grown < 10_000);
  Alcotest.(check (option string))
    "a receive still gets the next message"
    (Some "after")
    !after

let test_websocket_session_after_upgrade () =
  let upgraded = ref false
  and seen = ref None
  and finished = ref false in
  let session clock ws =
    seen := Some !upgraded;
    ignore (Ache_eio.Websocket.receive ws);
    Eio.Time.sleep clock 0.05;
    finished := true
  in
  check_handled
    (with_ws_session
       ~on_upgrade:(fun () -> upgraded := true)
       session
       (fun _ socket reader -> client_closes socket reader));
  Alcotest.(check (option bool))
    "the session starts after upgrade returned"
    (Some true)
    !seen;
  Alcotest.(check bool) "the handler waits for the session" true !finished

let test_websocket_session_exception () =
  let session _ ws =
    ignore (Ache_eio.Websocket.receive ws);
    failwith "session failed"
  in
  Alcotest.(check (result unit string))
    "the handler raises the exception"
    (Error (Printexc.to_string (Failure "session failed")))
    (with_ws_session session (fun _ socket reader ->
       Eio.Flow.copy_string (ws_frame 0x1 "boom") socket;
       Alcotest.check
         frame
         "close 1011"
         (0x88, "\x03\xf3")
         (read_ws_frame reader);
       Eio.Flow.copy_string (ws_frame 0x8 "\x03\xf3") socket;
       Alcotest.(check string)
         "then end of input"
         ""
         (Eio.Buf_read.take_all reader)))

let test_websocket_session_return_closes () =
  let session _ ws = Ache_eio.Websocket.send_text ws "hi" in
  check_handled
    (with_ws_session session (fun _ socket reader ->
       Alcotest.check frame "hi" (0x81, "hi") (read_ws_frame reader);
       Alcotest.check
         frame
         "close 1000 when the session returns"
         (0x88, "\x03\xe8")
         (read_ws_frame reader);
       Eio.Flow.copy_string
         (ws_frame 0x1 "unread" ^ ws_frame 0x8 "\x03\xe8")
         socket;
       Alcotest.(check string)
         "then end of input"
         ""
         (Eio.Buf_read.take_all reader)))

let test_websocket_session_closed_ignored () =
  let session _ ws =
    ignore (Ache_eio.Websocket.receive ws);
    Ache_eio.Websocket.send_text ws "late"
  in
  check_handled
    (with_ws_session session (fun _ socket reader ->
       client_closes socket reader))

let test_websocket_session_cancelled () =
  let waiting, resolve_waiting = Eio.Promise.create () in
  let outcome = ref "running" in
  Eio_main.run (fun _ ->
    Eio.Switch.run (fun sw ->
      let server, socket = Eio_unix.Net.socketpair_stream ~sw () in
      Eio.Flow.copy_string (ws_handshake "/ws") socket;
      Eio.Fiber.first
        (fun () ->
           Ache_eio.Server.create_connection_handler
             ~date:false
             ~request_handler:(fun _ reqd ->
               match
                 Ache_eio.Websocket.upgrade reqd (fun ws ->
                   Eio.Promise.resolve resolve_waiting ();
                   match Ache_eio.Websocket.receive ws with
                   | _ -> outcome := "receive returned"
                   | exception (Eio.Cancel.Cancelled _ as exn) ->
                     outcome := "cancelled";
                     raise exn)
               with
               | Ok () -> ()
               | Error _ -> Alcotest.fail "handshake rejected")
             ~error_handler
             server
             (`Unix "ws"))
        (fun () -> Eio.Promise.await waiting)));
  Alcotest.(check string)
    "cancelling the connection fiber cancels the session"
    "cancelled"
    !outcome

let test_websocket_upgrade_outside_connection () =
  Eio_main.run @@ fun _ ->
  let raised = ref None in
  let server =
    Server_connection.create (fun reqd ->
      match Ache_eio.Websocket.upgrade reqd ignore with
      | _ -> raised := Some "returned"
      | exception Invalid_argument message -> raised := Some message)
  in
  let s = ws_handshake "/ws" in
  let len = String.length s in
  let (_ : int) =
    Server_connection.read
      server
      (Ache.Bigstring.of_string s ~off:0 ~len)
      ~off:0
      ~len
  in
  Alcotest.(check (option string))
    "Invalid_argument"
    (Some
       "Ache_eio.Websocket.upgrade: not called in a request handler of \
        Ache_eio.Server")
    !raised;
  Alcotest.(check bool)
    "nothing responded"
    true
    (match Server_connection.next_write_operation server with
    | `Yield -> true
    | `Write _ | `Close _ -> false)

(* Client: response bytes on an idle connection (undici GHSA-35p6-xmwp-9g52).
   The server runs in another domain with blocking sockets, so that the client
   domain can block while the stray bytes arrive. For each of [strays], it
   accepts a connection, answers the first request, sends the stray bytes 50 ms
   later, and answers a second request if one comes within 1 s. The domain
   returns the second requests. *)
let stray_server strays =
  let listening = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.setsockopt listening Unix.SO_REUSEADDR true;
  Unix.bind listening (Unix.ADDR_INET (Unix.inet_addr_loopback, 0));
  Unix.listen listening 1;
  let port =
    match Unix.getsockname listening with
    | Unix.ADDR_INET (_, port) -> port
    | Unix.ADDR_UNIX _ -> assert false
  in
  let serve stray =
    let socket, _ = Unix.accept listening in
    let write s =
      try ignore (Unix.write_substring socket s 0 (String.length s) : int) with
      | Unix.Unix_error _ -> ()
    in
    let buf = Bytes.create 4096 in
    let rec read_head acc =
      let rec has_end i =
        i + 4 <= String.length acc
        && (String.sub acc i 4 = "\r\n\r\n" || has_end (i + 1))
      in
      if has_end 0
      then acc
      else
        match Unix.read socket buf 0 (Bytes.length buf) with
        | 0 -> acc
        | n -> read_head (acc ^ Bytes.sub_string buf 0 n)
        | exception Unix.Unix_error _ -> acc
    in
    ignore (read_head "" : string);
    write "HTTP/1.1 200 OK\r\nContent-Length: 2\r\n\r\nok";
    Unix.sleepf 0.05;
    write stray;
    Unix.setsockopt_float socket Unix.SO_RCVTIMEO 1.0;
    let second = read_head "" in
    if second <> ""
    then write "HTTP/1.1 200 OK\r\nContent-Length: 4\r\n\r\nreal";
    Unix.close socket;
    second
  in
  ( port
  , Domain.spawn (fun () ->
      Fun.protect
        ~finally:(fun () -> Unix.close listening)
        (fun () -> List.map serve strays)) )

let stray_cases =
  [ "a response", "HTTP/1.1 200 OK\r\nContent-Length: 4\r\n\r\nevil"
  ; "part of a head", "HTTP/1.1 200 OK\r\n"
  ]

let stray_outcome client =
  let result, resolve = Eio.Promise.create () in
  let finish r = ignore (Eio.Promise.try_resolve resolve r : bool) in
  let writer =
    Ache_eio.Client.request
      client
      (Ache.Request.create
         ~headers:(Ache.Headers.of_list [ "host", "localhost" ])
         `GET
         "/")
      ~error_handler:(function
        | `Connection_closed -> finish "Connection_closed"
        | `Malformed_response _ -> finish "Malformed_response"
        | `Invalid_response_body_length _ ->
          finish "Invalid_response_body_length"
        | `Exn exn -> finish (Printexc.to_string exn))
      ~response_handler:(fun response reader ->
        let body = Stdlib.Buffer.create 16 in
        let rec loop () =
          Ache.Body.Reader.schedule_read
            reader
            ~on_error:(fun () -> finish "body error")
            ~on_eof:(fun () ->
              finish
                (Printf.sprintf
                   "%d %s"
                   (Ache.Status.to_code response.status)
                   (Stdlib.Buffer.contents body)))
            ~on_read:(fun bs ~off ~len ->
              Stdlib.Buffer.add_string
                body
                (Ache.Bigstring.substring bs ~off ~len);
              loop ())
        in
        loop ())
  in
  Ache.Body.Writer.close writer;
  Eio.Promise.await result

let test_stray_bytes_on_idle_connection () =
  let port, server = stray_server (List.map snd stray_cases) in
  List.iter
    (fun (name, _) ->
       Eio_main.run (fun env ->
         Eio.Switch.run (fun sw ->
           let socket =
             Eio.Net.connect
               ~sw
               (Eio.Stdenv.net env)
               (`Tcp (Eio.Net.Ipaddr.V4.loopback, port))
           in
           let client = Ache_eio.Client.create_connection ~sw socket in
           Alcotest.(check string)
             (name ^ ": first")
             "200 ok"
             (stray_outcome client);
           (* The stray bytes arrive while this domain does not run the read
              loop. *)
           Unix.sleepf 0.2;
           Alcotest.(check string)
             (name ^ ": second")
             "Connection_closed"
             (stray_outcome client);
           Eio.Promise.await (Ache_eio.Client.shutdown client)));
       ())
    stray_cases;
  Alcotest.(check (list string))
    "second requests not sent"
    (List.map (fun _ -> "") stray_cases)
    (Domain.join server)

let with_stop_server ?(server_clock = true) ?grace f =
  server_errors := [];
  requests_seen := 0;
  later_reqd := None;
  Eio_main.run (fun env ->
    let clock = Eio.Stdenv.clock env in
    let server_clock = if server_clock then Some clock else None in
    Eio.Time.with_timeout_exn clock 10.0 (fun () ->
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
        let handled = Eio.Stream.create max_int in
        Eio.Fiber.fork_daemon ~sw (fun () ->
          let rec accept () =
            Eio.Net.accept_fork
              ~sw
              listening
              ~on_error:raise
              (fun socket addr ->
                 Ache_eio.Server.create_connection_handler
                   ~date:false
                   ?clock:server_clock
                   ~idle_timeout:None
                   ~stop
                   ?grace
                   ~request_handler
                   ~error_handler
                   socket
                   addr;
                 Eio.Stream.add handled ());
            accept ()
          in
          accept ());
        f
          ~clock
          ~sw
          ~net
          ~stop:(fun () ->
            Eio.Promise.resolve resolve_stop ();
            (* Let the connection fibers drain. *)
            for _ = 1 to 3 do
              Eio.Fiber.yield ()
            done)
          ~handled:(fun () -> Eio.Stream.take handled)
          (Eio.Net.listening_addr listening))))

let rec await_some clock r =
  match !r with
  | Some x ->
    r := None;
    x
  | None ->
    Eio.Time.sleep clock 0.001;
    await_some clock r

let get_later = "GET /later HTTP/1.1\r\nhost: localhost\r\n\r\n"

let late_response_close =
  "HTTP/1.1 200 OK\r\ncontent-length: 4\r\nconnection: close\r\n\r\nlate"

let respond_late reqd =
  Request_descriptor.respond_with_string
    reqd
    (Response.create ~headers:(Headers.of_list [ "content-length", "4" ]) `OK)
    "late"

let within clock seconds f =
  let start = Eio.Time.now clock in
  let result = f () in
  let elapsed = Eio.Time.now clock -. start in
  if elapsed > seconds
  then Alcotest.failf "took %.3f s, more than %.3f s" elapsed seconds;
  result

let test_stop_finishes_request () =
  with_stop_server (fun ~clock ~sw ~net ~stop ~handled addr ->
    let socket = Eio.Net.connect ~sw net addr in
    Eio.Flow.copy_string get_later socket;
    let reqd = await_some clock later_reqd in
    stop ();
    respond_late reqd;
    Alcotest.(check string) "response" late_response_close (read_all socket);
    Eio.Flow.close socket;
    within clock 0.5 handled)

let test_stop_closes_idle ?server_clock () =
  with_stop_server ?server_clock (fun ~clock ~sw ~net ~stop ~handled addr ->
    let socket = Eio.Net.connect ~sw net addr in
    let reader = Eio.Buf_read.of_flow socket ~max_size:65536 in
    Eio.Flow.copy_string get_hello socket;
    Alcotest.(check string)
      "response"
      hello_response
      (Eio.Buf_read.take (String.length hello_response) reader);
    stop ();
    within clock 0.5 (fun () ->
      Alcotest.(check bool) "EOF" true (Eio.Buf_read.at_end_of_input reader));
    Eio.Flow.close socket;
    within clock 0.5 handled;
    Alcotest.(check int) "requests" 1 !requests_seen)

let test_stop_grace_cancels () =
  with_stop_server ~grace:0.2 (fun ~clock ~sw ~net ~stop ~handled addr ->
    let socket = Eio.Net.connect ~sw net addr in
    Eio.Flow.copy_string get_later socket;
    let (_ : Request_descriptor.t) = await_some clock later_reqd in
    stop ();
    let output =
      within clock 1.0 (fun () ->
        match read_all socket with
        | output -> output
        | exception Eio.Io (Eio.Net.E (Connection_reset _), _) -> "")
    in
    Alcotest.(check string) "no response" "" output;
    within clock 0.5 handled;
    Eio.Flow.close socket)

let test_stop_before_handler () =
  server_errors := [];
  requests_seen := 0;
  Eio_main.run (fun _env ->
    Eio.Switch.run (fun sw ->
      let server, client = Eio_unix.Net.socketpair_stream ~sw () in
      Eio.Flow.copy_string get_hello client;
      Eio.Fiber.both
        (fun () ->
           Ache_eio.Server.create_connection_handler
             ~date:false
             ~stop:(Eio.Promise.create_resolved ())
             ~request_handler
             ~error_handler
             server
             (`Unix "socketpair"))
        (fun () ->
           Alcotest.(check string) "no response" "" (read_all client);
           Eio.Flow.close client);
      Alcotest.(check int) "requests" 0 !requests_seen))

let test_server_run_stop () =
  later_reqd := None;
  Eio_main.run (fun env ->
    let net = Eio.Stdenv.net env
    and clock = Eio.Stdenv.clock env in
    let port =
      Eio.Switch.run (fun sw ->
        match
          Eio.Net.listening_addr
            (Eio.Net.listen
               ~sw
               ~backlog:1
               net
               (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0)))
        with
        | `Tcp (_, port) -> port
        | `Unix _ -> assert false)
    in
    let addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, port) in
    let stop, resolve_stop = Eio.Promise.create () in
    let returned = ref false in
    Eio.Time.with_timeout_exn clock 10.0 (fun () ->
      Eio.Fiber.both
        (fun () ->
           Ache_eio.Server.run
             ~date:false
             ~clock
             ~stop
             ~grace:1.0
             ~domains:1
             ~on_error:raise
             ~net
             ~domain_mgr:(Eio.Stdenv.domain_mgr env)
             ~request_handler
             ~error_handler
             addr;
           returned := true)
        (fun () ->
           Eio.Switch.run (fun sw ->
             let rec connect () =
               match Eio.Net.connect ~sw net addr with
               | socket -> socket
               | exception Eio.Io (Eio.Net.E (Connection_failure (Refused _)), _)
                 ->
                 Eio.Time.sleep clock 0.01;
                 connect ()
             in
             let socket = connect () in
             Eio.Flow.copy_string get_later socket;
             let reqd = await_some clock later_reqd in
             Eio.Promise.resolve resolve_stop ();
             Eio.Time.sleep clock 0.05;
             Alcotest.(check bool) "running" false !returned;
             (match Eio.Net.connect ~sw net addr with
             | late ->
               Eio.Flow.copy_string get_hello late;
               let output =
                 match read_all late with
                 | output -> output
                 | exception Eio.Io (Eio.Net.E (Connection_reset _), _) -> ""
               in
               Alcotest.(check string) "after stop" "" output
             | exception Eio.Io (Eio.Net.E (Connection_failure _), _) -> ());
             Request_descriptor.respond_with_string
               reqd
               (Response.create
                  ~headers:(Headers.of_list [ "content-length", "4" ])
                  `OK)
               "late";
             Alcotest.(check string)
               "response"
               late_response_close
               (read_all socket);
             Eio.Flow.close socket));
      Alcotest.(check bool) "returned" true !returned))

let post_head target length =
  Printf.sprintf
    "POST %s HTTP/1.1\r\nhost: localhost\r\ncontent-length: %d\r\n\r\n"
    target
    length

(* Writes [chunk] every [every] seconds until the server stops reading. *)
let send_every ~clock socket ~every ~times chunk =
  let rec loop n =
    if n > 0
    then (
      Eio.Time.sleep clock every;
      match Eio.Flow.copy_string chunk socket with
      | () -> loop (n - 1)
      | exception Eio.Io _ -> ())
  in
  loop times

let elapsed_since clock start = Eio.Time.now clock -. start

let test_body_trickle_times_out () =
  pending_response := None;
  pending_body_failed := false;
  with_server ~body_timeout:0.3 (fun ~sw ~net addr ->
    let clock = !real_clock in
    let socket = Eio.Net.connect ~sw net addr in
    let start = Eio.Time.now clock in
    Eio.Flow.copy_string (post_head "/respond-later" 100) socket;
    let output =
      Eio.Fiber.first
        (fun () ->
           send_every ~clock socket ~every:0.2 ~times:100 "x";
           Eio.Fiber.await_cancel ())
        (fun () -> read_all socket)
    in
    let elapsed = elapsed_since clock start in
    Alcotest.(check string)
      "408"
      "HTTP/1.1 408 Request Timeout"
      (status_line output);
    Alcotest.(check bool) "on_error" true !pending_body_failed;
    if elapsed > 1.0 then Alcotest.failf "timed out after %.3f s" elapsed;
    Eio.Flow.close socket;
    Eio.Stream.take connections_done)

let test_body_progress_not_timed_out () =
  let chunks = 6 in
  with_server ~body_timeout:0.3 (fun ~sw ~net addr ->
    let clock = !real_clock in
    let socket = Eio.Net.connect ~sw net addr in
    let reader = Eio.Buf_read.of_flow socket ~max_size:(1024 * 1024) in
    let length = chunks * 2048 in
    Eio.Flow.copy_string (post_head "/echo" length) socket;
    Eio.Fiber.both
      (fun () ->
         send_every
           ~clock
           socket
           ~every:0.2
           ~times:chunks
           (String.make 2048 'x'))
      (fun () ->
         let head =
           Printf.sprintf "HTTP/1.1 200 OK\r\ncontent-length: %d\r\n\r\n" length
         in
         Alcotest.(check string)
           "head"
           head
           (Eio.Buf_read.take (String.length head) reader);
         Alcotest.(check int)
           "whole body echoed"
           length
           (String.length (Eio.Buf_read.take length reader)));
    Eio.Flow.close socket;
    Eio.Stream.take connections_done);
  Alcotest.(check int) "no error" 0 (List.length !server_errors)

let test_write_timeout () =
  let closed, resolve_closed = Eio.Promise.create () in
  stream_flush_closed := Some resolve_closed;
  with_server ~write_timeout:0.3 (fun ~sw ~net addr ->
    let clock = !real_clock in
    let socket = Eio.Net.connect ~sw net addr in
    let start = Eio.Time.now clock in
    Eio.Flow.copy_string
      "GET /stream HTTP/1.1\r\nhost: localhost\r\n\r\n"
      socket;
    Eio.Promise.await closed;
    Eio.Stream.take connections_done;
    let elapsed = elapsed_since clock start in
    if elapsed > 1.5 then Alcotest.failf "closed after %.3f s" elapsed;
    Eio.Flow.close socket);
  stream_flush_closed := None

let test_slow_handler_not_timed_out () =
  later_reqd := None;
  with_server
    ~idle_timeout:0.3
    ~head_timeout:0.3
    ~body_timeout:0.3
    ~write_timeout:0.3
    (fun ~sw ~net addr ->
       let clock = !real_clock in
       let socket = Eio.Net.connect ~sw net addr in
       let reader = Eio.Buf_read.of_flow socket ~max_size:65536 in
       Eio.Flow.copy_string
         "GET /later HTTP/1.1\r\nhost: localhost\r\n\r\n"
         socket;
       let reqd = await_some clock later_reqd in
       Eio.Time.sleep clock 1.0;
       respond_late reqd;
       let response = "HTTP/1.1 200 OK\r\ncontent-length: 4\r\n\r\nlate" in
       Alcotest.(check string)
         "response after 1 s"
         response
         (Eio.Buf_read.take (String.length response) reader);
       Alcotest.(check bool)
         "closed at the idle timeout"
         true
         (Eio.Buf_read.at_end_of_input reader);
       Eio.Flow.close socket;
       Eio.Stream.take connections_done);
  Alcotest.(check int) "no error" 0 (List.length !server_errors)

let test_websocket_idle_timeout () =
  let log = ref [] in
  let session _ ws =
    (match Ache_eio.Websocket.receive ws with
    | None -> log := "receive None" :: !log
    | Some _ -> log := "a message" :: !log);
    log := "session ended" :: !log
  in
  check_handled
    (with_ws_session ~idle_timeout:(Some 0.3) session (fun clock _ reader ->
       let start = Eio.Time.now clock in
       Alcotest.check
         frame
         "close 1001"
         (0x88, "\x03\xe9")
         (read_ws_frame reader);
       let elapsed = elapsed_since clock start in
       if elapsed < 0.25 || elapsed > 1.0
       then Alcotest.failf "closed after %.3f s" elapsed));
  Alcotest.(check (list string))
    "session"
    [ "receive None"; "session ended" ]
    (List.rev !log)

(* Serves [n] GET requests one after the other on a socket pair, and returns the
   head of each response. *)
let keep_alive_heads ?config ~idle_timeout n =
  Eio_main.run (fun _env ->
    Eio.Switch.run (fun sw ->
      let server, client = Eio_unix.Net.socketpair_stream ~sw () in
      let heads = ref [] in
      Eio.Fiber.both
        (fun () ->
           Ache_eio.Server.create_connection_handler
             ~date:false
             ?config
             ~idle_timeout
             ~request_handler
             ~error_handler
             server
             (`Unix "socketpair"))
        (fun () ->
           let reader = Eio.Buf_read.of_flow client ~max_size:65536 in
           for _ = 1 to n do
             Eio.Flow.copy_string get_hello client;
             let rec head acc =
               match Eio.Buf_read.line reader with
               | "" -> List.rev acc
               | line -> head (line :: acc)
             in
             heads := String.concat "\n" (head []) :: !heads;
             ignore (Eio.Buf_read.take 11 reader : string)
           done;
           Eio.Flow.close client);
      List.rev !heads))

let heads = Alcotest.(list string)

let test_keep_alive_hint () =
  Alcotest.check
    heads
    "idle_timeout 3.5"
    [ "HTTP/1.1 200 OK\ncontent-length: 11\nKeep-Alive: timeout=3" ]
    (keep_alive_heads ~idle_timeout:(Some 3.5) 1);
  Alcotest.check
    heads
    "no idle_timeout"
    [ "HTTP/1.1 200 OK\ncontent-length: 11" ]
    (keep_alive_heads ~idle_timeout:None 1);
  Alcotest.check
    heads
    "idle_timeout 0.5: no hint"
    [ "HTTP/1.1 200 OK\ncontent-length: 11" ]
    (keep_alive_heads ~idle_timeout:(Some 0.5) 1);
  Alcotest.check
    heads
    "max_requests_per_connection 3"
    [ "HTTP/1.1 200 OK\ncontent-length: 11\nKeep-Alive: timeout=3, max=2"
    ; "HTTP/1.1 200 OK\ncontent-length: 11\nKeep-Alive: timeout=3, max=1"
    ; "HTTP/1.1 200 OK\ncontent-length: 11\nconnection: close"
    ]
    (keep_alive_heads
       ~config:{ Ache.Config.default with max_requests_per_connection = Some 3 }
       ~idle_timeout:(Some 3.5)
       3)

let test_keep_alive_idle_one_second_longer () =
  with_server ~idle_timeout:1. (fun ~sw ~net addr ->
    let clock = !real_clock in
    let socket = Eio.Net.connect ~sw net addr in
    let reader = Eio.Buf_read.of_flow socket ~max_size:65536 in
    Eio.Flow.copy_string get_hello socket;
    let response =
      "HTTP/1.1 200 OK\r\n\
       content-length: 11\r\n\
       Keep-Alive: timeout=1\r\n\
       \r\n\
       hello world"
    in
    Alcotest.(check string)
      "response"
      response
      (Eio.Buf_read.take (String.length response) reader);
    let start = Eio.Time.now clock in
    let open_at_1_4 =
      Eio.Fiber.first
        (fun () ->
           ignore (Eio.Buf_read.at_end_of_input reader : bool);
           false)
        (fun () ->
           Eio.Time.sleep clock 1.4;
           true)
    in
    Alcotest.(check bool) "open 1.4 s after the response" true open_at_1_4;
    Alcotest.(check bool) "closed" true (Eio.Buf_read.at_end_of_input reader);
    let elapsed = Eio.Time.now clock -. start in
    if elapsed < 1.9 || elapsed > 2.3
    then Alcotest.failf "closed after %.3f s" elapsed;
    Eio.Flow.close socket;
    Eio.Stream.take connections_done)

(* A client on a socket pair; the test plays the server. *)
let with_raw_server f =
  Eio_main.run (fun env ->
    let clock = Eio.Stdenv.clock env in
    Eio.Time.with_timeout_exn clock 10.0 (fun () ->
      Eio.Switch.run (fun sw ->
        let client_socket, server = Eio_unix.Net.socketpair_stream ~sw () in
        let client = Ache_eio.Client.create_connection ~sw client_socket in
        let reader = Eio.Buf_read.of_flow server ~max_size:65536 in
        let serve response =
          let rec head () = if Eio.Buf_read.line reader <> "" then head () in
          head ();
          Eio.Flow.copy_string response server
        in
        f ~clock ~client ~server ~reader ~serve)))

let try_request client =
  let p, u = Eio.Promise.create () in
  let body =
    Ache_eio.Client.request
      client
      (Request.create
         ~headers:(Headers.of_list [ "host", "localhost" ])
         `GET
         "/")
      ~error_handler:(fun error ->
        Eio.Promise.resolve
          u
          (match error with
          | `Connection_closed -> Error "Connection_closed"
          | _ -> Error "another error"))
      ~response_handler:(fun response body ->
        Body.Reader.close body;
        Eio.Promise.resolve u (Ok (Status.to_code response.status)))
  in
  Body.Writer.close body;
  p

let outcome = Alcotest.(result int string)

let hinted =
  "HTTP/1.1 200 OK\r\ncontent-length: 0\r\nKeep-Alive: timeout=3\r\n\r\n"

let test_client_reuse_deadline () =
  with_raw_server (fun ~clock ~client ~server:_ ~reader ~serve ->
    let first = try_request client in
    serve hinted;
    Alcotest.check outcome "first" (Ok 200) (Eio.Promise.await first);
    Eio.Time.sleep clock 0.3;
    let second = try_request client in
    serve hinted;
    Alcotest.check
      outcome
      "reused within the hint"
      (Ok 200)
      (Eio.Promise.await second);
    Eio.Time.sleep clock 0.6;
    Alcotest.(check bool)
      "open after 0.6 s"
      false
      (Ache_eio.Client.is_closed client);
    Eio.Time.sleep clock 0.6;
    Alcotest.(check bool)
      "closed after 1.2 s"
      true
      (Ache_eio.Client.is_closed client);
    Alcotest.check
      outcome
      "a request after the deadline"
      (Error "Connection_closed")
      (Eio.Promise.await (try_request client));
    Alcotest.(check string)
      "no bytes reach the server"
      ""
      (Eio.Buf_read.take_all reader))

let test_client_deadline_before_timer () =
  (* timeout=2 gives 0 s: the next request closes the connection first, even if
     the timer has not run. *)
  with_raw_server (fun ~clock:_ ~client ~server:_ ~reader ~serve ->
    let first = try_request client in
    serve
      "HTTP/1.1 200 OK\r\ncontent-length: 0\r\nKeep-Alive: timeout=2\r\n\r\n";
    Alcotest.check outcome "first" (Ok 200) (Eio.Promise.await first);
    Alcotest.check
      outcome
      "next request"
      (Error "Connection_closed")
      (Eio.Promise.await (try_request client));
    Alcotest.(check string)
      "no bytes reach the server"
      ""
      (Eio.Buf_read.take_all reader))

let test_client_no_hint () =
  with_raw_server (fun ~clock ~client ~server:_ ~reader:_ ~serve ->
    let plain = "HTTP/1.1 200 OK\r\ncontent-length: 0\r\n\r\n" in
    let first = try_request client in
    serve plain;
    Alcotest.check outcome "first" (Ok 200) (Eio.Promise.await first);
    Eio.Time.sleep clock 1.2;
    Alcotest.(check bool) "open" false (Ache_eio.Client.is_closed client);
    let second = try_request client in
    serve plain;
    Alcotest.check outcome "reused" (Ok 200) (Eio.Promise.await second);
    Eio.Promise.await (Ache_eio.Client.shutdown client))

let continue_size = 2 * 1024 * 1024

let continue_request =
  Printf.sprintf
    "POST /upload HTTP/1.1\r\n\
     host: localhost\r\n\
     expect: 100-continue\r\n\
     content-length: %d\r\n\
     \r\n"
    continue_size

let continue_100 = "HTTP/1.1 100 Continue\r\n\r\n"

let continue_final =
  Printf.sprintf "HTTP/1.1 200 OK\r\ncontent-length: 7\r\n\r\n%d" continue_size

let continue_handler _ reqd =
  let body = Request_descriptor.request_body reqd in
  let received = ref 0 in
  let rec on_read _ ~off:_ ~len =
    received := !received + len;
    Body.Reader.schedule_read body ~on_eof ~on_error ~on_read
  and on_eof () =
    let s = string_of_int !received in
    Request_descriptor.respond_with_string
      reqd
      (Response.create
         ~headers:
           (Headers.of_list
              [ "content-length", string_of_int (String.length s) ])
         `OK)
      s
  and on_error () = () in
  Body.Reader.schedule_read body ~on_eof ~on_error ~on_read

let auto_continue = { Config.default with auto_continue = true }

let test_auto_continue_upload () =
  Eio_main.run (fun env ->
    let clock = Eio.Stdenv.clock env in
    Eio.Switch.run (fun sw ->
      let server, socket = Eio_unix.Net.socketpair_stream ~sw () in
      Eio.Fiber.fork ~sw (fun () ->
        Ache_eio.Server.create_connection_handler
          ~config:auto_continue
          ~date:false
          ~idle_timeout:None
          ~request_handler:continue_handler
          ~error_handler
          server
          (`Unix "continue"));
      let reader = Eio.Buf_read.of_flow socket ~max_size:4096 in
      Eio.Flow.copy_string continue_request socket;
      Alcotest.(check string)
        "100 Continue within 0.5 s"
        continue_100
        (Eio.Time.with_timeout_exn clock 0.5 (fun () ->
           Eio.Buf_read.take (String.length continue_100) reader));
      Eio.Flow.copy_string (String.make continue_size 'x') socket;
      Alcotest.(check string)
        "the whole body was read"
        continue_final
        (Eio.Buf_read.take (String.length continue_final) reader);
      Eio.Flow.shutdown socket `Send))

let () =
  Alcotest.run
    "ache-eio loopback"
    [ ( "loopback"
      , [ "GET", `Quick, test_get
        ; ( "client: response bytes on an idle connection are not a response"
          , `Quick
          , test_stray_bytes_on_idle_connection )
        ; "Date header from the runtime", `Quick, test_date_header
        ; ( "Expect: 100-continue upload with auto_continue"
          , `Quick
          , test_auto_continue_upload )
        ; "POST echo larger than the read buffer", `Quick, test_post_echo
        ; "two pipelined requests", `Quick, test_pipelined
        ; ( "many keep-alive requests on one connection keep the stack bounded"
          , `Quick
          , test_many_requests_bounded_stack )
        ; "101 upgrade to an echo protocol", `Quick, test_upgrade
        ; ( "a closed connection gives its buffers to the pool"
          , `Quick
          , test_buffer_pool )
        ; ( "CONNECT tunnel through respond_with_upgrade and Client.upgrade"
          , `Quick
          , test_connect_tunnel )
        ; ( "CONNECT tunnel gets the bytes sent with the head"
          , `Quick
          , test_connect_tunnel_bytes_after_head )
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
        ; ( "WebSocket server sending to a reset peer ends the connection"
          , `Quick
          , test_websocket_peer_reset )
        ; ( "run_server takes the partially applied handler"
          , `Quick
          , test_run_server )
        ; ( "Server.run with 2 domains serves, from both on Linux"
          , `Quick
          , test_server_run_domains )
        ; ( "Server.run keeps serving after accept errors"
          , `Quick
          , test_server_run_survives_accept_errors )
        ; ( "Server.run gives an error_handler exception to on_error"
          , `Quick
          , test_server_run_error_handler_exception )
        ; ( "a half-closed Connection: close request answered later"
          , `Quick
          , test_half_close_close_request )
        ; ( "a half-closed HTTP/1.0 request answered later"
          , `Quick
          , test_half_close_http_1_0 )
        ; ( "a half-closed request gets all of a 16 MiB streamed response"
          , `Quick
          , test_half_close_large_response )
        ; ( "Server.run: TCP_NODELAY off by default"
          , `Quick
          , test_server_run_tcp_nodelay false )
        ; ( "Server.run ~tcp_nodelay:true"
          , `Quick
          , test_server_run_tcp_nodelay ~tcp_nodelay:true true )
        ; ( "normal close with trailing bytes shuts each direction once"
          , `Quick
          , test_close_with_trailing_bytes )
        ; "request handler exception", `Quick, test_handler_exception
        ; "late error response", `Quick, test_late_error_response
        ; ( "cancelling the connection fiber is not an error"
          , `Quick
          , test_cancel_is_not_an_error )
        ; ( "without a clock lingering is bounded by the system clock"
          , `Quick
          , test_linger_system_clock )
        ; ( "a Connection: close request read whole closes at once"
          , `Quick
          , test_close_request_read_whole )
        ; ( "without idle_timeout lingering is bounded"
          , `Quick
          , test_linger_bounded_without_idle_timeout )
        ; ( "a failing Send shutdown ends the connection"
          , `Quick
          , test_failing_shutdown `Send )
        ; ( "a failing Receive shutdown ends the connection"
          , `Quick
          , test_failing_shutdown `Receive )
        ; ( "a failing All shutdown ends the connection"
          , `Quick
          , test_failing_shutdown_all )
        ; ( "an exception from report_exn ends the connection"
          , `Quick
          , test_report_exn_raises )
        ] )
    ; ( "progress"
      , [ ( "a body that trickles in times out"
          , `Quick
          , test_body_trickle_times_out )
        ; ( "a body with 2048 bytes per wait is not timed out"
          , `Quick
          , test_body_progress_not_timed_out )
        ; ( "a client that stops reading is closed after write_timeout"
          , `Quick
          , test_write_timeout )
        ; ( "a slow handler that reads no body is not timed out"
          , `Quick
          , test_slow_handler_not_timed_out )
        ; ( "an idle WebSocket closes with 1001"
          , `Quick
          , test_websocket_idle_timeout )
        ] )
    ; ( "keep-alive"
      , [ "the server sends the hint", `Quick, test_keep_alive_hint
        ; ( "the idle timer runs 1 s longer than the hint"
          , `Quick
          , test_keep_alive_idle_one_second_longer )
        ; ( "the client closes the connection at the reuse deadline"
          , `Quick
          , test_client_reuse_deadline )
        ; ( "a request past the deadline closes first, before the timer"
          , `Quick
          , test_client_deadline_before_timer )
        ; ( "without a hint the client keeps the connection"
          , `Quick
          , test_client_no_hint )
        ] )
    ; ( "stop"
      , [ ( "a request in flight finishes with connection: close"
          , `Quick
          , test_stop_finishes_request )
        ; ( "an idle connection closes at once"
          , `Quick
          , test_stop_closes_idle ?server_clock:None )
        ; ( "an idle connection closes at once, no clock"
          , `Quick
          , test_stop_closes_idle ~server_clock:false )
        ; ( "a request still running after grace is cancelled"
          , `Quick
          , test_stop_grace_cancels )
        ; ( "a handler applied after stop reads no request"
          , `Quick
          , test_stop_before_handler )
        ; ( "Server.run stops accepting, drains and returns"
          , `Quick
          , test_server_run_stop )
        ] )
    ; ( "websocket"
      , [ "echo through Ache_eio.Websocket", `Quick, test_websocket_echo
        ; "server close", `Quick, test_websocket_server_close
        ; "close timeout", `Quick, test_websocket_close_timeout
        ; ( "close timeout, silent client"
          , `Quick
          , test_websocket_close_timeout_silent )
        ; ( "close timeout, silent client, idle timeout"
          , `Quick
          , test_websocket_close_timeout_silent_idle )
        ; ( "close timeout, pong after close then silent client"
          , `Quick
          , test_websocket_close_timeout_pong_then_silent )
        ; ( "close timeout, starts at once for a read that waits"
          , `Quick
          , test_websocket_close_timeout_rearms_read )
        ; ( "close timeout, no linger"
          , `Quick
          , test_websocket_close_timeout_no_linger )
        ; ( "close handshake from the client: FIN at once"
          , `Quick
          , test_websocket_close_handshake_fin ~server_first:false )
        ; ( "close handshake from the server: FIN at once"
          , `Quick
          , test_websocket_close_handshake_fin ~server_first:true )
        ; ( "close timeout, callback API"
          , `Quick
          , test_websocket_close_timeout_callbacks )
        ; ( "server close outlives the idle deadline of the read"
          , `Quick
          , test_websocket_server_close_past_idle_deadline )
        ; ( "ping returns on the pong, close with a reason"
          , `Quick
          , test_websocket_ping )
        ; ( "ping timeout, Closed after close"
          , `Quick
          , test_websocket_ping_timeout )
        ; ( "a send loop to a reset peer ends"
          , `Quick
          , test_websocket_send_loop_peer_reset )
        ; ( "8 unread messages pause reading, receive resumes it"
          , `Quick
          , test_websocket_receive_backpressure )
        ; ( "cancelled receive loses no message"
          , `Quick
          , test_websocket_cancelled_receive )
        ; ( "cancelled receive, then close"
          , `Quick
          , test_websocket_cancelled_receive_then_close )
        ; ( "cancelled receives keep memory bounded"
          , `Quick
          , test_websocket_cancelled_receives_bounded )
        ; ( "the session starts after upgrade, the handler waits for it"
          , `Quick
          , test_websocket_session_after_upgrade )
        ; ( "a session that returns closes with 1000"
          , `Quick
          , test_websocket_session_return_closes )
        ; ( "a session exception closes with 1011 and goes to the handler"
          , `Quick
          , test_websocket_session_exception )
        ; ( "Closed from a session is ignored"
          , `Quick
          , test_websocket_session_closed_ignored )
        ; ( "cancelling the connection cancels the session"
          , `Quick
          , test_websocket_session_cancelled )
        ; ( "upgrade outside a connection raises Invalid_argument"
          , `Quick
          , test_websocket_upgrade_outside_connection )
        ] )
    ; ( "timeouts"
      , [ "idle connection is closed", `Quick, test_idle_timeout
        ; ( "connection idle between requests is closed"
          , `Quick
          , test_idle_timeout_between_requests )
        ; "incomplete head is rejected", `Quick, test_head_timeout
        ; ( "incomplete head gets 408 Request Timeout"
          , `Quick
          , test_head_timeout_408 )
        ; ( "after a head timeout, input is dropped until the linger ends"
          , `Quick
          , test_head_timeout_lingers )
        ; ( "head timeout counts from the first byte of the head"
          , `Quick
          , test_head_deadline )
        ; "slow request body is not cut", `Quick, test_slow_body_not_cut
        ; "no timeout configured", `Quick, test_no_timeout_configured
        ; ( "unread body after the response, no body_timeout: it is read"
          , `Quick
          , test_unread_body_without_body_timeout )
        ; ( "unread body after the response: body_timeout"
          , `Quick
          , test_unread_body_timeout )
        ; ( "unread body past 256 KiB closes the connection"
          , `Quick
          , test_unread_body_drain_limit )
        ; ( "response while a body read waits: body_timeout, system clock"
          , `Quick
          , test_respond_while_body_read_pending )
        ; ( "response while a body read waits: body_timeout, mock clock"
          , `Quick
          , test_respond_while_body_read_pending_mock_clock )
        ; ( "response while a body read waits, body on_error raises"
          , `Quick
          , test_body_on_error_raises )
        ; ( "response while a body read waits, body on_error raises: mock clock"
          , `Quick
          , test_body_on_error_raises_idle_timeout )
        ; ( "body read stops end with their read"
          , `Quick
          , test_body_read_stops_bounded )
        ; ( "body bytes that arrive at the body timeout are not delivered"
          , `Quick
          , test_body_bytes_at_the_deadline )
        ; "timeouts are on by default", `Quick, test_default_timeouts
        ; ( "when the timeout arguments are checked"
          , `Quick
          , test_timeout_check_timing )
        ; ( "input during linger does not extend it"
          , `Quick
          , test_linger_deadline_not_renewed )
        ; ( "a pipelined head gets its own deadline"
          , `Quick
          , test_pipelined_head_deadline )
        ; ( "input after a head timeout is read once, no read cancelled"
          , `Quick
          , test_head_timeout_then_data )
        ; ( "a request after the idle timeout is not read, no read cancelled"
          , `Quick
          , test_idle_timeout_then_request )
        ; ( "Client.shutdown cancels no read before its Send shutdown"
          , `Quick
          , test_client_shutdown_with_pending_read )
        ; ( "bytes sent before a delayed 101 reach the new protocol"
          , `Quick
          , test_upgrade_with_read_pending )
        ] )
    ]
