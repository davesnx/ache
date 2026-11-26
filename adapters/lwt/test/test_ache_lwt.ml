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
let stream_flush_closed : unit Lwt.u option ref = ref None
let stream_start : unit Lwt.t ref = ref Lwt.return_unit

let stream response_body =
  let chunk = String.make 65536 'x' in
  let rec loop () =
    Body.Writer.write_string response_body chunk;
    Body.Writer.flush response_body (function
      | `Written -> loop ()
      | `Closed ->
        Option.iter (fun u -> Lwt.wakeup_later u ()) !stream_flush_closed)
  in
  loop ()

let requests_seen = ref 0
let ws_ended : (string, exn) result Lwt.u option ref = ref None
let ws_flood_sends = ref 0

let end_ws result =
  Option.iter (fun u -> Lwt.wakeup_later u result) !ws_ended;
  ws_ended := None

let text_of = function Ache.Websocket.Text s -> s | Binary s -> "binary:" ^ s

let receive_text ws =
  Lwt.map (Option.map text_of) (Ache_lwt.Websocket.receive ws)

exception Ws_session_failed

let ws_echo ws =
  let rec loop () =
    Lwt.bind (Ache_lwt.Websocket.receive ws) (function
      | Some (Text "close-me") ->
        Lwt.bind (Ache_lwt.Websocket.close ws 1001) (fun () ->
          Lwt.map
            (fun next ->
               end_ws
                 (Ok
                    (match next, Ache_lwt.Websocket.client_close ws with
                    | None, None -> "closed, no client close"
                    | None, Some { code; reason } ->
                      Printf.sprintf "closed, client close %d %S" code reason
                    | Some m, _ -> "received " ^ text_of m)))
            (Ache_lwt.Websocket.receive ws))
      | Some (Text "ping-me") ->
        Lwt.bind (Ache_lwt.Websocket.ping ws "are you there") (fun () ->
          Lwt.bind (Ache_lwt.Websocket.send_text ws "pong seen") loop)
      | Some (Text "bye-reason") ->
        Lwt.bind
          (Ache_lwt.Websocket.close ~reason:"done here" ws 4000)
          (fun () ->
             end_ws (Ok "closed with a reason");
             Lwt.return_unit)
      | Some (Text message) ->
        Lwt.bind (Ache_lwt.Websocket.send_text ws message) loop
      | Some (Binary message) ->
        Lwt.bind (Ache_lwt.Websocket.send_binary ws message) loop
      | None ->
        end_ws
          (Ok
             (match Ache_lwt.Websocket.client_close ws with
             | None -> "receive None"
             | Some { code; reason } ->
               Printf.sprintf "receive None, client close %d %S" code reason));
        Lwt.return_unit)
  in
  loop ()

let ws_flood ws =
  let chunk = String.make 4096 'x' in
  let rec loop () =
    incr ws_flood_sends;
    Lwt.bind (Ache_lwt.Websocket.send_text ws chunk) loop
  in
  Lwt.catch loop (fun exn ->
    Lwt.map
      (fun next ->
         end_ws
           (match next with
           | None -> Error exn
           | Some _ -> Ok "receive returned a message after the reset"))
      (Ache_lwt.Websocket.receive ws))

exception Respond_later

let late_error_response : (unit -> unit) option ref = ref None
let pending_response : Request_descriptor.t option ref = ref None
let pending_body_failed = ref false
let slow_reqd : Request_descriptor.t option ref = ref None
let body_cut = ref false

let request_handler (_ : Unix.sockaddr) reqd =
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
  | `CONNECT, _ ->
    Request_descriptor.respond_with_upgrade reqd Headers.empty (echo_server ())
  | `GET, "/upgrade" ->
    Body.Reader.close body;
    Request_descriptor.respond_with_upgrade
      reqd
      (Headers.of_list [ "connection", "upgrade"; "upgrade", "echo" ])
      (echo_server ())
  | `GET, "/ws-return" ->
    (match
       Ache_lwt.Websocket.upgrade reqd (fun ws ->
         Ache_lwt.Websocket.send_text ws "hi")
     with
    | Ok () -> ()
    | Error _ -> Alcotest.fail "handshake rejected")
  | `GET, "/ws-raise" ->
    (match
       Ache_lwt.Websocket.upgrade reqd (fun _ -> Lwt.fail Ws_session_failed)
     with
    | Ok () -> ()
    | Error _ -> Alcotest.fail "handshake rejected")
  | `GET, "/ws-callback-close" ->
    (* The callback API: the first message closes the connection with 1000. *)
    (match
       Ache.Websocket.upgrade reqd (fun ws ->
         { Ache.Websocket.on_message =
             (fun _ -> Ache.Websocket.close ws ~code:1000)
         ; on_pong = ignore
         ; on_close =
             (function
               | None -> end_ws (Ok "on_close None")
               | Some { code; _ } ->
                 end_ws (Ok (Printf.sprintf "on_close %d" code)))
         })
     with
    | Ok () -> ()
    | Error _ -> Alcotest.fail "handshake rejected")
  | `GET, ("/ws-echo" | "/ws-flood") ->
    (match
       Ache_lwt.Websocket.upgrade
         reqd
         (if request.target = "/ws-echo" then ws_echo else ws_flood)
     with
    | Ok () -> ()
    | Error _ ->
      Request_descriptor.respond_with_string
        reqd
        (Response.create
           ~headers:(Headers.of_list [ "content-length", "0" ])
           `Bad_request)
        "")
  | `GET, "/stream" ->
    Body.Reader.close body;
    stream
      (Request_descriptor.respond_with_streaming reqd (Response.create `OK))
  | `GET, "/stream-late" ->
    Body.Reader.close body;
    Lwt.async (fun () ->
      Lwt.map
        (fun () ->
           stream
             (Request_descriptor.respond_with_streaming
                reqd
                (Response.create `OK)))
        !stream_start)
  | `GET, "/slow" ->
    Body.Reader.close body;
    slow_reqd := Some reqd
  | `POST, "/slow-noread" -> slow_reqd := Some reqd
  | `POST, ("/count" | "/count-started") ->
    let response_body =
      if request.target = "/count-started"
      then (
        let w =
          Request_descriptor.respond_with_streaming reqd (Response.create `OK)
        in
        Body.Writer.flush w ignore;
        Some w)
      else None
    in
    let total = ref 0 in
    let rec on_read _ ~off:_ ~len =
      total := !total + len;
      Body.Reader.schedule_read body ~on_eof ~on_error ~on_read
    and on_eof () =
      let text = string_of_int !total in
      match response_body with
      | Some w ->
        Body.Writer.write_string w text;
        Body.Writer.close w
      | None ->
        Request_descriptor.respond_with_string
          reqd
          (Response.create
             ~headers:
               (Headers.of_list
                  [ "content-length", string_of_int (String.length text) ])
             `OK)
          text
    and on_error () = body_cut := true in
    Body.Reader.schedule_read body ~on_eof ~on_error ~on_read
  | `GET, "/raise" -> failwith "handler failed"
  | `GET, "/raise-later" -> raise Respond_later
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
    let rec on_read _ ~off:_ ~len:_ =
      Body.Reader.schedule_read body ~on_eof ~on_error ~on_read
    and on_eof () = Alcotest.fail "a cut body completed"
    and on_error () = pending_body_failed := true in
    Body.Reader.schedule_read body ~on_eof ~on_error ~on_read;
    pending_response := Some reqd
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

let error_handler (_ : Unix.sockaddr) ?request:_ error start_response =
  server_errors := error :: !server_errors;
  match error with
  | `Exn Respond_later ->
    late_error_response :=
      Some (fun () -> Body.Writer.close (start_response Headers.empty))
  | _ -> Body.Writer.close (start_response Headers.empty)

open Lwt.Infix

let run f = Lwt_main.run (Lwt_unix.with_timeout 10.0 f)
let server_sockets : Lwt_unix.file_descr list ref = ref []

let with_server
      ?config
      ?idle_timeout
      ?head_timeout
      ?body_timeout
      ?write_timeout
      ?close_timeout
      ?(date = false)
      ?stop
      ?grace
      ?(connection_handler =
        fun ~request_handler ~error_handler addr socket ->
          Ache_lwt.Server.create_connection_handler
            ?config
            ~date
            ~idle_timeout
            ~head_timeout
            ~body_timeout
            ~write_timeout
            ~close_timeout
            ?stop
            ?grace
            ~request_handler
            ~error_handler
            addr
            socket)
      f
  =
  server_errors := [];
  requests_seen := 0;
  run (fun () ->
    let listening = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    Lwt_unix.setsockopt listening Unix.SO_REUSEADDR true;
    Lwt_unix.bind listening Unix.(ADDR_INET (inet_addr_loopback, 0))
    >>= fun () ->
    Lwt_unix.listen listening 16;
    let addr = Lwt_unix.getsockname listening in
    let connections = ref [] in
    server_sockets := [];
    let rec accept () =
      Lwt_unix.accept listening >>= fun (socket, client_addr) ->
      server_sockets := socket :: !server_sockets;
      connections :=
        connection_handler ~request_handler ~error_handler client_addr socket
        :: !connections;
      accept ()
    in
    let acceptor = accept () in
    let connect () =
      let socket = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
      Lwt_unix.connect socket addr >|= fun () -> socket
    in
    f connect >>= fun () ->
    Lwt.cancel acceptor;
    Lwt_unix.close listening >>= fun () -> Lwt.join !connections)

let read_body body =
  let buf = Stdlib.Buffer.create 1024 in
  let p, u = Lwt.wait () in
  let rec on_read bs ~off ~len =
    Stdlib.Buffer.add_string buf (Ache.Bigstring.substring bs ~off ~len);
    Body.Reader.schedule_read body ~on_eof ~on_error ~on_read
  and on_eof () = Lwt.wakeup_later u (Stdlib.Buffer.contents buf)
  and on_error () = Lwt.wakeup_later u "<response body cut short>" in
  Body.Reader.schedule_read body ~on_eof ~on_error ~on_read;
  p

let client_error_handler error =
  match error with
  | `Malformed_response msg -> Alcotest.failf "malformed response: %s" msg
  | `Invalid_response_body_length _ -> Alcotest.fail "invalid body length"
  | `Connection_closed -> Alcotest.fail "connection closed"
  | `Exn exn -> raise exn

let request client ?(body = "") request =
  let p, u = Lwt.wait () in
  let request_body =
    Ache_lwt.Client.request
      client
      request
      ~error_handler:client_error_handler
      ~response_handler:(fun response response_body ->
        Lwt.wakeup_later u (response, read_body response_body))
  in
  Body.Writer.write_string request_body body;
  Body.Writer.close request_body;
  p >>= fun (response, body) ->
  body >|= fun body -> response, body

let write_all socket s =
  let rec go off =
    if off = String.length s
    then Lwt.return_unit
    else
      Lwt_unix.write_string socket s off (String.length s - off) >>= fun n ->
      go (off + n)
  in
  go 0

let read_all socket =
  let buf = Stdlib.Buffer.create 1024 in
  let chunk = Bytes.create 4096 in
  let rec go () =
    Lwt_unix.read socket chunk 0 (Bytes.length chunk) >>= function
    | 0 -> Lwt.return (Stdlib.Buffer.contents buf)
    | n ->
      Stdlib.Buffer.add_subbytes buf chunk 0 n;
      go ()
  in
  go ()

let test_get () =
  with_server (fun connect ->
    connect () >>= Ache_lwt.Client.create_connection >>= fun client ->
    request
      client
      (Request.create
         ~headers:(Headers.of_list [ "host", "localhost" ])
         `GET
         "/hello")
    >>= fun (response, body) ->
    Alcotest.(check int) "status" 200 (Status.to_code response.status);
    Alcotest.(check string) "body" "hello world" body;
    Ache_lwt.Client.shutdown client)

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
       with_server ~date (fun connect ->
         connect () >>= Ache_lwt.Client.create_connection >>= fun client ->
         request
           client
           (Request.create
              ~headers:(Headers.of_list [ "host", "localhost" ])
              `GET
              "/hello")
         >>= fun (response, _) ->
         (match date, Headers.get_multi response.headers "date" with
         | true, [ value ] -> check_date_value value
         | false, [] -> ()
         | _, values ->
           Alcotest.failf
             "date:%b: Date fields %s"
             date
             (String.concat ", " values));
         Ache_lwt.Client.shutdown client))
    [ true; false ]

let test_post_echo () =
  with_server (fun connect ->
    connect () >>= Ache_lwt.Client.create_connection >>= fun client ->
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
    request client ~body:payload (Request.create ~headers `POST "/echo")
    >>= fun (response, body) ->
    Alcotest.(check int) "status" 200 (Status.to_code response.status);
    Alcotest.(check int) "length" (String.length payload) (String.length body);
    Alcotest.(check bool) "body" true (String.equal payload body);
    Ache_lwt.Client.shutdown client)

let test_pipelined () =
  with_server (fun connect ->
    connect () >>= fun socket ->
    write_all
      socket
      "GET /a HTTP/1.1\r\n\
       host: localhost\r\n\
       \r\n\
       GET /b HTTP/1.1\r\n\
       host: localhost\r\n\
       connection: close\r\n\
       \r\n"
    >>= fun () ->
    read_all socket >>= fun output ->
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
      output;
    Lwt_unix.close socket)

let test_upgrade () =
  with_server (fun connect ->
    connect () >>= Ache_lwt.Client.create_connection >>= fun client ->
    let upgraded, resolve_upgraded = Lwt.wait () in
    let request_body =
      Ache_lwt.Client.request
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
          Lwt.wakeup_later resolve_upgraded response)
    in
    Body.Writer.close request_body;
    upgraded >>= fun response ->
    Alcotest.(check int) "status" 101 (Status.to_code response.status);
    let message = "hello over echo" in
    let echoed, resolve_echoed = Lwt.wait () in
    let received = Stdlib.Buffer.create 16 in
    let echo_client =
      Echo.create ~pending:message (fun t s ->
        Stdlib.Buffer.add_string received s;
        if Stdlib.Buffer.length received >= String.length message
        then (
          Echo.shutdown t;
          Lwt.wakeup_later resolve_echoed (Stdlib.Buffer.contents received)))
    in
    Ache_lwt.Client.upgrade client (Runtime.make (module Echo) echo_client);
    echoed >>= fun echoed ->
    Alcotest.(check string) "echoed" message echoed;
    Ache_lwt.Client.shutdown client)

let read_exactly socket n =
  let buf = Bytes.create n in
  let rec go off =
    if off = n
    then Lwt.return (Bytes.to_string buf)
    else
      Lwt_unix.read socket buf off (n - off) >>= function
      | 0 -> Lwt.return (Bytes.sub_string buf 0 off)
      | k -> go (off + k)
  in
  go 0

let test_connect_tunnel_bytes_after_head () =
  with_server (fun connect ->
    connect () >>= fun socket ->
    write_all
      socket
      "CONNECT localhost:443 HTTP/1.1\r\nhost: localhost:443\r\n\r\nping"
    >>= fun () ->
    let expected = "HTTP/1.1 200 OK\r\n\r\nping" in
    read_exactly socket (String.length expected) >>= fun first ->
    Alcotest.(check string)
      "200, then the bytes sent with the head come back"
      expected
      first;
    write_all socket "pong" >>= fun () ->
    read_exactly socket 4 >>= fun second ->
    Alcotest.(check string) "later bytes" "pong" second;
    Lwt_unix.close socket)

let test_upgrade_incomplete_token_reaches_new_protocol () =
  with_server (fun connect ->
    connect () >>= fun socket ->
    write_all
      socket
      "GET /upgrade HTTP/1.1\r\n\
       host: localhost\r\n\
       connection: upgrade\r\n\
       upgrade: echo\r\n\
       \r\n\
       ping"
    >>= fun () ->
    let expected =
      "HTTP/1.1 101 Switching Protocols\r\n\
       connection: upgrade\r\n\
       upgrade: echo\r\n\
       \r\n\
       ping"
    in
    read_exactly socket (String.length expected) >>= fun output ->
    Alcotest.(check string) "101 then echoed bytes" expected output;
    Lwt_unix.close socket)

let fail_read = ref false
let fail_write = ref false
let fail_shutdown = ref false
let closes = ref 0

module Test_io = struct
  type socket = Lwt_unix.file_descr
  type addr = Unix.sockaddr

  let read socket bigstring ~off ~len =
    if !fail_read
    then Lwt.fail (Failure "read failed")
    else
      Lwt_bytes.read socket bigstring off len >>= function
      | 0 -> Lwt.fail End_of_file
      | n -> Lwt.return n

  let writev socket iovecs =
    if !fail_write
    then Lwt.fail (Failure "writev failed")
    else
      let vectors = Lwt_unix.IO_vectors.create () in
      List.iter
        (fun { IOVec.buffer; off; len } ->
           Lwt_unix.IO_vectors.append_bigarray vectors buffer off len)
        iovecs;
      Lwt_unix.writev socket vectors >|= fun n -> `Ok n

  let shutdown_send socket =
    try Lwt_unix.shutdown socket Unix.SHUTDOWN_SEND with
    | Unix.Unix_error _ -> ()

  let shutdown_receive socket =
    if !fail_shutdown
    then failwith "shutdown failed"
    else
      try Lwt_unix.shutdown socket Unix.SHUTDOWN_RECEIVE with
      | Unix.Unix_error _ -> ()

  let close socket =
    incr closes;
    Lwt_unix.close socket >>= fun () ->
    if !fail_shutdown
    then Lwt.fail (Failure "close failed")
    else Lwt.return_unit
end

module Test_runtime = Ache_lwt.Make (Test_io)
module Test_server = Test_runtime.Server

let with_failing_io ~read ~write =
  fail_read := read;
  fail_write := write;
  closes := 0;
  with_server
    ~connection_handler:(fun ~request_handler ~error_handler addr socket ->
      Test_server.create_connection_handler
        ~date:false
        ~request_handler
        ~error_handler
        addr
        socket)
    (fun connect ->
       connect () >>= fun socket ->
       write_all socket "GET /hello HTTP/1.1\r\nhost: localhost\r\n\r\n"
       >>= fun () ->
       Lwt.catch
         (fun () -> read_all socket >|= ignore)
         (function
           | Unix.Unix_error (Unix.ECONNRESET, _, _) -> Lwt.return_unit
           | exn -> Lwt.fail exn)
       >>= fun () -> Lwt_unix.close socket);
  Alcotest.(check int) "close called once" 1 !closes

let test_client_shutdown_closes_once () =
  fail_read := false;
  fail_write := false;
  closes := 0;
  with_server (fun connect ->
    connect () >>= Test_runtime.Client.create_connection >>= fun client ->
    let p, u = Lwt.wait () in
    let request_body =
      Test_runtime.Client.request
        client
        (Request.create
           ~headers:(Headers.of_list [ "host", "localhost" ])
           `GET
           "/hello")
        ~error_handler:client_error_handler
        ~response_handler:(fun response body ->
          Lwt.wakeup_later u (response, read_body body))
    in
    Body.Writer.close request_body;
    p >>= fun (_, body) ->
    body >>= fun body ->
    Alcotest.(check string) "body" "hello world" body;
    Test_runtime.Client.shutdown client >>= fun () -> Lwt_unix.sleep 0.05);
  Alcotest.(check int) "close called once" 1 !closes

let test_handler_exception () =
  fail_read := false;
  fail_write := false;
  closes := 0;
  with_server
    ~connection_handler:(fun ~request_handler ~error_handler addr socket ->
      Test_server.create_connection_handler
        ~date:false
        ~request_handler
        ~error_handler
        addr
        socket)
    (fun connect ->
       connect () >>= fun socket ->
       write_all socket "GET /raise HTTP/1.1\r\nhost: localhost\r\n\r\n"
       >>= fun () ->
       read_all socket >>= fun output ->
       Alcotest.(check string)
         "status line"
         "HTTP/1.1 500 Internal Server Error"
         (String.sub output 0 (String.index output '\r'));
       Lwt_unix.close socket);
  Alcotest.(check int) "close called once" 1 !closes

let test_read_failure () = with_failing_io ~read:true ~write:false
let test_write_failure () = with_failing_io ~read:false ~write:true

let test_eof_mid_body () =
  partial_body_cut := false;
  with_server (fun connect ->
    connect () >>= fun socket ->
    write_all
      socket
      "POST /partial HTTP/1.1\r\n\
       host: localhost\r\n\
       content-length: 100\r\n\
       \r\n\
       only ten b"
    >>= fun () ->
    Lwt_unix.shutdown socket Unix.SHUTDOWN_SEND;
    read_all socket >>= fun (_ : string) -> Lwt_unix.close socket);
  Alcotest.(check bool) "request body cut" true !partial_body_cut;
  match !server_errors with
  | [ `Bad_request _ ] -> ()
  | _ ->
    Alcotest.failf
      "expected one Bad_request, got %d errors"
      (List.length !server_errors)

let check_server_sockets_closed () =
  Alcotest.(check bool)
    "server socket closed"
    true
    (List.for_all (fun s -> Lwt_unix.state s = Lwt_unix.Closed) !server_sockets)

let read_head socket =
  let buf = Stdlib.Buffer.create 1024 in
  let byte = Bytes.create 1 in
  let rec go () =
    let n = Stdlib.Buffer.length buf in
    if n >= 4 && Stdlib.Buffer.sub buf (n - 4) 4 = "\r\n\r\n"
    then Lwt.return (Stdlib.Buffer.contents buf)
    else
      Lwt_unix.read socket byte 0 1 >>= function
      | 0 -> Lwt.return (Stdlib.Buffer.contents buf)
      | _ ->
        Stdlib.Buffer.add_bytes buf byte;
        go ()
  in
  go ()

let test_flush_after_peer_closed () =
  let flushed, resolve_flushed = Lwt.wait () in
  stream_flush_closed := Some resolve_flushed;
  with_server (fun connect ->
    connect () >>= fun socket ->
    write_all socket "GET /stream HTTP/1.1\r\nhost: localhost\r\n\r\n"
    >>= fun () ->
    read_head socket >>= fun head ->
    Alcotest.(check string)
      "status line"
      "HTTP/1.1 200 OK\r\n"
      (String.sub head 0 17);
    Lwt_unix.setsockopt_optint socket Unix.SO_LINGER (Some 0);
    Lwt_unix.close socket >>= fun () -> flushed);
  stream_flush_closed := None;
  check_server_sockets_closed ()

let test_flush_after_peer_closed_epipe () =
  let flushed, resolve_flushed = Lwt.wait () in
  let start, resolve_start = Lwt.wait () in
  stream_flush_closed := Some resolve_flushed;
  stream_start := start;
  with_server (fun connect ->
    connect () >>= fun socket ->
    write_all socket "GET /stream-late HTTP/1.1\r\nhost: localhost\r\n\r\n"
    >>= fun () ->
    Lwt_unix.close socket >>= fun () ->
    Lwt_unix.sleep 0.05 >>= fun () ->
    Lwt.wakeup_later resolve_start ();
    flushed);
  stream_flush_closed := None;
  stream_start := Lwt.return_unit;
  check_server_sockets_closed ()

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

let ws_101 =
  "HTTP/1.1 101 Switching Protocols\r\n\
   upgrade: websocket\r\n\
   connection: Upgrade\r\n\
   sec-websocket-accept: s3pPLMBiTxaQ9kYGzzhZRbK+xOo=\r\n\
   \r\n"

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

let read_ws_frame socket =
  read_exactly socket 2 >>= fun head ->
  let len = Char.code head.[1] in
  assert (len < 126);
  read_exactly socket len >|= fun payload -> Char.code head.[0], payload

let wait_ws_end () =
  let ended, u = Lwt.wait () in
  ws_ended := Some u;
  ended

let test_websocket_echo () =
  let ended = wait_ws_end () in
  with_server (fun connect ->
    connect () >>= fun socket ->
    write_all socket (ws_handshake "/ws-echo") >>= fun () ->
    read_head socket >>= fun head ->
    Alcotest.(check string) "101" ws_101 head;
    write_all socket (ws_frame 0x1 "hello") >>= fun () ->
    read_ws_frame socket >>= fun first ->
    write_all
      socket
      (ws_frame ~fin:false 0x1 "w\xc3"
      ^ ws_frame 0x9 "ping"
      ^ ws_frame 0x0 "\xb6rld")
    >>= fun () ->
    read_ws_frame socket >>= fun second ->
    read_ws_frame socket >>= fun third ->
    write_all socket (ws_frame 0x2 "\x00\xff") >>= fun () ->
    read_ws_frame socket >>= fun fourth ->
    Alcotest.(check (list (pair int string)))
      "echo, pong, echo of the fragmented message, binary echo"
      [ 0x81, "hello"; 0x8a, "ping"; 0x81, "w\xc3\xb6rld"; 0x82, "\x00\xff" ]
      [ first; second; third; fourth ];
    write_all socket (ws_frame 0x8 "\x03\xe8bye") >>= fun () ->
    read_ws_frame socket >>= fun close ->
    Alcotest.(check (pair int string)) "close echoed" (0x88, "\x03\xe8") close;
    read_all socket >>= fun rest ->
    Alcotest.(check string) "then end of input" "" rest;
    ended >>= fun result ->
    Alcotest.(check (result string reject))
      "server loop"
      (Ok "receive None, client close 1000 \"bye\"")
      result;
    Lwt_unix.close socket);
  check_server_sockets_closed ()

let test_websocket_server_close () =
  let ended = wait_ws_end () in
  with_server (fun connect ->
    connect () >>= fun socket ->
    write_all socket (ws_handshake "/ws-echo") >>= fun () ->
    read_head socket >>= fun head ->
    Alcotest.(check string) "101" ws_101 head;
    write_all socket (ws_frame 0x1 "close-me") >>= fun () ->
    read_ws_frame socket >>= fun close ->
    Alcotest.(check (pair int string)) "close 1001" (0x88, "\x03\xe9") close;
    (* The server waits for the client's close frame and drops the message
       before it. *)
    write_all socket (ws_frame 0x1 "dropped" ^ ws_frame 0x8 "\x0f\xa0done")
    >>= fun () ->
    ended >>= fun result ->
    Alcotest.(check (result string reject))
      "close resolved with the client's close frame"
      (Ok "closed, client close 4000 \"done\"")
      result;
    read_all socket >>= fun rest ->
    Alcotest.(check string) "then end of input" "" rest;
    Lwt_unix.close socket);
  check_server_sockets_closed ()

(* A client that never answers the server's close frame, and keeps sending
   messages, which do not extend the close timeout. *)
let test_websocket_close_timeout () =
  let ended = wait_ws_end () in
  with_server ~close_timeout:0.2 (fun connect ->
    connect () >>= fun socket ->
    write_all socket (ws_handshake "/ws-echo") >>= fun () ->
    read_head socket >>= fun head ->
    Alcotest.(check string) "101" ws_101 head;
    write_all socket (ws_frame 0x1 "close-me") >>= fun () ->
    read_ws_frame socket >>= fun close ->
    Alcotest.(check (pair int string)) "close 1001" (0x88, "\x03\xe9") close;
    let started = Unix.gettimeofday () in
    let sending = ref true in
    (* A message every 50 ms for up to 3 s, much longer than the timeout. *)
    let rec send () =
      if !sending && Unix.gettimeofday () -. started < 3.
      then
        Lwt.catch
          (fun () ->
             write_all socket (ws_frame 0x1 "dropped") >>= fun () ->
             Lwt_unix.sleep 0.05 >>= send)
          (function
            | Unix.Unix_error ((ECONNRESET | EPIPE), _, _) -> Lwt.return_unit
            | exn -> Lwt.fail exn)
      else Lwt.return_unit
    in
    let sender = send () in
    Lwt.catch
      (fun () -> read_all socket)
      (function
        | Unix.Unix_error (ECONNRESET, _, _) -> Lwt.return ""
        | exn -> Lwt.fail exn)
    >>= fun rest ->
    let elapsed = Unix.gettimeofday () -. started in
    sending := false;
    Alcotest.(check string) "then end of input" "" rest;
    if elapsed > 2.
    then
      Alcotest.failf "end of input after %.2f s, expected about 0.2 s" elapsed;
    ended >>= fun result ->
    Alcotest.(check (result string reject))
      "close resolved without the client's close frame"
      (Ok "closed, no client close")
      result;
    sender >>= fun () -> Lwt_unix.close socket);
  check_server_sockets_closed ()

(* The read that waits when the session closes has no idle limit: the close
   timeout must still apply to a client that then sends nothing. *)
let test_websocket_close_timeout_silent_client ?idle_timeout () =
  let ended = wait_ws_end () in
  with_server ?idle_timeout ~close_timeout:0.2 (fun connect ->
    connect () >>= fun socket ->
    write_all socket (ws_handshake "/ws-echo") >>= fun () ->
    read_head socket >>= fun head ->
    Alcotest.(check string) "101" ws_101 head;
    write_all socket (ws_frame 0x1 "close-me") >>= fun () ->
    read_ws_frame socket >>= fun close ->
    Alcotest.(check (pair int string)) "close 1001" (0x88, "\x03\xe9") close;
    let started = Unix.gettimeofday () in
    Lwt.pick
      [ (read_all socket >|= fun rest -> Some rest)
      ; (Lwt_unix.sleep 2.5 >|= fun () -> None)
      ]
    >>= fun rest ->
    Alcotest.(check (option string))
      (Printf.sprintf
         "end of input within 2.5 s (after %.2f s)"
         (Unix.gettimeofday () -. started))
      (Some "")
      rest;
    ended >>= fun result ->
    Alcotest.(check (result string reject))
      "close resolved without the client's close frame"
      (Ok "closed, no client close")
      result;
    Lwt_unix.close socket);
  check_server_sockets_closed ()

(* After the server's close frame, a ping from the client still gets a pong; the
   client then sends nothing and keeps its socket open, and the close timeout
   ends the connection. *)
let test_websocket_close_timeout_pong_then_silent () =
  let ended = wait_ws_end () in
  with_server ~idle_timeout:30. ~close_timeout:0.2 (fun connect ->
    connect () >>= fun socket ->
    write_all socket (ws_handshake "/ws-echo") >>= fun () ->
    read_head socket >>= fun head ->
    Alcotest.(check string) "101" ws_101 head;
    write_all socket (ws_frame 0x1 "close-me") >>= fun () ->
    read_ws_frame socket >>= fun close ->
    Alcotest.(check (pair int string)) "close 1001" (0x88, "\x03\xe9") close;
    let started = Unix.gettimeofday () in
    write_all socket (ws_frame 0x9 "p") >>= fun () ->
    read_ws_frame socket >>= fun pong ->
    Alcotest.(check (pair int string)) "pong after close" (0x8a, "p") pong;
    Lwt.pick
      [ (read_all socket >|= fun rest -> Some rest)
      ; (Lwt_unix.sleep 2.5 >|= fun () -> None)
      ]
    >>= fun rest ->
    Alcotest.(check (option string))
      (Printf.sprintf
         "end of input within 2.5 s (after %.2f s)"
         (Unix.gettimeofday () -. started))
      (Some "")
      rest;
    ended >>= fun result ->
    Alcotest.(check (result string reject))
      "close resolved without the client's close frame"
      (Ok "closed, no client close")
      result;
    check_server_sockets_closed ();
    Lwt_unix.close socket);
  check_server_sockets_closed ()

(* A client that never answers, keeps sending and never closes its socket: after
   the close timeout the server closes its socket and the connection promise
   resolves. *)
let test_websocket_close_timeout_server_closes () =
  let ended = wait_ws_end () in
  let served = ref [] in
  let connection_handler ~request_handler ~error_handler addr socket =
    let p =
      Ache_lwt.Server.create_connection_handler
        ~idle_timeout:None
        ~close_timeout:(Some 0.2)
        ~request_handler
        ~error_handler
        addr
        socket
    in
    served := p :: !served;
    p
  in
  with_server ~connection_handler (fun connect ->
    connect () >>= fun socket ->
    write_all socket (ws_handshake "/ws-echo") >>= fun () ->
    read_head socket >>= fun head ->
    Alcotest.(check string) "101" ws_101 head;
    write_all socket (ws_frame 0x1 "close-me") >>= fun () ->
    read_ws_frame socket >>= fun close ->
    Alcotest.(check (pair int string)) "close 1001" (0x88, "\x03\xe9") close;
    let started = Unix.gettimeofday () in
    let sending = ref true in
    let rec send () =
      if !sending
      then
        Lwt.catch
          (fun () ->
             write_all socket (ws_frame 0x1 "dropped") >>= fun () ->
             Lwt_unix.sleep 0.05 >>= send)
          (function
            | Unix.Unix_error ((ECONNRESET | EPIPE), _, _) -> Lwt.return_unit
            | exn -> Lwt.fail exn)
      else Lwt.return_unit
    in
    let sender = send () in
    Lwt.pick
      [ (Lwt.join !served >|= fun () -> true)
      ; (Lwt_unix.sleep 2.5 >|= fun () -> false)
      ]
    >>= fun resolved ->
    let elapsed = Unix.gettimeofday () -. started in
    Alcotest.(check bool)
      (Printf.sprintf
         "connection promise resolved within 2.5 s (%.2f s)"
         elapsed)
      true
      resolved;
    check_server_sockets_closed ();
    sending := false;
    sender >>= fun () ->
    ended >>= fun result ->
    Alcotest.(check (result string reject))
      "close resolved without the client's close frame"
      (Ok "closed, no client close")
      result;
    Lwt_unix.close socket);
  check_server_sockets_closed ()

(* As above, with the callback API of the core: the close frame of
   [Ache.Websocket.close] starts the close timeout. *)
let test_websocket_close_timeout_callback_api () =
  let ended = wait_ws_end () in
  with_server ~close_timeout:0.2 (fun connect ->
    connect () >>= fun socket ->
    write_all socket (ws_handshake "/ws-callback-close") >>= fun () ->
    read_head socket >>= fun head ->
    Alcotest.(check string) "101" ws_101 head;
    write_all socket (ws_frame 0x1 "close") >>= fun () ->
    read_ws_frame socket >>= fun close ->
    Alcotest.(check (pair int string)) "close 1000" (0x88, "\x03\xe8") close;
    let started = Unix.gettimeofday () in
    Lwt.pick
      [ (read_all socket >|= fun rest -> Some rest)
      ; (Lwt_unix.sleep 2.5 >|= fun () -> None)
      ]
    >>= fun rest ->
    Alcotest.(check (option string))
      (Printf.sprintf
         "end of input within 2.5 s (after %.2f s)"
         (Unix.gettimeofday () -. started))
      (Some "")
      rest;
    ended >>= fun result ->
    Alcotest.(check (result string reject))
      "on_close without the client's close frame"
      (Ok "on_close None")
      result;
    Lwt_unix.close socket);
  check_server_sockets_closed ()

let test_websocket_ping () =
  let ended = wait_ws_end () in
  with_server (fun connect ->
    connect () >>= fun socket ->
    write_all socket (ws_handshake "/ws-echo") >>= fun () ->
    read_head socket >>= fun head ->
    Alcotest.(check string) "101" ws_101 head;
    write_all socket (ws_frame 0x1 "ping-me") >>= fun () ->
    read_ws_frame socket >>= fun ping ->
    Alcotest.(check (pair int string))
      "server ping"
      (0x89, "are you there")
      ping;
    write_all socket (ws_frame 0xA "are you there") >>= fun () ->
    read_ws_frame socket >>= fun after ->
    Alcotest.(check (pair int string))
      "ping resolved by the pong"
      (0x81, "pong seen")
      after;
    write_all socket (ws_frame 0x1 "bye-reason") >>= fun () ->
    read_ws_frame socket >>= fun close ->
    Alcotest.(check (pair int string))
      "close with a reason"
      (0x88, "\x0f\xa0done here")
      close;
    write_all socket (ws_frame 0x8 "\x0f\xa0") >>= fun () ->
    ended >>= fun result ->
    Alcotest.(check (result string reject))
      "close resolved"
      (Ok "closed with a reason")
      result;
    read_all socket >>= fun rest ->
    Alcotest.(check string) "then end of input" "" rest;
    Lwt_unix.close socket);
  check_server_sockets_closed ()

let test_websocket_session_exception () =
  let hooked = ref [] in
  let previous = !Lwt.async_exception_hook in
  (Lwt.async_exception_hook := fun exn -> hooked := exn :: !hooked);
  Fun.protect
    ~finally:(fun () -> Lwt.async_exception_hook := previous)
    (fun () ->
       with_server (fun connect ->
         connect () >>= fun socket ->
         write_all socket (ws_handshake "/ws-raise") >>= fun () ->
         read_head socket >>= fun head ->
         Alcotest.(check string) "101" ws_101 head;
         read_ws_frame socket >>= fun close ->
         Alcotest.(check (pair int string))
           "close 1011"
           (0x88, "\x03\xf3")
           close;
         write_all socket (ws_frame 0x8 "\x03\xf3") >>= fun () ->
         read_all socket >>= fun rest ->
         Alcotest.(check string) "then end of input" "" rest;
         Lwt_unix.close socket);
       Alcotest.(check bool)
         "the hook got the exception"
         true
         (!hooked = [ Ws_session_failed ]));
  check_server_sockets_closed ()

let test_websocket_session_return_closes () =
  with_server (fun connect ->
    connect () >>= fun socket ->
    write_all socket (ws_handshake "/ws-return") >>= fun () ->
    read_head socket >>= fun head ->
    Alcotest.(check string) "101" ws_101 head;
    read_ws_frame socket >>= fun hi ->
    Alcotest.(check (pair int string)) "hi" (0x81, "hi") hi;
    read_ws_frame socket >>= fun close ->
    Alcotest.(check (pair int string))
      "close 1000 when the session returns"
      (0x88, "\x03\xe8")
      close;
    write_all socket (ws_frame 0x1 "unread" ^ ws_frame 0x8 "\x03\xe8")
    >>= fun () ->
    read_all socket >>= fun rest ->
    Alcotest.(check string) "then end of input" "" rest;
    Lwt_unix.close socket);
  check_server_sockets_closed ()

let test_websocket_receive_backpressure () =
  let ws = ref None in
  let server =
    Server_connection.create (fun reqd ->
      match
        Ache_lwt.Websocket.upgrade reqd (fun t ->
          ws := Some t;
          fst (Lwt.wait ()))
      with
      | Ok () -> ()
      | Error _ -> Alcotest.fail "handshake rejected")
  in
  let feed s =
    let len = String.length s in
    let n =
      Server_connection.read
        server
        (Ache.Bigstring.of_string ~off:0 ~len s)
        ~off:0
        ~len
    in
    Alcotest.(check int) "all consumed" len n
  in
  let rec drain () =
    match Server_connection.next_write_operation server with
    | `Write iovecs ->
      Server_connection.report_write_result
        server
        (`Ok (List.fold_left (fun acc iov -> acc + iov.IOVec.len) 0 iovecs));
      drain ()
    | `Yield | `Close _ -> ()
  in
  let read_operation () =
    match Server_connection.next_read_operation server with
    | `Read -> "`Read"
    | `Yield -> "`Yield"
    | `Close -> "`Close"
  in
  feed (ws_handshake "/ws");
  drain ();
  let ws = Option.get !ws in
  let fed = ref 0 in
  while read_operation () = "`Read" && !fed < 100 do
    feed (ws_frame 0x1 (string_of_int !fed));
    incr fed
  done;
  Alcotest.(check string)
    (Printf.sprintf "reader yields (after %d messages)" !fed)
    "`Yield"
    (read_operation ());
  Alcotest.(check int) "after 8 unconsumed messages" 8 !fed;
  let woken = ref false in
  Server_connection.yield_reader server (fun () -> woken := true);
  let first = Lwt_main.run (receive_text ws) in
  Alcotest.(check (option string)) "first message" (Some "0") first;
  Alcotest.(check bool) "receive wakes the reader" true !woken;
  Alcotest.(check string) "reads again" "`Read" (read_operation ());
  feed (ws_frame 0x1 "8");
  Alcotest.(check (list (option string)))
    "the rest, in order"
    (List.init 8 (fun i -> Some (string_of_int (i + 1))))
    (Lwt_main.run
       (Lwt_list.map_s (fun () -> receive_text ws) (List.init 8 ignore)))

let ws_sans_io () =
  let ws = ref None in
  let server =
    Server_connection.create (fun reqd ->
      match
        Ache_lwt.Websocket.upgrade reqd (fun t ->
          ws := Some t;
          fst (Lwt.wait ()))
      with
      | Ok () -> ()
      | Error _ -> Alcotest.fail "handshake rejected")
  in
  let feed s =
    let len = String.length s in
    let (_ : int) =
      Server_connection.read
        server
        (Ache.Bigstring.of_string ~off:0 ~len s)
        ~off:0
        ~len
    in
    ()
  in
  let rec drain () =
    match Server_connection.next_write_operation server with
    | `Write iovecs ->
      Server_connection.report_write_result
        server
        (`Ok (List.fold_left (fun acc iov -> acc + iov.IOVec.len) 0 iovecs));
      drain ()
    | `Yield | `Close _ -> ()
  in
  feed (ws_handshake "/ws");
  drain ();
  Option.get !ws, feed, drain

let receive_within seconds ws =
  Lwt.pick
    [ receive_text ws; (Lwt_unix.sleep seconds >|= fun () -> Some "<timeout>") ]

let test_websocket_cancelled_receive () =
  let ws, feed, (_ : unit -> unit) = ws_sans_io () in
  Alcotest.(check (option string))
    "first receive times out"
    (Some "<timeout>")
    (Lwt_main.run (receive_within 0.01 ws));
  feed (ws_frame 0x1 "hello");
  Alcotest.(check (option string))
    "the next receive gets the message"
    (Some "hello")
    (Lwt_main.run (receive_within 0.5 ws));
  let cancelled = Ache_lwt.Websocket.receive ws in
  Lwt.cancel cancelled;
  Alcotest.(check bool)
    "Lwt.cancel cancels receive"
    true
    (Lwt.state cancelled = Lwt.Fail Lwt.Canceled);
  feed (ws_frame 0x1 "again");
  Alcotest.(check (option string))
    "the receive after Lwt.cancel gets the message"
    (Some "again")
    (Lwt_main.run (receive_within 0.5 ws))

let test_websocket_cancelled_receive_then_close () =
  let ws, feed, drain = ws_sans_io () in
  Lwt.cancel (Ache_lwt.Websocket.receive ws);
  Alcotest.(check (option string))
    "receive times out"
    (Some "<timeout>")
    (Lwt_main.run (receive_within 0.01 ws));
  feed (ws_frame 0x8 "\x03\xe8");
  drain ();
  Alcotest.(check (option string))
    "a later receive gives None"
    None
    (Lwt_main.run (receive_within 0.5 ws))

let test_websocket_cancelled_receives_bounded () =
  let ws, feed, (_ : unit -> unit) = ws_sans_io () in
  let cancel_many n =
    for _ = 1 to n do
      Lwt.cancel (Ache_lwt.Websocket.receive ws)
    done
  in
  let live_words () =
    Gc.full_major ();
    (Gc.stat ()).live_words
  in
  cancel_many 1_000;
  let before = live_words () in
  cancel_many 100_000;
  let grown = live_words () - before in
  Alcotest.(check bool)
    (Printf.sprintf
       "100000 cancelled receives keep %d more live words (< 10000)"
       grown)
    true
    (grown < 10_000);
  feed (ws_frame 0x1 "after");
  Alcotest.(check (option string))
    "a receive still gets the next message"
    (Some "after")
    (Lwt_main.run (receive_within 0.5 ws))

let test_websocket_ping_timeout () =
  let ws, feed, drain = ws_sans_io () in
  let timed_out =
    Lwt_main.run
      (Lwt.catch
         (fun () ->
            Lwt_unix.with_timeout 0.01 (fun () -> Ache_lwt.Websocket.ping ws "")
            >|= fun () -> false)
         (function Lwt_unix.Timeout -> Lwt.return true | exn -> Lwt.fail exn))
  in
  Alcotest.(check bool) "no pong: with_timeout fires" true timed_out;
  let answered = Ache_lwt.Websocket.ping ws "x" in
  feed (ws_frame 0xA "x");
  Alcotest.(check bool)
    "a pong resolves the ping"
    true
    (Lwt.state answered = Lwt.Return ());
  let pending = Ache_lwt.Websocket.ping ws "y" in
  feed (ws_frame 0x8 "\x03\xe8");
  drain ();
  Alcotest.(check bool)
    "close fails the ping with Closed"
    true
    (Lwt.state pending = Lwt.Fail Ache_lwt.Websocket.Closed);
  Alcotest.(check bool)
    "client_close"
    true
    (Ache_lwt.Websocket.client_close ws
    = Some { Ache.Websocket.code = 1000; reason = "" });
  Alcotest.(check bool)
    "ping after close fails"
    true
    (Lwt.state (Ache_lwt.Websocket.ping ws "")
    = Lwt.Fail Ache_lwt.Websocket.Closed)

let test_websocket_peer_reset () =
  let ended = wait_ws_end () in
  ws_flood_sends := 0;
  with_server (fun connect ->
    connect () >>= fun socket ->
    write_all socket (ws_handshake "/ws-flood") >>= fun () ->
    read_head socket >>= fun head ->
    Alcotest.(check string) "101" ws_101 head;
    read_exactly socket 4 >>= fun first ->
    Alcotest.(check string) "server is sending" "\x81\x7e\x10\x00" first;
    Lwt_unix.setsockopt_optint socket Unix.SO_LINGER (Some 0);
    let reset_at = Unix.gettimeofday () in
    Lwt_unix.close socket >>= fun () ->
    Lwt.pick
      [ ended
      ; (Lwt_unix.sleep 2.0 >|= fun () -> Ok "still running 2 s after the reset")
      ]
    >|= fun result ->
    let elapsed = Unix.gettimeofday () -. reset_at in
    Alcotest.(check bool)
      "send failed with Closed, receive is None"
      true
      (result = Error Ache_lwt.Websocket.Closed);
    Alcotest.(check bool)
      (Printf.sprintf "ended within 1 s (%.3f s)" elapsed)
      true
      (elapsed < 1.0));
  check_server_sockets_closed ()

let get_hello = "GET /hello HTTP/1.1\r\nhost: localhost\r\n\r\n"
let hello_response = "HTTP/1.1 200 OK\r\ncontent-length: 11\r\n\r\nhello world"

(* [hello_response] with the [Keep-Alive] hint of an idle timeout of
   [seconds]. *)
let hello_with_hint seconds =
  Printf.sprintf
    "HTTP/1.1 200 OK\r\n\
     content-length: 11\r\n\
     Keep-Alive: timeout=%d\r\n\
     \r\n\
     hello world"
    seconds

let check_elapsed name ~since ~at_least =
  let elapsed = Unix.gettimeofday () -. since in
  if elapsed < at_least || elapsed > 2.0
  then Alcotest.failf "%s: closed after %.3f s" name elapsed

let test_idle_timeout () =
  with_server ~idle_timeout:0.1 (fun connect ->
    let since = Unix.gettimeofday () in
    connect () >>= fun socket ->
    read_all socket >>= fun output ->
    check_elapsed "idle" ~since ~at_least:0.1;
    Alcotest.(check string) "nothing written" "" output;
    Lwt_unix.close socket);
  Alcotest.(check int) "no error" 0 (List.length !server_errors);
  check_server_sockets_closed ()

let test_idle_timeout_between_requests () =
  with_server ~idle_timeout:0.3 (fun connect ->
    connect () >>= fun socket ->
    write_all socket get_hello >>= fun () ->
    read_exactly socket (String.length hello_response) >>= fun first ->
    Alcotest.(check string) "first response" hello_response first;
    Lwt_unix.sleep 0.2 >>= fun () ->
    write_all socket get_hello >>= fun () ->
    read_exactly socket (String.length hello_response) >>= fun second ->
    Alcotest.(check string) "second response" hello_response second;
    let since = Unix.gettimeofday () in
    read_all socket >>= fun rest ->
    check_elapsed "idle after a response" ~since ~at_least:0.25;
    Alcotest.(check string) "then closed" "" rest;
    Lwt_unix.close socket);
  Alcotest.(check int) "two requests" 2 !requests_seen

let head_timeout () =
  let output = ref "" in
  with_server ~head_timeout:0.1 (fun connect ->
    connect () >>= fun socket ->
    let since = Unix.gettimeofday () in
    write_all socket "GET /hello HTTP/1.1\r\nHo" >>= fun () ->
    read_all socket >>= fun s ->
    check_elapsed "head" ~since ~at_least:0.1;
    output := s;
    Lwt_unix.close socket);
  Alcotest.(check int) "request handler not called" 0 !requests_seen;
  check_server_sockets_closed ();
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

let rec respond_late () =
  match !late_error_response with
  | Some respond ->
    late_error_response := None;
    respond ();
    Lwt.return_unit
  | None -> Lwt_unix.sleep 0.01 >>= respond_late

let test_late_error_response () =
  late_error_response := None;
  with_server (fun connect ->
    connect () >>= fun socket ->
    write_all socket "GET /raise-later HTTP/1.1\r\nhost: localhost\r\n\r\n"
    >>= fun () ->
    Lwt_unix.sleep 0.05 >>= respond_late >>= fun () ->
    read_all socket >>= fun output ->
    Alcotest.(check string)
      "status line"
      "HTTP/1.1 500 Internal Server Error"
      (status_line output);
    Lwt_unix.close socket)

let test_head_deadline () =
  with_server ~head_timeout:0.4 (fun connect ->
    connect () >>= fun socket ->
    let since = Unix.gettimeofday () in
    write_all socket "GET /hello HTTP/1.1\r\n" >>= fun () ->
    Lwt_unix.sleep 0.2 >>= fun () ->
    write_all socket "a: b\r\n" >>= fun () ->
    read_all socket >>= fun output ->
    let elapsed = Unix.gettimeofday () -. since in
    if elapsed < 0.35 || elapsed > 0.55
    then Alcotest.failf "head rejected after %.3f s, not 0.4 s" elapsed;
    Alcotest.(check bool)
      "4xx status line"
      true
      (String.starts_with ~prefix:"HTTP/1.1 4" output);
    Lwt_unix.close socket)

let test_slow_body_not_cut () =
  with_server ~idle_timeout:0.1 ~head_timeout:0.1 (fun connect ->
    connect () >>= fun socket ->
    write_all
      socket
      "POST /echo HTTP/1.1\r\n\
       host: localhost\r\n\
       content-length: 10\r\n\
       \r\n\
       hello"
    >>= fun () ->
    let head = "HTTP/1.1 200 OK\r\ncontent-length: 10\r\n\r\nhello" in
    read_exactly socket (String.length head) >>= fun first ->
    Alcotest.(check string) "first half echoed" head first;
    Lwt_unix.sleep 0.3 >>= fun () ->
    write_all socket "world" >>= fun () ->
    read_exactly socket 5 >>= fun second ->
    Alcotest.(check string) "second half" "world" second;
    Lwt_unix.close socket);
  Alcotest.(check int) "no error" 0 (List.length !server_errors)

let unread_body_request target =
  Printf.sprintf
    "POST %s HTTP/1.1\r\nhost: localhost\r\ncontent-length: 100000\r\n\r\nabc"
    target

let early_close_response =
  "HTTP/1.1 200 OK\r\nconnection: close\r\ncontent-length: 2\r\n\r\nno"

let early_413_response =
  "HTTP/1.1 413 Content Too Large\r\ncontent-length: 2\r\n\r\nno"

(* A 413 keeps the connection: the body is read until [body_timeout], then the
   connection closes. A [Connection: close] response ends the output at once
   (the server still reads the body, which the close does not reset). *)
let test_unread_body_body_timeout () =
  List.iter
    (fun (target, response, closes_at_timeout) ->
       with_server ?idle_timeout:None ~body_timeout:0.3 (fun connect ->
         connect () >>= fun socket ->
         write_all socket (unread_body_request target) >>= fun () ->
         read_exactly socket (String.length response) >>= fun first ->
         Alcotest.(check string) (target ^ ": response") response first;
         let since = Unix.gettimeofday () in
         read_all socket >>= fun rest ->
         if closes_at_timeout
         then check_elapsed (target ^ ": body timeout") ~since ~at_least:0.25
         else
           Alcotest.(check bool)
             (target ^ ": at once")
             true
             (Unix.gettimeofday () -. since < 0.2);
         Alcotest.(check string) (target ^ ": then closed") "" rest;
         Lwt_unix.close socket))
    [ "/early-close", early_close_response, false
    ; "/early-413", early_413_response, true
    ]

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
  with_server ~idle_timeout:5.0 (fun connect ->
    connect () >>= fun socket ->
    write_all socket large_413_head >>= fun () ->
    read_exactly socket (String.length large_413_response) >>= fun response ->
    Alcotest.(check string) "413 closes" large_413_response response;
    let since = Unix.gettimeofday () in
    Lwt.catch
      (fun () -> write_all socket (String.make 0x50000 'x'))
      (fun _ -> Lwt.return_unit)
    >>= fun () ->
    Lwt.catch
      (fun () -> read_all socket)
      (function
        | Unix.Unix_error (Unix.ECONNRESET, _, _) -> Lwt.return ""
        | exn -> Lwt.fail exn)
    >>= fun rest ->
    Alcotest.(check string) "then closed" "" rest;
    if Unix.gettimeofday () -. since > 2.0
    then Alcotest.fail "closed by the idle timeout, not after 256 KiB";
    Lwt_unix.close socket)

(* [read_all], with a reset or a refused write read as the end of input. *)
let read_all_or_reset socket =
  Lwt.catch
    (fun () -> read_all socket)
    (function
      | Unix.Unix_error ((ECONNRESET | EPIPE), _, _) -> Lwt.return ""
      | exn -> Lwt.fail exn)

(* [true] when [socket] is still open, with nothing to read, after [seconds]. *)
let still_open socket seconds =
  let chunk = Bytes.create 16 in
  Lwt.pick
    [ (Lwt_unix.read socket chunk 0 16 >|= fun _ -> false)
    ; (Lwt_unix.sleep seconds >|= fun () -> true)
    ]

let test_unread_body_without_body_timeout () =
  with_server ~idle_timeout:0.2 (fun connect ->
    connect () >>= fun socket ->
    write_all socket (unread_body_request "/early-413") >>= fun () ->
    read_exactly socket (String.length early_413_response) >>= fun first ->
    Alcotest.(check string) "response" early_413_response first;
    still_open socket 0.5 >>= fun still ->
    Alcotest.(check bool) "idle_timeout does not apply to a body" true still;
    (* The rest of the body ends the request; then the idle timeout closes the
       connection. *)
    write_all socket (String.make (100_000 - 3) 'b') >>= fun () ->
    read_all socket >>= fun rest ->
    Alcotest.(check string) "then closed at the idle timeout" "" rest;
    Lwt_unix.close socket)

let respond_while_body_read_pending ?body_timeout ~expect_close () =
  pending_response := None;
  pending_body_failed := false;
  let stop, stop_now = Lwt.wait () in
  with_server ?body_timeout ~stop ~grace:0.1 (fun connect ->
    connect () >>= fun socket ->
    write_all socket (unread_body_request "/respond-later") >>= fun () ->
    let rec respond () =
      match !pending_response with
      | Some reqd ->
        Request_descriptor.respond_with_string reqd (Response.create `OK) "late";
        Lwt.return_unit
      | None -> Lwt_unix.sleep 0.001 >>= respond
    in
    respond () >>= fun () ->
    let response = "HTTP/1.1 200 OK\r\ncontent-length: 4\r\n\r\nlate" in
    read_exactly socket (String.length response) >>= fun first ->
    Alcotest.(check string) "response" response first;
    (if expect_close
     then (
       let since = Unix.gettimeofday () in
       read_all socket >|= fun rest ->
       check_elapsed "closed" ~since ~at_least:0.25;
       Alcotest.(check string) "then closed" "" rest)
     else
       still_open socket 0.5 >>= fun still ->
       Alcotest.(check bool) "body not timed" true still;
       Alcotest.(check bool) "body still pending" false !pending_body_failed;
       (* A graceful stop ends the connection. *)
       Lwt.wakeup_later stop_now ();
       read_all_or_reset socket >|= fun rest ->
       Alcotest.(check string) "closed by the stop" "" rest)
    >>= fun () -> Lwt_unix.close socket);
  if expect_close
  then Alcotest.(check bool) "request body failed" true !pending_body_failed

let test_respond_while_body_read_pending () =
  respond_while_body_read_pending ~body_timeout:0.3 ~expect_close:true ()

let test_respond_while_body_read_pending_untimed () =
  respond_while_body_read_pending ~expect_close:false ()

let test_no_timeout_configured () =
  with_server (fun connect ->
    connect () >>= fun socket ->
    write_all socket get_hello >>= fun () ->
    read_exactly socket (String.length hello_response) >>= fun first ->
    Alcotest.(check string) "first response" hello_response first;
    Lwt_unix.sleep 0.2 >>= fun () ->
    write_all socket get_hello >>= fun () ->
    read_exactly socket (String.length hello_response) >>= fun second ->
    Alcotest.(check string) "second response" hello_response second;
    Lwt_unix.close socket)

let test_timeout_must_be_positive () =
  let socket = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  (match
     Ache_lwt.Server.create_connection_handler
       ~date:false
       ~idle_timeout:(Some 0.)
       ~request_handler
       ~error_handler
       (Unix.ADDR_INET (Unix.inet_addr_loopback, 0))
       socket
   with
  | (_ : unit Lwt.t) -> Alcotest.fail "expected Invalid_argument"
  | exception Invalid_argument _ -> ());
  Lwt_main.run (Lwt_unix.close socket)

let test_timeout_check_timing () =
  let addr = Unix.ADDR_INET (Unix.inet_addr_loopback, 0) in
  let socket = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  let raises f =
    match f () with
    | (_ : unit Lwt.t) -> false
    | exception Invalid_argument _ -> true
  in
  let bound =
    Ache_lwt.Server.create_connection_handler
      ~date:false
      ~config:Ache.Config.default
      ~idle_timeout:(Some 0.)
      ~head_timeout:(Some 1.)
      ~request_handler
      ~error_handler
  in
  Alcotest.(check bool)
    "bound, every option given: raises on a socket"
    true
    (raises (fun () -> bound addr socket));
  let take (h : Unix.sockaddr -> Lwt_unix.file_descr -> unit Lwt.t) = h in
  let inline =
    take
      (Ache_lwt.Server.create_connection_handler
         ~date:false
         ~idle_timeout:(Some 0.)
         ~request_handler
         ~error_handler)
  in
  Alcotest.(check bool)
    "inline: raises on a socket"
    true
    (raises (fun () -> inline addr socket));
  Lwt_main.run (Lwt_unix.close socket)

let contains s sub =
  let n = String.length sub in
  let rec go i =
    i + n <= String.length s && (String.sub s i n = sub || go (i + 1))
  in
  go 0

let test_pipelined_head_deadline () =
  with_server ~head_timeout:1.0 (fun connect ->
    connect () >>= fun socket ->
    write_all socket "GET /a HTTP/1.1\r\n" >>= fun () ->
    Lwt_unix.sleep 0.7 >>= fun () ->
    write_all socket "host: x\r\n\r\nGET /b HTTP/1.1\r\n" >>= fun () ->
    Lwt_unix.sleep 0.6 >>= fun () ->
    write_all socket "host: x\r\nconnection: close\r\n\r\n" >>= fun () ->
    read_all socket >>= fun output ->
    Alcotest.(check bool)
      "second response is not 408"
      false
      (contains output "408");
    Lwt_unix.close socket);
  Alcotest.(check int) "no error" 0 (List.length !server_errors);
  Alcotest.(check int) "both requests served" 2 !requests_seen

let test_head_timer_cancelled () =
  let before = Lwt_engine.timer_count () in
  with_server ~head_timeout:30. (fun connect ->
    List.init 5 Fun.id
    |> Lwt_list.iter_s (fun (_ : int) ->
      connect () >>= fun socket ->
      write_all socket "GET /hello HTTP/1.1\r\nHo" >>= fun () ->
      Lwt_unix.sleep 0.05 >>= fun () ->
      Lwt_unix.shutdown socket Unix.SHUTDOWN_SEND;
      read_all socket >>= fun (_ : string) -> Lwt_unix.close socket));
  let left = Lwt_engine.timer_count () - before in
  if left > 1 then Alcotest.failf "%d timers left after 5 connections" left

let live_words () =
  Gc.full_major ();
  (Gc.stat ()).live_words

let check_timers_bounded
      ?idle_timeout
      ?head_timeout
      ?body_timeout
      ?write_timeout
      ?stop
      ?(response = hello_response)
      ~requests
      send
  =
  let previous = Lwt_engine.get () in
  Lwt_engine.set ~destroy:false (new Lwt_engine.select);
  let growth = ref 0 in
  Fun.protect
    ~finally:(fun () -> Lwt_engine.set previous)
    (fun () ->
       with_server
         ?idle_timeout
         ?head_timeout
         ?body_timeout
         ?write_timeout
         ?stop
         (fun connect ->
            let earlier_live_sleeper = Lwt_unix.sleep 20. in
            connect () >>= fun socket ->
            Lwt_unix.setsockopt socket Unix.TCP_NODELAY true;
            let rec get_hellos n =
              if n = 0
              then Lwt.return_unit
              else
                send socket >>= fun () ->
                read_exactly socket (String.length response) >>= fun got ->
                if got <> response
                then Alcotest.failf "unexpected response %S" got;
                get_hellos (n - 1)
            in
            get_hellos (requests / 10) >>= fun () ->
            let before = live_words () in
            get_hellos requests >>= fun () ->
            growth := live_words () - before;
            Lwt.cancel earlier_live_sleeper;
            Lwt_unix.close socket));
  if !growth > requests
  then Alcotest.failf "heap grew by %d words over %d requests" !growth requests

let test_idle_timer_bounded () =
  check_timers_bounded
    ~idle_timeout:30.
    ~response:(hello_with_hint 30)
    ~requests:10_000
    (fun socket -> write_all socket get_hello)

let test_head_timer_bounded () =
  check_timers_bounded ~head_timeout:30. ~requests:500 (fun socket ->
    write_all socket "GET /hello HTTP/1.1\r\n" >>= fun () ->
    Lwt_unix.sleep 0.001 >>= fun () ->
    write_all socket "host: localhost\r\n\r\n")

let test_body_write_timers_bounded () =
  check_timers_bounded
    ~body_timeout:30.
    ~write_timeout:30.
    ~response:"HTTP/1.1 200 OK\r\ncontent-length: 1\r\n\r\n5"
    ~requests:500
    (fun socket ->
       write_all
         socket
         "POST /count HTTP/1.1\r\nhost: localhost\r\ncontent-length: 5\r\n\r\n"
       >>= fun () ->
       Lwt_unix.sleep 0.001 >>= fun () -> write_all socket "hello")

let check_connection_timers_bounded ?stop ~connections exchange =
  let previous = Lwt_engine.get () in
  Lwt_engine.set ~destroy:false (new Lwt_engine.select);
  let growth = ref 0 in
  Fun.protect
    ~finally:(fun () -> Lwt_engine.set previous)
    (fun () ->
       run (fun () ->
         let listening = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
         Lwt_unix.setsockopt listening Unix.SO_REUSEADDR true;
         Lwt_unix.bind listening Unix.(ADDR_INET (inet_addr_loopback, 0))
         >>= fun () ->
         Lwt_unix.listen listening 16;
         let addr = Lwt_unix.getsockname listening in
         let open_connections = ref 0 in
         let rec accept () =
           Lwt_unix.accept listening >>= fun (socket, client_addr) ->
           incr open_connections;
           Lwt.async (fun () ->
             Ache_lwt.Server.create_connection_handler
               ~date:false
               ~idle_timeout:(Some 30.)
               ~head_timeout:(Some 30.)
               ?stop
               ~request_handler
               ~error_handler
               client_addr
               socket
             >|= fun () -> decr open_connections);
           accept ()
         in
         let acceptor = accept () in
         let earlier_live_sleeper = Lwt_unix.sleep 20. in
         let rec all_closed () =
           if !open_connections = 0
           then Lwt.return_unit
           else Lwt_unix.sleep 0.001 >>= all_closed
         in
         let rec exchanges n =
           if n = 0
           then all_closed ()
           else
             let socket = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
             Lwt_unix.connect socket addr >>= fun () ->
             Lwt_unix.sleep 0.001 >>= fun () ->
             exchange socket >>= fun () ->
             Lwt_unix.close socket >>= fun () -> exchanges (n - 1)
         in
         exchanges (connections / 10) >>= fun () ->
         let before = live_words () in
         exchanges connections >>= fun () ->
         growth := live_words () - before;
         Lwt.cancel earlier_live_sleeper;
         Lwt.cancel acceptor;
         Lwt_unix.close listening));
  if !growth > connections
  then
    Alcotest.failf
      "heap grew by %d words over %d connections"
      !growth
      connections

let test_closed_connection_timers_bounded () =
  check_connection_timers_bounded ~connections:1000 (fun socket ->
    write_all socket "GET /hello HTTP/1.1\r\n" >>= fun () ->
    Lwt_unix.sleep 0.001 >>= fun () ->
    write_all socket "host: localhost\r\nconnection: close\r\n\r\n"
    >>= fun () ->
    read_all socket >>= fun output ->
    Alcotest.(check string)
      "response"
      "HTTP/1.1 200 OK\r\n\
       content-length: 11\r\n\
       connection: close\r\n\
       \r\n\
       hello world"
      output;
    Lwt.return_unit)

let test_idle_connection_timers_bounded () =
  check_connection_timers_bounded ~connections:1000 (fun socket ->
    write_all socket get_hello >>= fun () ->
    let expected = hello_with_hint 30 in
    read_exactly socket (String.length expected) >|= fun response ->
    Alcotest.(check string) "response" expected response)

let test_pending_stop_bounded () =
  let stop, (_ : unit Lwt.u) = Lwt.wait () in
  check_connection_timers_bounded ~stop ~connections:1000 (fun socket ->
    write_all socket get_hello >>= fun () ->
    let expected = hello_with_hint 30 in
    read_exactly socket (String.length expected) >|= fun response ->
    Alcotest.(check string) "response" expected response);
  check_timers_bounded ~stop ~requests:10_000 (fun socket ->
    write_all socket get_hello)

let test_failing_shutdown_and_close () =
  fail_read := false;
  fail_write := false;
  fail_shutdown := true;
  closes := 0;
  Fun.protect
    ~finally:(fun () -> fail_shutdown := false)
    (fun () ->
       with_server
         ~connection_handler:(fun ~request_handler ~error_handler addr socket ->
           Test_server.create_connection_handler
             ~date:false
             ~request_handler
             ~error_handler
             addr
             socket)
         (fun connect ->
            connect () >>= fun socket ->
            write_all
              socket
              "GET /hello HTTP/1.1\r\n\
               host: localhost\r\n\
               connection: close\r\n\
               \r\n"
            >>= fun () ->
            read_all socket >>= fun output ->
            Alcotest.(check string)
              "status line"
              "HTTP/1.1 200 OK"
              (status_line output);
            Lwt_unix.close socket));
  Alcotest.(check int) "close called once" 1 !closes

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
  fail_read := false;
  fail_write := false;
  closes := 0;
  with_server (fun connect ->
    connect () >>= Test_runtime.Client.create_connection >>= fun client ->
    let upgraded, resolve_upgraded = Lwt.wait () in
    let request_body =
      Test_runtime.Client.request
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
          Lwt.wakeup_later resolve_upgraded response)
    in
    Body.Writer.close request_body;
    upgraded >>= fun (_ : Response.t) ->
    Test_runtime.Client.upgrade
      client
      (Runtime.make (module Raising) { Raising.sent = false });
    let rec until_closed () =
      if !closes > 0
      then Lwt.return_unit
      else Lwt_unix.sleep 0.01 >>= until_closed
    in
    until_closed ());
  Alcotest.(check int) "close called once" 1 !closes

let expired_after timer =
  let since = Unix.gettimeofday () in
  Ache_lwt.Timeout.expired timer >|= fun () -> Unix.gettimeofday () -. since

let check_between name ~min ~max elapsed =
  if elapsed < min || elapsed > max
  then Alcotest.failf "%s: %.3f s, expected %.3f to %.3f s" name elapsed min max

let test_timeout_expires () =
  run (fun () ->
    let timer = Ache_lwt.Timeout.create 0.1 in
    Ache_lwt.Timeout.set timer;
    expired_after timer >|= check_between "expired" ~min:0.1 ~max:0.5)

let test_timeout_set_moves_deadline () =
  run (fun () ->
    let timer = Ache_lwt.Timeout.create 0.2 in
    Ache_lwt.Timeout.set timer;
    let expired = expired_after timer in
    Lwt_unix.sleep 0.1 >>= fun () ->
    Ache_lwt.Timeout.set timer;
    expired >>= fun elapsed ->
    check_between "moved deadline" ~min:0.3 ~max:0.7 elapsed;
    Ache_lwt.Timeout.set timer;
    expired_after timer >|= check_between "set again" ~min:0.2 ~max:0.6)

let test_timeout_stop () =
  run (fun () ->
    let timer = Ache_lwt.Timeout.create 0.05 in
    Ache_lwt.Timeout.set timer;
    let expired = Ache_lwt.Timeout.expired timer in
    Ache_lwt.Timeout.stop timer;
    Ache_lwt.Timeout.stop timer;
    Lwt_unix.sleep 0.2 >>= fun () ->
    Alcotest.(check bool) "pending after stop" true (Lwt.is_sleeping expired);
    let since = Unix.gettimeofday () in
    Ache_lwt.Timeout.set timer;
    expired >|= fun () ->
    check_between
      "set after stop"
      ~min:0.05
      ~max:0.4
      (Unix.gettimeofday () -. since))

let test_timeout_lengths () =
  run (fun () ->
    let short = Ache_lwt.Timeout.create 0.05
    and long = Ache_lwt.Timeout.create 0.25 in
    Ache_lwt.Timeout.set long;
    Ache_lwt.Timeout.set short;
    let long_expired = expired_after long in
    expired_after short >>= fun elapsed ->
    check_between "short" ~min:0.05 ~max:0.2 elapsed;
    Alcotest.(check bool) "long pending" true (Lwt.is_sleeping long_expired);
    long_expired >|= check_between "long" ~min:0.25 ~max:0.6)

let test_timeout_cancelled_expired () =
  run (fun () ->
    let timer = Ache_lwt.Timeout.create 0.1 in
    Ache_lwt.Timeout.set timer;
    let since = Unix.gettimeofday () in
    Lwt.pick [ Ache_lwt.Timeout.expired timer; Lwt.return_unit ] >>= fun () ->
    Lwt.cancel (Ache_lwt.Timeout.expired timer);
    Ache_lwt.Timeout.expired timer >|= fun () ->
    check_between
      "after cancel"
      ~min:0.1
      ~max:0.5
      (Unix.gettimeofday () -. since))

let test_flood_does_not_block_event_loop () =
  let flood_seconds = 2.0 in
  let batch = String.concat "" (List.init 200 (fun _ -> get_hello)) in
  let max_gap = ref 0. in
  with_server (fun connect ->
    connect () >>= fun socket ->
    let fd = Lwt_unix.unix_file_descr socket in
    Unix.clear_nonblock fd;
    let received = Atomic.make (-1) in
    let writer =
      Domain.spawn (fun () ->
        let until = Unix.gettimeofday () +. flood_seconds in
        (try
           while Unix.gettimeofday () < until do
             ignore (Unix.write_substring fd batch 0 (String.length batch))
           done
         with
        | Unix.Unix_error _ -> ());
        try Unix.shutdown fd Unix.SHUTDOWN_SEND with Unix.Unix_error _ -> ())
    in
    let (_ : unit Domain.t) =
      Domain.spawn (fun () ->
        let chunk = Bytes.create 65536 in
        let rec go total =
          match Unix.read fd chunk 0 (Bytes.length chunk) with
          | 0 -> total
          | n -> go (total + n)
          | exception Unix.Unix_error _ -> total
        in
        Atomic.set received (go 0))
    in
    let flooding = ref true in
    let rec ticker last =
      Lwt_unix.sleep 0.01 >>= fun () ->
      let now = Unix.gettimeofday () in
      max_gap := Float.max !max_gap (now -. last);
      if !flooding then ticker now else Lwt.return_unit
    in
    let ticking = ticker (Unix.gettimeofday ()) in
    Lwt_unix.sleep flood_seconds >>= fun () ->
    flooding := false;
    ticking >>= fun () ->
    let rec drained () =
      if Atomic.get received >= 0
      then Lwt.return_unit
      else Lwt_unix.sleep 0.01 >>= drained
    in
    drained () >>= fun () ->
    Domain.join writer;
    if Atomic.get received = 0 then Alcotest.fail "no response";
    Lwt_unix.close socket);
  if !requests_seen = 0 then Alcotest.fail "no request served";
  if !max_gap > 1.0 then Alcotest.failf "event loop blocked for %.3f s" !max_gap

let test_timeout_create_must_be_positive () =
  List.iter
    (fun seconds ->
       match Ache_lwt.Timeout.create seconds with
       | (_ : Ache_lwt.Timeout.t) -> Alcotest.failf "accepted %g" seconds
       | exception Invalid_argument _ -> ())
    [ 0.; -1.; Float.nan ]

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
  let result, resolve = Lwt.wait () in
  let finish r = if Lwt.is_sleeping result then Lwt.wakeup_later resolve r in
  let writer =
    Ache_lwt.Client.request
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
  result

let test_stray_bytes_on_idle_connection () =
  let port, server = stray_server (List.map snd stray_cases) in
  List.iter
    (fun (name, _) ->
       Lwt_main.run
         (Lwt_unix.with_timeout 10.0 (fun () ->
            let socket = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
            Lwt.bind
              (Lwt_unix.connect
                 socket
                 (Unix.ADDR_INET (Unix.inet_addr_loopback, port)))
              (fun () ->
                 Lwt.bind (Ache_lwt.Client.create_connection socket)
                 @@ fun client ->
                 Lwt.bind (stray_outcome client) @@ fun first ->
                 Alcotest.(check string) (name ^ ": first") "200 ok" first;
                 (* The stray bytes arrive while this thread does not run the
                    read loop. *)
                 Unix.sleepf 0.2;
                 Lwt.bind (stray_outcome client) @@ fun second ->
                 Alcotest.(check string)
                   (name ^ ": second")
                   "Connection_closed"
                   second;
                 Ache_lwt.Client.shutdown client)));
       ())
    stray_cases;
  Alcotest.(check (list string))
    "second requests not sent"
    (List.map (fun _ -> "") stray_cases)
    (Domain.join server)

let slow_request = "GET /slow HTTP/1.1\r\nhost: localhost\r\n\r\n"

let rec until_slow () =
  match !slow_reqd with
  | Some reqd -> Lwt.return reqd
  | None -> Lwt_unix.sleep 0.005 >>= until_slow

let respond_done reqd =
  Request_descriptor.respond_with_string
    reqd
    (Response.create ~headers:(Headers.of_list [ "content-length", "4" ]) `OK)
    "done"

let check_within name ~since ~min ~max =
  let elapsed = Unix.gettimeofday () -. since in
  if elapsed < min || elapsed > max
  then
    Alcotest.failf
      "%s: after %.3f s, expected %.3f to %.3f s"
      name
      elapsed
      min
      max

let test_drain_finishes_request () =
  slow_reqd := None;
  let stop, stop_now = Lwt.wait () in
  with_server ~stop ~grace:5. (fun connect ->
    connect () >>= fun socket ->
    write_all socket (slow_request ^ get_hello) >>= until_slow >>= fun reqd ->
    Lwt.wakeup_later stop_now ();
    Lwt_unix.sleep 0.1 >>= fun () ->
    respond_done reqd;
    read_all socket >>= fun output ->
    Alcotest.(check bool)
      "connection: close"
      true
      (contains output "\r\nconnection: close\r\n");
    Alcotest.(check bool)
      "200 with its body, then end of input"
      true
      (String.starts_with ~prefix:"HTTP/1.1 200 OK\r\n" output
      && String.ends_with ~suffix:"\r\n\r\ndone" output);
    Lwt_unix.close socket);
  Alcotest.(check int) "pipelined request not served" 1 !requests_seen;
  check_server_sockets_closed ()

let test_drain_idle () =
  let stop, stop_now = Lwt.wait () in
  with_server ~stop (fun connect ->
    connect () >>= fun fresh ->
    connect () >>= fun used ->
    write_all used get_hello >>= fun () ->
    read_exactly used (String.length hello_response) >>= fun first ->
    Alcotest.(check string) "keep-alive response" hello_response first;
    Lwt_unix.sleep 0.05 >>= fun () ->
    let since = Unix.gettimeofday () in
    Lwt.wakeup_later stop_now ();
    read_all fresh >>= fun fresh_output ->
    read_all used >>= fun used_output ->
    check_within "idle connections closed" ~since ~min:0. ~max:0.3;
    Alcotest.(check string) "new connection: nothing written" "" fresh_output;
    Alcotest.(check string) "keep-alive: nothing more" "" used_output;
    Lwt_unix.close fresh >>= fun () -> Lwt_unix.close used);
  Alcotest.(check int) "no error" 0 (List.length !server_errors);
  check_server_sockets_closed ()

let test_drain_grace_cancels () =
  slow_reqd := None;
  let stop, stop_now = Lwt.wait () in
  let started = Unix.gettimeofday () in
  with_server ~stop ~grace:0.2 (fun connect ->
    connect () >>= fun socket ->
    write_all socket slow_request >>= until_slow
    >>= fun (_ : Request_descriptor.t) ->
    let since = Unix.gettimeofday () in
    Lwt.wakeup_later stop_now ();
    read_all_or_reset socket >>= fun output ->
    check_within "closed at the grace" ~since ~min:0.15 ~max:1.0;
    Alcotest.(check string) "no response" "" output;
    Lwt_unix.close socket);
  check_within "server ended" ~since:started ~min:0. ~max:2.0;
  check_server_sockets_closed ();
  (* The handler responds after the connection was cancelled. *)
  respond_done (Option.get !slow_reqd)

let test_handler_after_stop () =
  with_server ~stop:Lwt.return_unit (fun connect ->
    connect () >>= fun socket ->
    Lwt.catch
      (fun () -> write_all socket get_hello)
      (fun (_ : exn) -> Lwt.return_unit)
    >>= fun () ->
    read_all_or_reset socket >>= fun output ->
    Alcotest.(check string) "no response" "" output;
    Lwt_unix.close socket);
  Alcotest.(check int) "request handler not called" 0 !requests_seen;
  check_server_sockets_closed ()

let test_drain_websocket () =
  let ended = wait_ws_end () in
  let stop, stop_now = Lwt.wait () in
  with_server ~stop ~grace:5. (fun connect ->
    connect () >>= fun socket ->
    write_all socket (ws_handshake "/ws-echo") >>= fun () ->
    read_head socket >>= fun head ->
    Alcotest.(check string) "101" ws_101 head;
    Lwt.wakeup_later stop_now ();
    read_ws_frame socket >>= fun close ->
    Alcotest.(check (pair int string)) "close 1001" (0x88, "\x03\xe9") close;
    (* A drain closes at once, without waiting for the client's close frame:
       sending one could meet a closed socket and a reset. *)
    Lwt_unix.shutdown socket Unix.SHUTDOWN_SEND;
    read_all socket >>= fun rest ->
    Alcotest.(check string) "then end of input" "" rest;
    ended >>= fun (_ : (string, exn) result) -> Lwt_unix.close socket);
  check_server_sockets_closed ()

let test_grace_must_be_positive () =
  let socket = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  (match
     Ache_lwt.Server.create_connection_handler
       ~grace:0.
       ~request_handler
       ~error_handler
       (Unix.ADDR_INET (Unix.inet_addr_loopback, 0))
       socket
   with
  | (_ : unit Lwt.t) -> Alcotest.fail "expected Invalid_argument"
  | exception Invalid_argument _ -> ());
  Lwt_main.run (Lwt_unix.close socket)

let count_request ?(target = "/count") length =
  Printf.sprintf
    "POST %s HTTP/1.1\r\n\
     host: localhost\r\n\
     connection: close\r\n\
     content-length: %d\r\n\
     \r\n"
    target
    length

(* Writes [chunks] of [size] bytes, [gap] seconds apart, and stops at the first
   failed write (the server closed). *)
let trickle socket ~chunks ~size ~gap =
  let chunk = String.make size 'b' in
  let rec go n =
    if n = 0
    then Lwt.return_unit
    else
      Lwt_unix.sleep gap >>= fun () ->
      Lwt.try_bind
        (fun () -> write_all socket chunk)
        (fun () -> go (n - 1))
        (fun (_ : exn) -> Lwt.return_unit)
  in
  go chunks

(* The server lingers after its response (RFC 9112 9.6): the trickle does not
   make it reset the connection, so the response always arrives. *)
let test_body_trickle_times_out () =
  List.iter
    (fun (target, expected) ->
       body_cut := false;
       with_server ~body_timeout:0.3 (fun connect ->
         connect () >>= fun socket ->
         write_all socket (count_request ~target 100) >>= fun () ->
         let since = Unix.gettimeofday () in
         let writer = trickle socket ~chunks:20 ~size:1 ~gap:0.2 in
         read_all socket >>= fun output ->
         check_within (target ^ ": timed out") ~since ~min:0.3 ~max:1.0;
         (* ache-lwt closes without lingering (task 32): when the trickle meets
            the closed socket, the reset can drop the response. *)
         if output <> ""
         then
           Alcotest.(check bool)
             (target ^ ": " ^ expected)
             true
             (String.starts_with ~prefix:expected output);
         Lwt.cancel writer;
         Lwt_unix.close socket);
       Alcotest.(check bool) (target ^ ": on_error") true !body_cut)
    [ "/count", "HTTP/1.1 408 Request Timeout\r\n"
    ; "/count-started", "HTTP/1.1 200 OK\r\n"
    ]

let response_408 = "HTTP/1.1 408 Request Timeout\r\nconnection: close\r\n\r\n"

(* A client that keeps sending after the 408 reads all of it, then the end of
   input. Without [idle_timeout] the server lingers 2 s after its output ends,
   then closes, though the client still sends. *)
let test_body_timeout_linger () =
  body_cut := false;
  with_server ~body_timeout:0.3 (fun connect ->
    connect () >>= fun socket ->
    write_all socket (count_request 100_000_000) >>= fun () ->
    (* One byte every 50 ms until past [body_timeout], then 1 KiB every 10
       ms. *)
    let rec send n =
      let chunk = if n < 10 then "b" else String.make 1024 'b' in
      Lwt_unix.sleep (if n < 10 then 0.05 else 0.01) >>= fun () ->
      write_all socket chunk >>= fun () -> send (n + 1)
    in
    let writer =
      Lwt.catch (fun () -> send 0) (fun (_ : exn) -> Lwt.return_unit)
    in
    read_all socket >>= fun output ->
    Alcotest.(check string) "408, then the end of input" response_408 output;
    let since = Unix.gettimeofday () in
    Alcotest.(check bool) "client still sends" true (Lwt.is_sleeping writer);
    writer >>= fun () ->
    check_within "server closes after the linger" ~since ~min:1.5 ~max:3.0;
    check_server_sockets_closed ();
    Lwt_unix.close socket);
  Alcotest.(check bool) "on_error" true !body_cut

let test_body_progress_not_timed_out () =
  body_cut := false;
  with_server ~body_timeout:0.3 (fun connect ->
    connect () >>= fun socket ->
    write_all socket (count_request 12_600) >>= fun () ->
    let since = Unix.gettimeofday () in
    trickle socket ~chunks:6 ~size:2_100 ~gap:0.15 >>= fun () ->
    read_all_or_reset socket >>= fun output ->
    check_within "longer than body_timeout" ~since ~min:0.6 ~max:2.0;
    Alcotest.(check bool)
      "200 with the byte count"
      true
      (String.starts_with ~prefix:"HTTP/1.1 200 OK\r\n" output
      && String.ends_with ~suffix:"\r\n\r\n12600" output);
    Lwt_unix.close socket);
  Alcotest.(check bool) "no on_error" false !body_cut

let test_write_timeout () =
  let flushed, resolve_flushed = Lwt.wait () in
  stream_flush_closed := Some resolve_flushed;
  with_server ~write_timeout:0.3 (fun connect ->
    connect () >>= fun socket ->
    Lwt_unix.setsockopt_int socket Unix.SO_RCVBUF 4096;
    write_all socket "GET /stream HTTP/1.1\r\nhost: localhost\r\n\r\n"
    >>= fun () ->
    let since = Unix.gettimeofday () in
    flushed >>= fun () ->
    check_within "flush gets `Closed" ~since ~min:0.3 ~max:1.5;
    Lwt_unix.close socket);
  check_server_sockets_closed ()

let test_slow_handler_not_timed_out () =
  slow_reqd := None;
  with_server
    ~idle_timeout:0.3
    ~head_timeout:0.3
    ~body_timeout:0.3
    ~write_timeout:0.3
    (fun connect ->
       connect () >>= fun socket ->
       write_all socket (count_request ~target:"/slow-noread" 5 ^ "hello")
       >>= until_slow
       >>= fun reqd ->
       Lwt_unix.sleep 1.0 >>= fun () ->
       Body.Reader.close (Request_descriptor.request_body reqd);
       respond_done reqd;
       read_all socket >>= fun output ->
       Alcotest.(check string)
         "response after 1 s"
         "HTTP/1.1 200 OK\r\ncontent-length: 4\r\nconnection: close\r\n\r\ndone"
         output;
       Lwt_unix.close socket);
  Alcotest.(check int) "no error" 0 (List.length !server_errors)

let websocket_idle_timeout ~idle_timeout () =
  let ended = wait_ws_end () in
  with_server ~idle_timeout (fun connect ->
    connect () >>= fun socket ->
    write_all socket (ws_handshake "/ws-echo") >>= fun () ->
    read_head socket >>= fun head ->
    Alcotest.(check string) "101" ws_101 head;
    let since = Unix.gettimeofday () in
    read_exactly socket 4 >>= fun close ->
    check_within
      "idle close"
      ~since
      ~min:(idle_timeout -. 0.05)
      ~max:(idle_timeout +. 0.5);
    Alcotest.(check string) "close 1001" "\x88\x02\x03\xe9" close;
    ended >>= fun result ->
    Alcotest.(check (result string reject))
      "session sees the end"
      (Ok "receive None")
      result;
    read_all_or_reset socket >>= fun rest ->
    Alcotest.(check string) "then end of input" "" rest;
    Lwt_unix.close socket);
  check_server_sockets_closed ()

let test_websocket_idle_timeout = websocket_idle_timeout ~idle_timeout:0.3

(* With a [Keep-Alive] hint, the timer between requests runs 1 s longer, but a
   WebSocket keeps [idle_timeout], and the 101 gets no hint. *)
let test_websocket_idle_with_hint = websocket_idle_timeout ~idle_timeout:1.0

let test_timeouts_off () =
  with_server (fun connect ->
    connect () >>= fun socket ->
    write_all socket "GET /hello HTTP/1.1\r\n" >>= fun () ->
    still_open socket 0.5 >>= fun still ->
    Alcotest.(check bool) "partial head, every timeout None" true still;
    Lwt_unix.close socket)

let hello_head ?(extra = "") () =
  "HTTP/1.1 200 OK\r\ncontent-length: 11\r\n" ^ extra ^ "\r\nhello world"

let test_keep_alive_hint () =
  with_server ~idle_timeout:3.5 (fun connect ->
    connect () >>= fun socket ->
    write_all socket get_hello >>= fun () ->
    let expected = hello_with_hint 3 in
    read_exactly socket (String.length expected) >>= fun output ->
    Alcotest.(check string) "timeout=3 for 3.5 s" expected output;
    Lwt_unix.close socket);
  with_server ?idle_timeout:None (fun connect ->
    connect () >>= fun socket ->
    write_all socket get_hello >>= fun () ->
    read_exactly socket (String.length hello_response) >>= fun output ->
    Alcotest.(check string) "no hint without idle_timeout" hello_response output;
    Lwt_unix.close socket);
  with_server ~idle_timeout:0.5 (fun connect ->
    connect () >>= fun socket ->
    write_all socket get_hello >>= fun () ->
    read_exactly socket (String.length hello_response) >>= fun output ->
    Alcotest.(check string) "no hint under 1 s" hello_response output;
    Lwt_unix.close socket)

let test_keep_alive_hint_max () =
  let config =
    { Ache.Config.default with max_requests_per_connection = Some 3 }
  in
  with_server ~config ~idle_timeout:3.5 (fun connect ->
    connect () >>= fun socket ->
    write_all socket (get_hello ^ get_hello ^ get_hello) >>= fun () ->
    read_all socket >>= fun output ->
    Alcotest.(check string)
      "max counts down, then connection: close without a hint"
      (hello_head ~extra:"Keep-Alive: timeout=3, max=2\r\n" ()
      ^ hello_head ~extra:"Keep-Alive: timeout=3, max=1\r\n" ()
      ^ hello_head ~extra:"connection: close\r\n" ())
      output;
    Lwt_unix.close socket)

let test_keep_alive_idle_timer_longer () =
  with_server ~idle_timeout:1. (fun connect ->
    connect () >>= fun socket ->
    write_all socket get_hello >>= fun () ->
    let expected = hello_with_hint 1 in
    read_exactly socket (String.length expected) >>= fun output ->
    Alcotest.(check string) "timeout=1" expected output;
    let since = Unix.gettimeofday () in
    still_open socket 1.4 >>= fun still ->
    Alcotest.(check bool) "open 1.4 s after the response" true still;
    read_all socket >>= fun rest ->
    check_within "closed at 2 s" ~since ~min:1.9 ~max:2.5;
    Alcotest.(check string) "nothing more" "" rest;
    Lwt_unix.close socket)

(* [Ok body] or the client error of a request. *)
let request_result client target =
  let p, u = Lwt.wait () in
  let body =
    Ache_lwt.Client.request
      client
      (Request.create
         ~headers:(Headers.of_list [ "host", "localhost" ])
         `GET
         target)
      ~error_handler:(fun error ->
        if Lwt.is_sleeping p then Lwt.wakeup_later u (Error error))
      ~response_handler:(fun _ response_body ->
        Lwt.async (fun () ->
          read_body response_body >|= fun body ->
          if Lwt.is_sleeping p then Lwt.wakeup_later u (Ok body)))
  in
  Body.Writer.close body;
  p

let check_hello name = function
  | Ok body -> Alcotest.(check string) name "hello world" body
  | Error _ -> Alcotest.failf "%s: request failed" name

let test_client_reuse_deadline () =
  with_server ~idle_timeout:3.5 (fun connect ->
    connect () >>= Ache_lwt.Client.create_connection >>= fun client ->
    request_result client "/hello" >>= fun first ->
    check_hello "first" first;
    Lwt_unix.sleep 0.3 >>= fun () ->
    request_result client "/hello" >>= fun second ->
    check_hello "reused within the deadline" second;
    Alcotest.(check int) "one connection" 1 (List.length !server_sockets);
    Alcotest.(check bool) "open" false (Ache_lwt.Client.is_closed client);
    Lwt_unix.sleep 1.2 >>= fun () ->
    Alcotest.(check bool)
      "closed 1 s (3 - 2) after the response"
      true
      (Ache_lwt.Client.is_closed client);
    request_result client "/hello" >>= fun third ->
    (match third with
    | Error `Connection_closed -> ()
    | Ok _ -> Alcotest.fail "request after the deadline served"
    | Error _ -> Alcotest.fail "unexpected error");
    Lwt_unix.sleep 0.05);
  Alcotest.(check int)
    "the late request never reached the server"
    2
    !requests_seen

let test_client_deadline_before_timer () =
  with_server ~idle_timeout:2.5 (fun connect ->
    connect () >>= Ache_lwt.Client.create_connection >>= fun client ->
    request_result client "/hello" >>= fun first ->
    check_hello "first" first;
    (* timeout=2: s = 0, the connection is not reusable at once. *)
    request_result client "/hello" >>= fun second ->
    (match second with
    | Error `Connection_closed -> ()
    | Ok _ -> Alcotest.fail "reused with timeout=2"
    | Error _ -> Alcotest.fail "unexpected error");
    Ache_lwt.Client.shutdown client);
  Alcotest.(check int) "one request served" 1 !requests_seen

let test_client_deadline_timer_late () =
  with_server ~idle_timeout:3.5 (fun connect ->
    connect () >>= Ache_lwt.Client.create_connection >>= fun client ->
    request_result client "/hello" >>= fun first ->
    check_hello "first" first;
    Lwt_unix.sleep 0.05 >>= fun () ->
    (* Block the event loop past the deadline, so its timer has not run. *)
    Unix.sleepf 1.1;
    request_result client "/hello" >>= fun second ->
    (match second with
    | Error `Connection_closed -> ()
    | Ok _ -> Alcotest.fail "reused past the deadline"
    | Error _ -> Alcotest.fail "unexpected error");
    Ache_lwt.Client.shutdown client);
  Alcotest.(check int) "one request served" 1 !requests_seen

let test_client_without_hint () =
  with_server ?idle_timeout:None (fun connect ->
    connect () >>= Ache_lwt.Client.create_connection >>= fun client ->
    request_result client "/hello" >>= fun first ->
    check_hello "first" first;
    Lwt_unix.sleep 1.2 >>= fun () ->
    Alcotest.(check bool) "open" false (Ache_lwt.Client.is_closed client);
    request_result client "/hello" >>= fun second ->
    check_hello "reused after 1.2 s" second;
    Ache_lwt.Client.shutdown client)

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
  with_server
    ~connection_handler:(fun ~request_handler:_ ~error_handler addr socket ->
      Ache_lwt.Server.create_connection_handler
        ~config:auto_continue
        ~date:false
        ~idle_timeout:None
        ~request_handler:continue_handler
        ~error_handler
        addr
        socket)
    (fun connect ->
       connect () >>= fun socket ->
       write_all socket continue_request >>= fun () ->
       Lwt_unix.with_timeout 0.5 (fun () ->
         read_exactly socket (String.length continue_100))
       >>= fun interim ->
       Alcotest.(check string) "100 Continue within 0.5 s" continue_100 interim;
       write_all socket (String.make continue_size 'x') >>= fun () ->
       read_exactly socket (String.length continue_final) >>= fun final ->
       Alcotest.(check string) "the whole body was read" continue_final final;
       Lwt_unix.close socket)

let () =
  Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
  Alcotest.run
    "ache-lwt loopback"
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
        ; ( "a flooding client does not block the event loop"
          , `Quick
          , test_flood_does_not_block_event_loop )
        ; "101 upgrade to an echo protocol", `Quick, test_upgrade
        ; ( "CONNECT tunnel gets the bytes sent with the head"
          , `Quick
          , test_connect_tunnel_bytes_after_head )
        ; ( "an incomplete token sent with the upgrade request reaches the new \
             protocol"
          , `Quick
          , test_upgrade_incomplete_token_reaches_new_protocol )
        ; "WebSocket echo and close", `Quick, test_websocket_echo
        ; "WebSocket server close", `Quick, test_websocket_server_close
        ; ( "WebSocket close timeout for a client that does not answer"
          , `Quick
          , test_websocket_close_timeout )
        ; ( "WebSocket close timeout for a silent client and no idle timeout"
          , `Quick
          , test_websocket_close_timeout_silent_client ?idle_timeout:None )
        ; ( "WebSocket close timeout for a silent client and an idle timeout"
          , `Quick
          , test_websocket_close_timeout_silent_client ~idle_timeout:30. )
        ; ( "WebSocket close timeout after a pong to a client that then stays \
             silent"
          , `Quick
          , test_websocket_close_timeout_pong_then_silent )
        ; ( "WebSocket close timeout closes the server socket of a client that \
             keeps sending"
          , `Quick
          , test_websocket_close_timeout_server_closes )
        ; ( "WebSocket close timeout with the callback API"
          , `Quick
          , test_websocket_close_timeout_callback_api )
        ; "WebSocket ping, pong and close reason", `Quick, test_websocket_ping
        ; ( "WebSocket session that returns closes 1000"
          , `Quick
          , test_websocket_session_return_closes )
        ; ( "WebSocket session exception closes 1011"
          , `Quick
          , test_websocket_session_exception )
        ; ( "WebSocket ping timeout and Closed"
          , `Quick
          , test_websocket_ping_timeout )
        ; ( "WebSocket receive queue pauses reading"
          , `Quick
          , test_websocket_receive_backpressure )
        ; ( "WebSocket cancelled receive loses no message"
          , `Quick
          , test_websocket_cancelled_receive )
        ; ( "WebSocket cancelled receive, then close"
          , `Quick
          , test_websocket_cancelled_receive_then_close )
        ; ( "WebSocket cancelled receives do not pile up"
          , `Quick
          , test_websocket_cancelled_receives_bounded )
        ; ( "WebSocket server sending to a reset peer ends the connection"
          , `Quick
          , test_websocket_peer_reset )
        ; "read failure ends the connection", `Quick, test_read_failure
        ; "write failure ends the connection", `Quick, test_write_failure
        ; "request handler exception", `Quick, test_handler_exception
        ; "late error response", `Quick, test_late_error_response
        ; ( "client shutdown closes the socket once"
          , `Quick
          , test_client_shutdown_closes_once )
        ; "peer closes mid-body", `Quick, test_eof_mid_body
        ; ( "flush calls back with `Closed after the peer closes"
          , `Quick
          , test_flush_after_peer_closed )
        ; ( "flush calls back with `Closed after writing to a closed peer \
             (EPIPE)"
          , `Quick
          , test_flush_after_peer_closed_epipe )
        ; ( "failing shutdown_receive and close end the connection"
          , `Quick
          , test_failing_shutdown_and_close )
        ; ( "an exception from report_exn ends the connection"
          , `Quick
          , test_report_exn_raises )
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
        ; ( "head timeout counts from the first byte of the head"
          , `Quick
          , test_head_deadline )
        ; "slow request body is not cut", `Quick, test_slow_body_not_cut
        ; "no timeout configured", `Quick, test_no_timeout_configured
        ; ( "unread body after the response: body_timeout"
          , `Quick
          , test_unread_body_body_timeout )
        ; ( "unread body after the response, no body_timeout: stays open"
          , `Quick
          , test_unread_body_without_body_timeout )
        ; ( "unread body past 256 KiB closes the connection"
          , `Quick
          , test_unread_body_drain_limit )
        ; ( "response while a body read waits: body_timeout"
          , `Quick
          , test_respond_while_body_read_pending )
        ; ( "response while a body read waits, no body_timeout: stays open"
          , `Quick
          , test_respond_while_body_read_pending_untimed )
        ; "a timeout must be positive", `Quick, test_timeout_must_be_positive
        ; ( "when the timeout arguments are checked"
          , `Quick
          , test_timeout_check_timing )
        ; ( "a pipelined head gets its own deadline"
          , `Quick
          , test_pipelined_head_deadline )
        ; ( "the head timer ends with the connection"
          , `Quick
          , test_head_timer_cancelled )
        ; ( "keep-alive requests do not pile up idle timers"
          , `Quick
          , test_idle_timer_bounded )
        ; ( "requests with split heads do not pile up head timers"
          , `Quick
          , test_head_timer_bounded )
        ; ( "requests with body and write clocks do not pile up timers"
          , `Quick
          , test_body_write_timers_bounded )
        ; ( "closed connections do not pile up timers"
          , `Quick
          , test_closed_connection_timers_bounded )
        ; ( "keep-alive connections closed while idle do not pile up timers"
          , `Quick
          , test_idle_connection_timers_bounded )
        ] )
    ; ( "drain"
      , [ ( "a request in flight finishes with connection: close"
          , `Quick
          , test_drain_finishes_request )
        ; "idle connections close at once", `Quick, test_drain_idle
        ; ( "a request still running at the grace is cancelled"
          , `Quick
          , test_drain_grace_cancels )
        ; ( "a connection handled after stop is closed unread"
          , `Quick
          , test_handler_after_stop )
        ; "WebSocket gets close 1001", `Quick, test_drain_websocket
        ; "grace must be positive", `Quick, test_grace_must_be_positive
        ; ( "a pending stop does not grow with connections or requests"
          , `Quick
          , test_pending_stop_bounded )
        ] )
    ; ( "in-progress timeouts"
      , [ ( "a body trickle times out though each gap is shorter"
          , `Quick
          , test_body_trickle_times_out )
        ; ( "a client that sends after a 408 reads it, then the end of input"
          , `Quick
          , test_body_timeout_linger )
        ; ( "a body that makes progress is not timed out"
          , `Quick
          , test_body_progress_not_timed_out )
        ; ( "a client that stops reading is closed at write_timeout"
          , `Quick
          , test_write_timeout )
        ; ( "a slow handler that reads no body is not timed out"
          , `Quick
          , test_slow_handler_not_timed_out )
        ; ( "an idle WebSocket closes with 1001"
          , `Quick
          , test_websocket_idle_timeout )
        ; "None turns every timeout off", `Quick, test_timeouts_off
        ] )
    ; ( "keep-alive hints"
      , [ ( "the server sends timeout= with an idle timeout of 1 s or more"
          , `Quick
          , test_keep_alive_hint )
        ; ( "max= counts down, no hint on the closing response"
          , `Quick
          , test_keep_alive_hint_max )
        ; ( "the idle timer runs 1 s longer than the hint"
          , `Quick
          , test_keep_alive_idle_timer_longer )
        ; ( "a WebSocket keeps idle_timeout and the 101 has no hint"
          , `Quick
          , test_websocket_idle_with_hint )
        ; ( "the client closes a connection past its reuse deadline"
          , `Quick
          , test_client_reuse_deadline )
        ; ( "timeout=2 makes a connection not reusable at once"
          , `Quick
          , test_client_deadline_before_timer )
        ; ( "a request past the deadline fails before its timer runs"
          , `Quick
          , test_client_deadline_timer_late )
        ; ( "without a hint the client keeps the connection"
          , `Quick
          , test_client_without_hint )
        ] )
    ; ( "Timeout"
      , [ "expires after its deadline", `Quick, test_timeout_expires
        ; "set moves the deadline", `Quick, test_timeout_set_moves_deadline
        ; "stop keeps it from expiring", `Quick, test_timeout_stop
        ; "two lengths expire in order", `Quick, test_timeout_lengths
        ; ( "cancelling expired does not change it"
          , `Quick
          , test_timeout_cancelled_expired )
        ; ( "create rejects a length that is not positive"
          , `Quick
          , test_timeout_create_must_be_positive )
        ] )
    ]
