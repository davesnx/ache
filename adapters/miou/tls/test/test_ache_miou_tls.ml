open Ache

let certs = Ache_test_certs.generate ()
let server_config = Ache_test_certs.server_config certs
let client_config = Ache_test_certs.client_config certs
let localhost = Domain_name.(host_exn (of_string_exn "localhost"))
let server_errors : Server_connection.error list ref = ref []
let partial_body_cut = ref false

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

let pending_upgrade : Request_descriptor.t option ref = ref None
let pending_body : Request_descriptor.t option ref = ref None
let pending_body_failed = ref false
let stream_closed = ref false

(* A chunked response of many small buffers, each its own iovec. *)
let stream_piece = String.make 64 'x'
let stream_pieces = 1024

let request_handler (_ : Unix.sockaddr) reqd =
  let request = Request_descriptor.request reqd in
  let body = Request_descriptor.request_body reqd in
  match request.meth, request.target with
  | `GET, "/ws-echo" ->
    let rec echo ws =
      match Ache_miou.Websocket.receive ws with
      | Some (Text message) ->
        Ache_miou.Websocket.send_text ws message;
        echo ws
      | Some (Binary _) | None -> ()
    in
    (match Ache_miou.Websocket.upgrade reqd echo with
    | Ok () -> ()
    | Error _ -> Alcotest.fail "handshake rejected")
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
    and on_error () = Body.Writer.close response_body in
    Body.Reader.schedule_read body ~on_eof ~on_error ~on_read
  | `POST, "/partial" ->
    let rec on_read _ ~off:_ ~len:_ =
      Body.Reader.schedule_read body ~on_eof ~on_error ~on_read
    and on_eof () = Alcotest.fail "a cut body completed"
    and on_error () = partial_body_cut := true in
    Body.Reader.schedule_read body ~on_eof ~on_error ~on_read
  | `GET, "/upgrade" ->
    Body.Reader.close body;
    Request_descriptor.respond_with_upgrade
      reqd
      (Headers.of_list [ "connection", "upgrade"; "upgrade", "echo" ])
      (Runtime.make (module Echo) (Echo.create (fun t s -> Echo.send t s)))
  | `GET, "/upgrade-later" -> pending_upgrade := Some reqd
  | `POST, "/respond-later" ->
    pending_body := Some reqd;
    let rec on_read _ ~off:_ ~len:_ =
      Body.Reader.schedule_read body ~on_eof ~on_error ~on_read
    and on_eof () = ()
    and on_error () = pending_body_failed := true in
    Body.Reader.schedule_read body ~on_eof ~on_error ~on_read
  | `GET, "/raise" -> failwith "handler failed"
  | `GET, "/stream" ->
    Body.Reader.close body;
    let response_body =
      Request_descriptor.respond_with_streaming
        reqd
        (Response.create
           ~headers:(Headers.of_list [ "transfer-encoding", "chunked" ])
           `OK)
    in
    let piece =
      Ache.Bigstring.of_string
        ~off:0
        ~len:(String.length stream_piece)
        stream_piece
    in
    for _ = 1 to stream_pieces do
      Body.Writer.schedule_bigstring response_body piece
    done;
    Body.Writer.close response_body
  | `GET, "/stream-forever" ->
    Body.Reader.close body;
    let response_body =
      Request_descriptor.respond_with_streaming
        reqd
        (Response.create
           ~headers:(Headers.of_list [ "transfer-encoding", "chunked" ])
           `OK)
    in
    let chunk = String.make 0x10000 'x' in
    let rec write n =
      if n = 0
      then Body.Writer.close response_body
      else (
        Body.Writer.write_string response_body chunk;
        Body.Writer.flush response_body (function
          | `Written -> write (n - 1)
          | `Closed -> stream_closed := true))
    in
    write 1024
  | _, target ->
    Body.Reader.close body;
    Request_descriptor.respond_with_string reqd (Response.create `OK) target

let error_handler (_ : Unix.sockaddr) ?request:_ error start_response =
  server_errors := error :: !server_errors;
  Body.Writer.close (start_response Headers.empty)

let resolve computation value =
  ignore (Miou.Computation.try_return computation value : bool)

let await computation =
  match Miou.Computation.await computation with
  | Ok value -> value
  | Error (exn, bt) -> Printexc.raise_with_backtrace exn bt

let open_fds () = Array.length (Sys.readdir "/dev/fd")
let handler_errors : exn list ref = ref []

let with_timeout seconds f =
  let body = Miou.async f
  and timer =
    Miou.async (fun () ->
      Miou_unix.sleep seconds;
      Alcotest.fail "test timed out")
  in
  match Miou.await_first [ body; timer ] with
  | Ok value -> value
  | Error exn -> raise exn

let handlers_returned = ref 0

let with_listener handler f =
  handler_errors := [];
  handlers_returned := 0;
  let fds = open_fds () in
  Miou_unix.run ~domains:0 (fun () ->
    let listening = Miou_unix.tcpv4 () in
    Miou_unix.bind_and_listen
      ~backlog:16
      listening
      (Unix.ADDR_INET (Unix.inet_addr_loopback, 0));
    let addr = Unix.getsockname (Miou_unix.to_file_descr listening) in
    let connected = ref 0
    and handlers_done = ref 0 in
    let server =
      Miou.async (fun () ->
        let orphans = Miou.orphans () in
        let rec accept () =
          let socket, peer = Miou_unix.accept listening in
          ignore
            (Miou.async ~orphans (fun () ->
               (try handler socket peer with
               | Miou.Cancelled as exn -> raise exn
               | exn -> handler_errors := exn :: !handler_errors);
               Miou_unix.close socket;
               incr handlers_done;
               incr handlers_returned)
             : unit Miou.t);
          accept ()
        in
        accept ())
    in
    let connect () =
      incr connected;
      let socket = Miou_unix.tcpv4 () in
      Miou_unix.connect socket addr;
      socket
    in
    Fun.protect
      ~finally:(fun () ->
        Miou.cancel server;
        Miou_unix.close listening)
      (fun () ->
         with_timeout 10.0 (fun () ->
           f connect;
           let rec wait n =
             if !handlers_done < !connected && n > 0
             then (
               Miou_unix.sleep 0.01;
               wait (n - 1))
           in
           wait 500;
           Alcotest.(check int)
             "connection handlers returned"
             !connected
             !handlers_done)));
  Alcotest.(check int) "no file descriptor leaked" fds (open_fds ())

(* Timeouts that a test does not give are off, not at their defaults. *)
let with_server
      ?idle_timeout
      ?head_timeout
      ?body_timeout
      ?write_timeout
      ?stop
      ?grace
      f
  =
  server_errors := [];
  partial_body_cut := false;
  with_listener
    (Ache_miou_tls.Server.create_connection_handler_with_fd
       ~date:false
       ~idle_timeout
       ~head_timeout
       ~body_timeout
       ~write_timeout
       ?stop
       ?grace
       ~tls:server_config
       ~request_handler
       ~error_handler)
    f

let check_no_handler_error () =
  Alcotest.(check (list string))
    "no exception from the connection handler"
    []
    (List.map Printexc.to_string !handler_errors)

type raw =
  { fd : Miou_unix.file_descr
  ; mutable state : Tls.Engine.state
  ; mutable received : string
  ; mutable ending : [ `Tcp_eof | `Close_notify ] option
  }

let raw_pull raw =
  let buf = Bytes.create 0x4000 in
  match Miou_unix.read raw.fd buf with
  | 0 | (exception Unix.Unix_error (ECONNRESET, _, _)) ->
    raw.ending <- Some `Tcp_eof
  | n ->
    (match Tls.Engine.handle_tls raw.state (Bytes.sub_string buf 0 n) with
    | Ok (state, eof, `Response response, `Data data) ->
      raw.state <- state;
      Option.iter (fun data -> raw.received <- raw.received ^ data) data;
      Option.iter (Miou_unix.write raw.fd) response;
      if Option.is_some eof then raw.ending <- Some `Close_notify
    | Error (failure, _) ->
      Alcotest.failf "raw TLS peer: %s" (Tls.Engine.string_of_failure failure))

let raw_handshake fd state =
  let raw = { fd; state; received = ""; ending = None } in
  while Tls.Engine.handshake_in_progress raw.state do
    if Option.is_some raw.ending then Alcotest.fail "raw handshake cut";
    raw_pull raw
  done;
  raw

let raw_client fd =
  let state, init =
    Tls.Engine.client (Tls.Config.peer client_config localhost)
  in
  Miou_unix.write fd init;
  raw_handshake fd state

let raw_server fd = raw_handshake fd (Tls.Engine.server server_config)

let raw_write raw s =
  match Tls.Engine.send_application_data raw.state [ s ] with
  | Some (state, data) ->
    raw.state <- state;
    Miou_unix.write raw.fd data
  | None -> Alcotest.fail "raw TLS peer not ready"

let raw_close_notify raw =
  let state, data = Tls.Engine.send_close_notify raw.state in
  raw.state <- state;
  Miou_unix.write raw.fd data

let raw_take raw n =
  while String.length raw.received < n && Option.is_none raw.ending do
    raw_pull raw
  done;
  let n = min n (String.length raw.received) in
  let taken = String.sub raw.received 0 n in
  raw.received <- String.sub raw.received n (String.length raw.received - n);
  taken

let raw_read_until_eof raw =
  while Option.is_none raw.ending do
    raw_pull raw
  done;
  raw_take raw (String.length raw.received), Option.get raw.ending

let head_end s =
  let rec go i =
    if i + 4 > String.length s
    then None
    else if String.sub s i 4 = "\r\n\r\n"
    then Some (i + 4)
    else go (i + 1)
  in
  go 0

let raw_read_head raw =
  let rec find () =
    match head_end raw.received with
    | Some n -> n
    | None ->
      if Option.is_some raw.ending then Alcotest.fail "no request head";
      raw_pull raw;
      find ()
  in
  ignore (raw_take raw (find ()) : string)

let read_body body =
  let buf = Stdlib.Buffer.create 1024 in
  let result = Miou.Computation.create () in
  let rec on_read bs ~off ~len =
    Stdlib.Buffer.add_string buf (Ache.Bigstring.substring bs ~off ~len);
    Body.Reader.schedule_read body ~on_eof ~on_error ~on_read
  and on_eof () = resolve result (Ok (Stdlib.Buffer.contents buf))
  and on_error () = resolve result (Error (Stdlib.Buffer.contents buf)) in
  Body.Reader.schedule_read body ~on_eof ~on_error ~on_read;
  result

let request ?(errors = ref []) client ?(body = "") request =
  let result = Miou.Computation.create () in
  let request_body =
    Ache_miou_tls.Client.request
      client
      request
      ~error_handler:(fun error -> errors := error :: !errors)
      ~response_handler:(fun response response_body ->
        resolve result (response, read_body response_body))
  in
  Body.Writer.write_string request_body body;
  Body.Writer.close request_body;
  let response, body = await result in
  response, await body

let get target =
  Request.create ~headers:(Headers.of_list [ "host", "localhost" ]) `GET target

let with_client connect f =
  let fd = connect () in
  Fun.protect
    ~finally:(fun () -> Miou_unix.close fd)
    (fun () ->
       let client =
         Ache_miou_tls.Client.create_connection_with_fd
           ~tls:client_config
           ~peer:(`Host localhost)
           fd
       in
       f client;
       Ache_miou_tls.Client.shutdown client)

let with_raw_client connect f =
  let fd = connect () in
  Fun.protect ~finally:(fun () -> Miou_unix.close fd) (fun () -> f fd)

(* A TLS client that decrypts the bytes from the server one record at a time, to
   count the records that carry response bytes. *)
module Records = struct
  type t =
    { mutable state : Tls.Engine.state
    ; mutable pending : string
    ; plain : Stdlib.Buffer.t
    ; mutable data_records : int
    ; mutable closed : bool
    }

  let create state =
    { state
    ; pending = ""
    ; plain = Stdlib.Buffer.create 4096
    ; data_records = 0
    ; closed = false
    }

  let rec take t replies =
    let available = String.length t.pending in
    let size =
      if available < 5
      then max_int
      else 5 + ((Char.code t.pending.[3] lsl 8) lor Char.code t.pending.[4])
    in
    if available < size
    then List.rev replies
    else
      let record = String.sub t.pending 0 size in
      t.pending <- String.sub t.pending size (available - size);
      match Tls.Engine.handle_tls t.state record with
      | Ok (state, eof, `Response reply, `Data data) ->
        t.state <- state;
        if Option.is_some eof then t.closed <- true;
        (match data with
        | Some data when data <> "" ->
          t.data_records <- t.data_records + 1;
          Stdlib.Buffer.add_string t.plain data
        | _ -> ());
        take t (Option.fold ~none:replies ~some:(fun r -> r :: replies) reply)
      | Error (failure, _) ->
        Alcotest.failf "TLS client: %s" (Tls.Engine.string_of_failure failure)

  (* The replies to send back. *)
  let feed t bytes =
    t.pending <- t.pending ^ bytes;
    take t []

  let send t s =
    match Tls.Engine.send_application_data t.state [ s ] with
    | Some (state, data) ->
      t.state <- state;
      data
    | None -> Alcotest.fail "TLS client not ready"
end

let stream_request =
  "GET /stream HTTP/1.1\r\nhost: localhost\r\nconnection: close\r\n\r\n"

let check_stream_records (records : Records.t) =
  let plain = Stdlib.Buffer.contents records.plain in
  let body =
    String.concat "" (List.init stream_pieces (fun _ -> stream_piece))
  in
  Alcotest.(check bool)
    "chunked body"
    true
    (String.ends_with
       ~suffix:
         (Printf.sprintf "%x\r\n%s\r\n0\r\n\r\n" (String.length body) body)
       plain);
  (* One record for each 16 KiB, and one more in case the head goes in a write
     of its own. Without coalescing it is one for each iovec. *)
  let limit = ((String.length plain + 0x3fff) / 0x4000) + 1 in
  if records.data_records > limit
  then
    Alcotest.failf
      "%d application-data records for %d bytes, expected at most %d"
      records.data_records
      (String.length plain)
      limit

let test_stream_records () =
  with_server (fun connect ->
    let fd = connect () in
    Fun.protect
      ~finally:(fun () -> Miou_unix.close fd)
      (fun () ->
         let state, init =
           Tls.Engine.client (Tls.Config.peer client_config localhost)
         in
         let records = Records.create state in
         Miou_unix.write fd init;
         let buf = Bytes.create 0x4000 in
         let rec pump ~reply until =
           if not (until ())
           then
             match Miou_unix.read fd buf with
             | 0 -> ()
             | n ->
               let replies = Records.feed records (Bytes.sub_string buf 0 n) in
               if reply then List.iter (Miou_unix.write fd) replies;
               pump ~reply until
         in
         pump ~reply:true (fun () ->
           not (Tls.Engine.handshake_in_progress records.state));
         Miou_unix.write fd (Records.send records stream_request);
         pump ~reply:false (fun () -> records.closed);
         check_stream_records records));
  check_no_handler_error ()

let body_result = Alcotest.(result string string)

let test_get () =
  with_server (fun connect ->
    with_client connect (fun client ->
      let response, body = request client (get "/hello") in
      Alcotest.(check int) "status" 200 (Status.to_code response.status);
      Alcotest.check body_result "body" (Ok "/hello") body));
  check_no_handler_error ()

let test_epoch () =
  with_server (fun connect ->
    with_raw_client connect (fun fd ->
      let socket =
        Ache_miou_tls.client_of_fd client_config ~peer:(`Host localhost) fd
      in
      (match Ache_miou_tls.epoch socket with
      | Ok epoch ->
        Alcotest.(check bool)
          "server certificate"
          true
          (Option.is_some epoch.peer_certificate)
      | Error () -> Alcotest.fail "no epoch");
      let client = Ache_miou_tls.Client.create_connection socket in
      let _, body = request client (get "/epoch") in
      Alcotest.check body_result "body" (Ok "/epoch") body;
      Ache_miou_tls.Client.shutdown client));
  check_no_handler_error ()

let test_post_echo () =
  with_server (fun connect ->
    with_client connect (fun client ->
      let payload = String.init 200_000 (fun i -> Char.chr (97 + (i mod 26))) in
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
      Alcotest.check body_result "body" (Ok payload) body))

let test_client_pipelined () =
  with_server (fun connect ->
    with_client connect (fun client ->
      let a = Miou.async (fun () -> request client (get "/a")) in
      let b = Miou.async (fun () -> request client (get "/b")) in
      let (_, a), (_, b) = Miou.await_exn a, Miou.await_exn b in
      Alcotest.check body_result "first" (Ok "/a") a;
      Alcotest.check body_result "second" (Ok "/b") b))

let test_pipelined_connection_close () =
  with_server (fun connect ->
    with_raw_client connect (fun fd ->
      let raw = raw_client fd in
      raw_write
        raw
        "GET /a HTTP/1.1\r\n\
         host: localhost\r\n\
         \r\n\
         GET /b HTTP/1.1\r\n\
         host: localhost\r\n\
         connection: close\r\n\
         \r\n";
      let output, ending = raw_read_until_eof raw in
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
      Alcotest.(check bool)
        "server sent close_notify"
        true
        (ending = `Close_notify);
      raw_close_notify raw));
  check_no_handler_error ()

let test_client_sends_close_notify () =
  let server_saw = ref None in
  with_listener
    (fun fd _ ->
       let raw = raw_server fd in
       raw_read_head raw;
       raw_write raw "HTTP/1.1 200 OK\r\ncontent-length: 2\r\n\r\nok";
       server_saw := Some (raw_read_until_eof raw))
    (fun connect ->
       with_client connect (fun client ->
         let _, body = request client (get "/") in
         Alcotest.check body_result "body" (Ok "ok") body));
  Alcotest.(check bool)
    "client sent close_notify and nothing else"
    true
    (!server_saw = Some ("", `Close_notify))

let test_cut_request_body () =
  with_server (fun connect ->
    with_raw_client connect (fun fd ->
      let raw = raw_client fd in
      raw_write
        raw
        "POST /partial HTTP/1.1\r\n\
         host: localhost\r\n\
         content-length: 1000\r\n\
         \r\n\
         0123456789";
      Miou_unix.sleep 0.05;
      Unix.shutdown (Miou_unix.to_file_descr fd) SHUTDOWN_SEND));
  Alcotest.(check bool) "body reported cut" true !partial_body_cut;
  check_no_handler_error ()

let close_delimited = "HTTP/1.1 200 OK\r\nconnection: close\r\n\r\npartial"

let response_then_close ~response ~close_notify () =
  let errors = ref [] in
  let result = ref (Error "no response") in
  with_listener
    (fun fd _ ->
       let raw = raw_server fd in
       raw_read_head raw;
       if response <> "" then raw_write raw response;
       if close_notify then raw_close_notify raw)
    (fun connect ->
       with_client connect (fun client ->
         let finished = Miou.Computation.create () in
         let request_body =
           Ache_miou_tls.Client.request
             client
             (get "/")
             ~error_handler:(fun error ->
               errors := error :: !errors;
               resolve finished ())
             ~response_handler:(fun _ body ->
               let rec on_read bs ~off ~len =
                 (result :=
                    match !result with
                    | Ok s | Error s ->
                      Error (s ^ Ache.Bigstring.substring bs ~off ~len));
                 Body.Reader.schedule_read body ~on_eof ~on_error ~on_read
               and on_eof () =
                 (result := match !result with Ok s | Error s -> Ok s);
                 resolve finished ()
               and on_error () = resolve finished () in
               result := Error "";
               Body.Reader.schedule_read body ~on_eof ~on_error ~on_read)
         in
         Body.Writer.close request_body;
         await finished));
  !result, !errors

let test_truncated_response () =
  let body, errors =
    response_then_close ~response:close_delimited ~close_notify:false ()
  in
  Alcotest.check body_result "body cut" (Error "partial") body;
  Alcotest.(check bool)
    "error handler got Truncated"
    true
    (match errors with [ `Exn Ache_miou_tls.Truncated ] -> true | _ -> false)

let test_close_notify_response () =
  let body, errors =
    response_then_close ~response:close_delimited ~close_notify:true ()
  in
  Alcotest.check body_result "body complete" (Ok "partial") body;
  Alcotest.(check int) "no error" 0 (List.length errors)

let test_fixed_length_without_close_notify () =
  let body, errors =
    response_then_close
      ~response:"HTTP/1.1 200 OK\r\ncontent-length: 8\r\n\r\ncomplete"
      ~close_notify:false
      ()
  in
  Alcotest.check body_result "body complete" (Ok "complete") body;
  Alcotest.(check int) "no error" 0 (List.length errors)

let test_chunked_without_close_notify () =
  let body, errors =
    response_then_close
      ~response:
        "HTTP/1.1 200 OK\r\n\
         transfer-encoding: chunked\r\n\
         \r\n\
         8\r\n\
         complete\r\n\
         0\r\n\
         \r\n"
      ~close_notify:false
      ()
  in
  Alcotest.check body_result "body complete" (Ok "complete") body;
  Alcotest.(check int) "no error" 0 (List.length errors)

let test_no_response_without_close_notify () =
  let body, errors = response_then_close ~response:"" ~close_notify:false () in
  Alcotest.check body_result "no response" (Error "no response") body;
  Alcotest.(check bool)
    "Connection_closed"
    true
    (match errors with [ `Connection_closed ] -> true | _ -> false)

let test_timeout_check_timing () =
  let raises f =
    match f () with () -> false | exception Invalid_argument _ -> true
  in
  Miou_unix.run ~domains:0 (fun () ->
    let fd = Miou_unix.tcpv4 () in
    let addr = Unix.ADDR_INET (Unix.inet_addr_loopback, 0) in
    let bound =
      Ache_miou_tls.Server.create_connection_handler_with_fd
        ~date:false
        ~head_timeout:(Some (-1.))
        ~tls:server_config
        ~request_handler
        ~error_handler
    in
    Alcotest.(check bool)
      "bound: raises on a file descriptor"
      true
      (raises (fun () -> bound fd addr));
    Alcotest.(check bool)
      "every optional argument given: raises at once"
      true
      (raises (fun () ->
         ignore
           (Ache_miou_tls.Server.create_connection_handler_with_fd
              ~date:false
              ~config:Ache.Config.default
              ~idle_timeout:(Some 1.)
              ~head_timeout:(Some (-1.))
              ~close_timeout:(Some 1.)
              ~body_timeout:(Some 1.)
              ~write_timeout:(Some 1.)
              ~stop:(Miou.Computation.create ())
              ~grace:1.
              ~tls:server_config
              ~request_handler
              ~error_handler
            : _ -> _ -> unit)));
    Miou_unix.close fd)

let test_untrusted_certificate () =
  let other_ca = Ache_test_certs.generate () in
  with_server (fun connect ->
    with_raw_client connect (fun fd ->
      match
        Ache_miou_tls.Client.create_connection_with_fd
          ~tls:(Ache_test_certs.client_config other_ca)
          ~peer:(`Host localhost)
          fd
      with
      | _ -> Alcotest.fail "handshake succeeded"
      | exception Tls_miou_unix.Tls_failure (`Error (`AuthenticationFailure _))
        ->
        ()));
  check_no_handler_error ()

let send_plain_request fd =
  Miou_unix.write fd "GET / HTTP/1.1\r\nhost: localhost\r\n\r\n"

let drain fd =
  let chunk = Bytes.create 4096 in
  let rec go () =
    match Miou_unix.read fd chunk with
    | 0 | (exception Unix.Unix_error _) -> ()
    | _ -> go ()
  in
  go ()

let test_plain_text_client_closes () =
  with_server (fun connect ->
    with_raw_client connect (fun fd ->
      send_plain_request fd;
      Unix.shutdown (Miou_unix.to_file_descr fd) SHUTDOWN_SEND));
  check_no_handler_error ()

let check_bounded name ~max since =
  let elapsed = Unix.gettimeofday () -. since in
  if elapsed > max then Alcotest.failf "%s: closed after %.3f s" name elapsed

let test_plain_text_client_waits () =
  with_server ~idle_timeout:0.2 (fun connect ->
    with_raw_client connect (fun fd ->
      send_plain_request fd;
      let since = Unix.gettimeofday () in
      drain fd;
      check_bounded "plain text" ~max:2.0 since))

let stalled_handshake name ?idle_timeout ?head_timeout () =
  with_server ?idle_timeout ?head_timeout (fun connect ->
    with_raw_client connect (fun fd ->
      let since = Unix.gettimeofday () in
      Alcotest.(check int)
        "server closed"
        0
        (Miou_unix.read fd (Bytes.create 16));
      check_bounded name ~max:1.0 since))

let test_handshake_timeout () =
  stalled_handshake "idle_timeout" ~idle_timeout:0.2 ()

let test_handshake_head_timeout () =
  stalled_handshake "head_timeout" ~head_timeout:0.2 ()

let test_handshake_smaller_timeout () =
  stalled_handshake
    "head_timeout smaller"
    ~idle_timeout:5.0
    ~head_timeout:0.2
    ();
  stalled_handshake
    "idle_timeout smaller"
    ~idle_timeout:0.2
    ~head_timeout:5.0
    ()

let test_handshake_no_timeout () =
  with_server (fun connect ->
    with_raw_client connect (fun fd ->
      let state =
        Miou.await_first
          [ Miou.async (fun () ->
              ignore (Miou_unix.read fd (Bytes.create 16) : int);
              `Closed)
          ; Miou.async (fun () ->
              Miou_unix.sleep 0.5;
              `Open)
          ]
      in
      Alcotest.(check bool) "still open after 0.5 s" true (state = Ok `Open);
      Unix.shutdown (Miou_unix.to_file_descr fd) SHUTDOWN_SEND))

let handshake_with peer =
  let result = ref None in
  with_server (fun connect ->
    with_raw_client connect (fun fd ->
      match
        Ache_miou_tls.Client.create_connection_with_fd
          ~tls:client_config
          ~peer
          fd
      with
      | client ->
        result := Some (Ok ());
        Ache_miou_tls.Client.shutdown client
      | exception exn -> result := Some (Error exn)));
  Option.get !result

let check_accepted name peer =
  match handshake_with peer with
  | Ok () -> ()
  | Error exn ->
    Alcotest.failf "%s: rejected with %s" name (Printexc.to_string exn)

let check_rejected name peer =
  match handshake_with peer with
  | Error (Tls_miou_unix.Tls_failure (`Error (`AuthenticationFailure _))) -> ()
  | Error exn ->
    Alcotest.failf "%s: unexpected %s" name (Printexc.to_string exn)
  | Ok () -> Alcotest.failf "%s: handshake accepted" name

let test_peer_host () =
  check_accepted "localhost" (`Host localhost);
  check_rejected
    "example.com"
    (`Host Domain_name.(host_exn (of_string_exn "example.com")))

let test_peer_ip () =
  check_accepted "127.0.0.1" (`Ip (Ipaddr.of_string_exn "127.0.0.1"));
  check_rejected "127.0.0.2" (`Ip (Ipaddr.of_string_exn "127.0.0.2"))

let test_peer_insecure () = check_accepted "no name" `Insecure_no_name_check

let test_idle_timeout () =
  with_server ~idle_timeout:0.2 (fun connect ->
    with_raw_client connect (fun fd ->
      let raw = raw_client fd in
      let output, ending = raw_read_until_eof raw in
      Alcotest.(check string) "no response" "" output;
      Alcotest.(check bool) "close_notify" true (ending = `Close_notify);
      raw_close_notify raw));
  check_no_handler_error ()

let test_head_timeout () =
  with_server ~head_timeout:0.2 (fun connect ->
    with_raw_client connect (fun fd ->
      let raw = raw_client fd in
      raw_write raw "GET /a HTTP/1.1\r\n";
      let output, ending = raw_read_until_eof raw in
      Alcotest.(check bool)
        "408"
        true
        (String.starts_with ~prefix:"HTTP/1.1 408 Request Timeout\r\n" output);
      Alcotest.(check bool) "close_notify" true (ending = `Close_notify);
      raw_close_notify raw));
  Alcotest.(check int) "one error" 1 (List.length !server_errors);
  check_no_handler_error ()

let test_head_timeout_then_data () =
  with_server ~head_timeout:0.2 (fun connect ->
    with_raw_client connect (fun fd ->
      let raw = raw_client fd in
      raw_write raw "GET /a HTTP/1.1\r\n";
      let status = "HTTP/1.1 408 Request Timeout\r\n" in
      Alcotest.(check string)
        "status line"
        status
        (raw_take raw (String.length status));
      raw_write raw "host: localhost\r\n\r\n";
      raw_close_notify raw;
      let _, ending = raw_read_until_eof raw in
      Alcotest.(check bool) "close_notify" true (ending = `Close_notify)));
  Alcotest.(check int) "one error" 1 (List.length !server_errors);
  check_no_handler_error ()

let respond_while_body_read_waits ?idle_timeout () =
  pending_body := None;
  pending_body_failed := false;
  with_server ?idle_timeout ~body_timeout:0.2 (fun connect ->
    with_raw_client connect (fun fd ->
      let raw = raw_client fd in
      raw_write
        raw
        "POST /respond-later HTTP/1.1\r\n\
         host: localhost\r\n\
         content-length: 100000\r\n\
         \r\n\
         abc";
      let rec respond () =
        match !pending_body with
        | Some reqd ->
          Miou_unix.sleep 0.05;
          Request_descriptor.respond_with_string
            reqd
            (Response.create `OK)
            "late"
        | None ->
          Miou_unix.sleep 0.001;
          respond ()
      in
      respond ();
      let output, ending = raw_read_until_eof raw in
      Alcotest.(check string)
        "response"
        "HTTP/1.1 200 OK\r\ncontent-length: 4\r\n\r\nlate"
        output;
      Alcotest.(check bool) "close_notify" true (ending = `Close_notify);
      let rec wait n =
        if !handlers_returned = 0 && n > 0
        then (
          Miou_unix.sleep 0.01;
          wait (n - 1))
      in
      wait 200;
      Alcotest.(check int)
        "handler returns before the client closes"
        1
        !handlers_returned));
  Alcotest.(check bool) "request body failed" true !pending_body_failed;
  check_no_handler_error ()

let test_handler_exception () =
  with_server (fun connect ->
    with_client connect (fun client ->
      let response, _ = request client (get "/raise") in
      Alcotest.(check int) "status" 500 (Status.to_code response.status)));
  check_no_handler_error ()

let test_reset_mid_head () =
  with_server (fun connect ->
    let fd = connect () in
    let raw = raw_client fd in
    raw_write raw "GET /a HTTP/1.1\r\nHo";
    Unix.setsockopt_optint (Miou_unix.to_file_descr fd) SO_LINGER (Some 0);
    Miou_unix.close fd);
  check_no_handler_error ()

let upgrade_headers =
  Headers.of_list
    [ "host", "localhost"
    ; "connection", "upgrade"
    ; "upgrade", "echo"
    ; "content-length", "0"
    ]

let test_upgrade () =
  with_server (fun connect ->
    with_client connect (fun client ->
      let upgraded = Miou.Computation.create () in
      let request_body =
        Ache_miou_tls.Client.request
          client
          (Request.create ~headers:upgrade_headers `GET "/upgrade")
          ~error_handler:(fun _ -> Alcotest.fail "request failed")
          ~response_handler:(fun response _body -> resolve upgraded response)
      in
      Body.Writer.close request_body;
      let response = await upgraded in
      Alcotest.(check int) "status" 101 (Status.to_code response.status);
      let message = "hello over echo" in
      let echoed = Miou.Computation.create () in
      let received = Stdlib.Buffer.create 16 in
      let echo_client =
        Echo.create ~pending:message (fun t s ->
          Stdlib.Buffer.add_string received s;
          if Stdlib.Buffer.length received >= String.length message
          then (
            Echo.shutdown t;
            resolve echoed (Stdlib.Buffer.contents received)))
      in
      Ache_miou_tls.Client.upgrade
        client
        (Runtime.make (module Echo) echo_client);
      Alcotest.(check string) "echoed" message (await echoed)));
  check_no_handler_error ()

let upgrade_response =
  "HTTP/1.1 101 Switching Protocols\r\n\
   connection: upgrade\r\n\
   upgrade: echo\r\n\
   \r\n\
   ping"

let test_upgrade_bytes_after_head () =
  with_server (fun connect ->
    with_raw_client connect (fun fd ->
      let raw = raw_client fd in
      raw_write
        raw
        "GET /upgrade HTTP/1.1\r\n\
         host: localhost\r\n\
         connection: upgrade\r\n\
         upgrade: echo\r\n\
         \r\n\
         ping";
      Alcotest.(check string)
        "101 then echoed bytes"
        upgrade_response
        (raw_take raw (String.length upgrade_response));
      raw_write raw "pong";
      Alcotest.(check string) "later bytes echoed" "pong" (raw_take raw 4);
      raw_close_notify raw));
  check_no_handler_error ()

let test_upgrade_with_read_pending () =
  with_server (fun connect ->
    with_raw_client connect (fun fd ->
      let raw = raw_client fd in
      raw_write
        raw
        "GET /upgrade-later HTTP/1.1\r\n\
         host: localhost\r\n\
         connection: upgrade\r\n\
         upgrade: echo\r\n\
         \r\n";
      Miou_unix.sleep 0.05;
      raw_write raw "ping";
      Miou_unix.sleep 0.05;
      Request_descriptor.respond_with_upgrade
        (Option.get !pending_upgrade)
        (Headers.of_list [ "connection", "upgrade"; "upgrade", "echo" ])
        (Runtime.make (module Echo) (Echo.create (fun t s -> Echo.send t s)));
      Alcotest.(check string)
        "101 then the bytes sent before it"
        upgrade_response
        (raw_take raw (String.length upgrade_response));
      raw_close_notify raw));
  check_no_handler_error ()

let test_idle_timeout_then_request () =
  with_server ~idle_timeout:0.2 (fun connect ->
    with_raw_client connect (fun fd ->
      let raw = raw_client fd in
      Miou_unix.sleep 0.3;
      raw_write raw "GET /a HTTP/1.1\r\nhost: localhost\r\n\r\n";
      let output, ending = raw_read_until_eof raw in
      Alcotest.(check string) "no response" "" output;
      Alcotest.(check bool) "close_notify" true (ending = `Close_notify);
      (* The idle close does not linger (decision 0023): the socket may be
         closed, and our request reset it. *)
      try raw_close_notify raw with
      | Unix.Unix_error ((EPIPE | ECONNRESET), _, _) -> ()));
  Alcotest.(check int) "no error" 0 (List.length !server_errors);
  check_no_handler_error ()

let test_client_shutdown_with_pending_read () =
  let server_saw = ref None in
  let errors = ref [] in
  with_listener
    (fun fd _ ->
       let raw = raw_server fd in
       raw_read_head raw;
       raw_write raw "HTTP/1.1 200 OK\r\ncontent-le";
       server_saw := Some (raw_read_until_eof raw))
    (fun connect ->
       with_client connect (fun client ->
         let request_body =
           Ache_miou_tls.Client.request
             client
             (get "/")
             ~error_handler:(fun error -> errors := error :: !errors)
             ~response_handler:(fun _ _ -> Alcotest.fail "unexpected response")
         in
         Body.Writer.close request_body;
         Miou_unix.sleep 0.1));
  Alcotest.(check bool)
    "client sent close_notify"
    true
    (!server_saw = Some ("", `Close_notify));
  Alcotest.(check int) "one error" 1 (List.length !errors);
  check_no_handler_error ()

let live_words () =
  Gc.full_major ();
  (Gc.stat ()).live_words

let check_handshake_timers_bounded ~connections exchange =
  let growth = ref 0 in
  Miou_unix.run ~domains:0 (fun () ->
    let listening = Miou_unix.tcpv4 () in
    Miou_unix.bind_and_listen
      ~backlog:16
      listening
      (Unix.ADDR_INET (Unix.inet_addr_loopback, 0));
    let addr = Unix.getsockname (Miou_unix.to_file_descr listening) in
    let closed = ref 0 in
    let handler =
      Ache_miou_tls.Server.create_connection_handler_with_fd
        ~date:false
        ~idle_timeout:(Some 30.)
        ~head_timeout:(Some 30.)
        ~body_timeout:None
        ~write_timeout:None
        ~tls:server_config
        ~request_handler
        ~error_handler
    in
    let server =
      Miou.async (fun () ->
        let orphans = Miou.orphans () in
        let rec reap () =
          match Miou.care orphans with
          | Some (Some task) ->
            let (_ : (unit, exn) result) = Miou.await task in
            reap ()
          | Some None | None -> ()
        in
        let rec accept () =
          reap ();
          let socket, peer = Miou_unix.accept listening in
          ignore
            (Miou.async ~orphans (fun () ->
               Fun.protect
                 ~finally:(fun () ->
                   Miou_unix.close socket;
                   incr closed)
                 (fun () -> handler socket peer))
             : unit Miou.t);
          accept ()
        in
        accept ())
    in
    Fun.protect
      ~finally:(fun () ->
        Miou.cancel server;
        Miou_unix.close listening)
      (fun () ->
         with_timeout 10.0 (fun () ->
           let rec short_live_sleepers () =
             Miou_unix.sleep 0.01;
             short_live_sleepers ()
           in
           let live = Miou.async short_live_sleepers in
           let opened = ref 0 in
           let rec all_closed () =
             if !closed < !opened
             then (
               Miou_unix.sleep 0.001;
               all_closed ())
           in
           let exchanges n =
             for _ = 1 to n do
               let fd = Miou_unix.tcpv4 () in
               Miou_unix.connect fd addr;
               incr opened;
               Fun.protect
                 ~finally:(fun () -> Miou_unix.close fd)
                 (fun () -> exchange fd)
             done;
             all_closed ()
           in
           exchanges (connections / 10);
           let before = live_words () in
           exchanges connections;
           Miou_unix.sleep 0.6;
           growth := live_words () - before;
           Miou.cancel live)));
  if !growth > connections
  then
    Alcotest.failf
      "heap grew by %d words over %d connections"
      !growth
      connections

let test_handshake_timers_bounded () =
  check_handshake_timers_bounded ~connections:300 (fun fd ->
    let raw = raw_client fd in
    raw_close_notify raw)

let test_cut_handshake_timers_bounded () =
  check_handshake_timers_bounded
    ~connections:1000
    (fun (_ : Miou_unix.file_descr) -> Miou_unix.sleep 0.001)

let test_websocket_echo () =
  with_server (fun connect ->
    with_raw_client connect (fun fd ->
      let raw = raw_client fd in
      raw_write
        raw
        "GET /ws-echo HTTP/1.1\r\n\
         host: localhost\r\n\
         upgrade: websocket\r\n\
         connection: Upgrade\r\n\
         sec-websocket-key: dGhlIHNhbXBsZSBub25jZQ==\r\n\
         sec-websocket-version: 13\r\n\
         \r\n";
      raw_read_head raw;
      raw_write raw "\x81\x82\x00\x00\x00\x00hi";
      Alcotest.(check string) "echo" "\x81\x02hi" (raw_take raw 4);
      raw_write raw "\x88\x82\x00\x00\x00\x00\x03\xe8";
      Alcotest.(check string) "close echoed" "\x88\x02\x03\xe8" (raw_take raw 4);
      let rest, (_ : [ `Tcp_eof | `Close_notify ]) = raw_read_until_eof raw in
      Alcotest.(check string) "then the end" "" rest));
  check_no_handler_error ()

let hello_response = "HTTP/1.1 200 OK\r\ncontent-length: 6\r\n\r\n/hello"

let test_stop_idle_connection () =
  let stop = Miou.Computation.create () in
  with_server ~stop ~grace:2.0 (fun connect ->
    with_raw_client connect (fun fd ->
      let raw = raw_client fd in
      raw_write raw "GET /hello HTTP/1.1\r\nhost: localhost\r\n\r\n";
      Alcotest.(check string)
        "response"
        hello_response
        (raw_take raw (String.length hello_response));
      let since = Unix.gettimeofday () in
      resolve stop ();
      let rest, ending = raw_read_until_eof raw in
      Alcotest.(check string) "nothing more" "" rest;
      Alcotest.(check bool) "close_notify" true (ending = `Close_notify);
      check_bounded "idle TLS connection" ~max:0.5 since));
  check_no_handler_error ()

let test_stop_during_handshake () =
  let stop = Miou.Computation.create () in
  with_server ~stop (fun connect ->
    with_raw_client connect (fun fd ->
      Miou_unix.sleep 0.1;
      let since = Unix.gettimeofday () in
      resolve stop ();
      Alcotest.(check int)
        "server closed"
        0
        (Miou_unix.read fd (Bytes.create 16));
      check_bounded "handshake" ~max:0.5 since));
  check_no_handler_error ()

let test_write_timeout () =
  stream_closed := false;
  with_server ~write_timeout:0.3 (fun connect ->
    with_raw_client connect (fun fd ->
      let raw = raw_client fd in
      raw_write raw "GET /stream-forever HTTP/1.1\r\nhost: localhost\r\n\r\n";
      let from = Unix.gettimeofday () in
      while (not !stream_closed) && Unix.gettimeofday () -. from < 3. do
        Miou_unix.sleep 0.01
      done;
      check_bounded "write timeout" ~max:1.5 from;
      Alcotest.(check bool)
        "the handler's flush got `Closed"
        true
        !stream_closed;
      let rec at_end () =
        match Miou_unix.read fd (Bytes.create 0x10000) with
        | 0 -> ()
        | _ -> at_end ()
        | exception Unix.Unix_error (ECONNRESET, _, _) -> ()
      in
      at_end ()));
  check_no_handler_error ()

let test_keep_alive_hint () =
  with_server ~idle_timeout:3.5 (fun connect ->
    with_raw_client connect (fun fd ->
      let raw = raw_client fd in
      raw_write raw "GET /hint HTTP/1.1\r\nhost: localhost\r\n\r\n";
      let expected =
        "HTTP/1.1 200 OK\r\n\
         content-length: 5\r\n\
         Keep-Alive: timeout=3\r\n\
         \r\n\
         /hint"
      in
      Alcotest.(check string)
        "hint"
        expected
        (raw_take raw (String.length expected))));
  check_no_handler_error ()

let stray_outcome client =
  let result = Miou.Computation.create () in
  let finish r = ignore (Miou.Computation.try_return result r : bool) in
  let writer =
    Ache_miou_tls.Client.request
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
  match Miou.Computation.await result with
  | Ok outcome -> outcome
  | Error (exn, bt) -> Printexc.raise_with_backtrace exn bt

(* Client: TLS records that arrive on an idle connection (undici
   GHSA-35p6-xmwp-9g52). Application data or a close_notify closes the
   connection; a key update or an empty line does not. *)
let test_stray_records_on_idle_connection () =
  let port, server =
    Ache_test_tls_stray.start server_config Ache_test_tls_stray.strays
  in
  List.iter
    (fun (name, _, expected) ->
       Miou_unix.run ~domains:0 (fun () ->
         let socket = Miou_unix.tcpv4 () in
         Miou_unix.connect
           socket
           (Unix.ADDR_INET (Unix.inet_addr_loopback, port));
         let client =
           Ache_miou_tls.Client.create_connection_with_fd
             ~tls:client_config
             ~peer:(`Host localhost)
             socket
         in
         Alcotest.(check string)
           (name ^ ": first")
           "200 ok"
           (stray_outcome client);
         (* The stray records arrive while this domain does not run the read
            loop. *)
         Unix.sleepf 0.2;
         Alcotest.(check string)
           (name ^ ": second")
           expected
           (stray_outcome client);
         Ache_miou_tls.Client.shutdown client;
         Miou_unix.close socket))
    Ache_test_tls_stray.cases;
  Alcotest.(check (list bool))
    "second requests sent"
    Ache_test_tls_stray.second_requests_sent
    (List.map (fun second -> second <> "") (Domain.join server))

let () =
  Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
  Alcotest.run
    "ache-miou-tls"
    [ ( "tls"
      , [ Alcotest.test_case "GET" `Quick test_get
        ; Alcotest.test_case
            "client: TLS records on an idle connection"
            `Quick
            test_stray_records_on_idle_connection
        ; Alcotest.test_case "Keep-Alive hint" `Quick test_keep_alive_hint
        ; Alcotest.test_case
            "write timeout: a client that stops reading"
            `Quick
            test_write_timeout
        ; Alcotest.test_case
            "stop closes an idle connection"
            `Quick
            test_stop_idle_connection
        ; Alcotest.test_case
            "stop ends a handshake in progress"
            `Quick
            test_stop_during_handshake
        ; Alcotest.test_case "WebSocket echo" `Quick test_websocket_echo
        ; Alcotest.test_case
            "a streamed response: one TLS record for each 16 KiB"
            `Quick
            test_stream_records
        ; Alcotest.test_case
            "client_of_fd, epoch and create_connection"
            `Quick
            test_epoch
        ; Alcotest.test_case "POST echo of 200 kB" `Quick test_post_echo
        ; Alcotest.test_case "client pipelines" `Quick test_client_pipelined
        ; Alcotest.test_case
            "pipelined requests and connection: close"
            `Quick
            test_pipelined_connection_close
        ; Alcotest.test_case
            "client sends close_notify"
            `Quick
            test_client_sends_close_notify
        ; Alcotest.test_case
            "request body cut without close_notify"
            `Quick
            test_cut_request_body
        ; Alcotest.test_case
            "close-delimited response cut without close_notify"
            `Quick
            test_truncated_response
        ; Alcotest.test_case
            "close-delimited response ended by close_notify"
            `Quick
            test_close_notify_response
        ; Alcotest.test_case
            "Content-Length response, no close_notify"
            `Quick
            test_fixed_length_without_close_notify
        ; Alcotest.test_case
            "chunked response, no close_notify"
            `Quick
            test_chunked_without_close_notify
        ; Alcotest.test_case
            "no response and no close_notify is Connection_closed"
            `Quick
            test_no_response_without_close_notify
        ; Alcotest.test_case
            "when the timeout arguments are checked"
            `Quick
            test_timeout_check_timing
        ; Alcotest.test_case
            "untrusted certificate"
            `Quick
            test_untrusted_certificate
        ; Alcotest.test_case
            "plain-text client that closes"
            `Quick
            test_plain_text_client_closes
        ; Alcotest.test_case
            "plain-text client that waits"
            `Quick
            test_plain_text_client_waits
        ; Alcotest.test_case
            "handshake bounded by idle_timeout"
            `Quick
            test_handshake_timeout
        ; Alcotest.test_case
            "handshake bounded by head_timeout"
            `Quick
            test_handshake_head_timeout
        ; Alcotest.test_case
            "handshake bounded by the smaller timeout"
            `Quick
            test_handshake_smaller_timeout
        ; Alcotest.test_case
            "handshake without a timeout waits for the client"
            `Quick
            test_handshake_no_timeout
        ; Alcotest.test_case "peer `Host" `Quick test_peer_host
        ; Alcotest.test_case "peer `Ip" `Quick test_peer_ip
        ; Alcotest.test_case
            "peer `Insecure_no_name_check"
            `Quick
            test_peer_insecure
        ; Alcotest.test_case "idle timeout" `Quick test_idle_timeout
        ; Alcotest.test_case
            "response while a body read waits: body_timeout"
            `Quick
            (respond_while_body_read_waits ?idle_timeout:None)
        ; Alcotest.test_case
            "response while a body read waits: body_timeout, idle_timeout"
            `Quick
            (respond_while_body_read_waits ~idle_timeout:0.2)
        ; Alcotest.test_case "head timeout" `Quick test_head_timeout
        ; Alcotest.test_case
            "input after a head timeout"
            `Quick
            test_head_timeout_then_data
        ; Alcotest.test_case "handler exception" `Quick test_handler_exception
        ; Alcotest.test_case "peer reset mid-head" `Quick test_reset_mid_head
        ; Alcotest.test_case "client upgrade" `Quick test_upgrade
        ; Alcotest.test_case
            "server upgrade with bytes after the head"
            `Quick
            test_upgrade_bytes_after_head
        ; Alcotest.test_case
            "server upgrade with bytes sent before the 101"
            `Quick
            test_upgrade_with_read_pending
        ; Alcotest.test_case
            "request after the idle timeout"
            `Quick
            test_idle_timeout_then_request
        ; Alcotest.test_case
            "Client.shutdown with a read pending"
            `Quick
            test_client_shutdown_with_pending_read
        ; Alcotest.test_case
            "completed handshakes do not pile up timers"
            `Quick
            test_handshake_timers_bounded
        ; Alcotest.test_case
            "handshakes cut by the client do not pile up timers"
            `Quick
            test_cut_handshake_timers_bounded
        ] )
    ]
