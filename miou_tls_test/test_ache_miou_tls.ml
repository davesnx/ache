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

let pending_upgrade : Reqd.t option ref = ref None

let request_handler (_ : Unix.sockaddr) reqd =
  let request = Reqd.request reqd in
  let body = Reqd.request_body reqd in
  match request.meth, request.target with
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
    Reqd.respond_with_upgrade
      reqd
      (Headers.of_list [ "connection", "upgrade"; "upgrade", "echo" ])
      (Runtime.make (module Echo) (Echo.create (fun t s -> Echo.send t s)))
  | `GET, "/upgrade-later" -> pending_upgrade := Some reqd
  | `GET, "/raise" -> failwith "handler failed"
  | _, target ->
    Body.Reader.close body;
    Reqd.respond_with_string reqd (Response.create `OK) target

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

let with_listener handler f =
  handler_errors := [];
  let fds = open_fds () in
  Miou_unix.run ~domains:0 (fun () ->
    let listening = Miou_unix.tcpv4 () in
    Miou_unix.bind_and_listen
      ~backlog:16
      listening
      (Unix.ADDR_INET (Unix.inet_addr_loopback, 0));
    let addr = Unix.getsockname (Miou_unix.to_file_descr listening) in
    let accepted = ref 0
    and handlers_done = ref 0 in
    let server =
      Miou.async (fun () ->
        let orphans = Miou.orphans () in
        let rec accept () =
          let socket, peer = Miou_unix.accept listening in
          incr accepted;
          ignore
            (Miou.async ~orphans (fun () ->
               (try handler socket peer with
               | Miou.Cancelled as exn -> raise exn
               | exn -> handler_errors := exn :: !handler_errors);
               Miou_unix.close socket;
               incr handlers_done)
             : unit Miou.t);
          accept ()
        in
        accept ())
    in
    let connect () =
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
             if !handlers_done < !accepted && n > 0
             then (
               Miou_unix.sleep 0.01;
               wait (n - 1))
           in
           wait 500;
           Alcotest.(check int)
             "connection handlers returned"
             !accepted
             !handlers_done)));
  Alcotest.(check int) "no file descriptor leaked" fds (open_fds ())

let with_server ?idle_timeout ?head_timeout f =
  server_errors := [];
  partial_body_cut := false;
  with_listener
    (Ache_miou_tls.Server.create_connection_handler_with_fd
       ?idle_timeout
       ?head_timeout
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
    Stdlib.Buffer.add_string buf (Bigstringaf.substring bs ~off ~len);
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
                      Error (s ^ Bigstringaf.substring bs ~off ~len));
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
        ~head_timeout:(-1.)
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
              ~config:Ache.Config.default
              ~idle_timeout:1.
              ~head_timeout:(-1.)
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
      Reqd.respond_with_upgrade
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
      raw_close_notify raw));
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

let () =
  Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
  Alcotest.run
    "ache-miou-tls"
    [ ( "tls"
      , [ Alcotest.test_case "GET" `Quick test_get
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
        ] )
    ]
