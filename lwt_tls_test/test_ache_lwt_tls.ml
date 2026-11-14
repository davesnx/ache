open Ache
open Lwt.Infix

let certs = Ache_test_certs.generate ()
let server_config = Ache_test_certs.server_config certs
let client_config = Ache_test_certs.client_config certs
let localhost = Domain_name.(host_exn (of_string_exn "localhost"))
let server_errors : Server_connection.error list ref = ref []
let partial_body_cut = ref false

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
  | _, target ->
    Body.Reader.close body;
    Reqd.respond_with_string reqd (Response.create `OK) target

let error_handler (_ : Unix.sockaddr) ?request:_ error start_response =
  server_errors := error :: !server_errors;
  Body.Writer.close (start_response Headers.empty)

let run f = Lwt_main.run (Lwt_unix.with_timeout 10.0 f)

let with_listener handler f =
  let listening = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Lwt_unix.setsockopt listening Unix.SO_REUSEADDR true;
  Lwt_unix.bind listening Unix.(ADDR_INET (inet_addr_loopback, 0)) >>= fun () ->
  Lwt_unix.listen listening 16;
  let addr = Lwt_unix.getsockname listening in
  let accepted = ref [] in
  let rec accept () =
    Lwt_unix.accept listening >>= fun (fd, client_addr) ->
    accepted := (fd, handler client_addr fd) :: !accepted;
    accept ()
  in
  let acceptor = accept () in
  let connect () =
    let fd = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    Lwt_unix.connect fd addr >|= fun () -> fd
  in
  f connect >>= fun () ->
  Lwt.cancel acceptor;
  Lwt_unix.close listening >>= fun () ->
  Lwt.join (List.map snd !accepted) >|= fun () ->
  List.iter
    (fun (fd, _) ->
       Alcotest.(check bool)
         "server file descriptor closed"
         true
         (Lwt_unix.state fd = Lwt_unix.Closed))
    !accepted

let with_server ?idle_timeout ?head_timeout f =
  server_errors := [];
  partial_body_cut := false;
  run (fun () ->
    with_listener
      (Ache_lwt_tls.Server.create_connection_handler_with_fd
         ?idle_timeout
         ?head_timeout
         ~tls:server_config
         ~request_handler
         ~error_handler)
      f)

type raw =
  { tls : Tls_lwt.Unix.t
  ; output : Lwt_io.output_channel
  ; tcp_eof : bool ref
  ; fd : Lwt_unix.file_descr
  }

let raw_channels fd =
  let tcp_eof = ref false in
  let input =
    Lwt_io.make ~mode:Lwt_io.Input (fun buf off len ->
      Lwt_bytes.read fd buf off len >|= fun n ->
      if n = 0 then tcp_eof := true;
      n)
  in
  let output = Lwt_io.make ~mode:Lwt_io.Output (Lwt_bytes.write fd) in
  (input, output), tcp_eof

let raw_client ?(config = client_config) fd =
  let ((_, output) as channels), tcp_eof = raw_channels fd in
  Tls_lwt.Unix.client_of_channels config ~host:localhost channels >|= fun tls ->
  { tls; output; tcp_eof; fd }

let raw_server fd =
  let ((_, output) as channels), tcp_eof = raw_channels fd in
  Tls_lwt.Unix.server_of_channels server_config channels >|= fun tls ->
  { tls; output; tcp_eof; fd }

let raw_write raw s =
  Tls_lwt.Unix.write raw.tls s >>= fun () -> Lwt_io.flush raw.output

let raw_read_until_eof raw =
  let buf = Stdlib.Buffer.create 1024 in
  let chunk = Bytes.create 4096 in
  let rec go () =
    Tls_lwt.Unix.read raw.tls chunk >>= function
    | 0 ->
      Lwt.return
        ( Stdlib.Buffer.contents buf
        , if !(raw.tcp_eof) then `Tcp_eof else `Close_notify )
    | n ->
      Stdlib.Buffer.add_subbytes buf chunk 0 n;
      go ()
  in
  go ()

let raw_read_head raw =
  let buf = Stdlib.Buffer.create 256 in
  let chunk = Bytes.create 1 in
  let rec go () =
    if String.ends_with ~suffix:"\r\n\r\n" (Stdlib.Buffer.contents buf)
    then Lwt.return (Stdlib.Buffer.contents buf)
    else
      Tls_lwt.Unix.read raw.tls chunk >>= function
      | 0 -> Lwt.return (Stdlib.Buffer.contents buf)
      | _ ->
        Stdlib.Buffer.add_char buf (Bytes.get chunk 0);
        go ()
  in
  go ()

let raw_close raw =
  Tls_lwt.Unix.close raw.tls >>= fun () -> Lwt_unix.close raw.fd

let read_body body =
  let buf = Stdlib.Buffer.create 1024 in
  let p, u = Lwt.wait () in
  let rec on_read bs ~off ~len =
    Stdlib.Buffer.add_string buf (Bigstringaf.substring bs ~off ~len);
    Body.Reader.schedule_read body ~on_eof ~on_error ~on_read
  and on_eof () = Lwt.wakeup_later u (Ok (Stdlib.Buffer.contents buf))
  and on_error () = Lwt.wakeup_later u (Error (Stdlib.Buffer.contents buf)) in
  Body.Reader.schedule_read body ~on_eof ~on_error ~on_read;
  p

let request ?(errors = ref []) client ?(body = "") request =
  let p, u = Lwt.wait () in
  let request_body =
    Ache_lwt_tls.Client.request
      client
      request
      ~error_handler:(fun error -> errors := error :: !errors)
      ~response_handler:(fun response response_body ->
        Lwt.wakeup_later u (response, read_body response_body))
  in
  Body.Writer.write_string request_body body;
  Body.Writer.close request_body;
  p >>= fun (response, body) ->
  body >|= fun body -> response, body

let get target =
  Request.create ~headers:(Headers.of_list [ "host", "localhost" ]) `GET target

let connect_client connect =
  connect () >>= fun fd ->
  Ache_lwt_tls.Client.create_connection_with_fd
    ~tls:client_config
    ~peer:(`Host localhost)
    fd

let body_result = Alcotest.(result string string)

let test_get () =
  with_server (fun connect ->
    connect_client connect >>= fun client ->
    request client (get "/hello") >>= fun (response, body) ->
    Alcotest.(check int) "status" 200 (Status.to_code response.status);
    Alcotest.check body_result "body" (Ok "/hello") body;
    Ache_lwt_tls.Client.shutdown client)

let test_post_echo () =
  with_server (fun connect ->
    connect_client connect >>= fun client ->
    let payload = String.init 200_000 (fun i -> Char.chr (97 + (i mod 26))) in
    let headers =
      Headers.of_list
        [ "host", "localhost"
        ; "content-length", string_of_int (String.length payload)
        ]
    in
    request client ~body:payload (Request.create ~headers `POST "/echo")
    >>= fun (response, body) ->
    Alcotest.(check int) "status" 200 (Status.to_code response.status);
    Alcotest.check body_result "body" (Ok payload) body;
    Ache_lwt_tls.Client.shutdown client)

let test_client_pipelined () =
  with_server (fun connect ->
    connect_client connect >>= fun client ->
    Lwt.both (request client (get "/a")) (request client (get "/b"))
    >>= fun ((_, a), (_, b)) ->
    Alcotest.check body_result "first" (Ok "/a") a;
    Alcotest.check body_result "second" (Ok "/b") b;
    Ache_lwt_tls.Client.shutdown client)

let test_pipelined_connection_close () =
  with_server (fun connect ->
    connect () >>= raw_client >>= fun raw ->
    raw_write
      raw
      "GET /a HTTP/1.1\r\n\
       host: localhost\r\n\
       \r\n\
       GET /b HTTP/1.1\r\n\
       host: localhost\r\n\
       connection: close\r\n\
       \r\n"
    >>= fun () ->
    raw_read_until_eof raw >>= fun (output, ending) ->
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
    raw_close raw)

let test_client_sends_close_notify () =
  run (fun () ->
    let server_saw = ref None in
    with_listener
      (fun _ fd ->
         raw_server fd >>= fun raw ->
         raw_read_head raw >>= fun (_ : string) ->
         raw_write raw "HTTP/1.1 200 OK\r\ncontent-length: 2\r\n\r\nok"
         >>= fun () ->
         raw_read_until_eof raw >>= fun (rest, ending) ->
         server_saw := Some (rest, ending);
         raw_close raw)
      (fun connect ->
         connect_client connect >>= fun client ->
         request client (get "/") >>= fun (_, body) ->
         Alcotest.check body_result "body" (Ok "ok") body;
         Ache_lwt_tls.Client.shutdown client)
    >|= fun () ->
    Alcotest.(check bool)
      "client sent close_notify and nothing else"
      true
      (!server_saw = Some ("", `Close_notify)))

let test_cut_request_body () =
  with_server (fun connect ->
    connect () >>= raw_client >>= fun raw ->
    raw_write
      raw
      "POST /partial HTTP/1.1\r\n\
       host: localhost\r\n\
       content-length: 1000\r\n\
       \r\n\
       0123456789"
    >>= fun () ->
    Lwt_unix.sleep 0.05 >>= fun () ->
    Lwt_unix.close raw.fd >|= fun () -> ());
  Alcotest.(check bool) "body reported cut" true !partial_body_cut

let response_then_close
      ?(response = "HTTP/1.1 200 OK\r\nconnection: close\r\n\r\npartial")
      ~close_notify
      ()
  =
  let errors = ref [] in
  let body =
    run (fun () ->
      let result = ref (Error "no response") in
      with_listener
        (fun _ fd ->
           raw_server fd >>= fun raw ->
           raw_read_head raw >>= fun (_ : string) ->
           raw_write raw response >>= fun () ->
           if close_notify then raw_close raw else Lwt_unix.close raw.fd)
        (fun connect ->
           connect_client connect >>= fun client ->
           request ~errors client (get "/") >>= fun (_, body) ->
           result := body;
           Ache_lwt_tls.Client.shutdown client)
      >|= fun () -> !result)
  in
  body, !errors

let test_truncated_response () =
  let body, errors = response_then_close ~close_notify:false () in
  Alcotest.check body_result "body cut" (Error "partial") body;
  Alcotest.(check bool)
    "error handler got Truncated"
    true
    (match errors with [ `Exn Ache_lwt_tls.Truncated ] -> true | _ -> false)

let test_close_notify_response () =
  let body, errors = response_then_close ~close_notify:true () in
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
  let errors = ref [] in
  run (fun () ->
    with_listener
      (fun _ fd ->
         raw_server fd >>= fun raw ->
         raw_read_head raw >>= fun (_ : string) -> Lwt_unix.close raw.fd)
      (fun connect ->
         connect_client connect >>= fun client ->
         let failed, notify_failed = Lwt.wait () in
         let request_body =
           Ache_lwt_tls.Client.request
             client
             (get "/")
             ~error_handler:(fun error ->
               errors := error :: !errors;
               Lwt.wakeup_later notify_failed ())
             ~response_handler:(fun _ _ -> Alcotest.fail "unexpected response")
         in
         Body.Writer.close request_body;
         failed >>= fun () -> Ache_lwt_tls.Client.shutdown client));
  Alcotest.(check bool)
    "Connection_closed"
    true
    (match !errors with [ `Connection_closed ] -> true | _ -> false)

let test_untrusted_certificate () =
  let other_ca = Ache_test_certs.generate () in
  with_server (fun connect ->
    connect () >>= fun fd ->
    Lwt.try_bind
      (fun () ->
         Ache_lwt_tls.Client.create_connection_with_fd
           ~tls:(Ache_test_certs.client_config other_ca)
           ~peer:(`Host localhost)
           fd)
      (fun _ -> Alcotest.fail "handshake succeeded")
      (function
        | Tls_lwt.Tls_failure (`Error (`AuthenticationFailure _)) ->
          Alcotest.(check bool)
            "client file descriptor closed"
            true
            (Lwt_unix.state fd = Lwt_unix.Closed);
          Lwt.return_unit
        | exn -> Alcotest.failf "unexpected %s" (Printexc.to_string exn)))

let send_plain_request connect =
  connect () >>= fun fd ->
  let request = "GET / HTTP/1.1\r\nhost: localhost\r\n\r\n" in
  Lwt_unix.write_string fd request 0 (String.length request)
  >|= fun (_ : int) -> fd

let test_plain_text_client_closes () =
  with_server (fun connect ->
    send_plain_request connect >>= fun fd -> Lwt_unix.close fd)

let test_plain_text_client_waits () =
  let since = Unix.gettimeofday () in
  with_server ~idle_timeout:0.2 (fun connect ->
    send_plain_request connect >>= fun fd ->
    let chunk = Bytes.create 4096 in
    let rec drain () =
      Lwt_unix.read fd chunk 0 4096 >>= function
      | 0 -> Lwt.return_unit
      | _ -> drain ()
    in
    Lwt.catch drain (fun (_ : exn) -> Lwt.return_unit) >>= fun () ->
    Lwt_unix.close fd);
  let elapsed = Unix.gettimeofday () -. since in
  if elapsed > 2.0 then Alcotest.failf "closed after %.3f s" elapsed

let test_handshake_timeout () =
  let since = Unix.gettimeofday () in
  with_server ~idle_timeout:0.2 (fun connect ->
    connect () >>= fun fd ->
    let chunk = Bytes.create 16 in
    Lwt_unix.read fd chunk 0 16 >>= fun n ->
    Alcotest.(check int) "server closed" 0 n;
    Lwt_unix.close fd);
  let elapsed = Unix.gettimeofday () -. since in
  if elapsed > 2.0 then Alcotest.failf "closed after %.3f s" elapsed

let test_idle_timeout () =
  with_server ~idle_timeout:0.2 (fun connect ->
    connect () >>= raw_client >>= fun raw ->
    raw_read_until_eof raw >>= fun (output, ending) ->
    Alcotest.(check string) "no response" "" output;
    Alcotest.(check bool) "close_notify" true (ending = `Close_notify);
    raw_close raw)

let test_head_timeout () =
  with_server ~head_timeout:0.2 (fun connect ->
    connect () >>= raw_client >>= fun raw ->
    raw_write raw "GET /a HTTP/1.1\r\n" >>= fun () ->
    raw_read_until_eof raw >>= fun (output, ending) ->
    Alcotest.(check bool)
      "408"
      true
      (String.starts_with ~prefix:"HTTP/1.1 408 Request Timeout\r\n" output);
    Alcotest.(check bool) "close_notify" true (ending = `Close_notify);
    raw_close raw)

let handshake_with peer =
  let result = ref None in
  with_server (fun connect ->
    connect () >>= fun fd ->
    Lwt.try_bind
      (fun () ->
         Ache_lwt_tls.Client.create_connection_with_fd
           ~tls:client_config
           ~peer
           fd)
      (fun client ->
         result := Some (Ok ());
         Ache_lwt_tls.Client.shutdown client)
      (fun exn ->
         result := Some (Error exn);
         Alcotest.(check bool)
           "client file descriptor closed"
           true
           (Lwt_unix.state fd = Lwt_unix.Closed);
         Lwt.return_unit));
  Option.get !result

let check_accepted name peer =
  match handshake_with peer with
  | Ok () -> ()
  | Error exn ->
    Alcotest.failf "%s: rejected with %s" name (Printexc.to_string exn)

let check_rejected name peer =
  match handshake_with peer with
  | Error (Tls_lwt.Tls_failure (`Error (`AuthenticationFailure _))) -> ()
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

let stalled_handshake ?idle_timeout ?head_timeout () =
  let since = Unix.gettimeofday () in
  with_server ?idle_timeout ?head_timeout (fun connect ->
    connect () >>= fun fd ->
    let chunk = Bytes.create 16 in
    Lwt_unix.read fd chunk 0 16 >>= fun n ->
    Alcotest.(check int) "server closed" 0 n;
    Lwt_unix.close fd);
  Unix.gettimeofday () -. since

let check_bounded name elapsed =
  if elapsed > 1.0 then Alcotest.failf "%s: closed after %.3f s" name elapsed

let test_handshake_head_timeout () =
  check_bounded "head_timeout" (stalled_handshake ~head_timeout:0.2 ())

let test_handshake_smaller_timeout () =
  check_bounded
    "head_timeout smaller"
    (stalled_handshake ~idle_timeout:5.0 ~head_timeout:0.2 ());
  check_bounded
    "idle_timeout smaller"
    (stalled_handshake ~idle_timeout:0.2 ~head_timeout:5.0 ())

let test_handshake_no_timeout () =
  with_server (fun connect ->
    connect () >>= fun fd ->
    let chunk = Bytes.create 16 in
    Lwt.pick
      [ (Lwt_unix.read fd chunk 0 16 >|= fun _ -> `Closed)
      ; (Lwt_unix.sleep 0.5 >|= fun () -> `Open)
      ]
    >>= fun state ->
    Alcotest.(check bool) "still open after 0.5 s" true (state = `Open);
    Lwt_unix.close fd)

let () =
  Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
  Alcotest.run
    "ache-lwt-tls"
    [ ( "tls"
      , [ Alcotest.test_case "GET" `Quick test_get
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
        ] )
    ]
