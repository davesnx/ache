open Ache
open Lwt.Infix

let certs = Ache_test_certs.generate ()
let server_config = Ache_test_certs.server_config certs
let client_config = Ache_test_certs.client_config certs
let localhost = Domain_name.(host_exn (of_string_exn "localhost"))
let server_errors : Server_connection.error list ref = ref []
let partial_body_cut = ref false
let slow_reqd : Request_descriptor.t option ref = ref None
let stream_closed : unit Lwt.u option ref = ref None

(* A chunked response of many small buffers, each its own iovec. *)
let stream_piece = String.make 64 'x'
let stream_pieces = 1024

let request_handler (_ : Unix.sockaddr) reqd =
  let request = Request_descriptor.request reqd in
  let body = Request_descriptor.request_body reqd in
  match request.meth, request.target with
  | `GET, "/slow" ->
    Body.Reader.close body;
    slow_reqd := Some reqd
  | `GET, "/stream-forever" ->
    Body.Reader.close body;
    let response_body =
      Request_descriptor.respond_with_streaming reqd (Response.create `OK)
    in
    let chunk = String.make 65536 'x' in
    let rec loop () =
      Body.Writer.write_string response_body chunk;
      Body.Writer.flush response_body (function
        | `Written -> loop ()
        | `Closed -> Option.iter (fun u -> Lwt.wakeup_later u ()) !stream_closed)
    in
    loop ()
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
  | _, target ->
    Body.Reader.close body;
    Request_descriptor.respond_with_string reqd (Response.create `OK) target

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
  run (fun () ->
    with_listener
      (Ache_lwt_tls.Server.create_connection_handler_with_fd
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
    Stdlib.Buffer.add_string buf (Ache.Bigstring.substring bs ~off ~len);
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
    connect () >>= fun fd ->
    let state, init =
      Tls.Engine.client (Tls.Config.peer client_config localhost)
    in
    let records = Records.create state in
    let rec write s off =
      if off = String.length s
      then Lwt.return_unit
      else
        Lwt_unix.write_string fd s off (String.length s - off) >>= fun n ->
        write s (off + n)
    in
    let buf = Bytes.create 0x4000 in
    let rec pump ~reply until =
      if until ()
      then Lwt.return_unit
      else
        Lwt_unix.read fd buf 0 (Bytes.length buf) >>= function
        | 0 -> Lwt.return_unit
        | n ->
          let replies = Records.feed records (Bytes.sub_string buf 0 n) in
          (if reply
           then Lwt_list.iter_s (fun s -> write s 0) replies
           else Lwt.return_unit)
          >>= fun () -> pump ~reply until
    in
    write init 0 >>= fun () ->
    pump ~reply:true (fun () ->
      not (Tls.Engine.handshake_in_progress records.state))
    >>= fun () ->
    write (Records.send records stream_request) 0 >>= fun () ->
    pump ~reply:false (fun () -> records.closed) >>= fun () ->
    check_stream_records records;
    Lwt_unix.close fd)

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

let test_timeout_check_timing () =
  let fd = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  let addr = Unix.ADDR_INET (Unix.inet_addr_loopback, 0) in
  let bound =
    Ache_lwt_tls.Server.create_connection_handler_with_fd
      ~date:false
      ~config:Ache.Config.default
      ~idle_timeout:(Some 0.)
      ~head_timeout:(Some 1.)
      ~tls:server_config
      ~request_handler
      ~error_handler
  in
  (match bound addr fd with
  | (_ : unit Lwt.t) -> Alcotest.fail "expected Invalid_argument"
  | exception Invalid_argument _ -> ());
  Lwt_main.run (Lwt_unix.close fd)

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

let live_words () =
  Gc.full_major ();
  (Gc.stat ()).live_words

let check_handshake_timers_bounded ~connections exchange =
  let previous = Lwt_engine.get () in
  Lwt_engine.set ~destroy:false (new Lwt_engine.select);
  let growth = ref 0 in
  Fun.protect
    ~finally:(fun () -> Lwt_engine.set previous)
    (fun () ->
       Lwt_main.run
       @@ Lwt_unix.with_timeout 60.0
       @@ fun () ->
       let listening = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
       Lwt_unix.setsockopt listening Unix.SO_REUSEADDR true;
       Lwt_unix.bind listening Unix.(ADDR_INET (inet_addr_loopback, 0))
       >>= fun () ->
       Lwt_unix.listen listening 16;
       let addr = Lwt_unix.getsockname listening in
       let open_connections = ref 0 in
       let rec accept () =
         Lwt_unix.accept listening >>= fun (fd, client_addr) ->
         incr open_connections;
         Lwt.async (fun () ->
           Ache_lwt_tls.Server.create_connection_handler_with_fd
             ~date:false
             ~idle_timeout:(Some 30.)
             ~head_timeout:(Some 30.)
             ~tls:server_config
             ~request_handler
             ~error_handler
             client_addr
             fd
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
           let fd = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
           Lwt_unix.connect fd addr >>= fun () ->
           exchange fd >>= fun () -> exchanges (n - 1)
       in
       exchanges (connections / 10) >>= fun () ->
       (* On OCaml 5.3 the live heap keeps settling for a few full major cycles
          after the first exchanges, by a constant that does not grow with the
          number of connections. *)
       let rec measure windows =
         let before = live_words () in
         exchanges connections >>= fun () ->
         growth := live_words () - before;
         if !growth > connections && windows > 1
         then measure (windows - 1)
         else Lwt.return_unit
       in
       measure 5 >>= fun () ->
       Lwt.cancel earlier_live_sleeper;
       Lwt.cancel acceptor;
       Lwt_unix.close listening);
  if !growth > connections
  then
    Alcotest.failf
      "heap grew by %d words over %d connections"
      !growth
      connections

let test_handshake_timers_bounded () =
  check_handshake_timers_bounded ~connections:300 (fun fd ->
    raw_client fd >>= fun raw ->
    raw_write
      raw
      "GET /hello HTTP/1.1\r\nhost: localhost\r\nconnection: close\r\n\r\n"
    >>= fun () ->
    raw_read_until_eof raw >>= fun (output, _) ->
    if not (String.ends_with ~suffix:"/hello" output)
    then Alcotest.failf "unexpected response %S" output;
    raw_close raw)

let test_cut_handshake_timers_bounded () =
  check_handshake_timers_bounded ~connections:1000 (fun fd ->
    Lwt_unix.sleep 0.001 >>= fun () -> Lwt_unix.close fd)

let test_stop_drains () =
  slow_reqd := None;
  let stop, stop_now = Lwt.wait () in
  with_server ~stop ~grace:5. (fun connect ->
    connect () >>= fun stalled ->
    connect () >>= raw_client >>= fun raw ->
    raw_write raw "GET /slow HTTP/1.1\r\nhost: localhost\r\n\r\n" >>= fun () ->
    let rec until_slow () =
      match !slow_reqd with
      | Some reqd -> Lwt.return reqd
      | None -> Lwt_unix.sleep 0.005 >>= until_slow
    in
    until_slow () >>= fun reqd ->
    let since = Unix.gettimeofday () in
    Lwt.wakeup_later stop_now ();
    let chunk = Bytes.create 16 in
    Lwt_unix.read stalled chunk 0 16 >>= fun n ->
    Alcotest.(check int) "handshake in progress closed" 0 n;
    check_bounded "handshake closed" (Unix.gettimeofday () -. since);
    Request_descriptor.respond_with_string
      reqd
      (Response.create ~headers:(Headers.of_list [ "content-length", "4" ]) `OK)
      "done";
    raw_read_until_eof raw >>= fun (output, ending) ->
    Alcotest.(check string)
      "response with connection: close"
      "HTTP/1.1 200 OK\r\ncontent-length: 4\r\nconnection: close\r\n\r\ndone"
      output;
    Alcotest.(check bool) "close_notify" true (ending = `Close_notify);
    Lwt_unix.close stalled >>= fun () -> raw_close raw)

let test_write_timeout () =
  let closed, notify_closed = Lwt.wait () in
  stream_closed := Some notify_closed;
  with_server ~write_timeout:0.3 (fun connect ->
    connect () >>= fun fd ->
    Lwt_unix.setsockopt_int fd Unix.SO_RCVBUF 4096;
    raw_client fd >>= fun raw ->
    raw_write raw "GET /stream-forever HTTP/1.1\r\nhost: localhost\r\n\r\n"
    >>= fun () ->
    let since = Unix.gettimeofday () in
    closed >>= fun () ->
    let elapsed = Unix.gettimeofday () -. since in
    if elapsed < 0.3 || elapsed > 1.5
    then Alcotest.failf "flush got `Closed after %.3f s" elapsed;
    Lwt_unix.close fd)

let stray_outcome client =
  let result, resolve = Lwt.wait () in
  let finish r = if Lwt.is_sleeping result then Lwt.wakeup_later resolve r in
  let writer =
    Ache_lwt_tls.Client.request
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

(* Client: TLS records that arrive on an idle connection (undici
   GHSA-35p6-xmwp-9g52). Application data or a close_notify closes the
   connection; a key update or an empty line does not. *)
let test_stray_records_on_idle_connection () =
  let port, server =
    Ache_test_tls_stray.start server_config Ache_test_tls_stray.strays
  in
  List.iter
    (fun (name, _, expected) ->
       Lwt_main.run
         (Lwt_unix.with_timeout 10.0 (fun () ->
            let fd = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
            Lwt_unix.connect fd (Unix.ADDR_INET (Unix.inet_addr_loopback, port))
            >>= fun () ->
            Ache_lwt_tls.Client.create_connection_with_fd
              ~tls:client_config
              ~peer:(`Host localhost)
              fd
            >>= fun client ->
            stray_outcome client >>= fun first ->
            Alcotest.(check string) (name ^ ": first") "200 ok" first;
            (* The stray records arrive while this thread does not run the read
               loop. *)
            Unix.sleepf 0.2;
            stray_outcome client >>= fun second ->
            Alcotest.(check string) (name ^ ": second") expected second;
            Ache_lwt_tls.Client.shutdown client)))
    Ache_test_tls_stray.cases;
  Alcotest.(check (list bool))
    "second requests sent"
    Ache_test_tls_stray.second_requests_sent
    (List.map (fun second -> second <> "") (Domain.join server))

let () =
  Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
  Alcotest.run
    "ache-lwt-tls"
    [ ( "tls"
      , [ Alcotest.test_case "GET" `Quick test_get
        ; Alcotest.test_case
            "client: TLS records on an idle connection"
            `Quick
            test_stray_records_on_idle_connection
        ; Alcotest.test_case
            "a streamed response: one TLS record for each 16 KiB"
            `Quick
            test_stream_records
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
        ; Alcotest.test_case
            "stop closes a handshake and drains a request"
            `Quick
            test_stop_drains
        ; Alcotest.test_case
            "a client that stops reading is closed at write_timeout"
            `Quick
            test_write_timeout
        ; Alcotest.test_case "idle timeout" `Quick test_idle_timeout
        ; Alcotest.test_case "head timeout" `Quick test_head_timeout
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
