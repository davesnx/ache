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

let request_handler (_ : Unix.sockaddr) reqd =
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

let error_handler (_ : Unix.sockaddr) ?request:_ error start_response =
  server_errors := error :: !server_errors;
  Body.Writer.close (start_response Headers.empty)

open Lwt.Infix

let run f = Lwt_main.run (Lwt_unix.with_timeout 10.0 f)

let with_server
      ?(connection_handler =
        fun ~request_handler ~error_handler ->
          Ache_lwt.Server.create_connection_handler
            ~request_handler
            ~error_handler)
      f
  =
  server_errors := [];
  run (fun () ->
    let listening = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    Lwt_unix.setsockopt listening Unix.SO_REUSEADDR true;
    Lwt_unix.bind listening Unix.(ADDR_INET (inet_addr_loopback, 0))
    >>= fun () ->
    Lwt_unix.listen listening 16;
    let addr = Lwt_unix.getsockname listening in
    let connections = ref [] in
    let rec accept () =
      Lwt_unix.accept listening >>= fun (socket, client_addr) ->
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
    Stdlib.Buffer.add_string buf (Bigstringaf.substring bs ~off ~len);
    Body.Reader.schedule_read body ~on_eof ~on_read
  and on_eof () = Lwt.wakeup_later u (Stdlib.Buffer.contents buf) in
  Body.Reader.schedule_read body ~on_eof ~on_read;
  p

let client_error_handler error =
  match error with
  | `Malformed_response msg -> Alcotest.failf "malformed response: %s" msg
  | `Invalid_response_body_length _ -> Alcotest.fail "invalid body length"
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

  let shutdown_receive socket =
    try Lwt_unix.shutdown socket Unix.SHUTDOWN_RECEIVE with
    | Unix.Unix_error _ -> ()

  let close socket =
    incr closes;
    Lwt_unix.close socket
end

module Test_runtime = Ache_lwt.Make (Test_io)
module Test_server = Test_runtime.Server

let with_failing_io ~read ~write =
  fail_read := read;
  fail_write := write;
  closes := 0;
  with_server
    ~connection_handler:(fun ~request_handler ~error_handler ->
      Test_server.create_connection_handler ~request_handler ~error_handler)
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

let test_read_failure () = with_failing_io ~read:true ~write:false
let test_write_failure () = with_failing_io ~read:false ~write:true

let test_eof_mid_body () =
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
  match !server_errors with
  | [ `Bad_request ] -> ()
  | _ ->
    Alcotest.failf
      "expected one Bad_request, got %d errors"
      (List.length !server_errors)

let () =
  Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
  Alcotest.run
    "ache-lwt loopback"
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
        ] )
    ]
