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

let resolve computation value =
  ignore (Miou.Computation.try_return computation value : bool)

let await computation =
  match Miou.Computation.await computation with
  | Ok value -> value
  | Error (exn, bt) -> Printexc.raise_with_backtrace exn bt

let server_errors : Server_connection.error list ref = ref []
let partial_body_cut = ref false
let stream_flush_closed : unit Miou.Computation.t option ref = ref None
let requests_seen = ref 0
let on_ws_closed : (int option -> unit) ref = ref ignore
let pending_upgrade : Request_descriptor.t option ref = ref None
let pending_response : Request_descriptor.t option ref = ref None
let pending_body_failed = ref false

exception Respond_later

let late_error_response : (unit -> unit) option ref = ref None
let big_body = String.make (16 * 1024 * 1024) 'a'

module Ws = Ache_miou.Websocket

let ws_ended : (string, exn) result Miou.Computation.t option ref = ref None
let ws_held : Ws.t Miou.Computation.t option ref = ref None
let ws_release : unit Miou.Computation.t option ref = ref None

let end_ws result =
  Option.iter (fun ended -> resolve ended result) !ws_ended;
  ws_ended := None

let text_of = function Websocket.Text s -> s | Binary s -> "binary:" ^ s

let ws_echo ws =
  let rec loop () =
    match Ws.receive ws with
    | Some (Text "close-me") ->
      Ws.close ws 1001;
      end_ws
        (Ok
           (match Ws.receive ws, Ws.client_close ws with
           | None, None -> "closed, no client close"
           | None, Some { code; reason } ->
             Printf.sprintf "closed, client close %d %S" code reason
           | Some m, _ -> "received " ^ text_of m))
    | Some (Text "ping-me") ->
      Ws.ping ws "are you there";
      Ws.send_text ws "pong seen";
      loop ()
    | Some (Text "bye-reason") ->
      Ws.close ~reason:"done here" ws 4000;
      end_ws (Ok "closed with a reason")
    | Some (Text message) ->
      Ws.send_text ws message;
      loop ()
    | Some (Binary message) ->
      Ws.send_binary ws message;
      loop ()
    | None ->
      end_ws
        (Ok
           (match Ws.client_close ws with
           | None -> "receive None"
           | Some { code; reason } ->
             Printf.sprintf "receive None, client close %d %S" code reason))
  in
  loop ()

let ws_send_loop ws =
  let chunk = String.make 4096 'x' in
  let rec loop () =
    Ws.send_text ws chunk;
    loop ()
  in
  match (loop () : unit) with
  | () -> ()
  | exception exn ->
    end_ws
      (match Ws.receive ws with
      | None -> Error exn
      | Some _ -> Ok "receive returned a message after the reset")

let ws_hold ws =
  Option.iter (fun held -> resolve held ws) !ws_held;
  Option.iter await !ws_release

exception Session_failed

let ws_session = function
  | "/ws-echo" -> Some ws_echo
  | "/ws-send-loop" -> Some ws_send_loop
  | "/ws-hold" -> Some ws_hold
  | "/ws-raise" -> Some (fun (_ : Ws.t) -> raise Session_failed)
  | "/ws-return" -> Some (fun ws -> Ws.send_text ws "hi")
  | _ -> None

let ws_gates : unit Miou.Computation.t list ref = ref []
let body_failed = ref false
let stream_closed : unit Miou.Computation.t option ref = ref None

let request_handler (_ : Unix.sockaddr) reqd =
  incr requests_seen;
  let request = Request_descriptor.request reqd in
  let body = Request_descriptor.request_body reqd in
  match request.meth, request.target with
  | `POST, "/count-body" ->
    let count = ref 0 in
    let rec on_read _ ~off:_ ~len =
      count := !count + len;
      Body.Reader.schedule_read body ~on_eof ~on_error ~on_read
    and on_eof () =
      let text = string_of_int !count in
      Request_descriptor.respond_with_string
        reqd
        (Response.create
           ~headers:
             (Headers.of_list
                [ "content-length", string_of_int (String.length text) ])
           `OK)
        text
    and on_error () = body_failed := true in
    Body.Reader.schedule_read body ~on_eof ~on_error ~on_read
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
          | `Closed ->
            Option.iter (fun closed -> resolve closed ()) !stream_closed))
    in
    write 1024
  | `GET, "/sleep-1s" ->
    Body.Reader.close body;
    Miou_unix.sleep 1.0;
    Request_descriptor.respond_with_string
      reqd
      (Response.create ~headers:(Headers.of_list [ "content-length", "4" ]) `OK)
      "done"
  | `GET, "/slow" ->
    Body.Reader.close body;
    Miou_unix.sleep 0.4;
    Request_descriptor.respond_with_string
      reqd
      (Response.create ~headers:(Headers.of_list [ "content-length", "4" ]) `OK)
      "slow"
  | `GET, "/stuck" ->
    Body.Reader.close body;
    Miou_unix.sleep 5.0;
    Request_descriptor.respond_with_string
      reqd
      (Response.create ~headers:(Headers.of_list [ "content-length", "5" ]) `OK)
      "stuck"
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
  | `GET, "/ws-wait" ->
    let gate = Miou.Computation.create () in
    ws_gates := !ws_gates @ [ gate ];
    await gate;
    (match Ws.upgrade reqd ws_echo with
    | Ok () -> ()
    | Error _ -> Alcotest.fail "handshake rejected")
  | `GET, target when Option.is_some (ws_session target) ->
    (match Ws.upgrade reqd (Option.get (ws_session target)) with
    | Ok () -> ()
    | Error _ -> Alcotest.fail "handshake rejected")
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
        | `Closed -> Option.iter (fun c -> resolve c ()) !stream_flush_closed)
    in
    loop ()
  | `GET, "/big" ->
    Body.Reader.close body;
    Request_descriptor.respond_with_string
      reqd
      (Response.create
         ~headers:
           (Headers.of_list
              [ "content-length", string_of_int (String.length big_body) ])
         `OK)
      big_body
  | `GET, "/upgrade-later" -> pending_upgrade := Some reqd
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
  | `POST, "/respond-later" ->
    let rec on_read _ ~off:_ ~len:_ =
      Body.Reader.schedule_read body ~on_eof ~on_error ~on_read
    and on_eof () = Alcotest.fail "a cut body completed"
    and on_error () = pending_body_failed := true in
    Body.Reader.schedule_read body ~on_eof ~on_error ~on_read;
    pending_response := Some reqd
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

let error_handler (_ : Unix.sockaddr) ?request:_ error start_response =
  server_errors := error :: !server_errors;
  match error with
  | `Exn Respond_later ->
    late_error_response :=
      Some (fun () -> Body.Writer.close (start_response Headers.empty))
  | _ -> Body.Writer.close (start_response Headers.empty)

let connections_done = ref 0
let connections_taken = ref 0

let rec wait_connection_done () =
  if !connections_done > !connections_taken
  then incr connections_taken
  else (
    Miou_unix.sleep 0.001;
    wait_connection_done ())

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

(* Timeouts that a test does not give are off, not at their defaults. *)
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
      f
  =
  server_errors := [];
  requests_seen := 0;
  connections_done := 0;
  connections_taken := 0;
  Miou_unix.run ~domains:0 (fun () ->
    let listening = Miou_unix.tcpv4 () in
    Miou_unix.bind_and_listen
      ~backlog:16
      listening
      (Unix.ADDR_INET (Unix.inet_addr_loopback, 0));
    let addr = Unix.getsockname (Miou_unix.to_file_descr listening) in
    let server =
      Miou.async (fun () ->
        let orphans = Miou.orphans () in
        let rec accept () =
          let socket, peer = Miou_unix.accept listening in
          ignore
            (Miou.async ~orphans (fun () ->
               Fun.protect
                 ~finally:(fun () -> Miou_unix.close socket)
                 (fun () ->
                    Ache_miou.Server.create_connection_handler
                      ~date
                      ?config
                      ~idle_timeout
                      ~head_timeout
                      ~body_timeout
                      ~write_timeout
                      ~close_timeout
                      ?stop
                      ?grace
                      ~request_handler
                      ~error_handler
                      socket
                      peer);
               incr connections_done)
             : unit Miou.t);
          accept ()
        in
        accept ())
    in
    Fun.protect
      ~finally:(fun () ->
        Miou.cancel server;
        Miou_unix.close listening)
      (fun () -> with_timeout 10.0 (fun () -> f addr)))

let connect addr =
  let socket = Miou_unix.tcpv4 () in
  Miou_unix.connect socket addr;
  socket

let send socket s = Miou_unix.write socket s

let shutdown_send socket =
  Unix.shutdown (Miou_unix.to_file_descr socket) Unix.SHUTDOWN_SEND

let read_all socket =
  let out = Stdlib.Buffer.create 1024 in
  let buf = Bytes.create 4096 in
  let rec loop () =
    match Miou_unix.read socket buf with
    | 0 -> ()
    | n ->
      Stdlib.Buffer.add_subbytes out buf 0 n;
      loop ()
    | exception Unix.Unix_error (ECONNRESET, _, _) -> ()
  in
  loop ();
  Stdlib.Buffer.contents out

let take socket n =
  let buf = Bytes.create n in
  Miou_unix.really_read socket buf;
  Bytes.to_string buf

let at_end_of_input socket =
  match Miou_unix.read socket (Bytes.create 1) with
  | 0 -> true
  | _ -> false
  | exception Unix.Unix_error (ECONNRESET, _, _) -> true

let read_line socket =
  let out = Stdlib.Buffer.create 64 in
  let byte = Bytes.create 1 in
  let rec loop () =
    Miou_unix.really_read socket byte;
    match Bytes.get byte 0 with
    | '\n' -> ()
    | '\r' -> loop ()
    | c ->
      Stdlib.Buffer.add_char out c;
      loop ()
  in
  loop ();
  Stdlib.Buffer.contents out

let read_head socket =
  let status = read_line socket in
  let rec skip () = if read_line socket <> "" then skip () in
  skip ();
  status

let read_body body =
  let buf = Stdlib.Buffer.create 1024 in
  let result = Miou.Computation.create () in
  let rec on_read bs ~off ~len =
    Stdlib.Buffer.add_string buf (Ache.Bigstring.substring bs ~off ~len);
    Body.Reader.schedule_read body ~on_eof ~on_error ~on_read
  and on_eof () = resolve result (Stdlib.Buffer.contents buf)
  and on_error () = resolve result "<response body cut short>" in
  Body.Reader.schedule_read body ~on_eof ~on_error ~on_read;
  result

let client_error_handler error =
  match error with
  | `Malformed_response msg -> Alcotest.failf "malformed response: %s" msg
  | `Invalid_response_body_length _ -> Alcotest.fail "invalid body length"
  | `Connection_closed -> Alcotest.fail "connection closed"
  | `Exn exn -> raise exn

let request client ?(body = "") request =
  let result = Miou.Computation.create () in
  let request_body =
    Ache_miou.Client.request
      client
      request
      ~error_handler:client_error_handler
      ~response_handler:(fun response response_body ->
        resolve result (response, read_body response_body))
  in
  Body.Writer.write_string request_body body;
  Body.Writer.close request_body;
  let response, body = await result in
  response, await body

let get_hello = "GET /hello HTTP/1.1\r\nhost: localhost\r\n\r\n"
let hello_response = "HTTP/1.1 200 OK\r\ncontent-length: 11\r\n\r\nhello world"

let hello_response_close =
  "HTTP/1.1 200 OK\r\n\
   content-length: 11\r\n\
   connection: close\r\n\
   \r\n\
   hello world"

(* [keep_alive ?idle_timeout response] is [response] with the [Keep-Alive:
   timeout=n] field that a server with [idle_timeout] adds. *)
let keep_alive ?idle_timeout response =
  match idle_timeout with
  | Some seconds when seconds >= 1. ->
    let head_end =
      let rec find i =
        if String.sub response i 4 = "\r\n\r\n" then i else find (i + 1)
      in
      find 0
    in
    String.sub response 0 (head_end + 2)
    ^ Printf.sprintf "Keep-Alive: timeout=%d\r\n" (int_of_float seconds)
    ^ String.sub response (head_end + 2) (String.length response - head_end - 2)
  | Some _ | None -> response

let status_line output =
  match String.index_opt output '\r' with
  | Some i -> String.sub output 0 i
  | None -> output

let check_elapsed name ~from ~min ~max =
  let elapsed = Unix.gettimeofday () -. from in
  if elapsed < min || elapsed > max
  then Alcotest.failf "%s: %.3f s, expected %.3f to %.3f s" name elapsed min max

let test_get () =
  with_server (fun addr ->
    let socket = connect addr in
    let client = Ache_miou.Client.create_connection socket in
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
    Ache_miou.Client.shutdown client;
    Alcotest.(check bool) "closed" true (Ache_miou.Client.is_closed client);
    Miou_unix.close socket)

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
       with_server ~date (fun addr ->
         let socket = connect addr in
         let client = Ache_miou.Client.create_connection socket in
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
         Ache_miou.Client.shutdown client;
         Miou_unix.close socket))
    [ true; false ]

let test_post_echo () =
  with_server (fun addr ->
    let socket = connect addr in
    let client = Ache_miou.Client.create_connection socket in
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
    Ache_miou.Client.shutdown client;
    Miou_unix.close socket)

let test_pipelined () =
  with_server (fun addr ->
    let socket = connect addr in
    send
      socket
      "GET /a HTTP/1.1\r\n\
       host: localhost\r\n\
       \r\n\
       GET /b HTTP/1.1\r\n\
       host: localhost\r\n\
       connection: close\r\n\
       \r\n";
    let output = read_all socket in
    Miou_unix.close socket;
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

let test_upgrade () =
  with_server (fun addr ->
    let socket = connect addr in
    let client = Ache_miou.Client.create_connection socket in
    let upgraded = Miou.Computation.create () in
    let request_body =
      Ache_miou.Client.request
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
    Ache_miou.Client.upgrade client (Runtime.make (module Echo) echo_client);
    Alcotest.(check string) "echoed" message (await echoed);
    Ache_miou.Client.shutdown client;
    Miou_unix.close socket)

let test_connect_tunnel_bytes_after_head () =
  with_server (fun addr ->
    let socket = connect addr in
    send
      socket
      "CONNECT localhost:443 HTTP/1.1\r\nhost: localhost:443\r\n\r\nping";
    let expected = "HTTP/1.1 200 OK\r\n\r\nping" in
    Alcotest.(check string)
      "200, then the bytes sent with the head come back"
      expected
      (take socket (String.length expected));
    send socket "pong";
    Alcotest.(check string) "later bytes" "pong" (take socket 4);
    Miou_unix.close socket;
    wait_connection_done ())

let test_upgrade_incomplete_token_reaches_new_protocol () =
  with_server (fun addr ->
    let socket = connect addr in
    send
      socket
      "GET /upgrade HTTP/1.1\r\n\
       host: localhost\r\n\
       connection: upgrade\r\n\
       upgrade: echo\r\n\
       \r\n\
       ping";
    let expected =
      "HTTP/1.1 101 Switching Protocols\r\n\
       connection: upgrade\r\n\
       upgrade: echo\r\n\
       \r\n\
       ping"
    in
    Alcotest.(check string)
      "101 then echoed bytes"
      expected
      (take socket (String.length expected));
    Miou_unix.close socket)

let test_eof_mid_body () =
  partial_body_cut := false;
  with_server (fun addr ->
    let socket = connect addr in
    send
      socket
      "POST /partial HTTP/1.1\r\n\
       host: localhost\r\n\
       content-length: 100\r\n\
       \r\n\
       only ten b";
    shutdown_send socket;
    let (_ : string) = read_all socket in
    Miou_unix.close socket);
  Alcotest.(check bool) "request body cut" true !partial_body_cut;
  match !server_errors with
  | [ `Bad_request _ ] -> ()
  | _ ->
    Alcotest.failf
      "expected one Bad_request, got %d errors"
      (List.length !server_errors)

let test_half_close_gets_response () =
  with_server (fun addr ->
    let socket = connect addr in
    send socket get_hello;
    shutdown_send socket;
    let output = read_all socket in
    Miou_unix.close socket;
    Alcotest.(check string)
      "response after the peer half-closed"
      hello_response
      output;
    wait_connection_done ());
  Alcotest.(check int) "no error" 0 (List.length !server_errors)

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
  let closed = Miou.Computation.create () in
  on_ws_closed := resolve closed;
  with_server (fun addr ->
    let socket = connect addr in
    send socket ws_flood_request;
    Alcotest.(check string) "101" ws_101 (take socket (String.length ws_101));
    Alcotest.(check string)
      "server is sending"
      "\x81\x7e\x10\x00"
      (take socket 4);
    Unix.setsockopt_optint
      (Miou_unix.to_file_descr socket)
      Unix.SO_LINGER
      (Some 0);
    let reset_at = Unix.gettimeofday () in
    Miou_unix.close socket;
    with_timeout 2.0 (fun () ->
      Alcotest.(check (option int))
        "on_close without a close frame"
        None
        (await closed);
      wait_connection_done ());
    let elapsed = Unix.gettimeofday () -. reset_at in
    Alcotest.(check bool)
      (Printf.sprintf "ended within 1 s (%.3f s)" elapsed)
      true
      (elapsed < 1.0));
  on_ws_closed := ignore

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

let read_ws_frame socket =
  let head = take socket 2 in
  let len = Char.code head.[1] in
  assert (len < 126);
  Char.code head.[0], take socket len

let first_of tasks =
  match Miou.await_first (List.map Miou.async tasks) with
  | Ok value -> value
  | Error exn -> raise exn

let sleep_then seconds value () =
  Miou_unix.sleep seconds;
  value

let wait_ws_end () =
  let ended = Miou.Computation.create () in
  ws_ended := Some ended;
  ended

let ws_connect addr target =
  let socket = connect addr in
  send socket (ws_handshake target);
  Alcotest.(check string) "101" ws_101 (take socket (String.length ws_101));
  socket

let test_websocket_echo () =
  let ended = wait_ws_end () in
  with_server (fun addr ->
    let socket = ws_connect addr "/ws-echo" in
    send socket (ws_frame 0x1 "hello");
    let first = read_ws_frame socket in
    send
      socket
      (ws_frame ~fin:false 0x1 "w\xc3"
      ^ ws_frame 0x9 "ping"
      ^ ws_frame 0x0 "\xb6rld");
    let second = read_ws_frame socket in
    let third = read_ws_frame socket in
    send socket (ws_frame 0x2 "\x00\xff");
    let fourth = read_ws_frame socket in
    Alcotest.(check (list (pair int string)))
      "echo, pong, echo of the fragmented message, binary echo"
      [ 0x81, "hello"; 0x8a, "ping"; 0x81, "w\xc3\xb6rld"; 0x82, "\x00\xff" ]
      [ first; second; third; fourth ];
    send socket (ws_frame 0x8 "\x03\xe8bye");
    Alcotest.(check (pair int string))
      "close echoed"
      (0x88, "\x03\xe8")
      (read_ws_frame socket);
    Alcotest.(check string) "then end of input" "" (read_all socket);
    Alcotest.(check (result string reject))
      "session"
      (Ok "receive None, client close 1000 \"bye\"")
      (await ended);
    Miou_unix.close socket;
    wait_connection_done ())

let test_websocket_server_close () =
  let ended = wait_ws_end () in
  with_server (fun addr ->
    let socket = ws_connect addr "/ws-echo" in
    send socket (ws_frame 0x1 "close-me");
    Alcotest.(check (pair int string))
      "close 1001"
      (0x88, "\x03\xe9")
      (read_ws_frame socket);
    (* The server waits for the client's close frame and drops the message
       before it. *)
    send socket (ws_frame 0x1 "dropped" ^ ws_frame 0x8 "\x0f\xa0done");
    Alcotest.(check (result string reject))
      "close returned with the client's close frame"
      (Ok "closed, client close 4000 \"done\"")
      (await ended);
    Alcotest.(check string) "then end of input" "" (read_all socket);
    Miou_unix.close socket;
    wait_connection_done ())

(* The client never answers the server's close frame, and keeps sending messages
   for 3 s: they do not extend the close timeout. *)
let test_websocket_close_timeout () =
  let ended = wait_ws_end () in
  with_server ~close_timeout:0.2 (fun addr ->
    let socket = ws_connect addr "/ws-echo" in
    send socket (ws_frame 0x1 "close-me");
    Alcotest.(check (pair int string))
      "close 1001"
      (0x88, "\x03\xe9")
      (read_ws_frame socket);
    let from = Unix.gettimeofday () in
    let sender =
      Miou.async (fun () ->
        let rec loop n =
          if n > 0
          then (
            (try send socket (ws_frame 0x1 "dropped") with
            | Unix.Unix_error ((EPIPE | ECONNRESET), _, _) -> ());
            Miou_unix.sleep 0.05;
            loop (n - 1))
        in
        loop 60)
    in
    Alcotest.(check string) "then end of input" "" (read_all socket);
    check_elapsed "closed" ~from ~min:0. ~max:2.0;
    Miou.cancel sender;
    Alcotest.(check (result string reject))
      "close returned without the client's close frame"
      (Ok "closed, no client close")
      (await ended);
    Miou_unix.close socket;
    wait_connection_done ())

(* The read that waits when the session closes has the idle limit, or none: the
   close frame must give it the close deadline. The client sends nothing after
   the server's close frame. *)
(* After the close timeout, the connection does not linger for [idle_timeout]
   on a client that keeps sending and never closes its socket. *)
let test_websocket_close_timeout_no_linger () =
  let ended = wait_ws_end () in
  with_server ~idle_timeout:30. ~close_timeout:0.2 (fun addr ->
    let socket = ws_connect addr "/ws-echo" in
    send socket (ws_frame 0x1 "close-me");
    Alcotest.(check (pair int string))
      "close 1001"
      (0x88, "\x03\xe9")
      (read_ws_frame socket);
    let from = Unix.gettimeofday () in
    let sender =
      Miou.async (fun () ->
        let rec loop () =
          match send socket (ws_frame 0x1 "dropped") with
          | () ->
            Miou_unix.sleep 0.05;
            loop ()
          | exception Unix.Unix_error ((EPIPE | ECONNRESET), _, _) -> ()
        in
        loop ())
    in
    wait_connection_done ();
    check_elapsed "handler returned" ~from ~min:0. ~max:2.5;
    Miou.cancel sender;
    Alcotest.(check (result string reject))
      "close returned without the client's close frame"
      (Ok "closed, no client close")
      (await ended);
    Miou_unix.close socket)

let test_websocket_close_timeout_silent ?idle_timeout () =
  let ended = wait_ws_end () in
  with_server ?idle_timeout ~close_timeout:0.2 (fun addr ->
    let socket = ws_connect addr "/ws-echo" in
    send socket (ws_frame 0x1 "close-me");
    Alcotest.(check (pair int string))
      "close 1001"
      (0x88, "\x03\xe9")
      (read_ws_frame socket);
    let from = Unix.gettimeofday () in
    Alcotest.(check string) "then end of input" "" (read_all socket);
    check_elapsed "closed" ~from ~min:0. ~max:2.5;
    Alcotest.(check (result string reject))
      "close returned without the client's close frame"
      (Ok "closed, no client close")
      (await ended);
    Miou_unix.close socket;
    wait_connection_done ())

(* After the server's close frame, a ping from the client still gets a pong; the
   client then sends nothing and keeps its socket open, and the close timeout
   ends the connection. *)
let test_websocket_close_timeout_pong_then_silent () =
  let ended = wait_ws_end () in
  with_server ~idle_timeout:30. ~close_timeout:0.2 (fun addr ->
    let socket = ws_connect addr "/ws-echo" in
    send socket (ws_frame 0x1 "close-me");
    Alcotest.(check (pair int string))
      "close 1001"
      (0x88, "\x03\xe9")
      (read_ws_frame socket);
    let from = Unix.gettimeofday () in
    send socket (ws_frame 0x9 "p");
    Alcotest.(check (pair int string))
      "pong after close"
      (0x8a, "p")
      (read_ws_frame socket);
    Alcotest.(check string) "then end of input" "" (read_all socket);
    check_elapsed "closed" ~from ~min:0. ~max:2.5;
    wait_connection_done ();
    Alcotest.(check (result string reject))
      "close returned without the client's close frame"
      (Ok "closed, no client close")
      (await ended);
    Miou_unix.close socket)

let test_websocket_ping () =
  let ended = wait_ws_end () in
  with_server (fun addr ->
    let socket = ws_connect addr "/ws-echo" in
    send socket (ws_frame 0x1 "ping-me");
    Alcotest.(check (pair int string))
      "server ping"
      (0x89, "are you there")
      (read_ws_frame socket);
    send socket (ws_frame 0xA "are you there");
    Alcotest.(check (pair int string))
      "ping returned at the pong"
      (0x81, "pong seen")
      (read_ws_frame socket);
    send socket (ws_frame 0x1 "bye-reason");
    Alcotest.(check (pair int string))
      "close with a reason"
      (0x88, "\x0f\xa0done here")
      (read_ws_frame socket);
    send socket (ws_frame 0x8 "\x0f\xa0");
    Alcotest.(check (result string reject))
      "close returned"
      (Ok "closed with a reason")
      (await ended);
    Alcotest.(check string) "then end of input" "" (read_all socket);
    Miou_unix.close socket;
    wait_connection_done ())

let test_websocket_send_loop_peer_reset () =
  let ended = wait_ws_end () in
  with_server (fun addr ->
    let socket = ws_connect addr "/ws-send-loop" in
    Alcotest.(check string)
      "server is sending"
      "\x81\x7e\x10\x00"
      (take socket 4);
    Unix.setsockopt_optint
      (Miou_unix.to_file_descr socket)
      Unix.SO_LINGER
      (Some 0);
    let reset_at = Unix.gettimeofday () in
    Miou_unix.close socket;
    let result =
      first_of
        [ (fun () -> await ended)
        ; sleep_then 2.0 (Ok "still running 2 s after the reset")
        ]
    in
    let elapsed = Unix.gettimeofday () -. reset_at in
    Alcotest.(check bool)
      "send_text raised Closed, receive is None"
      true
      (result = Error Ws.Closed);
    Alcotest.(check bool)
      (Printf.sprintf "ended within 1 s (%.3f s)" elapsed)
      true
      (elapsed < 1.0);
    wait_connection_done ())

let with_held_session f =
  let held = Miou.Computation.create ()
  and release = Miou.Computation.create () in
  ws_held := Some held;
  ws_release := Some release;
  with_server (fun addr ->
    let socket = ws_connect addr "/ws-hold" in
    let ws = await held in
    Fun.protect ~finally:(fun () -> resolve release ()) (fun () -> f socket ws);
    Miou_unix.close socket;
    wait_connection_done ());
  ws_held := None;
  ws_release := None

let receive_text ws = Option.map text_of (Ws.receive ws)

let receive_within seconds ws =
  first_of
    [ (fun () -> receive_text ws); sleep_then seconds (Some "<timeout>") ]

let cancel_receive ws =
  let receiving = Miou.async (fun () -> Ws.receive ws) in
  Miou.yield ();
  Miou.cancel receiving

let test_websocket_receive_backpressure () =
  with_held_session (fun socket ws ->
    send
      socket
      (String.concat "" (List.init 8 (fun i -> ws_frame 0x1 (string_of_int i))));
    Miou_unix.sleep 0.05;
    send socket (ws_frame 0x9 "p");
    Alcotest.(check (option (pair int string)))
      "8 unread messages: reading paused, no pong"
      None
      (first_of
         [ (fun () -> Some (read_ws_frame socket)); sleep_then 0.2 None ]);
    Alcotest.(check (option string))
      "first message"
      (Some "0")
      (receive_text ws);
    Alcotest.(check (pair int string))
      "receive resumes reading: the pong comes"
      (0x8a, "p")
      (read_ws_frame socket);
    Alcotest.(check (list (option string)))
      "the rest, in order"
      (List.init 7 (fun i -> Some (string_of_int (i + 1))))
      (List.init 7 (fun _ -> receive_text ws)))

let test_websocket_cancelled_receive () =
  with_held_session (fun socket ws ->
    Alcotest.(check (option string))
      "first receive times out"
      (Some "<timeout>")
      (receive_within 0.01 ws);
    send socket (ws_frame 0x1 "hello");
    Alcotest.(check (option string))
      "the next receive gets the message"
      (Some "hello")
      (receive_within 1.0 ws);
    cancel_receive ws;
    send socket (ws_frame 0x1 "again");
    Alcotest.(check (option string))
      "the receive after Miou.cancel gets the message"
      (Some "again")
      (receive_within 1.0 ws))

let test_websocket_cancelled_receive_then_close () =
  with_held_session (fun socket ws ->
    cancel_receive ws;
    Alcotest.(check (option string))
      "receive times out"
      (Some "<timeout>")
      (receive_within 0.01 ws);
    send socket (ws_frame 0x8 "\x03\xe8");
    Alcotest.(check (option string))
      "a later receive gives None"
      None
      (receive_within 1.0 ws);
    Alcotest.(check (pair int string))
      "close echoed"
      (0x88, "\x03\xe8")
      (read_ws_frame socket))

let test_websocket_cancelled_receives_bounded () =
  with_held_session (fun socket ws ->
    let cancel_many n =
      for _ = 1 to n do
        cancel_receive ws
      done
    in
    let live_words () =
      Gc.full_major ();
      (Gc.stat ()).live_words
    in
    cancel_many 500;
    let before = live_words () in
    cancel_many 5_000;
    let grown = live_words () - before in
    Alcotest.(check bool)
      (Printf.sprintf
         "5000 cancelled receives keep %d more live words (< 10000)"
         grown)
      true
      (grown < 10_000);
    send socket (ws_frame 0x1 "after");
    Alcotest.(check (option string))
      "a receive still gets the next message"
      (Some "after")
      (receive_within 1.0 ws))

let raises_closed f =
  match f () with () -> false | exception Ws.Closed -> true

let test_websocket_ping_timeout () =
  with_held_session (fun socket ws ->
    Alcotest.(check bool)
      "no pong: the sleep wins"
      true
      (first_of
         [ (fun () ->
             Ws.ping ws "";
             false)
         ; sleep_then 0.05 true
         ]);
    Alcotest.(check (pair int string))
      "first ping"
      (0x89, "")
      (read_ws_frame socket);
    let answered = Miou.async (fun () -> Ws.ping ws "x") in
    Alcotest.(check (pair int string))
      "second ping"
      (0x89, "x")
      (read_ws_frame socket);
    send socket (ws_frame 0xA "x");
    Alcotest.(check (result unit reject))
      "a pong ends the ping"
      (Ok ())
      (Miou.await answered);
    let pending =
      Miou.async (fun () -> raises_closed (fun () -> Ws.ping ws "y"))
    in
    Alcotest.(check (pair int string))
      "third ping"
      (0x89, "y")
      (read_ws_frame socket);
    send socket (ws_frame 0x8 "\x03\xe8");
    Alcotest.(check bool)
      "close ends the ping with Closed"
      true
      (Miou.await_exn pending);
    Alcotest.(check bool)
      "client_close"
      true
      (Ws.client_close ws = Some { Websocket.code = 1000; reason = "" });
    Alcotest.(check bool)
      "ping after close raises Closed"
      true
      (raises_closed (fun () -> Ws.ping ws ""));
    Alcotest.(check bool)
      "send_text after close raises Closed"
      true
      (raises_closed (fun () -> Ws.send_text ws "late"));
    Ws.close ws 1000;
    Alcotest.(check (option string)) "receive is None" None (receive_text ws);
    Alcotest.(check (pair int string))
      "close echoed"
      (0x88, "\x03\xe8")
      (read_ws_frame socket))

let on_socketpair ?(cancel = false) client_side =
  Miou_unix.run ~domains:0 (fun () ->
    let server, client = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
    let server = Miou_unix.of_file_descr server
    and client = Miou_unix.of_file_descr client in
    Fun.protect
      ~finally:(fun () ->
        Miou_unix.close server;
        Miou_unix.close client)
      (fun () ->
         (* The timeout also covers the end of the connection. *)
         with_timeout 5.0 (fun () ->
           let handler =
             Miou.async (fun () ->
               Ache_miou.Server.create_connection_handler
                 ~date:false
                 ~idle_timeout:None
                 ~head_timeout:None
                 ~body_timeout:None
                 ~write_timeout:None
                 ~request_handler
                 ~error_handler
                 server
                 (Unix.ADDR_UNIX "socketpair"))
           in
           match client_side client with
           | () ->
             if cancel then Miou.cancel handler;
             Miou.await handler
           | exception exn ->
             Miou.cancel handler;
             raise exn)))

let test_websocket_session_exception () =
  match
    on_socketpair (fun client ->
      send client (ws_handshake "/ws-raise");
      Alcotest.(check string) "101" ws_101 (take client (String.length ws_101));
      Alcotest.(check (pair int string))
        "close 1011"
        (0x88, "\x03\xf3")
        (read_ws_frame client);
      send client (ws_frame 0x8 "\x03\xf3");
      Alcotest.(check string) "then end of input" "" (read_all client);
      shutdown_send client)
  with
  | Error Session_failed -> ()
  | Ok () -> Alcotest.fail "the handler returned"
  | Error exn -> raise exn

let test_websocket_session_return_closes () =
  match
    on_socketpair (fun client ->
      send client (ws_handshake "/ws-return");
      Alcotest.(check string) "101" ws_101 (take client (String.length ws_101));
      Alcotest.(check (pair int string))
        "hi"
        (0x81, "hi")
        (read_ws_frame client);
      Alcotest.(check (pair int string))
        "close 1000 when the session returns"
        (0x88, "\x03\xe8")
        (read_ws_frame client);
      send client (ws_frame 0x1 "unread" ^ ws_frame 0x8 "\x03\xe8");
      Alcotest.(check string) "then end of input" "" (read_all client);
      shutdown_send client)
  with
  | Ok () -> ()
  | Error exn -> raise exn

let test_websocket_waiting_handlers () =
  ws_gates := [];
  Miou_unix.run ~domains:0 (fun () ->
    let pair () =
      let server, client = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
      let server = Miou_unix.of_file_descr server
      and client = Miou_unix.of_file_descr client in
      let finished = Miou.Computation.create () in
      let handler =
        Miou.async (fun () ->
          Fun.protect
            ~finally:(fun () -> resolve finished ())
            (fun () ->
               Ache_miou.Server.create_connection_handler
                 ~date:false
                 ~idle_timeout:None
                 ~head_timeout:None
                 ~body_timeout:None
                 ~write_timeout:None
                 ~request_handler
                 ~error_handler
                 server
                 (Unix.ADDR_UNIX "socketpair")))
      in
      server, client, handler, finished
    in
    let wait_gates n =
      while List.length !ws_gates < n do
        Miou_unix.sleep 0.001
      done
    in
    let server_a, a, handler_a, finished_a = pair () in
    let server_b, b, handler_b, finished_b = pair () in
    Fun.protect
      ~finally:(fun () ->
        List.iter Miou_unix.close [ server_a; a; server_b; b ];
        List.iter (fun gate -> resolve gate ()) !ws_gates)
      (fun () ->
         with_timeout 5.0 (fun () ->
           send a (ws_handshake "/ws-wait");
           wait_gates 1;
           send b (ws_handshake "/ws-wait");
           wait_gates 2;
           let gate_a = List.nth !ws_gates 0 in
           resolve gate_a ();
           Alcotest.(check string)
             "A: 101"
             ws_101
             (take a (String.length ws_101));
           send a (ws_frame 0x1 "from A");
           Alcotest.(check (pair int string))
             "A: echo"
             (0x81, "from A")
             (read_ws_frame a);
           send a (ws_frame 0x8 "\x03\xe8");
           Alcotest.(check (pair int string))
             "A: close"
             (0x88, "\x03\xe8")
             (read_ws_frame a);
           shutdown_send a;
           await finished_a;
           Alcotest.(check bool)
             "B still waits"
             true
             (Miou.Computation.is_running finished_b);
           resolve (List.nth !ws_gates 1) ();
           Alcotest.(check string)
             "B: 101"
             ws_101
             (take b (String.length ws_101));
           send b (ws_frame 0x8 "\x03\xe8");
           Alcotest.(check (pair int string))
             "B: close"
             (0x88, "\x03\xe8")
             (read_ws_frame b);
           shutdown_send b;
           await finished_b);
         List.iter
           (fun handler ->
              match Miou.await handler with
              | Ok () -> ()
              | Error exn -> raise exn)
           [ handler_a; handler_b ]))

let test_websocket_session_cancelled_with_connection () =
  let held = Miou.Computation.create () in
  ws_held := Some held;
  ws_release := Some (Miou.Computation.create ());
  let result =
    on_socketpair ~cancel:true (fun client ->
      send client (ws_handshake "/ws-hold");
      Alcotest.(check string) "101" ws_101 (take client (String.length ws_101));
      let (_ : Ws.t) = await held in
      ())
  in
  ws_held := None;
  ws_release := None;
  match result with
  | Error Miou.Cancelled -> ()
  | Ok () -> Alcotest.fail "the handler returned"
  | Error exn -> raise exn

let test_websocket_upgrade_outside_server () =
  let raised = ref None in
  let server =
    Server_connection.create (fun reqd ->
      match Ws.upgrade reqd ignore with
      | Ok () | Error _ -> ()
      | exception Invalid_argument message -> raised := Some message)
  in
  let request = ws_handshake "/ws" in
  let len = String.length request in
  let (_ : int) =
    Server_connection.read
      server
      (Ache.Bigstring.of_string ~off:0 ~len request)
      ~off:0
      ~len
  in
  Alcotest.(check (option string))
    "Invalid_argument"
    (Some
       "Ache_miou.Websocket.upgrade: call it from the request handler of an \
        Ache_miou server")
    !raised

let test_flush_after_peer_closed () =
  let flushed = Miou.Computation.create () in
  stream_flush_closed := Some flushed;
  with_server (fun addr ->
    let socket = connect addr in
    send socket "GET /stream HTTP/1.1\r\nhost: localhost\r\n\r\n";
    Alcotest.(check string) "status line" "HTTP/1.1 200 OK" (read_head socket);
    Unix.setsockopt_optint
      (Miou_unix.to_file_descr socket)
      Unix.SO_LINGER
      (Some 0);
    Miou_unix.close socket;
    await flushed;
    wait_connection_done ());
  stream_flush_closed := None

let test_close_with_trailing_bytes () =
  with_server (fun addr ->
    let socket = connect addr in
    send
      socket
      "GET /hello HTTP/1.1\r\nhost: localhost\r\nconnection: close\r\n\r\n";
    Alcotest.(check string)
      "response"
      hello_response_close
      (take socket (String.length hello_response_close));
    Alcotest.(check bool) "closed" true (at_end_of_input socket);
    send socket "more bytes";
    Miou_unix.close socket;
    wait_connection_done ());
  Alcotest.(check int) "no error" 0 (List.length !server_errors)

let test_handler_exception () =
  with_server (fun addr ->
    let socket = connect addr in
    send socket "GET /raise HTTP/1.1\r\nhost: localhost\r\n\r\n";
    Alcotest.(check string)
      "status line"
      "HTTP/1.1 500 Internal Server Error"
      (status_line (read_all socket));
    Miou_unix.close socket;
    wait_connection_done ())

let rec respond_late () =
  match !late_error_response with
  | Some respond ->
    late_error_response := None;
    respond ()
  | None ->
    Miou_unix.sleep 0.01;
    respond_late ()

let test_late_error_response () =
  late_error_response := None;
  with_server (fun addr ->
    let socket = connect addr in
    send socket "GET /raise-later HTTP/1.1\r\nhost: localhost\r\n\r\n";
    Miou_unix.sleep 0.05;
    respond_late ();
    Alcotest.(check string)
      "status line"
      "HTTP/1.1 500 Internal Server Error"
      (status_line (read_all socket));
    Miou_unix.close socket;
    wait_connection_done ())

let test_cancel_is_not_an_error () =
  server_errors := [];
  Miou_unix.run ~domains:0 (fun () ->
    let server, client = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
    let server = Miou_unix.of_file_descr server
    and client = Miou_unix.of_file_descr client in
    send client "GET /hello HTTP/1.1\r\nHo";
    let handler =
      Miou.async (fun () ->
        Ache_miou.Server.create_connection_handler
          ~date:false
          ~idle_timeout:None
          ~head_timeout:None
          ~body_timeout:None
          ~write_timeout:None
          ~request_handler
          ~error_handler
          server
          (Unix.ADDR_UNIX "socketpair"))
    in
    Miou.yield ();
    Miou.cancel handler;
    (match Miou.await handler with
    | Error Miou.Cancelled -> ()
    | Ok () -> Alcotest.fail "the handler returned"
    | Error exn -> raise exn);
    Miou_unix.close server;
    Miou_unix.close client);
  Alcotest.(check int) "no error reported" 0 (List.length !server_errors)

let close_without_reading_eof ?idle_timeout ?head_timeout ~linger () =
  with_server ?idle_timeout ?head_timeout (fun addr ->
    let socket = connect addr in
    send
      socket
      "GET /hello HTTP/1.1\r\nhost: localhost\r\nconnection: close\r\n\r\n";
    Alcotest.(check string)
      "response"
      hello_response_close
      (take socket (String.length hello_response_close));
    let from = Unix.gettimeofday () in
    wait_connection_done ();
    check_elapsed "linger" ~from ~min:(linger -. 0.05) ~max:(linger +. 0.5);
    Miou_unix.close socket)

let test_no_linger_without_timeouts () = close_without_reading_eof ~linger:0. ()

let test_linger_bounded_by_idle_timeout () =
  close_without_reading_eof ~idle_timeout:0.3 ~linger:0.3 ()

let test_linger_bounded_without_idle_timeout () =
  close_without_reading_eof
    ~head_timeout:5.
    ~linger:Ache_miou.Server.linger_without_idle_timeout
    ()

let test_linger_ends_at_peer_close () =
  with_server ~idle_timeout:5. (fun addr ->
    let socket = connect addr in
    send
      socket
      "GET /hello HTTP/1.1\r\nhost: localhost\r\nconnection: close\r\n\r\n";
    Alcotest.(check string)
      "response"
      hello_response_close
      (take socket (String.length hello_response_close));
    let from = Unix.gettimeofday () in
    Miou_unix.close socket;
    wait_connection_done ();
    check_elapsed "returns at the peer close" ~from ~min:0. ~max:1.)

let half_close_during_large_response ?idle_timeout ?head_timeout () =
  with_server ?idle_timeout ?head_timeout (fun addr ->
    let socket = connect addr in
    send socket "GET /big HTTP/1.1\r\nhost: localhost\r\n\r\n";
    shutdown_send socket;
    Miou_unix.sleep 0.2;
    let output = read_all socket in
    Miou_unix.close socket;
    let head =
      keep_alive
        ?idle_timeout
        (Printf.sprintf
           "HTTP/1.1 200 OK\r\ncontent-length: %d\r\n\r\n"
           (String.length big_body))
    in
    Alcotest.(check int)
      "bytes received"
      (String.length head + String.length big_body)
      (String.length output);
    Alcotest.(check string)
      "head"
      head
      (String.sub output 0 (String.length head));
    wait_connection_done ());
  Alcotest.(check int) "no error" 0 (List.length !server_errors)

let test_half_close_large_response_idle_timeout () =
  half_close_during_large_response ~idle_timeout:5. ()

let test_half_close_large_response_head_timeout () =
  half_close_during_large_response ~head_timeout:5. ()

let test_linger_not_renewed_by_input () =
  with_server ~idle_timeout:0.3 (fun addr ->
    let socket = connect addr in
    send
      socket
      "GET /hello HTTP/1.1\r\nhost: localhost\r\nconnection: close\r\n\r\n";
    Alcotest.(check string)
      "response"
      hello_response_close
      (take socket (String.length hello_response_close));
    let from = Unix.gettimeofday () in
    let rec trickle () =
      if !connections_done > !connections_taken
      then incr connections_taken
      else if Unix.gettimeofday () -. from > 3.
      then Alcotest.fail "still lingering 3 s after the response"
      else (
        (try send socket "x" with Unix.Unix_error _ -> ());
        Miou_unix.sleep 0.1;
        trickle ())
    in
    trickle ();
    check_elapsed "linger" ~from ~min:0.25 ~max:1.5;
    Miou_unix.close socket)

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

let live_words () =
  Gc.full_major ();
  (Gc.stat ()).live_words

let check_timers_bounded ?idle_timeout ?head_timeout ~requests send_request =
  let growth = ref 0 in
  with_server ?idle_timeout ?head_timeout (fun addr ->
    let earlier_live_sleeper = Miou.async (fun () -> Miou_unix.sleep 20.) in
    let socket = connect addr in
    Unix.setsockopt (Miou_unix.to_file_descr socket) Unix.TCP_NODELAY true;
    let hello_response = keep_alive ?idle_timeout hello_response in
    let get_hellos n =
      for _ = 1 to n do
        send_request socket;
        let response = take socket (String.length hello_response) in
        if response <> hello_response
        then Alcotest.failf "unexpected response %S" response
      done
    in
    get_hellos (requests / 10);
    let before = live_words () in
    get_hellos requests;
    growth := live_words () - before;
    Miou.cancel earlier_live_sleeper;
    Miou_unix.close socket);
  if !growth > requests
  then Alcotest.failf "heap grew by %d words over %d requests" !growth requests

let test_idle_timer_bounded () =
  check_timers_bounded ~idle_timeout:30. ~requests:10_000 (fun socket ->
    send socket get_hello)

let test_head_timer_bounded () =
  check_timers_bounded ~head_timeout:30. ~requests:500 (fun socket ->
    send socket "GET /hello HTTP/1.1\r\n";
    Miou_unix.sleep 0.001;
    send socket "host: localhost\r\n\r\n")

let check_connection_timers_bounded ~connections exchange =
  let growth = ref 0 in
  Miou_unix.run ~domains:0 (fun () ->
    let listening = Miou_unix.tcpv4 () in
    Miou_unix.bind_and_listen
      ~backlog:16
      listening
      (Unix.ADDR_INET (Unix.inet_addr_loopback, 0));
    let addr = Unix.getsockname (Miou_unix.to_file_descr listening) in
    let closed = ref 0 in
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
                 (fun () ->
                    Ache_miou.Server.create_connection_handler
                      ~date:false
                      ~idle_timeout:(Some 30.)
                      ~head_timeout:(Some 30.)
                      ~body_timeout:None
                      ~write_timeout:None
                      ~request_handler
                      ~error_handler
                      socket
                      peer))
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
               let socket = connect addr in
               incr opened;
               Miou_unix.sleep 0.001;
               exchange socket;
               Miou_unix.close socket
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

let test_closed_connection_timers_bounded () =
  check_connection_timers_bounded ~connections:1000 (fun socket ->
    send socket "GET /hello HTTP/1.1\r\n";
    Miou_unix.sleep 0.001;
    send socket "host: localhost\r\nconnection: close\r\n\r\n";
    Alcotest.(check string)
      "response"
      hello_response_close
      (take socket (String.length hello_response_close)))

let test_idle_connection_timers_bounded () =
  let hello_response = keep_alive ~idle_timeout:30. hello_response in
  check_connection_timers_bounded ~connections:1000 (fun socket ->
    send socket get_hello;
    Alcotest.(check string)
      "response"
      hello_response
      (take socket (String.length hello_response)))

let test_report_exn_raises () =
  with_server (fun addr ->
    let socket = connect addr in
    let client = Ache_miou.Client.create_connection socket in
    let upgraded = Miou.Computation.create () in
    let request_body =
      Ache_miou.Client.request
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
        ~response_handler:(fun response _body -> resolve upgraded response)
    in
    Body.Writer.close request_body;
    let (_ : Response.t) = await upgraded in
    Ache_miou.Client.upgrade
      client
      (Runtime.make (module Raising) { Raising.sent = false });
    Ache_miou.Client.shutdown client;
    Miou_unix.close socket)

let test_idle_timeout () =
  with_server ~idle_timeout:0.3 (fun addr ->
    let from = Unix.gettimeofday () in
    let socket = connect addr in
    Alcotest.(check string) "nothing written" "" (read_all socket);
    check_elapsed "closed at the idle timeout" ~from ~min:0.25 ~max:0.6;
    wait_connection_done ();
    check_elapsed "stops waiting for the peer to close" ~from ~min:0.55 ~max:1.2;
    Miou_unix.close socket);
  Alcotest.(check int) "no error" 0 (List.length !server_errors)

let test_idle_timeout_after_cancelled_connection () =
  let client = ref None in
  with_server ~idle_timeout:0.3 (fun addr ->
    client := Some (connect addr);
    Miou_unix.sleep 0.05);
  Option.iter
    (fun socket -> Unix.close (Miou_unix.to_file_descr socket))
    !client;
  test_idle_timeout ()

let test_idle_timeout_between_requests () =
  with_server ~idle_timeout:0.3 (fun addr ->
    let socket = connect addr in
    send socket get_hello;
    Alcotest.(check string)
      "first response"
      hello_response
      (take socket (String.length hello_response));
    Miou_unix.sleep 0.2;
    send socket get_hello;
    Alcotest.(check string)
      "second response"
      hello_response
      (take socket (String.length hello_response));
    let from = Unix.gettimeofday () in
    Alcotest.(check bool) "closed" true (at_end_of_input socket);
    check_elapsed "closed at the idle timeout" ~from ~min:0.25 ~max:0.6;
    Miou_unix.close socket;
    wait_connection_done ());
  Alcotest.(check int) "two requests" 2 !requests_seen

let head_timeout () =
  let output = ref "" in
  with_server ~head_timeout:0.3 (fun addr ->
    let socket = connect addr in
    let from = Unix.gettimeofday () in
    send socket "GET /hello HTTP/1.1\r\nHo";
    output := read_all socket;
    check_elapsed "closed at the head timeout" ~from ~min:0.25 ~max:0.6;
    Miou_unix.close socket;
    wait_connection_done ());
  Alcotest.(check int) "request handler not called" 0 !requests_seen;
  !output

let test_head_timeout () =
  let output = head_timeout () in
  Alcotest.(check int) "one error" 1 (List.length !server_errors);
  Alcotest.(check string)
    "status line"
    "HTTP/1.1 408 Request Timeout"
    (status_line output)

let test_head_deadline () =
  with_server ~head_timeout:0.4 (fun addr ->
    let socket = connect addr in
    let from = Unix.gettimeofday () in
    send socket "GET /hello HTTP/1.1\r\n";
    Miou_unix.sleep 0.2;
    send socket "a: b\r\n";
    let output = read_all socket in
    check_elapsed
      "fired at the head timeout after the first bytes"
      ~from
      ~min:0.35
      ~max:0.55;
    Alcotest.(check string)
      "status line"
      "HTTP/1.1 408 Request Timeout"
      (status_line output);
    Miou_unix.close socket;
    wait_connection_done ())

let test_slow_body_not_cut () =
  with_server ~idle_timeout:0.1 ~head_timeout:0.1 (fun addr ->
    let socket = connect addr in
    send
      socket
      "POST /echo HTTP/1.1\r\n\
       host: localhost\r\n\
       content-length: 10\r\n\
       \r\n\
       hello";
    let head = "HTTP/1.1 200 OK\r\ncontent-length: 10\r\n\r\nhello" in
    Alcotest.(check string)
      "first half echoed"
      head
      (take socket (String.length head));
    Miou_unix.sleep 0.4;
    send socket "world";
    Alcotest.(check string) "second half" "world" (take socket 5);
    Miou_unix.close socket;
    wait_connection_done ());
  Alcotest.(check int) "no error" 0 (List.length !server_errors)

let unread_body_request target =
  Printf.sprintf
    "POST %s HTTP/1.1\r\nhost: localhost\r\ncontent-length: 100000\r\n\r\nabc"
    target

let early_close_response =
  "HTTP/1.1 200 OK\r\nconnection: close\r\ncontent-length: 2\r\n\r\nno"

let early_413_response =
  "HTTP/1.1 413 Content Too Large\r\ncontent-length: 2\r\n\r\nno"

let test_unread_body_closing_response () =
  with_server (fun addr ->
    let socket = connect addr in
    send socket (unread_body_request "/early-close");
    Alcotest.(check string)
      "response, then the connection closes"
      early_close_response
      (read_all socket);
    Miou_unix.close socket;
    wait_connection_done ())

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
  with_server ~idle_timeout:5.0 (fun addr ->
    let socket = connect addr in
    send socket large_413_head;
    Alcotest.(check string)
      "413 closes"
      large_413_response
      (take socket (String.length large_413_response));
    let from = Unix.gettimeofday () in
    (try send socket (String.make 0x50000 'x') with Unix.Unix_error _ -> ());
    Alcotest.(check string) "then closed" "" (read_all socket);
    check_elapsed "closed after 256 KiB" ~from ~min:0. ~max:2.0;
    wait_connection_done ();
    Miou_unix.close socket)

let test_unread_body_body_timeout () =
  with_server ~body_timeout:0.3 (fun addr ->
    let socket = connect addr in
    send socket (unread_body_request "/early-413");
    Alcotest.(check string)
      "response"
      early_413_response
      (take socket (String.length early_413_response));
    let from = Unix.gettimeofday () in
    Alcotest.(check bool) "closed" true (at_end_of_input socket);
    check_elapsed "closed at the body timeout" ~from ~min:0.2 ~max:1.0;
    wait_connection_done ();
    Miou_unix.close socket)

let respond_while_body_read_pending ?idle_timeout ~min ~max () =
  pending_response := None;
  pending_body_failed := false;
  with_server ?idle_timeout ~body_timeout:0.3 (fun addr ->
    let socket = connect addr in
    send socket (unread_body_request "/respond-later");
    let rec respond () =
      match !pending_response with
      | Some reqd ->
        Request_descriptor.respond_with_string reqd (Response.create `OK) "late"
      | None ->
        Miou_unix.sleep 0.001;
        respond ()
    in
    respond ();
    let response = "HTTP/1.1 200 OK\r\ncontent-length: 4\r\n\r\nlate" in
    Alcotest.(check string)
      "response"
      response
      (take socket (String.length response));
    let from = Unix.gettimeofday () in
    Alcotest.(check bool) "closed" true (at_end_of_input socket);
    check_elapsed "closed" ~from ~min ~max;
    wait_connection_done ();
    Miou_unix.close socket);
  Alcotest.(check bool) "request body failed" true !pending_body_failed

let test_respond_while_body_read_pending () =
  respond_while_body_read_pending ~min:0.15 ~max:1.0 ()

let test_respond_while_body_read_pending_idle_timeout () =
  respond_while_body_read_pending ~idle_timeout:0.3 ~min:0.15 ~max:1.0 ()

let respond_when_pending () =
  let rec respond () =
    match !pending_response with
    | Some reqd ->
      pending_response := None;
      Miou_unix.sleep 0.001;
      Request_descriptor.respond_with_string reqd (Response.create `OK) "late"
    | None ->
      Miou_unix.sleep 0.001;
      respond ()
  in
  respond ()

let late_response = "HTTP/1.1 200 OK\r\ncontent-length: 4\r\n\r\nlate"

let body_on_error_raises ?idle_timeout () =
  pending_response := None;
  with_server ?idle_timeout ~body_timeout:0.3 (fun addr ->
    let socket = connect addr in
    send socket (unread_body_request "/respond-later-raise");
    respond_when_pending ();
    Alcotest.(check string)
      "response"
      late_response
      (take socket (String.length late_response));
    Alcotest.(check bool) "closed" true (at_end_of_input socket);
    wait_connection_done ();
    Miou_unix.close socket)

let test_body_on_error_raises () = body_on_error_raises ()

let test_body_on_error_raises_idle_timeout () =
  body_on_error_raises ~idle_timeout:0.3 ()

let test_body_read_stop_deadlines_independent () =
  pending_response := None;
  pending_body_failed := false;
  with_server ~body_timeout:0.3 (fun addr ->
    let socket = connect addr in
    send
      socket
      "POST /respond-later-then-body HTTP/1.1\r\n\
       host: localhost\r\n\
       content-length: 6\r\n\
       \r\n\
       abc";
    respond_when_pending ();
    Alcotest.(check string)
      "first response"
      late_response
      (take socket (String.length late_response));
    Miou_unix.sleep 0.2;
    send socket "def";
    Miou_unix.sleep 0.01;
    let from = Unix.gettimeofday () in
    send socket (unread_body_request "/respond-later");
    respond_when_pending ();
    Alcotest.(check string)
      "second response"
      late_response
      (take socket (String.length late_response));
    Alcotest.(check bool) "closed" true (at_end_of_input socket);
    check_elapsed "closed" ~from ~min:0.25 ~max:0.6;
    wait_connection_done ();
    Miou_unix.close socket);
  Alcotest.(check bool) "second body failed" true !pending_body_failed

let test_no_timeout_configured () =
  with_server (fun addr ->
    let socket = connect addr in
    send socket get_hello;
    Alcotest.(check string)
      "first response"
      hello_response
      (take socket (String.length hello_response));
    Miou_unix.sleep 0.3;
    send socket get_hello;
    Alcotest.(check string)
      "second response"
      hello_response
      (take socket (String.length hello_response));
    Miou_unix.close socket;
    wait_connection_done ())

let test_timeout_check_timing () =
  let raises f =
    match f () with () -> false | exception Invalid_argument _ -> true
  in
  Miou_unix.run ~domains:0 (fun () ->
    let socket = Miou_unix.tcpv4 () in
    let addr = Unix.ADDR_INET (Unix.inet_addr_loopback, 0) in
    let bound =
      Ache_miou.Server.create_connection_handler
        ~date:false
        ~idle_timeout:(Some (-1.))
        ~request_handler
        ~error_handler
    in
    Alcotest.(check bool)
      "bound: raises on a socket"
      true
      (raises (fun () -> bound socket addr));
    Alcotest.(check bool)
      "zero head_timeout raises"
      true
      (raises (fun () ->
         Ache_miou.Server.create_connection_handler
           ~date:false
           ~head_timeout:(Some 0.)
           ~request_handler
           ~error_handler
           socket
           addr));
    let take (h : Miou_unix.file_descr -> Unix.sockaddr -> unit) = h in
    let inline = ref None in
    Alcotest.(check bool)
      "inline: no raise when passed"
      false
      (raises (fun () ->
         inline :=
           Some
             (take
                (Ache_miou.Server.create_connection_handler
                   ~date:false
                   ~idle_timeout:(Some (-1.))
                   ~request_handler
                   ~error_handler))));
    Alcotest.(check bool)
      "inline: raises on a socket"
      true
      (raises (fun () -> Option.get !inline socket addr));
    Alcotest.(check bool)
      "every optional argument given: raises at once"
      true
      (raises (fun () ->
         ignore
           (Ache_miou.Server.create_connection_handler
              ~date:false
              ~config:Ache.Config.default
              ~idle_timeout:(Some (-1.))
              ~head_timeout:(Some 1.)
              ~body_timeout:(Some 1.)
              ~write_timeout:(Some 1.)
              ~close_timeout:(Some 1.)
              ~stop:(Miou.Computation.create ())
              ~grace:1.
              ~request_handler
              ~error_handler
            : _ -> _ -> unit)));
    List.iter
      (fun (name, f) -> Alcotest.(check bool) name true (raises f))
      [ ( "zero body_timeout raises"
        , fun () ->
            Ache_miou.Server.create_connection_handler
              ~date:false
              ~body_timeout:(Some 0.)
              ~request_handler
              ~error_handler
              socket
              addr )
      ; ( "negative write_timeout raises"
        , fun () ->
            Ache_miou.Server.create_connection_handler
              ~date:false
              ~write_timeout:(Some (-1.))
              ~request_handler
              ~error_handler
              socket
              addr )
      ; ( "zero close_timeout raises"
        , fun () ->
            Ache_miou.Server.create_connection_handler
              ~date:false
              ~close_timeout:(Some 0.)
              ~request_handler
              ~error_handler
              socket
              addr )
      ];
    Alcotest.(check bool)
      "zero grace raises"
      true
      (raises (fun () ->
         Ache_miou.Server.create_connection_handler
           ~date:false
           ~grace:0.
           ~request_handler
           ~error_handler
           socket
           addr));
    Miou_unix.close socket)

let test_pipelined_head_deadline () =
  with_server ~head_timeout:0.5 (fun addr ->
    let socket = connect addr in
    send socket "GET /a HTTP/1.1\r\n";
    Miou_unix.sleep 0.3;
    let from = Unix.gettimeofday () in
    send socket "host: x\r\n\r\nGET /b HTTP/1.1\r\n";
    let first = "HTTP/1.1 200 OK\r\ncontent-length: 2\r\n\r\n/a" in
    Alcotest.(check string)
      "first response"
      first
      (take socket (String.length first));
    let output = read_all socket in
    check_elapsed
      "second head times out 0.5 s after its own first byte"
      ~from
      ~min:0.45
      ~max:0.8;
    Alcotest.(check string)
      "second response"
      "HTTP/1.1 408 Request Timeout"
      (status_line output);
    Miou_unix.close socket;
    wait_connection_done ())

let test_head_timeout_then_data () =
  let first = "GET /hello HTTP/1.1\r\nHo"
  and late = "st: localhost\r\n\r\nGET /hello HTTP/1.1\r\n\r\n" in
  with_server ~head_timeout:0.1 (fun addr ->
    let socket = connect addr in
    send socket first;
    Alcotest.(check string)
      "status line"
      "HTTP/1.1 408 Request Timeout"
      (read_head socket);
    send socket late;
    shutdown_send socket;
    Alcotest.(check bool) "nothing more" true (at_end_of_input socket);
    wait_connection_done ();
    Miou_unix.close socket);
  Alcotest.(check int) "no request handled" 0 !requests_seen;
  Alcotest.(check int) "one error" 1 (List.length !server_errors)

let test_idle_timeout_then_request () =
  with_server ~idle_timeout:0.1 (fun addr ->
    let socket = connect addr in
    Miou_unix.sleep 0.15;
    send socket get_hello;
    shutdown_send socket;
    Alcotest.(check string) "no response" "" (read_all socket);
    wait_connection_done ();
    Miou_unix.close socket);
  Alcotest.(check int) "no request handled" 0 !requests_seen;
  Alcotest.(check int) "no error" 0 (List.length !server_errors)

let test_client_shutdown_with_pending_read () =
  let errors = ref [] in
  Miou_unix.run ~domains:0 (fun () ->
    with_timeout 5.0 (fun () ->
      let client_end, server_end =
        Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0
      in
      let client_end = Miou_unix.of_file_descr client_end
      and server_end = Miou_unix.of_file_descr server_end in
      let client = Ache_miou.Client.create_connection client_end in
      let request_body =
        Ache_miou.Client.request
          client
          (Request.create
             ~headers:(Headers.of_list [ "host", "localhost" ])
             `GET
             "/")
          ~error_handler:(fun error -> errors := error :: !errors)
          ~response_handler:(fun _ _ -> Alcotest.fail "unexpected response")
      in
      Body.Writer.close request_body;
      let (_ : string) = read_head server_end in
      send server_end "HTTP/1.1 200 OK\r\ncontent-le";
      Miou_unix.sleep 0.05;
      Ache_miou.Client.shutdown client;
      Alcotest.(check bool)
        "peer sees the end of input"
        true
        (at_end_of_input server_end);
      Miou_unix.close client_end;
      Miou_unix.close server_end));
  Alcotest.(check int) "one error" 1 (List.length !errors)

let test_upgrade_with_read_pending () =
  with_server (fun addr ->
    let socket = connect addr in
    send
      socket
      "GET /upgrade-later HTTP/1.1\r\n\
       host: localhost\r\n\
       connection: upgrade\r\n\
       upgrade: echo\r\n\
       \r\n";
    Miou_unix.sleep 0.05;
    send socket "ping";
    Miou_unix.sleep 0.05;
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
    Alcotest.(check string)
      "101 then the bytes sent before it"
      expected
      (take socket (String.length expected));
    shutdown_send socket;
    wait_connection_done ();
    Miou_unix.close socket)

let test_client_shutdown_with_blocked_write () =
  Miou_unix.run ~domains:0 (fun () ->
    with_timeout 5.0 (fun () ->
      let client_end, server_end =
        Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0
      in
      let client_end = Miou_unix.of_file_descr client_end
      and server_end = Miou_unix.of_file_descr server_end in
      let client = Ache_miou.Client.create_connection client_end in
      let size = 4 * 1024 * 1024 in
      let request_body =
        Ache_miou.Client.request
          client
          (Request.create
             ~headers:
               (Headers.of_list
                  [ "host", "localhost"; "content-length", string_of_int size ])
             `POST
             "/")
          ~error_handler:ignore
          ~response_handler:(fun _ _ -> Alcotest.fail "unexpected response")
      in
      Body.Writer.write_string request_body (String.make size 'a');
      Body.Writer.flush request_body ignore;
      Miou_unix.sleep 0.1;
      let from = Unix.gettimeofday () in
      Ache_miou.Client.shutdown client;
      check_elapsed "Client.shutdown" ~from ~min:0. ~max:1.;
      Alcotest.(check bool) "closed" true (Ache_miou.Client.is_closed client);
      Miou_unix.close client_end;
      Miou_unix.close server_end))

let domain_handler (_ : Unix.sockaddr) reqd =
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

let rec connect_retrying addr =
  let socket = Miou_unix.tcpv4 () in
  match Miou_unix.connect socket addr with
  | () -> socket
  | exception Unix.Unix_error (ECONNREFUSED, _, _) ->
    Miou_unix.close socket;
    Miou_unix.sleep 0.01;
    connect_retrying addr

let test_server_run_domains () =
  server_errors := [];
  Miou_unix.run ~domains:1 (fun () ->
    let port =
      let socket = Miou_unix.tcpv4 () in
      Fun.protect
        ~finally:(fun () -> Miou_unix.close socket)
        (fun () ->
           Miou_unix.bind_and_listen
             socket
             (Unix.ADDR_INET (Unix.inet_addr_loopback, 0));
           match Unix.getsockname (Miou_unix.to_file_descr socket) with
           | ADDR_INET (_, port) -> port
           | ADDR_UNIX _ -> assert false)
    in
    let addr = Unix.ADDR_INET (Unix.inet_addr_loopback, port) in
    let server =
      Miou.async (fun () ->
        Ache_miou.Server.run
          ~date:false
          ~domains:2
          ~on_error:raise
          ~request_handler:domain_handler
          ~error_handler
          addr)
    in
    Fun.protect
      ~finally:(fun () -> Miou.cancel server)
      (fun () ->
         with_timeout 10.0 (fun () ->
           let seen = Hashtbl.create 2 in
           let get () =
             let socket = connect_retrying addr in
             Fun.protect
               ~finally:(fun () -> Miou_unix.close socket)
               (fun () ->
                  send socket get_close;
                  read_all socket)
           in
           let batch () =
             List.iter
               (fun response ->
                  check_domain_response seen (Result.get_ok response))
               (Miou.await_all (List.init 16 (fun _ -> Miou.async get)))
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
  server_errors := [];
  Miou_unix.run ~domains:0 (fun () ->
    let port =
      let socket = Miou_unix.tcpv4 () in
      Fun.protect
        ~finally:(fun () -> Miou_unix.close socket)
        (fun () ->
           Miou_unix.bind_and_listen
             socket
             (Unix.ADDR_INET (Unix.inet_addr_loopback, 0));
           match Unix.getsockname (Miou_unix.to_file_descr socket) with
           | ADDR_INET (_, port) -> port
           | ADDR_UNIX _ -> assert false)
    in
    let addr = Unix.ADDR_INET (Unix.inet_addr_loopback, port) in
    let request_handler client reqd =
      let client_port =
        match client with
        | Unix.ADDR_INET (_, port) -> port
        | ADDR_UNIX _ -> assert false
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
    let server =
      Miou.async (fun () ->
        Ache_miou.Server.run
          ~date:false
          ~domains:1
          ?tcp_nodelay
          ~on_error:raise
          ~request_handler
          ~error_handler
          addr)
    in
    Fun.protect
      ~finally:(fun () -> Miou.cancel server)
      (fun () ->
         with_timeout 10.0 (fun () ->
           let socket = connect_retrying addr in
           Fun.protect
             ~finally:(fun () -> Miou_unix.close socket)
             (fun () ->
                send socket get_close;
                Alcotest.(check string)
                  "response"
                  (nodelay_response expected)
                  (read_all socket)))))

let free_port () =
  let socket = Unix.socket PF_INET SOCK_STREAM 0 in
  Fun.protect
    ~finally:(fun () -> Unix.close socket)
    (fun () ->
       Unix.bind socket (ADDR_INET (Unix.inet_addr_loopback, 0));
       match Unix.getsockname socket with
       | ADDR_INET (_, port) -> port
       | ADDR_UNIX _ -> assert false)

let accept_error_server_env = "ACHE_MIOU_TEST_ACCEPT_ERROR_PORT"

let accept_error_server port =
  let seen = ref false in
  Miou_unix.run ~domains:0 (fun () ->
    Ache_miou.Server.run
      ~date:false
      ~domains:1
      ~on_error:(function
        | Unix.Unix_error ((EMFILE | ENFILE), "accept", _) -> seen := true
        | exn -> raise exn)
      ~request_handler:(fun _ reqd ->
        Request_descriptor.respond_with_string
          reqd
          (Response.create
             ~headers:(Headers.of_list [ "content-length", "1" ])
             `OK)
          (if !seen then "1" else "0"))
      ~error_handler
      (Unix.ADDR_INET (Unix.inet_addr_loopback, port)))

let test_server_run_accept_error () =
  let port = free_port () in
  let child =
    Unix.create_process_env
      "/bin/sh"
      [| "/bin/sh"; "-c"; "ulimit -n 64 && exec \"$0\""; Sys.executable_name |]
      (Array.append
         (Unix.environment ())
         [| Printf.sprintf "%s=%d" accept_error_server_env port |])
      Unix.stdin
      Unix.stdout
      Unix.stderr
  in
  Fun.protect
    ~finally:(fun () ->
      Unix.kill child Sys.sigkill;
      ignore (Unix.waitpid [] child : int * Unix.process_status))
    (fun () ->
       Miou_unix.run ~domains:0 (fun () ->
         with_timeout 20.0 (fun () ->
           let addr = Unix.ADDR_INET (Unix.inet_addr_loopback, port) in
           let response body =
             keep_alive
               ~idle_timeout:60.
               ("HTTP/1.1 200 OK\r\ncontent-length: 1\r\n\r\n" ^ body)
           in
           let get socket =
             send socket get_hello;
             take socket (String.length (response "0"))
           in
           let first = connect_retrying addr in
           Alcotest.(check string) "before" (response "0") (get first);
           let held = List.init 100 (fun _ -> connect addr) in
           Miou_unix.sleep 1.0;
           Alcotest.(check string)
             "a live connection is served after EMFILE"
             (response "1")
             (get first);
           List.iter Miou_unix.close held;
           let fresh = connect addr in
           Alcotest.(check string)
             "accepts again when descriptors are free"
             (response "1")
             (get fresh);
           Miou_unix.close fresh;
           Miou_unix.close first)))

exception Error_handler_failed

let test_server_run_on_error () =
  let errors = ref [] in
  Miou_unix.run ~domains:0 (fun () ->
    let addr = Unix.ADDR_INET (Unix.inet_addr_loopback, free_port ()) in
    let request_handler (_ : Unix.sockaddr) _reqd = failwith "handler failed" in
    let error_handler (_ : Unix.sockaddr) ?request:_ error start_response =
      match error with
      | `Exn _ -> raise Error_handler_failed
      | _ -> Body.Writer.close (start_response Headers.empty)
    in
    let server =
      Miou.async (fun () ->
        Ache_miou.Server.run
          ~date:false
          ~domains:1
          ~on_error:(fun exn -> errors := exn :: !errors)
          ~request_handler
          ~error_handler
          addr)
    in
    Fun.protect
      ~finally:(fun () -> Miou.cancel server)
      (fun () ->
         with_timeout 10.0 (fun () ->
           let reset = connect_retrying addr in
           send reset "GET /hello HTTP/1.1\r\nHo";
           Unix.setsockopt_optint
             (Miou_unix.to_file_descr reset)
             Unix.SO_LINGER
             (Some 0);
           Miou_unix.close reset;
           let socket = connect addr in
           send socket get_hello;
           Alcotest.(check string) "no response" "" (read_all socket);
           Miou_unix.close socket;
           let from = Unix.gettimeofday () in
           while !errors = [] && Unix.gettimeofday () -. from < 3. do
             Miou_unix.sleep 0.01
           done;
           Miou_unix.sleep 0.3)));
  match !errors with
  | [ Error_handler_failed ] -> ()
  | errors ->
    Alcotest.failf
      "on_error got [%s], expected [Error_handler_failed]"
      (String.concat "; " (List.map Printexc.to_string errors))

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
  let result = Miou.Computation.create () in
  let finish r = ignore (Miou.Computation.try_return result r : bool) in
  let writer =
    Ache_miou.Client.request
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

let test_stray_bytes_on_idle_connection () =
  let port, server = stray_server (List.map snd stray_cases) in
  List.iter
    (fun (name, _) ->
       Miou_unix.run ~domains:0 (fun () ->
         let socket = Miou_unix.tcpv4 () in
         Miou_unix.connect
           socket
           (Unix.ADDR_INET (Unix.inet_addr_loopback, port));
         let client = Ache_miou.Client.create_connection socket in
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
         Ache_miou.Client.shutdown client;
         Miou_unix.close socket);
       ())
    stray_cases;
  Alcotest.(check (list string))
    "second requests not sent"
    (List.map (fun _ -> "") stray_cases)
    (Domain.join server)

let get_target target =
  Printf.sprintf "GET %s HTTP/1.1\r\nhost: localhost\r\n\r\n" target

let slow_response_close =
  "HTTP/1.1 200 OK\r\ncontent-length: 4\r\nconnection: close\r\n\r\nslow"

let send_ignoring_reset socket s =
  try send socket s with Unix.Unix_error ((EPIPE | ECONNRESET), _, _) -> ()

let test_stop_finishes_request () =
  let stop = Miou.Computation.create () in
  with_server ~stop ~grace:2.0 (fun addr ->
    let socket = connect addr in
    send socket (get_target "/slow");
    Miou_unix.sleep 0.1;
    let from = Unix.gettimeofday () in
    resolve stop ();
    Alcotest.(check string)
      "response with connection: close, then end of input"
      slow_response_close
      (read_all socket);
    check_elapsed "response" ~from ~min:0.2 ~max:1.0;
    wait_connection_done ();
    Miou_unix.close socket)

let stop_closes_idle_connection ?idle_timeout () =
  let stop = Miou.Computation.create () in
  with_server ?idle_timeout ~stop (fun addr ->
    let socket = connect addr in
    send socket get_hello;
    let hello_response = keep_alive ?idle_timeout hello_response in
    Alcotest.(check string)
      "response"
      hello_response
      (take socket (String.length hello_response));
    let from = Unix.gettimeofday () in
    resolve stop ();
    Alcotest.(check string) "end of input" "" (read_all socket);
    wait_connection_done ();
    check_elapsed "connection closed" ~from ~min:0. ~max:0.3;
    Miou_unix.close socket)

let test_stop_idle_connection () = stop_closes_idle_connection ()

let test_stop_idle_connection_idle_timeout () =
  stop_closes_idle_connection ~idle_timeout:5.0 ()

let test_stop_grace_cancels_request () =
  let stop = Miou.Computation.create () in
  with_server ~stop ~grace:0.3 (fun addr ->
    let socket = connect addr in
    send socket (get_target "/stuck");
    Miou_unix.sleep 0.1;
    let from = Unix.gettimeofday () in
    resolve stop ();
    Alcotest.(check string) "no response" "" (read_all socket);
    wait_connection_done ();
    check_elapsed "cancelled after grace" ~from ~min:0.25 ~max:1.0;
    Miou_unix.close socket)

let test_stop_before_connection () =
  let stop = Miou.Computation.create () in
  resolve stop ();
  with_server ~stop (fun addr ->
    let socket = connect addr in
    send_ignoring_reset socket get_hello;
    Alcotest.(check string) "no response" "" (read_all socket);
    wait_connection_done ();
    Alcotest.(check int) "request handler not called" 0 !requests_seen;
    Miou_unix.close socket)

let test_stop_websocket () =
  let stop = Miou.Computation.create () in
  let ended = wait_ws_end () in
  with_server ~stop ~grace:2.0 (fun addr ->
    let socket = ws_connect addr "/ws-echo" in
    resolve stop ();
    Alcotest.(check (pair int string))
      "close frame 1001"
      (0x88, "\x03\xe9")
      (read_ws_frame socket);
    (* A drain closes at once, without waiting for the client's close frame:
       sending one could meet a closed socket and a reset. *)
    shutdown_send socket;
    Alcotest.(check string) "then end of input" "" (read_all socket);
    Alcotest.(check (result string reject))
      "session"
      (Ok "receive None")
      (await ended);
    wait_connection_done ();
    Miou_unix.close socket)

let test_stop_grace_cancels_session () =
  let stop = Miou.Computation.create () in
  ws_release := Some (Miou.Computation.create ());
  Fun.protect
    ~finally:(fun () -> ws_release := None)
    (fun () ->
       with_server ~stop ~grace:0.3 (fun addr ->
         let socket = ws_connect addr "/ws-hold" in
         let from = Unix.gettimeofday () in
         resolve stop ();
         Alcotest.(check (pair int string))
           "close frame 1001"
           (0x88, "\x03\xe9")
           (read_ws_frame socket);
         Alcotest.(check string) "then end of input" "" (read_all socket);
         wait_connection_done ();
         check_elapsed "cancelled after grace" ~from ~min:0.25 ~max:1.0;
         Miou_unix.close socket))

let test_server_run_stop () =
  server_errors := [];
  let stop = Miou.Computation.create ()
  and returned = Miou.Computation.create () in
  Miou_unix.run ~domains:1 (fun () ->
    let addr = Unix.ADDR_INET (Unix.inet_addr_loopback, free_port ()) in
    let server =
      Miou.async (fun () ->
        resolve
          returned
          (match
             Ache_miou.Server.run
               ~date:false
               ~domains:2
               ~stop
               ~grace:2.0
               ~on_error:raise
               ~request_handler
               ~error_handler
               addr
           with
          | () -> Ok ()
          | exception exn -> Error exn))
    in
    Fun.protect
      ~finally:(fun () -> Miou.cancel server)
      (fun () ->
         with_timeout 10.0 (fun () ->
           let hello_response = keep_alive ~idle_timeout:60. hello_response in
           let idle = connect_retrying addr in
           send idle get_hello;
           Alcotest.(check string)
             "idle: response"
             hello_response
             (take idle (String.length hello_response));
           let busy = connect addr in
           send busy (get_target "/slow");
           Miou_unix.sleep 0.1;
           let from = Unix.gettimeofday () in
           resolve stop ();
           Alcotest.(check string) "idle: end of input" "" (read_all idle);
           check_elapsed "idle connection closed" ~from ~min:0. ~max:0.3;
           Alcotest.(check string)
             "busy: response with connection: close"
             slow_response_close
             (read_all busy);
           (* With the default timeouts, a closing connection reads until the
              peer closes. *)
           Miou_unix.close idle;
           Miou_unix.close busy;
           Alcotest.(check (result unit reject))
             "run returns"
             (Ok ())
             (await returned);
           check_elapsed "run returned" ~from ~min:0.2 ~max:1.0;
           let socket = Miou_unix.tcpv4 () in
           Fun.protect
             ~finally:(fun () -> Miou_unix.close socket)
             (fun () ->
                match Miou_unix.connect socket addr with
                | () -> Alcotest.fail "a connection was accepted after stop"
                | exception Unix.Unix_error (ECONNREFUSED, _, _) -> ()))))

let count_body_head length =
  Printf.sprintf
    "POST /count-body HTTP/1.1\r\nhost: localhost\r\ncontent-length: %d\r\n\r\n"
    length

let test_body_trickle_times_out () =
  body_failed := false;
  with_server ~body_timeout:0.3 (fun addr ->
    let socket = connect addr in
    send socket (count_body_head 100);
    let from = Unix.gettimeofday () in
    let sender =
      Miou.async (fun () ->
        try
          for _ = 1 to 20 do
            send socket "x";
            Miou_unix.sleep 0.2
          done
        with
        | Unix.Unix_error ((EPIPE | ECONNRESET), _, _) -> ())
    in
    let output = read_all socket in
    Miou.cancel sender;
    Alcotest.(check string)
      "408"
      "HTTP/1.1 408 Request Timeout"
      (status_line output);
    check_elapsed "timed out" ~from ~min:0.3 ~max:1.5;
    wait_connection_done ();
    Miou_unix.close socket);
  Alcotest.(check bool) "on_error" true !body_failed

let test_body_progress_not_timed_out () =
  body_failed := false;
  let rounds = 6
  and size = 2100 in
  with_server ~body_timeout:0.3 (fun addr ->
    let socket = connect addr in
    send socket (count_body_head (rounds * size));
    for _ = 1 to rounds do
      Miou_unix.sleep 0.2;
      send socket (String.make size 'x')
    done;
    let text = string_of_int (rounds * size) in
    let response =
      Printf.sprintf
        "HTTP/1.1 200 OK\r\ncontent-length: %d\r\n\r\n%s"
        (String.length text)
        text
    in
    Alcotest.(check string)
      "whole body"
      response
      (take socket (String.length response));
    Miou_unix.close socket;
    wait_connection_done ());
  Alcotest.(check bool) "no on_error" false !body_failed

let test_write_timeout () =
  let closed = Miou.Computation.create () in
  stream_closed := Some closed;
  Fun.protect
    ~finally:(fun () -> stream_closed := None)
    (fun () ->
       with_server ~write_timeout:0.3 (fun addr ->
         let socket = connect addr in
         send socket (get_target "/stream-forever");
         let from = Unix.gettimeofday () in
         with_timeout 1.5 (fun () -> await closed);
         check_elapsed "flush got `Closed" ~from ~min:0.3 ~max:1.5;
         wait_connection_done ();
         Miou_unix.close socket))

let test_slow_handler_not_timed_out () =
  with_server
    ~idle_timeout:0.3
    ~head_timeout:0.3
    ~body_timeout:0.3
    ~write_timeout:0.3
    (fun addr ->
       let socket = connect addr in
       send socket (get_target "/sleep-1s");
       let response = "HTTP/1.1 200 OK\r\ncontent-length: 4\r\n\r\ndone" in
       Alcotest.(check string)
         "response"
         response
         (take socket (String.length response));
       Miou_unix.close socket;
       wait_connection_done ());
  Alcotest.(check int) "no error" 0 (List.length !server_errors)

let test_websocket_idle_timeout () =
  let ended = wait_ws_end () in
  with_server ~idle_timeout:0.3 (fun addr ->
    let socket = ws_connect addr "/ws-echo" in
    let from = Unix.gettimeofday () in
    Alcotest.(check string)
      "close frame 1001"
      "\x88\x02\x03\xe9"
      (take socket 4);
    check_elapsed "idle" ~from ~min:0.25 ~max:1.0;
    Alcotest.(check string) "then end of input" "" (read_all socket);
    Alcotest.(check (result string reject))
      "session"
      (Ok "receive None")
      (await ended);
    wait_connection_done ();
    Miou_unix.close socket)

let test_idle_timeout_none () =
  with_server
    ~head_timeout:0.1
    ~body_timeout:0.1
    ~write_timeout:0.1
    (fun addr ->
       let socket = connect addr in
       send socket get_hello;
       Alcotest.(check string)
         "first response"
         hello_response
         (take socket (String.length hello_response));
       Miou_unix.sleep 0.4;
       send socket get_hello;
       Alcotest.(check string)
         "second response"
         hello_response
         (take socket (String.length hello_response));
       Miou_unix.close socket;
       wait_connection_done ())

let hello_with_hint hint =
  "HTTP/1.1 200 OK\r\ncontent-length: 11\r\nKeep-Alive: "
  ^ hint
  ^ "\r\n\r\nhello world"

let test_keep_alive_hint () =
  with_server ~idle_timeout:3.5 (fun addr ->
    let socket = connect addr in
    send socket get_hello;
    let expected = hello_with_hint "timeout=3" in
    Alcotest.(check string)
      "hint"
      expected
      (take socket (String.length expected));
    Miou_unix.close socket);
  with_server (fun addr ->
    let socket = connect addr in
    send socket get_hello;
    Alcotest.(check string)
      "no hint without idle_timeout"
      hello_response
      (take socket (String.length hello_response));
    Miou_unix.close socket);
  let config = { Config.default with max_requests_per_connection = Some 3 } in
  with_server ~config ~idle_timeout:3.5 (fun addr ->
    let socket = connect addr in
    send socket (get_hello ^ get_hello ^ get_hello);
    Alcotest.(check string)
      "max counts down, the last response closes"
      (hello_with_hint "timeout=3, max=2"
      ^ hello_with_hint "timeout=3, max=1"
      ^ hello_response_close)
      (read_all socket);
    Miou_unix.close socket)

let test_keep_alive_idle_timer () =
  with_server ~idle_timeout:1. (fun addr ->
    let socket = connect addr in
    send socket get_hello;
    let expected = hello_with_hint "timeout=1" in
    Alcotest.(check string)
      "hint"
      expected
      (take socket (String.length expected));
    let from = Unix.gettimeofday () in
    Alcotest.(check bool) "closed" true (at_end_of_input socket);
    check_elapsed "idle timer is 1 s longer" ~from ~min:1.4 ~max:2.6;
    Miou_unix.close socket)

(* [hint_server listening ~hint] serves the requests of one connection with "ok"
   and [hint] as the Keep-Alive field, and returns how many it served and
   whether any byte came after the last one. *)
let hint_server listening ~hint =
  let socket, _ = Miou_unix.accept listening in
  Fun.protect
    ~finally:(fun () -> Miou_unix.close socket)
    (fun () ->
       let served = ref 0
       and extra = ref false in
       let response =
         "HTTP/1.1 200 OK\r\ncontent-length: 2\r\n"
         ^ (match hint with
           | Some hint -> "Keep-Alive: " ^ hint ^ "\r\n"
           | None -> "")
         ^ "\r\nok"
       in
       let byte = Bytes.create 1 in
       (* [line] holds the bytes of the current line, [pending] whether a byte
          came since the last response. *)
       let rec loop line =
         match Miou_unix.read socket byte with
         | 0 | (exception Unix.Unix_error _) -> ()
         | _ ->
           extra := true;
           (match Bytes.get byte 0 with
           | '\n' when line = "" ->
             incr served;
             extra := false;
             Miou_unix.write socket response;
             loop ""
           | '\n' -> loop ""
           | '\r' -> loop line
           | c -> loop (line ^ String.make 1 c))
       in
       loop "";
       !served, !extra)

let test_client_reuse_deadline () =
  let run ~hint ~closes =
    Miou_unix.run ~domains:0 (fun () ->
      let listening = Miou_unix.tcpv4 () in
      Miou_unix.bind_and_listen
        listening
        (Unix.ADDR_INET (Unix.inet_addr_loopback, 0));
      let addr = Unix.getsockname (Miou_unix.to_file_descr listening) in
      let server = Miou.async (fun () -> hint_server listening ~hint) in
      with_timeout 10.0 (fun () ->
        let socket = connect addr in
        let client = Ache_miou.Client.create_connection socket in
        let get () =
          request
            client
            (Request.create
               ~headers:(Headers.of_list [ "host", "localhost" ])
               `GET
               "/")
        in
        Alcotest.(check string) "first" "ok" (snd (get ()));
        Miou_unix.sleep 0.3;
        Alcotest.(check string) "reused" "ok" (snd (get ()));
        Miou_unix.sleep 1.3;
        Alcotest.(check bool)
          "closed after the reuse deadline"
          closes
          (Ache_miou.Client.is_closed client);
        if closes
        then (
          let errors = ref [] in
          let body =
            Ache_miou.Client.request
              client
              (Request.create
                 ~headers:(Headers.of_list [ "host", "localhost" ])
                 `GET
                 "/")
              ~error_handler:(fun error -> errors := error :: !errors)
              ~response_handler:(fun _ _ -> Alcotest.fail "a response")
          in
          Body.Writer.close body;
          Alcotest.(check bool)
            "Connection_closed"
            true
            (match !errors with [ `Connection_closed ] -> true | _ -> false))
        else Alcotest.(check string) "third" "ok" (snd (get ()));
        Ache_miou.Client.shutdown client;
        Miou_unix.close socket);
      let served, extra =
        match Miou.await server with Ok v -> v | Error exn -> raise exn
      in
      Alcotest.(check int) "requests served" (if closes then 2 else 3) served;
      Alcotest.(check bool) "no byte after the last request" false extra;
      Miou_unix.close listening)
  in
  run ~hint:(Some "timeout=3") ~closes:true;
  run ~hint:None ~closes:false

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
  Miou_unix.run ~domains:0 (fun () ->
    let server, client = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
    let server = Miou_unix.of_file_descr server
    and client = Miou_unix.of_file_descr client in
    let handler =
      Miou.async (fun () ->
        Ache_miou.Server.create_connection_handler
          ~config:auto_continue
          ~date:false
          ~idle_timeout:None
          ~request_handler:continue_handler
          ~error_handler
          server
          (Unix.ADDR_UNIX "socketpair"))
    in
    Fun.protect
      ~finally:(fun () ->
        Miou_unix.close server;
        Miou_unix.close client)
      (fun () ->
         with_timeout 5.0 (fun () ->
           send client continue_request;
           Alcotest.(check string)
             "100 Continue within 0.5 s"
             continue_100
             (with_timeout 0.5 (fun () ->
                take client (String.length continue_100)));
           send client (String.make continue_size 'x');
           Alcotest.(check string)
             "the whole body was read"
             continue_final
             (take client (String.length continue_final));
           shutdown_send client);
         match Miou.await handler with Ok () -> () | Error exn -> raise exn))

let () =
  Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
  Option.iter
    (fun port ->
       accept_error_server (int_of_string port);
       exit 0)
    (Sys.getenv_opt accept_error_server_env);
  Alcotest.run
    "ache-miou loopback"
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
        ; "101 upgrade to an echo protocol", `Quick, test_upgrade
        ; ( "CONNECT tunnel gets the bytes sent with the head"
          , `Quick
          , test_connect_tunnel_bytes_after_head )
        ; ( "an incomplete token sent with the upgrade request reaches the new \
             protocol"
          , `Quick
          , test_upgrade_incomplete_token_reaches_new_protocol )
        ; "peer closes mid-body", `Quick, test_eof_mid_body
        ; ( "a request followed by a half-close gets its response"
          , `Quick
          , test_half_close_gets_response )
        ; ( "flush calls back with `Closed after the peer closes"
          , `Quick
          , test_flush_after_peer_closed )
        ; ( "WebSocket server sending to a reset peer ends the connection"
          , `Quick
          , test_websocket_peer_reset )
        ; "WebSocket echo", `Quick, test_websocket_echo
        ; "WebSocket server close", `Quick, test_websocket_server_close
        ; ( "WebSocket close timeout: the client never answers"
          , `Quick
          , test_websocket_close_timeout )
        ; ( "WebSocket close timeout: silent client, no idle timeout"
          , `Quick
          , test_websocket_close_timeout_silent ?idle_timeout:None )
        ; ( "WebSocket close timeout: silent client, idle timeout 30 s"
          , `Quick
          , test_websocket_close_timeout_silent ~idle_timeout:30. )
        ; ( "WebSocket close timeout: pong after close, then a silent client"
          , `Quick
          , test_websocket_close_timeout_pong_then_silent )
        ; ( "WebSocket close timeout: no linger after it"
          , `Quick
          , test_websocket_close_timeout_no_linger )
        ; "WebSocket ping", `Quick, test_websocket_ping
        ; ( "WebSocket send loop to a reset peer ends"
          , `Quick
          , test_websocket_send_loop_peer_reset )
        ; ( "WebSocket receive backpressure"
          , `Quick
          , test_websocket_receive_backpressure )
        ; ( "WebSocket cancelled receive"
          , `Quick
          , test_websocket_cancelled_receive )
        ; ( "WebSocket cancelled receive then close"
          , `Quick
          , test_websocket_cancelled_receive_then_close )
        ; ( "WebSocket cancelled receives bounded"
          , `Quick
          , test_websocket_cancelled_receives_bounded )
        ; "WebSocket ping timeout", `Quick, test_websocket_ping_timeout
        ; ( "WebSocket session that returns closes 1000"
          , `Quick
          , test_websocket_session_return_closes )
        ; ( "WebSocket session exception"
          , `Quick
          , test_websocket_session_exception )
        ; ( "WebSocket upgrades of two waiting handlers"
          , `Quick
          , test_websocket_waiting_handlers )
        ; ( "WebSocket session cancelled with the connection"
          , `Quick
          , test_websocket_session_cancelled_with_connection )
        ; ( "WebSocket upgrade outside a server"
          , `Quick
          , test_websocket_upgrade_outside_server )
        ; ( "normal close with trailing bytes"
          , `Quick
          , test_close_with_trailing_bytes )
        ; "request handler exception", `Quick, test_handler_exception
        ; "late error response", `Quick, test_late_error_response
        ; ( "cancelling the connection task is not an error"
          , `Quick
          , test_cancel_is_not_an_error )
        ; ( "without a timeout the handler returns while the peer stays open"
          , `Quick
          , test_no_linger_without_timeouts )
        ; ( "lingering is bounded by idle_timeout"
          , `Quick
          , test_linger_bounded_by_idle_timeout )
        ; ( "without idle_timeout lingering is bounded"
          , `Quick
          , test_linger_bounded_without_idle_timeout )
        ; ( "lingering ends when the peer closes"
          , `Quick
          , test_linger_ends_at_peer_close )
        ; ( "a half-close does not cut a large response (idle_timeout)"
          , `Quick
          , test_half_close_large_response_idle_timeout )
        ; ( "a half-close does not cut a large response (head_timeout)"
          , `Quick
          , test_half_close_large_response_head_timeout )
        ; ( "input during lingering does not extend it"
          , `Quick
          , test_linger_not_renewed_by_input )
        ; ( "an exception from report_exn ends the connection"
          , `Quick
          , test_report_exn_raises )
        ; ( "Server.run with 2 domains serves, from both on Linux"
          , `Quick
          , test_server_run_domains )
        ; ( "Server.run: TCP_NODELAY off by default"
          , `Quick
          , test_server_run_tcp_nodelay false )
        ; ( "Server.run ~tcp_nodelay:true"
          , `Quick
          , test_server_run_tcp_nodelay ~tcp_nodelay:true true )
        ; ( "Server.run keeps serving after EMFILE on accept"
          , `Quick
          , test_server_run_accept_error )
        ; ( "Server.run gives an error_handler exception to on_error"
          , `Quick
          , test_server_run_on_error )
        ] )
    ; ( "graceful stop"
      , [ ( "a request in flight finishes with connection: close"
          , `Quick
          , test_stop_finishes_request )
        ; "an idle connection closes at once", `Quick, test_stop_idle_connection
        ; ( "an idle connection closes at once: idle_timeout"
          , `Quick
          , test_stop_idle_connection_idle_timeout )
        ; ( "a request still running after grace is cancelled"
          , `Quick
          , test_stop_grace_cancels_request )
        ; ( "a connection after stop is not served"
          , `Quick
          , test_stop_before_connection )
        ; "a WebSocket gets close 1001", `Quick, test_stop_websocket
        ; ( "a WebSocket session is cancelled after grace"
          , `Quick
          , test_stop_grace_cancels_session )
        ; ( "Server.run stops accepting, drains, and returns"
          , `Quick
          , test_server_run_stop )
        ] )
    ; ( "keep-alive"
      , [ "Keep-Alive hint", `Quick, test_keep_alive_hint
        ; ( "the idle timer runs 1 s longer than the hint"
          , `Quick
          , test_keep_alive_idle_timer )
        ; ( "client: not reused after the Keep-Alive deadline"
          , `Quick
          , test_client_reuse_deadline )
        ] )
    ; ( "timeouts"
      , [ "idle connection is closed", `Quick, test_idle_timeout
        ; ( "connection idle between requests is closed"
          , `Quick
          , test_idle_timeout_between_requests )
        ; "incomplete head gets 408 Request Timeout", `Quick, test_head_timeout
        ; ( "head timeout counts from the first byte of the head"
          , `Quick
          , test_head_deadline )
        ; "slow request body is not cut", `Quick, test_slow_body_not_cut
        ; "no timeout configured", `Quick, test_no_timeout_configured
        ; ( "unread body after a closing response: response, then the end"
          , `Quick
          , test_unread_body_closing_response )
        ; ( "unread body after the response: body_timeout"
          , `Quick
          , test_unread_body_body_timeout )
        ; ( "unread body past 256 KiB closes the connection"
          , `Quick
          , test_unread_body_drain_limit )
        ; ( "response while a body read waits: body_timeout"
          , `Quick
          , test_respond_while_body_read_pending )
        ; ( "response while a body read waits: body_timeout, idle_timeout"
          , `Quick
          , test_respond_while_body_read_pending_idle_timeout )
        ; ( "response while a body read waits, body on_error raises"
          , `Quick
          , test_body_on_error_raises )
        ; ( "response while a body read waits, body on_error raises: \
             idle_timeout"
          , `Quick
          , test_body_on_error_raises_idle_timeout )
        ; ( "the body clock restarts for each request"
          , `Quick
          , test_body_read_stop_deadlines_independent )
        ; ( "a body that trickles 1 byte at a time gets 408"
          , `Quick
          , test_body_trickle_times_out )
        ; ( "a body sent at 2048 bytes per wait is not timed out"
          , `Quick
          , test_body_progress_not_timed_out )
        ; ( "a client that stops reading is closed after write_timeout"
          , `Quick
          , test_write_timeout )
        ; ( "a slow handler that reads no body is not timed out"
          , `Quick
          , test_slow_handler_not_timed_out )
        ; ( "an idle WebSocket gets close 1001 after idle_timeout"
          , `Quick
          , test_websocket_idle_timeout )
        ; ( "~idle_timeout:None turns the idle timeout off"
          , `Quick
          , test_idle_timeout_none )
        ; ( "when the timeout arguments are checked"
          , `Quick
          , test_timeout_check_timing )
        ; ( "a pipelined head gets its own deadline"
          , `Quick
          , test_pipelined_head_deadline )
        ; ( "input after a head timeout is not handled"
          , `Quick
          , test_head_timeout_then_data )
        ; ( "a request after the idle timeout is not handled"
          , `Quick
          , test_idle_timeout_then_request )
        ; ( "Client.shutdown with a read pending"
          , `Quick
          , test_client_shutdown_with_pending_read )
        ; ( "Client.shutdown with a write blocked"
          , `Quick
          , test_client_shutdown_with_blocked_write )
        ; ( "bytes sent before a delayed 101 reach the new protocol"
          , `Quick
          , test_upgrade_with_read_pending )
        ; ( "keep-alive requests do not pile up idle timers"
          , `Quick
          , test_idle_timer_bounded )
        ; ( "requests with split heads do not pile up head timers"
          , `Quick
          , test_head_timer_bounded )
        ; ( "idle timeout fires after a connection was cancelled"
          , `Quick
          , test_idle_timeout_after_cancelled_connection )
        ; ( "closed connections do not pile up timers"
          , `Quick
          , test_closed_connection_timers_bounded )
        ; ( "keep-alive connections closed while idle do not pile up timers"
          , `Quick
          , test_idle_connection_timers_bounded )
        ] )
    ]
