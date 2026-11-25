# ache-eio

ache on Eio. The protocol behavior, the general limits and the TLS notes
are in [ache.md](ache.md).

## Quick start

A server that answers every request with `Hello, world!`. It takes the port
as its first argument. ache adds the `Content-Length` field.

<!-- example: examples/eio/eio_hello_server.ml -->
```ocaml
open Ache

let request_handler _client_address reqd =
  let body = "Hello, world!\n" in
  let headers = Headers.of_list [ "content-type", "text/plain" ] in
  Request_descriptor.respond_with_string
    reqd
    (Response.create ~headers `OK)
    body

let error_handler _client_address ?request:_ error start_response =
  let body = start_response Headers.empty in
  (match error with
  | `Exn exn -> Body.Writer.write_string body (Printexc.to_string exn)
  | `Bad_request _ ->
    Body.Writer.write_string body (Status.default_reason_phrase `Bad_request)
  | ( #Status.server_error
    | `Uri_too_long | `Request_header_fields_too_large | `Request_timeout ) as
    status ->
    Body.Writer.write_string body (Status.default_reason_phrase status));
  Body.Writer.close body

let () =
  let port = int_of_string Sys.argv.(1) in
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let socket =
    Eio.Net.listen
      ~sw
      ~backlog:128
      ~reuse_addr:true
      (Eio.Stdenv.net env)
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, port))
  in
  Printf.printf "Listening on 127.0.0.1:%d\n%!" port;
  Eio.Net.run_server
    socket
    ~on_error:(Eio.traceln "connection error: %a" Fmt.exn)
    (Ache_eio.Server.create_connection_handler ~request_handler ~error_handler)
```

A client that sends `GET /` to that server and prints the response. An
HTTP/1.1 request must have a `Host` field; without it the server answers 400.

<!-- example: examples/eio/eio_hello_client.ml -->
```ocaml
open Ache

let error_handler error =
  let message =
    match error with
    | `Malformed_response message -> message
    | `Invalid_response_body_length _ -> "invalid response body length"
    | `Connection_closed -> "connection closed before the response"
    | `Exn exn -> Printexc.to_string exn
  in
  prerr_endline ("request failed: " ^ message);
  exit 1

let () =
  let port = int_of_string Sys.argv.(1) in
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let flow =
    Eio.Net.connect
      ~sw
      (Eio.Stdenv.net env)
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, port))
  in
  let client = Ache_eio.Client.create_connection ~sw flow in
  let finished, resolve_finished = Eio.Promise.create () in
  let on_error () =
    Eio.Promise.resolve_error
      resolve_finished
      (Failure "response body cut short")
  in
  let response_handler response body =
    Format.printf "%a@." Response.pp_hum response;
    let rec on_read buffer ~off ~len =
      print_string (Ache.Bigstring.substring buffer ~off ~len);
      Body.Reader.schedule_read body ~on_read ~on_eof ~on_error
    and on_eof () = Eio.Promise.resolve_ok resolve_finished () in
    Body.Reader.schedule_read body ~on_read ~on_eof ~on_error
  in
  let headers =
    Headers.of_list [ "host", Printf.sprintf "127.0.0.1:%d" port ]
  in
  let request_body =
    Ache_eio.Client.request
      client
      (Request.create ~headers `GET "/")
      ~error_handler
      ~response_handler
  in
  Body.Writer.close request_body;
  Eio.Promise.await_exn finished;
  Eio.Promise.await (Ache_eio.Client.shutdown client)
```

```console
$ dune exec examples/eio/eio_hello_server.exe -- 8080 &
Listening on 127.0.0.1:8080
$ dune exec examples/eio/eio_hello_client.exe -- 8080
((version "HTTP/1.1") (status 200) (reason "OK") (headers (("content-type" "text/plain")
                                                          ("content-length" "14"))))
Hello, world!
```

## Serving on several cores

`Ache_eio.Server.run` runs one accept loop in each of `~domains` domains
(by default `Domain.recommended_domain_count ()`). Each domain listens on
its own socket with `SO_REUSEPORT`, so the domains share no socket and no
state. On Linux, the kernel spreads connections between these sockets. On
macOS and FreeBSD, `SO_REUSEPORT` does not spread TCP connections: one
socket gets them, so `run` works but only one domain accepts. The handlers
run in several domains at the same time. The address must have a fixed
port.
With `~tcp_nodelay:true`, `run` sets `TCP_NODELAY` on each accepted
socket, so that small writes are not held back by Nagle's algorithm. It is
off by default.

<!-- example: examples/eio/eio_multicore_server.ml -->
```ocaml
open Ache

let request_handler _client_address reqd =
  let body = "Hello, world!\n" in
  let headers = Headers.of_list [ "content-type", "text/plain" ] in
  Request_descriptor.respond_with_string
    reqd
    (Response.create ~headers `OK)
    body

let error_handler _client_address ?request:_ error start_response =
  let body = start_response Headers.empty in
  (match error with
  | `Exn exn -> Body.Writer.write_string body (Printexc.to_string exn)
  | `Bad_request _ ->
    Body.Writer.write_string body (Status.default_reason_phrase `Bad_request)
  | ( #Status.server_error
    | `Uri_too_long | `Request_header_fields_too_large | `Request_timeout ) as
    status ->
    Body.Writer.write_string body (Status.default_reason_phrase status));
  Body.Writer.close body

let () =
  let port = int_of_string Sys.argv.(1) in
  let domains = int_of_string Sys.argv.(2) in
  Eio_main.run @@ fun env ->
  Printf.printf "Listening on 127.0.0.1:%d with %d domains\n%!" port domains;
  Ache_eio.Server.run
    ~domains
    ~on_error:(Eio.traceln "connection error: %a" Fmt.exn)
    ~net:(Eio.Stdenv.net env)
    ~domain_mgr:(Eio.Stdenv.domain_mgr env)
    ~request_handler
    ~error_handler
    (`Tcp (Eio.Net.Ipaddr.V4.loopback, port))
```

## WebSocket

`Ache_eio.Websocket` gives the WebSocket server of
[ache.md](ache.md#websocket) a direct-style API. Call `upgrade` in the
request handler with the function that runs the session:

```ocaml
open Ache

let rec echo ws =
  match Ache_eio.Websocket.receive ws with
  | None -> ()
  | Some (Text "bye") -> Ache_eio.Websocket.close ~reason:"bye" ws 1000
  | Some (Text message) ->
    Ache_eio.Websocket.send_text ws message;
    echo ws
  | Some (Binary message) ->
    Ache_eio.Websocket.send_binary ws message;
    echo ws

let request_handler _client_address reqd =
  match Ache_eio.Websocket.upgrade reqd echo with
  | Ok () -> ()
  | Error _ ->
    let headers = Headers.of_list [ "content-length", "0" ] in
    Request_descriptor.respond_with_string
      reqd
      (Response.create ~headers `Bad_request)
      ""
```

- `upgrade ?max_message_size ?headers` checks the handshake and responds with
  `101`, as `Ache.Websocket.upgrade` does; `headers` answers a subprotocol
  (see [Choose a subprotocol](ache.md#choose-a-subprotocol)). On `Error _` it responds nothing: send the
  response yourself, for example `426` with `Sec-WebSocket-Version: 13` for
  `` Error `Unsupported_version ``. Call it in the request handler of
  `Server.create_connection_handler`, `Server.run` or
  `Ache_eio_tls.Server`, or in a fiber forked from it; elsewhere it raises
  `Invalid_argument`.
- The session runs in its own fiber of the connection, on the domain that
  runs the connection, after `upgrade` has returned. The connection handler
  returns when the connection has ended and the session has returned. The
  session is not cancelled when the socket closes: `receive` then returns
  `None`, and the other functions return or raise
  `Ache_eio.Websocket.Closed`, so the session ends. It is cancelled when the
  connection's fiber is, for example when the server stops.
- A session that raises `Closed` ends normally. When it raises another
  exception, the connection sends a close frame with code `1011`, and the
  handler raises the exception after the connection ends, to the `on_error`
  of `Eio.Net.run_server` or `Server.run`.
- `receive` waits for the next whole message, `Text s` or `Binary s`. Once
  the connection has ended and no message waits, it returns `None`. The
  connection ends after the client's close frame, the end of the input, a
  reset, a protocol error, or your `close`. `client_close ws` then gives the
  client's close code and reason, if it sent a valid close frame.
- `send_text` and `send_binary` return when the frame is written. They raise
  `Closed` when the connection is gone, also when it was gone before the
  call, so a send loop ends when the client leaves.
- `ping ws payload` sends a ping and returns when the next pong arrives, or
  raises `Closed` when the connection ends first. To find a client that is
  gone: `Eio.Time.with_timeout_exn clock 30. (fun () ->
  Ache_eio.Websocket.ping ws "")`, and `close` on `Eio.Time.Timeout`. The
  pong comes after the messages the client sent before it, so keep calling
  `receive` in another fiber meanwhile: while 8 messages wait, reading is
  paused and the pong is not seen.
- `close ?reason ws code` sends a close frame and returns when the frame is
  written or the connection has ended. It raises `Invalid_argument` at once
  for a code that an endpoint may not send, or a reason that is not UTF-8 or
  longer than 123 bytes. The socket closes when the read in progress ends
  too: when the client answers with its close frame or closes the
  connection. There is no timeout for a client that does neither.
- Messages that you have not received wait in a queue. When 8 messages wait,
  the connection stops reading from the client until `receive` takes one.
  Frames that one read already holds still join the queue.
- You can cancel a `receive`, for example with `Eio.Fiber.first` or
  `Eio.Time.with_timeout`. No message is lost: the next `receive` gets it.

With the callbacks of `Ache.Websocket` instead, as in [WebSocket in
ache.md](ache.md#websocket), the callbacks run in the read and write loops
of the connection. The connection has no lock: call `send_text`, `flush`,
`close`, `pause_reading` and `resume_reading` from the domain that runs the
connection. `adapters/eio/test/test_ache_eio.ml` has a server that sends in a
loop on `flush` and stops at `` `Closed ``.

## How it works

This continues [How it works](ache.md#how-it-works) in ache.md, which
explains the core and the read and write loops.

### Eio

An Eio program runs in `Eio_main.run`. On Linux it uses io_uring
(`eio_linux`), elsewhere `poll` (`eio_posix`). Code runs in fibers. A fiber
that waits, for example in `Promise.await` or in a socket read, is suspended
with an effect, and the scheduler runs another fiber. A fiber switch is a
function call in user space, not a thread switch. A `Switch` owns the fibers
that are forked in it: when the switch ends, they are cancelled.

### One connection

`Server.create_connection_handler` is the function that
`Eio.Net.run_server`, or the accept loop of `Server.run`, calls in a new
fiber for each accepted socket. It
creates the core connection and runs the two loops in one switch:

```
 Eio.Net.run_server ── one fiber per accepted socket
        │
        ▼
 Switch.run
  ├─ Fiber.first ─┬─ Fiber.both ─┬─ read loop  ── Eio.Flow.single_read
  │               │              │     │           into the read buffer
  │               │              │     └─ timed read ─► reader fiber
  │               │              │                     (daemon)
  │               │              └─ write loop ── Eio.Flow.write
  │               │                                (one writev)
  │               └─ waits for [failed] ─► shut down the socket
  └─ when the switch ends: the reader fiber is cancelled
```

- **Waiting.** On `Yield`, a loop creates a promise, gives the core a
  callback that resolves it (`yield_reader` or `yield_writer`), and awaits
  it. The core resolves the promise when it has work, and the fiber goes on.
- **Writes.** The write loop sends all the buffers of one `Write` with one
  `Eio.Flow.write` on a list of `Cstruct`s. Eio makes one `writev` of it.
- **Reads.** The read loop reads with `Eio.Flow.single_read` into free space
  in the read buffer, then gives the new bytes to the core.
- **The loops call themselves in tail position.** A loop must not call itself
  inside an exception handler: each request would leave frames on the
  fiber's stack, and every minor GC scans fiber stacks.
- **Errors.** If a write fails, or the core raises in `report_exn`, the
  connection resolves `failed`. `Fiber.first` then cancels both loops, and
  the socket is shut down in both directions.
- **Errors for `on_error`.** The core catches an exception that
  `error_handler` raises and closes the connection, so the handler wraps
  `error_handler` and keeps the first exception it raises, with its
  backtrace. The loops keep the first exception that `report_exn` raises in
  the same way. When the loops end, the handler raises the kept exception,
  and `Eio.Net.run_server`, `Eio.Net.accept_fork` or `Server.run` gives it to
  `on_error`. A peer that closes or resets the connection is the end of
  input, not an error, and does not reach `on_error`. The client keeps its
  behavior: an exception from `report_exn` ends the connection and is not
  raised.

### The reader fiber

Each connection also starts a reader fiber, which reads the socket when the
read loop asks it to, and keeps the result until the read loop takes it.

- A read with a timeout goes through the reader fiber. The read loop waits
  for the result under `Eio.Time.with_timeout`. When the timeout fires, it
  cancels the wait, not the read: tls-eio treats a cancelled read as a
  permanent error, and every later operation on that connection fails.
- The client always reads through the reader fiber, so `Client.shutdown` can
  stop the wait without cancelling the read.
- A server read of a request body before its response is complete goes
  through the reader fiber, with or without `?clock`, so that the write
  loop can stop the wait when the response completes (see
  [Timeouts](ache.md#timeouts)).
- Every other read is direct: the read loop calls `single_read` itself. A
  server without `?clock` has no timeouts, so its other reads are direct.
  With a clock, the reads of pipelined requests are direct, and the reads
  that wait for a new request head use the reader fiber.

A direct read saves two fiber switches and an `Eio.Condition` handoff for
each read, and the objects of one read die young. In alternating runs of a
hello-world server against cohttp-eio, direct reads made ache-eio about 19 %
faster on one core and about 27 % faster on 4 domains.

### Linger

With a clock, when the read loop gets `Close` (after a response that closes
the connection), it does not close at once. It reads and drops input until
the peer closes, for at most `idle_timeout` (2 s without one). If the server
closed with unread input, the kernel would reset the connection, and the
peer could lose the response.

The time limit is set once, when the linger starts: input from the peer
does not extend it. If the limit ends while the response is still being
written, a new limit starts, so that the linger ends at most `idle_timeout`
after the last write. An end of input during the linger means that the peer
stopped sending (a half-close). The linger then ends without an error, and
the response continues to the end.

### Several domains

`Server.run` starts the same handler on several domains. Each domain opens
its own listening socket on the same port with `SO_REUSEPORT` and runs its
own accept loop. The first domain is the caller's; the others start
with `Eio.Domain_manager.run`, and `Fiber.all` waits for all of them. The
domains share no state, so a connection stays on the domain that accepted
it.

The accept loop is not `Eio.Net.run_server`, because an accept error would
end `run_server`, and `Fiber.all` would then cancel every domain and its
connections. Most accept errors go away when the cause goes away, for example
`EMFILE` or `ENFILE` (too many open files) or `ECONNABORTED`. The loop gives
such an error to `on_error`, waits 50 ms, and accepts again. Only errors
that show the listening socket itself is not usable (`EBADF`, `EINVAL`,
`ENOTSOCK`, `EFAULT`, or an operation that is not available) end the loop.

## Runtime notes

`Ache_eio.Server.create_connection_handler ~request_handler
~error_handler` can be given to `Eio.Net.run_server`. The server and the
client accept any `_ Eio.Flow.two_way`; `ache-eio-tls` gives TLS. Timeouts
need `?clock`. With a clock, when the server stops reading (after a
response that closes the connection) it reads and drops input until the
peer closes, for at most `idle_timeout` (2 s without
one), so that the peer can read the response before the connection is
reset. Without a clock it closes at once.

## TLS

To make the test certificates, and for the TLS notes, see
[TLS in ache.md](ache.md#tls).

An Eio server. The handler does the handshake in the connection fiber; when
it fails, it raises and `run_server` gives the exception to `on_error`. The
time limit of the handshake is under "Timeouts" below.

<!-- example: examples/eio/eio_tls_hello_server.ml -->
```ocaml
open Ache

let request_handler _client_address reqd =
  let body = "Hello, TLS!\n" in
  let headers = Headers.of_list [ "content-type", "text/plain" ] in
  Request_descriptor.respond_with_string
    reqd
    (Response.create ~headers `OK)
    body

let error_handler _client_address ?request:_ error start_response =
  let body = start_response Headers.empty in
  (match error with
  | `Exn exn -> Body.Writer.write_string body (Printexc.to_string exn)
  | `Bad_request _ ->
    Body.Writer.write_string body (Status.default_reason_phrase `Bad_request)
  | ( #Status.server_error
    | `Uri_too_long | `Request_header_fields_too_large | `Request_timeout ) as
    status ->
    Body.Writer.write_string body (Status.default_reason_phrase status));
  Body.Writer.close body

let () =
  let port = int_of_string Sys.argv.(1) in
  let cert = Sys.argv.(2)
  and key = Sys.argv.(3) in
  Mirage_crypto_rng_unix.use_default ();
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let certificate =
    X509_eio.private_of_pems
      ~cert:Eio.Path.(fs / cert)
      ~priv_key:Eio.Path.(fs / key)
  in
  let tls =
    match Tls.Config.server ~certificates:(`Single certificate) () with
    | Ok tls -> tls
    | Error (`Msg message) -> failwith message
  in
  Eio.Switch.run @@ fun sw ->
  let socket =
    Eio.Net.listen
      ~sw
      ~backlog:128
      ~reuse_addr:true
      (Eio.Stdenv.net env)
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, port))
  in
  Printf.printf "Listening on https://127.0.0.1:%d\n%!" port;
  Eio.Net.run_server
    socket
    ~on_error:(Eio.traceln "connection error: %a" Fmt.exn)
    (Ache_eio_tls.Server.create_connection_handler
       ~tls
       ~request_handler
       ~error_handler)
```

An Eio client that trusts the test CA. A client for public servers uses the
system trust anchors, for example `Ca_certs.authenticator ()` from the
`ca-certs` package. `~peer` is required: `` `Host name `` or `` `Ip address ``
is the identity that the certificate must prove. `` `Insecure_no_name_check ``
accepts any certificate from a trusted CA for any name; use it only with an
authenticator that checks the identity itself. When the certificate is
rejected, `create_connection` raises
`` Tls_eio.Tls_failure (`Error (`AuthenticationFailure _)) `` (Lwt: the promise
fails with `Tls_lwt.Tls_failure` and the socket is closed).

<!-- example: examples/eio/eio_tls_hello_client.ml -->
```ocaml
open Ache

let error_handler error =
  let message =
    match error with
    | `Malformed_response message -> message
    | `Invalid_response_body_length _ -> "invalid response body length"
    | `Connection_closed -> "connection closed before the response"
    | `Exn exn -> Printexc.to_string exn
  in
  prerr_endline ("request failed: " ^ message);
  exit 1

let () =
  let port = int_of_string Sys.argv.(1) in
  let ca = Sys.argv.(2) in
  Mirage_crypto_rng_unix.use_default ();
  Eio_main.run @@ fun env ->
  let authenticator =
    X509_eio.authenticator (`Ca_file Eio.Path.(Eio.Stdenv.fs env / ca))
  in
  let tls =
    match Tls.Config.client ~authenticator () with
    | Ok tls -> tls
    | Error (`Msg message) -> failwith message
  in
  Eio.Switch.run @@ fun sw ->
  let socket =
    Eio.Net.connect
      ~sw
      (Eio.Stdenv.net env)
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, port))
  in
  let host = Domain_name.(host_exn (of_string_exn "localhost")) in
  let client =
    Ache_eio_tls.Client.create_connection ~sw ~tls ~peer:(`Host host) socket
  in
  let finished, resolve_finished = Eio.Promise.create () in
  let on_error () =
    Eio.Promise.resolve_error
      resolve_finished
      (Failure "response body cut short")
  in
  let response_handler response body =
    Format.printf "%a@." Response.pp_hum response;
    let rec on_read buffer ~off ~len =
      print_string (Ache.Bigstring.substring buffer ~off ~len);
      Body.Reader.schedule_read body ~on_read ~on_eof ~on_error
    and on_eof () = Eio.Promise.resolve_ok resolve_finished () in
    Body.Reader.schedule_read body ~on_read ~on_eof ~on_error
  in
  let headers =
    Headers.of_list [ "host", Printf.sprintf "localhost:%d" port ]
  in
  let request_body =
    Ache_eio_tls.Client.request
      client
      (Request.create ~headers `GET "/")
      ~error_handler
      ~response_handler
  in
  Body.Writer.close request_body;
  Eio.Promise.await_exn finished;
  Eio.Promise.await (Ache_eio_tls.Client.shutdown client)
```

```console
$ dune exec examples/eio/eio_tls_hello_server.exe -- 8443 /tmp/server.pem /tmp/server.key &
Listening on https://127.0.0.1:8443
$ dune exec examples/eio/eio_tls_hello_client.exe -- 8443 /tmp/ca.pem
((version "HTTP/1.1") (status 200) (reason "OK") (headers (("content-type" "text/plain")
                                                          ("content-length" "12"))))
Hello, TLS!
$ curl --cacert /tmp/ca.pem https://127.0.0.1:8443/
Hello, TLS!
```
