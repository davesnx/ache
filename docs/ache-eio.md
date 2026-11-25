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
  | error ->
    Body.Writer.write_string
      body
      (Status.to_string (Server_connection.error_status error)));
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
  | error ->
    Body.Writer.write_string
      body
      (Status.to_string (Server_connection.error_status error)));
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

## Timeouts

The server entry points (`Server.create_connection_handler`, `Server.run`
and the TLS handler) take four timeouts, in seconds, of type
`float option`. Each is on by default; `None` turns it off:

| Argument | Default | What it limits |
| --- | --- | --- |
| `?idle_timeout` | `Some 60.` | A connection that waits for its next request, or a WebSocket that receives nothing. |
| `?head_timeout` | `Some 30.` | A request head, from its first byte. The client gets 408. |
| `?body_timeout` | `Some 60.` | A request body that makes no progress while the connection waits for it. |
| `?write_timeout` | `Some 60.` | A write that makes no progress, for example to a client that stopped reading. |

The body and write limits count progress, not single reads or writes. The
body clock runs only while the connection waits for body bytes; it starts
again from zero with each request and after each 2048 bytes. A body that
trickles in a byte at a time times out even when each byte comes before
the limit. When the clock reaches the limit, the body's `on_error` runs,
and the client gets 408 when no response has started; otherwise the
connection closes. The write clock runs while a write waits; it starts
again when the write is done and after each 2048 bytes written. When it
reaches the limit, the write is cancelled, a `flush` in progress gets
`` `Closed ``, and the connection closes.

While the request handler runs and the connection waits for nothing from
the client, no limit applies: a slow handler is not timed out.

With `idle_timeout` of 1 s or more, each response after which the
connection stays open carries `Keep-Alive: timeout=n` (`n` is
`idle_timeout` rounded down, with `, max=m` when
`Config.max_requests_per_connection` is set), and the server waits
`idle_timeout + 1` seconds for the next request, 1 s more than it
announced. A WebSocket connection keeps `idle_timeout` itself. The client
reads the hint: `Client` does not reuse a connection `n - 2` seconds after
such a response. One fiber closes it then, and a `request` after that time
gets `` `Connection_closed `` without writing anything. See
[Keep-Alive](ache.md#keep-alive).

Timeouts use the system clock. `?clock` replaces it, for example with a mock
clock in tests. With TLS, the handshake must end within the smaller of
`idle_timeout` and `head_timeout`.

```ocaml
Ache_eio.Server.run
  ~idle_timeout:(Some 5.)
  ~body_timeout:None
  ~on_error ~net ~domain_mgr ~request_handler ~error_handler addr
```

## Graceful stop

`Server.run`, `Server.create_connection_handler` and the TLS handler take
`?stop:unit Eio.Promise.t` and `?grace:float`. When `stop` is resolved:

1. `run` stops accepting in every domain and closes its listening sockets.
   Connections that the kernel queued and `run` did not accept are reset.
2. Each connection calls `Ache.Server_connection.drain` in a fiber of its
   own. An idle connection closes at once. A connection with a request in
   progress serves it, with `Connection: close` if its head is not written
   yet, and then closes; requests pipelined after it are not served. A
   WebSocket connection sends a close frame with code 1001.
3. `grace` seconds after `stop`, each connection that is still open is
   cancelled, with its reads, writes and WebSocket sessions, and its handler
   returns. Without `grace` there is no limit.
4. `run` returns when every connection in every domain has ended.

A connection that closes still lingers as after any closing response (see
[Linger](#linger)): it drops input until the peer closes, for at most
`idle_timeout` (2 s without one), and `grace` cuts that short too. A handler
applied after `stop` was resolved closes the connection without reading a
request, and the TLS handler starts no handshake; a TLS handshake in
progress at `stop` is cancelled. With `create_connection_handler` on
`Eio.Net.run_server`, give the same promise to the handler and to the
`~stop` of `run_server`, which stops accepting.

Stop on `SIGTERM`. `Eio.Condition.broadcast` is safe in a signal handler;
`Eio.Promise.resolve` is not:

```ocaml
let () =
  Eio_main.run @@ fun env ->
  let stop, resolve_stop = Eio.Promise.create () in
  let terminated = Eio.Condition.create () in
  Sys.set_signal
    Sys.sigterm
    (Signal_handle (fun _ -> Eio.Condition.broadcast terminated));
  Eio.Fiber.both
    (fun () ->
      Eio.Condition.await_no_mutex terminated;
      Eio.Promise.resolve resolve_stop ())
    (fun () ->
      Ache_eio.Server.run
        ~stop
        ~grace:10.
        ~on_error:(Eio.traceln "connection error: %a" Fmt.exn)
        ~net:(Eio.Stdenv.net env)
        ~domain_mgr:(Eio.Stdenv.domain_mgr env)
        ~request_handler
        ~error_handler
        (`Tcp (Eio.Net.Ipaddr.V4.loopback, 8080)))
```

Each connection waits for `stop` in a fiber that ends with the connection,
so a `stop` that stays pending for the life of the server does not grow
with the number of connections served.

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
  connection, or at most `idle_timeout` later.
- A WebSocket that receives nothing for `idle_timeout` gets a close frame
  with code 1001 and closes; `receive` returns `None`.
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

- A read with a timeout goes through the reader fiber. When the timeout
  fires, the read loop stops waiting, but the read goes on: tls-eio treats
  a cancelled read as a permanent error, and every later operation on that
  connection fails.
- The client always reads through the reader fiber, so `Client.shutdown` can
  stop the wait without cancelling the read.
- With `?stop`, a read that waits for a new request goes through the
  reader fiber, also without `idle_timeout`, so that the drain at `stop`
  can end the wait.
- Every other read is direct: the read loop calls `single_read` itself, for
  example with all four timeouts off, in a tunnel after `CONNECT`, or after
  an error response.

A direct read saves two fiber switches and an `Eio.Condition` handoff for
each read, and the objects of one read die young. In alternating runs of a
hello-world server against cohttp-eio, direct reads made ache-eio about 19 %
faster on one core and about 27 % faster on 4 domains.

### The watchdog

A timer for each read or write would cost a fiber and a timer each time.
Instead, each connection has one watchdog fiber that sleeps until the
nearest deadline of its read or its write. A read or write that ends in
time only clears its deadline; a deadline that moves later does not wake
the watchdog, which sleeps again when it wakes and finds no deadline
passed. On a keep-alive connection it wakes about once per `idle_timeout`.
When a read deadline passes, the watchdog wakes the read loop, which calls
the core's timeout action. When a write deadline passes, it cancels the
write (`Eio.Cancel`), and the connection closes. On one core, a hello-world
server with the default timeouts serves about 25 % fewer requests per
second than with all four off, mostly for the reader fiber.

A write that tls-eio was in the middle of leaves the TLS session in an
error state. The connection then sends no `close_notify`, and later reads
end as the end of input.

### Linger

When the read loop gets `Close` (after a response that closes the
connection), it does not close at once. It reads and drops input until
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
client accept any `_ Eio.Flow.two_way`; `ache-eio-tls` gives TLS. The
server's timeouts are on by default (see [Timeouts](#timeouts)). When the
server stops reading (after a response that closes the connection) it reads
and drops input until the peer closes, for at most `idle_timeout` (2 s
without one), so that the peer can read the response before the connection
is reset.

The server adds a `Date` field with the current time to each response that
is not 1xx and has none (RFC 9110 §6.6.1). The text is formatted once per
second in each domain. `~date:false` on `create_connection_handler`, `run`
or the TLS handler turns it off.

## TLS

To make the test certificates, and for the TLS notes, see
[TLS in ache.md](ache.md#tls).

An Eio server. The handler does the handshake in the connection fiber; when
it fails, it raises and `run_server` gives the exception to `on_error`. The
time limit of the handshake is under [Timeouts](#timeouts).

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
  | error ->
    Body.Writer.write_string
      body
      (Status.to_string (Server_connection.error_status error)));
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
