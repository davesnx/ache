# ache-miou

ache on Miou. The protocol behavior, the general limits and the TLS notes
are in [ache.md](ache.md).

## Quick start

The same server and client as in [ache-eio](ache-eio.md#quick-start), with
`ache-miou`. As with Lwt, the application
must ignore `SIGPIPE`. The server runs each connection in its own task, made
with `Miou.async ~orphans`, closes the socket when the handler returns, and
collects ended tasks with `Miou.care`. The client calls `Client.shutdown`
from the task that called `Client.create_connection`, because Miou requires
a task to await the tasks it created.

<!-- example: examples/miou/miou_hello_server.ml -->
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

let rec clean_up orphans =
  match Miou.care orphans with
  | None | Some None -> ()
  | Some (Some connection) ->
    (match Miou.await connection with
    | Ok () -> ()
    | Error exn -> prerr_endline ("connection error: " ^ Printexc.to_string exn));
    clean_up orphans

let () =
  Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
  let port = int_of_string Sys.argv.(1) in
  let handler =
    Ache_miou.Server.create_connection_handler ~request_handler ~error_handler
  in
  Miou_unix.run @@ fun () ->
  let socket = Miou_unix.tcpv4 () in
  Miou_unix.bind_and_listen
    ~backlog:128
    socket
    Unix.(ADDR_INET (inet_addr_loopback, port));
  Printf.printf "Listening on 127.0.0.1:%d\n%!" port;
  let orphans = Miou.orphans () in
  let rec accept () =
    clean_up orphans;
    let client, client_address = Miou_unix.accept socket in
    ignore
      (Miou.async ~orphans (fun () ->
         Fun.protect
           ~finally:(fun () -> Miou_unix.close client)
           (fun () -> handler client client_address))
       : unit Miou.t);
    accept ()
  in
  accept ()
```

<!-- example: examples/miou/miou_hello_client.ml -->
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
  Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
  let port = int_of_string Sys.argv.(1) in
  Miou_unix.run @@ fun () ->
  let socket = Miou_unix.tcpv4 () in
  Miou_unix.connect socket Unix.(ADDR_INET (inet_addr_loopback, port));
  let client = Ache_miou.Client.create_connection socket in
  let finished = Miou.Computation.create () in
  let on_error () =
    ignore
      (Miou.Computation.try_cancel
         finished
         (Failure "response body cut short", Printexc.get_callstack 0)
       : bool)
  in
  let response_handler response body =
    Format.printf "%a@." Response.pp_hum response;
    let rec on_read buffer ~off ~len =
      print_string (Ache.Bigstring.substring buffer ~off ~len);
      Body.Reader.schedule_read body ~on_read ~on_eof ~on_error
    and on_eof () = ignore (Miou.Computation.try_return finished () : bool) in
    Body.Reader.schedule_read body ~on_read ~on_eof ~on_error
  in
  let headers =
    Headers.of_list [ "host", Printf.sprintf "127.0.0.1:%d" port ]
  in
  let request_body =
    Ache_miou.Client.request
      client
      (Request.create ~headers `GET "/")
      ~error_handler
      ~response_handler
  in
  Body.Writer.close request_body;
  Miou.Computation.await_exn finished;
  Ache_miou.Client.shutdown client;
  Miou_unix.close socket
```

The output is the same as with [Eio](ache-eio.md#quick-start).

## Serving on several cores

`Ache_miou.Server.run`, called inside `Miou_unix.run`, runs one accept loop
in each of `~domains` Miou domains (by default all of them: the calling
domain and the `~domains` of `Miou_unix.run`). Each domain listens on its
own socket with `SO_REUSEPORT`, so the domains share no socket and no
state. On Linux, the kernel spreads connections between these sockets. On
macOS and FreeBSD, `SO_REUSEPORT` does not spread TCP connections: one
socket gets them, so `run` works but only one domain accepts. Each
connection runs in a task of the domain that accepted it. The handlers run
in several domains at the same time. The address must have a fixed port.
With `~tcp_nodelay:true`, `run` sets `TCP_NODELAY` on each accepted
socket, so that small writes are not held back by Nagle's algorithm. It is
off by default.

`~on_error` gets the exception when the handler of a connection raises:
when `error_handler` raises, or when the core cannot handle an exception.
A peer that resets or closes the connection is not an error. When `accept`
fails because the process or the system has no free file descriptors or
memory (`EMFILE`, `ENFILE`, `ENOBUFS`, `ENOMEM`), `on_error` gets the
exception, and the loop waits 0.1 s and accepts again. For accept errors
about one connection, such as `ECONNABORTED`, `EPERM` or a network error,
`on_error` gets the exception, and the loop accepts again at once.

<!-- example: examples/miou/miou_multicore_server.ml -->
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
  Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
  let port = int_of_string Sys.argv.(1) in
  let domains = int_of_string Sys.argv.(2) in
  Miou_unix.run ~domains:(domains - 1) @@ fun () ->
  Printf.printf "Listening on 127.0.0.1:%d with %d domains\n%!" port domains;
  Ache_miou.Server.run
    ~on_error:(fun exn ->
      prerr_endline ("connection error: " ^ Printexc.to_string exn))
    ~request_handler
    ~error_handler
    Unix.(ADDR_INET (inet_addr_loopback, port))
```

## Graceful stop

`Server.run`, `Server.create_connection_handler` and the TLS
`create_connection_handler_with_fd` take `?stop:unit Miou.Computation.t`
and `?grace:float`. Resolve `stop` with `Miou.Computation.try_return`
(or cancel it) from any task of any domain. Then:

1. `run` stops accepting in each domain and closes its listening sockets.
   A connection accepted at the same moment is closed without a response,
   and so is a socket given to a handler after `stop`.
2. Each connection is drained (see [Graceful stop](ache.md#graceful-stop)
   in ache.md). An idle connection closes at once: the runtime shuts down
   reading on the socket to end the read that waits. A request in progress
   finishes and its response gets `Connection: close`. A WebSocket gets a
   close frame with code 1001. A TLS handshake in progress stops.
3. `grace` seconds after `stop`, each connection that is still open is
   cancelled with its tasks, its request handler and its WebSocket
   sessions, and the socket is shut down. Without `grace` there is no
   limit.
4. `run` returns when every connection of every domain has ended.

Each connection waits for `stop` in a task of its own, in the domain that
accepted it, so `Server_connection.drain` runs where the connection runs.
`grace` must be positive, or the handler raises `Invalid_argument`.
Without `stop`, `run` does not return; cancelling its task still stops
everything at once.

## WebSocket

`Ache_miou.Websocket` gives the WebSocket server of
[ache.md](ache.md#websocket) a direct-style API. Call `upgrade` in the
request handler with a session function. The session runs in its own task
and blocks in `receive`, `send_text` and the others:

```ocaml
open Ache

let rec echo ws =
  match Ache_miou.Websocket.receive ws with
  | None -> ()
  | Some (Text "bye") -> Ache_miou.Websocket.close ~reason:"bye" ws 1000
  | Some (Text message) ->
    Ache_miou.Websocket.send_text ws message;
    echo ws
  | Some (Binary message) ->
    Ache_miou.Websocket.send_binary ws message;
    echo ws

let request_handler _client_address reqd =
  match Ache_miou.Websocket.upgrade reqd echo with
  | Ok () -> ()
  | Error `Unsupported_version ->
    (* RFC 6455 §4.4: name the version this server speaks. *)
    let headers =
      Headers.of_list
        [ "sec-websocket-version", "13"
        ; "upgrade", "websocket"
        ; "connection", "upgrade"
        ; "content-length", "0"
        ]
    in
    Request_descriptor.respond_with_string
      reqd
      (Response.create ~headers `Upgrade_required)
      ""
  | Error (`Not_get | `Not_websocket | `Invalid_key) ->
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
  `` Error `Unsupported_version ``. Call it from the request handler of
  `Server.create_connection_handler`, `Server.run`, `Make` or
  `ache-miou-tls`, while the handler runs. Elsewhere it raises
  `Invalid_argument`.
- The session runs in a task that the connection task starts with
  `Miou.async`, in the domain of the connection. The server handler
  returns when the session returns. The connection does not cancel the
  session when the socket closes: the session sees the end, because
  `receive` returns `None` and the other functions raise `Closed` or
  return. The session is cancelled only when the connection task is.
- When the session returns, or raises `Closed`, the connection closes with
  code 1000, unless it has already ended. Another exception sends a
  close frame with code 1011, and the server handler raises it once the
  connection has ended (see `close`): with `Server.run`, it goes to
  `on_error`.
- `receive` returns the next whole message, `Text s` or `Binary s`. Once
  the connection has ended and no message waits, it returns `None`. The
  connection ends after the client's close frame, the end of the input, a
  reset, a protocol error, or your `close`. `client_close ws` gives the
  client's close code and reason once the connection has ended, if it sent
  a valid close frame; `None` does not tell an end of input, a reset or a
  protocol error apart.
- `send_text` and `send_binary` return when the frame is written. They
  raise `Ache_miou.Websocket.Closed` when the connection is gone, also when
  it was gone before the call, so a send loop ends when the client leaves.
- `ping ws payload` sends a ping and returns when the next pong arrives, or
  raises `Closed` when the connection ends first. To find a client that is
  gone, race it with a task that sleeps, with `Miou.await_first`, and
  `close` when the sleep wins. The connection waits at most
  `close_timeout` (10 s by default) for the client's close frame, then
  closes without it. The pong comes after the messages the
  client sent before it, so keep calling `receive` meanwhile: while 8
  messages wait, reading is paused and the pong is not seen.
- `close ?reason ws code` sends a close frame and returns when the
  connection has ended: the client answered with its close frame (then in
  `client_close`), closed the connection, or the connection failed.
  Messages that arrive meanwhile are dropped and do not extend the wait. A
  client that does neither for `close_timeout` (10 s by default) is cut
  off, and `client_close` is `None`. The clock starts when the close frame
  is queued, by `close` or by the end of the session. The close frame of
  the idle timeout or of a graceful stop stops reading at once, so the
  connection ends without waiting for the client's. It raises
  `Invalid_argument` at once for a code that an endpoint may not send, or a
  reason that is not UTF-8 or longer than 123 bytes.
- A session whose client sends nothing for `idle_timeout` (60 s by
  default) gets a close frame with code 1001, and `receive` returns
  `None`.
- Messages that you have not received wait in a queue. When 8 messages
  wait, the connection stops reading from the client until `receive` takes
  one. Frames that one read already holds still join the queue.
- You can cancel a task that waits in `receive` or `ping`, for example with
  `Miou.cancel` or `Miou.await_first`. A cancelled `receive` loses no
  message: the next `receive` gets it. A `receive` that has returned has
  taken its message: when `Miou.await_first` drops the result of a task
  that finished at the same time as the winner, that message is lost.
- Call these functions from the session task, or from tasks that it starts
  with `Miou.async`. The connection has no lock, so they must run in its
  domain; do not call them from a task of `Miou.call`.

To use the callbacks of `Ache.Websocket` directly, as in [WebSocket in
ache.md](ache.md#websocket): the callbacks run in the read and write loops
of the connection. The connection has no lock: call `send_text`, `flush`,
`close`, `pause_reading` and `resume_reading` from the domain that runs the
connection. `adapters/miou/test/test_ache_miou.ml` has a server that sends in
a loop on `flush` and stops at `` `Closed ``.

## How it works

This continues [How it works](ache.md#how-it-works) in ache.md, which
explains the core and the read and write loops.

### Miou

A Miou program runs in `Miou_unix.run`. Code runs in tasks. `Miou.async`
starts a task on the current domain, and `Miou.call` on another domain.
A task must be awaited or cancelled by the task that started it. A task that
waits, for example in a socket read, is suspended with an effect, and the
scheduler of that domain runs another task. Each domain has its own
scheduler, and a task stays on its domain.

### One connection

The server handler runs in a task for each accepted socket. It creates the
core connection and starts the two loops as two child tasks:

```
 accept loop ── Miou.async: one task per accepted socket
        │
        ▼
 connection task
  ├─ read task  ── Miou_unix.read into the read buffer
  │     └─ timed read ─► reader task (started on first use)
  ├─ write task ── one sendmsg for all buffers
  ├─ WebSocket session tasks (started on upgrade)
  └─ waits until both loops end, or one fails, then for the sessions
     ─► close the socket
```

- **Waiting.** On `Yield`, a loop gives the core a callback that wakes it
  (`yield_reader` or `yield_writer`), and waits on a signal. The core calls
  the callback when it has work.
- **Writes.** The write task sends all the buffers of one `Write` with one
  `sendmsg` (a C stub in `adapters/miou/ache_miou_stubs.c`). When the socket
  buffer is full, it waits until the socket can be written. On a file
  descriptor that is not a socket, it writes each buffer in turn.
- **Reads.** The read task reads with `Miou_unix.read` into free space in the
  read buffer, then gives the new bytes to the core.
- **The reader task.** As in `ache-eio`, a read with a timeout goes through
  a separate reader task, and only the wait is stopped when the timeout
  fires. The client always reads through it, so `Client.shutdown` can stop
  the wait. Every other read is direct: with the default timeouts, only
  reads in a tunnel, or with every timeout set to `None`. With
  `close_timeout`, the reads of a connection after the `101` of a
  WebSocket (`upgrade: websocket`) also go through it: when the session
  queues its close frame, the write task sets the close deadline and wakes
  the waiting read, which then waits at most until that deadline. When the
  deadline passes, the connection closes without lingering. The
  reader task
  starts only on the first read that needs it.
- **Body clock.** Before each read, the read task asks the core which
  timeout applies (`Server_connection.read_timeout`). For `` `Body ``,
  the read waits at most `body_timeout` minus the time already spent in
  body waits of this request. After the read, the time waited and the
  bytes read are added up; both start again from zero at each request and
  once 2048 bytes have arrived. When the wait runs out, the read task asks
  the core again and calls the action it gives (`on_error`, then 408 or a
  close).
- **Write clock.** Before a write, the write task starts the clock if it
  is not running; it stops it when the core has nothing more to write, and
  starts it again after each 2048 bytes written. A watchdog task sleeps
  until the clock's deadline, at most 0.5 s at a time. If the clock is
  still running then, it stops the connection as an error does: it cancels
  the read and write tasks, tells the core that the write was `Closed`,
  and shuts the socket down.
- **Timers.** A timer sleeps at most 0.5 s at a time and then checks its
  deadline again. A cancelled sleep stays in Miou's timer heap until it
  ends, so a short sleep keeps that heap small.
- **Errors.** The core gets an exception from a read, a write or a
  handler, and gives it to `error_handler` as `` `Exn ``. If
  `error_handler` raises, or the core raises when it gets the exception,
  the connection cancels both tasks and closes the socket, and the server
  handler raises that exception when it returns. With `Server.run`, the
  exception goes to `on_error`.
- **Client shutdown.** `Client.shutdown` cancels both tasks at once, so a
  write that waits for a peer that does not read does not hold it. Output
  that is not yet written is dropped.
- **Sessions.** While the request handler runs, the connection sets a
  domain-local value that `Websocket.upgrade` reads. `upgrade` adds the
  session there and wakes the connection task, which starts it with
  `Miou.async`. So the session is a child of the connection task, not of
  the read task, and can wait while the read loop runs. When both loops
  end, the connection shuts the core down, so that a waiting session sees
  the end, and awaits the sessions.
- **WebSocket waits.** Each `receive` or `ping` that waits has its own
  `Miou.Computation`. A message is queued first and then wakes one
  `receive`, which takes it from the queue: a `receive` that is cancelled
  leaves the message for the next one. A cancelled wait completes its
  computation, and the next wait drops the completed ones, so many
  cancelled calls do not pile up.
- **Graceful stop.** With `stop`, the connection task starts one more
  child task that waits for `stop`. It calls `Server_connection.drain`,
  shuts down reading on the socket when the connection is then closed, and
  sleeps `grace`. Then it marks the sessions to be cancelled and stops the
  connection as an error does: the connection task cancels the read and
  write tasks and the sessions. With `stop`, the accept loop of `run`
  waits for the listening socket or `stop`, whichever comes first, and
  accepts with a non-blocking `accept`.

### Several domains

`Server.run` starts one accept loop on each domain. Each loop opens its own
listening socket on the same port with `SO_REUSEPORT`. The first loop
starts with `Miou.async` on the caller's domain; the others start with
`Miou.call ~pin`, one for each other domain. A connection stays on the
domain that accepted it. If `on_error` raises, or an accept error that is
not temporary occurs, all the loops stop.

## Runtime notes

The application must ignore `SIGPIPE`. `Ache_miou.Server` and
`Ache_miou.Client` use `Miou_unix.file_descr`, and `ache-miou-tls` a TLS
socket. For another socket type, implement `Ache_miou.IO` and apply
`Ache_miou.Make`. Run the server handler in
its own task for each accepted socket, and close the socket when it
returns. The server timeouts are on by default (idle 60 s, head 30 s,
body 60 s, write 60 s, WebSocket close 10 s); `~idle_timeout:None` and the others turn one off.
With an idle timeout of at least 1 s, persistent responses carry
`Keep-Alive: timeout=n` (the idle timeout rounded down, with `max=` when
`Config.max_requests_per_connection` is set), and the server waits 1 s
longer than it announces. The client honours the hint: `n - 2` seconds
(at most 598) after a response with `Keep-Alive: timeout=n`, an idle
client connection closes, and `Client.request` on it after that time gets
`` `Connection_closed `` before anything is written.
Timeouts use `Miou_unix.sleep` and need no clock. With
`?idle_timeout` or `?head_timeout`, when the server stops reading it reads
and drops input until the peer stops sending. It waits at most
`idle_timeout` (2 s without one) after it stops reading or after its last
write, whichever is later; input that arrives does not extend this limit.
A peer that stops sending still gets the rest of the response. Without a
timeout it closes at once. Handlers and body
callbacks run in the connection tasks and must not leave a child task that
they do not await. `Client.shutdown` must be called by the task that
created the connection.

The server adds a `Date` field with the current time to each response that
is not 1xx and has none (RFC 9110 §6.6.1). The text is formatted once per
second in each domain. `~date:false` on `create_connection_handler`, `run`
or the TLS handler turns it off.

## TLS

To make the test certificates, and for the TLS notes, see
[TLS in ache.md](ache.md#tls).

The same as the [Eio TLS example](ache-eio.md#tls), with Miou. As with
`ache-miou`, the caller owns the socket and
closes it after the server handler returns, or after `Client.shutdown`; this
is also true when the handshake fails. `create_connection_handler_with_fd`
returns when the handshake fails, and `create_connection_with_fd` raises
`` Tls_miou_unix.Tls_failure (`Error (`AuthenticationFailure _)) `` when the
certificate is rejected.

<!-- example: examples/miou/miou_tls_hello_server.ml -->
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

let get_ok = function
  | Ok value -> value
  | Error (`Msg message) -> failwith message

let read_file path = In_channel.with_open_bin path In_channel.input_all

let rec clean_up orphans =
  match Miou.care orphans with
  | None | Some None -> ()
  | Some (Some connection) ->
    (match Miou.await connection with
    | Ok () -> ()
    | Error exn -> prerr_endline ("connection error: " ^ Printexc.to_string exn));
    clean_up orphans

let () =
  Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
  let port = int_of_string Sys.argv.(1) in
  let cert = Sys.argv.(2)
  and key = Sys.argv.(3) in
  Mirage_crypto_rng_unix.use_default ();
  let certificates =
    get_ok (X509.Certificate.decode_pem_multiple (read_file cert))
  and private_key = get_ok (X509.Private_key.decode_pem (read_file key)) in
  let tls =
    get_ok
      (Tls.Config.server ~certificates:(`Single (certificates, private_key)) ())
  in
  let handler =
    Ache_miou_tls.Server.create_connection_handler_with_fd
      ~tls
      ~request_handler
      ~error_handler
  in
  Miou_unix.run @@ fun () ->
  let socket = Miou_unix.tcpv4 () in
  Miou_unix.bind_and_listen
    ~backlog:128
    socket
    Unix.(ADDR_INET (inet_addr_loopback, port));
  Printf.printf "Listening on https://127.0.0.1:%d\n%!" port;
  let orphans = Miou.orphans () in
  let rec accept () =
    clean_up orphans;
    let client, client_address = Miou_unix.accept socket in
    ignore
      (Miou.async ~orphans (fun () ->
         Fun.protect
           ~finally:(fun () -> Miou_unix.close client)
           (fun () -> handler client client_address))
       : unit Miou.t);
    accept ()
  in
  accept ()
```

<!-- example: examples/miou/miou_tls_hello_client.ml -->
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

let get_ok = function
  | Ok value -> value
  | Error (`Msg message) -> failwith message

let () =
  Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
  let port = int_of_string Sys.argv.(1) in
  let ca = Sys.argv.(2) in
  Mirage_crypto_rng_unix.use_default ();
  let authenticator =
    let cas =
      get_ok
        (X509.Certificate.decode_pem_multiple
           (In_channel.with_open_bin ca In_channel.input_all))
    in
    X509.Authenticator.chain_of_trust
      ~time:(fun () -> Some (Ptime_clock.now ()))
      cas
  in
  let tls = get_ok (Tls.Config.client ~authenticator ()) in
  Miou_unix.run @@ fun () ->
  let socket = Miou_unix.tcpv4 () in
  Miou_unix.connect socket Unix.(ADDR_INET (inet_addr_loopback, port));
  let host = Domain_name.(host_exn (of_string_exn "localhost")) in
  let client =
    Ache_miou_tls.Client.create_connection_with_fd
      ~tls
      ~peer:(`Host host)
      socket
  in
  let finished = Miou.Computation.create () in
  let on_error () =
    ignore
      (Miou.Computation.try_cancel
         finished
         (Failure "response body cut short", Printexc.get_callstack 0)
       : bool)
  in
  let response_handler response body =
    Format.printf "%a@." Response.pp_hum response;
    let rec on_read buffer ~off ~len =
      print_string (Ache.Bigstring.substring buffer ~off ~len);
      Body.Reader.schedule_read body ~on_read ~on_eof ~on_error
    and on_eof () = ignore (Miou.Computation.try_return finished () : bool) in
    Body.Reader.schedule_read body ~on_read ~on_eof ~on_error
  in
  let headers =
    Headers.of_list [ "host", Printf.sprintf "localhost:%d" port ]
  in
  let request_body =
    Ache_miou_tls.Client.request
      client
      (Request.create ~headers `GET "/")
      ~error_handler
      ~response_handler
  in
  Body.Writer.close request_body;
  Miou.Computation.await_exn finished;
  Ache_miou_tls.Client.shutdown client;
  Miou_unix.close socket
```
