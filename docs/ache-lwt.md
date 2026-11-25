# ache-lwt

ache on Lwt. The protocol behavior, the general limits and the TLS notes
are in [ache.md](ache.md).

## Quick start

The same server and client as in [ache-eio](ache-eio.md#quick-start), with
`ache-lwt`. A Lwt server must ignore
`SIGPIPE`, as the server below does, or the process exits when it writes to a
peer that has closed the connection (ache installs no signal handlers;
`Eio_main.run` already ignores `SIGPIPE`).

<!-- example: examples/lwt/lwt_hello_server.ml -->
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
  | #Status.standard as status ->
    Body.Writer.write_string body (Status.default_reason_phrase status));
  Body.Writer.close body

let () =
  Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
  let port = int_of_string Sys.argv.(1) in
  let listen_address = Unix.(ADDR_INET (inet_addr_loopback, port)) in
  Lwt_main.run
    (let open Lwt.Syntax in
     let* _server =
       Lwt_io.establish_server_with_client_socket
         listen_address
         (Ache_lwt.Server.create_connection_handler
            ~request_handler
            ~error_handler)
     in
     Printf.printf "Listening on 127.0.0.1:%d\n%!" port;
     fst (Lwt.wait ()))
```

<!-- example: examples/lwt/lwt_hello_client.ml -->
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
  Lwt_main.run
    (let open Lwt.Syntax in
     let socket = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
     let* () =
       Lwt_unix.connect socket Unix.(ADDR_INET (inet_addr_loopback, port))
     in
     let* client = Ache_lwt.Client.create_connection socket in
     let finished, notify_finished = Lwt.wait () in
     let response_handler response body =
       Format.printf "%a@." Response.pp_hum response;
       let rec on_read buffer ~off ~len =
         print_string (Ache.Bigstring.substring buffer ~off ~len);
         Body.Reader.schedule_read body ~on_read ~on_eof ~on_error
       and on_eof () = Lwt.wakeup_later notify_finished ()
       and on_error () =
         Lwt.wakeup_later_exn
           notify_finished
           (Failure "response body cut short")
       in
       Body.Reader.schedule_read body ~on_read ~on_eof ~on_error
     in
     let headers =
       Headers.of_list [ "host", Printf.sprintf "127.0.0.1:%d" port ]
     in
     let request_body =
       Ache_lwt.Client.request
         client
         (Request.create ~headers `GET "/")
         ~error_handler
         ~response_handler
     in
     Body.Writer.close request_body;
     let* () = finished in
     Ache_lwt.Client.shutdown client)
```

The output is the same as with [Eio](ache-eio.md#quick-start).
`examples/` has more: streaming request
and response bodies, pipelined requests, and a CONNECT proxy.

## WebSocket

`Ache_lwt.Websocket` gives the WebSocket server of
[ache.md](ache.md#websocket) a promise API. Call `upgrade` in the request
handler, then receive and send in your own promise:

```ocaml
open Ache

let rec echo ws =
  let open Lwt.Syntax in
  let* message = Ache_lwt.Websocket.receive ws in
  match message with
  | None -> Lwt.return_unit
  | Some (Text "bye") -> Ache_lwt.Websocket.close ~reason:"bye" ws 1000
  | Some (Text message) ->
    let* () = Ache_lwt.Websocket.send_text ws message in
    echo ws
  | Some (Binary message) ->
    let* () = Ache_lwt.Websocket.send_binary ws message in
    echo ws

let request_handler _client_address reqd =
  match Ache_lwt.Websocket.upgrade reqd echo with
  | Ok () -> ()
  | Error _ ->
    let headers = Headers.of_list [ "content-length", "0" ] in
    Request_descriptor.respond_with_string
      reqd
      (Response.create ~headers `Bad_request)
      ""
```

- `upgrade reqd session` checks the handshake and responds with `101`, as
  `Ache.Websocket.upgrade` does, then runs `session ws` like `Lwt.async`.
  `Ache_lwt.Websocket.Closed` raised by the session is ignored, so a session
  ends quietly when a send finds the client gone. Another exception sends a
  close frame with code 1011, then goes to `Lwt.async_exception_hook`, whose
  default exits the program: set your own. On `Error _` it responds nothing and the
  session does not run: send the response yourself, for example `426` with
  `Sec-WebSocket-Version: 13` for `` Error `Unsupported_version ``.
- `receive` resolves with the next whole message, `Text s` or `Binary s`.
  Once the connection has ended and no message waits, it resolves with
  `None`. The connection ends after the client's close frame, the end of the
  input, a reset, a protocol error, or your `close`. `client_close ws` then
  gives the client's close code and reason, if it sent a valid close frame.
- `send_text` and `send_binary` resolve when the frame is written. They fail
  with `Ache_lwt.Websocket.Closed` when the connection is gone, also when it
  was gone before the call, so a send loop ends when the client leaves.
- `ping ws payload` sends a ping and resolves when the next pong arrives, or
  fails with `Closed` when the connection ends first. To find a client that
  is gone: `Lwt_unix.with_timeout 30. (fun () -> Ache_lwt.Websocket.ping ws
  "")`, and `close` on `Lwt_unix.Timeout`. The pong comes after the
  messages the client sent before it, so keep calling `receive` meanwhile:
  while 8 messages wait, reading is paused and the pong is not seen.
- `close ?reason ws code` sends a close frame and resolves when the frame is
  written or the connection has ended. It raises `Invalid_argument` at once
  for a code that an endpoint may not send, or a reason that is not UTF-8 or
  longer than 123 bytes. The socket closes when the read in
  progress ends too: when the client answers with its close frame or closes
  the connection. There is no timeout for a client that does neither.
- Messages that you have not received wait in a queue. When 8 messages wait,
  the connection stops reading from the client until `receive` takes one.
  Frames that one read already holds still join the queue.
- You can cancel a pending `receive`, for example with `Lwt.pick` or
  `Lwt_unix.with_timeout`. No message is lost: the next `receive` gets it.
- The module does not depend on the socket type, so it works the same with
  `Ache_lwt.Make` and with `ache-lwt-tls`.

## How it works

This continues [How it works](ache.md#how-it-works) in ache.md, which
explains the core and the read and write loops.

### Lwt

An Lwt program runs in `Lwt_main.run`, one event loop on one thread. The
engine is libev (`epoll` on Linux) when Lwt is built with it, or `select` or
`poll`. Code runs as callbacks on promises: a callback that must wait
returns a pending promise, and the loop runs it again when that promise
resolves.

### One connection

`Server.create_connection_handler` is the function that
`Lwt_io.establish_server_with_client_socket` calls for each accepted socket.
It creates the core connection and starts the two loops:

```
 Lwt_io.establish_server_with_client_socket
        │  one call per accepted socket
        ▼
 IO_loop.start
  ├─ read loop  (Lwt.async) ── Lwt_bytes.read into the read buffer
  │                            (Lwt.pick with a timer when a timeout applies)
  ├─ write loop (Lwt.async) ── Lwt_unix.writev, one call for all buffers
  └─ Lwt.choose [ both loops ended ; failed ] ─► close the socket once
```

- **Waiting.** On `Yield`, a loop gives the core a function that starts the
  loop again (`yield_reader t read_loop`), and returns. The core calls it
  when it has work. A loop starts again from a callback, so its stack does
  not grow with each request.
- **Writes.** The write loop sends all the buffers of one `Write` with one
  `Lwt_unix.writev`. Lwt wakes a waiting writer at once, so without the
  hold of the writer while one read is parsed (see
  [One write for each read](ache.md#one-write-for-each-read)), each
  pipelined response would be its own `writev`.
- **Reads.** The read loop reads with `Lwt_bytes.read` into free space in
  the read buffer, then gives the new bytes to the core. When the read
  completes without a wait, the loop calls `Lwt.pause` first. Without it, a
  client that always has data ready keeps the read and write loops in one
  chain of callbacks that does not return to `Lwt_main`, and no other
  connection or timer runs.
- **TLS.** `ache-lwt-tls` gives the `Lwt_io` channels under the TLS session
  17 KiB buffers, a little more than one full TLS record (16 KiB of data,
  its header and its padding). A large response goes out in about one write
  for each record. With the default 4 KiB buffer, each record takes 4
  writes.
- **Timeouts.** A timed read is `Lwt.pick` of the socket read and
  `Ache_lwt.Timeout.expired`; a timed write is `Lwt.pick` of the `writev`
  and the write timer. See [Timeouts](#timeouts) for which timer applies
  and why the timers are shared.
- **Errors.** If a write fails, or the core raises in `report_exn`, the
  connection resolves `failed`, and `Lwt.choose` closes the socket.

## Runtime notes

The application must ignore `SIGPIPE`. `Ache_lwt.Server` and
`Ache_lwt.Client` use `Lwt_unix.file_descr`, and `ache-lwt-tls` a TLS
socket. For another socket type, implement `Ache_lwt.IO` and apply
`Ache_lwt.Make`: `read`
fails with `End_of_file` at the end of the input or when the peer is gone,
and `writev` returns `` `Closed `` when the peer is gone. The `Ache_lwt.IO`
documentation has the full contract. The Lwt server does not wait for the
peer to close after the last response.

The server adds a `Date` field with the current time to each response that
is not 1xx and has none (RFC 9110 §6.6.1). The text is formatted once per
second in each domain. `~date:false` on `create_connection_handler` or the
TLS handler turns it off.

### Timeouts

The server timeouts are on by default; `~idle_timeout:None` (and so on)
turns one off. The TLS handler takes the same arguments.

| Argument | Default | Applies while |
|---|---|---|
| `?idle_timeout` | `Some 60.` | no request is in progress, or a WebSocket waits for a frame (it closes with 1001) |
| `?head_timeout` | `Some 30.` | a request head is partly read (408) |
| `?body_timeout` | `Some 60.` | the connection waits for request body bytes (408 if no response started, else close) |
| `?write_timeout` | `Some 60.` | output waits to be written, for example to a client that stopped reading (close) |

Before each read, the server asks the core which timeout applies
(`Server_connection.read_timeout`). While the request handler runs and the
connection waits for nothing from the client, no timeout applies.

The body and write timeouts measure progress, not single reads or writes:
their clock restarts only after 2048 bytes, so a client that sends or reads
one byte at a time cannot keep a connection. The body clock counts only
while a read waits for body bytes. It counts ticks of a tenth of
`body_timeout`, so all connections with the same `body_timeout` share one
timer queue, and the timeout can end up to a tenth late. The write clock
runs while output is pending and restarts when all of it is written. When
it runs out, the server closes the socket and ends the connection, and
pending `Body.Writer.flush` callbacks get `` `Closed ``.

With Lwt's `select` and `poll` engines, a cancelled `Lwt_unix.sleep` stays in
memory until no other sleep ends before it. A server that starts one sleep
for each connection and cancels it when the connection ends then keeps
about (connections per second × timeout) cancelled sleeps. The libev engine
frees a cancelled sleep at once.

`Ache_lwt.Timeout` avoids this. All timeouts of one length share one queue,
in deadline order, and one sleep for the front of the queue that is never
cancelled. `set` and `stop` only move an entry in the queue. The servers of
`ache-lwt` and `ache-lwt-tls` use it for each server timeout and the TLS
handshake. An application can use it for its own timeouts:

```ocaml
let read_with_timeout timeout read =
  Ache_lwt.Timeout.set timeout;
  Lwt.finalize
    (fun () ->
      Lwt.pick
        [ Lwt.map Option.some (read ())
        ; Lwt.map (fun () -> None) (Ache_lwt.Timeout.expired timeout)
        ])
    (fun () ->
      Ache_lwt.Timeout.stop timeout;
      Lwt.return_unit)
```

Create the timeout once, for example with `Ache_lwt.Timeout.create 30.`, and
reuse it for each read. Cancelling the `expired` promise, as `Lwt.pick` does,
does not change the timeout.

### Graceful stop

`create_connection_handler` takes `?stop:unit Lwt.t` and `?grace:float`.
Give the same `stop` promise to every connection. When it resolves, each
connection calls `Ache.Server_connection.drain`:

1. An idle connection closes at once.
2. A connection with a request in progress serves it, with
   `Connection: close` if its head is not written yet, and then closes.
   Requests pipelined after it are not served.
3. A WebSocket connection sends a close frame with code 1001.
4. `grace` seconds after `stop`, a connection that is still open is
   cancelled: its socket closes, and the promise of the connection
   resolves without waiting for the request handler. Without `grace` there
   is no limit.

A connection handler applied after `stop` resolved closes the socket
without reading a request. With `ache-lwt-tls`, a handshake in progress at
`stop` is closed. The handler does not stop the accept loop; the server
does that and waits for its connections:

```ocaml
let serve listen_address =
  let open Lwt.Syntax in
  let stop, stop_now = Lwt.wait () in
  let (_ : Lwt_unix.signal_handler_id) =
    Lwt_unix.on_signal Sys.sigterm (fun _ ->
      if Lwt.is_sleeping stop then Lwt.wakeup_later stop_now ())
  in
  let open_connections = ref 0
  and closed = Lwt_condition.create () in
  let* server =
    Lwt_io.establish_server_with_client_socket listen_address (fun addr fd ->
      incr open_connections;
      Lwt.finalize
        (fun () ->
          Ache_lwt.Server.create_connection_handler
            ~stop
            ~grace:10.
            ~request_handler
            ~error_handler
            addr
            fd)
        (fun () ->
          decr open_connections;
          Lwt_condition.broadcast closed ();
          Lwt.return_unit))
  in
  let* () = stop in
  let* () = Lwt_io.shutdown_server server in
  let rec all_closed () =
    if !open_connections = 0
    then Lwt.return_unit
    else Lwt.bind (Lwt_condition.wait closed) all_closed
  in
  all_closed ()
```

The callback that each connection puts on `stop` is removed when the
connection ends, so a `stop` that stays pending for the life of the server
does not grow with the number of connections served.

## TLS

To make the test certificates, and for the TLS notes, see
[TLS in ache.md](ache.md#tls).

The same as the [Eio TLS example](ache-eio.md#tls), with Lwt.
`create_connection_handler_with_fd` does the handshake and
closes the socket when it fails.

<!-- example: examples/lwt/lwt_tls_hello_server.ml -->
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
  | #Status.standard as status ->
    Body.Writer.write_string body (Status.default_reason_phrase status));
  Body.Writer.close body

let () =
  Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
  let port = int_of_string Sys.argv.(1) in
  let cert = Sys.argv.(2)
  and key = Sys.argv.(3) in
  let listen_address = Unix.(ADDR_INET (inet_addr_loopback, port)) in
  Lwt_main.run
    (let open Lwt.Syntax in
     let* certificate = X509_lwt.private_of_pems ~cert ~priv_key:key in
     let tls =
       match Tls.Config.server ~certificates:(`Single certificate) () with
       | Ok tls -> tls
       | Error (`Msg message) -> failwith message
     in
     let* _server =
       Lwt_io.establish_server_with_client_socket
         listen_address
         (Ache_lwt_tls.Server.create_connection_handler_with_fd
            ~tls
            ~request_handler
            ~error_handler)
     in
     Printf.printf "Listening on https://127.0.0.1:%d\n%!" port;
     fst (Lwt.wait ()))
```

<!-- example: examples/lwt/lwt_tls_hello_client.ml -->
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
  Lwt_main.run
    (let open Lwt.Syntax in
     let* authenticator = X509_lwt.authenticator (`Ca_file ca) in
     let tls =
       match Tls.Config.client ~authenticator () with
       | Ok tls -> tls
       | Error (`Msg message) -> failwith message
     in
     let socket = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
     let* () =
       Lwt_unix.connect socket Unix.(ADDR_INET (inet_addr_loopback, port))
     in
     let host = Domain_name.(host_exn (of_string_exn "localhost")) in
     let* client =
       Ache_lwt_tls.Client.create_connection_with_fd
         ~tls
         ~peer:(`Host host)
         socket
     in
     let finished, notify_finished = Lwt.wait () in
     let response_handler response body =
       Format.printf "%a@." Response.pp_hum response;
       let rec on_read buffer ~off ~len =
         print_string (Ache.Bigstring.substring buffer ~off ~len);
         Body.Reader.schedule_read body ~on_read ~on_eof ~on_error
       and on_eof () = Lwt.wakeup_later notify_finished ()
       and on_error () =
         Lwt.wakeup_later_exn
           notify_finished
           (Failure "response body cut short")
       in
       Body.Reader.schedule_read body ~on_read ~on_eof ~on_error
     in
     let headers =
       Headers.of_list [ "host", Printf.sprintf "localhost:%d" port ]
     in
     let request_body =
       Ache_lwt_tls.Client.request
         client
         (Request.create ~headers `GET "/")
         ~error_handler
         ~response_handler
     in
     Body.Writer.close request_body;
     let* () = finished in
     Ache_lwt_tls.Client.shutdown client)
```
