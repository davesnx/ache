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
  | Some "bye" -> Ache_lwt.Websocket.close ws 1000
  | Some message ->
    let* () = Ache_lwt.Websocket.send ws message in
    echo ws

let request_handler _client_address reqd =
  match Ache_lwt.Websocket.upgrade reqd with
  | Ok ws ->
    Lwt.async (fun () ->
      Lwt.catch
        (fun () -> echo ws)
        (function
          | Ache_lwt.Websocket.Closed -> Lwt.return_unit
          | exn -> Lwt.reraise exn))
  | Error _ ->
    let headers = Headers.of_list [ "content-length", "0" ] in
    Request_descriptor.respond_with_string
      reqd
      (Response.create ~headers `Bad_request)
      ""
```

- `upgrade` checks the handshake and responds with `101`, as
  `Ache.Websocket.upgrade` does. On `Error _` it responds nothing: send the
  response yourself, for example `426` with `Sec-WebSocket-Version: 13` for
  `` Error `Unsupported_version ``.
- `receive` resolves with the next whole text message. Once the connection
  has ended and no message waits, it resolves with `None`. The connection
  ends after the client's close frame, the end of the input, a reset, a
  protocol error, or your `close`.
- `send` resolves when the frame is written. It fails with
  `Ache_lwt.Websocket.Closed` when the connection is gone, also when it was
  gone before the call, so a send loop ends when the client leaves.
- `close ws code` sends a close frame and resolves when the frame is written
  or the connection has ended. It raises `Invalid_argument` at once for a
  code that an endpoint may not send. The socket closes when the read in
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
  the read buffer, then gives the new bytes to the core.
- **Timeouts.** A timed read is `Lwt.pick` of the socket read and
  `Ache_lwt.Timeout.expired`. See [Timeouts](#timeouts) for why the timers
  are shared.
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

### Timeouts

With Lwt's `select` and `poll` engines, a cancelled `Lwt_unix.sleep` stays in
memory until no other sleep ends before it. A server that starts one sleep
for each connection and cancels it when the connection ends then keeps
about (connections per second × timeout) cancelled sleeps. The libev engine
frees a cancelled sleep at once.

`Ache_lwt.Timeout` avoids this. All timeouts of one length share one queue,
in deadline order, and one sleep for the front of the queue that is never
cancelled. `set` and `stop` only move an entry in the queue. The servers of
`ache-lwt` and `ache-lwt-tls` use it for `?idle_timeout`, `?head_timeout` and
the TLS handshake. An application can use it for its own timeouts:

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
