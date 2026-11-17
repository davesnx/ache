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
  Reqd.respond_with_string reqd (Response.create ~headers `OK) body

let error_handler _client_address ?request:_ error start_response =
  let body = start_response Headers.empty in
  (match error with
  | `Exn exn -> Body.Writer.write_string body (Printexc.to_string exn)
  | #Status.standard as status ->
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
  Reqd.respond_with_string reqd (Response.create ~headers `OK) body

let error_handler _client_address ?request:_ error start_response =
  let body = start_response Headers.empty in
  (match error with
  | `Exn exn -> Body.Writer.write_string body (Printexc.to_string exn)
  | #Status.standard as status ->
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
`Eio.Net.run_server` calls in a new fiber for each accepted socket. It
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

### The reader fiber

Each connection also starts a reader fiber, which reads the socket when the
read loop asks it to, and keeps the result until the read loop takes it.

- A read with a timeout goes through the reader fiber. The read loop waits
  for the result under `Eio.Time.with_timeout`. When the timeout fires, it
  cancels the wait, not the read: tls-eio treats a cancelled read as a
  permanent error, and every later operation on that connection fails.
- The client always reads through the reader fiber, so `Client.shutdown` can
  stop the wait without cancelling the read.
- Every other read is direct: the read loop calls `single_read` itself. A
  server without `?clock` has no timeouts, so all its reads are direct. With
  a clock, the reads of a request body and of pipelined requests are direct,
  and the reads that wait for a new request head use the reader fiber.

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

### Several domains

`Server.run` starts the same handler on several domains. Each domain opens
its own listening socket on the same port with `SO_REUSEPORT` and runs its
own `Eio.Net.run_server`. The first domain is the caller's; the others start
with `Eio.Domain_manager.run`, and `Fiber.all` waits for all of them. The
domains share no state, so a connection stays on the domain that accepted
it.

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
  Reqd.respond_with_string reqd (Response.create ~headers `OK) body

let error_handler _client_address ?request:_ error start_response =
  let body = start_response Headers.empty in
  (match error with
  | `Exn exn -> Body.Writer.write_string body (Printexc.to_string exn)
  | #Status.standard as status ->
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
