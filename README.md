# ache

ache is an HTTP/1.1 client and server library for OCaml 5. The core package,
`ache`, does no I/O: it parses and serializes messages and keeps the state of
a connection. Two runtimes drive it on sockets:

- `ache-eio`, for [Eio](https://github.com/ocaml-multicore/eio)
- `ache-lwt`, for [Lwt](https://github.com/ocsigen/lwt)

ache is a fork of [httpun](https://github.com/anmonteiro/httpun), which is a
fork of [httpaf](https://github.com/inhabitedtype/httpaf). It keeps their
`Reqd`, `Body` and connection API, and changes this:

- Requests that RFC 9110 and RFC 9112 do not allow get an error response
  (400, 408, 414, 431, 501 or 505), and the connection closes.
- ache sets the framing of each response (`Content-Length`,
  `Transfer-Encoding`, `Connection: close`) and raises `Invalid_argument` for
  a response it cannot send correctly.
- The client reports every request that gets no response, and pipelines only
  behind idempotent requests.
- Message heads are parsed with [parseff](https://github.com/davesnx/parseff).
  There is no gluten and no Angstrom; Faraday is vendored.

Status: not released, and not publishable to opam until parseff releases
`Source.of_function ?initial_buffer_size` (see [Install](#install)). The API can change
before 1.0. Users of httpun 0.2 can read
[Migrating from httpun](#migrating-from-httpun-02).

## Install

ache is not on opam yet. Pin it from GitHub:

```sh
opam pin add ache https://github.com/davesnx/ache.git
opam pin add ache-eio https://github.com/davesnx/ache.git  # or ache-lwt
```

ache needs a parseff later than 0.5.0, which is not released yet. The
`pin-depends` field of `ache.opam` pins `parseff.dev` to
[davesnx/parseff#57](https://github.com/davesnx/parseff/pull/57), so the
commands above fetch it. ache cannot be published to the opam repository
until a parseff release contains that change.

ache needs OCaml 5.3 or later.

## Quick start with Eio

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
      print_string (Bigstringaf.substring buffer ~off ~len);
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

## Quick start with Lwt

The same server and client with `ache-lwt`. A Lwt server must ignore
`SIGPIPE`, as the server below does, or the process exits when it writes to a
peer that has closed the connection (ache installs no signal handlers;
`Eio_main.run` already ignores `SIGPIPE`).

<!-- example: examples/lwt/lwt_hello_server.ml -->
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
         print_string (Bigstringaf.substring buffer ~off ~len);
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

The output is the same as with Eio. `examples/` has more: streaming request
and response bodies, pipelined requests, and a CONNECT proxy.

## Differences from httpun

ache forks httpun at commit `37fdcd8` (httpun 0.2.0 and 10 commits).
[CHANGES.md](CHANGES.md) has the full list, with the httpun and httpaf issues
that each change fixes.

### Packages and internals

- `httpun-types` is part of `ache`. `httpun-lwt` and `httpun-lwt-unix` are
  one package, `ache-lwt`. The async and mirage runtimes are removed.
- Message heads, chunk-size lines and trailer sections are parsed with
  parseff in place of Angstrom. Bodies are framed by a small state machine.
- [Faraday](https://github.com/inhabitedtype/faraday) 0.8.2 is vendored in
  `vendor/faraday` and compiled inside `ache`. A program can also link the
  opam `faraday` package.
- There is no gluten. `ache-eio` and `ache-lwt` have their own I/O loops.
  `Ache.Runtime.S` is the interface that a runtime drives, and
  `Ache.Runtime.make` packs a protocol so that an HTTP upgrade can hand the
  connection to it.

### Requests the server rejects

The server calls the error handler, sends the error response with
`connection: close`, and closes the connection. Requests that came before it
on the same connection get their responses first.

| Request | Response |
|---|---|
| A bare LF or a bare CR as a line end, obs-fold, a field name that is not a token, a CTL (for example NUL) in a field value | 400 |
| A request line without a version | 400 |
| An HTTP/1.1 request without `Host`, two `Host` fields, a `Host` value that is not `uri-host [":" port]` | 400 |
| A target form that does not fit the method: `*` with a method other than OPTIONS, authority-form with a method other than CONNECT | 400 |
| A CONNECT target that is not `host:port` with a port of 0 to 65535 | 400 |
| A `Content-Length` that is not one repeated `1*DIGIT` value, such as `+3` or `3, 4` | 400 |
| Both `Content-Length` and `Transfer-Encoding`; `Transfer-Encoding` in an HTTP/1.0 request; `chunked` that is not the final coding or is there twice | 400 |
| A transfer coding other than `chunked`, as in `gzip, chunked` | 501 |
| A method longer than `Config.max_header_size` | 501 |
| A target that makes the request line longer than `max_header_size` | 414 |
| A head longer than `max_header_size` (default 16 KiB) | 431 |
| A head that is not complete when `?head_timeout` expires (off by default) | 408 |
| A major version other than 1 | 505 |

ache accepts HTTP/1.x with x > 1 as HTTP/1.1, one empty line before the
request line, chunk extensions (ignored) and trailer fields (discarded).
httpun rejected chunk extensions and trailers.

### Responses

- ache adds `Content-Length` to a string or bigstring response, and
  `Transfer-Encoding: chunked` to a streaming response on an HTTP/1.1
  persistent connection. Otherwise a streaming body ends when the connection
  closes.
- A response to `HEAD`, and a 1xx, 204 or 304 response, has no body.
- Each final response after which the connection closes has
  `connection: close`. A response to an HTTP/1.0 request on a connection that
  stays open has `connection: keep-alive`.
- The `respond_with_*` functions raise `Invalid_argument`, before they write
  anything, for: a 1xx status; both `Content-Length` and `Transfer-Encoding`;
  either field in a 1xx, a 204 or a 2xx response to `CONNECT`;
  `Transfer-Encoding` in a string or bigstring response with a body; a
  `Content-Length` that differs from the string or bigstring; a field name
  that is not a token, or a CR, LF or NUL in a field value or reason phrase.
  The body writer of a streaming response raises `Invalid_argument` for a
  write past its `Content-Length`.
- `Reqd.respond_with_informational` sends `100 Continue`, `103 Early Hints`
  or another interim response before the final response.
- Only a 2xx response to `CONNECT` starts a tunnel. Another response to
  `CONNECT` lets the connection serve the next request.
- `Status` has the codes of the IANA registry that httpun did not have (102,
  103, 207, 208, 226, 308, 421, 422, 423, 424, 425, 451, 506, 507, 508).
  `` `Payload_too_large `` is now `` `Content_too_large ``, and 422 is
  `` `Unprocessable_content ``.

### Bodies

- `Body.Reader.schedule_read` takes a required `~on_error`. A body cut by the
  end of the input, by a framing error or by a closed connection calls
  `on_error`, not `on_eof`; exactly one of the two ends a body. httpun called
  `on_eof`, as for a complete body.
- `on_read` gets all the bytes that are available in one call, so a body of
  many small chunks is delivered in linear time. ache does not reuse the
  bytes it gives to `on_read`.
- When the response is complete, the server reads and drops the rest of a
  request body that the application did not read.

### Client

- The new error `` `Connection_closed `` goes to each request that got no
  byte of its response before the connection stopped reading. httpun called
  no handler for these requests, or gave them
  `` `Malformed_response "unexpected eof" ``.
- A request is pipelined only behind requests with an idempotent method. It
  waits behind other requests, and behind `CONNECT` and `Upgrade` requests,
  until their responses are complete.
- Interim 1xx responses other than 101 are dropped. After a 101, or a 2xx
  response to `CONNECT`, the client stops reading HTTP and `upgrade` gives
  the bytes after the head to the new protocol.
- An obs-fold in a response field value becomes one space. Empty lines
  before a response are ignored.
- `Client_connection.request` raises `Invalid_argument` for a method or field
  name that is not a token, a CR, LF or NUL in a field value, and a CR, LF,
  NUL or space in the target. Later requests are still sent.

## Limits

- HTTP/1.0 and HTTP/1.1 only. There is no HTTP/2 or HTTP/3.
- `ache-lwt` has no TLS module (see [Runtime notes](#runtime-notes)).
- Trailer fields are read and discarded. The API does not give them to the
  application.
- Server timeouts are off by default. `?idle_timeout` closes a connection
  that waits for a request. `?head_timeout` answers 408 to a request head
  that is not complete in time. A request body or a response in progress is
  never timed out.
- Once its write side has closed, the server reads and dispatches no new
  request; the request in progress still gets its body.
- The API is not stable before 1.0.

## Runtime notes

- **Eio.** `Ache_eio.Server.create_connection_handler ~request_handler
  ~error_handler` can be given to `Eio.Net.run_server`. The server and the
  client accept any `_ Eio.Flow.two_way`, for example a TLS flow. Timeouts
  need `?clock`. With a clock, when the server stops reading (after a
  response that closes the connection) it reads and drops input until the
  peer closes, for at most `idle_timeout` (2 s without
  one), so that the peer can read the response before the connection is
  reset. Without a clock it closes at once.
- **Lwt.** The application must ignore `SIGPIPE`. `Ache_lwt.Server` and
  `Ache_lwt.Client` use `Lwt_unix.file_descr`. For another socket type, such
  as a TLS socket, implement `Ache_lwt.IO` and apply `Ache_lwt.Make`: `read`
  fails with `End_of_file` at the end of the input or when the peer is gone,
  and `writev` returns `` `Closed `` when the peer is gone. The `Ache_lwt.IO`
  documentation has the full contract. The Lwt server does not wait for the
  peer to close after the last response.

## Testing

`dune test` runs these suites (counts from `dune test --force` on
2026-11-13):

| Suite | Tests |
|---|---|
| `lib_test`: core unit tests | 256 |
| `rfc_test`: RFC 9110 | 159 |
| `rfc_test`: RFC 9112 | 178 |
| `eio_test`: Eio on loopback sockets | 28 |
| `lwt_test`: Lwt on loopback sockets | 24 |

The RFC suites start from each sentence of RFC 9110 and RFC 9112 §2–§11 that
has a requirement keyword (MUST, SHOULD, MAY and others), plus definitions
that a parser must follow. `rfc_test/COVERAGE.md` and
`rfc_test/COVERAGE_9112.md` give each requirement its tests, or the reason it
is out of scope:

| RFC | Requirements | Tested | Out of scope |
|---|---|---|---|
| 9110 | 445 | 93 | 352 |
| 9112 | 173 | 115 | 58 |

No test is marked as a known failure (`rfc_test/KNOWN_FAILURES*.md`). The
README check makes sure that the code in this file is the same as the
compiled examples.

`conformance/` runs the [h1spec](https://github.com/uNetworking/h1spec)
requests against an Eio and a Lwt echo server. It has not been run with h1spec
yet, and CI does not run it.

## Migrating from httpun 0.2

| httpun 0.2 | ache |
|---|---|
| Packages `httpun`, `httpun-types` | `ache` |
| Package `httpun-eio` | `ache-eio` |
| Packages `httpun-lwt`, `httpun-lwt-unix` | `ache-lwt` |
| Packages `httpun-async`, `httpun-mirage` | Removed |
| Modules `Httpun`, `Httpun_types` | `Ache` |
| `Httpun_eio` | `Ache_eio` |
| `Httpun_lwt_unix.Server`, `Httpun_lwt_unix.Client` | `Ache_lwt.Server`, `Ache_lwt.Client` |
| `Httpun_lwt.Server (Gluten_lwt.Server)`, `Httpun_lwt.Client (...)` | `Ache_lwt.Make (Io)`, with `Io : Ache_lwt.IO` |
| `Httpun_lwt_unix.Server.TLS`, `.SSL`, `Client.TLS`, `.SSL` | Removed. Implement `Ache_lwt.IO` for the TLS socket |
| Runtime `request_handler : addr -> Reqd.t Gluten.reqd -> unit` | `addr -> Reqd.t -> unit` |
| `Reqd.respond_with_upgrade : t -> Headers.t -> (unit -> unit) -> unit` | `t -> Headers.t -> Ache.Runtime.impl -> unit`; make the `impl` with `Ache.Runtime.make` |
| `Client.upgrade : t -> Gluten.impl -> unit` | `t -> Ache.Runtime.impl -> unit` |
| `Httpun_eio.Server.create_connection_handler ?config ~request_handler ~error_handler ~sw addr socket` | `Ache_eio.Server.create_connection_handler ?config ?clock ?idle_timeout ?head_timeout ~request_handler ~error_handler flow addr`: no `~sw`, the flow comes first |
| `Httpun_eio.Client.create_connection ~sw (_ Eio.Net.stream_socket)` | `~sw (_ Eio.Flow.two_way)` |
| `Client.t = { connection; runtime }` (Eio and Lwt) | Abstract |
| `Reqd.error`: `` `Bad_request ``, `` `Bad_gateway ``, `` `Internal_server_error ``, `` `Exn `` | Also `` `Not_implemented ``, `` `Http_version_not_supported ``, `` `Uri_too_long ``, `` `Request_header_fields_too_large ``, `` `Request_timeout `` |
| `Client_connection.error`: `` `Malformed_response ``, `` `Invalid_response_body_length ``, `` `Exn `` | Also `` `Connection_closed `` |
| `Response.Body_length.t` | Also `` `No_body `` (response to `HEAD`, 1xx, 204, 304) |
| `` `Payload_too_large `` | `` `Content_too_large `` |
| `Status.of_code 422` is `` `Code 422 `` (also 102, 103, 207, 208, 226, 308, 421, 423, 424, 425, 451, 506, 507, 508) | A named constructor, such as `` `Unprocessable_content ``. Exhaustive matches on the `Status` subtypes must handle them |
| `Body.Reader.schedule_read t ~on_eof ~on_read` | Also `~on_error` (required), called in place of `on_eof` for a cut body |
| `respond_with_string` with a 1xx status | `Reqd.respond_with_informational` |
| `respond_with_*` sends the head as given | Raises `Invalid_argument` in the cases listed in [Responses](#responses) |
| `Client_connection.request` sends the head as given | Raises `Invalid_argument` for an invalid head |
| HTTP/1.1 requests without `Host` are served | 400: send `Host` from clients |
| `Config.t` with four buffer sizes | Also `proxy` and `max_header_size`. Write `{ Config.default with ... }` |
| Requests after a failed write are still dispatched | Not dispatched: the reader closes after the request in progress |

## License

BSD-3-Clause, with the copyright lines of httpaf and httpun. See
[LICENSE](LICENSE). `vendor/faraday` is an unmodified copy of Faraday 0.8.2
under its own BSD-3-Clause license in `vendor/faraday/LICENSE`.
