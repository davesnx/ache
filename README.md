# ache

ache is an HTTP/1.1 client and server library for OCaml 5. The core package,
`ache`, does no I/O: it parses and serializes messages and keeps the state of
a connection. Two runtimes drive it on real sockets:

- `ache-eio`, for [Eio](https://github.com/ocaml-multicore/eio)
- `ache-lwt`, for [Lwt](https://github.com/ocsigen/lwt)

Status: not released yet. The API can change before 1.0.

## Lineage

ache is a fork of [httpun](https://github.com/anmonteiro/httpun) at commit
`37fdcd8`. httpun is a fork of
[httpaf](https://github.com/inhabitedtype/httpaf). The connection state
machines, the `Reqd` and `Body` API, and most types come from them. The
changes are:

- **Parser.** Message heads, chunk-size lines and trailer sections are parsed
  with [parseff](https://github.com/davesnx/parseff) in place of Angstrom.
  Bodies are framed by a small state machine, and a body of many small chunks
  is delivered in linear time.
- **Serializer.** [Faraday](https://github.com/inhabitedtype/faraday) 0.8.2
  is vendored in `vendor/faraday` and compiled inside `ache`, so a program can
  also link the opam `faraday` package.
- **No gluten.** `ache-eio` and `ache-lwt` have their own I/O loops. The
  interface that a runtime drives is `Ache.Runtime.S`. An HTTP upgrade hands
  the connection to any protocol packed with `Ache.Runtime.make`.
- **Packages.** `httpun-types` is part of `ache`; `httpun-lwt` and
  `httpun-lwt-unix` are one package, `ache-lwt`. The async and mirage
  runtimes are removed.
- **Strict parsing.** The server rejects requests that RFC 9110 and RFC 9112
  do not allow, and the connection closes after the error response:
  - 400 for a bare LF or bare CR as a line end, obs-fold, a field name that
    is not a token, a CTL in a field value, and a request line without a
    version;
  - 400 for an HTTP/1.1 request without `Host`, more than one `Host` field,
    or a `Host` value that is not `uri-host [":" port]`;
  - 400 for a CONNECT target that is not `host:port`;
  - 400 for a `Content-Length` that is not one repeated `1*DIGIT` value
    (httpun accepted `+3` and `0x3`), for both `Content-Length` and
    `Transfer-Encoding`, for `Transfer-Encoding` in an HTTP/1.0 request, and
    for `chunked` that is not the final coding or appears twice;
  - 501 for a transfer coding other than `chunked`, as in
    `Transfer-Encoding: gzip, chunked`;
  - 505 for a major HTTP version other than 1.

  The client applies the same head grammar and `Content-Length` rules to
  responses, and reports a violation as `` `Malformed_response `` or
  `` `Invalid_response_body_length ``. The server and the client raise
  `Invalid_argument` and write nothing when the application gives a head with
  an invalid field name, method or target, or a CR, LF or NUL in a value.
- **Limits.** `Config.max_header_size` (default 16 KiB) limits a request or
  response head, a chunk-size line and a trailer section. A larger request
  head gets `431 Request Header Fields Too Large`. A chunk size with more than
  15 hex digits is rejected. The head limit does not depend on
  `read_buffer_size`.
- **Accepted.** Chunk extensions are ignored and trailer fields are
  discarded; httpun rejected both.
- **Conformance tests.** `rfc_test/` has one test suite for RFC 9110 and one
  for RFC 9112. `rfc_test/COVERAGE.md` and `rfc_test/COVERAGE_9112.md` map
  each requirement sentence of the RFC to its tests, or give the reason it is
  out of scope. `rfc_test/KNOWN_FAILURES.md` and
  `rfc_test/KNOWN_FAILURES_9112.md` list the requirements that ache does not
  meet yet.

[CHANGES.md](CHANGES.md) has the full list.

## Install

ache is not on opam yet. Pin it from GitHub:

```sh
opam pin add ache https://github.com/davesnx/ache.git
opam pin add ache-eio https://github.com/davesnx/ache.git  # or ache-lwt
```

ache needs OCaml 5.3 or later.

## Quick start with Eio

A server that answers every request with `Hello, world!`. It takes the port
as its first argument.

<!-- example: examples/eio/eio_hello_server.ml -->
```ocaml
open Ache

let request_handler _client_address reqd =
  let body = "Hello, world!\n" in
  let headers =
    Headers.of_list
      [ "content-type", "text/plain"
      ; "content-length", Int.to_string (String.length body)
      ]
  in
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

A client that sends `GET /` to that server and prints the response. HTTP/1.1
requires a `Host` header; without it the server answers 400.

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
  let response_handler response body =
    Format.printf "%a@." Response.pp_hum response;
    let rec on_read buffer ~off ~len =
      print_string (Bigstringaf.substring buffer ~off ~len);
      Body.Reader.schedule_read body ~on_read ~on_eof
    and on_eof () = Eio.Promise.resolve resolve_finished () in
    Body.Reader.schedule_read body ~on_read ~on_eof
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
  Eio.Promise.await finished;
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
  let headers =
    Headers.of_list
      [ "content-type", "text/plain"
      ; "content-length", Int.to_string (String.length body)
      ]
  in
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
         Body.Reader.schedule_read body ~on_read ~on_eof
       and on_eof () = Lwt.wakeup_later notify_finished () in
       Body.Reader.schedule_read body ~on_read ~on_eof
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

`examples/` has more: streaming request and response bodies, pipelined
requests, and a CONNECT proxy.

## Limits of this version

- HTTP/1.0 and HTTP/1.1 only. There is no HTTP/2 or HTTP/3.
- `ache-lwt` has no TLS module. `Ache_lwt.Make` takes any socket type that
  implements `Ache_lwt.IO`, so you can give it a TLS socket. `ache-eio`
  accepts any `Eio.Flow.two_way`, for example a flow from `tls-eio`.
- Trailer fields are read and discarded; the API does not expose them.
- Server timeouts are off by default. `?idle_timeout` closes a connection
  that waits for a request; `?head_timeout` answers 400 to a request head
  that is not complete in time. A request body or a response in progress is
  never timed out. `Ache_eio` also needs `?clock` for either timeout.
- The API is not stable before 1.0.

## License

BSD-3-Clause, with the copyright lines of httpaf and httpun. See
[LICENSE](LICENSE). `vendor/faraday` is an unmodified copy of Faraday 0.8.2
under its own BSD-3-Clause license in `vendor/faraday/LICENSE`.
