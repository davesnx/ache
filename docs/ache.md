# ache

ache is an HTTP/1.1 client and server library for OCaml 5. The core package,
`ache`, does no I/O: it parses and serializes messages and keeps the state of
a connection. Three runtimes drive it on sockets.

Status: experimental. The API can change before 1.0.

## Features

- HTTP/1.0 and HTTP/1.1 client and server, with pipelining, interim 1xx
  responses, `CONNECT` tunnels and protocol upgrades.
- Follows RFC 9110 and RFC 9112: each requirement has its tests or the reason
  it is out of scope, and the h1spec conformance requests pass on Eio, Lwt
  and Miou (see [Testing](#testing)).
- Strict parsing: requests that can cause request smuggling get an error
  response (see [Requests the server rejects](#requests-the-server-rejects)).
- No dependencies: the core package `ache` needs only OCaml.
- Runtimes for Eio, Lwt and Miou, each with TLS through ocaml-tls.

## Packages

| Package | Runtime | Documentation |
|---|---|---|
| `ache-eio`, `ache-eio-tls` | [Eio](https://github.com/ocaml-multicore/eio) | [ache-eio.md](ache-eio.md) |
| `ache-lwt`, `ache-lwt-tls` | [Lwt](https://github.com/ocsigen/lwt) | [ache-lwt.md](ache-lwt.md) |
| `ache-miou`, `ache-miou-tls` | [Miou](https://github.com/robur-coop/miou) | [ache-miou.md](ache-miou.md) |

Each page has a quick start, a TLS example, the notes for its runtime, and
how that runtime runs a connection.

## Install

ache is not on opam yet. Pin it from GitHub:

```sh
opam pin add ache https://github.com/davesnx/ache.git
opam pin add ache-eio https://github.com/davesnx/ache.git  # or ache-lwt, ache-miou
opam pin add ache-eio-tls https://github.com/davesnx/ache.git  # or ache-lwt-tls, ache-miou-tls, for TLS
```

ache needs OCaml 5.3 or later.

## TLS

TLS uses [ocaml-tls](https://github.com/mirleft/ocaml-tls): `ache-eio-tls`
runs `ache-eio` over `tls-eio`, `ache-lwt-tls` runs `ache-lwt` over
`tls-lwt`, and `ache-miou-tls` runs `ache-miou` over the protocol engine of
ocaml-tls (`Tls.Engine`), with the exceptions of `tls-miou-unix`. OpenSSL
(`lwt_ssl`) is not supported.

The examples load a certificate from PEM files. To make a test CA and a
certificate for `localhost` and `127.0.0.1` (the repository has no key
material):

```console
$ dune exec examples/certs/gen_test_certs.exe -- /tmp
```

A response without `Content-Length` or `Transfer-Encoding: chunked` ends when
the connection closes. Over TLS, only a `close_notify` alert proves that the
peer closed the connection; without it, an attacker can cut the response.
The clients of `ache-eio-tls`, `ache-lwt-tls` and `ache-miou-tls` report
such a response as cut: the body gets `on_error`, and the error handler gets
`` `Exn Ache_eio_tls.Truncated ``, `` `Exn Ache_lwt_tls.Truncated `` or
`` `Exn Ache_miou_tls.Truncated ``. In
every other case the end of TCP without `close_notify` counts as the end of
the input, as without TLS: a complete response with `Content-Length` or
chunked framing is not an error, and a request that got no byte of its
response gets `` `Connection_closed ``, so that the application can retry
it. `Ache_eio` also accepts a `Tls_eio` flow directly,
but `tls-eio` 2.1.5 does not tell `close_notify` from the end of TCP, so a
client on such a flow reports a cut response as complete. A request body
always has an explicit length, so a server reports a cut body without this
check.

Timeouts: when `?idle_timeout` or `?head_timeout` is set (with `?clock` for
Eio), the TLS handshake must be complete within the smaller of them. Without
either, the handshake has no time limit. A client that sends plain HTTP to a
TLS port gets no answer, because ocaml-tls waits for the rest of what it
reads as a TLS record, so without a timeout the connection stays open until
the client closes it.

## Protocol behavior

### Requests the server rejects

The server calls the error handler, sends the error response with
`connection: close`, and closes the connection. Requests that came before it
on the same connection get their responses first.

| Request | Response |
|---|---|
| A bare LF or a bare CR as a line end, obs-fold, a field name that is not a token, a CTL (for example NUL) in a field value | 400 |
| A request line without a version | 400 |
| An HTTP/1.1 request without `Host`, two `Host` fields, a `Host` value that is not `uri-host [":" port]` | 400 |
| A target form that does not fit the method: `*` with a method other than OPTIONS; without CONNECT, a target that is neither `/...` nor `scheme:...` (an authority such as `example.com:80` reads as a URI with scheme `example.com` and is accepted; `127.0.0.1:80` or `[::1]:80` is not) | 400 |
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
- `Request_descriptor.respond_with_informational` sends `100 Continue`,
  `103 Early Hints` or another interim response before the final response.
- `Request_descriptor.respond_with_upgrade` on a `CONNECT` request sends a
  2xx (`200` unless `?status` says otherwise) and gives the connection to
  the tunnel, with the bytes that came after the `CONNECT` head. A 2xx
  response to `CONNECT` sent with another `respond_with_*` function is sent
  as it is, then the connection closes; the bytes after the request are not
  read. Another response to `CONNECT` lets the connection serve the next
  request.
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
  request body that the application did not read. It waits for the rest of
  the body at most `?idle_timeout` (see [Limits](#limits)).

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

## How it works

This section is for readers of the code and for people who compare ache with
other servers. Each runtime page continues it in its own "How it works"
section.

### Two layers

```
  your request_handler / error_handler
              │  Request_descriptor.t, Body, Headers
              ▼
  ┌────────────────────────────────────────┐
  │ ache (lib/)          no I/O, no clock  │
  │  Server_connection / Client_connection │
  │  parser · framing · keep-alive ·       │
  │  pipelining · serializer · timeouts    │
  └───────────────────┬────────────────────┘
                      │  next_read_operation / next_write_operation
                      │  read · read_eof · report_write_result
                      │  yield_reader · yield_writer
  ┌───────────────────┴────────────────────┐
  │ ache-eio │ ache-lwt │ ache-miou         │
  │ sockets, a read loop and a write loop,  │
  │ timers, TLS                             │
  └───────────────────┬────────────────────┘
                      ▼
                    kernel
```

The core keeps all the HTTP state of a connection. It never opens, reads or
writes a socket. A runtime asks it what to do next, does that I/O, and gives
the result back. So one core runs on Eio, Lwt and Miou, and the core tests
need no sockets.

### The loops

Each connection has two loops: one reads and one writes. The shape is the
same in all three runtimes.

```
      read loop                                  write loop
  ┌────────────────────┐                  ┌────────────────────┐
  │next_read_operation │                  │next_write_operation│
  └─┬────────┬───────┬─┘                  └─┬────────┬───────┬─┘
  `Read   `Yield  `Close                `Write   `Yield  `Close
    │        │       │                  iovecs     │       │
    ▼        │       ▼                     │       │       ▼
 read(2) into│   shut down                 ▼       │   loop ends
 the read    │   receiving             writev(2),  │
 buffer      │   loop ends             one call    │
    │        │                         for all the │
    ▼        │                         buffers     │
 core.read:  │                             │       │
 parse heads │                    report_write_result
 and bodies, │                             │       │
 call the    │                        back to the top
 request_handler                                   │
    │        ▼                                     ▼
 back to  yield_reader:                    yield_writer:
 the top  wait until the core              wait until the core
          wants more input                 has output
```

- `Read`: the runtime reads from the socket into the read buffer and gives
  the bytes to the core. While it parses, the core calls the
  `request_handler` with a `Request_descriptor.t` for each request head.
- `Write`: the core hands over a list of buffers (the response head and body
  parts). The runtime sends them with one system call and reports how many
  bytes went out.
- `Yield`: the core has nothing to do now. For example, the write loop waits
  because the handler has not answered yet, or the read loop waits because a
  response will close the connection and the core reads no more requests.
  The loop gives the core a callback and stops. The core calls it when its
  state changes, and the loop starts again.
- `Close`: the loop ends. When both loops have ended, or one has failed, the
  runtime closes the socket once.

### One write for each read

A client can send several requests in one packet (pipelining). While the core
parses one read, it holds the writer (`hold` and `release` in
`lib/serialize.ml`). The responses to all the requests in that read are queued
and leave in one write when the read is done. While more than
`Config.max_pending_output` bytes (64 KiB) wait to be written, the core
answers no more pipelined requests and reads no new request, so a client
that pipelines and does not read cannot make the server buffer without
limit. Without the hold, a runtime
that wakes the writer at once, such as Lwt, would make one write call for each
response.

### Timeouts

The core decides which limit applies to the next read
(`Server_connection.read_timeout`):

- the head timeout, while a request head is only partly received;
- the idle timeout, on a keep-alive connection that waits for its next
  request;
- the idle timeout, once the response is complete and the rest of the
  request body has not arrived;
- none, while a request is in progress and its response is not complete,
  for example while a body arrives for a handler that has not answered.

The runtime owns the clock and starts the timer. When the timer fires first,
the core sends `408`, or closes the connection (a request body that has not
ended gets `on_error`). Without `?idle_timeout` and `?head_timeout` (and, in
`ache-eio`, without `?clock`), no timer is used, and the runtime does not
wait for the rest of a body after its response: it closes the connection
at once.

A read can start before the response is complete, and the response can
complete while the read waits. Each time the write loop waits or ends, it
tells the read loop, which then asks the core for the limit again.

### Where the time of a request goes

For a small `GET` with a small response on one core, most of the time is in
the kernel and the event loop, not in the HTTP code. In one measurement,
`ache-eio` used about 2 µs of CPU in user space and about 5–6 µs in the
kernel for each request. A faster parser moves such a benchmark only a
little. Fewer system calls, fewer fiber switches and fewer objects that live
past a minor GC move it more.

### Compared with Node.js

The shape is close to Node's HTTP server: one thread, an event loop with
non-blocking sockets, and a parser that does no I/O (llhttp in Node). Node
also batches the writes of one tick (`socket.cork`), and groups its timers by
duration.

The main difference is the direction. In Node the socket pushes data to the
parser (`'data'` events), and the server pauses the socket to slow down. In
ache the runtime pulls: it asks the core for the next operation, so the core
decides when it reads more. The rules for keep-alive and pipelining are in
the core, not in the runtime, which is why the same core runs on three
runtimes.

## Limits

- HTTP/1.0 and HTTP/1.1 only. There is no HTTP/2 or HTTP/3.
- TLS is ocaml-tls only (see [TLS](#tls)): there is no OpenSSL runtime.
- Trailer fields are read and discarded. The API does not give them to the
  application.
- Server timeouts are off by default. `?idle_timeout` closes a connection
  that waits for a request, or for the rest of a request body after its
  response. `?head_timeout` answers 408 to a request head that is not
  complete in time. A request body is not timed out before its response is
  complete, and a response in progress is never timed out.
- After the response, the server waits for the rest of the request body at
  most `?idle_timeout`. Without `?idle_timeout` it does not wait: the body
  gets `on_error` and the connection closes. This applies to a response that
  closes the connection and to a keep-alive response, such as a 413, sent
  before the body was read.
- Once its write side has closed, the server reads and dispatches no new
  request; the request in progress still gets its body, within the limit
  above.
- While more than `Config.max_pending_output` bytes of responses wait to be
  written, the server reads no new request.
- The API is not stable before 1.0.

## Testing

`dune test` runs these suites (counts from `dune test --force` on
2026-11-15):

| Suite | Tests |
|---|---|
| `lib/test`: core unit tests | 280 |
| `lib/test/rfc`: RFC 9110 | 159 |
| `lib/test/rfc`: RFC 9112 | 178 |
| `adapters/eio/test`: Eio on loopback sockets | 33 |
| `adapters/eio/tls/test`: `ache-eio-tls` on loopback sockets | 34 |
| `adapters/lwt/test`: Lwt on loopback sockets | 25 |
| `adapters/lwt/tls/test`: `ache-lwt-tls` on loopback sockets | 24 |
| `adapters/miou/test`: Miou on loopback sockets | 28 |
| `adapters/miou/tls/test`: `ache-miou-tls` on loopback sockets | 33 |

The RFC suites start from each sentence of RFC 9110 and RFC 9112 §2–§11 that
has a requirement keyword (MUST, SHOULD, MAY and others), plus definitions
that a parser must follow. `lib/test/rfc/COVERAGE.md` and
`lib/test/rfc/COVERAGE_9112.md` give each requirement its tests, or the reason
it is out of scope:

| RFC | Requirements | Tested | Out of scope |
|---|---|---|---|
| 9110 | 445 | 93 | 352 |
| 9112 | 173 | 115 | 58 |

No test is marked as a known failure (`lib/test/rfc/KNOWN_FAILURES*.md`). The
README check makes sure that the code in the three runtime pages is the same
as the compiled examples.

`conformance/` runs the [h1spec](https://github.com/uNetworking/h1spec)
requests against Eio, Lwt and Miou echo servers. On 2026-11-16, h1spec at
`f0a5650` (under Node 22 with `conformance/deno_shim.mjs`) passed 33 of 33
checks on all three servers.

## License

BSD-3-Clause, with the copyright lines of httpaf and httpun. See
[LICENSE](../LICENSE).
