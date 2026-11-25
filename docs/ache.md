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
- A WebSocket server (RFC 6455) for text and binary messages, on every
  runtime (see
  [WebSocket](#websocket)).

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
it when its method is idempotent. `Ache_eio` also accepts a `Tls_eio` flow directly,
but `tls-eio` 2.1.5 does not tell `close_notify` from the end of TCP, so a
client on such a flow reports a cut response as complete. A request body
always has an explicit length, so a server reports a cut body without this
check.

Timeouts: the TLS handshake must be complete within the smaller of
`?idle_timeout` and `?head_timeout` (30 s by default). With both off, the
handshake has no time limit. A client that sends plain HTTP to a
TLS port gets no answer, because ocaml-tls waits for the rest of what it
reads as a TLS record, so without a timeout the connection stays open until
the client closes it.

## Protocol behavior

### Requests the server rejects

The server calls the error handler, sends the error response with
`connection: close`, and closes the connection. Requests that came before it
on the same connection get their responses first. For a 400 the error handler
gets `` `Bad_request reason ``, a short text for logs such as
`"unexpected ' ' in field line"` or `"invalid Content-Length"`. The default
error handler does not send it.

| Request | Response |
|---|---|
| A bare LF or a bare CR as a line end, obs-fold, a field name that is not a token, a CTL (for example NUL) in a field value | 400 |
| A request line without a version | 400 |
| A target byte outside RFC 3986: `#`, a byte above 0x7F, a CTL, or one of `"` `<` `>` `{` `}` `\|` `^` `` ` `` `\` | 400 |
| An absolute-form target with userinfo (`http://u:p@host/`) or an empty host (`http:///x`, `http://:80/`) | 400 |
| An HTTP/1.1 request without `Host`, two `Host` fields, a `Host` value that is not `uri-host [":" port]`, a `Host` with an empty host (`:80`) | 400 |
| A target form that does not fit the method: `*` with a method other than OPTIONS; without CONNECT, a target that is neither `/...` nor `scheme:...` (an authority such as `example.com:80` reads as a URI with scheme `example.com` and is accepted; `127.0.0.1:80` or `[::1]:80` is not) | 400 |
| A CONNECT target that is not `host:port` with a port of 0 to 65535 | 400 |
| A `Content-Length` that is not one repeated `1*DIGIT` value, such as `+3` or `3, 4` | 400 |
| Both `Content-Length` and `Transfer-Encoding`; `Transfer-Encoding` in an HTTP/1.0 request; `chunked` that is not the final coding or is there twice | 400 |
| A chunked body whose chunk lines and extensions exceed its data by more than 16 KiB: each chunk line counts its bytes, less 16 and twice the chunk size (Go's budget) | 400 |
| A transfer coding other than `chunked`, as in `gzip, chunked` | 501 |
| A method longer than `Config.max_header_size` | 501 |
| A target that makes the request line longer than `max_header_size` | 414 |
| A head longer than `max_header_size` (default 16 KiB) | 431 |
| A head that is not complete when `?head_timeout` expires (30 s by default) | 408 |
| A major version other than 1 | 505 |

ache accepts HTTP/1.x with x > 1 as HTTP/1.1, any number of empty lines
before the request line (they count against `max_header_size`, as part of
the head), chunk extensions (ignored) and trailer fields (see
[Trailers](#trailers)). httpun rejected chunk extensions and trailers.

A `CONNECT` request has no body (RFC 9110 §9.3.6): the server ignores its
`Content-Length` and `Transfer-Encoding` fields, as llhttp does, and does
not check them. The bytes after the head belong to the tunnel.

### Responses

- ache adds `Content-Length` to a string or bigstring response, and
  `Transfer-Encoding: chunked` to a streaming response on an HTTP/1.1
  persistent connection. Otherwise a streaming body ends when the connection
  closes.
- A response to `HEAD`, and a 1xx, 204 or 304 response, has no body.
- `Server_connection.create ~date` adds `Date: (date ())` after the other
  fields of each response that is not 1xx and has no `Date` field, error
  responses included (RFC 9110 §6.6.1). A `Date` set by the application is
  kept. Without `~date` the core adds no `Date`. The servers of `ache-eio`,
  `ache-lwt` and `ache-miou` pass the current time by default, formatted
  once per second in each domain; `~date:false` turns it off.
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
  With `Config.auto_continue` (default `false`), the first
  `Body.Reader.schedule_read` on the body of an HTTP/1.1 request with
  `Expect: 100-continue` sends `100 Continue`, unless a response has started
  or a `100 Continue` was already sent. A handler that responds without
  reading the body sends no `100 Continue`.
- `Request_descriptor.respond_with_upgrade` on a `CONNECT` request sends a
  2xx (`200` unless `?status` says otherwise) and gives the connection to
  the tunnel, with the bytes that came after the `CONNECT` head, also when
  the request has `Content-Length` or `Transfer-Encoding`. Any other
  response to `CONNECT` closes the connection, and the bytes after the
  request are not read: they may be tunnel data. A 2xx is sent as it is,
  another status gets `connection: close`.
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
  request body that the application did not read or closed, within the
  body timeout (see [Timeouts](#timeouts)), and reads at most
  `Config.max_discard_body` (256 KiB) of it. A response sent while more
  than that remains of a `Content-Length` body has `connection: close`; a
  chunked body that passes the limit closes the connection after the
  response. httpun read the whole body, so a 413 to a 10 GB upload read
  10 GB.

### Trailers

A chunked body can end with trailer fields, such as the `grpc-status` of
gRPC. On both sides:

- `Body.Reader.trailers body` returns the received trailer fields. It is
  `Headers.empty` until `on_eof` is called, so read it in `on_eof`.
- `Body.Writer.close ~trailers body` sends trailer fields after the last
  chunk. A server sends them only when the request has `TE: trailers`, and
  drops them otherwise; a client always sends them. On a close-delimited
  body, or a response with no body (`HEAD`, 204, 304), they are dropped.
- Fields that frame, route or authenticate a message (`content-length`,
  `transfer-encoding`, `host`, `trailer`, `te`, `connection`,
  `authorization`, `content-type`, and the rest of Go's `badTrailer` list)
  are dropped when received. `Body.Writer.close ~trailers` raises
  `Invalid_argument` for them, for an invalid field, and on a body framed
  by `Content-Length`. A `Trailer` field in the head is ignored.
- The trailer section counts against `Config.max_header_size` on its own;
  a larger one ends the body with `on_error`, and the server answers 431.

<!-- example: examples/readme/trailers_example.ml -->
```ocaml
open Ache

(* The server streams a gRPC response and sends its status in trailers. *)
let request_handler reqd =
  let body =
    Request_descriptor.respond_with_streaming
      reqd
      (Response.create
         ~headers:(Headers.of_list [ "content-type", "application/grpc" ])
         `OK)
  in
  Body.Writer.write_string body "\000\000\000\000\002hi";
  Body.Writer.close
    ~trailers:(Headers.of_list [ "grpc-status", "0"; "grpc-message", "OK" ])
    body

(* The client reads grpc-status when the response body ends. *)
let response_handler status _response body =
  let rec read () =
    Body.Reader.schedule_read
      body
      ~on_read:(fun _ ~off:_ ~len:_ -> read ())
      ~on_error:ignore
      ~on_eof:(fun () ->
        status := Headers.get (Body.Reader.trailers body) "grpc-status")
  in
  read ()

let () =
  let request =
    Request.create
      ~headers:
        (Headers.of_list
           [ "host", "a"; "te", "trailers"; "content-length", "0" ])
      `POST
      "/helloworld.Greeter/SayHello"
  in
  let client = Client_connection.create () in
  let status = ref None in
  let _ : Body.Writer.t =
    Client_connection.request
      client
      request
      ~error_handler:ignore
      ~response_handler:(response_handler status)
  in
  let output =
    Testing.serve
      request_handler
      "POST /helloworld.Greeter/SayHello HTTP/1.1\r\n\
       Host: a\r\n\
       TE: trailers\r\n\
       Content-Length: 0\r\n\
       \r\n"
  in
  let len = String.length output in
  let read =
    Client_connection.read
      client
      (Bigstring.of_string output ~off:0 ~len)
      ~off:0
      ~len
  in
  assert (read = len);
  assert (!status = Some "0")
```

### Client

- The new error `` `Connection_closed `` goes to each request that got no
  byte of its response before the connection stopped reading. httpun called
  no handler for these requests, or gave them
  `` `Malformed_response "unexpected eof" ``.
- `Config.max_pipelined_requests` (default 1) limits the requests that wait
  for a response on one connection: by default a request is written after
  the previous response is complete. httpun pipelined without a limit. A
  request with a non-idempotent method, or a `CONNECT` or `Upgrade` request,
  is never written while another response is pending, and the requests
  behind it wait too. A request that gets `` `Connection_closed `` may have
  reached the server: retry it only when its method is idempotent.
- A 204 or 304 response (not to `HEAD`) with `Transfer-Encoding` or a
  `Content-Length` other than 0 has no body and closes the connection after
  it.
- Response bytes that arrive when no request waits for a response close the
  connection, also before their head is complete. On an idle connection,
  `Client.request` first looks, without blocking, for such bytes that a read
  returned and the connection has not handled yet, or that wait on the
  socket, and for the end of input; if there are some, the connection closes
  and the request gets `` `Connection_closed `` instead of them as its
  response (undici GHSA-35p6-xmwp-9g52). `Ache_eio.Client.request` yields
  twice first, so that a read that io_uring completed returns. The socket is
  looked at by `ache-lwt` and `ache-miou`, and by `ache-eio` on a
  non-blocking descriptor (`eio_posix`). The TLS clients of
  `ache-lwt-tls`, `ache-eio-tls` and `ache-miou-tls` read the TLS records
  that wait on the socket, without blocking, and decrypt them: application
  data other than empty lines, a `close_notify` or the end of TCP closes
  the connection; a session ticket or a key update does not, and decrypted
  bytes are kept for the next read. On a blocking descriptor (Lwt) or with
  io_uring (Eio) they look only at the records that reads returned.
- Interim 1xx responses other than 101 are dropped. After a 101, or a 2xx
  response to `CONNECT`, the client stops reading HTTP and `upgrade` gives
  the bytes after the head to the new protocol.
- An obs-fold in a response field value becomes one space. Empty lines
  before a response are ignored. A status-line with CRLF right after the
  status code (`HTTP/1.1 301\r\n`) has an empty reason phrase.
- A response whose `Transfer-Encoding` lists no coding gives
  `` `Malformed_response ``.
- A chunked response body over the chunk overhead budget of the server
  table above gives `` `Malformed_response ``.
- `Client_connection.request` raises `Invalid_argument` for a method or field
  name that is not a token, a CR, LF or NUL in a field value, a CR, LF,
  NUL or space in the target, two `Host` fields, both `Content-Length` and
  `Transfer-Encoding`, an invalid `Content-Length`, and a
  `Transfer-Encoding` whose last coding is not `chunked`. Later requests are
  still sent.

## WebSocket

`Ache.Websocket` is a WebSocket server (RFC 6455) for text and binary
messages. It runs
after a `101 Switching Protocols` response on the same connection, so it
works on Eio, Lwt and Miou, with or without TLS, and needs no runtime code.
`ache-lwt` adds a promise API on top of it (see
[WebSocket in ache-lwt.md](ache-lwt.md#websocket)).

### Upgrade a request

Call `Websocket.upgrade` in the request handler. It checks the opening
handshake (RFC 6455 §4.2.1):

- a `GET` request of HTTP/1.1 or later;
- a `websocket` token in `Upgrade` and an `upgrade` token in `Connection`,
  compared without case;
- one `Sec-WebSocket-Version` field with the value `13`;
- one `Sec-WebSocket-Key` field with the canonical base64 encoding of 16
  bytes (its padding bits are zero, RFC 4648 §3.5).

When the handshake is valid, `upgrade` calls your function with the new
connection, responds with `101`, `Upgrade: websocket`, `Connection: Upgrade`
and `Sec-WebSocket-Accept`, and returns `Ok ()`. Your function returns the
three callbacks: `on_message` gets each whole message, `Text s` or
`Binary s`; `on_pong` gets the payload of each pong; and `on_close` is called
once when the connection ends. When the handshake is not valid,
`upgrade` responds nothing and returns the error, so that you send the
response. `Websocket.check_handshake` does the same check without the
upgrade.

ache does not check `Origin`, cookies or tokens. Check them before you call
`upgrade`.

This handler echoes each message and closes the connection when the client
sends `bye`:

```ocaml
open Ache

let respond_empty reqd ?(headers = []) status =
  let headers = Headers.of_list (("content-length", "0") :: headers) in
  Request_descriptor.respond_with_string
    reqd
    (Response.create ~headers status)
    ""

let request_handler _client_address reqd =
  let result =
    Websocket.upgrade reqd (fun ws ->
      { on_message =
          (function
            | Text "bye" -> Websocket.close ~reason:"bye" ws ~code:1000
            | Text message -> Websocket.send_text ws message
            | Binary message -> Websocket.send_binary ws message)
      ; on_pong = ignore
      ; on_close = (fun _close -> ())
      })
  in
  match result with
  | Ok () -> ()
  | Error `Unsupported_version ->
    respond_empty
      reqd
      ~headers:[ "sec-websocket-version", "13" ]
      `Upgrade_required
  | Error (`Not_get | `Not_websocket | `Invalid_key) ->
    respond_empty reqd `Bad_request
```

### Choose a subprotocol

A client can offer subprotocols in `Sec-WebSocket-Protocol`, for example
`new WebSocket(url, ["chat"])` in a browser. ache has none of its own: read
the offered ones with `Websocket.requested_protocols request` (the members of
all `Sec-WebSocket-Protocol` fields, in order), choose one, and answer it with
`?headers` of `upgrade`. `upgrade` adds `headers` to the `101` after its own
fields. A browser that offers protocols fails the connection when the `101`
names none of them, so answer only when you support one, and reject the
request otherwise if your service needs a subprotocol.

```ocaml
open Ache

let upgrade_chat reqd make_handler =
  let request = Request_descriptor.request reqd in
  let headers =
    if List.mem "chat" (Websocket.requested_protocols request)
    then Headers.of_list [ "sec-websocket-protocol", "chat" ]
    else Headers.empty
  in
  Websocket.upgrade ~headers reqd make_handler
```

`headers` cannot set `Upgrade`, `Connection` or `Sec-WebSocket-Accept`, in
any case: `upgrade` owns them and raises `Invalid_argument`, as it does for
an invalid field. It raises before it checks the handshake and writes
nothing.

### Send messages

`send_text` queues one text frame, and `send_binary` one binary frame. The
connection writes them after the `101`, also when you call them from your
function before the `101` is written. `send_text` does not check that the
message is valid UTF-8. To know when the
frames are written, call `Websocket.flush` with a callback: it gets
`` `Written `` when all frames queued before it are written, or `` `Closed ``
when the connection closed first.

### What the connection does

- It answers a ping with a pong that has the same payload.
- It gives a fragmented message to `on_message` in one piece.
- When the client sends a close frame, it sends a close frame with the same
  code, then closes the connection. `on_close` gets the client's code and
  reason, or `Some { code = 1005; reason = "" }` for a close frame without a
  code.
- `Websocket.close ?reason ws ~code` sends a close frame, stops reading, and
  closes the connection when the frame is written. It does not wait for the
  client's close frame. The reason must be valid UTF-8 and at most 123
  bytes.
- `Websocket.ping ws payload` sends a ping; the client's pong reaches
  `on_pong`. To find a client that is gone, ping it on a timer and close the
  connection when no pong comes in time.
- It closes the connection with a code when the client breaks the protocol.
  `on_close` then gets `None`:

| Code | Cause |
|---|---|
| 1002 | An unmasked frame, an unknown opcode, a reserved bit, a control frame that is fragmented or longer than 125 bytes, a continuation frame without a message, a new message inside a fragmented one, a length with the high bit set or not in its shortest form, or a close frame with a 1-byte payload or an invalid code |
| 1007 | A text message or close reason that is not valid UTF-8 |
| 1009 | A message longer than `max_message_size` |

`?max_message_size` of `upgrade` limits the length of a message, all
fragments together. The default is `Websocket.default_max_message_size`,
1 MiB. The connection checks the length in the frame header, before it reads
the payload. Control frames do not count.

Text is checked as UTF-8 while it arrives, across fragments: the first byte
that cannot be valid UTF-8 closes the connection with 1007, without reading
the rest of the message. Binary messages are not checked.

### Backpressure

The connection stops reading while more than 64 KiB of frames are queued
and not written, and starts again when the writes bring the queue down to
64 KiB. A client that sends pings and does not read the pongs cannot make the
queue grow without limit, and no pong is dropped.

To stop `on_message` calls, for example while your own queue is full, call
`Websocket.pause_reading`, then `Websocket.resume_reading`. Frames that the
current read already holds still reach `on_message`. While reading is
paused, a ping gets no pong and the connection does not see the end of the
input.

### When the client is gone

When a write fails because the client closed or reset the connection, the
connection drops the frames it has not written, calls `on_close None`, and
gives `` `Closed `` to each pending `flush`. Later calls to `send_text` do
nothing, and `flush` gets `` `Closed `` at once, so a send loop that stops
on `` `Closed `` ends. The connection writes nothing more, so the
runtime loop ends and closes the socket. httpun-ws kept writing after this
error, which made the server use 100% CPU
([dream#467](https://github.com/camlworks/dream/issues/467)). Each runtime
has a test for this case: `adapters/eio/test`, `adapters/lwt/test` and
`adapters/miou/test` reset the connection while the server sends, and check
that the connection ends within 1 second.

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
  request, and on a WebSocket connection that reads frames;
- the body timeout, while the connection waits for request body bytes: the
  application has scheduled a read on the body, or the response is complete
  and the rest of the body has not arrived;
- none, while no read waits for the peer, for example while the handler
  runs and has not asked for the body, or after an upgrade to a protocol
  other than WebSocket.

The runtime owns the clock and starts the timer, and asks again before each
read. When the timer fires first, it calls the action that the core gave
for the state at that time: `408` for a head; for a body, `on_error` and
then `408` if no response has started, or a close; a close for an idle
connection, with close code 1001 on a WebSocket.

The runtimes count the body and write limits as progress, after warp and
Cowboy: the body clock runs only while the connection waits for body bytes,
the write clock only while a write waits, and each starts again from zero
after 2048 bytes. A peer that trickles one byte at a time cannot keep a
connection open, and a handler that is slow to answer is never timed out.
A write that makes no progress closes the connection; the core sees it as
a failed write (`` `Closed ``).

All four timeouts are on by default in every runtime: head 30 s, idle,
body and write 60 s. Passing `None` turns one off.

### Keep-Alive

A client cannot see when a server closes an idle connection. If it sends a
request just as the server closes, the request is lost, and it cannot
always be retried. ache announces the limit and keeps a margin on both
sides (Node's `keepAliveTimeoutBuffer`, undici's reading of the hint):

- The server: with an idle timeout of 1 s or more, every response after
  which the connection stays open gets `Keep-Alive: timeout=n`, `n` being
  the idle timeout rounded down (`Server_connection.create
  ?keep_alive_timeout`), with `, max=m` when
  `Config.max_requests_per_connection` is set. A response that closes the
  connection, a 1xx, a tunnel, or a response whose handler set
  `Keep-Alive` itself gets none. The runtimes then wait 1 s longer than
  they announced before they close an idle connection. With the idle
  timeout off there is no hint.
- The client: after a response with `Keep-Alive: timeout=n` that keeps the
  connection open, `Client_connection.reuse_timeout` gives `n - 2` seconds
  (`n` at most 600). The runtimes count from the end of the response and,
  when that time has passed with no request started, close the
  connection; a request after it gets `` `Connection_closed `` and nothing
  is written. Without the hint a connection is reused as before.

### Graceful stop

`Server_connection.drain` makes the request in progress the last one on a
connection. An idle connection closes at once: the core shuts down, and the
runtime ends a read that waits for the next request, as when the idle
timeout fires. A connection with a request in progress, or with a head
partly read, serves that request and reads no other; its response gets
`Connection: close` if its head is not written yet, and requests pipelined
after it are not served. A WebSocket connection sends a close frame with
code 1001. `drain` is not thread-safe: call it where the connection runs.

The runtimes call `drain` for you. Their servers take a `stop` signal and a
`grace` period in seconds: when `stop` resolves, `Server.run` (Eio, Miou)
stops accepting, every connection drains, and what is left closes `grace`
seconds later. With Lwt the caller stops its accept loop. See
[ache-eio](ache-eio.md#graceful-stop),
[ache-lwt](ache-lwt.md#graceful-stop) and
[ache-miou](ache-miou.md#graceful-stop).

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
- Trailer fields named in Go's `badTrailer` list are dropped on receipt
  and refused on send. The server sends trailer fields only to a request
  with `TE: trailers` (see [Trailers](#trailers)).
- Server timeouts are on by default (see [Timeouts](#timeouts)): 30 s for a
  request head, 60 s for an idle connection, and 60 s without progress for
  a request body or a write. `None` turns one off. With
  `~body_timeout:None` the server waits without a time limit for the rest
  of an unread request body (at most `Config.max_discard_body` of it); with
  `~write_timeout:None` a client that stops reading keeps the connection
  open.
- After the response, the server reads the rest of the request body
  within the body timeout, to find the next request. This applies to a
  response that closes the connection and to a keep-alive response, such
  as a 413, sent before the body was read.
- After the response, the server reads at most `Config.max_discard_body`
  (256 KiB) of a request body the application did not read or closed, then
  closes the connection.
- Once its write side has closed, the server reads and dispatches no new
  request; the request in progress still gets its body, within the limits
  above.
- While more than `Config.max_pending_output` bytes of responses wait to be
  written, the server reads no new request.
- WebSocket: server only. There are no extensions. The application chooses
  a subprotocol and answers it with `?headers` of `upgrade`. A WebSocket
  connection that receives nothing for the idle timeout is closed with
  code 1001, and one whose writes make no progress for the write timeout
  is closed.
- The API is not stable before 1.0.

## Test a handler

`Ache.Testing` runs a request handler without sockets and without a
runtime, so a unit test can call it directly:

- `Testing.serve handler input` gives the bytes `input` to a server
  connection, then the end of input, and returns every byte the connection
  writes. `input` can hold several pipelined requests.
- `Testing.serve_request ?body handler request` writes `request` and `body`
  as a client does (it adds `Content-Length` when the request has no framing
  field), and returns the final response and its decoded body: a chunked
  body is decoded, and `1xx` responses are dropped. It raises `Failure` when
  no complete response comes.

An exception that the handler raises gets the error handler's response,
`500` by default, as with a runtime.

<!-- example: examples/readme/testing_example.ml -->
```ocaml
open Ache

let request_handler reqd =
  let request = Request_descriptor.request reqd in
  Request_descriptor.respond_with_string
    reqd
    (Response.create `OK)
    ("Hello, " ^ request.target)

let () =
  let output =
    Testing.serve request_handler "GET /a HTTP/1.1\r\nHost: a\r\n\r\n"
  in
  assert (output = "HTTP/1.1 200 OK\r\ncontent-length: 9\r\n\r\nHello, /a");
  let response, body =
    Testing.serve_request
      request_handler
      (Request.create ~headers:(Headers.of_list [ "host", "a" ]) `GET "/b")
  in
  assert (response.status = `OK);
  assert (body = "Hello, /b")
```

`serve` and `serve_request` return when the input is used up. They see
only what the handler does before that: a response that the handler gives
later, for example when a promise resolves, is not in their result. For
such a handler, drive the connection step by step: `Testing.create`, then
`Testing.feed t bytes`, run your scheduler until the handler has responded,
then `Testing.output t` returns the bytes written since the last call.
`Testing.feed_eof t` ends the input, and `Testing.is_closed t` tells when
the connection has closed. The handler itself must return without
blocking, as with every runtime.

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
| 9110 | 445 | 101 | 344 |
| 9112 | 174 | 117 | 57 |

No test is marked as a known failure (`lib/test/rfc/KNOWN_FAILURES*.md`). The
README check makes sure that the code in the three runtime pages is the same
as the compiled examples.

`conformance/` runs the [h1spec](https://github.com/uNetworking/h1spec)
requests against Eio, Lwt and Miou echo servers. On 2026-11-16, h1spec at
`f0a5650` (under Node 22 with `conformance/deno_shim.mjs`) passed 33 of 33
checks on all three servers.

`conformance/autobahn.sh` runs the
[Autobahn test suite](https://github.com/crossbario/autobahn-testsuite)
(fuzzingclient, Docker image pinned by digest, `--network host`, so Linux
only) against the WebSocket echo of `conformance/ws_echo.ml` with a
`max_message_size` of 16 MiB. It skips cases 12 and 13, which test
permessage-deflate: ache negotiates no extension. Run it from an opam
environment with Docker and Node:

```sh
RUNTIME=lwt sh conformance/autobahn.sh   # RUNTIME=eio (default), lwt or miou
```

The report goes to `_build/autobahn/<runtime>/index.html`. The script fails
when a case does worse than `conformance/autobahn-expected.txt` allows. On
2026-11-25 all three servers passed 298 of the 301 cases, and 3 were
informational (7.1.6, 7.13.1, 7.13.2), which Autobahn does not grade.

`conformance/wpt.sh` runs `conformance/wpt/ache.any.js` in a browser with
[web-platform-tests](https://github.com/web-platform-tests/wpt). It is not
run in CI. The test uses `fetch` for a GET with a JSON body, a UTF-8 POST
body, HEAD, a redirect, a simple and a preflighted CORS request and 20
concurrent requests, and a browser `WebSocket` for a text and a binary echo
and a close that the server starts. The script copies the test into the WPT
checkout, starts the server on `ACHE_PORT` (default 8090), runs `wpt run`,
then stops the server and removes the copy. With `WPT_BROWSER=chromium`, the
default, it runs the Chromium on `PATH` as wpt's `chrome` product, which
needs no WPT hosts in `/etc/hosts`; give it a ChromeDriver of the same major
version:

```sh
git clone --depth 1 https://github.com/web-platform-tests/wpt /tmp/wpt
WPT_ROOT=/tmp/wpt RUNTIME=eio sh conformance/wpt.sh \
  --webdriver-binary /path/to/chromedriver --headless
```

Other arguments go to `wpt run`. On 2026-11-25, with Chromium 156, the 10
subtests passed on the three servers.

`lib/test/llhttp` runs the request and response cases of the
[llhttp](https://github.com/nodejs/llhttp) test corpus (`5aa5ac1`, MIT)
through `Server_connection` and `Client_connection`, at every split of the
input, and compares accept or reject, complete messages and body bytes with
llhttp's log. An HTTP/1.1 request without `Host` gets `Host: a`; a
`*-lenient-*` case must be rejected; pause and finish cases are not run. On
2026-11-25, 162 of 192 strict and 35 of 40 lenient cases agreed.
`lib/test/llhttp/expected-differences.txt` gives each difference its reason,
and the test fails on a difference not listed there.

`lib/test/fuzz` generates inputs with
[QCheck](https://github.com/c-cube/qcheck): requests, responses, chunked
bodies and WebSocket frames, from the grammar corpus, the oracle inputs and
mutations of both. It checks properties, not fixed outputs: a server or
client connection gives the same handler calls, body bytes, errors and output
for every split of its input, and raises nothing; what ache writes, with
partial writes, ache reads back the same; the chunk-size, Content-Length,
field value and trailer parsers agree with their grammar; a WebSocket text
message is delivered when it is valid UTF-8. `dune test` runs a fixed seed in
about 2 seconds. For a longer run:

```sh
ACHE_FUZZ_SEED=7 ACHE_FUZZ_SCALE=10 dune exec lib/test/fuzz/fuzz.exe
```

`lib/test/fuzz/known_bugs.ml` lists the bugs it found that are not fixed
(none now), and runs the minimal input of each fixed one.

## License

BSD-3-Clause, with the copyright lines of httpaf and httpun. See
[LICENSE](../LICENSE).
