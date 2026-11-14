ache Unreleased
--------------

Fork of httpun at 37fdcd8 (httpun 0.2.0 and 10 commits).

### Breaking

- Packages: `httpun` and `httpun-types` become `ache`, `httpun-eio` becomes
  `ache-eio`, and `httpun-lwt` and `httpun-lwt-unix` become `ache-lwt`.
  `httpun-async` and `httpun-mirage` are removed.
- No gluten dependency. Request handlers in the runtimes take `Reqd.t` in
  place of `Reqd.t Gluten.reqd`. The new `Ache.Runtime` has `module type S`,
  the interface a runtime drives, and `Runtime.make` to pack a protocol as a
  `Runtime.impl`.
- `Reqd.respond_with_upgrade` takes a `Runtime.impl` in place of a
  `unit -> unit` callback. When the 101 is written, the connection hands the
  socket and its unread input to that protocol. `Ache_eio.Client.upgrade` and
  `Ache_lwt.Client.upgrade` take a `Runtime.impl`.
- `ache-lwt`: `Make (Io)` over the new `module type IO` replaces the functors
  over gluten runtimes. `Server` and `Client` run on `Lwt_unix.file_descr`.
  The `TLS` and `SSL` modules are removed: TLS is in the new package
  `ache-lwt-tls`, and there is no OpenSSL runtime.
- `ache-eio`: `Server.create_connection_handler` takes the flow before the
  address and no longer takes `~sw`, so that
  `create_connection_handler ~request_handler ~error_handler` can be given to
  `Eio.Net.run_server`. The server and `Client.create_connection` accept any
  `_ Eio.Flow.two_way`.
- `Client.t` is abstract in `ache-eio` and `ache-lwt`.
- New cases in `Reqd.error`: `` `Not_implemented `` (501),
  `` `Http_version_not_supported `` (505), `` `Uri_too_long `` (414),
  `` `Request_header_fields_too_large `` (431) and `` `Request_timeout ``
  (408).
- New case in `Client_connection.error`: `` `Connection_closed ``.
- New case in `Response.Body_length.t`: `` `No_body ``, for a response to
  `HEAD` and for status 1xx, 204 and 304, in place of `` `Fixed 0L ``.
- `Status`: `` `Payload_too_large `` is renamed `` `Content_too_large ``
  ("Content Too Large", RFC 9110 §15.5.14). The IANA codes 102, 103, 207, 208,
  226, 308, 421, 422 (`` `Unprocessable_content ``), 423, 424, 425, 451, 506,
  507 and 508 have constructors, and `of_code` returns them in place of
  `` `Code n ``.
- New field `Config.max_header_size`.
- The `respond_with_*` functions and `Client_connection.request` raise
  `Invalid_argument` for heads and framing they cannot send correctly (see
  Server and Client below). httpun sent them as given.
- `Body.Reader.schedule_read` takes a required `~on_error:(unit -> unit)`.
  A body cut by end of input, a framing error, an exception or `shutdown`
  calls it in place of `on_eof`, and the cause goes to the error handler
  (RFC 9112 §8). Exactly one of `on_eof` and `on_error` ends a body. A
  close-delimited response body still ends with `on_eof`. Add `~on_error`
  to every call.

### Server

- Request line: the method is a token and the target is not empty and has no
  CTL. A request line without a version gets 400. HTTP/1.x with x > 1 is
  handled as HTTP/1.1; another major version gets 505. One empty line before
  the request line is ignored (RFC 9112 §2.2).
- The target form must fit the method: `*` only with OPTIONS, authority-form
  only with CONNECT, and a `host:port` CONNECT target with a port of 0 to
  65535. Otherwise 400 (RFC 9112 §3.2, RFC 9110 §9.3.6).
- An HTTP/1.1 request without `Host`, with more than one `Host`, or with a
  `Host` value that is not `uri-host [":" port]` gets 400 (RFC 9112 §3.2).
- Body framing: `Content-Length` must be one repeated `1*DIGIT` value. Both
  `Content-Length` and `Transfer-Encoding`, `Transfer-Encoding` in an HTTP/1.0
  request, and `chunked` that is not the final coding or is there twice get
  400. Another transfer coding gets 501.
- `Config.max_header_size` (default 16384 bytes) limits a request head
  independently of `read_buffer_size`. A head that reaches it in the method
  gets 501, in the rest of the request line 414, and later 431.
- A rejected request gets its error response after the responses to earlier
  pipelined requests, and the connection closes.
- Response framing: `respond_with_string` and `respond_with_bigstring` add
  `Content-Length`. `respond_with_streaming` adds `Transfer-Encoding: chunked`
  on an HTTP/1.1 persistent connection; otherwise the body ends when the
  connection closes. A final coding of `chunked` in a list such as
  `gzip, chunked` is chunked by ache.
- No body is sent in a response to `HEAD` or with status 1xx, 204 or 304.
- Every final response after which the connection closes has
  `connection: close` (RFC 9112 §9.6). A response to an HTTP/1.0 request on a
  connection that stays open has `connection: keep-alive`.
- `respond_with_*` raise `Invalid_argument` before writing for a 1xx status,
  both `Content-Length` and `Transfer-Encoding`, either field in a 1xx, 204 or
  2xx-to-`CONNECT` response, `Transfer-Encoding` in a string or bigstring
  response with a body, a `Content-Length` that differs from the body, and an
  invalid field name, field value or reason phrase.
- A streaming body with `Content-Length` raises `Invalid_argument` for a
  write past the length. A body closed short of it closes the connection.
- Add `Reqd.respond_with_informational` for interim responses such as
  `100 Continue` and `103 Early Hints`. Nothing is sent to an HTTP/1.0
  request.
- Only a 2xx response to `CONNECT` starts a tunnel. After `CONNECT` or an
  `Upgrade` request, the server reads nothing more until the response is
  decided; a 101 gives the following bytes to the new protocol, and a
  response that does not switch reads them as HTTP.
- A request body that the application did not read is read to its end and
  dropped once the response is complete.
- When the write side is closed, `respond_with_*` record the response and
  drop its bytes, and pending `Body.Writer.flush` callbacks get
  `` `Closed ``.
- Once the write side has closed, no new request is read or dispatched; the
  request in progress still gets its body, then the reader closes. This
  reverses httpun #136: a client cannot tell a processed but unanswered
  request from an unprocessed one, so its retries would duplicate side
  effects.
- When the error handler raises, the connection drops its pending output and
  closes. Its response function raises `Failure` when called twice.
- `Reqd.report_exn` before the error handler has responded closes the
  connection; httpun raised `Failure "httpun.Reqd.report_exn: NYI"`.
- A head cut by end of input gets its 400 whether the runtime asks to write
  or to read first.
- Timeouts, off by default: `?idle_timeout` closes a connection that waits
  for a request; `?head_timeout` answers 408 to a request head that is not
  complete in time, counted per head from its first byte. A request body or
  a response in progress is never timed out. `Server_connection.read_timeout`
  tells a runtime which timeout applies.

### Client

- Each request that gets no byte of its response before the connection
  stops reading gets `` `Connection_closed `` once. A request made after the
  connection stopped writing gets it before `request` returns.
- A request is pipelined only behind requests with an idempotent method, and
  never behind `CONNECT` or an `Upgrade` request (RFC 9112 §9.3.2).
- A response that closes the connection also stops the request body being
  sent (RFC 9112 §9.5), and the bytes after it are dropped. This includes an
  HTTP/1.0 response with `Transfer-Encoding` and a response with both
  `Content-Length` and `Transfer-Encoding`.
- Interim 1xx responses other than 101 are dropped; the response handler gets
  the final response.
- After a 101, or a 2xx response to `CONNECT`, the client stops reading HTTP
  and `upgrade` gives the bytes after the head to the new protocol.
- An obs-fold in a response field value becomes one space (RFC 9112 §5.2).
  Empty lines before a response are ignored.
- Response bytes when no request waits for a response close the connection.
- `request` checks the head before it queues the request and raises
  `Invalid_argument` for an invalid method, field or target; later requests
  are still sent.
- `request` raises `Failure` for a request with both `Content-Length` and
  `Transfer-Encoding` (`Request.body_length` is `` `Error `Bad_request ``).
- Request body flushes get `` `Closed `` when the connection closes or a
  write fails.

### Bodies

- `on_read` gets all the available bytes in one call, so a body of many
  small chunks is delivered in linear time. Body bytes are copied into one
  buffer per body, and bytes given to `on_read` are never overwritten.

### Parsing

- parseff (`> 0.5.0`) replaces Angstrom for message heads,
  chunk-size lines and trailer sections. Bodies are framed by hand.
  `Ache_private.Parse.Grammar` exposes the head grammars.
- Faraday 0.8.2 is vendored in `vendor/faraday` and compiled inside `ache`,
  which can be linked with the opam `faraday` package.
- Field lines in requests, responses and trailers: the name is a token
  directly followed by `:`, and values and reason phrases accept only HTAB,
  SP, VCHAR and obs-text. A bare LF or bare CR line end and an obs-fold in a
  request get 400.
- Chunk extensions are ignored and trailer fields are discarded; httpun
  rejected both. A chunk size with more than 15 hex digits after its leading
  zeros is rejected.
- A response status below 100 or with a leading zero, and a response major
  version other than 1, are malformed responses. A received status outside
  100..599 counts as a server error in `Status.is_server_error`.
- `Content-Length` rules apply to responses too. A response head larger than
  `max_header_size` is `` `Malformed_response "header fields too large" ``.
- Connection options are matched case-insensitively in every member of every
  `Connection` field.
- `read` and `read_eof` consume every byte of an incomplete head, chunk-size
  line or trailer section, and of a head that fails to parse.
- Head parses size parseff's read buffer with `Source.of_function
  ?initial_buffer_size` from the bytes at hand: a 462-byte GET head read at once
  allocates 190 fewer words (647 to 457), and no benchmark row allocates
  more. This needs a parseff later than 0.5.0. Until a parseff release has
  `?initial_buffer_size`, `ache.opam` pins `parseff.dev` to davesnx/parseff#57
  (commit b5673d8) with `pin-depends`, and ache cannot be published to the
  opam repository.

### Runtimes

- A write that fails because the peer is gone (`EPIPE`, `ECONNRESET`,
  `ENOTCONN`, Eio `Connection_reset`) reports `` `Closed ``. In `ache-lwt` a
  read that fails with `ECONNRESET` or `ENOTCONN` is end of input.
- A `shutdown` or `close` of the flow that raises, and an exception from
  `Runtime.S.report_exn`, end the connection; the handler returns normally.
- `yield_reader` and `yield_writer` on a closed side call their callback at
  once in place of raising.
- `ache-eio`: with `?clock`, a server that stops reading drops input until
  the peer closes, for at most `idle_timeout` or
  `Server.linger_without_idle_timeout` (2 s). Without a clock it shuts down
  the flow at once. Cancelling the connection fiber is not reported to the
  error handler.
- `ache-lwt`: the socket is closed exactly once. The application must ignore
  `SIGPIPE`; ache installs no signal handlers.

### Fixes of upstream issues

- httpun #95: line ends must be CRLF; a bare LF gets 400 (the issue asked
  for tolerance; ache follows RFC 9112 §2.2 strictly).
- httpun #143: a 101 that is not written because the connection closed does
  not start the new protocol.
- httpun #150: `Reqd.respond_with_informational` sends `100 Continue`.
- httpun #151: the opam files have `x-maintenance-intent: ["(latest)"]`.
- httpun #154: `ache-eio` handler argument order.
- httpun #155: `Body.Writer.flush` callbacks get `` `Closed `` when the peer
  is gone.
- httpun #156: `conformance/` holds h1spec echo servers and a runner. It has
  not been run with h1spec yet.
- httpun #157, #159: invalid field names and control characters in field
  values get 400.
- httpun #158, #162: missing or repeated `Host` gets 400.
- httpun #160, #161: unsupported versions get 505 and request lines without
  a version get 400.
- httpun #163: body bytes are copied into one buffer per body.
- httpun #164: `Content-Length` with `Transfer-Encoding` gets 400.
- httpun #165: no body in a response to `HEAD`.
- httpaf #18: the head limit no longer depends on `read_buffer_size`.
- httpaf #38: chunk sizes are parsed by the parseff grammar with a length
  limit.
- httpaf #86: idle and head timeouts.
- httpaf #191: 422 `` `Unprocessable_content ``.
- httpaf #192: `respond_with_string` adds `Content-Length`.

### TLS

- New packages `ache-eio-tls` (module `Ache_eio_tls`, over `tls-eio`) and
  `ache-lwt-tls` (module `Ache_lwt_tls`, over `tls-lwt`). Each has a server
  handler that does the handshake on the accepted socket (limited by
  `?idle_timeout`) and a client constructor that takes a `Tls.Config.client`
  and `?host`. A client reports a response that ends at the close of the
  connection without a TLS `close_notify` alert as
  `` `Exn Ache_eio_tls.Truncated `` or `` `Exn Ache_lwt_tls.Truncated ``,
  so a cut response body calls `on_error`. A complete response with
  `Content-Length` or chunked framing, and a request that got no byte of its
  response (`` `Connection_closed ``), are not affected.
- `ache-eio`: a read is no longer cancelled by an idle or head timeout, by
  the linger after the last response, or by `Client.shutdown`; the
  connection waits for the result of the read in place of cancelling it.
  `tls-eio` keeps the cancellation of a read as the permanent error of the
  flow, so over TLS the 408 response and the `close_notify` alert were not
  sent and the handler raised `Eio.Cancel.Cancelled`.
- The README has TLS quick starts for Eio and Lwt.

httpun history
==============

Unreleased
--------------

- client, server: support tunnels with `` `CONNECT ``
  ([#140](https://github.com/anmonteiro/httpun/pull/140))
- types: add `Headers.singleton`
  ([#146](https://github.com/anmonteiro/httpun/pull/146))

0.2.0 2024-09-04
--------------

- client: report exceptions before closing the response body
  ([#135](https://github.com/anmonteiro/httpun/pull/135))
- server: process requests after EOF
  ([#136](https://github.com/anmonteiro/httpun/pull/136))
- surface (body) write errors through `flush`
  ([#138](https://github.com/anmonteiro/httpun/pull/138))
    - `Body.Writer.flush` now takes a callback of the type
       ``([ `Written | ` Closed] -> unit)``, informing the caller whether the
       previous writes have been written or whether the output channel was
       closed.

0.1.0 2024-06-08
--------------

- Initial public release
