ache Unreleased
--------------

- Fork of httpun at 37fdcd8. Packages: `ache`, `ache-eio`, `ache-lwt`.
  `httpun-types` merges into `ache`; `httpun-lwt` and `httpun-lwt-unix` merge
  into `ache-lwt`. The Lwt `TLS` and `SSL` modules, `httpun-async` and
  `httpun-mirage` are removed.
- Remove the gluten dependency. `ache-eio` and `ache-lwt` have their own IO
  loops.
- Add `Ache.Runtime`: `module type S` is the interface a runtime drives,
  `Runtime.make` packs a protocol as a `Runtime.impl`.
  `Server_connection` and `Client_connection` implement `Runtime.S`.
- Upgrade API: `Reqd.respond_with_upgrade : t -> Headers.t -> Runtime.impl ->
  unit` replaces the `unit -> unit` callback. When the 101 response is
  written, the server connection hands the socket to the given protocol,
  including input it has not consumed. Request handlers take `Reqd.t` in
  place of `Reqd.t Gluten.reqd`. `Ache_eio.Client.upgrade` and
  `Ache_lwt.Client.upgrade` take a `Runtime.impl`.
- `ache-lwt`: add `module type IO` and `Make (Io)`. `Server` and `Client` run
  on `Lwt_unix.file_descr`. The Lwt `TLS` and `SSL` modules are removed; write
  an `IO` for a TLS socket instead. `Client.t` is abstract.
- `ache-eio`: `Server.create_connection_handler` and
  `Client.create_connection` accept any `_ Eio.Flow.two_way`, for example a
  TLS flow. `Client.t` is abstract.
- Vendor Faraday 0.8.2 in `vendor/faraday`. It compiles as `Ache__Faraday`
  inside `ache`, so `ache` no longer depends on the opam `faraday` package and
  links with it in the same program. `Ache.IOVec.t` is unchanged.
- Replace Angstrom with parseff (`>= 0.5.0 & < 0.6.0`) for message heads,
  chunk-size lines and trailer sections. Bodies are framed without a parser
  combinator. `Ache_private.Parse` exposes `Grammar` (string parsers) in place
  of Angstrom parsers.
- Add `Config.max_header_size` (default 16384 bytes). A request or response
  head, chunk-size line or trailer section larger than this fails. The server
  answers `431 Request Header Fields Too Large` and reports the new
  `` `Request_header_fields_too_large `` case of `Reqd.error`. The client
  reports `` `Malformed_response "header fields too large" ``. The size of a
  head no longer depends on `read_buffer_size`: a 6 KiB header line read in
  4096-byte reads is accepted.
- Chunk extensions are accepted and ignored. httpun rejected them.
- Trailer fields after the last chunk are accepted and discarded. httpun
  rejected them.
- A chunk size with more than 15 hex digits after its leading zeros is
  rejected; leading zeros are accepted, as RFC 9112 §7.1 allows. httpun
  accepted 16-digit sizes up to `7fffffffffffffff`, and wrapped larger sizes to
  negative values.
- A status code below 100 or with a leading zero, such as `HTTP/1.1 99 x`, is
  a malformed response. httpun raised `Failure` from the parser.
- Parse errors carry one message: `` `Malformed_response "expected ' ', found
  '\r'" `` in place of Angstrom marks such as `": char ' '"`.
- `read` and `read_eof` consume every byte of an incomplete head, chunk-size
  line or trailer section, and every byte given to a head that fails to
  parse. httpun consumed up to the last complete header line, or up to the
  failure position. An incomplete head that follows a complete message in the
  same read is still not consumed.
- Request line: the method is a non-empty token, the request-target is
  non-empty with no CTL. A request line without a version gets 400; httpun
  waited for more input. HTTP/1.x with x > 1 is processed as HTTP/1.1 and
  keeps its minor version in `Request.t`. Other major versions get the new
  `` `Http_version_not_supported `` case of `Reqd.error` (505). A response
  with a major version other than 1 is a malformed response. One CRLF before
  the request line is ignored (RFC 9112 §2.2).
- Field lines in requests, responses and trailers: the field name is a
  non-empty token directly followed by `:`; the value and the reason phrase
  accept only HTAB, SP, VCHAR and obs-text. httpun accepted any name byte
  except whitespace and `:`, and NUL, bare LF and other CTLs in values.
- The server answers 400 to an HTTP/1.1 request without Host, a request with
  more than one Host field line, and a Host value that is not
  `uri-host [":" port]`: a reg-name or IPv4 address, or an IP-literal in
  brackets, then an optional `:` and digits (RFC 9112 §3.2, RFC 3986 §3.2).
- The server answers 400 to a CONNECT request whose target is not
  `host:port` with a non-empty host and a port of 0 to 65535 (RFC 9110
  §9.3.6).
- A request that the server rejects after earlier pipelined requests gets its
  error response after the responses to those requests, and the connection
  closes, also when both requests arrive in one read. httpun's
  `Server_connection` asserts that no parsed request comes with an error while
  an earlier request is active.
- Request body framing: Content-Length is a list of `1*DIGIT` values that must
  all be equal; httpun accepted `+3`, `0x3`, `1_0` and `0o3`. This applies to
  responses too. A request with both Content-Length and Transfer-Encoding, or
  an HTTP/1.0 request with Transfer-Encoding, gets 400 and the connection
  closes. chunked must be the final transfer coding, read across all
  Transfer-Encoding field lines, and appear once; otherwise 400. Any other
  transfer coding gets the new `` `Not_implemented `` case of `Reqd.error`
  (501). `Request.body_length` returns `` `Error `Bad_request `` for a request
  with both Content-Length and Transfer-Encoding, so
  `Client_connection.request` raises `Failure` for it.
- Connection options are matched case-insensitively in every list member of
  every Connection field line: `Connection: Close` and
  `Connection: keep-alive, close` close the connection.
- `Body.Reader`: `on_read` receives every byte available in one call, in place
  of one call per received slice. A body of many small chunks is delivered in
  linear time; httpun took quadratic time (100,000 one-byte chunks in one
  read: 64 s, now 14 ms). Body bytes are copied into a buffer per body, not
  one new buffer per slice. Bytes passed to `on_read` are never overwritten.
- The server and the client raise `Invalid_argument`, naming the field, and
  write nothing, when the application gives a head with a field name that is
  not a token, a field value with CR, LF or NUL, a method that is not a token,
  a request target with CR, LF, NUL or space, or a reason phrase with CR, LF
  or NUL. httpun wrote them as given.
- `Status`: add the codes of the IANA registry that were missing: 102, 103,
  207, 208, 226, 308, 421, 422, 423, 424, 425, 451, 506, 507, 508 (not 104,
  which is temporary, and not 510, which is obsolete). `of_code` returns the
  named constructor for them, such as `` `Permanent_redirect ``, in place of
  `` `Code n ``.
- `Status`: rename `` `Payload_too_large `` to `` `Content_too_large ``, with
  reason phrase "Content Too Large" (RFC 9110 §15.5.14).
- `Status.is_server_error` (and `is_error`) is true for a code outside
  100..599, such as a received `600` (RFC 9110 §15: treat an invalid status
  code as 5xx).
- `Response.body_length` returns the new `` `No_body `` for a response to
  `HEAD` and for status 1xx, 204 and 304, in place of `` `Fixed 0L ``.
- Server: a response to `HEAD` or with status 204 or 304 has no body bytes.
  `respond_with_string` and `respond_with_bigstring` drop the body, and the
  writer of `respond_with_streaming` drops what is written and answers
  `flush` with `` `Written ``. The error response to a `HEAD` request has no
  body. httpun sent the body, which a client reads as the start of the next
  response.
- Server: `respond_with_string` and `respond_with_bigstring` add
  `content-length` when the response has neither `Content-Length` nor
  `Transfer-Encoding`, and raise `Invalid_argument` when its `Content-Length`
  differs from the body length. httpun sent a close-delimited body and kept
  the connection open.
- Server: `respond_with_streaming` without `Content-Length` or
  `Transfer-Encoding` adds `transfer-encoding: chunked` for an HTTP/1.1
  request on a persistent connection. Otherwise the body is close-delimited:
  the connection closes after it, and `connection: close` is added if the
  connection was persistent. A 2xx response to `CONNECT` gets no framing
  fields.
- Server: add `Reqd.respond_with_informational` to send interim 1xx
  responses, such as `100 Continue` for `Expect: 100-continue` or
  `103 Early Hints`, before the final response. It sends nothing to an
  HTTP/1.0 request. `respond_with_string`, `respond_with_bigstring` and
  `respond_with_streaming` raise `Invalid_argument` for a 1xx status. httpun
  treated a 1xx as the final response.
- Client: interim 1xx responses other than 101 are parsed and dropped; the
  response handler gets the final response. httpun gave the 1xx to the
  response handler, and the final response failed an assertion.
- Server: ache owns the response framing (rules in the `Reqd` docs). The
  `respond_with_*` functions raise `Invalid_argument`, before writing, for a
  response with both `Content-Length` and `Transfer-Encoding`, for either
  field in a 1xx, a 204 or a 2xx response to `CONNECT` (also in
  `respond_with_informational`), and for `Transfer-Encoding` in a string or
  bigstring response with a body. httpun sent these fields as given.
- Server: a streaming response with `Transfer-Encoding` to an HTTP/1.0
  request loses the field and ends with the connection. With a final coding
  other than `chunked`, the body ends with the connection; `gzip, chunked` is
  chunked by ache.
- Server: a response to an HTTP/1.0 request on a connection that stays open
  gets `connection: keep-alive` unless it has a `Connection` field.
- `Body.Writer` with a `Content-Length` counts bytes: a write past the length
  raises `Invalid_argument` and writes nothing. A response body closed short
  of its `Content-Length` closes the connection after the bytes written.
  httpun sent extra bytes into the next response, or kept the connection
  open after a short body.
- `Response.body_length` reads the final transfer coding of a list such as
  `gzip, chunked`. httpun read only a field value equal to `chunked`.
- Server: only a 2xx response to `CONNECT` starts a tunnel (RFC 9110 §9.3.6).
  A non-2xx response to `CONNECT` completes like any other response, and the
  connection serves the next request. httpun treated every response to
  `CONNECT` as a tunnel, so the connection hung.
- Client: after a response with both `Content-Length` and
  `Transfer-Encoding: chunked`, the connection is not reused (RFC 9112 §6.3).
- Client: bytes that follow a response after which the connection is not
  reused (for example `Connection: close`) are dropped. httpun parsed them as
  the response to the next pipelined request.
- A request head that reaches `Config.max_header_size` inside the method gets
  501 (`` `Not_implemented ``), inside the request-target or the rest of the
  request line gets 414 (new `` `Uri_too_long `` case of `Reqd.error`), and
  later gets 431 as before (RFC 9112 §3).
- The server answers 400 to a request-target form that does not fit the
  method: `*` is only for OPTIONS, authority-form only for CONNECT, and other
  methods need origin-form or absolute-form (RFC 9112 §3.2). httpun passed
  `GET *` and `GET 127.0.0.1:80` to the handler.
- Client: add `` `Connection_closed `` to `Client_connection.error`. Every
  request without any byte of its response when the connection stops reading
  (end of input, a response that closes the connection, `shutdown`, or an
  error on an earlier response) gets it once. httpun called no handler for
  pipelined requests behind a closing response, and gave
  `` `Malformed_response "unexpected eof" `` to every request at end of input.
  An end of input inside a response head or inside a body framed by
  Content-Length or chunked coding is still `` `Malformed_response ``.
- Client: response bytes with no request waiting for a response close the
  connection. httpun raised `Assert_failure` from `read`.
- Client: a request is not pipelined behind a request with a non-idempotent
  method until that request's response is complete (RFC 9112 §9.3.2).
- Client: a response that closes the connection also stops the request body
  being sent (RFC 9112 §9.5). httpun kept writing it.
- Client: `Client_connection.request` checks the request head before it
  queues the request, so an invalid request raises `Invalid_argument` and
  later requests are still sent. httpun left the invalid request at the head
  of the queue and sent nothing after it.
- Client: an HTTP/1.0 response with Transfer-Encoding closes the connection
  after it (RFC 9112 §6.1).
- Client: empty lines before a response are discarded (RFC 9112 §2.2).
- Client: an obs-fold in a response field value is replaced with one space
  (RFC 9112 §5.2). Requests with obs-fold still get 400.
- Client: after a 101 response, and after a 2xx response to CONNECT, the
  connection stops reading HTTP; the bytes after the head are not consumed,
  so `upgrade` gives them to the new protocol. httpun parsed them as the next
  response or as the CONNECT response body.

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
