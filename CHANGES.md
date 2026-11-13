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
