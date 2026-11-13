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
