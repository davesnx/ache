ache Unreleased
--------------

Fork of httpun at 37fdcd8 (httpun 0.2.0 and 10 commits).

### Breaking

- `Ache_lwt.IO` needs `shutdown_send`. The Lwt server now lingers before it
  closes, as ache-eio does: it shuts down its send side, reads and drops
  input until the client's end of input or `idle_timeout` (2 s without it),
  then closes, so a client that is still sending does not lose the response
  to a reset (RFC 9112 §9.6).

- `httpun` and `httpun-types` become `ache`, `httpun-eio` becomes `ache-eio`,
  and `httpun-lwt` and `httpun-lwt-unix` become `ache-lwt`. The async and
  mirage runtimes are removed.
- No gluten, Angstrom, Faraday or bigstringaf dependency. `Ache.Runtime`
  replaces gluten, and `Ache.Bigstring.t` replaces `Bigstringaf.t` (the same
  type).
- `Body.Reader.schedule_read` takes a required `~on_error`.
- `Reqd` is `Request_descriptor`.
- `Request_descriptor.respond_with_upgrade` and `Client.upgrade` take a
  `Runtime.impl`.
- `Request_descriptor.respond_with_upgrade` on a `CONNECT` request sends a
  2xx (`?status`, default `200`) and starts a tunnel. It sent `101`.
- `ache-eio`: `Server.create_connection_handler` takes no `~sw`, and can be
  given to `Eio.Net.run_server`.
- `ache-lwt`: `Make (Io)` replaces the gluten functors. TLS moves to
  `ache-lwt-tls`. There is no OpenSSL runtime.
- New error cases (408, 414, 431, 501, 505 and `` `Connection_closed ``), and
  `` `No_body `` in `Response.Body_length.t`.
- `Status`: `` `Payload_too_large `` is `` `Content_too_large ``, and more IANA
  codes have constructors.
- The `respond_with_*` functions and `Client_connection.request` raise
  `Invalid_argument` for a head they cannot send correctly.
- The client pipelines only up to `Config.max_pipelined_requests`, default
  1: a request is written after the previous response is complete. A
  non-idempotent request, or one that may switch protocols, is never
  written while another response is pending. Set the limit to pipeline.
- `ache-miou`, `ache-miou-tls`: the server timeouts are on by default and
  take a `float option`: `?idle_timeout` (60 s), `?head_timeout` (30 s),
  and the new `?body_timeout` (60 s, request body progress) and
  `?write_timeout` (60 s, write progress); `None` turns one off. An idle
  WebSocket is closed with code 1001 after `idle_timeout`. A request body
  left unread after its response is no longer dropped at once without
  `idle_timeout`: `body_timeout` bounds it.
- `ache-lwt`, `ache-lwt-tls`: the server timeouts are on by default and
  take a `float option`: `?idle_timeout` (60 s), `?head_timeout` (30 s),
  and the new `?body_timeout` (60 s, request body progress, counted in
  shared ticks of a tenth) and `?write_timeout` (60 s, write progress);
  `None` turns one off. An idle WebSocket is closed with code 1001 after
  `idle_timeout`. A request body left unread after its response is bounded
  by `body_timeout`, no longer by `idle_timeout` (or dropped at once).
- `ache-eio`, `ache-eio-tls`: the server timeouts are on by default and
  take a `float option`: `?idle_timeout` (60 s), `?head_timeout` (30 s),
  and the new `?body_timeout` (60 s, request body progress) and
  `?write_timeout` (60 s, write progress); `None` turns one off. They no
  longer need `?clock`: without it the server uses the system clock, and
  it always lingers after a closing response. An idle WebSocket is closed
  with code 1001 after `idle_timeout`. A request body left unread after its
  response is bounded by `body_timeout`, no longer by `idle_timeout` (or
  dropped at once).
- `Server_connection.read_timeout` answers `` `Body `` whenever a read
  waits for request body bytes, also before the response is complete, and
  its action sends 408 when no response has started (otherwise it closes).
  On a connection upgraded with `Websocket.upgrade` it answers `` `Idle ``,
  whose action closes with code 1001 (new `Websocket.read_timeout`), or
  `` `Close `` while the server waits for the client's close frame. It
  answers `None` only while no read waits for the peer.
- `Websocket.close` waits for the client's close frame and drops the
  messages that arrive meanwhile. Every runtime takes `?close_timeout`
  (default 10 s) to bound that wait. A WebSocket session that returns
  closes the connection with 1000.
- `Server_connection.error` has `` `Bad_request of string `` instead of
  `` `Bad_request ``: the reason the request was rejected, for logs. The
  default error handler does not send it. An error handler that matches
  `#Status.standard` must match `` `Bad_request _ `` first, or get the
  status with `Server_connection.error_status error`.
- `Body.Writer.close` has the type `?trailers:Headers.t -> t -> unit`. A call
  compiles as before; a module that gives it where a `t -> unit` value is
  expected must apply it (`fun w -> Body.Writer.close w`).

### New

- Packages `ache-miou`, `ache-eio-tls`, `ache-lwt-tls` and `ache-miou-tls`.
- `Server_connection.error_status` gives the status of the error response
  for each `Server_connection.error`.
- `Ache_lwt.Make_with_idle_check`, `Ache_miou.Make_with_idle_check` and
  `Ache_eio.Client.create_connection ?input_waiting`: a runtime on another
  socket type can give `Client.request` its look at an idle connection.
- `Ache.Testing` runs a request handler on bytes or on a `Request.t` and a
  body, with no sockets and no runtime.
- `Request_descriptor.respond_with_informational`, for `100 Continue` and
  `103 Early Hints`.
- `Config.auto_continue` (default `false`): the first read of the body of a
  request with `Expect: 100-continue` sends `100 Continue`, unless a
  response has started.
- Server timeouts `?idle_timeout` and `?head_timeout`, and
  `Config.max_header_size`.
- `Server_connection.create ?date` adds a `Date` field to each final
  response that has none. The servers of `ache-eio`, `ache-lwt` and
  `ache-miou` now send `Date` by default (RFC 9110 §6.6.1); `~date:false`
  turns it off.
- `Config.max_pending_output` (64 KiB): while more response bytes wait to
  be written, the server reads no new request.
- `Server.run` in `ache-eio` and `ache-miou`: one accept loop for each
  domain, each on its own `SO_REUSEPORT` socket, with no state shared
  between domains.
  `?tcp_nodelay:true` sets `TCP_NODELAY` on each accepted socket.
- `Ache_lwt.Timeout`: timeouts that share one Lwt timer for each length, so
  that cancelled timers do not pile up with the `select` and `poll` engines.
- `Ache.Websocket`: a WebSocket server (RFC 6455) for text and binary
  messages, which runs after `Request_descriptor.respond_with_upgrade` on
  every runtime. It checks text as UTF-8 while it arrives, sends and reports
  close reasons, and sends pings (`on_pong` gets the answers).
  `Ache_lwt.Websocket` gives it a promise API: `upgrade`, `send_text`,
  `send_binary`, `receive`, `ping`, `client_close` and `close`.
  `Ache_eio.Websocket` and `Ache_miou.Websocket` give the same in direct
  style, with the session in a fiber (Eio) or task (Miou) of the connection.
  A write to a client that reset the connection ends the connection
  (camlworks/dream#467).
- `Server_connection.drain` makes the request in progress the last one on
  a connection (an idle connection closes at once, a WebSocket sends close
  1001), and `Config.max_requests_per_connection` (default `None`) makes
  the n-th request the last one.
- `ache-lwt`, `ache-lwt-tls`: graceful stop with `?stop:unit Lwt.t` and
  `?grace:float` on `create_connection_handler` (and
  `create_connection_handler_with_fd`). When `stop` resolves, each
  connection drains; `grace` seconds later the ones still open are closed.
- `ache-eio`, `ache-eio-tls`: graceful stop with `?stop:unit Eio.Promise.t`
  and `?grace:float` on `Server.create_connection_handler` and
  `Server.run`. When `stop` is resolved, `run` stops accepting, each
  connection drains, `grace` seconds later the ones still open are
  cancelled, and `run` returns when every connection has ended.
- `ache-miou`, `ache-miou-tls`: graceful stop with
  `?stop:unit Miou.Computation.t` and `?grace:float` on
  `Server.create_connection_handler`, `Server.run` and
  `create_connection_handler_with_fd`. When `stop` is resolved, from any
  domain, `run` stops accepting, each connection drains in its own domain,
  `grace` seconds later the ones still open are cancelled with their
  WebSocket sessions, and `run` returns when every connection has ended.
- `Server_connection.create ?keep_alive_timeout` adds `Keep-Alive:
  timeout=n` (and `max=m` with `Config.max_requests_per_connection`) to
  persistent responses, and `Client_connection.reuse_timeout` reads the
  hint of the last response (at most 600 s, minus 2 s).
- `ache-eio`, `ache-eio-tls`: with `idle_timeout` of 1 s or more the server
  sends `Keep-Alive: timeout=n` on persistent responses and waits 1 s
  longer than it announced; `Client` closes a connection `n - 2` seconds
  after such a response, and a request after that gets
  `` `Connection_closed `` without writing anything.
- `ache-lwt`, `ache-lwt-tls`: with `?idle_timeout` of 1 s or more, the
  server sends `Keep-Alive: timeout=n` and keeps its idle timer 1 s longer;
  the client closes an idle connection `n - 2` s after a response with that
  hint, and a request on it after that gets `` `Connection_closed ``.
- `ache-miou`, `ache-miou-tls`: with `idle_timeout` of at least 1 s (the
  default 60 s), the server sends `Keep-Alive: timeout=n` on persistent
  responses and waits 1 s longer than it announces. The client closes an
  idle connection `n - 2` seconds after a response with that hint, and a
  request on it after then gets `` `Connection_closed `` before anything is
  written.
- `Websocket.upgrade ?headers` (core, Eio, Lwt and Miou) adds fields to the
  `101`, for example `Sec-WebSocket-Protocol`; `Websocket.requested_protocols`
  reads the subprotocols that the client offers.
- HTTP/1.1 trailers. `Body.Reader.trailers` gives the trailer fields of a
  chunked body in `on_eof`, and `Body.Writer.close ~trailers` sends them
  after the last chunk; a server sends them only when the request has
  `TE: trailers`. Framing, routing and authentication fields (Go's
  `badTrailer` list) are dropped when received and raise `Invalid_argument`
  when sent. Trailers were read and discarded.

### Changed

- Strict parsing and framing after RFC 9110 and RFC 9112. `docs/ache.md` lists
  what the server rejects.
- Bodies are delivered in linear time.
- An obs-folded response field is joined once, in linear time.
- `ache-lwt` shares one timer for each timeout value among all connections,
  and `ache-miou` keeps one timer for each timeout on a connection that
  sleeps at most 0.5 s at a time, so cancelled timers do not pile up in the
  heap. The TLS handshake timeout does the same: `ache-lwt-tls` uses
  `Ache_lwt.Timeout`, and `ache-miou-tls` sleeps at most 0.5 s at a time.
- The responses to all pipelined requests in one read go out in one write.
  `ache-miou` sends all buffers with one `sendmsg`, not one write per buffer.
- The TLS runtimes join the buffers of a write into TLS records of up to
  16 KiB. ocaml-tls made one record for each buffer, so a chunked response
  of many small writes sent a record, with its overhead, for each one.
- The server stops reading new requests once a response will close the
  connection.
- After a response that is sent before the request body has arrived, the
  body is still delivered, within `?idle_timeout`. Without `?idle_timeout`,
  the body gets `on_error` and the connection closes.
- A 2xx response to `CONNECT` sent with `respond_with_string`,
  `respond_with_bigstring` or `respond_with_streaming` closes the
  connection after its body. It kept the connection open with no end. A
  tunnel uses `respond_with_upgrade`.
- An exception from the request handler of a pipelined request gets that
  request an error response. It escaped `next_read_operation` or
  `next_write_operation`.
- A bad byte in a head line split over reads gets the 400 in the read that
  brings it. It came when the line ended, at the head limit or at end of
  input.
- Client: bytes that do not start a valid response, read with the end of a
  response, give the error to the next request, or close the connection when
  no request waits or the response closes it. They gave the error to the
  request whose response was complete.
- Fixes httpun #95, #143, #150, #151, #154 to #165, and httpaf #18, #38,
  #86, #191, #192.
- When `Request_descriptor.respond_with_upgrade` cannot write the 101, it
  shuts the new protocol down.
- `Config.max_discard_body` (256 KiB): after the response, the server reads
  at most this much of a request body the application did not read. A
  `Content-Length` remainder above it gets `connection: close`; a chunked
  body that passes it closes the connection.
- The server answers 400 to a target byte outside RFC 3986 (`#`, a byte
  above 0x7F, `"`, `<`, `>`, `{`, `}`, `|`, `^`, `` ` ``, `\`), to userinfo
  or an empty host in an absolute-form target, and to a `Host` with an
  empty host (`:80`). Bytes above 0x7F were accepted.
- Every response to `CONNECT` other than `respond_with_upgrade` closes the
  connection, and the bytes after the request are not read. A non-2xx
  response let the connection read them as the next request.
- A `CONNECT` request has no body: the server ignores its `Content-Length`
  and `Transfer-Encoding` (RFC 9110 §9.3.6). It read a body framed by them.
- The server skips any number of empty lines before a request line,
  counted against `Config.max_header_size`. It answered 400 to a second one.
- The client accepts a status-line with CRLF right after the status code
  (`HTTP/1.1 301\r\n`) and gives `` `Malformed_response `` for a response
  whose `Transfer-Encoding` lists no coding.
- A chunked body fails when its chunk lines and extensions exceed its data
  by more than 16 KiB (Go's budget, CVE-2023-39326): the server answers 400,
  the client gets `` `Malformed_response ``.
- Client: a 204 or 304 (not to `HEAD`) with `Transfer-Encoding` or a
  nonzero `Content-Length` closes the connection after it.
- Client: `Client_connection.request` raises `Invalid_argument` for two
  `Host` fields and for conflicting or invalid `Content-Length` and
  `Transfer-Encoding`. It raised `Failure` for the latter.
- Client: response bytes on an idle connection close it also before their
  head is complete, and `Client.request` checks an idle socket for such
  bytes before it writes (undici GHSA-35p6-xmwp-9g52). The TLS clients
  decrypt the records that wait, so a session ticket or a key update does
  not close the connection.
- `ache-lwt-tls` and the client of `ache-eio-tls` drive `Tls.Engine`
  themselves, as `ache-miou-tls` does, instead of `Tls_lwt.Unix` and
  `Tls_eio`.

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
