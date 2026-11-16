ache Unreleased
--------------

Fork of httpun at 37fdcd8 (httpun 0.2.0 and 10 commits).

### Breaking

- `httpun` and `httpun-types` become `ache`, `httpun-eio` becomes `ache-eio`,
  and `httpun-lwt` and `httpun-lwt-unix` become `ache-lwt`. The async and
  mirage runtimes are removed.
- No gluten, Angstrom, Faraday or bigstringaf dependency. `Ache.Runtime`
  replaces gluten, and `Ache.Bigstring.t` replaces `Bigstringaf.t` (the same
  type).
- `Body.Reader.schedule_read` takes a required `~on_error`.
- `Reqd.respond_with_upgrade` and `Client.upgrade` take a `Runtime.impl`.
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

### New

- Packages `ache-miou`, `ache-eio-tls`, `ache-lwt-tls` and `ache-miou-tls`.
- `Reqd.respond_with_informational`, for `100 Continue` and `103 Early Hints`.
- Server timeouts `?idle_timeout` and `?head_timeout`, and
  `Config.max_header_size`.
- `Ache_lwt.Timeout`: timeouts that share one Lwt timer for each length, so
  that cancelled timers do not pile up with the `select` and `poll` engines.

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
- Fixes httpun #95, #143, #150, #151, #154 to #165, and httpaf #18, #38,
  #86, #191, #192.

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
