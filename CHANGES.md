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
