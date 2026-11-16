# ache

ache is an HTTP/1.1 client and server library for OCaml 5. The core package,
`ache`, does no I/O: it parses and serializes messages and keeps the state of
a connection. Three runtimes drive it on sockets:

- `ache-eio`, for [Eio](https://github.com/ocaml-multicore/eio)
- `ache-lwt`, for [Lwt](https://github.com/ocsigen/lwt)
- `ache-miou`, for [Miou](https://github.com/robur-coop/miou)

With `ache-eio` and `ache-miou`, `Server.run` serves on every CPU core: one
accept loop in each domain, each on its own `SO_REUSEPORT` socket. The
kernel spreads connections between the domains on Linux, not on macOS.

`ache-eio-tls`, `ache-lwt-tls` and `ache-miou-tls` add TLS (see
[TLS](docs/ache.md#tls)).

Status: experimental. The API can change before 1.0.

Documentation: [docs/ache.md](docs/ache.md).
