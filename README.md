# ache

ache is an HTTP/1.1 client and server library for OCaml 5. The core package,
`ache`, does no I/O: it parses and serializes messages and keeps the state of
a connection. Three runtimes drive it on sockets:

- `ache-eio`, for [Eio](https://github.com/ocaml-multicore/eio)
- `ache-lwt`, for [Lwt](https://github.com/ocsigen/lwt)
- `ache-miou`, for [Miou](https://github.com/robur-coop/miou)

`ache-eio-tls`, `ache-lwt-tls` and `ache-miou-tls` add TLS (see
[TLS](docs/README.md#tls)).

ache is a fork of [httpun](https://github.com/anmonteiro/httpun), which is a
fork of [httpaf](https://github.com/inhabitedtype/httpaf). It keeps their
`Reqd`, `Body` and connection API, and changes this:

- Requests that RFC 9110 and RFC 9112 do not allow get an error response
  (400, 408, 414, 431, 501 or 505), and the connection closes.
- ache sets the framing of each response (`Content-Length`,
  `Transfer-Encoding`, `Connection: close`) and raises `Invalid_argument` for
  a response it cannot send correctly.
- The client reports every request that gets no response, and pipelines only
  behind idempotent requests.
- Message heads are parsed by a hand-written parser. A complete line is
  parsed once. An unfinished line is not parsed again until it ends, and once
  more at the head limit or at end of input, so the work is linear in the head
  size. There is no gluten and no Angstrom; Faraday is vendored.

Status: experimental. The API can change before 1.0.

Documentation: [docs/README.md](docs/README.md).
