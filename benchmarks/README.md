# Benchmarks

`parse_bench.exe` measures the ache request parser alone.

`vs/` compares ache with httpaf 0.7.1 and httpun 0.2.0. It has two layers.

- **core**: `core_bench.exe` drives `Server_connection` and
  `Client_connection` of each library in one process, with no sockets. It
  gives the same bytes to each library and reports nanoseconds and minor
  words per message. It isolates the library from the kernel and the runtime.
- **wire**: one server executable per library and runtime: ache-lwt,
  httpun-lwt, httpaf-lwt, ache-eio and httpun-eio. `wrk` loads each server
  over loopback.

## What is the same for every server

- The request handler: one functor (`vs/vs.ml`) applied to a small adapter
  per library. Response bodies are made once and kept.
- The response header list. The handler sets `content-length` or
  `transfer-encoding: chunked`. A library can add fields of its own; the
  report shows such differences.
- The accept loop and the socket options of each runtime (`vs/vs_lwt.ml`,
  `vs/vs_eio.ml`): a listen backlog of 4096, `SO_REUSEADDR`, `TCP_NODELAY`,
  one domain. An exception that escapes a connection is logged to stderr and
  the server goes on (`Lwt.async_exception_hook` for Lwt, `on_error` for
  Eio). The report lists every run whose server wrote to stderr.
- The buffer sizes of `Config`, given by flags. The read, response and
  response-body buffers matter; `request_body_buffer_size` has no effect in
  the server path of any of the three libraries. ache has more `Config`
  fields; they keep their defaults.
- `OCAMLRUNPARAM`: unset, unless the cell sets it.
- The compiler, the opam switch and the dune profile (release).

## Routes

| Route | Response |
|---|---|
| `GET /fixed/<bytes>` | body with `content-length`, one `respond_with_bigstring` |
| `GET /close/<bytes>` | same, and the handler sets `connection: close` |
| `GET /stream/<chunk>/<count>` | `transfer-encoding: chunked`, one write and one flush per chunk |
| `GET /streamcl/<chunk>/<count>` | same, with `content-length` |
| `POST /sink` | reads the whole body, answers the byte count |
| `POST /echo` | sends the request body back, chunked |
| `GET /gc` | `Gc` counters as JSON, read before and after a run |

## Matrix

One base cell, `GET /fixed/12` on 64 connections with default buffers, and
one sweep per axis from it. `node benchmarks/vs/run.ts list` prints all
cells.

- Load: 1, 16, 64, 256 and 1024 connections; pipelining depth 16;
  `Connection: close` on each request (`GET /close/12`: every server then
  sends `connection: close`, so wrk opens a new connection for each
  request).
- Response size: 0, 1 KiB, 16 KiB, 256 KiB, 4 MiB.
- Streaming: 1 KiB x 16, 16 KiB x 16, 64 KiB x 64 chunked; 16 KiB x 16 with
  `content-length`.
- Request body: 1 KiB, 64 KiB and 4 MiB to `/sink` with `content-length`;
  64 KiB chunked; 64 KiB to `/echo`.
- Request head: 8, 32 and 96 header fields; a 4 KiB target, with the default
  read buffer and with a 16 KiB read buffer.
- Buffers: read, response and response-body buffers at 1 KiB, 64 KiB and
  256 KiB, on the base cell, on a 256 KiB response and on a 4 MiB upload.
- GC settings: `OCAMLRUNPARAM=s=4M` (minor heap of 4M words) on the base
  cell, 32 header fields, a 64 KiB upload and a 16 KiB x 16 stream.
- Eio backend: `EIO_BACKEND=posix` on the base cell and on a 256 KiB
  response. The other cells use the default backend (io_uring on Linux).

## How to run

```sh
make bench-deps    # opam install httpaf, httpun and their runtimes
make bench-quick   # 1 run per cell and server, 2 s each, about 15 min
make bench         # 5 runs per cell and server, 5 s each, about 2 h 30 min
```

`opam exec` selects the switch. Set `OPAMSWITCH` to use another one. Pass
driver flags with `BENCH_FLAGS`, for example
`make bench-quick BENCH_FLAGS="--only '^base$' --servers lwt"`.
`node benchmarks/vs/run.ts --help` lists the flags. Node 22.18 or later runs
the TypeScript driver directly. `core_bench.exe --help` lists its flags.

The driver writes `benchmarks/vs/results/<profile>-<time>.json` after each
run and a Markdown report next to it at the end.
`node benchmarks/vs/run.ts report <file.json>` writes the report again.

## How a run works

1. Before the first run of a cell, the driver picks the most idle cores of
   one L3 cache group from `/proc/stat`: one for the server, one per wrk
   thread. All servers and repetitions of that cell use these CPUs.
2. It starts a new server process on its core (`taskset`).
3. Gate before the run: it sends the cell's request twice on one
   connection, checks the status and the body bytes, and keeps the response
   bytes. In the `Connection: close` cell it checks for exactly one
   `connection: close` field and the close, and records whether the server
   adds that field by itself. In the pipelining cell it sends 2 and 16
   requests in one socket write, 3 times each, and counts the replies.
4. Optional warm-up with wrk; the result is dropped.
5. It reads `/gc` and the server's CPU time, runs wrk, reads them again,
   and reads the peak RSS and the number of open file descriptors.
6. It stops the server and waits until the port is free.
7. Gate after the run: a run with any wrk socket error or non-2xx response
   fails and stays out of the medians.

Repetitions go over all cells, with the servers in a new random order for
each cell. In the full profile a run is done again, at most 3 times, when
the SMT sibling of the server core was more than 20 % busy. A run kept
above that limit is flagged in the report. The driver never builds: it
records the commit and warns when the binaries are older than the newest
source file.

## How to read the report

- Ratio, in every table: the speed of that server or library divided by
  the speed of ache on the same runtime. Above 1.00 means faster than ache.
- One summary table per axis: one row per cell, one column per server, the
  median requests/s and the ratio. `[3/5, 1 flagged]` shows passed runs and
  flagged runs when not all runs are clean.
- † marks a cell where the server used less than 90 % of one core. There
  the client, the network or a wait was the limit, not the server.
- Detail tables: req/s with min and max, response and request MB/s,
  latency p50 and p90, server CPU, CPU µs and minor words per request, major
  collections, peak RSS, open file descriptors, preemptions, and the load of
  other tenants on the server core's sibling and L3 cache.
- Minor words do not count Bigstrings (malloc) or fiber and effect stacks.
  Peak RSS and major collections show part of that cost.
- Failed and skipped runs, with the reason.
- Wire differences: cells where the servers sent different response bytes.
- The core table: ns per message (median and min), the ratio (ache ns over
  library ns) and minor words per message.

## Limits

- Only p50 and p90 latency are reported. wrk 4.1 adds synthetic samples
  after a slow response, so one stall can move p99 from microseconds to tens
  of milliseconds, and in pipelined runs it returns 0 for high percentiles.
  The JSON keeps those values under `latency_unreliable_us`.
- Latency is closed loop: wrk sends the next request after the response.
  It is not the latency of an open system under a fixed rate (that needs
  wrk2, which is not used).
- Loopback only. There is no network latency, loss or bandwidth limit.
- One server core, one domain. No multi-core scaling.
- A shared machine adds noise. The report shows the load next to the server
  core. Compare servers within one result file, not across files.
- Lwt here has no libev, so it uses its `select` engine. That engine fails
  above 1024 file descriptors, so Lwt servers skip cells above 512
  connections (`--lwt-max-connections`). It also costs time per call that
  grows with the number of connections, for all three Lwt servers.
- ache limits the request head to 16 KiB (`max_header_size`), separate from
  the read buffer. httpaf and httpun need the whole head inside the read
  buffer, so they close the connection on a head larger than
  `read_buffer_size`: the 4 KiB target fails for them with the default
  4096-byte buffer, and passes with a 16 KiB buffer.
- Out of scope: TLS (its cost is ocaml-tls), a wire-level client benchmark
  (the client is measured in core only), HTTP/2.
