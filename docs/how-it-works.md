# How ache works

This page explains how a connection runs: what the core does, what a runtime
adds, and where the work of one request goes. It is for readers of the code
and for people who compare ache with other servers. To use ache, start with
[ache.md](ache.md).

## Two layers

```
  your request_handler / error_handler
              │  Reqd.t, Body, Headers
              ▼
  ┌────────────────────────────────────────┐
  │ ache (lib/)          no I/O, no clock  │
  │  Server_connection / Client_connection │
  │  parser · framing · keep-alive ·       │
  │  pipelining · serializer · timeouts    │
  └───────────────────┬────────────────────┘
                      │  next_read_operation / next_write_operation
                      │  read · read_eof · report_write_result
                      │  yield_reader · yield_writer
  ┌───────────────────┴────────────────────┐
  │ ache-lwt │ ache-eio │ ache-miou         │
  │ sockets, a read loop and a write loop,  │
  │ timers, TLS                             │
  └───────────────────┬────────────────────┘
                      ▼
                    kernel
```

The core keeps all the HTTP state of a connection. It never opens, reads or
writes a socket. A runtime asks it what to do next, does that I/O, and gives
the result back. So one core runs on Lwt, Eio and Miou, and the core tests
need no sockets.

## The loops

Each connection has two loops: one reads and one writes. They are Lwt
callbacks in ache-lwt, fibers in ache-eio and tasks in ache-miou. The shape
is the same in all three.

```
      read loop                                  write loop
  ┌────────────────────┐                  ┌────────────────────┐
  │next_read_operation │                  │next_write_operation│
  └─┬────────┬───────┬─┘                  └─┬────────┬───────┬─┘
  `Read   `Yield  `Close                `Write   `Yield  `Close
    │        │       │                  iovecs     │       │
    ▼        │       ▼                     │       │       ▼
 read(2) into│   shut down                 ▼       │   loop ends
 the read    │   receiving             writev(2),  │
 buffer      │   loop ends             one call    │
    │        │                         for all the │
    ▼        │                         buffers     │
 core.read:  │                             │       │
 parse heads │                    report_write_result
 and bodies, │                             │       │
 call the    │                        back to the top
 request_handler                                   │
    │        ▼                                     ▼
 back to  yield_reader:                    yield_writer:
 the top  wait until the core              wait until the core
          wants more input                 has output
```

- `Read`: the runtime reads from the socket into the read buffer and gives
  the bytes to the core. While it parses, the core calls the
  `request_handler` with a `Reqd.t` for each request head.
- `Write`: the core hands over a list of buffers (the response head and body
  parts). The runtime sends them with one `writev`, or with one `sendmsg` in
  ache-miou, and reports how many bytes went out.
- `Yield`: the core has nothing to do now. For example, the write loop waits
  because the handler has not answered yet, or the read loop waits because a
  response will close the connection and the core reads no more requests.
  The loop gives the core a callback and stops. The core calls it when its
  state changes, and the loop starts again.
- `Close`: the loop ends. When both loops have ended, or one has failed, the
  runtime closes the socket once.

## One write for each read

A client can send several requests in one packet (pipelining). While the core
parses one read, it holds the writer (`hold` and `release` in
`lib/serialize.ml`). The responses to all the requests in that read are queued
and leave in one write when the read is done. Without the hold, a runtime
that wakes the writer at once, such as Lwt, would make one write call for each
response.

## Timeouts

The core decides which limit applies to the next read
(`Server_connection.read_timeout`):

- the head timeout, while a request head is only partly received;
- the idle timeout, on a keep-alive connection that waits for its next
  request;
- none, while a request is in progress, for example while a body arrives.

The runtime owns the clock and starts the timer. When the timer fires first,
the core sends `408` or closes the connection. Without `?idle_timeout` and
`?head_timeout` (and without `~clock` in ache-eio), no timer is used.

## The runtimes

### ache-lwt

- The loops are Lwt callbacks, started with `Lwt.async`. A loop starts again
  from a callback, so its stack does not grow with each request.
- A timed read is `Lwt.pick` of the socket read and the timer.
- `Ache_lwt.Timeout` keeps one Lwt sleep for each timeout length, shared by
  all connections. Each connection only moves its own entry in a list sorted
  by deadline. With Lwt's `select` and `poll` engines, a cancelled
  `Lwt_unix.sleep` stays in memory, so one sleep for each request would grow
  the heap.
- Lwt runs on one domain. To use more cores, run more processes.

### ache-eio

- The loops are two fibers. The read loop reads the socket itself when no
  timeout can cancel the read.
- A read under a timeout goes through a separate reader fiber, and the read
  loop waits for its result. A timeout then stops the wait, not the read:
  tls-eio treats a cancelled read as a permanent error. The client also
  always reads through the reader fiber, so `Client.shutdown` can stop the
  wait without cancelling the read.
- `Server.run` starts one accept loop for each domain, each with its own
  `SO_REUSEPORT` socket and no shared state. On Linux the kernel spreads the
  connections across the sockets. On macOS and FreeBSD one socket gets them
  all.

### ache-miou

- The loops are Miou tasks. As in ache-eio, the read loop reads the socket
  itself when no timeout can cancel the read, and a reader task, started on
  the first read that needs it, handles timed reads and the client.
- A timer sleeps at most 0.5 s at a time, so a cancelled timer does not stay
  in Miou's heap for the whole timeout.
- Writes use one `sendmsg` for all the buffers.
- `Server.run` works as in ache-eio, with one Miou domain for each accept
  loop.

## Where the time of a request goes

For a small `GET` with a small response on one core, most of the time is in
the kernel and the event loop, not in the HTTP code. In one measurement,
ache-eio used about 2 µs of CPU in user space and about 5–6 µs in the kernel
for each request. A faster parser moves such a benchmark only a little.
Fewer system calls, fewer fiber switches and fewer objects that live past a
minor GC move it more. The changes in the read loops above came from such
measurements.

## Compared with Node.js

The shape is close to Node's HTTP server: one thread, an event loop with
non-blocking sockets, and a parser that does no I/O (llhttp in Node). Node
also batches the writes of one tick (`socket.cork`), and groups its timers by
duration.

The main difference is the direction. In Node the socket pushes data to the
parser (`'data'` events), and the server pauses the socket to slow down. In
ache the runtime pulls: it asks the core for the next operation, so the core
decides when it reads more. The rules for keep-alive and pipelining are in
the core, not in the runtime, which is why the same core runs on three
runtimes.
