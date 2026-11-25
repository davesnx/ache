(*----------------------------------------------------------------------------
  Copyright (c) 2018 Inhabited Type LLC. Copyright (c) 2018 Anton Bachin
  Copyright (c) 2019-2020 António Nuno Monteiro

  All rights reserved.

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions are met:

  1. Redistributions of source code must retain the above copyright notice, this
  list of conditions and the following disclaimer.

  2. Redistributions in binary form must reproduce the above copyright notice,
  this list of conditions and the following disclaimer in the documentation
  and/or other materials provided with the distribution.

  3. Neither the name of the author nor the names of his contributors may be
  used to endorse or promote products derived from this software without
  specific prior written permission.

  THIS SOFTWARE IS PROVIDED BY THE CONTRIBUTORS ``AS IS'' AND ANY EXPRESS OR
  IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
  MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO
  EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
  INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
  LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
  PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
  LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
  NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,
  EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
  ----------------------------------------------------------------------------*)

module type IO = Ache_lwt_intf.IO
(** A socket type that the IO loop can drive. The loop runs one read and one
    write at a time on a socket.

    - [read socket buffer ~off ~len] reads into [buffer] in the region
      [\[off, off + len)] and returns the number of bytes read, at least 1.
      [len] is always positive. At end of input, and when the peer is gone (a
      reset, a socket that is not connected), it fails with [End_of_file].
      Any other failure ends the connection through the error path
      ([error_handler] with [`Exn]).
    - [writev socket iovecs] writes the start of [iovecs] and returns
      [`Ok n], where [n] is the number of bytes written. [n] can be smaller
      than the total: the loop gives the remaining bytes again in the next
      call. When the peer is gone ([EPIPE], [ECONNRESET], [ENOTCONN], or the
      socket was closed) it returns [`Closed] and does not fail; pending
      [Body.Writer.flush] callbacks then get [`Closed]. Any other failure ends
      the connection through the error path.
    - [shutdown_receive socket] is called once, when the connection stops
      reading. An exception from it is ignored.
    - [close socket] is called once per socket, when both directions are
      done, also when [Client.shutdown] closes it first. A failure of the
      returned promise is ignored. *)

module type Server = Ache_lwt_intf.Server

module type Client = Ache_lwt_intf.Client
(** [Client.upgrade t impl] shuts down the HTTP state of [t] and hands reading
    and writing on its socket to [impl].

    Reuse deadline: when a response keeps the connection open and has
    [Keep-Alive: timeout=n], the connection is not reused after [s] seconds,
    from when its reader sees the end of that response ([s] is
    {!Ache.Client_connection.reuse_timeout}: [min n 600] minus 2). When [s]
    passes and no request was made, the connection shuts down as with
    [shutdown]: [is_closed] becomes [true] and the socket closes. A
    [request] made after [s] passed shuts the connection down first, so its
    [error_handler] gets [`Connection_closed] and nothing is written. A new
    [request] before [s] clears the deadline. Without the hint nothing
    changes. *)

(** Timeouts that share one Lwt timer for each length.

    With Lwt's [select] and [poll] engines, a cancelled [Lwt_unix.sleep] stays
    in memory until no other sleep in the engine ends before it. A server that
    starts a sleep for each connection and cancels it when the connection ends
    keeps about (connections per second × timeout) cancelled sleeps. [Timeout]
    puts all timeouts of one length in one queue, in deadline order, with one
    sleep for the front of the queue that is never cancelled. [set] and [stop]
    do not start or cancel a sleep.

    The servers of [ache-lwt] and [ache-lwt-tls] use [Timeout] for
    [idle_timeout], [head_timeout], [close_timeout] and the TLS handshake.
    Use it only from the thread that runs Lwt. *)
module Timeout : sig
  type t

  val create : float -> t
  (** [create seconds] is a timeout of [seconds] that is not set. Timeouts with
      equal [seconds] share one queue, which stays allocated for the life of
      the program.

      @raise Invalid_argument if [seconds] is not positive. *)

  val set : t -> unit
  (** [set t] sets the deadline of [t] to now + [seconds], also when [t] is
      already set. *)

  val stop : t -> unit
  (** [stop t] removes the deadline of [t]. It does nothing when [t] is not
      set. *)

  val expired : t -> unit Lwt.t
  (** [expired t] resolves when the deadline of [t] passes. A [set] before the
      deadline moves it. After [stop], the promise stays pending until a later
      [set] and its deadline. After the promise resolved, [set t] starts a new
      deadline: call [expired t] again to wait for it. Cancelling the promise
      does not change [t]. *)
end

(** A promise API for {!Ache.Websocket}, for any socket type: it does not
    depend on [IO], so the servers of [Make] and [ache-lwt-tls] use it too. *)
module Websocket : sig
  type t

  exception Closed
  (** The connection closed before a message given to {!send_text} or
      {!send_binary} was written, or before a pong answered {!ping}. *)

  val upgrade :
     ?max_message_size:int
    -> ?headers:Ache.Headers.t
    -> Ache.Request_descriptor.t
    -> (t -> unit Lwt.t)
    -> (unit, Ache.Websocket.handshake_error) result
  (** [upgrade ?max_message_size ?headers reqd session] is
      {!Ache.Websocket.upgrade}: it responds with [101], with [headers]
      added, when the request is a valid opening handshake,
      starts [session] with the new connection, and returns [Ok ()]. When it
      returns [Error _], it responded nothing and [session] does not run:
      respond, for example with [426] and [Sec-WebSocket-Version: 13] for
      [`Unsupported_version], [400] otherwise.

      [session] runs like [Lwt.async]: the request handler does not wait for
      it. When the promise of [session] resolves, or fails with {!Closed},
      the connection closes with code [1000], unless it has already ended:
      a session can end on a failed send. Another exception sends a close frame with code [1011],
      then goes to [Lwt.async_exception_hook]. The default hook exits the
      program: a server sets its own.

      Messages that arrive before {!receive} is called are queued. When 8
      messages are queued, the connection stops reading from the client
      ({!Ache.Websocket.pause_reading}) until {!receive} takes one. Frames
      that a read already holds are still queued, so the queue can pass 8 by
      what one read buffer holds.

      @raise Invalid_argument for the reasons that {!Ache.Websocket.upgrade}
      gives. *)

  val send_text : t -> string -> unit Lwt.t
  (** [send_text t message] sends [message] as one text frame. [message] must
      be valid UTF-8; it is not checked. The promise resolves when the frame
      is written. It fails with {!Closed} when the connection closed before,
      also when it closed before [send_text] was called: a loop of
      [send_text] ends when the client is gone. *)

  val send_binary : t -> string -> unit Lwt.t
  (** [send_binary t message] is {!send_text} for one binary frame. *)

  val receive : t -> Ache.Websocket.message option Lwt.t
  (** [receive t] is the next whole message, or [None] after the connection
      closed, whether the client sent a close frame, the connection ended, or
      the server closed it. {!client_close} then tells which.

      The promise can be cancelled, for example by [Lwt.pick] or
      [Lwt_unix.with_timeout]. No message is lost: the next [receive] gets
      it. *)

  val ping : t -> string -> unit Lwt.t
  (** [ping t payload] sends a ping. The promise resolves when the next pong
      from the client arrives (any pong: RFC 6455 §5.5.3 lets a client answer
      only the latest ping, or send pongs unasked), and fails with {!Closed}
      when the connection closes first. To find a client that is gone:
      [Lwt_unix.with_timeout 30. (fun () -> ping t "")], then {!close} on
      [Lwt_unix.Timeout]. A client that is gone does not answer the close
      frame: the connection waits for it at most [close_timeout] (10 s by
      default). The promise can be cancelled.

      The pong comes after the messages the client sent before it. While 8
      messages wait for {!receive}, reading is paused, so the pong is not
      seen until {!receive} takes messages: keep receiving while you wait
      for a pong.

      @raise Invalid_argument
        at once if [payload] is longer than 125 bytes. *)

  val client_close : t -> Ache.Websocket.close option
  (** [client_close t] is the client's close frame (code and reason), once
      the connection has ended: see the [on_close] of
      {!Ache.Websocket.handler}. It is set when the connection ends, also
      while {!receive} still returns queued messages. [None] before that, or
      when the connection ended without a valid close frame from the client:
      an end of input, a reset, a protocol error, or the server's own close
      that the client did not answer. *)

  val close : ?reason:string -> t -> int -> unit Lwt.t
  (** [close ?reason t code] sends a close frame with [code] and [reason] (see
      {!Ache.Websocket.close}). The promise resolves when the connection has
      ended: the client answered with its close frame (then in
      {!client_close}), closed the connection, or the connection failed.
      Messages that arrive meanwhile are dropped.

      The connection waits at most [close_timeout] of
      [create_connection_handler] (10 s by default) for the client's close
      frame, then closes without it: {!client_close} is [None]. Messages
      that arrive meanwhile do not extend the wait. With
      [~close_timeout:None] it waits until the client answers or closes.

      @raise Invalid_argument
        at once, before it returns a promise, if [code] is not one an endpoint
        may send, or [reason] is not valid UTF-8 or longer than 123 bytes. *)
end

module Make (Io : IO) : sig
  module Server : Server with type socket = Io.socket and type addr = Io.addr
  module Client : Client with type socket = Io.socket
end

(** The function that results from [create_connection_handler] can be passed to
    [Lwt_io.establish_server_with_client_socket].

    Timeouts of [create_connection_handler] are in seconds. Each is on by
    default; [~idle_timeout:None] (and so on) turns one off.
    - [idle_timeout] (default [Some 60.]): when no request is in progress and
      no byte arrives for [idle_timeout], the connection closes without a
      response. On a connection upgraded with {!Websocket.upgrade}: when no
      frame arrives for [idle_timeout], the connection sends a close frame
      with code [1001] and closes at once, without waiting for the client's
      close frame.
    - [head_timeout] (default [Some 30.]): when a request head is not
      complete [head_timeout] after its first bytes were read,
      [error_handler] is called with [`Request_timeout] (408) and the
      connection closes after the error response. Each request head,
      pipelined or not, has its own limit.
    - [body_timeout] (default [Some 60.]): the connection waits for request
      body bytes (a read is scheduled on the body, or the response is
      complete and the rest of the body has not arrived) and the client
      sends less than 2048 bytes in [body_timeout] of waiting. The body gets
      [on_error]; then the client gets 408 if no response started, and
      otherwise the connection closes after its pending output. The clock
      counts only while the connection waits for body bytes, restarts at
      each request and after each 2048 bytes, and counts in tenths of
      [body_timeout], so it can run out up to a tenth late.
    - [write_timeout] (default [Some 60.]): output waits to be written and
      less than 2048 bytes of it are written in [write_timeout], for example
      because the client stopped reading. The connection closes its socket
      and ends; pending [Body.Writer.flush] callbacks get [`Closed]. The
      clock restarts when all output is written and after each 2048 bytes.
    - [close_timeout] (default [Some 10.]): on a connection upgraded with
      {!Websocket.upgrade}, the server sent its close frame and the client
      does not answer with its own within [close_timeout]. The connection
      closes without it. The clock starts when the server's close frame is
      queued, by {!Websocket.close}, the end of a session or
      {!Ache.Websocket.close}. Messages that arrive meanwhile do not restart
      it. The close frames of [idle_timeout] and of a drain end the
      connection at once and do not use it.

    Keep-Alive hint: when [idle_timeout] is [Some x] with [x >= 1.], each
    response after which the connection stays open gets
    [Keep-Alive: timeout=n] ([n] is [x] rounded down), with [, max=m] when
    [Config.max_requests_per_connection] is set, unless the application set
    a [Keep-Alive] field (see {!Ache.Server_connection.create}). The timer
    between requests then runs [x +. 1.] seconds, so that a client that
    trusts the hint does not write into a connection that is closing. The
    WebSocket idle timer stays [x]. With [x < 1.] or [None] no hint is
    sent.

    While the request handler runs and the connection waits for nothing
    from the client, no timeout applies.

    [date] defaults to [true]: each response that is not 1xx and has no [Date]
    field gets [Date] with the current time (RFC 9110 §6.6.1), as
    {!Ache.Server_connection.create} describes. The text is built at most once
    per second in each domain. With [~date:false] no [Date] is added.

    Graceful stop: give the same [stop] promise to each connection. When
    [stop] resolves (or is rejected):
    + the connection calls {!Ache.Server_connection.drain}. An idle
      connection closes at once. A connection with a request in progress
      serves it, with [Connection: close] if its head is not written yet, and
      then closes; requests pipelined after it are not served. A WebSocket
      connection sends a close frame with code [1001] and closes at once.
    + [grace] seconds later, a connection that is still open is cancelled:
      it stops, and its socket is closed without waiting for the handler or
      for pending output, and the promise of the connection resolves.
      Without [grace] the connection has no time limit.

    A handler applied to a socket after [stop] resolved closes the socket
    and does not read a request. [create_connection_handler] does not stop
    the accept loop: stop it yourself, for example with
    [Lwt_io.shutdown_server], and wait for the promises of the connections.
    A [stop] that is never resolved holds no memory for connections that
    ended.

    Closing: when the connection stops reading it calls [shutdown_receive]
    and does not wait for the peer to close; it closes the socket once the
    response is written. Input that the peer sent and the connection did not
    read can make the kernel reset the connection. [Ache_eio] with a clock
    instead reads and discards input for a bounded time first.

    [create_connection_handler] raises [Invalid_argument] when applied to a
    socket if a timeout or [grace] is not positive.

    The application must ignore [SIGPIPE], for example with
    [Sys.set_signal Sys.sigpipe Sys.Signal_ignore], before it serves
    connections. Otherwise a write to a peer that closed the connection kills
    the process. With [SIGPIPE] ignored, that write ends the connection and
    pending [Body.Writer.flush] callbacks get [`Closed]. ache does not install
    signal handlers. *)
module Server :
  Server with type socket = Lwt_unix.file_descr and type addr = Unix.sockaddr

module Client : Client with type socket = Lwt_unix.file_descr
