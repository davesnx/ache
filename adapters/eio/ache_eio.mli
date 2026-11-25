(*----------------------------------------------------------------------------
  Copyright (c) 2018 Inhabited Type LLC. Copyright (c) 2018 Anton Bachin
  Copyright (c) 2019 António Nuno Monteiro. Copyright (c) 2022 António Nuno
  Monteiro

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

module Server : sig
  val create_connection_handler :
     ?config:Ache.Config.t
    -> ?clock:_ Eio.Time.clock
    -> ?idle_timeout:float option
    -> ?head_timeout:float option
    -> ?body_timeout:float option
    -> ?write_timeout:float option
    -> ?date:bool
    -> ?stop:unit Eio.Promise.t
    -> ?grace:float
    -> request_handler:
         (Eio.Net.Sockaddr.stream -> Ache.Request_descriptor.t -> unit)
    -> error_handler:
         (Eio.Net.Sockaddr.stream -> Ache.Server_connection.error_handler)
    -> _ Eio.Flow.two_way
    -> Eio.Net.Sockaddr.stream
    -> unit
  (** [create_connection_handler ~request_handler ~error_handler flow addr]
      serves HTTP/1.1 on [flow] until the connection closes. The partial
      application [create_connection_handler ~request_handler ~error_handler]
      can be passed to [Eio.Net.run_server] or [Eio.Net.accept_fork]. The
      caller owns [flow] and closes it after the handler returns.

      Timeouts are in seconds. Each is on by default; [None] turns it off.
      - [idle_timeout] (default [Some 60.]): when no request is in progress
        and no byte arrives for [idle_timeout], the connection closes
        without a response. When [idle_timeout] is at least 1 s, each
        response after which the connection stays open gets
        [Keep-Alive: timeout=n], [n] being [idle_timeout] rounded down
        (see [keep_alive_timeout] in {!Ache.Server_connection.create}), and
        the connection then waits [idle_timeout + 1.] seconds for the next
        request, so that a client that trusts the hint does not send a
        request into a connection that is closing. On a connection upgraded
        with {!Websocket.upgrade}, when no byte arrives for [idle_timeout],
        the connection sends a close frame with code [1001] and closes.
      - [head_timeout] (default [Some 30.]): when a request head is not
        complete [head_timeout] after its first byte was read,
        [error_handler] is called with [`Request_timeout] (408) and the
        connection closes after the error response. Each request head,
        pipelined or not, has its own limit.
      - [body_timeout] (default [Some 60.]): while the connection waits for
        request body bytes (the application has scheduled a read on the
        body, or the response is complete and the rest of the body has not
        arrived), a clock runs. It stops while the connection does not
        wait for body bytes, and starts again from zero with each request
        and after each 2048 bytes of input. When it reaches
        [body_timeout], the body's [on_error] runs; then, if no response
        has started, [error_handler] gets [`Request_timeout] (408),
        otherwise the connection closes after its pending output. A body
        that trickles in, a few bytes at a time, times out.
      - [write_timeout] (default [Some 60.]): while a write to [flow]
        waits, a clock runs. It starts again from zero when the write is
        complete and after each 2048 bytes written. When it reaches
        [write_timeout], the write is cancelled and the connection closes:
        a flush in progress gets [`Closed], and nothing more is read or
        written. A client that stops reading a response is closed this way.

      While the request handler runs and the connection waits for nothing
      from the peer, no timeout applies: a slow handler is not timed out.

      Timeouts use [clock] when it is given, and the system clock of the
      Eio backend otherwise.

      [date] defaults to [true]: each response that is not 1xx and has no
      [Date] field gets [Date] with the current time (RFC 9110 §6.6.1), as
      {!Ache.Server_connection.create} describes. The text is built at most
      once per second in each domain. With [~date:false] no [Date] is added.

      Closing: when the connection stops reading (after a response with
      [Connection: close], an error response, or a timeout), it reads and
      discards input until the peer closes, so that unread input does not
      make the kernel reset the connection before the peer has read the
      response (RFC 9112 §9.6). It waits at most [idle_timeout], or
      {!linger_without_idle_timeout} without [idle_timeout], after its last
      write; input from the peer does not extend this time. A peer that
      closes its sending side (a half-close) ends the wait; it is not an
      error, and the response is sent in full. It then ends a read that
      still waits with [Eio.Flow.shutdown flow `Receive]. A flow whose
      [`Receive] shutdown does not end a pending read, such as a plain
      [Tls_eio] flow, keeps that read until input arrives;
      [Ache_eio_tls.Server] passes this shutdown to the TCP flow.

      A [shutdown] of [flow] that raises is ignored, so a flow without
      half-close (for example some TLS flows) ends the connection normally.

      The handler returns when the connection has ended and the sessions
      that {!Websocket.upgrade} started have returned.

      Graceful stop: when [stop] is resolved, the handler calls
      {!Ache.Server_connection.drain} on the connection, in a fiber of the
      connection:
      - An idle connection (no request in progress, no byte of a next
        request read) closes at once: a read that waits stops, and the
        connection closes as described below.
      - A request in progress, or one whose head is partly read, is served;
        its response gets [Connection: close] if its head is not written
        yet. No other request is read; pipelined requests are not served.
      - A WebSocket connection sends a close frame with code [1001].
      When [stop] is already resolved when the handler is applied, the
      connection closes without reading a request. [grace] is in seconds:
      [grace] seconds after [stop] is resolved, the
      handler cancels what is left of the connection (its reads, writes and
      WebSocket sessions) and returns. Without [grace] it waits for the
      connection to end. The handler does not close [flow].

      When [error_handler] or a WebSocket session raises, the connection
      closes, and after the connection ends the handler raises the first
      such exception with its backtrace, so that the [on_error] of [Eio.Net.run_server],
      [Eio.Net.accept_fork] or {!run} gets it. An exception that the
      connection gets from the core while it reports an error is raised in
      the same way. A peer that closes or resets the connection is not an
      error, and the handler returns normally.

      @raise Invalid_argument
        if a timeout or [grace] is not positive. It is raised when
        [~error_handler] is applied if all nine optional arguments are
        given too, and otherwise when the handler is applied
        to a flow: with [Eio.Net.run_server], in each connection fiber. *)

  val run :
     ?config:Ache.Config.t
    -> ?clock:_ Eio.Time.clock
    -> ?idle_timeout:float option
    -> ?head_timeout:float option
    -> ?body_timeout:float option
    -> ?write_timeout:float option
    -> ?date:bool
    -> ?stop:unit Eio.Promise.t
    -> ?grace:float
    -> ?domains:int
    -> ?backlog:int
    -> ?tcp_nodelay:bool
    -> on_error:(exn -> unit)
    -> net:_ Eio.Net.t
    -> domain_mgr:_ Eio.Domain_manager.t
    -> request_handler:
         (Eio.Net.Sockaddr.stream -> Ache.Request_descriptor.t -> unit)
    -> error_handler:
         (Eio.Net.Sockaddr.stream -> Ache.Server_connection.error_handler)
    -> [ `Tcp of Eio.Net.Ipaddr.v4v6 * int ]
    -> unit
  (** [run ~on_error ~net ~domain_mgr ~request_handler ~error_handler addr]
      serves HTTP/1.1 on [addr] with one accept loop in each of [domains]
      domains: the calling domain and [domains - 1] domains started with
      [domain_mgr]. Each domain opens its own listening socket on [addr] with
      [SO_REUSEADDR] and [SO_REUSEPORT]. On Linux, the kernel spreads new
      connections between these sockets. On macOS and FreeBSD,
      [SO_REUSEPORT] does not spread TCP connections: one socket gets them,
      so [run] works but only one domain accepts. Domains share no socket and
      no state of ache. [addr] must name a fixed port: with port 0, each
      domain listens on a different port.

      Each loop accepts connections with [Eio.Net.accept_fork] and [on_error],
      and serves them with the handler from {!create_connection_handler},
      which takes [config], [clock], [idle_timeout], [head_timeout],
      [body_timeout], [write_timeout], [date], [stop], [grace],
      [request_handler] and [error_handler].
      [request_handler] and [error_handler] run in several domains at the
      same time.

      When [tcp_nodelay] is [true], [run] sets [TCP_NODELAY] on each
      accepted socket before it is served, so that small writes are sent at
      once and are not held back by Nagle's algorithm.

      [domains] defaults to [Domain.recommended_domain_count ()], [backlog]
      to 128, [tcp_nodelay] to [false].

      [on_error] also gets the exception that a connection handler raises,
      for example one that [error_handler] raised; see
      {!create_connection_handler}.

      When an accept fails with an error that can go away, for example
      [EMFILE], [ENFILE] or [ECONNABORTED], the loop gives the exception to
      [on_error], waits 50 ms and accepts again. The connections in progress
      continue.

      Without [stop], [run] does not return: cancel its fiber to stop all
      domains at once. When [stop] is resolved, each domain stops accepting
      and closes its listening socket, so connections that the kernel
      queued and [run] did not accept are reset. Each connection then
      drains, as {!create_connection_handler} describes: a connection
      accepted just before [stop] closes without reading a request, and
      what is left [grace] seconds after [stop] is cancelled. [run] returns
      when every connection in every domain has ended. If
      [on_error] raises, a listen fails, or an accept fails because the
      listening socket is not usable ([EBADF], [EINVAL], [ENOTSOCK],
      [EFAULT], or an operation that is not available) in one domain, [run]
      cancels all domains and raises the exception.

      @raise Invalid_argument
        if [domains] is less than 1, or for the reasons that
        {!create_connection_handler} gives. *)

  val linger_without_idle_timeout : float
  (** Seconds a connection without [idle_timeout] waits for the peer to
      close after its last write: 2.0. *)
end

module Client : sig
  type t

  val create_connection :
     ?config:Ache.Config.t
    -> sw:Eio.Switch.t
    -> _ Eio.Flow.two_way
    -> t
  (** [create_connection ~sw flow] runs an HTTP/1.1 client on [flow].

      When a response keeps the connection open with
      [Keep-Alive: timeout=n], the connection is not reused once the time
      that {!Ache.Client_connection.reuse_timeout} gives ([n] minus 2 s)
      has passed since the response ended: one fiber of [sw] then shuts it
      down as {!shutdown} does, if no request has started. A {!request}
      after that time, also before that fiber has run, shuts the connection
      down first, and gets [`Connection_closed] without writing anything.
      Without the hint, the connection stays open. *)

  val request :
     t
    -> ?flush_headers_immediately:bool
    -> Ache.Request.t
    -> error_handler:Ache.Client_connection.error_handler
    -> response_handler:Ache.Client_connection.response_handler
    -> Ache.Body.Writer.t
  (** [request] is {!Ache.Client_connection.request}. When no request waits
      for a response, it first yields twice, so that a read that is complete
      reaches the connection, and closes the connection if response bytes
      or the end of input already arrived: the request then gets
      [`Connection_closed]. *)

  val shutdown : t -> unit Eio.Promise.t
  val is_closed : t -> bool

  val upgrade : t -> Ache.Runtime.impl -> unit
  (** [upgrade t impl] shuts down the HTTP state of [t] and hands reading and
      writing on its flow to [impl]. *)
end

(** A direct-style API for {!Ache.Websocket}. It does not depend on the flow
    type, so the servers of [Ache_eio.Server] and [Ache_eio_tls.Server] use
    it alike. *)
module Websocket : sig
  type t

  exception Closed
  (** The connection closed before a message given to {!send_text} or
      {!send_binary} was written, or before a pong answered {!ping}. *)

  val upgrade :
     ?max_message_size:int
    -> ?headers:Ache.Headers.t
    -> Ache.Request_descriptor.t
    -> (t -> unit)
    -> (unit, Ache.Websocket.handshake_error) result
  (** [upgrade ?max_message_size ?headers reqd session] is
      {!Ache.Websocket.upgrade}: it responds with [101], with [headers]
      added, when the request is a valid opening handshake
      and returns [Ok ()]. When it returns [Error _], it responded nothing:
      respond, for example with [400].

      After [Ok ()], [session t] runs in a new fiber of the connection, on the
      domain that runs it, once [upgrade] has returned. The handler of
      {!Server.create_connection_handler} returns when the connection has
      ended and [session] has returned: it does not cancel [session] when the
      socket closes. Once the connection has ended, {!receive} returns [None]
      and the other functions return or raise {!Closed}, so a session that
      uses them ends. [session] is cancelled only when the connection's fiber
      is, for example when the server stops.

      When [session] raises {!Closed}, it is ignored. When it raises another
      exception, the connection sends a close frame with code [1011] and the
      handler raises the exception after the connection ends, as for
      [error_handler].

      Messages that arrive before {!receive} is called are queued. When 8
      messages are queued, the connection stops reading from the client
      ({!Ache.Websocket.pause_reading}) until {!receive} takes one. Frames
      that a read already holds are still queued, so the queue can pass 8 by
      what one read buffer holds.

      @raise Invalid_argument
        for the reasons that {!Ache.Websocket.upgrade} gives, or if it is not
        called in a request handler of
        {!Server.create_connection_handler}, {!Server.run} or
        [Ache_eio_tls.Server], or in a fiber forked from it. *)

  val send_text : t -> string -> unit
  (** [send_text t message] sends [message] as one text frame and returns
      when the frame is written. [message] must be valid UTF-8; it is not
      checked.

      @raise Closed
        when the connection closed before, also when it closed before
        [send_text] was called: a loop of [send_text] ends when the client is
        gone. *)

  val send_binary : t -> string -> unit
  (** [send_binary t message] is {!send_text} for one binary frame. *)

  val receive : t -> Ache.Websocket.message option
  (** [receive t] waits for the next whole message. It returns [None] after
      the connection closed, whether the client sent a close frame, the
      connection ended, or the server closed it. {!client_close} then tells
      which.

      [receive] can be cancelled, for example by [Eio.Fiber.first] or
      [Eio.Time.with_timeout]. No message is lost: the next [receive] gets
      it. *)

  val ping : t -> string -> unit
  (** [ping t payload] sends a ping and returns when the next pong from the
      client arrives (any pong: RFC 6455 §5.5.3 lets a client answer only the
      latest ping, or send pongs unasked). To find a client that is gone:
      [Eio.Time.with_timeout_exn clock 30. (fun () -> ping t "")], then
      {!close} on [Eio.Time.Timeout]. [ping] can be cancelled.

      The pong comes after the messages the client sent before it. While 8
      messages wait for {!receive}, reading is paused, so the pong is not
      seen until {!receive} takes messages: keep receiving in another fiber
      while you wait for a pong.

      @raise Closed when the connection closes first, or closed before.
      @raise Invalid_argument
        at once if [payload] is longer than 125 bytes. *)

  val client_close : t -> Ache.Websocket.close option
  (** [client_close t] is the client's close frame (code and reason), once
      {!receive} returned [None]: see the [on_close] of
      {!Ache.Websocket.handler}. [None] before that, or when the client sent
      no valid close frame. *)

  val close : ?reason:string -> t -> int -> unit
  (** [close ?reason t code] sends a close frame with [code] and [reason] (see
      {!Ache.Websocket.close}) and returns when the connection is closed: the
      frame is written, or the connection ended. The socket closes when the
      read in progress also ends: when the client answers with its close
      frame or closes the connection, or at most [idle_timeout] later, as
      after any closing response (see
      {!Server.create_connection_handler}).

      @raise Invalid_argument
        at once if [code] is not one an endpoint may send, or [reason] is not
        valid UTF-8 or longer than 123 bytes. *)
end
