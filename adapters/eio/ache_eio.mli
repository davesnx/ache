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
    -> ?idle_timeout:float
    -> ?head_timeout:float
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

      Timeouts are in seconds and are off by default:
      - [idle_timeout]: when no request is in progress and no byte arrives
        for [idle_timeout], the connection closes without a response.
      - [head_timeout]: when a request head is not complete [head_timeout]
        after its first byte was read, [error_handler] is called with
        [`Request_timeout] (408) and the connection closes after the error
        response. Each request head, pipelined or not, has its own limit.

      A request body or a response in progress is never timed out.

      Closing: when the connection stops reading (after a response with
      [Connection: close], an error response, or a timeout), it reads and
      discards input until the peer closes, so that unread input does not
      make the kernel reset the connection before the peer has read the
      response (RFC 9112 §9.6). With [clock], it waits at most
      [idle_timeout], or {!linger_without_idle_timeout} without
      [idle_timeout], after its last write; input from the peer does not
      extend this time. A peer that closes its sending side (a half-close)
      ends the wait; it is not an error, and the response is sent in full.
      Without [clock] it does not wait: it shuts down the flow at once, and
      the handler returns.

      A [shutdown] of [flow] that raises is ignored, so a flow without
      half-close (for example some TLS flows) ends the connection normally.

      When [error_handler] raises, the connection closes, and after the
      connection ends the handler raises the first such exception with its
      backtrace, so that the [on_error] of [Eio.Net.run_server],
      [Eio.Net.accept_fork] or {!run} gets it. An exception that the
      connection gets from the core while it reports an error is raised in
      the same way. A peer that closes or resets the connection is not an
      error, and the handler returns normally.

      @raise Invalid_argument
        if a timeout is given without [clock] or a timeout is not positive.
        It is raised when [~error_handler] is applied if all four optional
        arguments are given too, and otherwise when the handler is applied
        to a flow: with [Eio.Net.run_server], in each connection fiber. *)

  val run :
     ?config:Ache.Config.t
    -> ?clock:_ Eio.Time.clock
    -> ?idle_timeout:float
    -> ?head_timeout:float
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

      [run] does not return. Cancel its fiber to stop all domains. If
      [on_error] raises, a listen fails, or an accept fails because the
      listening socket is not usable ([EBADF], [EINVAL], [ENOTSOCK],
      [EFAULT], or an operation that is not available) in one domain, [run]
      cancels all domains and raises the exception.

      @raise Invalid_argument
        if [domains] is less than 1, or for the reasons that
        {!create_connection_handler} gives. *)

  val linger_without_idle_timeout : float
  (** Seconds a connection with [clock] and no [idle_timeout] waits for the
      peer to close after its last write: 2.0. *)
end

module Client : sig
  type t

  val create_connection :
     ?config:Ache.Config.t
    -> sw:Eio.Switch.t
    -> _ Eio.Flow.two_way
    -> t

  val request :
     t
    -> ?flush_headers_immediately:bool
    -> Ache.Request.t
    -> error_handler:Ache.Client_connection.error_handler
    -> response_handler:Ache.Client_connection.response_handler
    -> Ache.Body.Writer.t

  val shutdown : t -> unit Eio.Promise.t
  val is_closed : t -> bool

  val upgrade : t -> Ache.Runtime.impl -> unit
  (** [upgrade t impl] shuts down the HTTP state of [t] and hands reading and
      writing on its flow to [impl]. *)
end
