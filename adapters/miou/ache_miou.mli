(** Miou runtime for ache, on [Miou_unix] sockets.

    The application must ignore [SIGPIPE], for example with
    [Sys.set_signal Sys.sigpipe Sys.Signal_ignore], before it serves
    connections. Otherwise a write to a peer that closed the connection kills
    the process. ache does not install signal handlers.

    Each connection runs in tasks that the calling task creates with
    [Miou.async], in the domain of the caller. Callbacks given to ache (request
    and response handlers, body callbacks) run in these tasks: they must not
    leave a child task behind that they do not await, or Miou raises
    [Still_has_children]. A WebSocket session ({!Websocket.upgrade}) runs in
    its own task, which the connection task starts and awaits. *)

module type IO = Ache_miou_intf.IO
(** A socket type that the IO loop can drive. The loop runs one read and one
    write at a time on a socket, in different tasks.

    - [read socket buffer ~off ~len] reads into [buffer] in the region
      [\[off, off + len)] and returns the number of bytes read, at least 1.
      [len] is always positive. At end of input, and when the peer is gone (a
      reset, a socket that is not connected), it raises [End_of_file]. Any
      other exception ends the connection through the error path
      ([error_handler] with [`Exn]). The loop can cancel the task of a
      pending read.
    - [writev socket iovecs] writes the start of [iovecs] and returns
      [`Ok n], where [n] is the number of bytes written. When the peer is gone
      it returns [`Closed] and does not raise. Any other exception ends the
      connection through the error path.
    - [shutdown socket command] is called at most once for each direction.
      It must not raise. *)

module type Server = Ache_miou_intf.Server
module type Client = Ache_miou_intf.Client

module Make (Io : IO) : sig
  module Server : Server with type socket = Io.socket
  module Client : Client with type socket = Io.socket
end

module Server : sig
  include Server with type socket = Miou_unix.file_descr

  val run :
     ?config:Ache.Config.t
    -> ?idle_timeout:float option
    -> ?head_timeout:float option
    -> ?body_timeout:float option
    -> ?write_timeout:float option
    -> ?date:bool
    -> ?domains:int
    -> ?backlog:int
    -> ?tcp_nodelay:bool
    -> ?stop:unit Miou.Computation.t
    -> ?grace:float
    -> on_error:(exn -> unit)
    -> request_handler:(Unix.sockaddr -> Ache.Request_descriptor.t -> unit)
    -> error_handler:(Unix.sockaddr -> Ache.Server_connection.error_handler)
    -> Unix.sockaddr
    -> unit
  (** [run ~on_error ~request_handler ~error_handler addr] serves HTTP/1.1 on
      [addr] with one accept loop in each of [domains] Miou domains: the
      domain of the caller and [domains - 1] other domains of [Miou_unix.run].
      Each domain opens its own listening socket on [addr] with
      [SO_REUSEADDR] and [SO_REUSEPORT]. On Linux, the kernel spreads new
      connections between these sockets. On macOS and FreeBSD,
      [SO_REUSEPORT] does not spread TCP connections: one socket gets them,
      so [run] works but only one domain accepts. Domains share no socket and
      no state of ache. [addr] must name a fixed port: with port 0, each
      domain listens on a different port.

      Call it inside [Miou_unix.run]. Each accepted socket is served in its
      own task, in the domain that accepted it, by the handler from
      {!create_connection_handler}, which takes [config], the four timeouts
      (on by default), [date], [stop], [grace], [request_handler] and
      [error_handler].
      [request_handler] and [error_handler] run in several domains at the
      same time. When the handler of a connection raises (see
      {!create_connection_handler}), [on_error] is called with the exception
      at once, in the task of the connection, and the socket is closed.

      When [accept] fails because the process or the system has no free file
      descriptors or memory ([EMFILE], [ENFILE], [ENOBUFS], [ENOMEM]),
      [on_error] is called with the [Unix.Unix_error], in the accept loop of
      that domain, and the loop waits 0.1 s and accepts again. For accept
      errors about one connection ([ECONNABORTED], [EPERM], and network
      errors), [on_error] is called the same way, and the loop accepts again
      at once.

      When [tcp_nodelay] is [true], [run] sets [TCP_NODELAY] on each
      accepted socket before it is served, so that small writes are sent at
      once and are not held back by Nagle's algorithm.

      [domains] defaults to all domains: [Miou_unix.run ~domains:n] gives
      [n + 1]. [backlog] defaults to 128, [tcp_nodelay] to [false].

      Graceful stop: when [stop] is resolved (returned or cancelled, from any
      domain), each domain stops accepting and closes its listening socket.
      A connection that the kernel queued but that was not accepted yet is
      reset when the socket closes; one accepted at the same time as [stop]
      is closed without a response. Each connection is drained as
      {!create_connection_handler} describes for [stop] and [grace]: idle
      connections close at once, requests in progress finish with
      [Connection: close], and [grace] seconds after [stop] what is left is
      cancelled. [run] returns when every connection of every domain has
      ended.

      Without [stop], [run] does not return. Cancelling its task stops all
      domains at once, with their connections. If [on_error] raises, or a
      listen or another accept error occurs in one domain, [run] cancels all
      domains and raises the exception.

      @raise Invalid_argument
        if [domains] is less than 1 or more than the domains available, if
        [addr] is a Unix domain socket address, or for the reasons that
        {!create_connection_handler} gives. *)
end

module Client : Client with type socket = Miou_unix.file_descr

(** WebSocket sessions in direct style.

    {[
      let rec echo ws =
        match Ache_miou.Websocket.receive ws with
        | None -> ()
        | Some (Text message) ->
          Ache_miou.Websocket.send_text ws message;
          echo ws
        | Some (Binary message) ->
          Ache_miou.Websocket.send_binary ws message;
          echo ws

      let request_handler _client_address reqd =
        match Ache_miou.Websocket.upgrade reqd echo with
        | Ok () -> ()
        | Error _ -> (* respond, for example with 400 *) ...
    ]}

    The module does not depend on the socket type: it works with the servers
    of {!Server}, of {!Make} and of [ache-miou-tls]. *)
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
      added, when the request is a valid opening handshake,
      and then runs [session] in its own task. When it returns [Error _], it
      responded nothing: respond, for example with [400].

      The session task is a child of the connection task, in the same domain,
      started with [Miou.async]. The server handler of the connection returns
      when the session returns, also after the socket closed: the connection
      does not cancel the session. Once the connection has ended, {!receive}
      returns [None] and the other functions raise {!Closed} or return, so a
      session that uses them ends. The session is cancelled only when the
      connection task is cancelled.

      {!Closed} raised by [session] is ignored. Another exception sends a
      close frame with code [1011] and closes the connection, and the server
      handler raises it when it returns: with {!Server.run}, it goes to
      [on_error].

      [upgrade] finds its connection through an effect handler around the
      request handler, so call it from the request handler itself, not from
      a task that the handler starts.

      Call the functions of this module from the session task or from tasks
      that it starts with [Miou.async]: the connection has no lock, so they
      must run in the domain of the connection.

      Messages that arrive before {!receive} is called are queued. When 8
      messages are queued, the connection stops reading from the client
      ({!Ache.Websocket.pause_reading}) until {!receive} takes one. Frames
      that a read already holds are still queued, so the queue can pass 8 by
      what one read buffer holds.

      @raise Invalid_argument
        for the reasons that {!Ache.Websocket.upgrade} gives, or when
        [upgrade] is not called from the request handler of a server of this
        library, while the handler runs. *)

  val send_text : t -> string -> unit
  (** [send_text t message] sends [message] as one text frame and returns
      when the frame is written. [message] must be valid UTF-8; it is not
      checked.

      @raise Closed
        when the connection closed before, also when it closed before
        [send_text] was called: a loop of [send_text] ends when the client
        is gone. *)

  val send_binary : t -> string -> unit
  (** [send_binary t message] is {!send_text} for one binary frame. *)

  val receive : t -> Ache.Websocket.message option
  (** [receive t] waits for the next whole message. It returns [None] after
      the connection closed, whether the client sent a close frame, the
      connection ended, or the server closed it. {!client_close} then tells
      which.

      The task that waits in [receive] can be cancelled, for example with
      [Miou.cancel] or by losing a [Miou.await_first]. No message is lost:
      the next [receive] gets it. *)

  val ping : t -> string -> unit
  (** [ping t payload] sends a ping and waits for the next pong from the
      client (any pong: RFC 6455 §5.5.3 lets a client answer only the latest
      ping, or send pongs unasked). To find a client that is gone, race
      [ping] with a task that sleeps, with [Miou.await_first], and {!close}
      when the sleep wins. The task that waits in [ping] can be cancelled.

      The pong comes after the messages the client sent before it. While 8
      messages wait for {!receive}, reading is paused, so the pong is not
      seen until {!receive} takes messages: keep receiving while you wait
      for a pong.

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
      {!Ache.Websocket.close}). It returns when the connection is closed: the
      frame is written, or the connection ended. The connection stops
      reading, and closes as any closing connection of
      {!Server.create_connection_handler} does: it reads and drops input
      until the client closes, at most [idle_timeout] (the default), and at
      once without timeouts.

      A session whose client sends nothing for [idle_timeout] is closed with
      code 1001: {!receive} returns [None].

      @raise Invalid_argument
        at once if [code] is not one an endpoint may send, or [reason] is not
        valid UTF-8 or longer than 123 bytes. *)
end
