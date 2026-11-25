module type IO = sig
  type socket

  val read : socket -> Ache.Bigstring.t -> off:int -> len:int -> int

  val writev :
     socket
    -> Ache.Bigstring.t Ache.IOVec.t list
    -> [ `Closed | `Ok of int ]

  val shutdown : socket -> Unix.shutdown_command -> unit
end

module type Server = sig
  type socket

  val create_connection_handler :
     ?config:Ache.Config.t
    -> ?idle_timeout:float option
    -> ?head_timeout:float option
    -> ?body_timeout:float option
    -> ?write_timeout:float option
    -> ?date:bool
    -> ?stop:unit Miou.Computation.t
    -> ?grace:float
    -> request_handler:(Unix.sockaddr -> Ache.Request_descriptor.t -> unit)
    -> error_handler:(Unix.sockaddr -> Ache.Server_connection.error_handler)
    -> socket
    -> Unix.sockaddr
    -> unit
  (** [create_connection_handler ~request_handler ~error_handler socket addr]
      serves HTTP/1.1 on [socket] until the connection closes. Run it in its
      own task for each socket that [Miou_unix.accept] returns. The caller
      owns [socket] and closes it after the handler returns.

      Timeouts are in seconds. Each one is on by default; [None] turns it
      off:
      - [idle_timeout] (default [Some 60.]): when no request is in progress
        and no byte arrives for [idle_timeout], the connection closes without
        a response. On a connection upgraded with
        [Ache_miou.Websocket.upgrade], when no byte arrives for
        [idle_timeout], the connection sends a close frame with code 1001
        and closes when it is written.
      - [head_timeout] (default [Some 30.]): when a request head is not
        complete [head_timeout] after its first byte was read,
        [error_handler] is called with [`Request_timeout] (408) and the
        connection closes after the error response. Each request head,
        pipelined or not, has its own limit.
      - [body_timeout] (default [Some 60.]): the connection waits for request
        body bytes when the application has scheduled a read of the body, or
        when the response is complete and the rest of the body has not
        arrived. The time spent in these waits adds up, and starts again from
        zero at each request and after each 2048 bytes of the body, so a
        peer that sends a few bytes at a time does not keep it from running
        out. When it reaches [body_timeout], the body fails ([on_error]
        runs); then [error_handler] gets [`Request_timeout] (408) if no
        response has started, and otherwise the connection closes after the
        output that waits.
      - [write_timeout] (default [Some 60.]): while output waits to be
        written, the time adds up, and starts again from zero when all of it
        is written and after each 2048 bytes written. When it reaches
        [write_timeout], for example because the peer stopped reading, the
        write is dropped, the flushes that wait get [`Closed], and the
        connection closes at once.

      While the request handler runs and waits for nothing from the peer, no
      timeout applies.

      [date] defaults to [true]: each response that is not 1xx and has no
      [Date] field gets [Date] with the current time (RFC 9110 §6.6.1), as
      {!Ache.Server_connection.create} describes. The text is built at most
      once per second in each domain. With [~date:false] no [Date] is added.

      Closing: when the connection stops reading (after a response with
      [Connection: close], an error response, or a timeout), it reads and
      discards input until the peer stops sending, so that unread input does
      not make the kernel reset the connection before the peer has read the
      response (RFC 9112 §9.6). With [idle_timeout] or [head_timeout] (the
      default), it
      waits at most [idle_timeout], or {!linger_without_idle_timeout} without
      [idle_timeout], after it stops reading or after its last write,
      whichever is later. Input that arrives does not extend this limit. When
      the peer stops sending, the response in progress is still written.
      Without a timeout it does not wait: it shuts down reading at once, and
      the handler returns.

      When [error_handler] raises, or the connection gets an exception that
      it cannot give to [error_handler], the connection closes and the
      handler raises that exception. A peer that resets or closes the
      connection is not an exception.

      With [~body_timeout:None], a request body is waited for without limit,
      also after its response is complete.

      A [shutdown] of [socket] that fails is ignored.

      The handler starts the sessions of [Ache_miou.Websocket.upgrade] in
      tasks of its own, and returns when they return.

      Graceful stop: when [stop] is resolved (returned or cancelled, from any
      domain), the connection is drained ({!Ache.Server_connection.drain}):
      - An idle connection (no request in progress, no byte of a next request
        read) closes at once. The read that waits is ended with a shutdown of
        reading on [socket].
      - A request in progress finishes. Its response gets
        [Connection: close] if its head is not written yet, and the
        connection closes after it. Pipelined requests are not served.
      - A WebSocket connection sends a close frame with code 1001 and closes
        when the frame is written.

      [grace] counts from [stop]. When it passes, the connection tasks and
      its WebSocket sessions are cancelled, the socket is shut down, and the
      handler returns. A request handler that still runs is cancelled with
      them. Without [grace], the handler waits for the connection to end. A
      handler applied to a socket after [stop] is resolved returns at once
      and reads nothing. [grace] without [stop] does nothing.

      The handler waits for [stop] in a task of the connection, so [drain]
      runs in the domain of the connection.

      @raise Invalid_argument
        if a timeout or [grace] is not positive. It is raised when
        [~error_handler] is applied if all eight optional arguments are given
        too, and otherwise when the handler is applied to a socket. *)

  val linger_without_idle_timeout : float
  (** Seconds a connection with [head_timeout] and no [idle_timeout] waits
      for the peer to close after its last write: 2.0. *)
end

module type Client = sig
  type socket
  type t

  val create_connection : ?config:Ache.Config.t -> socket -> t
  (** [create_connection socket] starts a task that runs the connection on the
      connected [socket]. The caller owns [socket] and closes it after
      {!shutdown}. *)

  val request :
     t
    -> ?flush_headers_immediately:bool
    -> Ache.Request.t
    -> error_handler:Ache.Client_connection.error_handler
    -> response_handler:Ache.Client_connection.response_handler
    -> Ache.Body.Writer.t

  val shutdown : t -> unit
  (** [shutdown t] closes the connection at once and waits for its task to
      end. Output that is not yet written to the socket is dropped, so a
      write that waits for a peer that does not read does not hold it. Call
      it from the task that called {!create_connection}, before that task
      returns: Miou requires a task to await the tasks it created. A second
      call does not wait. *)

  val is_closed : t -> bool

  val upgrade : t -> Ache.Runtime.impl -> unit
  (** [upgrade t impl] shuts down the HTTP state of [t] and hands reading and
      writing on its socket to [impl]. *)
end
