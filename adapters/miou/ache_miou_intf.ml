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
    -> ?idle_timeout:float
    -> ?head_timeout:float
    -> ?date:bool
    -> request_handler:(Unix.sockaddr -> Ache.Request_descriptor.t -> unit)
    -> error_handler:(Unix.sockaddr -> Ache.Server_connection.error_handler)
    -> socket
    -> Unix.sockaddr
    -> unit
  (** [create_connection_handler ~request_handler ~error_handler socket addr]
      serves HTTP/1.1 on [socket] until the connection closes. Run it in its
      own task for each socket that [Miou_unix.accept] returns. The caller
      owns [socket] and closes it after the handler returns.

      Timeouts are in seconds and are off by default:
      - [idle_timeout]: when no request is in progress and no byte arrives
        for [idle_timeout], the connection closes without a response.
      - [head_timeout]: when a request head is not complete [head_timeout]
        after its first byte was read, [error_handler] is called with
        [`Request_timeout] (408) and the connection closes after the error
        response. Each request head, pipelined or not, has its own limit.

      A request body or a response in progress is never timed out.

      [date] defaults to [true]: each response that is not 1xx and has no
      [Date] field gets [Date] with the current time (RFC 9110 §6.6.1), as
      {!Ache.Server_connection.create} describes. The text is built at most
      once per second in each domain. With [~date:false] no [Date] is added.

      Closing: when the connection stops reading (after a response with
      [Connection: close], an error response, or a timeout), it reads and
      discards input until the peer stops sending, so that unread input does
      not make the kernel reset the connection before the peer has read the
      response (RFC 9112 §9.6). With [idle_timeout] or [head_timeout], it
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

      When a response completes while a read of its request body waits, the
      request body fails ([on_error] runs) at once without [idle_timeout], or
      after [idle_timeout] with it when no byte arrives first. The connection
      then closes as described above: with a timeout it waits at most
      [idle_timeout], or {!linger_without_idle_timeout}, for the peer, and
      then ends the pending read with a shutdown of reading on [socket];
      without a timeout it ends the read at once. A body read that starts
      after the response is complete has the same limits.

      A [shutdown] of [socket] that fails is ignored.

      The handler starts the sessions of [Ache_miou.Websocket.upgrade] in
      tasks of its own, and returns when they return.

      @raise Invalid_argument
        if a timeout is not positive. It is raised when [~error_handler] is
        applied if all four optional arguments are given too, and otherwise
        when the handler is applied to a socket. *)

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
