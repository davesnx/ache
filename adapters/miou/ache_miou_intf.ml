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

      Closing: when the connection stops reading (after a response with
      [Connection: close], an error response, or a timeout), it reads and
      discards input until the peer closes, so that unread input does not
      make the kernel reset the connection before the peer has read the
      response (RFC 9112 §9.6). With [idle_timeout] or [head_timeout], it
      waits at most [idle_timeout], or {!linger_without_idle_timeout} without
      [idle_timeout], after its last write. Without a timeout it does not
      wait: it shuts down reading at once, and the handler returns.

      A [shutdown] of [socket] that fails is ignored.

      @raise Invalid_argument
        if a timeout is not positive. It is raised when [~error_handler] is
        applied if all three optional arguments are given too, and otherwise
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
  (** [shutdown t] closes the connection and waits for its task to end. Call
      it from the task that called {!create_connection}, before that task
      returns: Miou requires a task to await the tasks it created. A second
      call does not wait. *)

  val is_closed : t -> bool

  val upgrade : t -> Ache.Runtime.impl -> unit
  (** [upgrade t impl] shuts down the HTTP state of [t] and hands reading and
      writing on its socket to [impl]. *)
end
