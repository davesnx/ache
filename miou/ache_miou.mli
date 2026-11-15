(** Miou runtime for ache, on [Miou_unix] sockets.

    The application must ignore [SIGPIPE], for example with
    [Sys.set_signal Sys.sigpipe Sys.Signal_ignore], before it serves
    connections. Otherwise a write to a peer that closed the connection kills
    the process. ache does not install signal handlers.

    Each connection runs in tasks that the calling task creates with
    [Miou.async], in the domain of the caller. Callbacks given to ache (request
    and response handlers, body callbacks) run in these tasks: they must not
    leave a child task behind that they do not await, or Miou raises
    [Still_has_children]. *)

module Server : sig
  val create_connection_handler :
     ?config:Ache.Config.t
    -> ?idle_timeout:float
    -> ?head_timeout:float
    -> request_handler:(Unix.sockaddr -> Ache.Reqd.t -> unit)
    -> error_handler:(Unix.sockaddr -> Ache.Server_connection.error_handler)
    -> Miou_unix.file_descr
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
      response (RFC 9112 §9.6). It waits at most [idle_timeout], or
      {!linger_without_idle_timeout} without [idle_timeout], after its last
      write.

      A [shutdown] of [socket] that fails is ignored.

      @raise Invalid_argument
        if a timeout is not positive. It is raised when [~error_handler] is
        applied if all three optional arguments are given too, and otherwise
        when the handler is applied to a socket. *)

  val linger_without_idle_timeout : float
  (** Seconds a connection without [idle_timeout] waits for the peer to close
      after its last write: 2.0. *)
end

module Client : sig
  type t

  val create_connection : ?config:Ache.Config.t -> Miou_unix.file_descr -> t
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
