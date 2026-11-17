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
    -> ?idle_timeout:float
    -> ?head_timeout:float
    -> ?domains:int
    -> ?backlog:int
    -> ?tcp_nodelay:bool
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
      {!create_connection_handler}, which takes [config], [idle_timeout],
      [head_timeout], [request_handler] and [error_handler].
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

      [run] does not return. Cancel its task to stop all domains. If
      [on_error] raises, or a listen or another accept error occurs in one
      domain, [run] cancels all domains and raises the exception.

      @raise Invalid_argument
        if [domains] is less than 1 or more than the domains available, if
        [addr] is a Unix domain socket address, or for the reasons that
        {!create_connection_handler} gives. *)
end

module Client : Client with type socket = Miou_unix.file_descr
