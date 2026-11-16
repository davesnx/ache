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

module Server : Server with type socket = Miou_unix.file_descr
module Client : Client with type socket = Miou_unix.file_descr
