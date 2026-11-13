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
    and writing on its socket to [impl]. *)

module Make (Io : IO) : sig
  module Server : Server with type socket = Io.socket and type addr = Io.addr
  module Client : Client with type socket = Io.socket
end

(** The function that results from [create_connection_handler] can be passed to
    [Lwt_io.establish_server_with_client_socket].

    Timeouts of [create_connection_handler] are in seconds and are off by
    default:
    - [idle_timeout]: when no request is in progress and no byte arrives for
      [idle_timeout], the connection closes without a response.
    - [head_timeout]: when a request head is not complete [head_timeout] after
      its first bytes were read, [error_handler] is called with
      [`Request_timeout] (408) and the connection closes after the error
      response.

    A request body or a response in progress is never timed out. Each request
    head, pipelined or not, has its own limit.

    Closing: when the connection stops reading it calls [shutdown_receive]
    and does not wait for the peer to close; it closes the socket once the
    response is written. Input that the peer sent and the connection did not
    read can make the kernel reset the connection. [Ache_eio] with a clock
    instead reads and discards input for a bounded time first.

    [create_connection_handler] raises [Invalid_argument] when applied to a
    socket if a timeout is not positive.

    The application must ignore [SIGPIPE], for example with
    [Sys.set_signal Sys.sigpipe Sys.Signal_ignore], before it serves
    connections. Otherwise a write to a peer that closed the connection kills
    the process. With [SIGPIPE] ignored, that write ends the connection and
    pending [Body.Writer.flush] callbacks get [`Closed]. ache does not install
    signal handlers. *)
module Server :
  Server with type socket = Lwt_unix.file_descr and type addr = Unix.sockaddr

module Client : Client with type socket = Lwt_unix.file_descr
