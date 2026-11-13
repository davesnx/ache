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
(** A socket type that the IO loop can drive. [read socket buffer ~off ~len]
    reads into [buffer] in the region [\[off, off + len)] and fails with
    [End_of_file] when the peer has closed the connection. *)

module type Server = Ache_lwt_intf.Server

module type Client = Ache_lwt_intf.Client
(** [Client.upgrade t impl] shuts down the HTTP state of [t] and hands reading
    and writing on its socket to [impl]. *)

module Make (Io : IO) : sig
  module Server : Server with type socket = Io.socket and type addr = Io.addr
  module Client : Client with type socket = Io.socket
end

(** The function that results from [create_connection_handler] can be passed to
    [Lwt_io.establish_server_with_client_socket]. *)
module Server :
  Server with type socket = Lwt_unix.file_descr and type addr = Unix.sockaddr

module Client : Client with type socket = Lwt_unix.file_descr
