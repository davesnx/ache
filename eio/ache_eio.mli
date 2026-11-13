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
    -> request_handler:(Eio.Net.Sockaddr.stream -> Ache.Reqd.t -> unit)
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
      [idle_timeout], after its last write. Without [clock] it does not wait:
      it shuts down the flow at once, and the handler returns.

      A [shutdown] of [flow] that raises is ignored, so a flow without
      half-close (for example some TLS flows) ends the connection normally.

      @raise Invalid_argument
        when applied to a flow, if a timeout is given without [clock] or a
        timeout is not positive. *)

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
