(** TLS for [ache-lwt], with {{:https://github.com/mirleft/ocaml-tls}ocaml-tls}
    ([tls-lwt]).

    A server:
    {[
      Lwt_io.establish_server_with_client_socket
        listen_address
        (Ache_lwt_tls.Server.create_connection_handler_with_fd
           ~tls:server_config
           ~request_handler
           ~error_handler)
    ]}

    A client, on a connected [fd]:
    {[
      Ache_lwt_tls.Client.create_connection_with_fd
        ~tls:client_config
        ~peer:(`Host host)
        fd
    ]}

    [tls-lwt] seeds the random number generator when it is linked. OpenSSL
    ([lwt_ssl]) is not supported. *)

type socket
(** A TLS session on a connected [Lwt_unix.file_descr]. *)

exception Truncated
(** A read fails with [Truncated] when the response in progress has no
    [Content-Length] and no chunked framing, so that its body ends at the close
    of the connection, and TCP ends, or is reset, without a TLS [close_notify]
    alert. The [error_handler] of that request gets [`Exn Truncated] and the
    body calls [on_error], not [on_eof]: an attacker who cuts the TCP
    connection cannot make a truncated body look complete (RFC 9110 §9.8).

    In every other case the end of TCP without [close_notify] is the end of the
    input, as without TLS: a complete body with [Content-Length] or chunked
    framing is not an error, an incomplete one is reported as cut, and a
    request that got no byte of its response gets [`Connection_closed]. A
    server never raises [Truncated]: a request body has an explicit length.

    Only {!Client.request} sees the framing of a response: a client built with
    [Ache_lwt.Make (Io)] does not detect truncation. *)

val server_of_fd : Tls.Config.server -> Lwt_unix.file_descr -> socket Lwt.t
(** [server_of_fd config fd] does the server side of the TLS handshake on [fd].
    When the handshake fails, it closes [fd] and fails with the exception of
    [tls-lwt] ([End_of_file], [Tls_lwt.Tls_alert], [Tls_lwt.Tls_failure], or a
    [Unix.Unix_error]). *)

type peer =
  [ `Host of [ `host ] Domain_name.t
  | `Ip of Ipaddr.t
  | `Insecure_no_name_check
  ]
(** The identity that the server certificate must prove. With [`Host name],
    the certificate must be valid for [name]; with [`Ip ip], it must have [ip]
    as an IP address subject alternative name. [`Insecure_no_name_check]
    accepts a certificate for any name or address, as long as the
    authenticator of the [Tls.Config.client] accepts it: any server with a
    certificate from a trusted CA can then impersonate the intended one. Use it
    only with an authenticator that checks the identity itself, for example by
    key fingerprint, or with a [Tls.Config.client] that has [~peer_name] or
    [~ip]. *)

val client_of_fd :
   Tls.Config.client
  -> peer:peer
  -> Lwt_unix.file_descr
  -> socket Lwt.t
(** [client_of_fd config ~peer fd] does the client side of the TLS handshake on
    [fd]. When the handshake fails, for example because the certificate is not
    valid for [peer], the promise fails with the exception of [tls-lwt]
    ([Tls_lwt.Tls_failure (`Error (`AuthenticationFailure _))] for a rejected
    certificate) and [fd] is closed. *)

val epoch : socket -> (Tls.Core.epoch_data, unit) result
(** The parameters of the TLS session, such as the peer certificate and the
    ALPN protocol. *)

(** [close] sends a TLS [close_notify] alert, unless a write is in progress or
    the alert cannot be written at once, and then closes the file descriptor.
*)
module Io : Ache_lwt.IO with type socket = socket and type addr = Unix.sockaddr

module Server : sig
  include
    Ache_lwt.Server with type socket = socket and type addr = Unix.sockaddr

  val create_connection_handler_with_fd :
     ?config:Ache.Config.t
    -> ?idle_timeout:float
    -> ?head_timeout:float
    -> ?date:bool
    -> ?stop:unit Lwt.t
    -> ?grace:float
    -> tls:Tls.Config.server
    -> request_handler:(Unix.sockaddr -> Ache.Request_descriptor.t -> unit)
    -> error_handler:(Unix.sockaddr -> Ache.Server_connection.error_handler)
    -> Unix.sockaddr
    -> Lwt_unix.file_descr
    -> unit Lwt.t
  (** [create_connection_handler_with_fd ~tls ~request_handler ~error_handler]
      can be passed to [Lwt_io.establish_server_with_client_socket]. It does the
      TLS handshake on the accepted file descriptor and then serves HTTP/1.1 as
      {!Ache_lwt.Server.create_connection_handler} does, with the same
      timeouts, [date], [stop] and [grace]. A handshake in progress when
      [stop] resolves is closed, as an idle connection. When [idle_timeout] or [head_timeout] is set, the handshake
      must be complete within the smaller of them; without either, it has no
      time limit.

      When the handshake fails, the promise resolves after the file descriptor
      is closed; the error is not reported. To see it, call {!server_of_fd} and
      then [create_connection_handler].

      @raise Invalid_argument
        when applied to a file descriptor, if a timeout or [grace] is not
        positive. *)
end

module Client : sig
  include Ache_lwt.Client with type socket = socket

  val create_connection_with_fd :
     ?config:Ache.Config.t
    -> tls:Tls.Config.client
    -> peer:peer
    -> Lwt_unix.file_descr
    -> t Lwt.t
  (** [create_connection_with_fd ~tls ~peer fd] is {!client_of_fd} followed by
      [create_connection]. *)
end
