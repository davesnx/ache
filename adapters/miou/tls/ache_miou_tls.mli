(** TLS for [ache-miou], with {{:https://github.com/mirleft/ocaml-tls}ocaml-tls}.

    A server, in the task of an accepted [fd]:
    {[
      Ache_miou_tls.Server.create_connection_handler_with_fd
        ~tls:server_config
        ~request_handler
        ~error_handler
        fd
        client_address
    ]}

    A client, on a connected [fd]:
    {[
      Ache_miou_tls.Client.create_connection_with_fd
        ~tls:client_config
        ~peer:(`Host host)
        fd
    ]}

    The TLS session runs the protocol engine of ocaml-tls ([Tls.Engine]) on
    the file descriptor, and fails with the exceptions of [tls-miou-unix]. It
    does not use [Tls_miou_unix.t]: that session does not tell the end of TCP
    from a [close_notify] alert (see {!Truncated}), and it closes the file
    descriptor itself. Here the caller owns the file descriptor, as with
    {!Ache_miou}: it closes it after the server handler returns or after
    {!Client.shutdown}, and also when a handshake fails.

    ocaml-tls needs a random number generator, for example
    [Mirage_crypto_rng_unix.use_default ()] at the start of the program. The
    application must ignore [SIGPIPE], as with {!Ache_miou}. OpenSSL is not
    supported. *)

type socket
(** A TLS session on a connected [Miou_unix.file_descr]. *)

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
    server never raises [Truncated]: a request body has an explicit length. *)

val server_of_fd : Tls.Config.server -> Miou_unix.file_descr -> socket
(** [server_of_fd config fd] does the server side of the TLS handshake on [fd].
    When the handshake fails, it raises [End_of_file] (the peer closed),
    [Tls_miou_unix.Tls_alert], [Tls_miou_unix.Tls_failure], or a
    [Unix.Unix_error]. It has no time limit. *)

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
  -> Miou_unix.file_descr
  -> socket
(** [client_of_fd config ~peer fd] does the client side of the TLS handshake on
    [fd]. When the handshake fails, for example because the certificate is not
    valid for [peer], it raises the exceptions of {!server_of_fd}
    ([Tls_miou_unix.Tls_failure (`Error (`AuthenticationFailure _))] for a
    rejected certificate). *)

val epoch : socket -> (Tls.Core.epoch_data, unit) result
(** The parameters of the TLS session, such as the peer certificate and the
    ALPN protocol. *)

module Server : sig
  include Ache_miou.Server with type socket = socket

  val create_connection_handler_with_fd :
     ?config:Ache.Config.t
    -> ?idle_timeout:float
    -> ?head_timeout:float
    -> tls:Tls.Config.server
    -> request_handler:(Unix.sockaddr -> Ache.Reqd.t -> unit)
    -> error_handler:(Unix.sockaddr -> Ache.Server_connection.error_handler)
    -> Miou_unix.file_descr
    -> Unix.sockaddr
    -> unit
  (** [create_connection_handler_with_fd ~tls ~request_handler ~error_handler
       fd addr] does the TLS handshake on the accepted [fd] and then serves
      HTTP/1.1 as {!create_connection_handler} does, with the same timeouts.
      When [idle_timeout] or [head_timeout] is set, the handshake must be
      complete within the smaller of them; without either, it has no time
      limit.

      When the handshake fails or takes too long, the handler returns; the
      error is not reported. To see it, call {!server_of_fd} and then
      [create_connection_handler]. In every case the caller closes [fd] after
      the handler returns.

      @raise Invalid_argument
        in the cases of {!create_connection_handler}, before the handshake. *)
end

module Client : sig
  include Ache_miou.Client with type socket = socket

  val create_connection_with_fd :
     ?config:Ache.Config.t
    -> tls:Tls.Config.client
    -> peer:peer
    -> Miou_unix.file_descr
    -> t
  (** [create_connection_with_fd ~tls ~peer fd] is {!client_of_fd} followed by
      [create_connection]. *)
end
