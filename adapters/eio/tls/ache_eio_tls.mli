(** TLS for [ache-eio], with {{:https://github.com/mirleft/ocaml-tls}ocaml-tls}
    ([tls-eio]).

    A server:
    {[
      Eio.Net.run_server
        socket
        ~on_error:(Eio.traceln "connection error: %a" Fmt.exn)
        (Ache_eio_tls.Server.create_connection_handler
           ~tls:server_config
           ~request_handler
           ~error_handler)
    ]}

    A client, on a connected [flow]:
    {[
      Ache_eio_tls.Client.create_connection
        ~sw
        ~tls:client_config
        ~peer:(`Host host)
        flow
    ]}

    [tls-eio] needs a random number generator, for example
    [Mirage_crypto_rng_unix.use_default ()] at the start of the program.

    {!Ache_eio} also accepts a [Tls_eio.t] flow directly. A client on such a
    flow does not detect a truncated response (see {!Truncated}): [tls-eio]
    reports the end of TCP without a [close_notify] alert as the end of the
    input, so a response body that ends at the close of the connection looks
    complete. OpenSSL is not supported. *)

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

module Client : sig
  type t

  val create_connection :
     ?config:Ache.Config.t
    -> sw:Eio.Switch.t
    -> tls:Tls.Config.client
    -> peer:peer
    -> [> Eio.Flow.two_way_ty | Eio.Resource.close_ty ] Eio.Resource.t
    -> t
  (** [create_connection ~sw ~tls ~peer flow] does the client side of the TLS
      handshake on [flow] and runs {!Ache_eio.Client} on the TLS flow, which
      it drives with [Tls.Engine]. When the handshake fails, for example
      because the certificate is not valid for [peer], it raises the
      exceptions of [tls-eio] ([Tls_eio.Tls_failure
      (`Error (`AuthenticationFailure _))] for a rejected certificate,
      [Tls_eio.Tls_alert], or [End_of_file] when the server closes). The
      caller closes [flow].

      On an idle connection, {!request} first reads the TLS records that wait
      on a non-blocking [flow] ([eio_posix]) and decrypts them; see
      {!Ache_eio.Client.create_connection} for what closes the connection. *)

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
  (** [request], [shutdown], [is_closed] and [upgrade] are as in
      {!Ache_eio.Client}. *)
end

module Server : sig
  val create_connection_handler :
     ?config:Ache.Config.t
    -> ?clock:_ Eio.Time.clock
    -> ?idle_timeout:float option
    -> ?head_timeout:float option
    -> ?body_timeout:float option
    -> ?write_timeout:float option
    -> ?date:bool
    -> ?stop:unit Eio.Promise.t
    -> ?grace:float
    -> tls:Tls.Config.server
    -> request_handler:
         (Eio.Net.Sockaddr.stream -> Ache.Request_descriptor.t -> unit)
    -> error_handler:
         (Eio.Net.Sockaddr.stream -> Ache.Server_connection.error_handler)
    -> [> Eio.Flow.two_way_ty | Eio.Resource.close_ty ] Eio.Resource.t
    -> Eio.Net.Sockaddr.stream
    -> unit
  (** [create_connection_handler ~tls ~request_handler ~error_handler] can be
      passed to [Eio.Net.run_server]. In the connection fiber, it does the TLS
      handshake on the accepted flow and then serves HTTP/1.1 as
      {!Ache_eio.Server.create_connection_handler} does, with the same
      timeouts, [date], [stop] and [grace]. A handshake in progress when [stop]
      is resolved is treated as an idle connection: it is cancelled and the
      handler returns. When [stop] is already resolved, no handshake starts.
      The handshake must be complete within the smaller of [idle_timeout]
      and [head_timeout] (30 s by default). With both [None], the handshake
      has no time limit: a client that sends plain HTTP to the TLS port gets
      no answer (ocaml-tls waits for the rest of what it reads as a TLS
      record), and the connection stays open until the client closes it.
      [write_timeout] applies to the TLS records: a write that is cancelled
      leaves the TLS session unusable, and the connection closes without
      [close_notify].

      When the handshake fails, it raises the exception of [tls-eio]
      ([End_of_file], [Tls_eio.Tls_alert], [Tls_eio.Tls_failure], an
      [Eio.Io] error) or [Eio.Time.Timeout]; [run_server] gives it to
      [on_error] and serves the other connections. The caller closes the flow.

      @raise Invalid_argument
        in the cases of {!Ache_eio.Server.create_connection_handler}: when
        [~error_handler] is applied if all nine optional arguments are given
        too, and otherwise when the handler is applied to a flow, before the
        handshake. *)
end
