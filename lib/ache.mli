(*----------------------------------------------------------------------------
  Copyright (c) 2017 Inhabited Type LLC. Copyright (c) 2019 Antonio Nuno
  Monteiro.

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

(** ache is a high-performance, memory-efficient, and scalable HTTP/1.x
    library for OCaml. It implements the HTTP 1.1 specification with respect to
    parsing, serialization, and pipelining. *)

(** {2 Basic HTTP Types} *)

module Bigstring : module type of Bigstring
module IOVec : module type of IOVec
module Version : module type of Version
module Method : module type of Method
module Status : module type of Status
module Headers : module type of Headers

(** {2 Runtime Interface} *)

module Runtime : sig
  (** The interface that a runtime (such as [ache-eio], [ache-lwt] or
      [ache-miou]) drives. {!Server_connection} and {!Client_connection}
      implement it. A protocol that takes over a connection after an HTTP
      upgrade implements it too. *)
  module type ENGINE = sig
    type t

    val next_read_operation : t -> [ `Read | `Yield | `Close ]
    val read : t -> Bigstring.t -> off:int -> len:int -> int
    val read_eof : t -> Bigstring.t -> off:int -> len:int -> int
    val yield_reader : t -> (unit -> unit) -> unit

    val next_write_operation :
       t
      -> [ `Write of Bigstring.t IOVec.t list | `Yield | `Close of int ]

    val report_write_result : t -> [ `Ok of int | `Closed ] -> unit
    val yield_writer : t -> (unit -> unit) -> unit
    val report_exn : t -> exn -> unit
    val is_closed : t -> bool
    val shutdown : t -> unit
  end

  type impl
  (** A value of some type together with its {!ENGINE} implementation. *)

  val make : (module ENGINE with type t = 't) -> 't -> impl
end

(** {2 Message Body} *)

module Body : sig
  module Reader : sig
    type t

    val schedule_read :
       t
      -> on_eof:(unit -> unit)
      -> on_error:(unit -> unit)
      -> on_read:(Bigstring.t -> off:int -> len:int -> unit)
      -> unit
    (** [schedule_read t ~on_eof ~on_error ~on_read] schedules one read of
        [t]. Exactly one of the three callbacks is called, once:
        - [on_read] when bytes are available. It receives every byte
          available at that time; schedule the next read to get more.
        - [on_eof] when the body is complete and every byte has been given to
          [on_read]. A close-delimited response body is complete at end of
          input.
        - [on_error] when the body ends before its framing ends: end of input
          or a shutdown of the connection in the middle of the body, an
          invalid framing, or an exception reported for the request. Such a
          body is incomplete (RFC 9112 §8). The cause goes to the connection's
          error handler, and to {!Request_descriptor.error_code} on a server.

        Exactly one of [on_eof] and [on_error] ends a body. After it, no
        callback of [t] is called again, also for later [schedule_read]
        calls. Bytes received before the end are given to [on_read] first.

        [on_read] receives every byte available at the time of the call. The
        library does not reuse the bytes it passes to [on_read], so the
        application may keep them after [on_read] returns. *)

    val close : t -> unit
    (** [close t] closes [t]: input received after it is discarded. Bytes
        already received are still given to [on_read], then [on_eof] is
        called; a later failure of the connection does not call [on_error]. *)

    val is_closed : t -> bool
    (** [is_closed t] is [true] if {!close} has been called on [t] and [false]
        otherwise. A closed [t] may still have bytes available for reading. *)
  end

  module Writer : sig
    type t

    val write_char : t -> char -> unit
    (** [write_char w char] copies [char] into an internal buffer. If possible,
        this write will be combined with previous and/or subsequent writes
        before transmission. *)

    val write_string : t -> ?off:int -> ?len:int -> string -> unit
    (** [write_string w ?off ?len str] copies [str] into an internal buffer. If
        possible, this write will be combined with previous and/or subsequent
        writes before transmission. *)

    val write_bigstring : t -> ?off:int -> ?len:int -> Bigstring.t -> unit
    (** [write_bigstring w ?off ?len bs] copies [bs] into an internal buffer. If
        possible, this write will be combined with previous and/or subsequent
        writes before transmission. *)

    val schedule_bigstring : t -> ?off:int -> ?len:int -> Bigstring.t -> unit
    (** [schedule_bigstring w ?off ?len bs] schedules [bs] to be transmitted at
        the next opportunity without performing a copy. [bs] should not be
        modified until a subsequent call to {!flush} has successfully completed.
    *)

    val flush : t -> ([ `Written | `Closed ] -> unit) -> unit
    (** [flush t f] makes all bytes in [t] available for writing to the awaiting
        output channel. Once those bytes have reached that output channel,
        [f `Written] will be called. If instead, the output channel is closed
        before all of those bytes are successfully written, [f `Closed] will be
        called.

        The type of the output channel is runtime-dependent, as are guarantees
        about whether those packets have been queued for delivery or have
        actually been received by the intended recipient. *)

    val close : t -> unit
    (** [close t] closes [t]. Later writes to [t] are ignored. If [t] is
        writable, this will cause any pending output to become available to the
        output channel. *)

    val is_closed : t -> bool
    (** [is_closed t] is [true] if {!close} has been called on [t], or if the
        attached output channel is closed (e.g. because
        [report_write_result `Closed] has been called). A closed [t] may still
        have pending output. *)
  end
end

(** {2 Message Types} *)

(** Request

    A client-initiated HTTP message. *)
module Request : sig
  type t =
    { meth : Method.t
    ; target : string
    ; version : Version.t
    ; headers : Headers.t
    }

  val create :
     ?version:Version.t (** default is HTTP 1.1 *)
    -> ?headers:Headers.t (** default is {!Headers.empty} *)
    -> Method.t
    -> string
    -> t

  module Body_length : sig
    type t =
      [ `Fixed of Int64.t
      | `Chunked
      | `Error of [ `Bad_request ]
      ]

    val pp_hum : Format.formatter -> t -> unit
  end

  val body_length : t -> Body_length.t
  (** [body_length t] is the length of the message body accompanying [t]. It is
      an error to generate a request with a close-delimited message body.
      [`Error `Bad_request] when [t] has both Content-Length and
      Transfer-Encoding, when chunked is not the final transfer coding, or when
      the Content-Length field lines do not all hold the same [1*DIGIT] value
      (RFC 9110 §8.6). {!Client_connection.request} raises [Failure] for such a
      request.

      See {{:https://tools.ietf.org/html/rfc7230#section-3.3.3} RFC7230§3.3.3}
      for more details. *)

  val persistent_connection : ?proxy:bool -> t -> bool
  (** [persistent_connection ?proxy t] indicates whether the connection for [t]
      can be reused for multiple requests and responses. If the calling code
      is acting as a proxy, it should pass [~proxy:true]. Connection options
      are read case-insensitively from every member of every Connection field
      line.

      See {{:https://tools.ietf.org/html/rfc7230#section-6.3} RFC7230§6.3} for
      more details. *)

  val pp_hum : Format.formatter -> t -> unit [@@ocaml.toplevel_printer]
end

(** Response

    A server-generated message to a {!Request}. *)
module Response : sig
  type t =
    { version : Version.t
    ; status : Status.t
    ; reason : string
    ; headers : Headers.t
    }

  val create :
     ?reason:string
       (** default is determined by {!Status.default_reason_phrase} *)
    -> ?version:Version.t (** default is HTTP 1.1 *)
    -> ?headers:Headers.t (** default is {!Headers.empty} *)
    -> Status.t
    -> t
  (** [create ?reason ?version ?headers status] creates an HTTP response with
      the given parameters. For typical use cases, it's sufficient to provide
      values for [headers] and [status]. *)

  module Body_length : sig
    type t =
      [ `No_body
      | `Fixed of Int64.t
      | `Chunked
      | `Close_delimited
      | `Error of [ `Bad_gateway | `Internal_server_error ]
      ]

    val pp_hum : Format.formatter -> t -> unit
  end

  val body_length :
     ?proxy:bool
    -> request_method:Method.standard
    -> t
    -> Body_length.t
  (** [body_length ?proxy ~request_method t] is the length of the message body
      accompanying [t] assuming it is a response to a request whose method was
      [request_method]. If the calling code is acting as a proxy, it should pass
      [~proxy:true]. This optional parameter only affects error reporting.

      [`No_body] is a response to [HEAD] or with status 1xx, 204 or 304: the
      message ends after the header section, whatever its [Content-Length] or
      [Transfer-Encoding]. A 2xx response to [CONNECT] is [`Close_delimited]:
      the connection becomes a tunnel.

      See {{:https://www.rfc-editor.org/rfc/rfc9112#section-6.3} RFC 9112§6.3}
      for more details. *)

  val persistent_connection : ?proxy:bool -> t -> bool
  (** [persistent_connection ?proxy t] indicates whether the connection for [t]
      can be reused for multiple requests and responses. If the calling code
      is acting as a proxy, it should pass [~proxy:true]. Connection options
      are read case-insensitively from every member of every Connection field
      line.

      See {{:https://tools.ietf.org/html/rfc7230#section-6.3} RFC7230§6.3} for
      more details. *)

  val pp_hum : Format.formatter -> t -> unit [@@ocaml.toplevel_printer]
end

(** {2 Request Descriptor} *)
module Request_descriptor : sig
  type t

  type error =
    [ `Bad_request
    | `Not_implemented
    | `Http_version_not_supported
    | `Uri_too_long
    | `Bad_gateway
    | `Internal_server_error
    | `Request_header_fields_too_large
    | `Request_timeout
    | `Exn of exn
    ]

  val request : t -> Request.t
  val request_body : t -> Body.Reader.t
  val response : t -> Response.t option
  val response_exn : t -> Response.t

  (** {3 Responding}

      The [respond_with_*] functions start the response to the request in [t].
      ache owns the framing of the response: the [Content-Length] and
      [Transfer-Encoding] fields that the application sets are checked, and ache
      adds the fields it needs.

      No body is sent in a response to [HEAD] or with status 1xx, 204 or 304
      (RFC 9112 §6.3). A [HEAD] or 304 response can carry the [Content-Length]
      of the matching [GET] response. To start a tunnel for a [CONNECT]
      request, use {!respond_with_upgrade}. The body of a 2xx response to
      [CONNECT] given to another [respond_with_*] function is sent as it is,
      then the connection closes, and the bytes after the [CONNECT] request
      are not read.

      ache adds:
      - [Content-Length] to a string or bigstring response that has neither
        field;
      - [Transfer-Encoding: chunked] to a streaming response that has neither
        field, for an HTTP/1.1 request on a persistent connection. Otherwise
        the body ends when the connection closes;
      - [Connection: close] to a final response after which the connection
        closes (the request has [Connection: close], an HTTP/1.0 request has
        no [keep-alive], the body ends with the connection, or the response is
        an error response), unless the response's [Connection] field already
        has the [close] option (RFC 9112 §9.6). A 2xx response to [CONNECT]
        does not get it;
      - [Connection: keep-alive] to a response to an HTTP/1.0 request when the
        connection stays open and the response has no [Connection] field.

      A streaming response with [Transfer-Encoding] whose final coding is
      [chunked] (for example [gzip, chunked]) is chunked by ache; with another
      final coding, the body ends when the connection closes. To an HTTP/1.0
      request, the [Transfer-Encoding] field is removed and the body ends when
      the connection closes (RFC 9112 §6.1).

      The body writer of a streaming response with [Content-Length] counts
      bytes: a write past the declared length raises [Invalid_argument] and
      writes nothing. A body closed short of the declared length is sent as
      it is, and the connection closes after it; the client sees an
      incomplete message (RFC 9112 §8). The error handler is not called: the
      response has already started.

      These raise [Invalid_argument], before anything is written, so that the
      application can still respond:
      - a 1xx status (use {!respond_with_informational} or
        {!respond_with_upgrade});
      - both [Content-Length] and [Transfer-Encoding] (RFC 9112 §6.2);
      - [Content-Length] or [Transfer-Encoding] in a 1xx, a 204 or a 2xx
        response to [CONNECT] (RFC 9110 §8.6);
      - [Transfer-Encoding] in a string or bigstring response with a body;
      - a [Content-Length] that differs from the length of the string or
        bigstring body;
      - a [Content-Length] that is not one repeated [1*DIGIT] value
        (RFC 9110 §8.6);
      - an invalid head, naming the offending field: a field name that is
        not a token (RFC 9110 §5.1), a field value with CR, LF or NUL (§5.5),
        or a reason phrase with CR, LF or NUL.

      When the connection can no longer write, for example because the peer
      closed it, the [respond_with_*] functions do not raise: the response is
      recorded and its bytes are dropped. The body writer of a streaming
      response is then closed, and {!Body.Writer.flush} answers [`Closed].
      When the connection closes during a streaming response, or
      {!report_exn} is called for it, the bytes of its body that are not
      written yet are dropped and every pending flush gets [`Closed]. *)

  val respond_with_string : t -> Response.t -> string -> unit
  val respond_with_bigstring : t -> Response.t -> Bigstring.t -> unit

  val respond_with_streaming :
     ?flush_headers_immediately:bool
    -> t
    -> Response.t
    -> Body.Writer.t
  (** [respond_with_streaming t response] sends [response] and returns the
      writer for its body. In a response without a body, the writer drops the
      bytes written to it and answers [flush] with [`Written]. *)

  val respond_with_informational : t -> Response.t -> unit
  (** [respond_with_informational t response] sends the interim [response],
      for example [100 Continue] to a request with [Expect: 100-continue]
      (RFC 9110 §10.1.1), or [103 Early Hints]. Only the head is sent. A final
      response must follow with one of the other [respond_with_*] functions.
      It can be called more than once. ache never sends [100 Continue] on its
      own: the request handler decides.

      Nothing is sent to an HTTP/1.0 request (RFC 9110 §15.2).

      @raise Invalid_argument
        if the status is not 1xx, is 101, or the response has
        [Content-Length] or [Transfer-Encoding]. *)

  val respond_with_upgrade :
     ?status:Status.successful
    -> t
    -> Headers.t
    -> Runtime.impl
    -> unit
  (** [respond_with_upgrade ?status t headers impl] responds with
      [101 Switching Protocols] and [headers]. To a [CONNECT] request it
      responds with [status] instead (default [`OK]), which starts a tunnel
      (RFC 9110 §9.3.6). When the response has been written, the connection
      shuts down its HTTP state and hands reading and writing to [impl].
      Input that the connection has not consumed goes to [impl]. When the
      response cannot be written, [impl] is shut down and never reads or
      writes.

      @raise Invalid_argument
        if [status] is given for a request that is not [CONNECT], or, for a
        [CONNECT] request, if [headers] has [Content-Length] or
        [Transfer-Encoding] or an invalid field. *)

  (** {3 Exception Handling} *)

  val error_code : t -> error option
  val report_exn : t -> exn -> unit
  val try_with : t -> (unit -> unit) -> (unit, exn) result
end

(** {2 Buffer Size Configuration} *)
module Config : sig
  type t =
    { read_buffer_size : int  (** Default is [4096] *)
    ; request_body_buffer_size : int  (** Default is [4096] *)
    ; response_buffer_size : int  (** Default is [1024] *)
    ; response_body_buffer_size : int  (** Default is [4096] *)
    ; max_header_size : int
      (** Largest request or response head, chunk-size line or trailer
              section, in bytes. Default is [16384]. A larger request head gets
              a [501], [414] or [431] response, see
              {!Server_connection.create}. *)
    ; proxy : bool
    ; max_pending_output : int
      (** Server: while more response bytes than this wait to be written, the
          connection reads and dispatches no new request. A client that
          pipelines requests and does not read the responses then stops
          being read. Default is [65536]. *)
    }

  val default : t
  (** [default] is a configuration record with all parameters set to their
      default values. *)
end

(** {2 Server Connection} *)

module Server_connection : sig
  type t
  type error = Request_descriptor.error
  type request_handler = Request_descriptor.t -> unit

  type error_handler =
    ?request:Request.t -> error -> (Headers.t -> Body.Writer.t) -> unit

  val create :
     ?config:Config.t
    -> ?error_handler:error_handler
    -> ?date:(unit -> string)
    -> request_handler
    -> t
  (** [create ?config ?error_handler ?date request_handler] creates a
      connection handler that will service individual requests with
      [request_handler].

      With [date], every response that is not 1xx and has no [Date] field
      (in any case) gets [Date: (date ())] after its other fields (RFC 9110
      §6.6.1: an origin server with a clock sends [Date]). [date] returns an
      IMF-fixdate such as [Sun, 06 Nov 1994 08:49:37 GMT]; the runtimes pass
      a function that formats the time once per second. Without [date] the
      connection adds no [Date] field.

      The connection rejects a request before [request_handler] runs, calls
      [error_handler] and closes after the error response:
      - [`Bad_request] (400): the request line or a field line does not match
        RFC 9112 §3 and §5 (lines end with CRLF; a bare LF is an error;
        obs-fold is an error); an HTTP/1.1 request has no Host field; a request
        has more than one Host field or a Host value that is not
        [uri-host [":" port]] (RFC 3986 §3.2); a CONNECT target is not
        [host:port] with a port of 0 to 65535; the target is [*] and the
        method is not OPTIONS; the method is not CONNECT and the target is
        not [*] for OPTIONS, nor origin-form ([/...]), nor absolute-form
        ([scheme:...]) (an authority such as [example.com:80] reads as
        absolute-form with scheme [example.com] and is accepted with any
        method; one such as [127.0.0.1:80] or [[::1]:80] is not); Content-Length is not a list of one
        repeated [1*DIGIT] value; the request has both Content-Length and
        Transfer-Encoding; an HTTP/1.0 request has Transfer-Encoding; chunked
        is not the final transfer coding or is applied twice.
      - [`Not_implemented] (501): a transfer coding other than chunked, or a
        method that reaches [Config.max_header_size] before its end.
      - [`Uri_too_long] (414): a request-target that reaches
        [Config.max_header_size] before the end of the request line.
      - [`Request_header_fields_too_large] (431): a head that reaches
        [Config.max_header_size] after the request line.
      - [`Http_version_not_supported] (505): a major version other than 1.
      - [`Request_timeout] (408): a head that is not complete when the
        runtime's head timeout expires (see {!read_timeout}).

      A request with version HTTP/1.x, x > 1, is processed as HTTP/1.1. Its
      [version] field keeps the received minor version. Request-target bytes
      0x80 to 0xFF are accepted, because clients send them unencoded.

      A request that is rejected after earlier pipelined requests gets its
      error response after the responses to those requests. A request that
      follows a request with [Connection: close] is not read.

      The error response follows the framing rules of {!Request_descriptor}: it has no body
      in a response to [HEAD], and a body without [Content-Length] ends when
      the connection closes. When [error_handler] raises, for example because
      the head it gives is invalid, the connection drops its pending output
      and closes. Its response function raises [Failure] when called a second
      time.

      After a request that may switch protocols ([CONNECT], or an [Upgrade]
      field with the [upgrade] connection option), the connection reads
      nothing past the end of that request until its response is decided
      (RFC 9110 §7.8, §9.3.6). After {!Request_descriptor.respond_with_upgrade}
      (a 101, or a 2xx to [CONNECT]), those bytes go to the new protocol.
      After a 2xx response to [CONNECT] sent another way, they are not read,
      and the connection closes when the response is written. After any
      other response, they are read as the next request. When the input ends
      before the upgrade response is written, the new protocol gets end of
      input after the switch.

      Once the write side has closed (a failed write or a closing
      response), no new request is read or dispatched: the request in
      progress still receives its body, within the limit that
      {!read_timeout} gives, then the reader closes.

      While more than [Config.max_pending_output] bytes of responses are not
      written, the connection reads no new request, and does not call
      [request_handler] for a pipelined request that it has read. When
      [request_handler] raises for a pipelined request, that request gets
      the error response, as if {!Request_descriptor.report_exn} was called.

      When the response is complete and no read is scheduled on the request
      body, the connection reads the rest of the body to find the next
      request and drops its bytes, also the bytes that the body holds. *)

  val next_read_operation : t -> [ `Read | `Yield | `Close ]
  (** [next_read_operation t] returns a value describing the next operation that
      the caller should conduct on behalf of the connection. *)

  val read : t -> Bigstring.t -> off:int -> len:int -> int
  (** [read t bigstring ~off ~len] reads bytes of input from the provided range
      of [bigstring] and returns the number of bytes consumed by the connection.
      {!read} should be called after {!next_read_operation} returns a [`Read]
      value and additional input is available for the connection to consume. *)

  val read_eof : t -> Bigstring.t -> off:int -> len:int -> int
  (** [read_eof t bigstring ~off ~len] reads bytes of input from the provided
      range of [bigstring] and returns the number of bytes consumed by the
      connection. {!read_eof} should be called after {!next_read_operation}
      returns a [`Read] and an EOF has been received from the communication
      channel. The connection will attempt to consume any buffered input and
      then shutdown the HTTP parser for the connection. *)

  val yield_reader : t -> (unit -> unit) -> unit
  (** [yield_reader t continue] registers with the connection to call
      [continue] when reading should resume. {!yield_reader} should be called
      after {!next_read_operation} returns a [`Yield] value. *)

  val next_write_operation :
     t
    -> [ `Write of Bigstring.t IOVec.t list | `Yield | `Close of int ]
  (** [next_write_operation t] returns a value describing the next operation
      that the caller should conduct on behalf of the connection. *)

  val report_write_result : t -> [ `Ok of int | `Closed ] -> unit
  (** [report_write_result t result] reports the result of the latest write
      attempt to the connection. {!report_write_result} should be called after a
      call to {!next_write_operation} that returns a [`Write buffer] value.

        {ul
        {- [`Ok n] indicates that the caller successfully wrote [n] bytes of
        output from the buffer that the caller was provided by
        {!next_write_operation}. }
        {- [`Closed] indicates that the output destination will no longer
        accept bytes from the write processor. }} *)

  val yield_writer : t -> (unit -> unit) -> unit
  (** [yield_writer t continue] registers with the connection to call
      [continue] when writing should resume. {!yield_writer} should be called
      after {!next_write_operation} returns a [`Yield] value. *)

  val report_exn : t -> exn -> unit
  (** [report_exn t exn] reports that an error [exn] has been caught and that it
      has been attributed to [t]. Calling this function will switch [t] into an
      error state. Depending on the state [t] is transitioning from, it may call
      its error handler before terminating the connection. *)

  val is_closed : t -> bool
  (** [is_closed t] is [true] if both the read and write processors have been
      shutdown. When this is the case {!next_read_operation} will return
      [`Close] and {!next_write_operation} will return [`Write _] until all
      buffered output has been flushed. *)

  val error_code : t -> error option
  (** [error_code t] returns the [error_code] that caused the connection to
      close, if one exists. *)

  (**/**)

  val shutdown : t -> unit

  (**/**)

  val read_timeout : t -> ([ `Idle | `Head | `Body ] * (unit -> unit)) option
  (** [read_timeout t] tells a runtime which read timeout applies while it
      waits for input on [t], and the action to run when that timeout expires.
      - [Some (`Idle, close)]: no request is in progress. [close] closes the
        connection and writes nothing.
      - [Some (`Head, reject)]: a request head is partly read. [reject] calls
        [error_handler] with [`Request_timeout] (408) and closes the connection
        after the error response. The request handler is not called.
      - [Some (`Body, close)]: the response is complete, and the rest of the
        request body has not arrived. The application reads it, or the
        connection drops it to find the next request. A runtime waits for it
        at most its idle timeout; a runtime without an idle timeout calls
        [close] instead of waiting. [close] calls the body's [on_error] and
        closes the connection after its pending output.
      - [None]: a request is in progress and its response is not complete,
        or the connection is closing or upgraded. No timeout applies. The
        answer can change to [Some (`Body, _)] while the runtime waits for
        input, when the response completes. A runtime asks again each time
        its write loop waits or ends. *)
end

(** {2 WebSocket Server} *)

(** A WebSocket server connection (RFC 6455), for text and binary messages,
    that runs
    after a [101] response sent with {!Request_descriptor.respond_with_upgrade}. It needs no
    runtime code: every runtime drives it like {!Server_connection}.

    It has no extensions and no subprotocols. It does not check [Origin] or
    authentication: the request handler does that before
    {!Websocket.upgrade}.

    What the connection does on its own:
    - it answers a ping with a pong that has the same payload;
    - it reassembles a fragmented message and gives it whole to
      [on_message];
    - when the client sends a close frame, it sends a close frame with the
      same code (without the reason), then closes the connection;
    - it fails the connection, that is, it sends a close frame with a code
      and closes the connection, with code [1002] for a protocol error (an
      unmasked frame, an unknown opcode, a reserved bit set, a control frame
      that is fragmented or longer than 125 bytes, a continuation frame
      without a message to continue, a new data frame inside a fragmented
      message, a payload length with the high bit set or not in its shortest
      form, a close frame with a 1-byte payload or an invalid code), [1007]
      for a text message or close reason that is not valid UTF-8, and [1009]
      for a message longer than [max_message_size] (before it reads the
      payload) (RFC 6455 §7.4.1). Text is checked as it arrives: the first
      byte that cannot start or continue valid UTF-8 fails the connection,
      without reading the rest of the message.

    After it sends a close frame, the connection reads no more messages and
    closes the TCP connection once the frame is written. It does not wait for
    the client's close frame.

    ache has no timeout on an upgraded connection. A client that stops
    reading, or never answers a close frame, keeps the connection open until
    the caller closes the socket, for example with its own timer.

    Reading stops while more than 64 KiB of frames (messages, pongs and close
    frames) are queued and not yet written, and starts again when the writes
    bring it down to 64 KiB. A client that sends pings and does not read the
    pongs therefore cannot make the queue grow without limit: no pong is
    dropped. One read can add the pongs for the frames it holds, so the queue
    can pass 64 KiB by at most that. *)
module Websocket : sig
  type t

  type handshake_error =
    [ `Not_get  (** Not a [GET] request of HTTP/1.1 or later. *)
    | `Not_websocket
      (** No [websocket] token in [Upgrade], or no [upgrade] token in
          [Connection]. Tokens are compared without case. *)
    | `Unsupported_version
      (** [Sec-WebSocket-Version] is not one field with value [13]. Respond
          with [426 Upgrade Required] and [Sec-WebSocket-Version: 13]
          (RFC 6455 §4.4). *)
    | `Invalid_key
      (** [Sec-WebSocket-Key] is not one field with the canonical base64
          encoding of 16 bytes (RFC 4648 §3.5: the padding bits are zero). *)
    ]

  val accept_key : string -> string
  (** [accept_key key] is the [Sec-WebSocket-Accept] value for the
      [Sec-WebSocket-Key] value [key]: the base64 encoding of the SHA-1 of
      [key] and the GUID of RFC 6455 §1.3. *)

  val check_handshake : Request.t -> (string, handshake_error) result
  (** [check_handshake request] checks that [request] is a WebSocket opening
      handshake (RFC 6455 §4.2.1) and returns the [Sec-WebSocket-Accept]
      value. *)

  type message =
    | Text of string  (** A text message: valid UTF-8. *)
    | Binary of string  (** A binary message: any bytes. *)

  type close =
    { code : int
    ; reason : string  (** Valid UTF-8, at most 123 bytes. *)
    }
  (** The client's close frame. *)

  type handler =
    { on_message : message -> unit
      (** Called with each whole message, in order. *)
    ; on_pong : string -> unit
      (** Called with the payload of each pong from the client, for example
          the answer to {!ping}. *)
    ; on_close : close option -> unit
      (** Called once, when the connection is closed and no more messages
          come. The argument is the client's close frame: its code and
          reason, [{ code = 1005; reason = "" }] for a close frame without a
          code, or [None] when the client sent no valid close frame (end of
          input, a reset, a protocol error, or a close that the server
          started). *)
    }

  val default_max_message_size : int
  (** [1048576] bytes (1 MiB). *)

  val upgrade :
     ?max_message_size:int
    -> Request_descriptor.t
    -> (t -> handler)
    -> (unit, handshake_error) result
  (** [upgrade ?max_message_size reqd make_handler] checks the request of
      [reqd] with {!check_handshake}. When it is valid, [upgrade] calls
      [make_handler] with the new connection, then responds with [101] and
      the headers [Upgrade: websocket], [Connection: Upgrade] and
      [Sec-WebSocket-Accept], and returns [Ok ()]. The connection starts to
      read and write when the [101] is written. Messages sent before that are
      written after the [101].

      When the request is not valid, [upgrade] responds with nothing and
      returns the error: the caller responds, for example with [400].

      [max_message_size] limits the length of a message, all fragments
      together. Default {!default_max_message_size}.

      @raise Invalid_argument if [max_message_size] is not positive. *)

  val send_text : t -> string -> unit
  (** [send_text t message] queues [message] as one text frame without a
      mask. After the connection started to close, it does nothing. [message]
      must be valid UTF-8: [send_text] does not check it. *)

  val send_binary : t -> string -> unit
  (** [send_binary t message] queues [message] as one binary frame without a
      mask. After the connection started to close, it does nothing. *)

  val ping : t -> string -> unit
  (** [ping t payload] queues a ping frame. The client answers with a pong,
      reported to [on_pong]. A server that wants to find a client that is
      gone can ping it and close the connection when no pong comes in time.
      After the connection started to close, it does nothing.

      @raise Invalid_argument if [payload] is longer than 125 bytes. *)

  val flush : t -> ([ `Written | `Closed ] -> unit) -> unit
  (** [flush t k] calls [k `Written] when all frames queued before it are
      written, and [k `Closed] when the connection closed first. After the
      connection started to close, it calls [k `Closed] at once. *)

  val close : ?reason:string -> t -> code:int -> unit
  (** [close ?reason t ~code] sends a close frame with [code] and [reason]
      (default [""]), stops reading messages, and closes the connection when
      the frame is written. It does nothing when the connection already
      started to close.

      @raise Invalid_argument
        if [code] is not one that an endpoint may send: [1000] to [1003],
        [1007] to [1014], or [3000] to [4999]; or if [reason] is not valid
        UTF-8 or longer than 123 bytes. *)

  val pause_reading : t -> unit
  (** [pause_reading t] stops reading from the client, so that no
      [on_message] call comes until {!resume_reading}. Frames that the current
      read already holds are still processed, so a few more messages can come
      after the call. While reading is paused, a ping gets no pong and the end
      of input is not seen. *)

  val resume_reading : t -> unit
  (** [resume_reading t] starts reading again after {!pause_reading}. Reading
      can still wait for the writes, as above. *)

  val is_closed : t -> bool
  (** [is_closed t] is [true] once the connection is closed: [on_close] was
      called and nothing more is written. *)
end

(** {2 Client Connection} *)

module Client_connection : sig
  type t

  type error =
    [ `Malformed_response of string
    | `Invalid_response_body_length of Response.t
    | `Connection_closed
    | `Exn of exn
    ]
  (** The error given to a request's [error_handler]. Each request gets at most
      one error, and no error after its response is complete.

      - [`Malformed_response]: the response to this request is not valid
        HTTP/1.1, or it ended early: the connection closed inside its head, or
        before the end of a body framed by [Content-Length] or chunked coding
        (RFC 9112 §8). A body without framing ends normally when the connection
        closes.
      - [`Invalid_response_body_length]: the response has an invalid
        [Content-Length].
      - [`Connection_closed]: the connection stopped reading responses before
        any byte of the response to this request arrived: end of input, a
        previous response that closes the connection, {!shutdown}, or an error
        on an earlier response. RFC 9112 §9.3.1 lets the application send the
        request again on a new connection.
      - [`Exn]: an exception from the application or reported with
        {!report_exn}. *)

  type response_handler = Response.t -> Body.Reader.t -> unit
  type error_handler = error -> unit

  val create : ?config:Config.t -> unit -> t

  val request :
     t
    -> ?flush_headers_immediately:bool
    -> Request.t
    -> error_handler:error_handler
    -> response_handler:response_handler
    -> Body.Writer.t
  (** [request t req ~error_handler ~response_handler] sends [req] on [t] and
      returns the writer for its body. [response_handler] is called once, with
      the final response.

      Interim 1xx responses other than [101 Switching Protocols] (for example
      [100 Continue] or [103 Early Hints]) are parsed and dropped; they are
      not given to [response_handler] (RFC 9110 §15.2). A [101] is final: it is
      given to [response_handler]. After a [101] response, and after a 2xx
      response to [CONNECT], the connection stops reading HTTP: the bytes that
      follow the head are left unconsumed for the protocol that takes over
      (the body given with a 2xx response to [CONNECT] receives none of them).
      An obs-fold in a response field value is replaced with one space
      (RFC 9112 §5.2).

      A request is written right away, or after the requests before it: it is
      pipelined behind a request with an idempotent method (RFC 9110 §9.2.2)
      whose request has been sent, and it waits behind any other request, and
      behind a request that may switch protocols ([CONNECT], or [Upgrade] with
      the [upgrade] connection option), until
      that request's response is complete (RFC 9112 §9.3.2). When a response
      ends the connection ([Connection: close], an HTTP/1.0 response without
      keep-alive or with [Transfer-Encoding], or both [Content-Length] and
      [Transfer-Encoding]), the connection stops writing, also in the middle of
      a request body, and the later requests get [`Connection_closed]. A
      request made after the connection stopped writing gets
      [`Connection_closed] once: from inside [request], before it returns,
      when no earlier request is pending; otherwise once the earlier
      responses are complete.

      Response bytes that arrive when no request waits for a response close the
      connection. Empty lines before a response are discarded.

      Raises [Invalid_argument], naming the offending field, when the head of
      [req] is invalid, before the request is queued: a method or field name that is not a token (RFC 9110
      §9.1, §5.1), a field value with CR, LF or NUL (§5.5), or a request target
      with CR, LF, NUL or space. *)

  val next_read_operation : t -> [ `Read | `Yield | `Close ]
  (** [next_read_operation t] returns a value describing the next operation that
      the caller should conduct on behalf of the connection. *)

  val read : t -> Bigstring.t -> off:int -> len:int -> int
  (** [read t bigstring ~off ~len] reads bytes of input from the provided range
      of [bigstring] and returns the number of bytes consumed by the connection.
      {!read} should be called after {!next_read_operation} returns a [`Read]
      value and additional input is available for the connection to consume. *)

  val read_eof : t -> Bigstring.t -> off:int -> len:int -> int
  (** [read_eof t bigstring ~off ~len] reads bytes of input from the provided
      range of [bigstring] and returns the number of bytes consumed by the
      connection. {!read_eof} should be called after {!next_read_operation}
      returns a [`Read] and an EOF has been received from the communication
      channel. The connection will attempt to consume any buffered input and
      then shutdown the HTTP parser for the connection. *)

  val next_write_operation :
     t
    -> [ `Write of Bigstring.t IOVec.t list | `Yield | `Close of int ]
  (** [next_write_operation t] returns a value describing the next operation
      that the caller should conduct on behalf of the connection. *)

  val report_write_result : t -> [ `Ok of int | `Closed ] -> unit
  (** [report_write_result t result] reports the result of the latest write
      attempt to the connection. {!report_write_result} should be called after a
      call to {!next_write_operation} that returns a [`Write buffer] value.

        {ul
        {- [`Ok n] indicates that the caller successfully wrote [n] bytes of
        output from the buffer that the caller was provided by
        {!next_write_operation}. }
        {- [`Closed] indicates that the output destination will no longer
        accept bytes from the write processor. }} *)

  val yield_reader : t -> (unit -> unit) -> unit

  val yield_writer : t -> (unit -> unit) -> unit
  (** [yield_writer t continue] registers with the connection to call
      [continue] when writing should resume. {!yield_writer} should be called
      after {!next_write_operation} returns a [`Yield] value. *)

  val report_exn : t -> exn -> unit
  (** [report_exn t exn] reports that an error [exn] has been caught and that it
      has been attributed to [t]. Calling this function will switch [t] into an
      error state. Depending on the state [t] is transitioning from, it may call
      its error handler before terminating the connection. *)

  val is_closed : t -> bool
  (** [is_closed t] is [true] if both the read and write processors have been
      shutdown. When this is the case {!next_read_operation} will return
      [`Close] and {!next_write_operation} will return [`Write _] until all
      buffered output has been flushed, at which point it will also return
      `Close. *)

  val shutdown : t -> unit
  (** [shutdown connection] closes the underlying input and output channels of
      the connection, rendering it unusable for any further communication. *)
end

(**/**)

module Ache_private : sig
  module Encoder = Encoder

  module Runtime : sig
    module Upgradable : sig
      include Runtime.ENGINE

      val create : Runtime.impl -> t
      val upgrade : t -> Runtime.impl -> unit
    end

    module Buffer : sig
      type t

      val create : int -> t
      val get : t -> f:(Bigstring.t -> off:int -> len:int -> int) -> int

      val unread : t -> bool
      (** [unread t] is [true] when the last {!get} consumed some bytes and
          left others. The runtime offers the rest again before it reads more
          input, so that a protocol that took over the connection receives
          them. *)

      val put :
         t
        -> f:(Bigstring.t -> off:int -> len:int -> (int -> unit) -> unit)
        -> (int -> unit)
        -> unit
    end
  end

  module Http_date : sig
    val imf_fixdate : int -> string
    (** [imf_fixdate seconds] is the IMF-fixdate (RFC 9110 §5.6.7) of
        [seconds] since the Unix epoch, such as
        [Sun, 06 Nov 1994 08:49:37 GMT]. *)

    val cached : int -> string
    (** [cached seconds] is [imf_fixdate seconds], kept per domain: it
        allocates only when [seconds] differs from the previous call in the
        same domain. *)

    val now : unit -> string
    (** [now ()] is [cached] of the current second ([time(2)]): the [Date]
        function that the runtimes give to {!Server_connection.create}. *)
  end

  module Response_descriptor : sig
    type t

    val awaiting : Request.t -> (Response.t -> Body.Reader.t -> unit) -> t
  end

  module Parse : sig
    type more =
      | Complete
      | Incomplete

    module Grammar : sig
      val request : string -> (Request.t, string) result
      val response : string -> (Response.t, string) result
      val chunk_size : string -> (int64, string) result
      val trailers : string -> (Headers.t, string) result
    end

    module Reader : sig
      type request_error =
        [ `Invalid_request of
            Request.t
            * [ `Bad_request | `Not_implemented | `Http_version_not_supported ]
        | `Parse of string
        | `Method_too_long
        | `Target_too_long
        | `Header_too_large
        ]

      type response_error =
        [ `Invalid_response_body_length of Response.t
        | `Parse of string
        | `Header_too_large
        | `Unsolicited_response
        ]

      type ('head, 'error) t
      type request = (Request.t, request_error) t
      type response = (Response.t, response_error) t

      val request :
         max_header_size:int
        -> (Request.t -> Body.Reader.t -> unit)
        -> request

      val response :
         max_header_size:int
        -> Response_descriptor.t Queue.t
        -> response

      val read_with_more :
         (_, _) t
        -> Bigstring.t
        -> off:int
        -> len:int
        -> more
        -> int

      val force_close : (_, _) t -> unit
      val resume : (_, _) t -> unit

      val next :
         (_, 'error) t
        -> [> `Read | `Start | `Close | `Error of 'error ]
    end
  end

  module Serialize : sig
    val write_request : Encoder.t -> Request.t -> unit
    val write_response : Encoder.t -> Response.t -> unit
  end
end
