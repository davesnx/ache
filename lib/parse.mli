type more =
  | Complete
  | Incomplete

module Grammar : sig
  (** Each function parses a complete input and never raises. *)

  val request : string -> (Request.t, string) result
  (** Request line and header section, with the final CRLF. *)

  val response : string -> (Response.t, string) result
  (** Status line and header section, with the final CRLF. *)

  val chunk_size : string -> (int64, string) result
  (** Chunk-size line with its CRLF. Chunk extensions are parsed and
      discarded. *)

  val trailers : string -> (Headers.t, string) result
  (** Trailer section after the last chunk, with the final CRLF. *)
end

module Reader : sig
  type request_error =
    [ `Invalid_request of Request.t * Request.error
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

  val may_switch_protocols : Request.t -> bool
  (** [CONNECT], or an [Upgrade] field with the [upgrade] connection option. *)

  val request :
     max_header_size:int
    -> (Request.t -> Body.Reader.t -> unit)
    -> request
  (** After a request that may switch protocols (CONNECT, or an [Upgrade]
      field with the [upgrade] connection option), the reader consumes nothing
      past the end of that message until {!resume}. *)

  val response : max_header_size:int -> Respd.t Queue.t -> response

  val read_with_more :
     (_, _) t
    -> Bigstringaf.t
    -> off:int
    -> len:int
    -> more
    -> int
  (** Returns the number of bytes consumed. A head, chunk-size line or trailer
      section that is still incomplete consumes all the bytes it was given.
      Nothing is consumed after a message that may switch protocols until the
      response decides. *)

  val force_close : (_, _) t -> unit
  (** Closes the reader and cancels a suspended parse. *)

  val is_closed : (_, _) t -> bool

  val resume : (_, _) t -> unit
  (** Continues parsing after a request that may switch protocols, once its
      response does not switch. *)

  val next : (_, 'error) t -> [> `Read | `Start | `Close | `Error of 'error ]
  val on_wakeup : (_, _) t -> (unit -> unit) -> unit
  val wakeup : (_, _) t -> unit
end
