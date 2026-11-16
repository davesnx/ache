(** Byte buffers outside the OCaml heap, the buffers that ache reads from and
    writes to.

    [t] is the same type as [Bigstring.t], [Cstruct.buffer] and
    [Lwt_bytes.t], so their values pass to ache without a copy.

    Functions named [unsafe_*] do not check bounds: an offset or length outside
    the buffer reads or writes memory outside it. The other functions raise
    [Invalid_argument] for a range outside the buffer. *)

type t = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

val create : int -> t
(** [create len] is a buffer of [len] bytes with unspecified contents. *)

val empty : t
(** A buffer of length 0. *)

external length : t -> int = "%caml_ba_dim_1"
external get : t -> int -> char = "%caml_ba_ref_1"
external unsafe_get : t -> int -> char = "%caml_ba_unsafe_ref_1"
external unsafe_set : t -> int -> char -> unit = "%caml_ba_unsafe_set_1"

external unsafe_get_int32_ne : t -> int -> int32 = "%caml_bigstring_get32u"
(** Reads 4 bytes in native byte order. *)

external unsafe_get_int64_ne : t -> int -> int64 = "%caml_bigstring_get64u"
(** Reads 8 bytes in native byte order. *)

external unsafe_set_int16_ne :
   t
  -> int
  -> int
  -> unit
  = "%caml_bigstring_set16u"
(** Writes the low 16 bits of the int in native byte order. *)

external unsafe_set_int64_ne :
   t
  -> int
  -> int64
  -> unit
  = "%caml_bigstring_set64u"
(** Writes 8 bytes in native byte order. *)

external unsafe_blit :
   t
  -> src_off:(int[@untagged])
  -> t
  -> dst_off:(int[@untagged])
  -> len:(int[@untagged])
  -> unit
  = "ache_bigstring_blit_byte" "ache_bigstring_blit"
[@@noalloc]
(** Copies [len] bytes. The two ranges can overlap. *)

external unsafe_blit_from_string :
   string
  -> src_off:(int[@untagged])
  -> t
  -> dst_off:(int[@untagged])
  -> len:(int[@untagged])
  -> unit
  = "ache_bigstring_blit_from_string_byte" "ache_bigstring_blit_from_string"
[@@noalloc]

external unsafe_blit_to_bytes :
   t
  -> src_off:(int[@untagged])
  -> bytes
  -> dst_off:(int[@untagged])
  -> len:(int[@untagged])
  -> unit
  = "ache_bigstring_blit_to_bytes_byte" "ache_bigstring_blit_to_bytes"
[@@noalloc]

val blit : t -> src_off:int -> t -> dst_off:int -> len:int -> unit
(** Copies [len] bytes. The two ranges can overlap. *)

val blit_from_string :
   string
  -> src_off:int
  -> t
  -> dst_off:int
  -> len:int
  -> unit

val substring : t -> off:int -> len:int -> string
(** A copy of [len] bytes from [off]. *)

val of_string : off:int -> len:int -> string -> t
(** A new buffer with a copy of [len] bytes of the string from [off]. *)

val memchr : t -> int -> char -> int -> int
(** [memchr t off c len] is the index in [t] of the first [c] in the [len]
    bytes from [off], or [-1] when there is none. *)
