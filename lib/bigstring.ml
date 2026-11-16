type t = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

let create len = Bigarray.Array1.create Bigarray.char Bigarray.c_layout len
let empty = create 0

external length : t -> int = "%caml_ba_dim_1"
external get : t -> int -> char = "%caml_ba_ref_1"
external unsafe_get : t -> int -> char = "%caml_ba_unsafe_ref_1"
external unsafe_set : t -> int -> char -> unit = "%caml_ba_unsafe_set_1"
external unsafe_get_int32_ne : t -> int -> int32 = "%caml_bigstring_get32u"
external unsafe_get_int64_ne : t -> int -> int64 = "%caml_bigstring_get64u"

external unsafe_set_int16_ne :
   t
  -> int
  -> int
  -> unit
  = "%caml_bigstring_set16u"

external unsafe_set_int64_ne :
   t
  -> int
  -> int64
  -> unit
  = "%caml_bigstring_set64u"

external unsafe_blit :
   t
  -> src_off:(int[@untagged])
  -> t
  -> dst_off:(int[@untagged])
  -> len:(int[@untagged])
  -> unit
  = "ache_bigstring_blit_byte" "ache_bigstring_blit"
[@@noalloc]

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

external unsafe_memchr :
   t
  -> (int[@untagged])
  -> (char[@untagged])
  -> (int[@untagged])
  -> (int[@untagged])
  = "ache_bigstring_memchr_byte" "ache_bigstring_memchr"
[@@noalloc]

let[@inline never] invalid_range op ~len ~off ~count =
  invalid_arg
    (Printf.sprintf
       "Bigstring.%s: range { off = %d; len = %d } outside a buffer of %d bytes"
       op
       off
       count
       len)

let[@inline] check op ~len ~off ~count =
  if off < 0 || count < 0 || len - off < count
  then invalid_range op ~len ~off ~count

let blit src ~src_off dst ~dst_off ~len =
  check "blit" ~len:(length src) ~off:src_off ~count:len;
  check "blit" ~len:(length dst) ~off:dst_off ~count:len;
  unsafe_blit src ~src_off dst ~dst_off ~len

let blit_from_string src ~src_off dst ~dst_off ~len =
  check "blit_from_string" ~len:(String.length src) ~off:src_off ~count:len;
  check "blit_from_string" ~len:(length dst) ~off:dst_off ~count:len;
  unsafe_blit_from_string src ~src_off dst ~dst_off ~len

let substring t ~off ~len =
  check "substring" ~len:(length t) ~off ~count:len;
  let b = Bytes.create len in
  unsafe_blit_to_bytes t ~src_off:off b ~dst_off:0 ~len;
  Bytes.unsafe_to_string b

let of_string ~off ~len s =
  check "of_string" ~len:(String.length s) ~off ~count:len;
  let t = create len in
  unsafe_blit_from_string s ~src_off:off t ~dst_off:0 ~len;
  t

let memchr t off c len =
  check "memchr" ~len:(length t) ~off ~count:len;
  unsafe_memchr t off c len
