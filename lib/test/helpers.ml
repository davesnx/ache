open Ache
module Encoder = Ache_private.Encoder

let maybe_serialize_body f body =
  match body with None -> () | Some body -> Encoder.write_string f body

let request_to_string ?body r =
  let f = Encoder.create 0x1000 in
  Ache_private.Serialize.write_request f r;
  maybe_serialize_body f body;
  Encoder.serialize_to_string f

let response_to_string ?body r =
  let f = Encoder.create 0x1000 in
  Ache_private.Serialize.write_response f r;
  maybe_serialize_body f body;
  Encoder.serialize_to_string f

module Read_operation = struct
  type t =
    [ `Read
    | `Yield
    | `Close
    ]

  let pp_hum fmt (t : t) =
    let str =
      match t with `Read -> "Read" | `Yield -> "Yield" | `Close -> "Close"
    in
    Format.pp_print_string fmt str
end

module Write_operation = struct
  type t =
    [ `Write of Ache.Bigstring.t IOVec.t list
    | `Yield
    | `Close of int
    ]

  let iovecs_to_string iovecs =
    let len = IOVec.lengthv iovecs in
    let bytes = Bytes.create len in
    let dst_off = ref 0 in
    List.iter
      (fun { IOVec.buffer; off = src_off; len } ->
         Ache.Bigstring.unsafe_blit_to_bytes
           buffer
           ~src_off
           bytes
           ~dst_off:!dst_off
           ~len;
         dst_off := !dst_off + len)
      iovecs;
    Bytes.unsafe_to_string bytes

  let pp_hum fmt (t : t) =
    match t with
    | `Write iovecs -> Format.fprintf fmt "Write %S" (iovecs_to_string iovecs)
    | `Yield -> Format.pp_print_string fmt "Yield"
    | `Close len -> Format.fprintf fmt "Close %i" len

  let to_write_as_string t =
    match t with
    | `Write iovecs -> Some (iovecs_to_string iovecs)
    | `Close _ | `Yield -> None
end

let write_operation = Alcotest.of_pp Write_operation.pp_hum
let read_operation = Alcotest.of_pp Read_operation.pp_hum

module Headers = struct
  include Headers

  let ( @ ) a b = Headers.add_list a (Headers.to_list b)
  let connection_close = Headers.of_list [ "connection", "close" ]
  let encoding_chunked = Headers.of_list [ "transfer-encoding", "chunked" ]
  let encoding_fixed n = Headers.of_list [ "content-length", string_of_int n ]
end

let cut_bodies = ref []

let body_must_complete loc () =
  cut_bodies := loc :: !cut_bodies;
  Alcotest.failf "body cut short at %s" loc

let no_body_cut () =
  Alcotest.(check (list string)) "bodies cut short" [] (List.rev !cut_bodies)
