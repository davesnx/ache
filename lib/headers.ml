(*----------------------------------------------------------------------------
  Copyright (c) 2017 Inhabited Type LLC.

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

type name = string
type value = string
type t = (name * value) list

let empty : t = []
let singleton nv = [ nv ]
let of_rev_list t = t
let of_list t = of_rev_list (List.rev t)
let to_rev_list t = t
let to_list t = List.rev (to_rev_list t)

module CI = struct
  (* Convert codes to upper case and compare them. This is a port of assembly
     code from the page:

     http://www.azillionmonkeys.com/qed/asmexample.html *)
  let[@inline always] char_code_equal_ci x y =
    let codes = (x lsl 8) lor y in
    let b = 0x8080 lor codes in
    let c = b - 0x6161 in
    let d = lnot (b - 0x7b7b) in
    let e = c land d land (lnot codes land 0x8080) in
    let upper = codes - (e lsr 2) in
    upper lsr 8 = upper land 0xff

  external unsafe_get_int64 : string -> int -> int64 = "%caml_string_get64u"
  external unsafe_get_int32 : string -> int -> int32 = "%caml_string_get32u"

  let[@inline always] lower_ascii w =
    let h = Int64.logand w 0x7f7f7f7f7f7f7f7fL in
    let ge_a = Int64.add h 0x3f3f3f3f3f3f3f3fL in
    let gt_z = Int64.add h 0x2525252525252525L in
    let m =
      Int64.logand
        (Int64.logand ge_a (Int64.lognot gt_z))
        (Int64.logand (Int64.lognot w) 0x8080808080808080L)
    in
    Int64.logor w (Int64.shift_right_logical m 2)

  let[@inline always] word_equal x y i =
    let a = unsafe_get_int64 x i in
    let b = unsafe_get_int64 y i in
    Int64.equal a b || Int64.equal (lower_ascii a) (lower_ascii b)

  let[@inline always] word32_equal x y i =
    let a = unsafe_get_int32 x i in
    let b = unsafe_get_int32 y i in
    Int32.equal a b
    || Int64.equal
         (lower_ascii (Int64.of_int32 a))
         (lower_ascii (Int64.of_int32 b))

  let rec words_equal x y i last =
    if i >= last
    then word_equal x y last
    else word_equal x y i && words_equal x y (i + 8) last

  let rec bytes_equal x y i len =
    i >= len
    || (char_code_equal_ci
          (Char.code (String.unsafe_get x i))
          (Char.code (String.unsafe_get y i))
       && bytes_equal x y (i + 1) len)

  let equal_same_length x y len =
    if len >= 8
    then words_equal x y 0 (len - 8)
    else if len >= 4
    then word32_equal x y 0 && word32_equal x y (len - 4)
    else bytes_equal x y 0 len

  let[@inline] equal x y =
    let len = String.length x in
    len = String.length y && equal_same_length x y len
end

let ci_equal = CI.equal

let rec mem t name =
  match t with
  | (name', _) :: t' -> CI.equal name name' || mem t' name
  | _ -> false

let add t name value = (name, value) :: t
let add_list t ls = ls @ t (* XXX(seliopou): do better here *)

let add_multi =
  let rec loop_outer t lss =
    match lss with [] -> t | (n, vs) :: lss' -> loop_inner t n vs lss'
  and loop_inner t n vs lss =
    match vs with
    | [] -> loop_outer t lss
    | v :: vs' -> loop_inner ((n, v) :: t) n vs' lss
  in
  loop_outer

let add_unless_exists t name value =
  if mem t name then t else (name, value) :: t

exception Local

let replace t name value =
  let rec loop t needle nv seen =
    match t with
    | [] -> if not seen then raise Local else []
    | ((name, _) as nv') :: t ->
      if CI.equal needle name
      then if seen then loop t needle nv true else nv :: loop t needle nv true
      else nv' :: loop t needle nv seen
  in
  try loop t name (name, value) false with Local -> t

let remove t name =
  let rec loop s needle seen =
    match s with
    | [] -> if not seen then raise Local else []
    | ((name, _) as nv') :: s' ->
      if CI.equal needle name
      then loop s' needle true
      else nv' :: loop s' needle seen
  in
  try loop t name false with Local -> t

let get t name =
  let rec loop t n =
    match t with
    | [] -> None
    | (n', v) :: t' -> if CI.equal n n' then Some v else loop t' n
  in
  loop t name

let get_exn t name =
  let rec loop t n =
    match t with
    | [] -> raise Not_found
    | (n', v) :: t' -> if CI.equal n n' then v else loop t' n
  in
  loop t name

let get_multi t name =
  let rec loop t n acc =
    match t with
    | [] -> acc
    | (n', v) :: t' ->
      if CI.equal n n' then loop t' n (v :: acc) else loop t' n acc
  in
  loop t name []

let iter ~f t = List.iter (fun (name, value) -> f name value) t

let fold ~f ~init t =
  List.fold_left (fun acc (name, value) -> f name value acc) init t

let to_string t =
  let b = Buffer.create 128 in
  iter (to_list t) ~f:(fun name value ->
    Buffer.add_string b name;
    Buffer.add_string b ": ";
    Buffer.add_string b value;
    Buffer.add_string b "\r\n");
  Buffer.add_string b "\r\n";
  Buffer.contents b

let pp_hum fmt t =
  let pp_elem fmt (n, v) = Format.fprintf fmt "@[(%S %S)@]" n v in
  Format.fprintf fmt "@[(";
  Format.pp_print_list pp_elem fmt (to_list t);
  Format.fprintf fmt ")@]"
