(* Generators of valid message parts, for properties where the input must be
   accepted. *)

open Ache

let tchars =
  "!#$%&'*+-.^_`|~0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

let token st = Gen.string_of st tchars (Gen.range st 1 8)

(* RFC 9110 §5.5 field-vchar: VCHAR / obs-text. *)
let vchar st =
  if Gen.chance st 5
  then Char.chr (Gen.range st 0x80 0xff)
  else Char.chr (Gen.range st 0x21 0x7e)

(* RFC 9110 §5.5 field-value, without leading or trailing whitespace, which a
   parser removes. *)
let field_value st =
  if Gen.chance st 6
  then ""
  else
    let inner =
      String.init (Gen.int st 10) (fun _ ->
        match Gen.int st 5 with 0 -> ' ' | 1 -> '\t' | _ -> vchar st)
    in
    let last =
      if inner <> "" || Gen.bool st then String.make 1 (vchar st) else ""
    in
    String.make 1 (vchar st) ^ inner ^ last

(* Field names start with "x-", so that no generated field changes the framing
   or the connection. *)
let headers st =
  List.init (Gen.int st 4) (fun _ -> "x-" ^ token st, field_value st)

let path st =
  let pchars = "abcXYZ019-._~!$&'()*+,;=:@/" in
  "/"
  ^ Gen.string_of st pchars (Gen.int st 12)
  ^ (if Gen.bool st
     then "?" ^ Gen.string_of st (pchars ^ "?") (Gen.int st 8)
     else "")
  ^ if Gen.chance st 4 then "%4a" else ""

(* RFC 9112 §4 reason-phrase: HTAB, SP, VCHAR, obs-text. *)
let reason st =
  String.init (Gen.int st 12) (fun _ ->
    match Gen.int st 6 with 0 -> ' ' | 1 -> '\t' | _ -> vchar st)

let pieces st = List.init (Gen.int st 5) (fun _ -> Gen.body st)
let meths : Method.t list = [ `GET; `POST; `PUT; `DELETE; `HEAD; `OPTIONS ]

let meth st =
  if Gen.chance st 5
  then Method.of_string (String.uppercase_ascii (token st))
  else Gen.pick st meths

let host = "host", "example.com"

let is_prefix prefix l =
  let rec go = function
    | [], _ -> true
    | x :: xs, y :: ys -> x = y && go (xs, ys)
    | _ :: _, [] -> false
  in
  go (prefix, l)

let rec drop n l =
  if n = 0 then l else match l with [] -> [] | _ :: t -> drop (n - 1) t

(* The fields received are the fields sent, then fields that ache adds for
   framing and the connection. *)
let check_fields ~sent ~received =
  let added = [ "content-length"; "transfer-encoding"; "connection" ] in
  if not (is_prefix sent received)
  then Some "the received fields do not start with the sent fields"
  else
    match
      List.find_opt
        (fun (name, _) -> not (List.mem (String.lowercase_ascii name) added))
        (drop (List.length sent) received)
    with
    | Some (name, _) -> Some (Printf.sprintf "unexpected added field %S" name)
    | None -> None

let fields_to_string fields =
  String.concat
    "; "
    (List.map (fun (n, v) -> Printf.sprintf "%S: %S" n v) fields)
