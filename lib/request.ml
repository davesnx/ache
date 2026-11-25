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

type t =
  { meth : Method.t
  ; target : string
  ; version : Version.t
  ; headers : Headers.t
  }

let create ?(version = Version.v1_1) ?(headers = Headers.empty) meth target =
  { meth; target; version; headers }

let bad_request = `Error `Bad_request

module Body_length = struct
  type t =
    [ `Fixed of Int64.t
    | `Chunked
    | `Error of [ `Bad_request ]
    ]

  let pp_hum fmt (len : t) =
    match len with
    | `Fixed n -> Format.fprintf fmt "Fixed %Li" n
    | `Chunked -> Format.pp_print_string fmt "Chunked"
    | `Error `Bad_request -> Format.pp_print_string fmt "Error: Bad request"
end

let transfer_codings headers =
  List.filter
    (fun coding -> coding <> "")
    (Message.list_members (Headers.get_multi headers "transfer-encoding"))

let body_length { headers; _ } : Body_length.t =
  if Headers.mem headers "transfer-encoding"
  then
    if Headers.mem headers "content-length"
    then bad_request
    else
      match List.rev (transfer_codings headers) with
      | last :: _ when Headers.ci_equal last "chunked" -> `Chunked
      | _ -> bad_request
  else
    match Headers.get_multi headers "content-length" with
    | [] -> `Fixed 0L
    | value :: values ->
      let len = Message.content_length value values in
      if len >= 0L then `Fixed len else bad_request

type error =
  [ `Bad_request
  | `Not_implemented
  | `Http_version_not_supported
  ]

let is_reg_name_char = function
  | 'a' .. 'z'
  | 'A' .. 'Z'
  | '0' .. '9'
  | '-' | '.' | '_' | '~' | '!' | '$' | '&' | '\'' | '(' | ')' | '*' | '+' | ','
  | ';' | '=' | '%' ->
    true
  | _ -> false

let is_ip_literal_char = function
  | '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' | ':' | '.' -> true
  | _ -> false

let reg_name_class = 1
let ip_literal_class = 2
let digit_class = 4

let host_classes =
  String.init 256 (fun i ->
    let c = Char.chr i in
    Char.chr
      ((if is_reg_name_char c then reg_name_class else 0)
      lor (if is_ip_literal_char c then ip_literal_class else 0)
      lor if Message.is_digit c then digit_class else 0))

(* [stop] must not exceed [String.length s]. *)
let rec skip_class s i stop cls =
  if
    i < stop
    && Char.code
         (String.unsafe_get host_classes (Char.code (String.unsafe_get s i)))
       land cls
       <> 0
  then skip_class s (i + 1) stop cls
  else i

(* RFC 3986 §3.2.2: the end of the uri-host that starts [s], or -1. *)
let host_end s =
  let length = String.length s in
  if length > 0 && String.unsafe_get s 0 = '['
  then
    let j = skip_class s 1 length ip_literal_class in
    if j > 1 && j < length && String.unsafe_get s j = ']' then j + 1 else -1
  else
    let j = skip_class s 0 length reg_name_class in
    if j = length || String.unsafe_get s j = ':' then j else -1

(* RFC 3986 §3.2: uri-host [ ":" port ]. An empty value is valid (RFC 9112
   §3.2), an empty host with a port is not (RFC 9110 §4.2.1). *)
let is_host_field s =
  let length = String.length s in
  let j = host_end s in
  (j > 0 || length = 0)
  && (j = length
     || (String.unsafe_get s j = ':'
        && skip_class s (j + 1) length digit_class = length))

let rec decimal_value s i stop acc =
  if i >= stop
  then acc
  else
    decimal_value
      s
      (i + 1)
      stop
      ((acc * 10) + Char.code (String.unsafe_get s i) - Char.code '0')

(* RFC 9110 §9.3.6: CONNECT targets uri-host ":" port. *)
let is_connect_target target =
  let length = String.length target in
  let j = host_end target in
  let digits = length - j - 1 in
  j > 0
  && j < length
  && String.unsafe_get target j = ':'
  && digits >= 1
  && digits <= 5
  && skip_class target (j + 1) length digit_class = length
  && decimal_value target (j + 1) length 0 <= 65535

let is_scheme_char = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '+' | '-' | '.' -> true
  | _ -> false

(* RFC 3986 §3.1: scheme = ALPHA *( ALPHA / DIGIT / "+" / "-" / "." ). *)
let is_absolute_form target =
  match String.index_opt target ':' with
  | Some i when i > 0 ->
    (match target.[0] with
    | 'a' .. 'z' | 'A' .. 'Z' ->
      String.for_all is_scheme_char (String.sub target 0 i)
    | _ -> false)
  | _ -> false

(* RFC 9110 §4.2.1 and §4.2.4: the authority of an absolute-form target has a
   host and no userinfo (decision 0007). *)
let has_valid_authority target =
  let scheme_end = String.index target ':' in
  let length = String.length target in
  if
    length < scheme_end + 3
    || target.[scheme_end + 1] <> '/'
    || target.[scheme_end + 2] <> '/'
  then true
  else
    let start = scheme_end + 3 in
    let rec stop i =
      if i < length && target.[i] <> '/' && target.[i] <> '?'
      then stop (i + 1)
      else i
    in
    let stop = stop start in
    stop > start
    && target.[start] <> ':'
    && not (String.contains (String.sub target start (stop - start)) '@')

(* RFC 9112 §3.2. *)
let target_fits_method meth target =
  match meth with
  | `CONNECT -> is_connect_target target
  | `OPTIONS when target = "*" -> true
  | _ ->
    String.starts_with ~prefix:"/" target
    || (is_absolute_form target && has_valid_authority target)

let transfer_coding_error { version; headers; _ } =
  if not (Headers.mem headers "transfer-encoding")
  then None
  else if not (Message.at_least_v1_1 version)
  then Some `Bad_request
  else
    match List.rev (transfer_codings headers) with
    | last :: rest when Headers.ci_equal last "chunked" ->
      if List.exists (Headers.ci_equal "chunked") rest
      then Some `Bad_request
      else if rest <> []
      then Some `Not_implemented
      else None
    | _ -> Some `Bad_request

let received_body_length ({ meth; target; version; headers } as t) =
  if version.major <> 1
  then Error `Http_version_not_supported
  else
    match Headers.get_multi headers "host" with
    | [] when Message.at_least_v1_1 version -> Error `Bad_request
    | _ :: _ :: _ -> Error `Bad_request
    | [ host ] when not (is_host_field host) -> Error `Bad_request
    | _ when not (target_fits_method meth target) -> Error `Bad_request
    | _ ->
      (match transfer_coding_error t with
      | Some error -> Error error
      | None ->
        (match body_length t with
        | `Error `Bad_request -> Error `Bad_request
        | (`Fixed _ | `Chunked) as length -> Ok length))

let persistent_connection ?proxy { version; headers; _ } =
  Message.persistent_connection ?proxy version headers

let pp_hum fmt { meth; target; version; headers } =
  Format.fprintf
    fmt
    "((method \"%a\") (target %S) (version \"%a\") (headers %a))"
    Method.pp_hum
    meth
    target
    Version.pp_hum
    version
    Headers.pp_hum
    headers
