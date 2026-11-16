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

(* This module contains functionality that applies to both requests and
   responses, which are collectively referred to in the HTTP 1.1 specifications
   as 'messages'. *)

let list_members values =
  List.concat_map
    (fun value -> List.map String.trim (String.split_on_char ',' value))
    values

let is_trimmed = function
  | ' ' | '\t' | '\n' | '\r' | '\012' -> true
  | _ -> false

let rec ci_equal_at value off token i =
  i = String.length token
  || (Char.lowercase_ascii value.[off + i] = Char.lowercase_ascii token.[i]
     && ci_equal_at value off token (i + 1))

let rec member_end value i =
  if i < String.length value && value.[i] <> ','
  then member_end value (i + 1)
  else i

let rec skip_forward value i stop =
  if i < stop && is_trimmed value.[i]
  then skip_forward value (i + 1) stop
  else i

let rec skip_backward value start i =
  if i > start && is_trimmed value.[i - 1]
  then skip_backward value start (i - 1)
  else i

let rec has_member_from value token start =
  let stop = member_end value start in
  let first = skip_forward value start stop in
  let last = skip_backward value first stop in
  (last - first = String.length token && ci_equal_at value first token 0)
  || (stop < String.length value && has_member_from value token (stop + 1))

let rec has_member values token =
  match values with
  | [] -> false
  | value :: values -> has_member_from value token 0 || has_member values token

let at_least_v1_1 ({ major; minor } : Version.t) =
  major > 1 || (major = 1 && minor >= 1)

let persistent_connection ?(proxy = false) version headers =
  let _ = proxy in
  (* XXX(seliopou): use proxy argument in the case of HTTP/1.0 as per
     https://tools.ietf.org/html/rfc7230#section-6.3 *)
  match Headers.get_multi headers "connection" with
  | [] -> at_least_v1_1 version
  | options ->
    if has_member options "close"
    then false
    else at_least_v1_1 version || has_member options "keep-alive"

let is_digit = function '0' .. '9' -> true | _ -> false

let rec same_chars a i stop b j =
  i = stop
  || (String.unsafe_get a i = String.unsafe_get b j
     && same_chars a (i + 1) stop b (j + 1))

let rec members_equal_from value start reference first last =
  let stop = member_end value start in
  let member_first = skip_forward value start stop in
  let member_last = skip_backward value member_first stop in
  member_last - member_first = last - first
  && same_chars value member_first member_last reference first
  && (stop = String.length value
     || members_equal_from value (stop + 1) reference first last)

let rec values_equal values reference first last =
  match values with
  | [] -> true
  | value :: values ->
    members_equal_from value 0 reference first last
    && values_equal values reference first last

let rec digits_value s i stop acc =
  if i = stop
  then acc
  else
    match String.unsafe_get s i with
    | '0' .. '9' as c ->
      digits_value s (i + 1) stop ((acc * 10) + Char.code c - Char.code '0')
    | _ -> -1

let rec all_digits s i stop =
  i = stop || (is_digit s.[i] && all_digits s (i + 1) stop)

let max_int_digits = if Sys.int_size >= 63 then 18 else 9

let content_length_of_span s first last =
  if first = last
  then -1L
  else if last - first <= max_int_digits
  then Int64.of_int (digits_value s first last 0)
  else if all_digits s first last
  then
    Option.value
      (Int64.of_string_opt (String.sub s first (last - first)))
      ~default:(-1L)
  else -1L

(* RFC 9110 §8.6, RFC 9112 §6.3: Content-Length = 1*DIGIT; a list of identical
   values is one value. [-1L] marks an invalid value. *)
let content_length value values =
  let stop = member_end value 0 in
  let first = skip_forward value 0 stop in
  let last = skip_backward value first stop in
  if
    (stop = String.length value
    || members_equal_from value (stop + 1) value first last)
    && values_equal values value first last
  then content_length_of_span value first last
  else -1L
