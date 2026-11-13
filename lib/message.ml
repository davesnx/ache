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

let persistent_connection ?(proxy = false) version headers =
  let _ = proxy in
  (* XXX(seliopou): use proxy argument in the case of HTTP/1.0 as per
     https://tools.ietf.org/html/rfc7230#section-6.3 *)
  match Headers.get_multi headers "connection" with
  | [] -> Version.(compare version v1_1) >= 0
  | options ->
    if has_member options "close"
    then false
    else Version.(compare version v1_1) >= 0 || has_member options "keep-alive"

let sort_uniq xs =
  (* Though {!List.sort_uniq} performs a check on the input length and returns *
     immediately for lists of length less than [2], it still allocates closures
     * before it does that check! To avoid that just do our own checking here to
     * avoid the allocations in the common case. *)
  match xs with
  | [] | [ _ ] -> xs
  | _ -> List.sort_uniq String.compare xs

let unique_content_length_values headers =
  sort_uniq (list_members (Headers.get_multi headers "content-length"))

let is_digit = function '0' .. '9' -> true | _ -> false

(* RFC 9110 §8.6: Content-Length = 1*DIGIT. [-1L] marks an invalid value. *)
let content_length_of_string s =
  if s <> "" && String.for_all is_digit s
  then Option.value (Int64.of_string_opt s) ~default:(-1L)
  else -1L
