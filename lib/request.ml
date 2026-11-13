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
    match Message.unique_content_length_values headers with
    | [] -> `Fixed 0L
    | [ len ] ->
      let len = Message.content_length_of_string len in
      if len >= 0L then `Fixed len else bad_request
    | _ -> bad_request

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

(* RFC 3986 §3.2: uri-host [ ":" port ]. *)
let authority s =
  let length = String.length s in
  let host_end =
    if length > 0 && s.[0] = '['
    then
      match String.index_opt s ']' with
      | Some j
        when j > 1 && String.for_all is_ip_literal_char (String.sub s 1 (j - 1))
        ->
        Some (j + 1)
      | _ -> None
    else
      let j = Option.value (String.index_opt s ':') ~default:length in
      if String.for_all is_reg_name_char (String.sub s 0 j)
      then Some j
      else None
  in
  match host_end with
  | None -> None
  | Some j when j = length -> Some (s, None)
  | Some j when s.[j] = ':' ->
    let port = String.sub s (j + 1) (length - j - 1) in
    if String.for_all Message.is_digit port
    then Some (String.sub s 0 j, Some port)
    else None
  | Some _ -> None

(* RFC 9110 §9.3.6: CONNECT targets uri-host ":" port. *)
let is_connect_target target =
  match authority target with
  | Some (host, Some port) ->
    host <> ""
    && port <> ""
    && String.length port <= 5
    && int_of_string port <= 65535
  | Some (_, None) | None -> false

let transfer_coding_error { version; headers; _ } =
  if not (Headers.mem headers "transfer-encoding")
  then None
  else if Version.compare version Version.v1_1 < 0
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
    | [] when Version.compare version Version.v1_1 >= 0 -> Error `Bad_request
    | _ :: _ :: _ -> Error `Bad_request
    | [ host ] when Option.is_none (authority host) -> Error `Bad_request
    | _ when meth = `CONNECT && not (is_connect_target target) ->
      Error `Bad_request
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
