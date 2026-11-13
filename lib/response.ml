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
  { version : Version.t
  ; status : Status.t
  ; reason : string
  ; headers : Headers.t
  }

let create ?reason ?(version = Version.v1_1) ?(headers = Headers.empty) status =
  let reason =
    match reason with
    | Some reason -> reason
    | None ->
      (match status with
      | #Status.standard as status -> Status.default_reason_phrase status
      | `Code _ -> "Non-standard status code")
  in
  { version; status; reason; headers }

let persistent_connection ?proxy { version; headers; _ } =
  Message.persistent_connection ?proxy version headers

let proxy_error = `Error `Bad_gateway
let server_error = `Error `Internal_server_error

module Body_length = struct
  type t =
    [ `No_body
    | `Fixed of Int64.t
    | `Chunked
    | `Close_delimited
    | `Error of [ `Bad_gateway | `Internal_server_error ]
    ]

  let pp_hum fmt (len : t) =
    match len with
    | `No_body -> Format.pp_print_string fmt "No body"
    | `Fixed n -> Format.fprintf fmt "Fixed %Li" n
    | `Chunked -> Format.pp_print_string fmt "Chunked"
    | `Close_delimited -> Format.pp_print_string fmt "Close delimited"
    | `Error `Bad_gateway -> Format.pp_print_string fmt "Error: Bad gateway"
    | `Error `Internal_server_error ->
      Format.pp_print_string fmt "Error: Internal server error"
end

let final_coding headers =
  Headers.get_multi headers "transfer-encoding"
  |> List.concat_map (String.split_on_char ',')
  |> List.map String.trim
  |> List.filter (fun coding -> coding <> "")
  |> List.rev
  |> function
  | [] when Headers.mem headers "transfer-encoding" -> Some ""
  | [] -> None
  | coding :: _ -> Some coding

let body_length ?(proxy = false) ~request_method { status; headers; _ } :
  Body_length.t
  =
  match status, request_method with
  | _, `HEAD | (`No_content | `Not_modified), _ -> `No_body
  | s, _ when Status.is_informational s -> `No_body
  | s, `CONNECT when Status.is_successful s -> `Close_delimited
  | _, _ ->
    (match final_coding headers with
    | Some coding when Headers.ci_equal coding "chunked" -> `Chunked
    | Some _ -> `Close_delimited
    | None ->
      (match Message.unique_content_length_values headers with
      | [] -> `Close_delimited
      | [ len ] ->
        let len = Message.content_length_of_string len in
        if len >= 0L
        then `Fixed len
        else if proxy
        then proxy_error
        else server_error
      | _ -> if proxy then proxy_error else server_error))

let frame ~fn ~proxy ~request_method ~request_version ~persistent ~length t =
  let reject reason = invalid_arg (Printf.sprintf "%s: %s" fn reason) in
  let add name value t =
    { t with headers = Headers.add t.headers name value }
  in
  let has_length = Headers.mem t.headers "content-length" in
  let has_coding = Headers.mem t.headers "transfer-encoding" in
  let http_1_1 = Version.compare request_version Version.v1_1 >= 0 in
  let tunnel = request_method = `CONNECT && Status.is_successful t.status in
  let persists t = persistent && persistent_connection ~proxy t in
  (* RFC 9112 §6.2 *)
  if has_length && has_coding
  then reject "content-length and transfer-encoding are both set";
  (* RFC 9110 §8.6, RFC 9112 §6.1 *)
  if
    (has_length || has_coding)
    && (tunnel
       || Status.is_informational t.status
       || Status.to_code t.status = 204)
  then
    reject
      "content-length or transfer-encoding in a 1xx, a 204, or a 2xx to CONNECT";
  let close t = t, `Close_delimited in
  let t, encoding =
    match body_length ~proxy ~request_method t with
    | `Error (`Bad_gateway | `Internal_server_error) ->
      failwith (fn ^ ": invalid response body length")
    | `No_body -> t, `No_body
    | `Fixed n ->
      (match length with
      | Some length when Int64.of_int length <> n ->
        reject
          (Printf.sprintf
             "content-length is %Ld but the body has %d bytes"
             n
             length)
      | _ -> t, `Fixed n)
    | `Close_delimited when tunnel -> t, `Close_delimited
    | (`Chunked | `Close_delimited) when length <> None && has_coding ->
      reject "transfer-encoding is set on a body of known length"
    | (`Chunked | `Close_delimited) when has_coding && not http_1_1 ->
      close { t with headers = Headers.remove t.headers "transfer-encoding" }
    | `Chunked -> t, `Chunked
    | `Close_delimited when has_coding -> close t
    | `Close_delimited ->
      (match length with
      | Some length ->
        ( add "content-length" (string_of_int length) t
        , `Fixed (Int64.of_int length) )
      | None when http_1_1 && persists t ->
        add "transfer-encoding" "chunked" t, `Chunked
      | None -> close t)
  in
  let closes =
    (not tunnel) && (encoding = `Close_delimited || not (persists t))
  in
  (* RFC 9112 §9.6 *)
  if closes && not (Message.has_option t.headers "close")
  then add "connection" "close" t, encoding
  else if
    (not http_1_1) && persists t && not (Headers.mem t.headers "connection")
  then add "connection" "keep-alive" t, encoding
  else t, encoding

let pp_hum fmt { version; status; reason; headers } =
  Format.fprintf
    fmt
    "((version \"%a\") (status %a) (reason %S) (headers %a))"
    Version.pp_hum
    version
    Status.pp_hum
    status
    reason
    Headers.pp_hum
    headers
