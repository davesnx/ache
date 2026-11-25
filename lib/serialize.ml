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

open Encoder

let write_space t = write_char t ' '
let write_crlf t = LE.write_uint16 t 0x0a0d

let write_version t (version : Version.t) =
  match version with
  | { major = 1; minor = 1 } -> LE.write_uint64 t 0x312e312f50545448L
  | { major = 1; minor = 0 } -> LE.write_uint64 t 0x302e312f50545448L
  | _ -> write_string t (Version.to_string version)

let write_status t status =
  let s = Status.to_string status in
  if String.length s = 3
  then (
    LE.write_uint16
      t
      (Char.code (String.unsafe_get s 0)
      lor (Char.code (String.unsafe_get s 1) lsl 8));
    write_char t (String.unsafe_get s 2))
  else write_string t s

(* RFC 9110 §5.6.2 tchar *)
let is_tchar = function
  | '0' .. '9'
  | 'a' .. 'z'
  | 'A' .. 'Z'
  | '!' | '#' | '$' | '%' | '&' | '\'' | '*' | '+' | '-' | '.' | '^' | '_' | '`'
  | '|' | '~' ->
    true
  | _ -> false

let tchars =
  Bytes.init 256 (fun i -> if is_tchar (Char.chr i) then '\001' else '\000')

let rec is_token_from s i =
  i = String.length s
  || (Bytes.unsafe_get tchars (Char.code (String.unsafe_get s i)) = '\001'
     && is_token_from s (i + 1))

let is_token s = s <> "" && is_token_from s 0

let rec has_cr_lf_nul s i =
  i < String.length s
  &&
  let c = String.unsafe_get s i in
  (c <= '\r' && (c = '\r' || c = '\n' || c = '\000')) || has_cr_lf_nul s (i + 1)

let rec has_cr_lf_nul_space s i =
  i < String.length s
  &&
  let c = String.unsafe_get s i in
  (c <= ' ' && (c = ' ' || c = '\r' || c = '\n' || c = '\000'))
  || has_cr_lf_nul_space s (i + 1)

let invalid_argf fmt = Printf.ksprintf invalid_arg fmt

let rec check_fields = function
  | [] -> ()
  | (name, value) :: rest ->
    if not (is_token name) then invalid_argf "ache: invalid field name %S" name;
    if has_cr_lf_nul value 0
    then invalid_argf "ache: invalid value for field %S" name;
    check_fields rest

let check_headers headers = check_fields (Headers.to_rev_list headers)

let rec write_fields t = function
  | [] -> ()
  | (name, value) :: rest ->
    write_string t name;
    LE.write_uint16 t 0x203a;
    write_string t value;
    write_crlf t;
    write_fields t rest

let write_headers t headers =
  write_fields t (Headers.to_list headers);
  write_crlf t

let check_request_head { Request.meth; target; headers; _ } =
  let meth = Method.to_string meth in
  if not (is_token meth) then invalid_argf "ache: invalid method %S" meth;
  if has_cr_lf_nul_space target 0
  then invalid_argf "ache: invalid request target %S" target;
  check_headers headers

(* RFC 9110 §7.2: one Host field. RFC 9112 §6.1-6.3: a body length the recipient
   can read. *)
let check_request ({ Request.headers; _ } as request) =
  check_request_head request;
  (match Headers.get_multi headers "host" with
  | _ :: _ :: _ -> invalid_arg "ache: more than one host field"
  | [] | [ _ ] -> ());
  match Request.body_length request with
  | `Fixed _ | `Chunked -> ()
  | `Error `Bad_request ->
    if Headers.mem headers "content-length"
    then
      if Headers.mem headers "transfer-encoding"
      then invalid_arg "ache: content-length and transfer-encoding are both set"
      else
        invalid_argf
          "ache: invalid content-length %S"
          (String.concat ", " (Headers.get_multi headers "content-length"))
    else invalid_arg "ache: transfer-encoding does not end with chunked"

let write_request t ({ Request.meth; target; version; headers } as request) =
  check_request_head request;
  write_string t (Method.to_string meth);
  write_space t;
  write_string t target;
  write_space t;
  write_version t version;
  write_crlf t;
  write_headers t headers

let check_response { Response.reason; headers; _ } =
  if has_cr_lf_nul reason 0
  then invalid_argf "ache: invalid reason phrase %S" reason;
  check_headers headers

let write_response t ({ Response.version; status; reason; headers } as response)
  =
  check_response response;
  write_version t version;
  write_space t;
  write_status t status;
  write_space t;
  write_string t reason;
  write_crlf t;
  write_headers t headers

let check_date date =
  if has_cr_lf_nul date 0 then invalid_argf "ache: invalid date %S" date

let write_response_with
      t
      ({ Response.version; status; reason; headers } as response)
      ~date
      ~keep_alive
  =
  check_response response;
  write_version t version;
  write_space t;
  write_status t status;
  write_space t;
  write_string t reason;
  write_crlf t;
  write_fields t (Headers.to_list headers);
  if date <> ""
  then (
    write_string t "Date: ";
    write_string t date;
    write_crlf t);
  if keep_alive <> ""
  then (
    write_string t "Keep-Alive: ";
    write_string t keep_alive;
    write_crlf t);
  write_crlf t

let rec write_hex_digits t len shift =
  if shift >= 0
  then (
    String.unsafe_get "0123456789abcdef" ((len lsr shift) land 0xf)
    |> write_char t;
    write_hex_digits t len (shift - 4))

let rec hex_top_shift len shift =
  if shift < 60 && len lsr (shift + 4) <> 0
  then hex_top_shift len (shift + 4)
  else shift

let write_chunk_length t len =
  write_hex_digits t len (hex_top_shift len 0);
  write_crlf t

let write_string_chunk t chunk =
  write_chunk_length t (String.length chunk);
  write_string t chunk;
  write_crlf t

let write_bigstring_chunk t chunk =
  write_chunk_length t (Bigstring.length chunk);
  write_bigstring t chunk;
  write_crlf t

let schedule_bigstring_chunk t chunk =
  write_chunk_length t (Bigstring.length chunk);
  schedule_bigstring t chunk;
  write_crlf t

module Writer = struct
  type t =
    { buffer : Bigstring.t
      (* The buffer that the encoder uses for buffered writes. Managed by the *
         control module for the encoder. *)
    ; encoder : Encoder.t
      (* The encoder that handles encoding for writes. Uses the [buffer] *
         referenced above internally. *)
    ; mutable drained_bytes : int
      (* The number of bytes that were not written due to the output stream *
         being closed before all buffered output could be written. Useful for *
         detecting error cases. *)
    ; mutable wakeup : Optional_thunk.t
      (* The callback from the runtime to be invoked when output is ready to be
       * flushed. *)
    ; mutable held : bool
    ; mutable woken_while_held : bool
    }

  let create ?(buffer_size = 0x800) () =
    let buffer = Bigstring.create buffer_size in
    let encoder = Encoder.of_bigstring buffer in
    { buffer
    ; encoder
    ; drained_bytes = 0
    ; wakeup = Optional_thunk.none
    ; held = false
    ; woken_while_held = false
    }

  let encoder t = t.encoder
  let write_request t request = write_request t.encoder request
  let write_response t response = write_response t.encoder response

  let write_response_with t response ~date ~keep_alive =
    write_response_with t.encoder response ~date ~keep_alive

  let write_string t ?off ?len string = write_string t.encoder ?off ?len string

  let write_bigstring t ?off ?len bigstring =
    write_bigstring t.encoder ?off ?len bigstring

  let schedule_bigstring t ?off ?len bigstring =
    schedule_bigstring t.encoder ?off ?len bigstring

  let schedule_fixed t iovecs =
    List.iter
      (fun { IOVec.buffer; off; len } -> schedule_bigstring t ~off ~len buffer)
      iovecs

  let schedule_chunk t iovecs =
    let length = IOVec.lengthv iovecs in
    write_chunk_length t.encoder length;
    schedule_fixed t iovecs;
    write_crlf t.encoder

  let write_last_chunk t trailers =
    write_chunk_length t.encoder 0;
    write_headers t.encoder trailers

  let on_wakeup t k =
    if Encoder.is_closed t.encoder
    then failwith "on_wakeup on closed writer"
    else if Optional_thunk.is_some t.wakeup
    then failwith "on_wakeup: only one callback can be registered at a time"
    else t.wakeup <- Optional_thunk.some k

  let wakeup t =
    if t.held
    then t.woken_while_held <- true
    else
      let f = t.wakeup in
      if Optional_thunk.is_some f
      then (
        t.wakeup <- Optional_thunk.none;
        Optional_thunk.call_if_some f)

  let hold t = t.held <- true

  let release t =
    t.held <- false;
    if t.woken_while_held
    then (
      t.woken_while_held <- false;
      wakeup t)

  let flush t f =
    flush_with_reason t.encoder (fun reason ->
      let result =
        match reason with
        | Nothing_pending | Shift -> `Written
        | Drain -> `Closed
      in
      f result)

  let unyield t =
    (* This would be better implemented by a function that just takes the
       encoder out of a yielded state if it's in that state. Requires a change
       to Encoder. *)
    flush t (fun _reason -> ())

  let yield t = Encoder.yield t.encoder
  let close t = Encoder.close t.encoder

  let close_and_drain t =
    Encoder.close t.encoder;
    let drained = Encoder.drain t.encoder in
    t.drained_bytes <- t.drained_bytes + drained;
    wakeup t

  let is_closed t = Encoder.is_closed t.encoder
  let pending_bytes t = Encoder.pending_bytes t.encoder
  let drained_bytes t = t.drained_bytes

  let report_result t result =
    match result with
    | `Closed -> close_and_drain t
    | `Ok len -> shift t.encoder len

  let next t =
    (* assert (Optional_thunk.is_none t.wakeup); *)
    match Encoder.operation t.encoder with
    | `Close -> `Close (drained_bytes t)
    | `Yield -> `Yield
    | `Writev iovecs -> `Write iovecs
end
