(*----------------------------------------------------------------------------
  Copyright (c) 2016 Inhabited Type LLC.

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

type more = Parse_v1.more =
  | Complete
  | Incomplete

type 'a line =
  | Line of 'a * int
  | Partial
  | Invalid of string

let table f =
  String.init 256 (fun i -> if f (Char.chr i) then '\001' else '\000')

let is table c = String.get table (Char.code c) = '\001'

(* RFC 9110 §5.6.2 tchar *)
let tchar =
  table (function
    | '0' .. '9'
    | 'a' .. 'z'
    | 'A' .. 'Z'
    | '!' | '#' | '$' | '%' | '&' | '\'' | '*' | '+' | '-' | '.' | '^' | '_'
    | '`' | '|' | '~' ->
      true
    | _ -> false)

(* RFC 9110 §5.6.4 qdtext *)
let qdtext =
  table (function
    | '\t' | ' ' | '!' | '\x23' .. '\x5b' | '\x5d' .. '\x7e' | '\x80' .. '\xff'
      ->
      true
    | _ -> false)

let quoted_pair_char =
  table (function
    | '\t' | ' ' | '\x21' .. '\x7e' | '\x80' .. '\xff' -> true
    | _ -> false)

let target_char = table (fun c -> c > ' ' && c <> '\x7f')

let field_char =
  table (function
    | '\t' | ' ' .. '\x7e' | '\x80' .. '\xff' -> true
    | _ -> false)

let space = table (function ' ' | '\t' -> true | _ -> false)
let digit = table (function '0' .. '9' -> true | _ -> false)

let hex =
  table (function '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' -> true | _ -> false)

(* Past [stop] every byte reads as NUL, which no rule accepts. *)
let get bs stop i = if i < stop then Bigstringaf.get bs i else '\000'

let rec skip table bs stop i =
  if i < stop && is table (Bigstringaf.get bs i)
  then skip table bs stop (i + 1)
  else i

let rec skip_zeros bs stop i =
  if i < stop && Bigstringaf.get bs i = '0'
  then skip_zeros bs stop (i + 1)
  else i

let invalid ~eof stop p message =
  if p >= stop && not eof then Partial else Invalid message

let unexpected ~eof bs stop p where =
  if p < stop
  then
    Invalid (Printf.sprintf "unexpected %C in %s" (Bigstringaf.get bs p) where)
  else if eof
  then Invalid "unexpected end of input"
  else Partial

let end_line ~eof bs stop p where =
  if get bs stop p <> '\r'
  then unexpected ~eof bs stop p where
  else if get bs stop (p + 1) <> '\n'
  then unexpected ~eof bs stop (p + 1) where
  else Line ((), p + 2)

let version ~eof bs stop p where =
  let rec literal i =
    if i = 5
    then
      let major = get bs stop (p + 5) in
      if not (is digit major)
      then unexpected ~eof bs stop (p + 5) where
      else if get bs stop (p + 6) <> '.'
      then unexpected ~eof bs stop (p + 6) where
      else
        let minor = get bs stop (p + 7) in
        if not (is digit minor)
        then unexpected ~eof bs stop (p + 7) where
        else
          Line
            ( { Version.major = Char.code major - 48
              ; minor = Char.code minor - 48
              }
            , p + 8 )
    else if get bs stop (p + i) <> "HTTP/".[i]
    then unexpected ~eof bs stop (p + i) where
    else literal (i + 1)
  in
  literal 0

let request_line ~eof bs off stop =
  let where = "request-line" in
  let meth_end = skip tchar bs stop off in
  if meth_end = off || get bs stop meth_end <> ' '
  then unexpected ~eof bs stop meth_end where
  else
    let target_end = skip target_char bs stop (meth_end + 1) in
    if target_end = meth_end + 1 || get bs stop target_end <> ' '
    then unexpected ~eof bs stop target_end where
    else
      match version ~eof bs stop (target_end + 1) where with
      | (Partial | Invalid _) as e -> e
      | Line (version, p) ->
        (match end_line ~eof bs stop p where with
        | (Partial | Invalid _) as e -> e
        | Line ((), p) ->
          let meth =
            Method.of_string
              (Bigstringaf.substring bs ~off ~len:(meth_end - off))
          in
          let target =
            Bigstringaf.substring
              bs
              ~off:(meth_end + 1)
              ~len:(target_end - meth_end - 1)
          in
          Line ((meth, target, version), p))

let status_line ~eof bs off stop =
  let where = "status-line" in
  match version ~eof bs stop off where with
  | (Partial | Invalid _) as e -> e
  | Line (version, _) when version.major <> 1 ->
    Invalid
      (Printf.sprintf
         "unsupported HTTP-version: %d.%d"
         version.major
         version.minor)
  | Line (version, p) ->
    if get bs stop p <> ' '
    then unexpected ~eof bs stop p where
    else
      let code_end = skip digit bs stop (p + 1) in
      let n = code_end - p - 1 in
      let code () = Bigstringaf.substring bs ~off:(p + 1) ~len:n in
      if n = 0
      then invalid ~eof stop code_end "status-code empty"
      else if n > 3
      then
        invalid
          ~eof
          stop
          code_end
          (Printf.sprintf "status-code too long: %S" (code ()))
      else if n < 3 || Bigstringaf.get bs (p + 1) = '0'
      then
        invalid
          ~eof
          stop
          code_end
          (Printf.sprintf "invalid status-code: %S" (code ()))
      else if get bs stop code_end <> ' '
      then unexpected ~eof bs stop code_end where
      else
        let reason_end = skip field_char bs stop (code_end + 1) in
        (match end_line ~eof bs stop reason_end where with
        | (Partial | Invalid _) as e -> e
        | Line ((), q) ->
          let d i = Char.code (Bigstringaf.get bs (p + i)) - 48 in
          let status = Status.of_code ((100 * d 1) + (10 * d 2) + d 3) in
          let reason =
            Bigstringaf.substring
              bs
              ~off:(code_end + 1)
              ~len:(reason_end - code_end - 1)
          in
          Line ((version, status, reason), q))

let rec trim_end bs off i =
  if i > off && is space (Bigstringaf.get bs (i - 1))
  then trim_end bs off (i - 1)
  else i

(* RFC 9112 §5.2: a recipient of a response replaces each obs-fold with SP. *)
let field_line ~eof ~obs_fold fields bs off stop =
  let where = "field line" in
  let fold = obs_fold && fields <> [] && is space (get bs stop off) in
  let name_end = if fold then off else skip tchar bs stop off in
  if (not fold) && (name_end = off || get bs stop name_end <> ':')
  then unexpected ~eof bs stop name_end where
  else
    let start = skip space bs stop (if fold then off else name_end + 1) in
    let value_end = skip field_char bs stop start in
    match end_line ~eof bs stop value_end where with
    | (Partial | Invalid _) as e -> e
    | Line ((), p) ->
      let value =
        Bigstringaf.substring
          bs
          ~off:start
          ~len:(trim_end bs start value_end - start)
      in
      (match fields with
      | (name, previous) :: rest when fold ->
        Line ((name, String.trim (previous ^ " " ^ value)) :: rest, p)
      | _ ->
        let name = Bigstringaf.substring bs ~off ~len:(name_end - off) in
        Line ((name, value) :: fields, p))

let rec fields ~eof ~obs_fold ~where acc bs p stop =
  if get bs stop p = '\r'
  then
    match end_line ~eof bs stop p where with
    | (Partial | Invalid _) as e -> e
    | Line ((), p) -> Line (Headers.of_rev_list acc, p)
  else
    match field_line ~eof ~obs_fold acc bs p stop with
    | (Partial | Invalid _) as e -> e
    | Line (acc, p) -> fields ~eof ~obs_fold ~where acc bs p stop

let rec quoted_string ~eof bs stop p where =
  let p = skip qdtext bs stop p in
  match get bs stop p with
  | '"' -> Line ((), p + 1)
  | '\\' ->
    if is quoted_pair_char (get bs stop (p + 1))
    then quoted_string ~eof bs stop (p + 2) where
    else unexpected ~eof bs stop (p + 1) where
  | _ -> unexpected ~eof bs stop p where

(* RFC 9112 §7.1.1 chunk-ext *)
let rec chunk_ext ~eof bs stop p where =
  match get bs stop p with
  | ' ' | '\t' | ';' ->
    let p = skip space bs stop p in
    if get bs stop p <> ';'
    then unexpected ~eof bs stop p where
    else
      let name = skip space bs stop (p + 1) in
      let name_end = skip tchar bs stop name in
      if name_end = name
      then unexpected ~eof bs stop name where
      else (
        match get bs stop name_end with
        | ' ' | '\t' | '=' ->
          let p = skip space bs stop name_end in
          if get bs stop p <> '='
          then unexpected ~eof bs stop p where
          else
            let value = skip space bs stop (p + 1) in
            if get bs stop value = '"'
            then
              match quoted_string ~eof bs stop (value + 1) where with
              | (Partial | Invalid _) as e -> e
              | Line ((), p) -> chunk_ext ~eof bs stop p where
            else
              let value_end = skip tchar bs stop value in
              if value_end = value
              then unexpected ~eof bs stop value where
              else chunk_ext ~eof bs stop value_end where
        | _ -> chunk_ext ~eof bs stop name_end where)
  | _ -> Line ((), p)

(* 15 hex digits keep the size below 2^60, so it fits an Int64 and an int. *)
let max_chunk_size_digits = 15

let hex_value c =
  match c with
  | '0' .. '9' -> Char.code c - 48
  | 'a' .. 'f' -> Char.code c - 87
  | _ -> Char.code c - 55

let chunk_size_line ~eof bs off stop =
  let where = "chunk-size line" in
  let digits = skip_zeros bs stop off in
  let digits_end = skip hex bs stop digits in
  if digits_end = off
  then unexpected ~eof bs stop off where
  else if digits_end - digits > max_chunk_size_digits
  then
    invalid
      ~eof
      stop
      digits_end
      (Printf.sprintf
         "chunk-size too long: %S"
         (Bigstringaf.substring bs ~off:digits ~len:(digits_end - digits)))
  else
    match chunk_ext ~eof bs stop digits_end where with
    | (Partial | Invalid _) as e -> e
    | Line ((), p) ->
      (match end_line ~eof bs stop p where with
      | (Partial | Invalid _) as e -> e
      | Line ((), p) ->
        let rec value acc i =
          if i = digits_end
          then acc
          else value ((acc * 16) + hex_value (Bigstringaf.get bs i)) (i + 1)
        in
        Line (Int64.of_int (value 0 digits), p))

module Grammar = struct
  let run head where input =
    let len = String.length input in
    let bs = Bigstringaf.of_string input ~off:0 ~len in
    match head bs len with
    | Line (v, p) when p = len -> Ok v
    | Line (_, p) ->
      Error (Printf.sprintf "unexpected %C after %s" input.[p] where)
    | Partial -> Error "unexpected end of input"
    | Invalid message -> Error message

  let leading_empty_line bs stop p where =
    if get bs stop p = '\r'
    then end_line ~eof:true bs stop p where
    else Line ((), p)

  (* RFC 9112 §2.2: ignore one empty line before the request-line. *)
  let request =
    run
      (fun bs stop ->
         match leading_empty_line bs stop 0 "request-line" with
         | (Partial | Invalid _) as e -> e
         | Line ((), p) ->
           (match request_line ~eof:true bs p stop with
           | (Partial | Invalid _) as e -> e
           | Line ((meth, target, version), p) ->
             (match
                fields
                  ~eof:true
                  ~obs_fold:false
                  ~where:"header section"
                  []
                  bs
                  p
                  stop
              with
             | (Partial | Invalid _) as e -> e
             | Line (headers, p) ->
               Line (Request.create ~version ~headers meth target, p))))
      "header section"

  (* RFC 9112 §2.2: empty lines before a response are discarded. *)
  let response =
    run
      (fun bs stop ->
         let rec skip_empty p =
           match leading_empty_line bs stop p "status-line" with
           | Line ((), q) when q > p -> skip_empty q
           | r -> r
         in
         match skip_empty 0 with
         | (Partial | Invalid _) as e -> e
         | Line ((), p) ->
           (match status_line ~eof:true bs p stop with
           | (Partial | Invalid _) as e -> e
           | Line ((version, status, reason), p) ->
             (match
                fields
                  ~eof:true
                  ~obs_fold:true
                  ~where:"header section"
                  []
                  bs
                  p
                  stop
              with
             | (Partial | Invalid _) as e -> e
             | Line (headers, p) ->
               Line (Response.create ~reason ~version ~headers status, p))))
      "header section"

  let chunk_size =
    run (fun bs stop -> chunk_size_line ~eof:true bs 0 stop) "chunk-size line"

  let trailers =
    run
      (fun bs stop ->
         fields ~eof:true ~obs_fold:false ~where:"trailer section" [] bs 0 stop)
      "trailer section"
end

module Reader = Parse_v1.Reader
