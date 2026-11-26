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

type more =
  | Complete
  | Incomplete

let partial = -1
let invalid = -2

type out =
  { mutable meth : Method.t
  ; mutable text : string
  ; mutable version : Version.t
  ; mutable status : Status.t
  ; mutable fields : (string * string) list
  ; mutable folds : string list
  ; mutable size : int
  ; mutable error : string
  }

let out () =
  { meth = `GET
  ; text = ""
  ; version = Version.v1_1
  ; status = `OK
  ; fields = []
  ; folds = []
  ; size = 0
  ; error = ""
  }

let join_folds o =
  match o.fields with
  | (name, first) :: rest ->
    let parts = List.filter (fun s -> s <> "") (first :: List.rev o.folds) in
    o.fields <- (name, String.concat " " parts) :: rest;
    o.folds <- []
  | [] -> o.folds <- []

let[@inline] close_fold o =
  match o.folds with [] -> () | _ :: _ -> join_folds o

let request_of o =
  close_fold o;
  { Request.meth = o.meth
  ; target = o.text
  ; version = o.version
  ; headers = Headers.of_rev_list o.fields
  }

let response_of o =
  close_fold o;
  { Response.version = o.version
  ; status = o.status
  ; reason = o.text
  ; headers = Headers.of_rev_list o.fields
  }

let table f =
  String.init 256 (fun i -> if f (Char.chr i) then '\001' else '\000')

let is table c = String.unsafe_get table (Char.code c) = '\001'

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

external bytes_set64 : bytes -> int -> int64 -> unit = "%caml_bytes_set64u"

let[@inline] substring bs ~off ~len =
  if len < 0 || off < 0 || Bigstring.length bs - off < len
  then invalid_arg "Parse.substring";
  let b = Bytes.create len in
  if len <= 64
  then (
    let i = ref 0 in
    while !i + 8 <= len do
      bytes_set64 b !i (Bigstring.unsafe_get_int64_ne bs (off + !i));
      i := !i + 8
    done;
    while !i < len do
      Bytes.unsafe_set b !i (Bigstring.unsafe_get bs (off + !i));
      incr i
    done)
  else Bigstring.unsafe_blit_to_bytes bs ~src_off:off b ~dst_off:0 ~len;
  Bytes.unsafe_to_string b

(* Past [stop] every byte reads as NUL, which no rule accepts. *)
let get bs stop i = if i < stop then Bigstring.unsafe_get bs i else '\000'

let rec skip table bs stop i =
  if i < stop && is table (Bigstring.unsafe_get bs i)
  then skip table bs stop (i + 1)
  else i

let[@inline] zero_bytes x =
  Int64.logand
    (Int64.logand (Int64.sub x 0x0101010101010101L) (Int64.lognot x))
    0x8080808080808080L

(* Index of the lowest byte whose high bit is set in [m]. *)
let[@inline] first_mark m =
  let low = Int64.shift_right_logical (Int64.logand m (Int64.neg m)) 7 in
  Int64.to_int
    (Int64.shift_right_logical (Int64.mul low 0x0001020304050607L) 56)

(* [skip table] for a table that accepts every byte from [below] up, except DEL,
   and possibly some bytes under [below]. Eight bytes at a time: a word with no
   byte under [below] and no DEL is accepted whole; a flagged byte is looked up
   in [table]. [below] is the byte repeated in each lane, at most 0x80. *)
let[@inline] skip_word ~below table bs stop i =
  let i = ref i
  and found = ref (-1) in
  if not Sys.big_endian
  then
    while !found < 0 && !i + 8 <= stop do
      let x = Bigstring.unsafe_get_int64_ne bs !i in
      let m =
        Int64.logor
          (Int64.logand
             (Int64.logand (Int64.sub x below) (Int64.lognot x))
             0x8080808080808080L)
          (zero_bytes (Int64.logxor x 0x7f7f7f7f7f7f7f7fL))
      in
      if m = 0L
      then i := !i + 8
      else
        let m = ref m in
        while !found < 0 && !m <> 0L do
          let j = !i + first_mark !m in
          if is table (Bigstring.unsafe_get bs j)
          then m := Int64.logand !m (Int64.sub !m 1L)
          else found := j
        done;
        if !found < 0 then i := !i + 8
    done;
  if !found >= 0 then !found else skip table bs stop !i

(* field-char: HTAB, SP to 0x7e, 0x80 to 0xff. *)
let skip_field bs stop i =
  skip_word ~below:0x2020202020202020L field_char bs stop i

(* Request target: 0x21 to 0x7e, 0x80 to 0xff. *)
let skip_target bs stop i =
  skip_word ~below:0x2121212121212121L target_char bs stop i

let rec skip_zeros bs stop i =
  if i < stop && Bigstring.unsafe_get bs i = '0'
  then skip_zeros bs stop (i + 1)
  else i

let fail o message =
  o.error <- message;
  invalid

let fail_at ~eof o stop p message =
  if p >= stop && not eof then partial else fail o message

let unexpected ~eof o bs stop p where =
  if p < stop
  then fail o (Printf.sprintf "unexpected %C in %s" (Bigstring.get bs p) where)
  else if eof
  then fail o "unexpected end of input"
  else partial

let end_line ~eof o bs stop p where =
  if get bs stop p <> '\r'
  then unexpected ~eof o bs stop p where
  else if get bs stop (p + 1) <> '\n'
  then unexpected ~eof o bs stop (p + 1) where
  else p + 2

let rec http_slash ~eof o bs stop p i where =
  if i = 5
  then p + 5
  else if get bs stop (p + i) <> String.unsafe_get "HTTP/" i
  then unexpected ~eof o bs stop (p + i) where
  else http_slash ~eof o bs stop p (i + 1) where

let http = if Sys.big_endian then 0x48545450l else 0x50545448l

let version ~eof o bs stop p where =
  let p =
    if
      p + 5 <= stop
      && Int32.equal (Bigstring.unsafe_get_int32_ne bs p) http
      && Bigstring.unsafe_get bs (p + 4) = '/'
    then p + 5
    else http_slash ~eof o bs stop p 0 where
  in
  if p < 0
  then p
  else
    let major = get bs stop p
    and minor = get bs stop (p + 2) in
    if not (is digit major)
    then unexpected ~eof o bs stop p where
    else if get bs stop (p + 1) <> '.'
    then unexpected ~eof o bs stop (p + 1) where
    else if not (is digit minor)
    then unexpected ~eof o bs stop (p + 2) where
    else (
      o.version <-
        (match major, minor with
        | '1', '1' -> Version.v1_1
        | '1', '0' -> Version.v1_0
        | _ ->
          { Version.major = Char.code major - 48; minor = Char.code minor - 48 });
      p + 3)

let rec same bs off s i =
  i = String.length s
  || (Bigstring.unsafe_get bs (off + i) = String.unsafe_get s i
     && same bs off s (i + 1))

let literal bs off len s = String.length s = len && same bs off s 0

let methods =
  [ "GET", `GET
  ; "POST", `POST
  ; "HEAD", `HEAD
  ; "PUT", `PUT
  ; "DELETE", `DELETE
  ; "CONNECT", `CONNECT
  ; "OPTIONS", `OPTIONS
  ; "TRACE", `TRACE
  ]

let rec meth bs off len = function
  | [] -> Method.of_string (substring bs ~off ~len)
  | (s, m) :: rest -> if literal bs off len s then m else meth bs off len rest

let request_line ~eof o bs off stop =
  let where = "request-line" in
  let meth_end = skip tchar bs stop off in
  if meth_end = off || get bs stop meth_end <> ' '
  then unexpected ~eof o bs stop meth_end where
  else
    let target_end = skip_target bs stop (meth_end + 1) in
    if target_end = meth_end + 1 || get bs stop target_end <> ' '
    then unexpected ~eof o bs stop target_end where
    else
      let p = version ~eof o bs stop (target_end + 1) where in
      let p = if p < 0 then p else end_line ~eof o bs stop p where in
      if p >= 0
      then (
        o.meth <- meth bs off (meth_end - off) methods;
        o.text <-
          substring bs ~off:(meth_end + 1) ~len:(target_end - meth_end - 1));
      p

let digit_at bs i = Char.code (Bigstring.unsafe_get bs i) - 48

let status_line ~eof o bs off stop =
  let where = "status-line" in
  let p = version ~eof o bs stop off where in
  if p < 0
  then p
  else if o.version.major <> 1
  then
    fail
      o
      (Printf.sprintf
         "unsupported HTTP-version: %d.%d"
         o.version.major
         o.version.minor)
  else if get bs stop p <> ' '
  then unexpected ~eof o bs stop p where
  else
    let code_end = skip digit bs stop (p + 1) in
    let n = code_end - p - 1 in
    if n = 0
    then fail_at ~eof o stop code_end "status-code empty"
    else if n <> 3 || Bigstring.unsafe_get bs (p + 1) = '0'
    then
      fail_at
        ~eof
        o
        stop
        code_end
        (Printf.sprintf
           (if n > 3
            then "status-code too long: %S"
            else "invalid status-code: %S")
           (substring bs ~off:(p + 1) ~len:n))
    else if get bs stop code_end <> ' '
    then unexpected ~eof o bs stop code_end where
    else
      let reason_end = skip_field bs stop (code_end + 1) in
      let q = end_line ~eof o bs stop reason_end where in
      if q >= 0
      then (
        o.status <-
          Status.of_code
            ((100 * digit_at bs (p + 1))
            + (10 * digit_at bs (p + 2))
            + digit_at bs (p + 3));
        o.text <-
          substring bs ~off:(code_end + 1) ~len:(reason_end - code_end - 1));
      q

let rec trim_end bs off i =
  if i > off && is space (Bigstring.unsafe_get bs (i - 1))
  then trim_end bs off (i - 1)
  else i

(* RFC 9112 §5.2: a recipient of a response replaces each obs-fold with SP. *)
let field_line ~eof ~obs_fold o bs off stop =
  let where = "field line" in
  let fold =
    obs_fold
    && (match o.fields with [] -> false | _ :: _ -> true)
    && is space (get bs stop off)
  in
  let name_end = if fold then off else skip tchar bs stop off in
  if (not fold) && (name_end = off || get bs stop name_end <> ':')
  then unexpected ~eof o bs stop name_end where
  else
    let start = skip space bs stop (if fold then off else name_end + 1) in
    let value_end = skip_field bs stop start in
    let p = end_line ~eof o bs stop value_end where in
    (if p >= 0
     then
       let value =
         substring bs ~off:start ~len:(trim_end bs start value_end - start)
       in
       if fold
       then o.folds <- value :: o.folds
       else (
         close_fold o;
         o.fields <-
           (substring bs ~off ~len:(name_end - off), value) :: o.fields));
    p

let rec fields ~eof ~obs_fold o bs p stop where =
  if get bs stop p = '\r'
  then end_line ~eof o bs stop p where
  else
    let p = field_line ~eof ~obs_fold o bs p stop in
    if p < 0 then p else fields ~eof ~obs_fold o bs p stop where

let rec quoted_string ~eof o bs stop p where =
  let p = skip qdtext bs stop p in
  match get bs stop p with
  | '"' -> p + 1
  | '\\' ->
    if is quoted_pair_char (get bs stop (p + 1))
    then quoted_string ~eof o bs stop (p + 2) where
    else unexpected ~eof o bs stop (p + 1) where
  | _ -> unexpected ~eof o bs stop p where

(* RFC 9112 §7.1.1 chunk-ext *)
let rec chunk_ext ~eof o bs stop p where =
  match get bs stop p with
  | ' ' | '\t' | ';' ->
    let p = skip space bs stop p in
    if get bs stop p <> ';'
    then unexpected ~eof o bs stop p where
    else
      let name = skip space bs stop (p + 1) in
      let name_end = skip tchar bs stop name in
      if name_end = name
      then unexpected ~eof o bs stop name where
      else (
        match get bs stop name_end with
        | ' ' | '\t' | '=' ->
          let p = skip space bs stop name_end in
          if get bs stop p <> '='
          then unexpected ~eof o bs stop p where
          else
            let value = skip space bs stop (p + 1) in
            if get bs stop value = '"'
            then
              let p = quoted_string ~eof o bs stop (value + 1) where in
              if p < 0 then p else chunk_ext ~eof o bs stop p where
            else
              let value_end = skip tchar bs stop value in
              if value_end = value
              then unexpected ~eof o bs stop value where
              else chunk_ext ~eof o bs stop value_end where
        | _ -> chunk_ext ~eof o bs stop name_end where)
  | _ -> p

(* 15 hex digits keep the size below 2^60, so it fits an Int64 and an int. *)
let max_chunk_size_digits = 15

let hex_value c =
  match c with
  | '0' .. '9' -> Char.code c - 48
  | 'a' .. 'f' -> Char.code c - 87
  | _ -> Char.code c - 55

let rec hex_number bs acc i stop =
  if i = stop
  then acc
  else
    hex_number
      bs
      ((acc * 16) + hex_value (Bigstring.unsafe_get bs i))
      (i + 1)
      stop

let chunk_size_line ~eof o bs off stop =
  let where = "chunk-size line" in
  let digits = skip_zeros bs stop off in
  let digits_end = skip hex bs stop digits in
  if digits_end = off
  then unexpected ~eof o bs stop off where
  else if digits_end - digits > max_chunk_size_digits
  then
    fail_at
      ~eof
      o
      stop
      digits_end
      (Printf.sprintf
         "chunk-size too long: %S"
         (substring bs ~off:digits ~len:(digits_end - digits)))
  else
    let p = chunk_ext ~eof o bs stop digits_end where in
    let p = if p < 0 then p else end_line ~eof o bs stop p where in
    if p >= 0 then o.size <- hex_number bs 0 digits digits_end;
    p

module Grammar = struct
  let run parse value where input =
    let len = String.length input in
    let bs = Bigstring.of_string input ~off:0 ~len in
    let o = out () in
    let p = parse o bs len in
    if p = len
    then Ok (value o)
    else if p >= 0
    then Error (Printf.sprintf "unexpected %C after %s" input.[p] where)
    else Error o.error

  let empty_line o bs stop p where =
    if get bs stop p = '\r' then end_line ~eof:true o bs stop p where else p

  (* RFC 9112 §2.2: ignore one empty line before the request-line. *)
  let request =
    run
      (fun o bs stop ->
         let p = empty_line o bs stop 0 "request-line" in
         let p = if p < 0 then p else request_line ~eof:true o bs p stop in
         if p < 0
         then p
         else fields ~eof:true ~obs_fold:false o bs p stop "header section")
      request_of
      "header section"

  (* RFC 9112 §2.2: empty lines before a response are discarded. *)
  let rec empty_lines o bs stop p =
    let q = empty_line o bs stop p "status-line" in
    if q > p then empty_lines o bs stop q else q

  let response =
    run
      (fun o bs stop ->
         let p = empty_lines o bs stop 0 in
         let p = if p < 0 then p else status_line ~eof:true o bs p stop in
         if p < 0
         then p
         else fields ~eof:true ~obs_fold:true o bs p stop "header section")
      response_of
      "header section"

  let chunk_size =
    run
      (fun o bs stop -> chunk_size_line ~eof:true o bs 0 stop)
      (fun o -> Int64.of_int o.size)
      "chunk-size line"

  let trailers =
    run
      (fun o bs stop ->
         fields ~eof:true ~obs_fold:false o bs 0 stop "trailer section")
      (fun o -> Headers.of_rev_list o.fields)
      "trailer section"
end

module Reader = struct
  type request_error =
    [ `Invalid_request of Request.t * Request.error
    | `Parse of string
    | `Method_too_long
    | `Target_too_long
    | `Header_too_large
    ]

  type response_error =
    [ `Invalid_response_body_length of Response.t
    | `Parse of string
    | `Header_too_large
    | `Unsolicited_response
    ]

  type framing =
    | Fixed of int64
    | Chunk_line
    | Chunk_data of int64
    | Chunk_crlf of int
    | Trailers
    | Close_delimited

  type ('head, 'error) state =
    | Done
    | Head
    | Head_done
    | Body of Body.Reader.t * framing
    | Fail of 'error
    | Cancelled_parse

  type stage =
    | Leading
    | First
    | Fields
    | Trailer
    | Chunk_size
    | Ended

  type ('head, 'error) t =
    { limit : int
    ; request : bool
    ; out : out
    ; on_head : ('head, 'error) t -> unit
    ; fail :
        [ `Parse of string
        | `Header_too_large
        | `Method_too_long
        | `Target_too_long
        ]
        -> 'error
    ; mutable state : ('head, 'error) state
    ; mutable stage : stage
    ; mutable fed : int
    ; mutable buffer : Bigstring.t
    ; mutable buffered : int
    ; mutable paused : bool
    ; mutable closed : bool
    ; mutable wakeup : Optional_thunk.t
    }

  type request = (Request.t, request_error) t
  type response = (Response.t, response_error) t

  let create ~max_header_size ~request ~on_head ~fail =
    { limit = Int.max 0 max_header_size
    ; request
    ; out = out ()
    ; on_head
    ; fail
    ; state = Done
    ; stage = Ended
    ; fed = 0
    ; buffer = Bigstring.empty
    ; buffered = 0
    ; paused = false
    ; closed = false
    ; wakeup = Optional_thunk.none
    }

  let is_closed t = t.closed
  let resume t = t.paused <- false
  let pause t = t.paused <- true

  let on_wakeup t k =
    if is_closed t
    then failwith "on_wakeup on closed reader"
    else if Optional_thunk.is_some t.wakeup
    then failwith "on_wakeup: only one callback can be registered at a time"
    else t.wakeup <- Optional_thunk.some k

  let wakeup t =
    let f = t.wakeup in
    if Optional_thunk.is_some f
    then (
      t.wakeup <- Optional_thunk.none;
      Optional_thunk.call_if_some f)

  let create_body t =
    Body.Reader.create
      ~when_ready_to_read:(Optional_thunk.some (fun () -> wakeup t))

  let chunk_line t body =
    t.stage <- Chunk_size;
    Body (body, Chunk_line)

  (* RFC 9110 §7.8, §9.3.6: the connection may stop carrying HTTP/1.1 after the
     response to this request. *)
  let may_switch_protocols (request : Request.t) =
    request.meth = `CONNECT
    || (Headers.mem request.headers "upgrade"
       && Message.has_member
            (Headers.get_multi request.headers "connection")
            "upgrade")

  let request ~max_header_size handler =
    let on_head t =
      let request = request_of t.out in
      t.out.fields <- [];
      t.paused <- may_switch_protocols request;
      match Request.received_body_length request with
      | Error error -> t.state <- Fail (`Invalid_request (request, error))
      | Ok (`Fixed 0L) ->
        handler request (Body.Reader.create_empty ());
        t.state <- Done
      | Ok ((`Fixed _ | `Chunked) as encoding) ->
        let body = create_body t in
        t.state <-
          (match encoding with
          | `Fixed n -> Body (body, Fixed n)
          | `Chunked -> chunk_line t body);
        handler request body
    in
    create ~max_header_size ~request:true ~on_head ~fail:(fun e ->
      (e :> request_error))

  let response ~max_header_size request_queue =
    let on_head t =
      let response = response_of t.out in
      t.out.fields <- [];
      let exception Local of Response_descriptor.t option in
      match
        Queue.iter
          (fun respd ->
             if respd.Response_descriptor.state = Awaiting_response
             then raise (Local (Some respd))
             else if not (Response_descriptor.persistent_connection respd)
             then raise (Local None))
          request_queue
      with
      | () -> t.state <- Fail `Unsolicited_response
      | exception Local None -> t.state <- Cancelled_parse
      | exception Local (Some respd) ->
        let request = Response_descriptor.request respd in
        if response.Response.status = `Switching_protocols
        then
          Response_descriptor.handle_response
            respd
            response
            (Body.Reader.create_empty ())
        else if request.meth = `CONNECT && Status.is_successful response.status
        then Response_descriptor.handle_response respd response (create_body t)
        else (
          match Response.body_length ~request_method:request.meth response with
          | `No_body when Status.is_informational response.status ->
            t.state <- Done
          | `Error `Bad_gateway -> assert false
          | `Error `Internal_server_error ->
            t.state <- Fail (`Invalid_response_body_length response)
          | `No_body | `Fixed 0L ->
            Response_descriptor.handle_response
              respd
              response
              (Body.Reader.create_empty ());
            t.state <- Done
          | (`Fixed _ | `Chunked | `Close_delimited) as encoding ->
            let body = create_body t in
            t.state <-
              (match encoding with
              | `Fixed n -> Body (body, Fixed n)
              | `Chunked -> chunk_line t body
              | `Close_delimited -> Body (body, Close_delimited));
            Response_descriptor.handle_response respd response body)
    in
    create ~max_header_size ~request:false ~on_head ~fail:(function
      | (`Parse _ | `Header_too_large) as e -> e
      | `Method_too_long | `Target_too_long -> `Header_too_large)

  let line t bs p stop =
    let o = t.out in
    match t.stage with
    | Leading when get bs stop p = '\r' ->
      let where = if t.request then "request-line" else "status-line" in
      let q = end_line ~eof:false o bs stop p where in
      if q >= 0 && t.request then t.stage <- First;
      q
    | Leading | First ->
      let q =
        if t.request
        then request_line ~eof:false o bs p stop
        else status_line ~eof:false o bs p stop
      in
      if q >= 0 then t.stage <- Fields;
      q
    | (Fields | Trailer) when get bs stop p = '\r' ->
      let where =
        if t.stage = Fields then "header section" else "trailer section"
      in
      let q = end_line ~eof:false o bs stop p where in
      if q >= 0 then t.stage <- Ended;
      q
    | Fields -> field_line ~eof:false ~obs_fold:(not t.request) o bs p stop
    | Trailer -> field_line ~eof:false ~obs_fold:false o bs p stop
    | Chunk_size ->
      let q = chunk_size_line ~eof:false o bs p stop in
      if q >= 0 then t.stage <- Ended;
      q
    | Ended -> p

  let rec lines t bs p stop =
    match t.stage with
    | Ended -> p
    | _ ->
      let q = line t bs p stop in
      if q = partial then p else if q < 0 then q else lines t bs q stop

  let save t bs off len =
    let size = t.buffered + len in
    if size > Bigstring.length t.buffer
    then (
      let buffer =
        Bigstring.create (Int.max 256 (Int.max size (2 * t.buffered)))
      in
      Bigstring.blit t.buffer ~src_off:0 buffer ~dst_off:0 ~len:t.buffered;
      t.buffer <- buffer);
    Bigstring.blit bs ~src_off:off t.buffer ~dst_off:t.buffered ~len;
    t.buffered <- size

  let drop t =
    t.fed <- 0;
    t.buffered <- 0;
    if Bigstring.length t.buffer > 4096 then t.buffer <- Bigstring.empty

  let finish t body =
    Body.Reader.close body;
    t.state <- Done

  let ended t =
    drop t;
    match t.state with
    | Head ->
      t.state <- Head_done;
      t.on_head t
    | Body (body, Chunk_line) ->
      if t.out.size = 0
      then (
        t.stage <- Trailer;
        t.state <- Body (body, Trailers))
      else t.state <- Body (body, Chunk_data (Int64.of_int t.out.size))
    | Body (body, Trailers) ->
      t.out.fields <- [];
      finish t body
    | Done | Head_done | Body _ | Fail _ | Cancelled_parse -> ()

  let failed t error =
    t.buffer <- Bigstring.empty;
    t.state <- Fail (t.fail error)

  let too_large t =
    match t.stage with
    | (Leading | First) when t.request ->
      let meth_end = skip tchar t.buffer t.buffered 0 in
      if get t.buffer t.buffered meth_end = ' '
      then `Target_too_long
      else `Method_too_long
    | _ -> `Header_too_large

  let unfinished t bs ~off ~take more p =
    save t bs p (off + take - p);
    t.fed <- t.fed + take;
    if t.fed < t.limit && more = Incomplete
    then take
    else
      let q = line t t.buffer 0 t.buffered in
      failed
        t
        (if q <> partial
         then `Parse t.out.error
         else if t.fed >= t.limit
         then too_large t
         else `Parse "unexpected end of input");
      take

  let parse_lines t bs ~off ~take more p =
    let q = lines t bs p (off + take) in
    if q < 0
    then (
      let n =
        match t.stage with
        | Chunk_size when t.fed = 0 ->
          let nl = Bigstring.memchr bs off '\n' take in
          if nl < 0 then take else nl + 1 - off
        | _ -> take
      in
      failed t (`Parse t.out.error);
      n)
    else
      match t.stage with
      | Ended ->
        ended t;
        q - off
      | _ -> unfinished t bs ~off ~take more q

  let section t bs ~off ~len more =
    if off < 0 || len < 0 || off > Bigstring.length bs - len
    then invalid_arg "Ache: read range outside the buffer";
    let take = Int.min len (t.limit - t.fed) in
    if t.buffered = 0
    then parse_lines t bs ~off ~take more off
    else
      let nl = Bigstring.memchr bs off '\n' take in
      if nl < 0
      then unfinished t bs ~off ~take more off
      else (
        save t bs off (nl + 1 - off);
        if line t t.buffer 0 t.buffered < 0
        then (
          failed t (`Parse t.out.error);
          take)
        else (
          t.buffered <- 0;
          parse_lines t bs ~off ~take more (nl + 1)))

  let deliver body bs ~off ~len remaining =
    let n =
      if Int64.compare remaining (Int64.of_int len) < 0
      then Int64.to_int remaining
      else len
    in
    Body.Reader.write body bs ~off ~len:n;
    n

  let step t bs ~off ~len more =
    match t.state with
    | Fail _ | Head_done -> 0
    | Cancelled_parse -> len
    | Done when len = 0 || t.paused -> 0
    | Done ->
      t.state <- Head;
      t.stage <- Leading;
      section t bs ~off ~len more
    | Head
    | Body (_, (Chunk_line | Chunk_data _ | Chunk_crlf _ | Trailers | Fixed _))
      when len = 0 && more = Incomplete ->
      0
    | Head | Body (_, (Chunk_line | Trailers)) -> section t bs ~off ~len more
    | Body (_, ((Fixed _ | Chunk_data _ | Chunk_crlf _) as framing))
      when len = 0 ->
      failed
        t
        (`Parse
            (match framing with
            | Fixed _ -> "expected more from fixed body"
            | _ -> "expected more from body chunk"));
      0
    | Body (body, ((Fixed remaining | Chunk_data remaining) as framing)) ->
      let n = deliver body bs ~off ~len remaining in
      let remaining = Int64.sub remaining (Int64.of_int n) in
      (match framing with
      | Fixed _ when remaining = 0L -> finish t body
      | Fixed _ -> t.state <- Body (body, Fixed remaining)
      | _ when remaining = 0L -> t.state <- Body (body, Chunk_crlf 0)
      | _ -> t.state <- Body (body, Chunk_data remaining));
      n
    | Body (body, Chunk_crlf seen) ->
      if Bigstring.get bs off <> if seen = 0 then '\r' else '\n'
      then (
        failed t (`Parse "expected CRLF after chunk data");
        0)
      else (
        t.state <-
          (if seen = 0 then Body (body, Chunk_crlf 1) else chunk_line t body);
        1)
    | Body (body, Close_delimited) ->
      if len > 0 then ignore (deliver body bs ~off ~len Int64.max_int : int);
      (match more with Complete -> finish t body | Incomplete -> ());
      len

  let in_message = function
    | Head | Body _ -> true
    | Done | Head_done | Fail _ | Cancelled_parse -> false

  let rec read_loop t bs ~off ~len more consumed =
    let previous = t.state in
    let n = step t bs ~off ~len more in
    let consumed = consumed + n in
    let len = len - n in
    if
      (n > 0 || t.state != previous)
      && (len > 0 || (more = Complete && in_message t.state))
    then read_loop t bs ~off:(off + n) ~len more consumed
    else consumed

  let read_with_more t bs ~off ~len more =
    let consumed = read_loop t bs ~off ~len more 0 in
    (match more with Complete -> t.closed <- true | Incomplete -> ());
    consumed

  let force_close t =
    t.closed <- true;
    match t.state with
    | (Head | Body (_, (Chunk_line | Trailers))) when t.fed > 0 ->
      t.state <- Cancelled_parse;
      t.buffer <- Bigstring.empty
    | Done | Head | Head_done | Body _ | Fail _ | Cancelled_parse -> ()

  let next t =
    match t.state with
    | Fail failure -> `Error failure
    | _ when t.closed -> `Close
    | Done -> `Start
    | Head | Head_done | Body _ | Cancelled_parse -> `Read
end
