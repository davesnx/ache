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

module Grammar = struct
  let is_space = function ' ' | '\t' -> true | _ -> false
  let is_digit = function '0' .. '9' -> true | _ -> false

  let is_hex = function
    | '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' -> true
    | _ -> false

  (* RFC 9110 §5.6.2 tchar *)
  let is_tchar = function
    | '0' .. '9'
    | 'a' .. 'z'
    | 'A' .. 'Z'
    | '!' | '#' | '$' | '%' | '&' | '\'' | '*' | '+' | '-' | '.' | '^' | '_'
    | '`' | '|' | '~' ->
      true
    | _ -> false

  (* RFC 9110 §5.6.4 qdtext *)
  let is_qdtext = function
    | '\t' | ' ' | '!' | '\x23' .. '\x5b' | '\x5d' .. '\x7e' | '\x80' .. '\xff'
      ->
      true
    | _ -> false

  let is_quoted_pair_char = function
    | '\t' | ' ' | '\x21' .. '\x7e' | '\x80' .. '\xff' -> true
    | _ -> false

  let is_target_char c = c > ' ' && c <> '\x7f'

  let is_field_char = function
    | '\t' | ' ' .. '\x7e' | '\x80' .. '\xff' -> true
    | _ -> false

  (* [Parseff.consume] reads the whole literal before it compares any byte. *)
  let crlf () =
    ignore (Parseff.char '\r');
    ignore (Parseff.char '\n')

  let http_slash () =
    ignore (Parseff.char 'H');
    ignore (Parseff.char 'T');
    ignore (Parseff.char 'T');
    ignore (Parseff.char 'P');
    ignore (Parseff.char '/')

  let eol () = Parseff.expect {|"\r\n"|} crlf
  let digit () = Char.code (Parseff.satisfy is_digit ~label:"digit") - 48

  let version () =
    Parseff.expect {|"HTTP/"|} http_slash;
    let major = digit () in
    ignore (Parseff.char '.');
    let minor = digit () in
    { Version.major; minor }

  let header () =
    let name = Parseff.take_while ~at_least:1 ~label:"field-name" is_tchar in
    ignore (Parseff.char ':');
    Parseff.skip_while is_space;
    let value = Parseff.take_while is_field_char in
    eol ();
    name, String.trim value

  let headers () =
    let rec loop acc =
      match Parseff.peek_char () with
      | Some '\r' -> Headers.of_rev_list acc
      | Some _ -> loop (header () :: acc)
      | None -> Parseff.fail "unexpected end of input"
    in
    loop []

  (* RFC 9112 §2.2: ignore one empty line before the request-line. *)
  let request_head ~on_line () =
    on_line `Method;
    if Parseff.peek_char () = Some '\r' then eol ();
    let meth = Parseff.take_while ~at_least:1 ~label:"method" is_tchar in
    ignore (Parseff.char ' ');
    on_line `Target;
    let target =
      Parseff.take_while ~at_least:1 ~label:"request-target" is_target_char
    in
    ignore (Parseff.char ' ');
    let version = version () in
    eol ();
    on_line `Fields;
    let headers = headers () in
    eol ();
    Request.create ~version ~headers (Method.of_string meth) target

  let status () =
    let code = Parseff.take_while is_digit in
    match String.length code with
    | 0 -> Parseff.fail "status-code empty"
    | 1 | 2 -> Parseff.fail (Printf.sprintf "invalid status-code: %S" code)
    | 3 when code.[0] = '0' ->
      Parseff.fail (Printf.sprintf "invalid status-code: %S" code)
    | 3 -> Status.of_code (int_of_string code)
    | _ -> Parseff.fail (Printf.sprintf "status-code too long: %S" code)

  (* RFC 9112 §5.2: a recipient of a response replaces each obs-fold with SP. *)
  let folded_headers () =
    let rec loop acc =
      match Parseff.peek_char (), acc with
      | Some '\r', _ -> Headers.of_rev_list acc
      | Some (' ' | '\t'), (name, value) :: acc ->
        Parseff.skip_while is_space;
        let more = Parseff.take_while is_field_char in
        eol ();
        loop ((name, String.trim (value ^ " " ^ more)) :: acc)
      | Some _, _ -> loop (header () :: acc)
      | None, _ -> Parseff.fail "unexpected end of input"
    in
    loop []

  (* RFC 9112 §2.2: empty lines before a response are discarded. *)
  let response_head () =
    let rec skip_empty_lines () =
      if Parseff.peek_char () = Some '\r'
      then (
        eol ();
        skip_empty_lines ())
    in
    skip_empty_lines ();
    let version = version () in
    if version.major <> 1
    then
      Parseff.fail
        (Printf.sprintf
           "unsupported HTTP-version: %d.%d"
           version.major
           version.minor);
    ignore (Parseff.char ' ');
    let status = status () in
    ignore (Parseff.char ' ');
    let reason = Parseff.take_while is_field_char in
    eol ();
    let headers = folded_headers () in
    eol ();
    Response.create ~reason ~version ~headers status

  let quoted_string () =
    ignore (Parseff.char '"');
    let rec loop () =
      Parseff.skip_while is_qdtext;
      match Parseff.peek_char () with
      | Some '\\' ->
        ignore (Parseff.char '\\');
        ignore (Parseff.satisfy is_quoted_pair_char ~label:"quoted-pair");
        loop ()
      | _ -> ignore (Parseff.char '"')
    in
    loop ()

  (* RFC 9112 §7.1.1 chunk-ext *)
  let rec chunk_ext () =
    match Parseff.peek_char () with
    | Some (' ' | '\t' | ';') ->
      Parseff.skip_while is_space;
      ignore (Parseff.char ';');
      Parseff.skip_while is_space;
      ignore (Parseff.take_while ~at_least:1 ~label:"chunk-ext-name" is_tchar);
      (match Parseff.peek_char () with
      | Some (' ' | '\t' | '=') ->
        Parseff.skip_while is_space;
        ignore (Parseff.char '=');
        Parseff.skip_while is_space;
        (match Parseff.peek_char () with
        | Some '"' -> quoted_string ()
        | _ ->
          ignore
            (Parseff.take_while ~at_least:1 ~label:"chunk-ext-val" is_tchar))
      | _ -> ());
      chunk_ext ()
    | _ -> ()

  (* 15 hex digits keep the size below 2^60, so it fits an Int64 and an int. *)
  let max_chunk_size_digits = 15

  let chunk_size_line () =
    let start = Parseff.position () in
    Parseff.skip_while (fun c -> c = '0');
    let digits = Parseff.take_while is_hex in
    if Parseff.position () = start
    then ignore (Parseff.take_while ~at_least:1 ~label:"chunk-size" is_hex);
    if String.length digits > max_chunk_size_digits
    then Parseff.fail (Printf.sprintf "chunk-size too long: %S" digits);
    chunk_ext ();
    eol ();
    if digits = "" then 0L else Int64.of_string ("0x" ^ digits)

  let trailer_section () =
    let headers = headers () in
    eol ();
    headers

  let run grammar input =
    match Parseff.parse input grammar with
    | Ok v -> Ok v
    | Error e -> Error (Parseff.error_message e)

  let request = run (request_head ~on_line:ignore)
  let response = run response_head
  let chunk_size = run chunk_size_line
  let trailers = run trailer_section
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

  type _ Effect.t += Need_input : unit Effect.t

  exception Cancelled

  type 'a step =
    | Parsed of ('a * int, Parseff.error) result
    | Suspended of 'a suspended

  and 'a suspended = (unit, 'a step) Effect.Deep.continuation

  type feed =
    { mutable bs : Bigstringaf.t
    ; mutable off : int
    ; mutable len : int
    ; mutable eof : bool
    ; mutable fed : int
    ; mutable hit_limit : bool
    ; mutable hit_eof : bool
    ; limit : int
    }

  type framing =
    | Fixed of int64
    | Chunk_line of int64 suspended option
    | Chunk_data of int64
    | Chunk_crlf of int
    | Trailers of Headers.t suspended option
    | Close_delimited

  type ('head, 'error) state =
    | Done
    | Head of 'head suspended
    | Head_done
    | Body of Body.Reader.t * framing
    | Fail of 'error
    | Cancelled_parse

  type ('head, 'error) t =
    { feed : feed
    ; read : bytes -> int -> int -> int
    ; head : unit -> 'head * int
    ; on_head : ('head, 'error) t -> 'head -> unit
    ; fail : [ `Parse of string | `Header_too_large ] -> 'error
    ; mutable state : ('head, 'error) state
    ; mutable paused : bool
    ; mutable closed : bool
    ; mutable wakeup : Optional_thunk.t
    }

  type request = (Request.t, request_error) t
  type response = (Response.t, response_error) t

  let rec read_feed feed buf off len =
    if feed.fed >= feed.limit
    then (
      feed.hit_limit <- true;
      0)
    else if feed.len > 0
    then (
      let n = min (min len feed.len) (feed.limit - feed.fed) in
      Bigstringaf.blit_to_bytes
        feed.bs
        ~src_off:feed.off
        buf
        ~dst_off:off
        ~len:n;
      feed.off <- feed.off + n;
      feed.len <- feed.len - n;
      feed.fed <- feed.fed + n;
      n)
    else if feed.eof
    then (
      feed.hit_eof <- true;
      0)
    else (
      Effect.perform Need_input;
      read_feed feed buf off len)

  let with_position grammar () =
    let v = grammar () in
    v, Parseff.position ()

  let chunk_size = with_position Grammar.chunk_size_line
  let trailer_section = with_position Grammar.trailer_section

  let handler () =
    { Effect.Deep.retc = (fun r -> Parsed r)
    ; exnc = raise
    ; effc =
        (fun (type a) (e : a Effect.t) ->
          match e with
          | Need_input ->
            Some (fun (k : (a, _) Effect.Deep.continuation) -> Suspended k)
          | _ -> None)
    }

  let cancel k =
    match Effect.Deep.discontinue k Cancelled with
    | _ -> ()
    | exception Cancelled -> ()

  let create ~max_header_size ~head ~on_head ~fail =
    let feed =
      { bs = Bigstringaf.empty
      ; off = 0
      ; len = 0
      ; eof = false
      ; fed = 0
      ; hit_limit = false
      ; hit_eof = false
      ; limit = max_header_size
      }
    in
    { feed
    ; read = read_feed feed
    ; head
    ; on_head
    ; fail
    ; state = Done
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
    t.wakeup <- Optional_thunk.none;
    Optional_thunk.call_if_some f

  let create_body t =
    Body.Reader.create
      ~when_ready_to_read:(Optional_thunk.some (fun () -> wakeup t))

  (* RFC 9110 §7.8, §9.3.6: the connection may stop carrying HTTP/1.1 after the
     response to this request. *)
  let may_switch_protocols (request : Request.t) =
    request.meth = `CONNECT
    || (Headers.mem request.headers "upgrade"
       && Message.has_member
            (Headers.get_multi request.headers "connection")
            "upgrade")

  let request ~max_header_size handler =
    let line = ref `Method in
    let on_head t request =
      t.paused <- may_switch_protocols request;
      match Request.received_body_length request with
      | Error error -> t.state <- Fail (`Invalid_request (request, error))
      | Ok (`Fixed 0L) ->
        handler request (Body.Reader.create_empty ());
        t.state <- Done
      | Ok ((`Fixed _ | `Chunked) as encoding) ->
        let body = create_body t in
        let framing =
          match encoding with
          | `Fixed n -> Fixed n
          | `Chunked -> Chunk_line None
        in
        t.state <- Body (body, framing);
        handler request body
    in
    create
      ~max_header_size
      ~head:(with_position (Grammar.request_head ~on_line:(( := ) line)))
      ~on_head
      ~fail:(function
        | `Header_too_large ->
          (match !line with
          | `Method -> `Method_too_long
          | `Target -> `Target_too_long
          | `Fields -> `Header_too_large)
        | `Parse _ as e -> e)

  let response ~max_header_size request_queue =
    let on_head t response =
      let exception Local of Respd.t option in
      match
        Queue.iter
          (fun respd ->
             if respd.Respd.state = Awaiting_response
             then raise (Local (Some respd))
             else if not (Respd.persistent_connection respd)
             then raise (Local None))
          request_queue
      with
      | () -> t.state <- Fail `Unsolicited_response
      | exception Local None -> t.state <- Cancelled_parse
      | exception Local (Some respd) ->
        let request = Respd.request respd in
        if response.Response.status = `Switching_protocols
        then respd.response_handler response (Body.Reader.create_empty ())
        else if request.meth = `CONNECT && Status.is_successful response.status
        then respd.response_handler response (create_body t)
        else (
          match Response.body_length ~request_method:request.meth response with
          | `No_body when Status.is_informational response.status ->
            t.state <- Done
          | `Error `Bad_gateway -> assert false
          | `Error `Internal_server_error ->
            t.state <- Fail (`Invalid_response_body_length response)
          | `No_body | `Fixed 0L ->
            respd.response_handler response (Body.Reader.create_empty ());
            t.state <- Done
          | (`Fixed _ | `Chunked | `Close_delimited) as encoding ->
            let body = create_body t in
            let framing =
              match encoding with
              | `Fixed n -> Fixed n
              | `Chunked -> Chunk_line None
              | `Close_delimited -> Close_delimited
            in
            t.state <- Body (body, framing);
            respd.response_handler response body)
    in
    create
      ~max_header_size
      ~head:(with_position Grammar.response_head)
      ~on_head
      ~fail:(fun e -> (e :> response_error))

  (* Runs [grammar], or resumes [suspended], on the bytes in [bs]. A parse that
     suspends has taken every byte of [bs]. *)
  let run t grammar suspended bs ~off ~len more =
    let feed = t.feed in
    feed.bs <- bs;
    feed.off <- off;
    feed.len <- len;
    feed.eof <- (match more with Complete -> true | Incomplete -> false);
    match suspended with
    | Some k -> Effect.Deep.continue k ()
    | None ->
      feed.fed <- 0;
      feed.hit_limit <- false;
      feed.hit_eof <- false;
      Effect.Deep.match_with
        (fun () ->
           Parseff.parse_source
             ~backtrack_window:(max 2 feed.limit)
             ~consume:`Partial
             (Parseff.Source.of_function t.read)
             grammar)
        ()
        (handler ())

  let parse_error t (e : Parseff.error) =
    match e.kind with
    | Backtrack_window_exceeded _ -> t.fail `Header_too_large
    | _ when t.feed.hit_limit -> t.fail `Header_too_large
    | _ when t.feed.hit_eof -> t.fail (`Parse "unexpected end of input")
    | _ -> t.fail (`Parse (Parseff.error_message e))

  (* Parses with [grammar] and returns the bytes of [bs] it consumed. A parse
     that ends consumes up to its last byte. A parse that fails consumes all the
     bytes it was given. [on_suspend] returns the bytes consumed by a parse that
     needs more input. *)
  let parse t grammar suspended bs ~off ~len more ~on_suspend ~on_parsed =
    let fed = match suspended with None -> 0 | Some _ -> t.feed.fed in
    match run t grammar suspended bs ~off ~len more with
    | Suspended k -> on_suspend k
    | Parsed (Ok (v, position)) ->
      on_parsed v;
      position - fed
    | Parsed (Error e) ->
      t.state <- Fail (parse_error t e);
      t.feed.fed - fed

  let deliver body bs ~off ~len remaining =
    let n =
      if Int64.compare remaining (Int64.of_int len) < 0
      then Int64.to_int remaining
      else len
    in
    Body.Reader.write body bs ~off ~len:n;
    n

  let finish t body =
    Body.Reader.close body;
    t.state <- Done

  let fail_parse t message = t.state <- Fail (t.fail (`Parse message))

  let head_parsed t head =
    t.state <- Head_done;
    t.on_head t head

  let step t bs ~off ~len more =
    match t.state with
    | Fail _ | Head_done -> 0
    | Cancelled_parse -> len
    | Done when len = 0 || t.paused -> 0
    | Done ->
      parse
        t
        t.head
        None
        bs
        ~off
        ~len
        more
        ~on_suspend:(fun k ->
          t.state <- Head k;
          len)
        ~on_parsed:(head_parsed t)
    | Head _ when len = 0 && more = Incomplete -> 0
    | Head k ->
      t.state <- Done;
      parse
        t
        t.head
        (Some k)
        bs
        ~off
        ~len
        more
        ~on_suspend:(fun k ->
          t.state <- Head k;
          len)
        ~on_parsed:(head_parsed t)
    | Body
        (_, (Chunk_line _ | Chunk_data _ | Chunk_crlf _ | Trailers _ | Fixed _))
      when len = 0 && more = Incomplete ->
      0
    | Body (body, Fixed remaining) ->
      if len = 0
      then (
        fail_parse t "expected more from fixed body";
        0)
      else
        let n = deliver body bs ~off ~len remaining in
        let remaining = Int64.sub remaining (Int64.of_int n) in
        if remaining = 0L
        then finish t body
        else t.state <- Body (body, Fixed remaining);
        n
    | Body (body, Chunk_line None)
      when Bigstringaf.memchr bs off '\n' (min len t.feed.limit) >= 0 ->
      let n = Bigstringaf.memchr bs off '\n' (min len t.feed.limit) - off + 1 in
      (match
         Parseff.parse
           (Bigstringaf.substring bs ~off ~len:n)
           Grammar.chunk_size_line
       with
      | Ok size ->
        t.state <-
          Body (body, if size = 0L then Trailers None else Chunk_data size)
      | Error e -> fail_parse t (Parseff.error_message e));
      n
    | Body (body, Chunk_line suspended) ->
      t.state <- Body (body, Chunk_line None);
      parse
        t
        chunk_size
        suspended
        bs
        ~off
        ~len
        more
        ~on_suspend:(fun k ->
          t.state <- Body (body, Chunk_line (Some k));
          len)
        ~on_parsed:(fun size ->
          t.state <-
            Body (body, if size = 0L then Trailers None else Chunk_data size))
    | Body (body, Chunk_data remaining) ->
      if len = 0
      then (
        fail_parse t "expected more from body chunk";
        0)
      else
        let n = deliver body bs ~off ~len remaining in
        let remaining = Int64.sub remaining (Int64.of_int n) in
        t.state <-
          Body
            (body, if remaining = 0L then Chunk_crlf 0 else Chunk_data remaining);
        n
    | Body (body, Chunk_crlf seen) ->
      if len = 0
      then (
        fail_parse t "expected more from body chunk";
        0)
      else
        let expected = if seen = 0 then '\r' else '\n' in
        if Bigstringaf.get bs off <> expected
        then (
          fail_parse t "expected CRLF after chunk data";
          0)
        else (
          t.state <-
            Body (body, if seen = 0 then Chunk_crlf 1 else Chunk_line None);
          1)
    | Body (body, Trailers None)
      when len >= 2
           && Bigstringaf.get bs off = '\r'
           && Bigstringaf.get bs (off + 1) = '\n' ->
      finish t body;
      2
    | Body (body, Trailers suspended) ->
      t.state <- Body (body, Trailers None);
      parse
        t
        trailer_section
        suspended
        bs
        ~off
        ~len
        more
        ~on_suspend:(fun k ->
          t.state <- Body (body, Trailers (Some k));
          len)
        ~on_parsed:(fun (_ : Headers.t) -> finish t body)
    | Body (body, Close_delimited) ->
      if len > 0 then ignore (deliver body bs ~off ~len Int64.max_int : int);
      (match more with Complete -> finish t body | Incomplete -> ());
      len

  let in_message = function
    | Head _ | Body _ -> true
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
    | Head k ->
      t.state <- Cancelled_parse;
      cancel k
    | Body (_, Chunk_line (Some k)) ->
      t.state <- Cancelled_parse;
      cancel k
    | Body (_, Trailers (Some k)) ->
      t.state <- Cancelled_parse;
      cancel k
    | Done | Head_done | Body _ | Fail _ | Cancelled_parse -> ()

  let next t =
    match t.state with
    | Fail failure -> `Error failure
    | _ when t.closed -> `Close
    | Done -> `Start
    | Head _ | Head_done | Body _ | Cancelled_parse -> `Read
end
