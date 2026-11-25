open Oracle

let data = "oracle/oracle.data"

let ache_messages =
  [ "chunk-size too long: "
  ; "invalid status-code: "
  ; "status-code empty"
  ; "status-code too long: "
  ; "unsupported HTTP-version: "
  ; "unexpected end of input"
  ; "expected more from "
  ; "expected CRLF after chunk data"
  ]

let is_ache message =
  List.exists (fun prefix -> String.starts_with ~prefix message) ache_messages

let utf8_first_byte code =
  Char.chr
    (if code < 0x80
     then code
     else if code < 0x800
     then 0xc0 lor (code lsr 6)
     else if code < 0x10000
     then 0xe0 lor (code lsr 12)
     else 0xf0 lor (code lsr 18))

let found_token message =
  let key = ", found " in
  let rec find i =
    if i < 0
    then None
    else if String.sub message i (String.length key) = key
    then
      let at = i + String.length key in
      Some (String.sub message at (String.length message - at))
    else find (i - 1)
  in
  find (String.length message - String.length key)

let first_byte token =
  if String.length token < 3 || token.[0] <> '\''
  then None
  else if token.[1] <> '\\' || String.length token = 3
  then Some token.[1]
  else
    match token.[2] with
    | 'x' -> Some (Char.chr (int_of_string ("0x" ^ String.sub token 3 2)))
    | 'u' ->
      let close = String.index token '}' in
      Some
        (utf8_first_byte
           (int_of_string ("0x" ^ String.sub token 4 (close - 4))))
    | 'n' -> Some '\n'
    | 'r' -> Some '\r'
    | 't' -> Some '\t'
    | c -> Some c

let unexpected_byte message =
  match Scanf.sscanf message "unexpected %C" Fun.id with
  | c -> Some c
  | exception (Scanf.Scan_failure _ | Failure _ | End_of_file) -> None

let same_message expected actual =
  expected = actual
  || ((not (is_ache expected))
     &&
     match found_token expected with
     | Some "end of input" -> actual = "unexpected end of input"
     | Some token ->
       (match first_byte token, unexpected_byte actual with
       | Some e, Some a -> e = a
       | _ -> false)
     | None -> false)

let same_result expected actual =
  match expected, actual with
  | Ok e, Ok a -> e = a
  | Error e, Error a -> same_message e a
  | _ -> false

let same_next expected actual =
  match expected, actual with
  | Failed ("parse", e), Failed ("parse", a) -> same_message e a
  | e, a -> e = a

let same_snapshot e a =
  same_next e.next a.next && e.left = a.left && e.heads = a.heads

(* A bad byte in a line that began in an earlier read is found when the line
   ends or at EOF; v1 found it at the byte. When the second read neither starts
   nor ends a line, v1 rejects before EOF and this reader at EOF, with the same
   error and the same bytes consumed. *)
let rejected_at_eof input k e a =
  k > 0
  && input.[k - 1] <> '\n'
  && (not (String.contains_from input k '\n'))
  && (match e.pre.next, a.pre.next with
    | Failed ("parse", _), Read -> same_next e.pre.next a.post.next
    | _ -> false)
  && e.pre.left = a.pre.left
  && e.pre.heads = a.pre.heads

let same_outcome input k e a =
  same_snapshot e.mid a.mid
  && (same_snapshot e.pre a.pre || rejected_at_eof input k e a)
  && same_snapshot e.post a.post
  && e.messages = a.messages

let cases = lazy (load data)

module Parse_oracle = Run (Ache.Ache_private.Parse)

(* Decision 0007: a request-target byte outside RFC 3986, which v1 accepted. *)
let rfc3986_target_char = function
  | 'a' .. 'z'
  | 'A' .. 'Z'
  | '0' .. '9'
  | '-' | '.' | '_' | '~' | ':' | '/' | '?' | '[' | ']' | '@' | '!' | '$' | '&'
  | '\'' | '(' | ')' | '*' | '+' | ',' | ';' | '=' | '%' ->
    true
  | _ -> false

let drop_empty_line s =
  if String.starts_with ~prefix:"\r\n" s
  then String.sub s 2 (String.length s - 2)
  else s

let rejected_target input =
  let s = drop_empty_line input in
  match String.index_opt s ' ' with
  | None -> false
  | Some i ->
    let rec go j =
      j < String.length s
      && s.[j] > ' '
      && s.[j] <> '\x7f'
      && ((not (rfc3986_target_char s.[j])) || go (j + 1))
    in
    go (i + 1)

(* Task 13: a status-line with CRLF right after the status code, which v1
   rejected. *)
let rec drop_empty_lines s =
  let t = drop_empty_line s in
  if t == s then s else drop_empty_lines t

let reasonless_status_line input =
  let s = drop_empty_lines input in
  String.length s >= 14
  && String.sub s 0 5 = "HTTP/"
  && s.[8] = ' '
  && String.for_all
       (function '0' .. '9' -> true | _ -> false)
       (String.sub s 9 3)
  && String.sub s 12 2 = "\r\n"

(* A CR right after a status code: v1 rejected at the CR, the reader now waits
   for the next byte. *)
let cr_after_status_code input =
  let n = String.length input in
  let rec from i =
    i + 13 <= n
    && ((String.sub input i 5 = "HTTP/"
        && input.[i + 8] = ' '
        && String.for_all
             (function '0' .. '9' -> true | _ -> false)
             (String.sub input (i + 9) 3)
        && input.[i + 12] = '\r')
       || from (i + 1))
  in
  from 0

let changed_on_purpose c =
  rejected_target c.input || cr_after_status_code c.input

let test_changed_on_purpose () =
  let changed = List.filter changed_on_purpose (Lazy.force cases) in
  List.iter
    (fun c ->
       let g = Parse_oracle.grammar c.input in
       if rejected_target c.input
       then
         Alcotest.(check bool)
           (Printf.sprintf "%S: request rejected" c.input)
           true
           (Result.is_error g.request);
       if reasonless_status_line c.input
       then
         Alcotest.(check bool)
           (Printf.sprintf "%S: response accepted" c.input)
           true
           (Result.is_ok g.response))
    changed;
  Alcotest.(check int) "cases changed on purpose" 1367 (List.length changed)

let test_size () =
  let cases = Lazy.force cases in
  Alcotest.(check int) "cases" 21294 (List.length cases);
  Alcotest.(check int)
    "outcomes"
    620341
    (List.fold_left (fun n c -> n + Array.length c.outcomes) 0 cases)

let check name differs =
  let cases =
    List.filter (fun c -> not (changed_on_purpose c)) (Lazy.force cases)
  in
  let failures = List.filter differs cases in
  List.iteri
    (fun i c -> if i < 20 then Printf.printf "%s differs: %S\n" name c.input)
    failures;
  Alcotest.(check int) (name ^ " cases that differ") 0 (List.length failures)

let test_grammar () =
  check "grammar" (fun c ->
    let g = Parse_oracle.grammar c.input in
    not
      (same_result c.grammar.request g.request
      && same_result c.grammar.response g.response
      && same_result c.grammar.chunk_size g.chunk_size
      && same_result c.grammar.trailers g.trailers))

let test_reader () =
  check "reader" (fun c ->
    let now = Parse_oracle.case c.kind ~limit:c.limit c.input in
    not
      (Array.for_all
         Fun.id
         (Array.mapi
            (fun k e -> same_outcome c.input k e now.outcomes.(k))
            c.outcomes)))

let test_splits_end_as_one_read () =
  check "split" (fun c ->
    let now = Parse_oracle.case c.kind ~limit:c.limit c.input in
    let one = now.outcomes.(0) in
    not
      (Array.for_all
         (fun o ->
            same_next one.post.next o.post.next
            && one.post.heads = o.post.heads
            && one.messages = o.messages)
         now.outcomes))

let tests =
  [ "frozen data size", `Quick, test_size
  ; "grammar matches v1", `Quick, test_grammar
  ; "reader matches v1 at every split", `Quick, test_reader
  ; "every split ends as one read", `Quick, test_splits_end_as_one_read
  ; ( "inputs changed by decision 0007 and the status-line fix"
    , `Quick
    , test_changed_on_purpose )
  ]
