(* Reader for the llhttp markdown test files (test/{request,response}/*.md).
   Each case is a [<!-- meta={...} -->] comment, an [http] code block with the
   input and a [log] code block with the parser callbacks. The input escapes
   follow llhttp test/md-test.ts:150-175. *)

type outcome =
  | Reject
  | Accept of
      { messages : int
      ; body : int
      }
  | Crash of string

let outcome_to_string = function
  | Reject -> "reject"
  | Accept { messages; body } ->
    Printf.sprintf "ok messages=%d body=%d" messages body
  | Crash e -> "raise " ^ e

type case =
  { file : string  (** e.g. [request/sample.md] *)
  ; line : int  (** line of the [```http] fence *)
  ; heading : string
  ; kind : string  (** the meta [type], e.g. [request-lenient-headers] *)
  ; pause : bool
  ; skip_body : bool
  ; input : string
  ; expected : outcome  (** what llhttp logs *)
  }

let id c = Printf.sprintf "%s:%d" c.file c.line

let starts_with ~prefix s =
  String.length s >= String.length prefix
  && String.sub s 0 (String.length prefix) = prefix

let ends_with ~suffix s =
  let n = String.length s
  and k = String.length suffix in
  n >= k && String.sub s (n - k) k = suffix

let find_sub s sub =
  let n = String.length s
  and k = String.length sub in
  let rec go i =
    if i + k > n
    then None
    else if String.sub s i k = sub
    then Some i
    else go (i + 1)
  in
  go 0

(* The value of [key] in the flat JSON object [json], unquoted. *)
let meta_field json key =
  match find_sub json (Printf.sprintf "%S" key) with
  | None -> None
  | Some i ->
    let n = String.length json in
    let i = ref (i + String.length key + 2) in
    while !i < n && (json.[!i] = ':' || json.[!i] = ' ') do
      incr i
    done;
    if !i < n && json.[!i] = '"'
    then
      let j = String.index_from json (!i + 1) '"' in
      Some (String.sub json (!i + 1) (j - !i - 1))
    else
      let j = ref !i in
      while !j < n && not (List.mem json.[!j] [ ','; '}'; ' ' ]) do
        incr j
      done;
      Some (String.sub json !i (!j - !i))

(* One left-to-right pass like [String.prototype.replace] with a global regexp:
   [f s i] returns the replacement and the bytes matched at [i]. *)
let replace_all f s =
  let b = Buffer.create (String.length s) in
  let rec go i =
    if i < String.length s
    then
      match f s i with
      | Some (r, len) ->
        Buffer.add_string b r;
        go (i + len)
      | None ->
        Buffer.add_char b s.[i];
        go (i + 1)
  in
  go 0;
  Buffer.contents b

let at s i c = i < String.length s && s.[i] = c

let newline s i =
  if at s i '\r' && at s (i + 1) '\n'
  then Some 2
  else if at s i '\r' || at s i '\n'
  then Some 1
  else None

let is_hex c =
  match c with '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' -> true | _ -> false

let is_octal c = match c with '0' .. '7' -> true | _ -> false

let byte_of_digits ~base digits =
  String.make 1 (Char.chr (int_of_string (base ^ digits) land 0xff))

let escape c r s i = if at s i '\\' && at s (i + 1) c then Some (r, 2) else None

let digits ~pred ~max s i =
  let j = ref i in
  while !j < String.length s && !j - i < max && pred s.[!j] do
    incr j
  done;
  String.sub s i (!j - i)

let unescape input =
  let input =
    if ends_with ~suffix:"\n" input
    then String.sub input 0 (String.length input - 1)
    else input
  in
  input
  |> replace_all (fun s i ->
    if at s i '\\'
    then Option.map (fun n -> "", n + 1) (newline s (i + 1))
    else None)
  |> replace_all (fun s i -> Option.map (fun n -> "\r\n", n) (newline s i))
  |> replace_all (escape 'r' "\r")
  |> replace_all (escape 'n' "\n")
  |> replace_all (escape 't' "\t")
  |> replace_all (escape 'f' "\012")
  |> replace_all (fun s i ->
    if at s i '\\' && at s (i + 1) 'x'
    then
      match digits ~pred:is_hex ~max:max_int s (i + 2) with
      | "" -> None
      | d -> Some (byte_of_digits ~base:"0x" d, 2 + String.length d)
    else None)
  |> replace_all (fun s i ->
    if at s i '\\'
    then
      match digits ~pred:is_octal ~max:3 s (i + 1) with
      | "" -> None
      | d -> Some (byte_of_digits ~base:"0o" d, 1 + String.length d)
    else None)
  |> fun s ->
  if Option.is_some (find_sub s "${")
  then failwith "inline JavaScript in an llhttp input is not supported"
  else s

let int_after s key =
  match find_sub s key with
  | None -> None
  | Some i ->
    let d =
      digits
        ~pred:(function '0' .. '9' -> true | _ -> false)
        ~max:max_int
        s
        (i + String.length key)
    in
    if d = "" then None else Some (int_of_string d)

(* llhttp's outcome from its log. A pause on CONNECT or Upgrade (codes 22, 23)
   ends parsing and is not an error. A 1xx other than 101 is not counted: ache
   drops those responses (RFC 9110 §15.2). *)
let outcome_of_log log =
  let rec go ~messages ~body ~status = function
    | [] -> Accept { messages; body }
    | l :: rest ->
      (match find_sub l " error code=" with
      | Some _ ->
        (match int_after l " error code=" with
        | Some (22 | 23) -> Accept { messages; body }
        | _ -> Reject)
      | None ->
        let status =
          match int_after l "headers complete status=" with
          | Some s -> s
          | None -> status
        in
        let messages =
          if
            ends_with ~suffix:" message complete" l
            && not (status >= 100 && status < 200 && status <> 101)
          then messages + 1
          else messages
        in
        let body =
          match find_sub l " span[body]=" with
          | Some _ -> body + Option.value ~default:0 (int_after l " len=")
          | None -> body
        in
        go ~messages ~body ~status rest)
  in
  go ~messages:0 ~body:0 ~status:0 log

let read_lines path =
  let ic = open_in_bin path in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  String.split_on_char '\n' s

(* The lines of the code block that starts after [lines.(i)], and the index of
   its closing fence. *)
let block lines i =
  let rec go j acc =
    if j >= Array.length lines
    then failwith "unterminated code block"
    else if starts_with ~prefix:"```" lines.(j)
    then List.rev acc, j
    else go (j + 1) (lines.(j) :: acc)
  in
  go (i + 1) []

let read ~dir file =
  let lines = Array.of_list (read_lines (Filename.concat dir file)) in
  let cases = ref [] in
  let heading = ref ""
  and meta = ref None in
  let i = ref 0 in
  while !i < Array.length lines do
    let l = lines.(!i) in
    if starts_with ~prefix:"#" l
    then (
      let j = ref 0 in
      while !j < String.length l && l.[!j] = '#' do
        incr j
      done;
      heading := String.trim (String.sub l !j (String.length l - !j)));
    if starts_with ~prefix:"<!-- meta=" l
    then meta := Some (String.sub l 10 (String.length l - 10));
    if starts_with ~prefix:"```http" l
    then (
      let input, close = block lines !i in
      let rec log_fence j =
        if starts_with ~prefix:"```log" lines.(j) then j else log_fence (j + 1)
      in
      let log, close' = block lines (log_fence (close + 1)) in
      let json =
        match !meta with
        | Some m -> m
        | None -> failwith (Printf.sprintf "%s:%d: no meta" file (!i + 1))
      in
      meta := None;
      let field k = meta_field json k in
      cases :=
        { file
        ; line = !i + 1
        ; heading = !heading
        ; kind = Option.get (field "type")
        ; pause = Option.is_some (field "pause")
        ; skip_body = field "skipBody" = Some "true"
        ; input = unescape (String.concat "\n" input ^ "\n")
        ; expected = outcome_of_log log
        }
        :: !cases;
      i := close');
    incr i
  done;
  List.rev !cases
