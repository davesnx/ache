(* Target 5: the small parsers. Each one either agrees with a direct reading of
   its grammar, or, on arbitrary bytes, returns without an exception. *)

open Ache
module Grammar = Ache_private.Parse.Grammar

let no_exception name f =
  match f () with
  | v -> v
  | exception e -> Prop.fail "%s raised %s" name (Printexc.to_string e)

(* chunk-size *)

let gen_size st =
  let bits = Gen.range st 0 60 in
  let n = (Random.State.bits st lsl 30) lor Random.State.bits st in
  if bits = 0 then 0 else n land ((1 lsl bits) - 1)

let gen_size_line st =
  let n = gen_size st in
  n, Gen.hex_size st n ^ Gen.valid_extension st ^ "\r\n"

let size_line_roundtrip (n, line) =
  match no_exception "chunk_size" (fun () -> Grammar.chunk_size line) with
  | Ok v when v = Int64.of_int n -> true
  | Ok v -> Prop.fail "read %Ld" v
  | Error e -> Prop.fail "rejected: %s" e

let gen_size_bytes st =
  let s =
    Gen.string_of
      st
      "0123456789abcdefABCDEFxX; =\"\t\r\n\000\x80"
      (Gen.int st 24)
  in
  if Gen.bool st then s ^ "\r\n" else s

let is_hex c =
  match c with '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' -> true | _ -> false

let size_bytes line =
  match no_exception "chunk_size" (fun () -> Grammar.chunk_size line) with
  | Error _ -> true
  | Ok v ->
    let digits =
      let rec go i =
        if i < String.length line && is_hex line.[i] then go (i + 1) else i
      in
      String.sub line 0 (go 0)
    in
    (match Int64.of_string_opt ("0x" ^ digits) with
    | Some d when d = v && Int64.compare v 0L >= 0 -> true
    | _ -> Prop.fail "read %Ld from the digits %S" v digits)

(* Content-Length *)

let ows st = Gen.pick st [ ""; ""; " "; "\t"; "  " ]

let members st values =
  String.concat "," (List.map (fun v -> ows st ^ v ^ ows st) values)

type length_case =
  { values : string list list (* field lines, each a list of members *)
  ; expected : int option
  }

let gen_length st =
  let n = if Gen.bool st then Gen.int st 1000 else gen_size st in
  let one () = List.init (Gen.range st 1 3) (fun _ -> string_of_int n) in
  match Gen.int st 3 with
  | 0 ->
    { values = List.init (Gen.range st 1 2) (fun _ -> one ())
    ; expected = Some n
    }
  | 1 ->
    let other = string_of_int (n + Gen.range st 1 9) in
    { values = [ one () @ [ other ] ]; expected = None }
  | _ ->
    { values =
        List.init (Gen.range st 1 2) (fun _ ->
          [ Gen.string_of st "0123456789 ,\t+-x" (Gen.int st 14) ])
    ; expected = None
    }

let print_length c =
  String.concat
    " | "
    (List.map (fun l -> Printf.sprintf "[%s]" (String.concat "," l)) c.values)

let trim s =
  let is_ows c = c = ' ' || c = '\t' in
  let rec l i = if i < String.length s && is_ows s.[i] then l (i + 1) else i in
  let rec r j = if j > 0 && is_ows s.[j - 1] then r (j - 1) else j in
  let i = l 0 in
  let j = r (String.length s) in
  if j <= i then "" else String.sub s i (j - i)

let length_prop st_seed c =
  let st = Random.State.make [| st_seed |] in
  let lines = List.map (fun l -> "content-length", members st l) c.values in
  let headers = Headers.of_list (Valid.host :: lines) in
  let request = Request.create ~headers `POST "/" in
  let response = Response.create ~headers `OK in
  let from_request =
    match
      no_exception "Request.body_length" (fun () -> Request.body_length request)
    with
    | `Fixed n -> Some n
    | `Chunked | `Error _ -> None
  in
  let from_response =
    match
      no_exception "Response.body_length" (fun () ->
        Response.body_length ~request_method:`GET response)
    with
    | `Fixed n -> Some n
    | `Chunked | `Error _ | `No_body | `Close_delimited -> None
  in
  let shown = String.concat " | " (List.map snd lines) in
  if from_request <> from_response
  then Prop.fail "request and response disagree on %S" shown
  else
    match c.expected, from_request with
    | Some n, Some m when Int64.of_int n = m -> true
    | Some n, _ -> Prop.fail "%S: expected %d" shown n
    | None, None -> true
    | None, Some m ->
      (* Arbitrary values: accepted only when every member is digits of the same
         value. *)
      let all =
        List.concat_map (fun (_, v) -> String.split_on_char ',' v) lines
        |> List.map trim
      in
      if
        Int64.compare m 0L >= 0
        && List.for_all
             (fun d ->
                d = ""
                || (String.for_all
                      (function '0' .. '9' -> true | _ -> false)
                      d
                   && Int64.of_string_opt d = Some m))
             all
      then true
      else Prop.fail "%S read as %Ld" shown m

(* Field values *)

let gen_value st =
  Gen.string_of st "ab \t~!\"\x7f\x80\xff\000\r\n:" (Gen.int st 12)

let field_char c = c = '\t' || (c >= ' ' && c <> '\x7f')

let value_prop v =
  let input = "GET / HTTP/1.1\r\nX:" ^ v ^ "\r\n\r\n" in
  let result =
    no_exception "Grammar.request" (fun () -> Grammar.request input)
  in
  if String.contains v '\r' || String.contains v '\n'
  then true
  else
    match result, String.for_all field_char v with
    | Ok r, true ->
      (match Headers.get_multi r.headers "x" with
      | [ got ] when got = trim v -> true
      | _ -> Prop.fail "read %s" (Harness.request_summary r))
    | Error _, false -> true
    | Ok r, false -> Prop.fail "accepted: %s" (Harness.request_summary r)
    | Error e, true -> Prop.fail "rejected: %s" e

(* Heads: an accepted head written back by ache reads the same. *)

type head_case =
  | Request_head of string
  | Response_head of string

let gen_head st =
  let cut s =
    let rec find i =
      if i + 4 > String.length s
      then s
      else if String.sub s i 4 = "\r\n\r\n"
      then String.sub s 0 (i + 4)
      else find (i + 1)
    in
    find 0
  in
  if Gen.bool st
  then Request_head (cut (Gen.messages st Gen.request ~seeds:Gen.request_seeds))
  else
    Response_head (cut (Gen.messages st Gen.response ~seeds:Gen.response_seeds))

let print_head = function
  | Request_head s -> Printf.sprintf "request %S" s
  | Response_head s -> Printf.sprintf "response %S" s

let serialize write v =
  let e = Ache_private.Encoder.create 0x1000 in
  match write e v with
  | () -> Some (Ache_private.Encoder.serialize_to_string e)
  | exception Invalid_argument _ -> None

let head_prop = function
  | Request_head s ->
    (match no_exception "Grammar.request" (fun () -> Grammar.request s) with
    | Error _ -> true
    | Ok r ->
      (match serialize Ache_private.Serialize.write_request r with
      | None -> true
      | Some s' ->
        (match Grammar.request s' with
        | Ok r' when Harness.request_summary r' = Harness.request_summary r ->
          true
        | Ok r' ->
          Prop.fail "written %S, read %s" s' (Harness.request_summary r')
        | Error e -> Prop.fail "written %S, rejected: %s" s' e)))
  | Response_head s ->
    (match no_exception "Grammar.response" (fun () -> Grammar.response s) with
    | Error _ -> true
    | Ok r ->
      (match serialize Ache_private.Serialize.write_response r with
      | None -> true
      | Some s' ->
        (match Grammar.response s' with
        | Ok r' when Harness.response_summary r' = Harness.response_summary r ->
          true
        | Ok r' ->
          Prop.fail "written %S, read %s" s' (Harness.response_summary r')
        | Error e -> Prop.fail "written %S, rejected: %s" s' e)))

(* Trailer sections *)

let gen_trailers st =
  let line () =
    if Gen.bool st
    then Printf.sprintf "%s: %s\r\n" (Valid.token st) (Valid.field_value st)
    else Gen.string_of st "ab: \t\r\n\000\x80" (Gen.int st 10)
  in
  String.concat "" (List.init (Gen.int st 4) (fun _ -> line ())) ^ "\r\n"

let trailers_prop s =
  match no_exception "Grammar.trailers" (fun () -> Grammar.trailers s) with
  | Error _ -> true
  | Ok h ->
    let s' =
      String.concat
        ""
        (List.map (fun (n, v) -> n ^ ": " ^ v ^ "\r\n") (Headers.to_list h))
      ^ "\r\n"
    in
    (match Grammar.trailers s' with
    | Ok h' when Headers.to_list h' = Headers.to_list h -> true
    | _ -> Prop.fail "written back as %S, read differently" s')

let tests =
  [ Prop.test
      ~name:"small: chunk-size reads its hex value"
      ~count:5000
      ~print:(fun (n, line) -> Printf.sprintf "%d %S" n line)
      gen_size_line
      size_line_roundtrip
  ; Prop.test
      ~name:"small: chunk-size on arbitrary bytes"
      ~count:5000
      ~print:(Printf.sprintf "%S")
      gen_size_bytes
      size_bytes
  ; Prop.test
      ~name:"small: Content-Length lists"
      ~count:5000
      ~print:(fun (_, c) -> print_length c)
      (fun st -> Gen.int st 1_000_000, gen_length st)
      (fun (seed, c) -> length_prop seed c)
  ; Prop.test
      ~name:"small: field values"
      ~count:5000
      ~print:(Printf.sprintf "%S")
      gen_value
      value_prop
  ; Prop.test
      ~name:"small: heads written back read the same"
      ~count:5000
      ~print:print_head
      gen_head
      head_prop
  ; Prop.test
      ~name:"small: trailer sections"
      ~count:5000
      ~print:(Printf.sprintf "%S")
      gen_trailers
      trailers_prop
  ]
