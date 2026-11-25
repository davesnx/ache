(* Runs the llhttp markdown corpus (nodejs/llhttp @ 5aa5ac1, MIT, see LICENSE)
   through Server_connection and Client_connection, at every split of the input,
   and compares the outcome with llhttp's log. Differences are listed in
   expected-differences.txt. *)

open Ache
open Corpus

let files =
  [ "request/sample.md"
  ; "request/lenient-headers.md"
  ; "request/lenient-header-value-relaxed.md"
  ; "request/lenient-version.md"
  ; "request/method.md"
  ; "request/uri.md"
  ; "request/connection.md"
  ; "request/content-length.md"
  ; "request/transfer-encoding.md"
  ; "request/invalid.md"
  ; "request/finish.md"
  ; "request/pausing.md"
  ; "request/pipelining.md"
  ; "response/sample.md"
  ; "response/connection.md"
  ; "response/content-length.md"
  ; "response/transfer-encoding.md"
  ; "response/invalid.md"
  ; "response/finish.md"
  ; "response/lenient-version.md"
  ; "response/pausing.md"
  ; "response/pipelining.md"
  ]

(* Splits, as in lib/test/test_parse.ml [all_splits]. *)
let all_splits s =
  let every_byte = List.init (String.length s) (fun i -> String.make 1 s.[i]) in
  let two_way =
    List.init
      (String.length s + 1)
      (fun i -> [ String.sub s 0 i; String.sub s i (String.length s - i) ])
  in
  [ s ] :: every_byte :: two_way

module Feeder = struct
  type t =
    { mutable pending : string
    ; next_read_operation : unit -> [ `Read | `Yield | `Close ]
    ; read : Bigstring.t -> off:int -> len:int -> int
    ; drain : unit -> unit
    }

  let offer t =
    let len = String.length t.pending in
    let bs = Bigstring.of_string t.pending ~off:0 ~len in
    let n = t.read bs ~off:0 ~len in
    if n < 0 || n > len
    then failwith (Printf.sprintf "read consumed %d of %d bytes" n len);
    t.pending <- String.sub t.pending n (len - n);
    n

  let rec pump t =
    match t.next_read_operation () with
    | `Read when t.pending <> "" ->
      if offer t > 0
      then pump t
      else (
        (* A runtime asks again before it reads more: the connection reports an
           error found by the last read here. *)
        t.drain ();
        ignore (t.next_read_operation () : [ `Read | `Yield | `Close ]);
        t.drain ())
    | `Yield ->
      t.drain ();
      if t.next_read_operation () = `Read && t.pending <> "" then pump t
    | `Read | `Close -> t.drain ()

  let feed t chunk =
    t.pending <- t.pending ^ chunk;
    pump t
end

type tally =
  { mutable messages : int
  ; mutable body : int
  ; mutable errors : string list
  }

let read_body body ~tally ~on_done =
  let rec on_read _ ~off:_ ~len =
    tally.body <- tally.body + len;
    Body.Reader.schedule_read body ~on_error ~on_read ~on_eof
  and on_eof () =
    tally.messages <- tally.messages + 1;
    on_done ()
  and on_error () = () in
  Body.Reader.schedule_read body ~on_error ~on_read ~on_eof

let tally_outcome t =
  match t.errors with
  | [] -> Accept { messages = t.messages; body = t.body }
  | _ -> Reject

let error_name : Server_connection.error -> string = function
  | `Bad_request -> "400"
  | `Not_implemented -> "501"
  | `Http_version_not_supported -> "505"
  | `Uri_too_long -> "414"
  | `Bad_gateway -> "502"
  | `Internal_server_error -> "500"
  | `Request_header_fields_too_large -> "431"
  | `Request_timeout -> "408"
  | `Exn e -> Printexc.to_string e

(* A request after which llhttp stops parsing (it pauses on CONNECT and
   Upgrade). The handler does not answer it, so ache does not read on. *)
let may_switch (r : Request.t) =
  r.meth = `CONNECT
  || (Headers.mem r.headers "upgrade"
     && List.exists
          (fun v ->
             List.exists
               (fun o -> String.lowercase_ascii (String.trim o) = "upgrade")
               (String.split_on_char ',' v))
          (Headers.get_multi r.headers "connection"))

let run_server chunks =
  let tally = { messages = 0; body = 0; errors = [] } in
  let conn = ref None in
  let rec drain () =
    match Server_connection.next_write_operation (Option.get !conn) with
    | `Write iovecs ->
      Server_connection.report_write_result
        (Option.get !conn)
        (`Ok (IOVec.lengthv iovecs));
      drain ()
    | `Yield | `Close _ -> ()
  in
  let request_handler reqd =
    read_body (Request_descriptor.request_body reqd) ~tally ~on_done:(fun () ->
      if
        (not (may_switch (Request_descriptor.request reqd)))
        && Request_descriptor.response reqd = None
        && Request_descriptor.error_code reqd = None
      then (
        Request_descriptor.respond_with_string
          reqd
          (Response.create
             ~headers:(Headers.of_list [ "content-length", "0" ])
             `OK)
          "";
        drain ()))
  in
  let error_handler ?request:_ error start_response =
    tally.errors <- error_name error :: tally.errors;
    Body.Writer.close (start_response Headers.empty)
  in
  let c = Server_connection.create ~error_handler request_handler in
  conn := Some c;
  let feeder =
    { Feeder.pending = ""
    ; next_read_operation = (fun () -> Server_connection.next_read_operation c)
    ; read = Server_connection.read c
    ; drain
    }
  in
  List.iter (Feeder.feed feeder) chunks;
  tally

let run_client ~meth chunks =
  let tally = { messages = 0; body = 0; errors = [] } in
  let c = Client_connection.create () in
  let rec drain () =
    match Client_connection.next_write_operation c with
    | `Write iovecs ->
      Client_connection.report_write_result c (`Ok (IOVec.lengthv iovecs));
      drain ()
    | `Yield | `Close _ -> ()
  in
  for _ = 1 to 8 do
    let error_handler = function
      | `Connection_closed -> ()
      | `Malformed_response m -> tally.errors <- m :: tally.errors
      | `Invalid_response_body_length _ ->
        tally.errors <- "invalid body length" :: tally.errors
      | `Exn e -> tally.errors <- Printexc.to_string e :: tally.errors
    in
    let body =
      Client_connection.request
        c
        (Request.create ~headers:(Headers.of_list [ "host", "a" ]) meth "/")
        ~error_handler
        ~response_handler:(fun _ body -> read_body body ~tally ~on_done:drain)
    in
    Body.Writer.close body
  done;
  drain ();
  let feeder =
    { Feeder.pending = ""
    ; next_read_operation = (fun () -> Client_connection.next_read_operation c)
    ; read = Client_connection.read c
    ; drain
    }
  in
  List.iter (Feeder.feed feeder) chunks;
  tally

(* Split on CRLF. *)
let lines s =
  let rec go i start acc =
    if i + 1 >= String.length s
    then List.rev (String.sub s start (String.length s - start) :: acc)
    else if s.[i] = '\r' && s.[i + 1] = '\n'
    then go (i + 2) (i + 2) (String.sub s start (i - start) :: acc)
    else go (i + 1) start acc
  in
  go 0 0 []

let is_request_line_1_1 l =
  match String.split_on_char ' ' l with
  | [ m; t; "HTTP/1.1" ] -> m <> "" && t <> ""
  | _ -> false

(* Most llhttp requests have no Host, which ache answers with 400 (RFC 9112
   §3.2): add [Host: a] to each HTTP/1.1 request head that has none. *)
let add_host input =
  let has_host rest =
    let rec go = function
      | [] | "" :: _ -> false
      | l :: rest ->
        starts_with ~prefix:"host:" (String.lowercase_ascii l) || go rest
    in
    go rest
  in
  let rec go = function
    | [] -> []
    | l :: rest when is_request_line_1_1 l && not (has_host rest) ->
      l :: "Host: a" :: go rest
    | l :: rest -> l :: go rest
  in
  String.concat "\r\n" (go (lines input))

type kind =
  | Strict
  | Lenient
  | Not_compared of string

let classify c =
  if c.pause
  then Not_compared "pause"
  else
    match c.kind with
    | "request" | "response" -> Strict
    | k when find_sub k "-lenient-" <> None -> Lenient
    | k -> Not_compared k

let is_request c = starts_with ~prefix:"request" c.kind

(* ache's outcome on every split of the input; [None] when all agree with
   [expected], else the first split that does not, and its outcome. *)
let check c ~expected =
  let input = if is_request c then add_host c.input else c.input in
  let run chunks =
    match
      if is_request c
      then run_server chunks
      else run_client ~meth:(if c.skip_body then `HEAD else `GET) chunks
    with
    | t -> tally_outcome t, String.concat "; " (List.rev t.errors)
    | exception e -> Crash (Printexc.to_string e), ""
  in
  let rec go i = function
    | [] -> None
    | chunks :: rest ->
      let got, detail = run chunks in
      if got = expected then go (i + 1) rest else Some (i, got, detail)
  in
  go 0 (all_splits input)

type listed =
  { key : string
  ; reason : string
  }

let read_expected path =
  read_lines path
  |> List.filter_map (fun l ->
    let l = String.trim l in
    if l = "" || l.[0] = '#'
    then None
    else
      match List.map String.trim (String.split_on_char '|' l) with
      | key :: reason :: _
        when List.exists
               (fun prefix -> starts_with ~prefix reason)
               [ "by design"; "harness"; "ticket" ] ->
        Some { key; reason }
      | _ -> failwith ("expected-differences.txt: bad line: " ^ l))

let reason_kind r =
  if starts_with ~prefix:"ticket" r
  then r
  else if starts_with ~prefix:"by design" r
  then "by design"
  else "harness"

let () =
  let cases = List.concat_map (read ~dir:".") files in
  let listed = read_expected "expected-differences.txt" in
  let failures = ref [] in
  let fail fmt = Printf.ksprintf (fun s -> failures := s :: !failures) fmt in
  let compared = ref 0
  and strict = ref 0
  and lenient = ref 0
  and strict_agree = ref 0
  and lenient_agree = ref 0
  and differ = Hashtbl.create 16
  and seen = Hashtbl.create 64 in
  List.iter
    (fun c ->
       let key = id c in
       let compare ~expected ~agreed =
         incr compared;
         Hashtbl.replace seen key ();
         let entry = List.find_opt (fun l -> l.key = key) listed in
         match check c ~expected, entry with
         | None, None -> incr agreed
         | None, Some _ ->
           fail
             "%s [%s]: listed in expected-differences.txt but agrees now (%s); \
              remove it"
             key
             c.heading
             (outcome_to_string expected)
         | Some (split, got, detail), None ->
           fail
             "%s [%s] %s: llhttp %s, ache %s%s at split %d"
             key
             c.heading
             c.kind
             (outcome_to_string expected)
             (outcome_to_string got)
             (if detail = "" then "" else " (" ^ detail ^ ")")
             split
         | Some _, Some l ->
           let k = reason_kind l.reason in
           Hashtbl.replace
             differ
             k
             (1 + Option.value ~default:0 (Hashtbl.find_opt differ k))
       in
       match classify c with
       | Strict ->
         incr strict;
         compare ~expected:c.expected ~agreed:strict_agree
       | Lenient ->
         incr lenient;
         compare ~expected:Reject ~agreed:lenient_agree
       | Not_compared _ -> ())
    cases;
  List.iter
    (fun l ->
       if not (Hashtbl.mem seen l.key)
       then
         fail
           "%s: listed in expected-differences.txt, not a compared case"
           l.key)
    listed;
  Printf.printf
    "llhttp corpus: %d cases, %d compared, %d of %d strict and %d of %d \
     lenient agree"
    (List.length cases)
    !compared
    !strict_agree
    !strict
    !lenient_agree
    !lenient;
  Hashtbl.fold (fun k n acc -> (k, n) :: acc) differ []
  |> List.sort compare
  |> List.iter (fun (k, n) -> Printf.printf ", %d %s" n k);
  print_newline ();
  match List.rev !failures with
  | [] -> ()
  | fs ->
    List.iter prerr_endline fs;
    Printf.eprintf "%d unexpected results\n" (List.length fs);
    exit 1
