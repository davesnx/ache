(* Target 1: bytes into a server connection. No exception escapes, and what the
   application sees and the bytes written are the same for every split of the
   input into reads. *)

open Ache

type case =
  { input : string
  ; chunks : string list
  ; respond : Harness.respond
  ; max_header_size : int
  ; eof : bool
  }

let gen st =
  let input = Gen.messages st Gen.request ~seeds:Gen.request_seeds in
  { input
  ; chunks = Gen.split st input
  ; respond =
      Gen.pick
        st
        Harness.
          [ After_body; Immediately; Without_reading; Continue_then_after_body ]
  ; max_header_size = Gen.weighted st [ 8, 16384; 1, Gen.range st 16 200 ]
  ; eof = Gen.weighted st [ 3, true; 1, false ]
  }

let print c =
  Printf.sprintf
    "input: %S\nreads: [%s]\nrespond %s, max_header_size %d, end of input %b"
    c.input
    (String.concat
       "; "
       (List.map (fun s -> string_of_int (String.length s)) c.chunks))
    (Harness.respond_to_string c.respond)
    c.max_header_size
    c.eof

let run c chunks =
  let config = { Config.default with max_header_size = c.max_header_size } in
  Harness.run_server ~config ~eof:c.eof ~respond:c.respond chunks

let prop c =
  (* [Prop.split_invariance] runs one read of an input before its splits. *)
  let one = ref None in
  let run chunks =
    let t = run c chunks in
    (match Contract.server t with
    | Some why -> raise (Harness.Escaped ("contract", Failure why))
    | None -> ());
    (match chunks, !one with
    | [ _ ], _ -> one := Some t
    | _, Some one -> Known_bugs.server_late_rejection ~one ~split:t
    | _, None -> ());
    t
  in
  match Prop.split_invariant ~run ~input:c.input ~chunks:c.chunks with
  | ok -> ok
  | exception Known_bugs.Known _ -> QCheck2.assume_fail ()

(* How a case ended, for ACHE_FUZZ_VERBOSE=1. *)
let collect c =
  match run c [ c.input ] with
  | t -> Harness.outcome t
  | exception Known_bugs.Known _ -> "known bug"
  | exception e -> Printexc.to_string e

let tests =
  [ Prop.test
      ~collect
      ~name:"server_read: split invariance"
      ~count:20000
      ~print
      gen
      prop
  ]
