(* Target 2: bytes into a client connection, after requests whose methods change
   the framing of the response (HEAD, CONNECT). *)

open Ache

type case =
  { input : string
  ; chunks : string list
  ; methods : Method.t list
  ; max_header_size : int
  }

let gen st =
  let input = Gen.messages st Gen.response ~seeds:Gen.response_seeds in
  { input
  ; chunks = Gen.split st input
  ; methods =
      (* Every request is written before any response arrives: a request that
         waits (behind a POST or a CONNECT) and gets a response before it is
         written sees an unsolicited response, which depends on the split. *)
      (match Gen.weighted st [ 6, 1; 3, 2; 1, 3 ] with
      | 1 -> [ Gen.weighted st [ 5, `GET; 2, `HEAD; 1, `CONNECT; 1, `POST ] ]
      | n -> List.init n (fun _ -> Gen.weighted st [ 3, `GET; 1, `HEAD ]))
  ; max_header_size = Gen.weighted st [ 8, 16384; 1, Gen.range st 16 200 ]
  }

let print c =
  Printf.sprintf
    "input: %S\nreads: [%s]\nmethods: %s, max_header_size %d"
    c.input
    (Prop.sizes c.chunks)
    (String.concat " " (List.map Method.to_string c.methods))
    c.max_header_size

(* A run that also checks the documented error rules of the client. *)
let run ~max_header_size methods chunks =
  let config =
    { Ache.Config.default with max_pipelined_requests = 4; max_header_size }
  in
  let t = Harness.run_client ~config methods chunks in
  Known_bugs.client_error_after_complete t;
  match Contract.client_errors t, Contract.once t with
  | Some e, _ ->
    raise (Harness.Escaped ("contract", Failure (Contract.message e)))
  | None, Some why -> raise (Harness.Escaped ("contract", Failure why))
  | None, None -> t

let prop c =
  match
    Prop.split_invariant
      ~run:(run ~max_header_size:c.max_header_size c.methods)
      ~input:c.input
      ~chunks:c.chunks
  with
  | ok -> ok
  | exception Known_bugs.Known _ -> QCheck2.assume_fail ()

(* How a case ended, for ACHE_FUZZ_VERBOSE=1. *)
let collect c =
  match run ~max_header_size:c.max_header_size c.methods [ c.input ] with
  | t -> Harness.outcome t
  | exception Known_bugs.Known _ -> "known bug"
  | exception e -> Printexc.to_string e

let tests =
  [ Prop.test
      ~collect
      ~name:"client_read: split invariance"
      ~count:20000
      ~print
      gen
      prop
  ]
