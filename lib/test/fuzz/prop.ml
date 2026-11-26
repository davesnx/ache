(* Test construction. ACHE_FUZZ_SEED sets the seed (default 1, so that [make
   test] is reproducible); ACHE_FUZZ_SCALE multiplies every case count (default
   1), for longer runs; ACHE_FUZZ_VERBOSE=1 prints how the cases ended:

   ACHE_FUZZ_SEED=7 ACHE_FUZZ_SCALE=20 dune exec lib/test/fuzz/fuzz.exe *)

let env_int name default =
  match Sys.getenv_opt name with
  | None | Some "" -> default
  | Some s ->
    (match int_of_string_opt s with
    | Some n -> n
    | None -> failwith (Printf.sprintf "%s: not an integer: %S" name s))

let seed = env_int "ACHE_FUZZ_SEED" 1
let scale = env_int "ACHE_FUZZ_SCALE" 1
let verbose = env_int "ACHE_FUZZ_VERBOSE" 0 = 1

let test ?collect ~name ~count ~print gen prop =
  QCheck2.Test.make
    ?collect:(if verbose then collect else None)
    ~name
    ~count:(count * scale)
    ~print
    (QCheck2.Gen.make_primitive ~gen ~shrink:(fun _ -> Seq.empty))
    prop

let fail fmt = QCheck2.Test.fail_reportf fmt

(* Runs [check] on [input]; on failure, minimizes the input with [fails] and
   reports both. [check] returns [None] when the property holds. *)
let check_minimized ~check ~fails input =
  match check input with
  | None -> true
  | Some why ->
    let fails s = try fails s with Known_bugs.Known _ -> false in
    let small = Harness.minimize fails input in
    let small_why =
      match check small with
      | Some w -> w
      | None -> "(the minimal input fails another split; see above)"
      | exception Known_bugs.Known w -> "known bug: " ^ w
    in
    fail "%s\n\nminimal input: %S\n%s" why small small_why

let sizes chunks =
  String.concat "," (List.map (fun s -> string_of_int (String.length s)) chunks)

(* [None] when [run] gives the same trace for one read of [input] and for each
   split in [chunks_list], and no exception escapes. *)
let split_invariance ~run input chunks_list =
  match run [ input ] with
  | exception Harness.Escaped (w, e) ->
    Some
      (Printf.sprintf "one read: %s escaped from %s" (Printexc.to_string e) w)
  | one ->
    List.find_map
      (fun chunks ->
         match run chunks with
         | exception Harness.Escaped (w, e) ->
           Some
             (Printf.sprintf
                "reads [%s]: %s escaped from %s"
                (sizes chunks)
                (Printexc.to_string e)
                w)
         | split when split = one -> None
         | split ->
           Some
             (Printf.sprintf
                "reads [%s] differ from one read.\none read:\n%s\nsplit:\n%s"
                (sizes chunks)
                (Harness.trace_to_string one)
                (Harness.trace_to_string split)))
      chunks_list

(* Split invariance of [input] under [chunks], minimized over every split into
   two reads and one byte per read. *)
let split_invariant ~run ~input ~chunks =
  check_minimized
    ~check:(fun s ->
      split_invariance
        ~run
        s
        (if s = input then [ chunks ] else Harness.splits s))
    ~fails:(fun s ->
      Option.is_some (split_invariance ~run s (Harness.splits s)))
    input
