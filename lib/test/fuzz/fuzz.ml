(* ACHE_FUZZ_SAMPLE=n prints n generated inputs of the server and client targets
   with how they ended, to check what the generators produce. *)
let sample n =
  let st = Random.State.make [| Prop.seed |] in
  for _ = 1 to n do
    let input = Gen.request st in
    Printf.printf
      "request %S\n  -> %s\n"
      input
      (Harness.outcome (Harness.run_server ~respond:After_body [ input ]))
  done;
  for _ = 1 to n do
    let c = Server_read.gen st in
    Printf.printf "server %S\n  -> %s\n" c.input (Server_read.collect c)
  done;
  for _ = 1 to n do
    let c = Client_read.gen st in
    Printf.printf "client %S\n  -> %s\n" c.input (Client_read.collect c)
  done

let () =
  Printf.printf "ACHE_FUZZ_SEED=%d ACHE_FUZZ_SCALE=%d\n%!" Prop.seed Prop.scale;
  match Prop.env_int "ACHE_FUZZ_SAMPLE" 0 with
  | n when n > 0 -> sample n
  | _ ->
    Known_bugs.report ();
    let rand = Random.State.make [| Prop.seed |] in
    let code =
      QCheck_base_runner.run_tests
        ~colors:false
        ~verbose:Prop.verbose
        ~rand
        (Server_read.tests
        @ Client_read.tests
        @ Roundtrip.tests
        @ Write_path.tests
        @ Small.tests
        @ Ws.tests)
    in
    exit code
