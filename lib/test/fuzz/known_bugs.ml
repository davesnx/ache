(* Bugs that the properties found. A property discards a case that shows a known
   bug that is not fixed yet, unless ACHE_FUZZ_KNOWN_BUGS=1: the hooks below
   call [discard] for it. None is open now. A fixed bug keeps its minimal input
   in [regressions], which run on every run. *)

let enabled = Sys.getenv_opt "ACHE_FUZZ_KNOWN_BUGS" = Some "1"

exception Known of string

let discard why = if not enabled then raise (Known why)

(* Hooks of the client and server targets, for known bugs. *)
let client_error_after_complete (_ : Harness.trace) = ()

let server_late_rejection ~one:(_ : Harness.trace) ~split:(_ : Harness.trace) =
  ()

(* Bugs found by the properties and fixed, run on every run. Each returns [None]
   when it passes, or what goes wrong. *)
let regressions : (string * (unit -> string option)) list =
  [ ( "client: bytes after a complete response give it no error"
    , fun () ->
        let input = "HTTP/1.1 200 OK\r\nContent-Length: 0\r\n\r\n\n" in
        let check chunks =
          Option.map
            Contract.message
            (Contract.client_errors (Harness.run_client [ `GET ] chunks))
        in
        match check [ input ], check (Gen.every_byte input) with
        | None, None -> None
        | one, bytes ->
          let show = Option.value ~default:"no error" in
          Some
            (Printf.sprintf
               "one read: %s; one byte per read: %s"
               (show one)
               (show bytes)) )
  ; ( "server: an invalid byte in a split line is found when it arrives"
    , fun () ->
        List.find_map
          (fun chunks ->
             let t = Harness.run_server ~eof:false ~respond:After_body chunks in
             if List.exists (String.starts_with ~prefix:"error") t.events
             then None
             else
               Some
                 (Printf.sprintf
                    "no error after the reads %s"
                    (String.concat ", " (List.map (Printf.sprintf "%S") chunks))))
          [ [ "g"; "/" ]; [ "\r"; "." ] ] )
  ]

let report () =
  let failed =
    List.filter_map
      (fun (name, check) ->
         match check () with
         | None -> None
         | Some why -> Some (Printf.sprintf "regression %s: %s" name why)
         | exception e ->
           Some (Printf.sprintf "regression %s: %s" name (Printexc.to_string e)))
      regressions
  in
  match failed with
  | [] ->
    Printf.printf "%d fixed bug regressions pass\n" (List.length regressions)
  | _ -> failwith (String.concat "\n" failed)
