(* Documented rules that a single trace must follow, whatever the split. *)

let index_of prefix event =
  try
    Scanf.sscanf
      event
      (Scanf.format_from_string (prefix ^ " %d") "%d")
      Option.some
  with
  | Scanf.Scan_failure _ | Failure _ | End_of_file -> None

(* Client_connection.error: "Each request gets at most one error, and no error
   after its response is complete." *)
let client_errors (t : Harness.trace) =
  let complete = Hashtbl.create 4
  and errored = Hashtbl.create 4 in
  List.find_map
    (fun event ->
       (match index_of "body" event with
       | Some i when String.ends_with ~suffix:" eof" event ->
         Hashtbl.replace complete i ()
       | _ -> ());
       match index_of "error" event with
       | Some i when Hashtbl.mem complete i ->
         Some
           (`After_complete
               (Printf.sprintf
                  "request %d got an error after its response completed: %s"
                  i
                  event))
       | Some i when Hashtbl.mem errored i ->
         Some
           (`Second (Printf.sprintf "request %d got a second error: %s" i event))
       | Some i ->
         Hashtbl.replace errored i ();
         None
       | None -> None)
    t.events

let message = function `After_complete m | `Second m -> m

(* A body ends once, and a head is given to the application once. *)
let once (t : Harness.trace) =
  let seen = Hashtbl.create 4 in
  List.find_map
    (fun event ->
       List.find_map
         (fun kind ->
            match index_of kind event with
            | Some i when Hashtbl.mem seen (kind, i) ->
              Some (Printf.sprintf "%s %d twice: %s" kind i event)
            | Some i ->
              Hashtbl.replace seen (kind, i) ();
              None
            | None -> None)
         [ "body"; "request"; "response" ])
    t.events

(* Server_connection.create: an invalid request gets the error handler, and the
   connection closes after the error response; it calls the error handler once
   and the request handler not after it. *)
let server (t : Harness.trace) =
  match once t with
  | Some _ as e -> e
  | None ->
    let rec go errored = function
      | [] -> None
      | e :: rest when String.starts_with ~prefix:"error" e ->
        if errored then Some ("a second error: " ^ e) else go true rest
      | e :: _ when errored && String.starts_with ~prefix:"request" e ->
        Some ("a request after an error: " ^ e)
      | _ :: rest -> go errored rest
    in
    go false t.events
