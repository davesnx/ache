(* Target 4: the application writes a body with random writes and flushes, and
   the runtime reports partial writes. The bytes on the wire read back to the
   same body, and every flush is answered once with [`Written]. *)

open Ache

type action =
  | Write_string of string
  | Write_char of char
  | Write_bigstring of string
  | Schedule_bigstring of string
  | Flush

let action_to_string = function
  | Write_string s -> Printf.sprintf "write_string %S" s
  | Write_char c -> Printf.sprintf "write_char %C" c
  | Write_bigstring s -> Printf.sprintf "write_bigstring %S" s
  | Schedule_bigstring s -> Printf.sprintf "schedule_bigstring %S" s
  | Flush -> "flush"

let action_data = function
  | Write_string s | Write_bigstring s | Schedule_bigstring s -> s
  | Write_char c -> String.make 1 c
  | Flush -> ""

type framing =
  | Chunked
  | Fixed
  | Close_delimited

type side =
  | Server
  | Client

type case =
  { side : side
  ; framing : framing
  ; flush_headers : bool
  ; actions : action list
  ; seed : int
  }

let gen_action st =
  let data () =
    if Gen.chance st 10
    then String.make (Gen.range st 1000 5000) 'b'
    else Gen.body st
  in
  match Gen.int st 6 with
  | 0 -> Write_string (data ())
  | 1 -> Write_char (Char.chr (Gen.int st 256))
  | 2 -> Write_bigstring (data ())
  | 3 -> Schedule_bigstring (data ())
  | _ -> Flush

let gen st =
  let side = if Gen.bool st then Server else Client in
  { side
  ; framing =
      Gen.pick
        st
        (match side with
        | Server -> [ Chunked; Fixed; Close_delimited ]
        | Client -> [ Chunked; Fixed ])
  ; flush_headers = Gen.bool st
  ; actions = List.init (Gen.int st 12) (fun _ -> gen_action st)
  ; seed = Gen.int st 1_000_000
  }

let print c =
  Printf.sprintf
    "%s, %s, flush headers %b, seed %d\n%s"
    (match c.side with Server -> "server" | Client -> "client")
    (match c.framing with
    | Chunked -> "chunked"
    | Fixed -> "Content-Length"
    | Close_delimited -> "close-delimited")
    c.flush_headers
    c.seed
    (String.concat "\n" (List.map action_to_string c.actions))

(* A bigstring with [s] at offset 1, between two bytes that must not be sent. *)
let padded s =
  let len = String.length s in
  Bigstring.of_string ("Z" ^ s ^ "Z") ~off:0 ~len:(len + 2), len

let apply writer flushes = function
  | Write_string s -> Body.Writer.write_string writer s
  | Write_char c -> Body.Writer.write_char writer c
  | Write_bigstring s ->
    let bs, len = padded s in
    Body.Writer.write_bigstring writer ~off:1 ~len bs
  | Schedule_bigstring s ->
    let bs, len = padded s in
    Body.Writer.schedule_bigstring writer ~off:1 ~len bs
  | Flush ->
    let answer = ref [] in
    flushes := answer :: !flushes;
    Body.Writer.flush writer (fun r -> answer := r :: !answer)

(* One write operation that writes a random part of what is offered. *)
let step st ~next ~report wire =
  match Harness.guard "next_write_operation" next with
  | `Write iovecs ->
    let total = IOVec.lengthv iovecs in
    let n = if total = 0 then 0 else Gen.range st 1 total in
    Buffer.add_string wire (Harness.iovecs_prefix iovecs n);
    Harness.guard "report_write_result" (fun () -> report (`Ok n));
    true
  | `Yield | `Close _ -> false

let run_actions st c writer ~next ~report wire =
  let flushes = ref [] in
  List.iter
    (fun a ->
       apply writer flushes a;
       for _ = 1 to Gen.int st 3 do
         ignore (step st ~next ~report wire : bool)
       done)
    c.actions;
  Body.Writer.close writer;
  let rec finish n =
    if n > 100_000
    then raise (Harness.Escaped ("the write loop", Failure "no progress"));
    if step st ~next ~report wire then finish (n + 1)
  in
  finish 0;
  List.rev_map (fun answer -> List.rev !answer) !flushes

let check_flushes flushes =
  List.find_map
    (fun answers ->
       match answers with
       | [ `Written ] -> None
       | [] -> Some "a flush was not answered"
       | [ `Closed ] -> Some "a flush was answered `Closed"
       | _ -> Some "a flush was answered twice")
    flushes

let server_side st c data =
  let headers =
    match c.framing with
    | Fixed -> [ "content-length", string_of_int (String.length data) ]
    | Chunked | Close_delimited -> []
  in
  let writer = ref None in
  let conn =
    Server_connection.create (fun reqd ->
      writer :=
        Some
          (Request_descriptor.respond_with_streaming
             ~flush_headers_immediately:c.flush_headers
             reqd
             (Response.create ~headers:(Headers.of_list headers) `OK)))
  in
  let request =
    match c.framing with
    | Close_delimited -> "GET / HTTP/1.0\r\n\r\n"
    | Chunked | Fixed -> "GET / HTTP/1.1\r\nHost: example.com\r\n\r\n"
  in
  let len = String.length request in
  let n =
    Server_connection.read
      conn
      (Bigstring.of_string request ~off:0 ~len)
      ~off:0
      ~len
  in
  if n <> len
  then Prop.fail "the server read %d of %d bytes of the request" n len;
  let writer =
    match !writer with Some w -> w | None -> Prop.fail "no request"
  in
  let wire = Buffer.create 256 in
  let flushes =
    run_actions
      st
      c
      writer
      ~next:(fun () -> Server_connection.next_write_operation conn)
      ~report:(Server_connection.report_write_result conn)
      wire
  in
  let wire = Buffer.contents wire in
  (* Read the wire back with a client. *)
  let version =
    if c.framing = Close_delimited then Version.v1_0 else Version.v1_1
  in
  let request =
    Request.create ~version ~headers:(Headers.of_list [ Valid.host ]) `GET "/"
  in
  let _, body, _, drain, feeder, responses, errors =
    Roundtrip.recording_client request
  in
  Body.Writer.close body;
  drain ();
  Harness.Feeder.run feeder [ wire ];
  match
    Roundtrip.check_one
      "responses"
      ~errors:!errors
      ~got:!responses
      ~expected_data:data
  with
  | Some why -> Some (why ^ Printf.sprintf "\nwire: %S" wire)
  | None -> check_flushes flushes

let client_side st c data =
  let headers =
    Valid.host
    ::
    (match c.framing with
    | Fixed -> [ "content-length", string_of_int (String.length data) ]
    | Chunked | Close_delimited -> [ "transfer-encoding", "chunked" ])
  in
  let request = Request.create ~headers:(Headers.of_list headers) `POST "/" in
  let conn, writer, _, _, _, _, _ = Roundtrip.recording_client request in
  ignore c.flush_headers;
  let wire = Buffer.create 256 in
  let flushes =
    run_actions
      st
      c
      writer
      ~next:(fun () -> Client_connection.next_write_operation conn)
      ~report:(Client_connection.report_write_result conn)
      wire
  in
  let wire = Buffer.contents wire in
  let _, feeder, requests, errors = Roundtrip.recording_server () in
  Harness.Feeder.feed feeder wire;
  match
    Roundtrip.check_one
      "requests"
      ~errors:!errors
      ~got:!requests
      ~expected_data:data
  with
  | Some why -> Some (why ^ Printf.sprintf "\nwire: %S" wire)
  | None -> check_flushes flushes

let prop c =
  let st = Random.State.make [| c.seed |] in
  let data = String.concat "" (List.map action_data c.actions) in
  match
    match c.side with
    | Server -> server_side st c data
    | Client -> client_side st c data
  with
  | None -> true
  | Some why -> Prop.fail "%s" why
  | exception Harness.Escaped (where, e) ->
    Prop.fail "%s escaped from %s" (Printexc.to_string e) where

let tests =
  [ Prop.test ~name:"write_path: partial writes" ~count:5000 ~print gen prop ]
