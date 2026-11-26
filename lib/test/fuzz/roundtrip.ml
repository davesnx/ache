(* Target 3: what ache writes, ache reads back the same. A client writes a
   generated request and a server reads it; a server writes a generated response
   and a client reads it; and chunked bodies with extensions and trailers read
   back to their data. *)

open Ache

type body =
  | No_body
  | Fixed of string list
  | Chunked of string list

let body_to_string = function
  | No_body -> "no body"
  | Fixed p ->
    Printf.sprintf
      "fixed [%s]"
      (String.concat "; " (List.map (Printf.sprintf "%S") p))
  | Chunked p ->
    Printf.sprintf
      "chunked [%s]"
      (String.concat "; " (List.map (Printf.sprintf "%S") p))

let body_data = function
  | No_body -> ""
  | Fixed p | Chunked p -> String.concat "" p

let framing = function
  | No_body -> []
  | Fixed p ->
    [ "content-length", string_of_int (String.length (String.concat "" p)) ]
  | Chunked _ -> [ "transfer-encoding", "chunked" ]

(* Writes everything the connection has to write. *)
let drain_all ~next ~report buf =
  let rec go () =
    match Harness.guard "next_write_operation" next with
    | `Write iovecs ->
      let out = Harness.iovecs_to_string iovecs in
      Buffer.add_string buf out;
      Harness.guard "report_write_result" (fun () ->
        report (`Ok (String.length out)));
      go ()
    | `Yield | `Close _ -> ()
  in
  go ()

(* A server that records the requests it reads, with their bodies. *)
let recording_server () =
  let requests = ref []
  and errors = ref [] in
  let conn =
    Server_connection.create
      ~error_handler:(fun ?request:_ error start_response ->
        errors := Harness.server_error error :: !errors;
        Body.Writer.close (start_response Headers.empty))
      (fun reqd ->
         let request = Request_descriptor.request reqd in
         Harness.read_body
           (Request_descriptor.request_body reqd)
           ~on_done:(fun data finish ->
             requests := (request, data, finish) :: !requests;
             Request_descriptor.respond_with_string
               reqd
               (Response.create `OK)
               ""))
  in
  let output = Buffer.create 64 in
  let feeder =
    Harness.Feeder.make
      ~next_read_operation:(fun () ->
        Server_connection.next_read_operation conn)
      ~read:(Server_connection.read conn)
      ~read_eof:(Server_connection.read_eof conn)
      ~drain:(fun () ->
        drain_all
          ~next:(fun () -> Server_connection.next_write_operation conn)
          ~report:(Server_connection.report_write_result conn)
          output)
  in
  conn, feeder, requests, errors

(* A client with one request, that records the response. *)
let recording_client ?(config = Config.default) request =
  let responses = ref []
  and errors = ref [] in
  let conn = Client_connection.create ~config () in
  let body =
    Client_connection.request
      conn
      request
      ~error_handler:(fun e -> errors := Harness.client_error e :: !errors)
      ~response_handler:(fun response body ->
        Harness.read_body body ~on_done:(fun data finish ->
          responses := (response, data, finish) :: !responses))
  in
  let wire = Buffer.create 64 in
  let drain () =
    drain_all
      ~next:(fun () -> Client_connection.next_write_operation conn)
      ~report:(Client_connection.report_write_result conn)
      wire
  in
  let feeder =
    Harness.Feeder.make
      ~next_read_operation:(fun () ->
        Client_connection.next_read_operation conn)
      ~read:(Client_connection.read conn)
      ~read_eof:(Client_connection.read_eof conn)
      ~drain
  in
  conn, body, wire, drain, feeder, responses, errors

let check_one what ~errors ~got ~expected_data =
  match errors, got with
  | _ :: _, _ -> Some ("errors: " ^ String.concat ", " (List.rev errors))
  | [], [ (_, data, finish) ] ->
    if data <> expected_data
    then Some (Printf.sprintf "body %S, expected %S" data expected_data)
    else if finish <> "eof"
    then Some ("body ended with " ^ finish)
    else None
  | [], l -> Some (Printf.sprintf "%d %s, expected 1" (List.length l) what)

(* Requests *)

type request_case =
  { meth : Method.t
  ; target : string
  ; version : Version.t
  ; headers : (string * string) list
  ; body : body
  ; flush : bool
  ; reads : int list
  }

let gen_request st =
  let meth = Valid.meth st in
  let version = if Gen.chance st 4 then Version.v1_0 else Version.v1_1 in
  let body =
    match Gen.int st 3 with
    | 0 -> No_body
    | 1 -> Fixed (Valid.pieces st)
    | _ ->
      if version = Version.v1_0
      then Fixed (Valid.pieces st)
      else Chunked (Valid.pieces st)
  in
  { meth
  ; target = (if meth = `OPTIONS && Gen.bool st then "*" else Valid.path st)
  ; version
  ; headers = Valid.headers st
  ; body
  ; flush = Gen.bool st
  ; reads = List.init (Gen.range st 1 6) (fun _ -> Gen.range st 1 40)
  }

let print_request c =
  Printf.sprintf
    "%s %S %s\nfields: %s\n%s, flush between writes: %b\nreads: [%s]"
    (Method.to_string c.meth)
    c.target
    (Version.to_string c.version)
    (Valid.fields_to_string c.headers)
    (body_to_string c.body)
    c.flush
    (String.concat "," (List.map string_of_int c.reads))

let request_roundtrip c =
  let sent = (Valid.host :: c.headers) @ framing c.body in
  let request =
    Request.create
      ~version:c.version
      ~headers:(Headers.of_list sent)
      c.meth
      c.target
  in
  let _, writer, wire, drain, _, _, _ = recording_client request in
  (match c.body with
  | No_body -> ()
  | Fixed pieces | Chunked pieces ->
    List.iter
      (fun piece ->
         Body.Writer.write_string writer piece;
         if c.flush then Body.Writer.flush writer ignore;
         drain ())
      pieces);
  Body.Writer.close writer;
  drain ();
  let wire = Buffer.contents wire in
  let _, feeder, requests, errors = recording_server () in
  List.iter (Harness.Feeder.feed feeder) (Gen.chunks_of_sizes wire c.reads);
  let failure =
    match
      check_one
        "requests"
        ~errors:!errors
        ~got:!requests
        ~expected_data:(body_data c.body)
    with
    | Some _ as f -> f
    | None ->
      let r, _, _ = List.hd !requests in
      if Method.to_string r.meth <> Method.to_string c.meth
      then Some ("method " ^ Method.to_string r.meth)
      else if r.target <> c.target
      then Some (Printf.sprintf "target %S" r.target)
      else if r.version <> c.version
      then Some ("version " ^ Version.to_string r.version)
      else Valid.check_fields ~sent ~received:(Headers.to_list r.headers)
  in
  match failure with
  | None -> true
  | Some why ->
    Prop.fail
      "%s\nwire: %S\nread back: %s"
      why
      wire
      (String.concat
         " | "
         (List.map
            (fun (r, d, f) ->
               Printf.sprintf "%s %S %s" (Harness.request_summary r) d f)
            !requests))

(* Responses *)

type response_body =
  | String of string
  | Stream of
      { length : bool
      ; pieces : string list
      }

type response_case =
  { status : int
  ; reason : string
  ; fields : (string * string) list
  ; rbody : response_body
  ; close : bool
  ; http10 : bool
  ; head : bool
  ; rreads : int list
  }

let gen_response st =
  { status = Gen.pick st [ 200; 200; 201; 206; 299; 301; 400; 404; 500; 599 ]
  ; reason = Valid.reason st
  ; fields = Valid.headers st
  ; rbody =
      (if Gen.chance st 3
       then String (Gen.body st)
       else Stream { length = Gen.bool st; pieces = Valid.pieces st })
  ; close = Gen.chance st 4
  ; http10 = Gen.chance st 5
  ; head = Gen.chance st 6
  ; rreads = List.init (Gen.range st 1 6) (fun _ -> Gen.range st 1 40)
  }

let response_data c =
  match c.rbody with
  | String s -> s
  | Stream { pieces; _ } -> String.concat "" pieces

let print_response c =
  Printf.sprintf
    "%d %S\n\
     fields: %s\n\
     %s\n\
     Connection: close %b, HTTP/1.0 request %b, HEAD %b\n\
     reads: [%s]"
    c.status
    c.reason
    (Valid.fields_to_string c.fields)
    (match c.rbody with
    | String s -> Printf.sprintf "string %S" s
    | Stream { length; pieces } ->
      Printf.sprintf
        "stream%s [%s]"
        (if length then " with Content-Length" else "")
        (String.concat "; " (List.map (Printf.sprintf "%S") pieces)))
    c.close
    c.http10
    c.head
    (String.concat "," (List.map string_of_int c.rreads))

let response_roundtrip c =
  let data = response_data c in
  let sent =
    c.fields
    @ (match c.rbody with
      | Stream { length = true; _ } ->
        [ "content-length", string_of_int (String.length data) ]
      | String _ | Stream _ -> [])
    @ if c.close then [ "connection", "close" ] else []
  in
  let response =
    Response.create
      ~reason:c.reason
      ~headers:(Headers.of_list sent)
      (Status.of_code c.status)
  in
  let output = Buffer.create 64 in
  let conn =
    Server_connection.create (fun reqd ->
      match c.rbody with
      | String s -> Request_descriptor.respond_with_string reqd response s
      | Stream { pieces; _ } ->
        let w = Request_descriptor.respond_with_streaming reqd response in
        List.iter (Body.Writer.write_string w) pieces;
        Body.Writer.close w)
  in
  let meth = if c.head then `HEAD else `GET in
  let version = if c.http10 then Version.v1_0 else Version.v1_1 in
  let request =
    Request.create ~version ~headers:(Headers.of_list [ Valid.host ]) meth "/"
  in
  let request_bytes =
    Printf.sprintf
      "%s / %s\r\nhost: example.com\r\n\r\n"
      (Method.to_string meth)
      (Version.to_string version)
  in
  let feeder =
    Harness.Feeder.make
      ~next_read_operation:(fun () ->
        Server_connection.next_read_operation conn)
      ~read:(Server_connection.read conn)
      ~read_eof:(Server_connection.read_eof conn)
      ~drain:(fun () ->
        drain_all
          ~next:(fun () -> Server_connection.next_write_operation conn)
          ~report:(Server_connection.report_write_result conn)
          output)
  in
  Harness.Feeder.feed feeder request_bytes;
  let wire = Buffer.contents output in
  let _, writer, _, drain, feeder, responses, errors =
    recording_client request
  in
  Body.Writer.close writer;
  drain ();
  Harness.Feeder.run feeder (Gen.chunks_of_sizes wire c.rreads);
  let expected_data = if c.head then "" else data in
  let failure =
    match
      check_one "responses" ~errors:!errors ~got:!responses ~expected_data
    with
    | Some _ as f -> f
    | None ->
      let r, _, _ = List.hd !responses in
      if Status.to_code r.status <> c.status
      then Some (Printf.sprintf "status %d" (Status.to_code r.status))
      else if r.reason <> c.reason
      then Some (Printf.sprintf "reason %S" r.reason)
      else Valid.check_fields ~sent ~received:(Headers.to_list r.headers)
  in
  match failure with
  | None -> true
  | Some why -> Prop.fail "%s\nwire: %S" why wire

(* Chunked bodies with extensions and trailers, written by hand. *)

type chunked_case =
  { data : string
  ; encoded : string
  ; creads : int list
  }

let gen_chunked st =
  let data = Gen.body st in
  { data
  ; encoded = Gen.chunked ~valid:true st data
  ; creads = List.init (Gen.range st 1 6) (fun _ -> Gen.range st 1 30)
  }

let print_chunked c =
  Printf.sprintf
    "data %S\nencoded %S\nreads [%s]"
    c.data
    c.encoded
    (String.concat "," (List.map string_of_int c.creads))

let chunked_roundtrip c =
  let request_failure =
    let _, feeder, requests, errors = recording_server () in
    List.iter
      (Harness.Feeder.feed feeder)
      (Gen.chunks_of_sizes
         ("POST / HTTP/1.1\r\n\
           Host: example.com\r\n\
           Transfer-Encoding: chunked\r\n\
           \r\n"
         ^ c.encoded)
         c.creads);
    check_one "requests" ~errors:!errors ~got:!requests ~expected_data:c.data
  in
  let response_failure =
    let request =
      Request.create ~headers:(Headers.of_list [ Valid.host ]) `GET "/"
    in
    let _, writer, _, drain, feeder, responses, errors =
      recording_client request
    in
    Body.Writer.close writer;
    drain ();
    List.iter
      (Harness.Feeder.feed feeder)
      (Gen.chunks_of_sizes
         ("HTTP/1.1 200 OK\r\nTransfer-Encoding: chunked\r\n\r\n" ^ c.encoded)
         c.creads);
    check_one "responses" ~errors:!errors ~got:!responses ~expected_data:c.data
  in
  match request_failure, response_failure with
  | None, None -> true
  | Some why, _ -> Prop.fail "server: %s" why
  | None, Some why -> Prop.fail "client: %s" why

let tests =
  [ Prop.test
      ~name:"roundtrip: request, client to server"
      ~count:5000
      ~print:print_request
      gen_request
      request_roundtrip
  ; Prop.test
      ~name:"roundtrip: response, server to client"
      ~count:5000
      ~print:print_response
      gen_response
      response_roundtrip
  ; Prop.test
      ~name:"roundtrip: chunked extensions and trailers"
      ~count:5000
      ~print:print_chunked
      gen_chunked
      chunked_roundtrip
  ]
