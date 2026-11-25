(* Drives a server or client connection the way a runtime does, and records what
   the application sees: handler calls, body bytes, errors and output. *)

open Ache

exception Escaped of string * exn

let () =
  Printexc.register_printer (function
    | Escaped (where, exn) ->
      Some (Printf.sprintf "%s escaped from %s" (Printexc.to_string exn) where)
    | _ -> None)

let guard where f =
  try f () with Escaped _ as e -> raise e | e -> raise (Escaped (where, e))

let iovecs_to_string iovecs =
  String.concat
    ""
    (List.map
       (fun { IOVec.buffer; off; len } -> Bigstring.substring buffer ~off ~len)
       iovecs)

(* The first [n] bytes of [iovecs]. *)
let iovecs_prefix iovecs n = String.sub (iovecs_to_string iovecs) 0 n

let headers_to_string headers =
  String.concat
    ";"
    (List.map
       (fun (name, value) -> Printf.sprintf "%S:%S" name value)
       (Headers.to_list headers))

let request_summary { Request.meth; target; version; headers } =
  Printf.sprintf
    "%S %S %s [%s]"
    (Method.to_string meth)
    target
    (Version.to_string version)
    (headers_to_string headers)

let response_summary { Response.version; status; reason; headers } =
  Printf.sprintf
    "%s %d %S [%s]"
    (Version.to_string version)
    (Status.to_code status)
    reason
    (headers_to_string headers)

let server_error = function
  | `Bad_request _ -> "Bad_request"
  | `Not_implemented -> "Not_implemented"
  | `Http_version_not_supported -> "Http_version_not_supported"
  | `Uri_too_long -> "Uri_too_long"
  | `Bad_gateway -> "Bad_gateway"
  | `Internal_server_error -> "Internal_server_error"
  | `Request_header_fields_too_large -> "Request_header_fields_too_large"
  | `Request_timeout -> "Request_timeout"
  | `Exn exn -> Printexc.to_string exn

let client_error = function
  | `Malformed_response msg -> "Malformed_response " ^ msg
  | `Invalid_response_body_length r ->
    "Invalid_response_body_length " ^ response_summary r
  | `Connection_closed -> "Connection_closed"
  | `Exn exn -> Printexc.to_string exn

(* Reads a whole body; [on_done] gets its bytes and how it ended. *)
let read_body body ~on_done =
  let buf = Buffer.create 64 in
  let rec on_read bs ~off ~len =
    Buffer.add_string buf (Bigstring.substring bs ~off ~len);
    Body.Reader.schedule_read body ~on_error ~on_read ~on_eof
  and on_eof () = on_done (Buffer.contents buf) "eof"
  and on_error () = on_done (Buffer.contents buf) "error" in
  Body.Reader.schedule_read body ~on_error ~on_read ~on_eof

(* Input is offered at a nonzero offset of a larger bigstring, so that a read
   outside [off, off + len) shows up as a difference between splits. *)
let pad = 3

module Feeder = struct
  type t =
    { mutable pending : string
    ; mutable consumed : int
    ; mutable steps : int
    ; next_read_operation : unit -> [ `Read | `Yield | `Close ]
    ; read : Bigstring.t -> off:int -> len:int -> int
    ; read_eof : Bigstring.t -> off:int -> len:int -> int
    ; drain : unit -> unit
    }

  let make ~next_read_operation ~read ~read_eof ~drain =
    { pending = ""
    ; consumed = 0
    ; steps = 0
    ; next_read_operation
    ; read
    ; read_eof
    ; drain
    }

  let tick t =
    t.steps <- t.steps + 1;
    if t.steps > 1_000_000
    then raise (Escaped ("the read loop", Failure "no progress"))

  let next t = guard "next_read_operation" t.next_read_operation

  let offer t where f =
    tick t;
    let len = String.length t.pending in
    let bs =
      Bigstring.of_string
        (String.make pad 'Z' ^ t.pending ^ "ZZZ")
        ~off:0
        ~len:(len + pad + 3)
    in
    let n = guard where (fun () -> f bs ~off:pad ~len) in
    if n < 0 || n > len
    then
      raise
        (Escaped
           (where, Failure (Printf.sprintf "consumed %d of %d bytes" n len)));
    t.consumed <- t.consumed + n;
    t.pending <- String.sub t.pending n (len - n);
    n

  let rec pump t =
    tick t;
    match next t with
    | `Read when t.pending <> "" ->
      if offer t "read" t.read > 0
      then pump t
      else (
        t.drain ();
        (* A runtime asks again before it waits for input; a connection reports
           some errors there. *)
        match next t with
        | `Read -> t.drain ()
        | `Yield | `Close -> pump t)
    | `Yield ->
      t.drain ();
      if next t = `Read && t.pending <> "" then pump t
    | `Read | `Close -> t.drain ()

  let feed t chunk =
    t.pending <- t.pending ^ chunk;
    pump t

  (* Like a runtime, ask for the next operation after every read, also after the
     last [read_eof]: a connection reports some errors there. *)
  let rec eof t =
    pump t;
    match next t with
    | `Read ->
      let n = offer t "read_eof" t.read_eof in
      if n > 0 then eof t else pump t
    | `Yield | `Close -> ()

  let run t chunks =
    List.iter (feed t) chunks;
    eof t
end

let write_op_to_string = function
  | `Write _ -> "write"
  | `Yield -> "yield"
  | `Close n -> Printf.sprintf "close %d" n

let read_op_to_string = function
  | `Read -> "read"
  | `Yield -> "yield"
  | `Close -> "close"

(* [consumed] is [-1] after an error: the connection closes, and how much of the
   bad input it read does not matter. *)
type trace =
  { events : string list
  ; output : string
  ; consumed : int
  ; read_op : string
  ; write_op : string
  }

let consumed_unless_error events consumed =
  if List.exists (String.starts_with ~prefix:"error") events
  then -1
  else consumed

(* A server connection whose reader closed drops the bytes it did not read. *)
let consumed_unless_closed read_op consumed =
  if read_op = `Close then -1 else consumed

let trace_to_string t =
  Printf.sprintf
    "events:\n  %s\noutput: %S\nconsumed: %d, read: %s, write: %s"
    (String.concat "\n  " t.events)
    t.output
    t.consumed
    t.read_op
    t.write_op

let outcome t =
  let count prefix =
    List.length (List.filter (String.starts_with ~prefix) t.events)
  in
  let errors = List.filter (String.starts_with ~prefix:"error") t.events in
  Printf.sprintf
    "%d heads, %d bodies, %s"
    (count "request" + count "response")
    (count "body")
    (match errors with
    | [] -> "no error"
    | e :: _ ->
      (match String.split_on_char ' ' e with
      | "error" :: i :: kind :: _ when int_of_string_opt i <> None -> kind
      | "error" :: kind :: _ -> kind
      | _ -> e))

(* Server *)

type respond =
  | After_body
  | Immediately
  | Without_reading
  | Continue_then_after_body

let respond_to_string = function
  | After_body -> "after the body"
  | Immediately -> "immediately, then read the body"
  | Without_reading -> "immediately, never read the body"
  | Continue_then_after_body -> "100 Continue, then after the body"

let server ?(config = Config.default) ~respond () =
  let events = ref [] in
  let event fmt = Printf.ksprintf (fun s -> events := s :: !events) fmt in
  let output = Buffer.create 256 in
  let count = ref 0 in
  let request_handler reqd =
    incr count;
    let i = !count in
    event "request %d %s" i (request_summary (Request_descriptor.request reqd));
    let reply () =
      match
        Request_descriptor.error_code reqd, Request_descriptor.response reqd
      with
      | None, None ->
        Request_descriptor.respond_with_string reqd (Response.create `OK) "ok"
      | _ -> ()
    in
    let body = Request_descriptor.request_body reqd in
    let on_done data finish = event "body %d %S %s" i data finish in
    match respond with
    | After_body ->
      read_body body ~on_done:(fun data finish ->
        on_done data finish;
        reply ())
    | Immediately ->
      reply ();
      read_body body ~on_done
    | Without_reading -> reply ()
    | Continue_then_after_body ->
      Request_descriptor.respond_with_informational
        reqd
        (Response.create `Continue);
      read_body body ~on_done:(fun data finish ->
        on_done data finish;
        reply ())
  in
  let error_handler ?request error start_response =
    event
      "error %s%s"
      (server_error error)
      (match request with None -> "" | Some r -> " " ^ request_summary r);
    Body.Writer.close (start_response Headers.empty)
  in
  let conn = Server_connection.create ~config ~error_handler request_handler in
  let rec drain () =
    match
      guard "next_write_operation" (fun () ->
        Server_connection.next_write_operation conn)
    with
    | `Write iovecs ->
      let out = iovecs_to_string iovecs in
      Buffer.add_string output out;
      guard "report_write_result" (fun () ->
        Server_connection.report_write_result conn (`Ok (String.length out)));
      drain ()
    | `Yield | `Close _ -> ()
  in
  let feeder =
    Feeder.make
      ~next_read_operation:(fun () ->
        Server_connection.next_read_operation conn)
      ~read:(Server_connection.read conn)
      ~read_eof:(Server_connection.read_eof conn)
      ~drain
  in
  let finish () =
    let events = List.rev !events in
    let read_op = Server_connection.next_read_operation conn in
    { events
    ; output = Buffer.contents output
    ; consumed =
        consumed_unless_closed
          read_op
          (consumed_unless_error events feeder.consumed)
    ; read_op = read_op_to_string read_op
    ; write_op =
        write_op_to_string (Server_connection.next_write_operation conn)
    }
  in
  conn, feeder, finish

let run_server ?config ?(eof = true) ~respond chunks =
  let _, feeder, finish = server ?config ~respond () in
  if eof
  then Feeder.run feeder chunks
  else List.iter (Feeder.feed feeder) chunks;
  finish ()

(* Client *)

let client_request meth =
  let target = match meth with `CONNECT -> "example.com:443" | _ -> "/" in
  Request.create
    meth
    target
    ~headers:(Headers.of_list [ "host", "example.com" ])

let client ?(config = Config.default) methods =
  let events = ref [] in
  let event fmt = Printf.ksprintf (fun s -> events := s :: !events) fmt in
  let output = Buffer.create 256 in
  let conn = Client_connection.create ~config () in
  let tunnel = ref false in
  let rec drain () =
    match
      guard "next_write_operation" (fun () ->
        Client_connection.next_write_operation conn)
    with
    | `Write iovecs ->
      let out = iovecs_to_string iovecs in
      Buffer.add_string output out;
      guard "report_write_result" (fun () ->
        Client_connection.report_write_result conn (`Ok (String.length out)));
      drain ()
    | `Yield | `Close _ -> ()
  in
  List.iteri
    (fun i meth ->
       let body =
         Client_connection.request
           conn
           (client_request meth)
           ~error_handler:(fun e -> event "error %d %s" i (client_error e))
           ~response_handler:(fun response body ->
             event "response %d %s" i (response_summary response);
             if
               response.status = `Switching_protocols
               || (meth = `CONNECT && Status.is_successful response.status)
             then tunnel := true;
             read_body body ~on_done:(fun data finish ->
               event "body %d %S %s" i data finish))
       in
       Body.Writer.close body)
    methods;
  drain ();
  let feeder =
    Feeder.make
      ~next_read_operation:(fun () ->
        Client_connection.next_read_operation conn)
      ~read:(Client_connection.read conn)
      ~read_eof:(Client_connection.read_eof conn)
      ~drain
  in
  let finish () =
    let events = List.rev !events in
    let read_op = Client_connection.next_read_operation conn in
    { events
    ; (* Which requests are written depends on when responses complete, so it
         differs between splits. *)
      output = ""
    ; consumed =
        (* After a tunnel starts, the bytes not consumed go to it. *)
        (if !tunnel then Fun.id else consumed_unless_closed read_op)
          (consumed_unless_error events feeder.consumed)
    ; read_op = read_op_to_string read_op
    ; write_op =
        write_op_to_string (Client_connection.next_write_operation conn)
    }
  in
  conn, feeder, finish

let run_client ?config methods chunks =
  let _, feeder, finish = client ?config methods in
  Feeder.run feeder chunks;
  finish ()

(* Minimization: drop byte ranges while [fails] still holds. *)

let minimize ?(budget = 4000) fails input =
  let budget = ref budget in
  let fails s =
    decr budget;
    !budget > 0 && fails s
  in
  let rec shrink s n =
    if n = 0
    then s
    else
      let len = String.length s in
      let rec try_at p =
        if p + n > len
        then None
        else
          let s' = String.sub s 0 p ^ String.sub s (p + n) (len - p - n) in
          if fails s' then Some s' else try_at (p + 1)
      in
      match try_at 0 with
      | Some s' -> shrink s' (min n (String.length s'))
      | None -> shrink s (n / 2)
  in
  shrink input (String.length input / 2 |> max 1)

(* Every split into two reads, every byte as one read, and one read. *)
let splits s =
  let len = String.length s in
  Gen.every_byte s
  :: List.init
       (max 0 (len - 1))
       (fun i -> [ String.sub s 0 (i + 1); String.sub s (i + 1) (len - i - 1) ])
