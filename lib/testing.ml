type side =
  { mutable pending : string
  ; mutable eof : bool
  ; mutable eof_given : bool
  ; mutable reading : bool
  ; mutable writing : bool
  ; out : Buffer.t
  }

let side () =
  { pending = ""
  ; eof = false
  ; eof_given = false
  ; reading = true
  ; writing = true
  ; out = Buffer.create 256
  }

let run (type c) (module E : Runtime.ENGINE with type t = c) (conn : c) side =
  let read () =
    match E.next_read_operation conn with
    | `Read ->
      let len = String.length side.pending in
      if side.eof && not (len = 0 && side.eof_given)
      then (
        let bs = Bigstring.of_string side.pending ~off:0 ~len in
        let n = E.read_eof conn bs ~off:0 ~len in
        side.pending <- String.sub side.pending n (len - n);
        let first = not side.eof_given in
        side.eof_given <- true;
        n > 0 || first)
      else if len > 0 && not side.eof
      then (
        let bs = Bigstring.of_string side.pending ~off:0 ~len in
        let n = E.read conn bs ~off:0 ~len in
        side.pending <- String.sub side.pending n (len - n);
        n > 0)
      else false
    | `Yield | `Close -> false
  in
  let write () =
    match E.next_write_operation conn with
    | `Write iovecs ->
      let n =
        List.fold_left
          (fun n { IOVec.buffer; off; len } ->
             Buffer.add_string side.out (Bigstring.substring buffer ~off ~len);
             n + len)
          0
          iovecs
      in
      E.report_write_result conn (`Ok n);
      n > 0
    | `Yield | `Close _ -> false
  in
  let read () =
    side.reading
    &&
    match read () with
    | progress -> progress
    | exception exn ->
      side.reading <- false;
      E.report_exn conn exn;
      true
  in
  let write () =
    side.writing
    &&
    match write () with
    | progress -> progress
    | exception exn ->
      side.reading <- false;
      side.writing <- false;
      E.report_exn conn exn;
      false
  in
  let rec loop () =
    let read = read () in
    let write = write () in
    if read || write
    then (
      if write then side.eof_given <- false;
      loop ())
  in
  loop ()

type t =
  { conn : Server_connection.t
  ; side : side
  }

let create ?config ?error_handler handler =
  { conn = Server_connection.create ?config ?error_handler handler
  ; side = side ()
  }

let advance t = run (module Server_connection) t.conn t.side

let feed t input =
  if t.side.eof then invalid_arg "Ache.Testing.feed: the input has ended";
  t.side.pending <- t.side.pending ^ input;
  advance t

let feed_eof t =
  t.side.eof <- true;
  advance t

let output t =
  advance t;
  let output = Buffer.contents t.side.out in
  Buffer.clear t.side.out;
  output

let is_closed t = Server_connection.is_closed t.conn

let serve ?config ?error_handler handler input =
  let t = create ?config ?error_handler handler in
  feed t input;
  feed_eof t;
  output t

let describe = function
  | `Malformed_response message -> "malformed response: " ^ message
  | `Invalid_response_body_length _ -> "invalid response body length"
  | `Connection_closed -> "no response"
  | `Exn exn -> Printexc.to_string exn

let serve_request ?config ?error_handler ?body handler (request : Request.t) =
  let request =
    match body with
    | Some body
      when not
             (Headers.mem request.headers "content-length"
             || Headers.mem request.headers "transfer-encoding") ->
      { request with
        headers =
          Headers.add
            request.headers
            "content-length"
            (string_of_int (String.length body))
      }
    | _ -> request
  in
  let client = Client_connection.create () in
  let response = ref None in
  let complete = ref false in
  let error = ref None in
  let response_body = Buffer.create 256 in
  let response_handler response' reader =
    response := Some response';
    let rec next () =
      Body.Reader.schedule_read
        reader
        ~on_eof:(fun () -> complete := true)
        ~on_error:ignore
        ~on_read:(fun bs ~off ~len ->
          Buffer.add_string response_body (Bigstring.substring bs ~off ~len);
          next ())
    in
    next ()
  in
  let error_handler' e = if !error = None then error := Some e in
  let writer =
    Client_connection.request
      client
      request
      ~error_handler:error_handler'
      ~response_handler
  in
  Option.iter (Body.Writer.write_string writer) body;
  Body.Writer.close writer;
  let client_side = side () in
  run (module Client_connection) client client_side;
  client_side.pending <-
    serve ?config ?error_handler handler (Buffer.contents client_side.out);
  client_side.eof <- true;
  run (module Client_connection) client client_side;
  match !response, !complete, !error with
  | Some response, true, _ -> response, Buffer.contents response_body
  | _, _, Some error ->
    failwith ("Ache.Testing.serve_request: " ^ describe error)
  | None, _, None -> failwith "Ache.Testing.serve_request: no response"
  | Some _, false, None ->
    failwith "Ache.Testing.serve_request: the response body is not complete"
