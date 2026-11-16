open Ache

let test name f = Alcotest.test_case name `Quick f

exception Api_changed of string

let known_failure ~ticket name f =
  let label = Printf.sprintf "%s [known failure: ticket %s]" name ticket in
  Alcotest.test_case label `Quick (fun () ->
    match f () with
    | () ->
      Alcotest.failf
        "known failure now passes: remove the known_failure marker (ticket %s) \
         from \"%s\""
        ticket
        name
    | exception Api_changed msg ->
      Alcotest.failf
        "the API this known failure uses changed (%s): rewrite \"%s\" for the \
         new API (ticket %s)"
        msg
        name
        ticket
    | exception exn ->
      Printf.printf
        "KNOWN FAILURE (ticket %s) %s\n  observed: %s\n%!"
        ticket
        name
        (Printexc.to_string exn))

let iovecs_to_string iovecs =
  let buf = Buffer.create 256 in
  List.iter
    (fun { IOVec.buffer; off; len } ->
       Buffer.add_string buf (Ache.Bigstring.substring buffer ~off ~len))
    iovecs;
  Buffer.contents buf

type read_op =
  [ `Read
  | `Yield
  | `Close
  ]

type write_op =
  [ `Write
  | `Yield
  | `Close
  ]

let read_op_to_string : read_op -> string = function
  | `Read -> "Read"
  | `Yield -> "Yield"
  | `Close -> "Close"

let write_op_to_string : write_op -> string = function
  | `Write -> "Write"
  | `Yield -> "Yield"
  | `Close -> "Close"

let read_op =
  Alcotest.of_pp (fun ppf x -> Format.pp_print_string ppf (read_op_to_string x))

let write_op =
  Alcotest.of_pp (fun ppf x ->
    Format.pp_print_string ppf (write_op_to_string x))

module type Connection = sig
  type t

  val next_read_operation : t -> [ `Read | `Yield | `Close ]
  val read : t -> Ache.Bigstring.t -> off:int -> len:int -> int
  val read_eof : t -> Ache.Bigstring.t -> off:int -> len:int -> int
  val yield_reader : t -> (unit -> unit) -> unit

  val next_write_operation :
     t
    -> [ `Write of Ache.Bigstring.t IOVec.t list | `Yield | `Close of int ]

  val report_write_result : t -> [ `Ok of int | `Closed ] -> unit
  val yield_writer : t -> (unit -> unit) -> unit
end

module Driver (Under_test : Connection) = struct
  type t =
    { conn : Under_test.t
    ; mutable pending : string
    ; mutable eof_requested : bool
    ; mutable eof_delivered : bool
    ; output : Buffer.t
    ; mutable reader_parked : bool
    ; mutable writer_parked : bool
    ; mutable read_op : read_op
    ; mutable write_op : write_op
    }

  let make conn =
    { conn
    ; pending = ""
    ; eof_requested = false
    ; eof_delivered = false
    ; output = Buffer.create 256
    ; reader_parked = false
    ; writer_parked = false
    ; read_op = `Read
    ; write_op = `Yield
    }

  let read_step t =
    match Under_test.next_read_operation t.conn with
    | `Read ->
      t.read_op <- `Read;
      let len = String.length t.pending in
      let bs = Ache.Bigstring.of_string t.pending ~off:0 ~len in
      if t.eof_requested && not t.eof_delivered
      then (
        t.eof_delivered <- true;
        let consumed = Under_test.read_eof t.conn bs ~off:0 ~len in
        t.pending <- String.sub t.pending consumed (len - consumed);
        true)
      else if len > 0
      then (
        let consumed = Under_test.read t.conn bs ~off:0 ~len in
        t.pending <- String.sub t.pending consumed (len - consumed);
        consumed > 0)
      else false
    | `Yield ->
      t.read_op <- `Yield;
      if not t.reader_parked
      then (
        t.reader_parked <- true;
        Under_test.yield_reader t.conn (fun () -> t.reader_parked <- false));
      false
    | `Close ->
      t.read_op <- `Close;
      false

  let write_step t =
    match Under_test.next_write_operation t.conn with
    | `Write iovecs ->
      t.write_op <- `Write;
      let s = iovecs_to_string iovecs in
      Buffer.add_string t.output s;
      Under_test.report_write_result t.conn (`Ok (String.length s));
      true
    | `Yield ->
      t.write_op <- `Yield;
      if not t.writer_parked
      then (
        t.writer_parked <- true;
        Under_test.yield_writer t.conn (fun () -> t.writer_parked <- false));
      false
    | `Close _ ->
      t.write_op <- `Close;
      false

  let rec pump t =
    let read_progress = read_step t in
    let write_progress = write_step t in
    if read_progress || write_progress then pump t

  let feed t s =
    t.pending <- t.pending ^ s;
    pump t

  let close_input t =
    t.eof_requested <- true;
    pump t

  let output t = Buffer.contents t.output

  let take_output t =
    let s = Buffer.contents t.output in
    Buffer.clear t.output;
    s

  let unconsumed t = t.pending
  let is_closed t = t.read_op = `Close && t.write_op = `Close
end

module Wire = struct
  type message =
    { start_line : string
    ; headers : (string * string) list
    ; body : string
    }

  let index_from s i sub =
    let n = String.length s
    and m = String.length sub in
    let rec go i =
      if i + m > n
      then None
      else if String.sub s i m = sub
      then Some i
      else go (i + 1)
    in
    go i

  let split_head s =
    match index_from s 0 "\r\n\r\n" with
    | None -> None
    | Some i ->
      let head = String.sub s 0 i in
      let rest = String.sub s (i + 4) (String.length s - i - 4) in
      (match String.split_on_char '\n' head with
      | [] -> None
      | start :: lines ->
        let strip_cr l =
          if String.length l > 0 && l.[String.length l - 1] = '\r'
          then String.sub l 0 (String.length l - 1)
          else l
        in
        let header l =
          let l = strip_cr l in
          match String.index_opt l ':' with
          | None -> l, ""
          | Some j ->
            ( String.sub l 0 j
            , String.trim (String.sub l (j + 1) (String.length l - j - 1)) )
        in
        Some (strip_cr start, List.map header lines, rest))

  let header m name =
    List.find_map
      (fun (n, v) -> if Headers.ci_equal n name then Some v else None)
      m.headers

  let has_header m name = Option.is_some (header m name)

  let status m =
    match String.split_on_char ' ' m.start_line with
    | _ :: code :: _ -> int_of_string code
    | _ -> Alcotest.failf "not a status line: %S" m.start_line

  let decode_chunked s =
    let buf = Buffer.create 64 in
    let rec go pos =
      match index_from s pos "\r\n" with
      | None -> None
      | Some eol ->
        let line = String.sub s pos (eol - pos) in
        let size_str =
          match String.index_opt line ';' with
          | Some i -> String.sub line 0 i
          | None -> line
        in
        let size = int_of_string ("0x" ^ String.trim size_str) in
        if size = 0
        then
          let rec trailers p =
            match index_from s p "\r\n" with
            | None -> None
            | Some e when e = p -> Some (e + 2)
            | Some e -> trailers (e + 2)
          in
          Option.map
            (fun stop -> Buffer.contents buf, stop)
            (trailers (eol + 2))
        else (
          Buffer.add_string buf (String.sub s (eol + 2) size);
          go (eol + 2 + size + 2))
    in
    go 0

  let parse_responses ?(methods = []) s =
    let rec go acc i s =
      match split_head s with
      | None -> List.rev acc, s
      | Some (start_line, headers, rest) ->
        let m0 = { start_line; headers; body = "" } in
        let code = status m0 in
        let meth = List.nth_opt methods i |> Option.value ~default:`GET in
        let interim = code >= 100 && code < 200 in
        let no_body = interim || code = 204 || code = 304 || meth = `HEAD in
        let next_i = if interim then i else i + 1 in
        if no_body
        then go (m0 :: acc) next_i rest
        else (
          match header m0 "transfer-encoding", header m0 "content-length" with
          | Some te, _
            when Headers.ci_equal
                   (String.trim
                      (List.hd (List.rev (String.split_on_char ',' te))))
                   "chunked" ->
            (match decode_chunked rest with
            | Some (body, stop) ->
              go
                ({ m0 with body } :: acc)
                next_i
                (String.sub rest stop (String.length rest - stop))
            | None -> List.rev ({ m0 with body = rest } :: acc), "")
          | _, Some cl ->
            let n = int_of_string cl in
            let n = min n (String.length rest) in
            go
              ({ m0 with body = String.sub rest 0 n } :: acc)
              next_i
              (String.sub rest n (String.length rest - n))
          | _, None -> List.rev ({ m0 with body = rest } :: acc), "")
    in
    go [] 0 s

  let response ?methods s =
    match parse_responses ?methods s with
    | [ r ], "" -> r
    | rs, rest ->
      Alcotest.failf
        "expected exactly one response, got %d (unparsed %S) in %S"
        (List.length rs)
        rest
        s

  let statuses ?methods s = List.map status (fst (parse_responses ?methods s))
end

let contains s sub = Option.is_some (Wire.index_from s 0 sub)

module Server = struct
  module Driver = Driver (Server_connection)

  type exchange =
    { reqd : Reqd.t
    ; request : Request.t
    ; mutable body : string
    ; mutable body_complete : bool
    ; mutable body_cut : bool
    }

  type t =
    { driver : Driver.t
    ; mutable exchanges : exchange list
    ; mutable errors : Server_connection.error list
    }

  let read_body ex ~on_complete =
    let body = Reqd.request_body ex.reqd in
    let rec loop () =
      Body.Reader.schedule_read
        body
        ~on_eof:(fun () ->
          ex.body_complete <- true;
          on_complete ex.body)
        ~on_error:(fun () -> ex.body_cut <- true)
        ~on_read:(fun bs ~off ~len ->
          ex.body <- ex.body ^ Ache.Bigstring.substring bs ~off ~len;
          loop ())
    in
    loop ()

  let respond ?(headers = []) ?(body = "") status ex =
    let headers =
      if List.exists (fun (n, _) -> Headers.ci_equal n "content-length") headers
      then headers
      else headers @ [ "content-length", string_of_int (String.length body) ]
    in
    Reqd.respond_with_string
      ex.reqd
      (Response.create ~headers:(Headers.of_list headers) status)
      body

  let respond_raw ?(headers = []) ?(body = "") status ex =
    Reqd.respond_with_string
      ex.reqd
      (Response.create ~headers:(Headers.of_list headers) status)
      body

  let send_continue ex =
    Reqd.respond_with_informational ex.reqd (Response.create `Continue)

  let respond_after_body ?headers ?body status ex =
    read_body ex ~on_complete:(fun _ -> respond ?headers ?body status ex)

  let echo ex = read_body ex ~on_complete:(fun body -> respond ~body `OK ex)

  let default_error_handler ?request:_ _error respond =
    let body = respond Headers.empty in
    Body.Writer.write_string body "error";
    Body.Writer.close body

  let create
        ?config
        ?(handler = echo)
        ?(error_handler = default_error_handler)
        ()
    =
    let state = ref None in
    let get () = Option.get !state in
    let request_handler reqd =
      let s = get () in
      let ex =
        { reqd
        ; request = Reqd.request reqd
        ; body = ""
        ; body_complete = false
        ; body_cut = false
        }
      in
      s.exchanges <- s.exchanges @ [ ex ];
      handler ex
    in
    let error_handler ?request error respond =
      let s = get () in
      s.errors <- s.errors @ [ error ];
      error_handler ?request error respond
    in
    let conn =
      Server_connection.create ?config ~error_handler request_handler
    in
    let s = { driver = Driver.make conn; exchanges = []; errors = [] } in
    state := Some s;
    s

  let run ?config ?handler ?error_handler ?(eof = false) input =
    let s = create ?config ?handler ?error_handler () in
    Driver.feed s.driver input;
    if eof then Driver.close_input s.driver;
    s

  let feed s input = Driver.feed s.driver input
  let close_input s = Driver.close_input s.driver
  let pump s = Driver.pump s.driver
  let output s = Driver.output s.driver
  let take_output s = Driver.take_output s.driver
  let read_op s = s.driver.read_op
  let write_op s = s.driver.write_op
  let is_closed s = Driver.is_closed s.driver
  let unconsumed s = Driver.unconsumed s.driver
  let handler_calls s = List.length s.exchanges
  let requests s = List.map (fun ex -> ex.request) s.exchanges
  let statuses ?methods s = Wire.statuses ?methods (output s)
  let response ?methods s = Wire.response ?methods (output s)
end

module Client = struct
  module Driver = Driver (Client_connection)

  type exchange =
    { request : Request.t
    ; writer : Body.Writer.t
    ; mutable responses : Response.t list
    ; mutable body : string
    ; mutable body_eof : bool
    ; mutable body_cut : bool
    ; mutable errors : string list
    }

  type t = { driver : Driver.t }

  let error_to_string = function
    | `Malformed_response _ -> "Malformed_response"
    | `Invalid_response_body_length _ -> "Invalid_response_body_length"
    | `Connection_closed -> "Connection_closed"
    | `Exn exn -> "Exn " ^ Printexc.to_string exn

  let create ?config () =
    { driver = Driver.make (Client_connection.create ?config ()) }

  let request ?body ?(close_body = true) t request =
    let rec ex =
      lazy
        (let response_handler response reader =
           let ex = Lazy.force ex in
           ex.responses <- ex.responses @ [ response ];
           let rec loop () =
             Body.Reader.schedule_read
               reader
               ~on_eof:(fun () -> ex.body_eof <- true)
               ~on_error:(fun () -> ex.body_cut <- true)
               ~on_read:(fun bs ~off ~len ->
                 ex.body <- ex.body ^ Ache.Bigstring.substring bs ~off ~len;
                 loop ())
           in
           loop ()
         in
         let error_handler error =
           let ex = Lazy.force ex in
           ex.errors <- ex.errors @ [ error_to_string error ]
         in
         let writer =
           Client_connection.request
             t.driver.conn
             ~flush_headers_immediately:true
             request
             ~error_handler
             ~response_handler
         in
         { request
         ; writer
         ; responses = []
         ; body = ""
         ; body_eof = false
         ; body_cut = false
         ; errors = []
         })
    in
    let ex = Lazy.force ex in
    Option.iter (fun b -> Body.Writer.write_string ex.writer b) body;
    if close_body then Body.Writer.close ex.writer;
    Driver.pump t.driver;
    ex

  let feed t input = Driver.feed t.driver input
  let close_input t = Driver.close_input t.driver
  let pump t = Driver.pump t.driver
  let written t = Driver.output t.driver
  let take_written t = Driver.take_output t.driver
  let read_op t = t.driver.read_op
  let write_op t = t.driver.write_op
  let is_closed t = Driver.is_closed t.driver
  let unconsumed t = Driver.unconsumed t.driver

  let statuses ex =
    List.map (fun r -> Status.to_code r.Response.status) ex.responses
end

let req
      ?(version = "HTTP/1.1")
      ?(headers = [ "Host", "example.com" ])
      ?(body = "")
      meth
      target
  =
  let lines = List.map (fun (n, v) -> n ^ ": " ^ v ^ "\r\n") headers in
  Printf.sprintf
    "%s %s %s\r\n%s\r\n%s"
    meth
    target
    version
    (String.concat "" lines)
    body

let get ?headers target = req ?headers "GET" target
let int_list = Alcotest.(list int)

let check_status ?methods msg expected s =
  Alcotest.check int_list msg expected (Server.statuses ?methods s)

let check_bad_request ?(msg = "400 then close") s =
  Alcotest.check int_list msg [ 400 ] (Server.statuses s);
  Alcotest.(check int) "request handler not called" 0 (Server.handler_calls s);
  Alcotest.check read_op "reader closed" `Close (Server.read_op s)

let check_closed msg s = Alcotest.(check bool) msg true (Server.is_closed s)
let check_open msg s = Alcotest.check read_op msg `Read (Server.read_op s)
let targets s = List.map (fun (r : Request.t) -> r.target) (Server.requests s)

let check_rejected ?(allowed = [ 400 ]) s =
  (match Server.statuses s with
  | [ code ] when List.mem code allowed -> ()
  | codes ->
    Alcotest.failf
      "expected one response with a status in [%s], got [%s] in %S"
      (String.concat "; " (List.map string_of_int allowed))
      (String.concat "; " (List.map string_of_int codes))
      (Server.output s));
  Alcotest.(check int) "request handler not called" 0 (Server.handler_calls s);
  check_closed "connection closed" s

let check_not_dispatched target s =
  if List.mem target (targets s)
  then
    Alcotest.failf
      "%s reached the request handler; targets: [%s]"
      target
      (String.concat "; " (targets s))
