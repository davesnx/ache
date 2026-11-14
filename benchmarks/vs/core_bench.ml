module type CONN = sig
  type t
  type iovec

  val next_read_operation : t -> [ `Read | `Yield | `Close ]
  val read : t -> Bigstringaf.t -> off:int -> len:int -> int

  val next_write_operation :
     t
    -> [ `Write of iovec list | `Yield | `Close of int ]

  val report_write_result : t -> [ `Ok of int | `Closed ] -> unit
  val lengthv : iovec list -> int
  val capture : Buffer.t -> iovec list -> unit
end

module type LIB = sig
  val name : string

  module Server : CONN
  module Client : CONN

  val server : unit -> Server.t
  val get : on_read:(int -> unit) -> on_eof:(unit -> unit) -> Client.t
end

let window = 4096

module Drive (C : CONN) = struct
  let rec drain conn capture written =
    match C.next_write_operation conn with
    | `Write iovecs ->
      let n = C.lengthv iovecs in
      (match capture with Some b -> C.capture b iovecs | None -> ());
      C.report_write_result conn (`Ok n);
      drain conn capture (written + n)
    | `Yield | `Close _ -> written

  let feed conn input capture =
    let total = Bigstringaf.length input in
    let rec go off want stalled written =
      if off >= total
      then drain conn capture written
      else
        match C.next_read_operation conn with
        | `Read ->
          let len = min want (total - off) in
          let n = C.read conn input ~off ~len in
          let written = drain conn capture written in
          if n > 0
          then go (off + n) window 0 written
          else if len < total - off
          then go off (want * 2) 0 written
          else failwith "the connection does not consume its input"
        | `Yield ->
          let written = drain conn capture written in
          if stalled > 2
          then failwith "the connection does not read"
          else go off want (stalled + 1) written
        | `Close -> failwith "the connection closed"
    in
    go 0 window 0 0
end

module Ache_lib : LIB = struct
  open Ache

  let name = "ache"

  module Server = struct
    include Server_connection

    type iovec = Bigstringaf.t IOVec.t

    let lengthv = IOVec.lengthv

    let capture b =
      List.iter (fun { IOVec.buffer; off; len } ->
        Buffer.add_string b (Bigstringaf.substring buffer ~off ~len))
  end

  module Client = struct
    include Client_connection

    type iovec = Bigstringaf.t IOVec.t

    let lengthv = IOVec.lengthv

    let capture b =
      List.iter (fun { IOVec.buffer; off; len } ->
        Buffer.add_string b (Bigstringaf.substring buffer ~off ~len))
  end

  let server () =
    Server_connection.create
      ~error_handler:Vs_ache.error_handler
      Vs_ache.Handler.handle

  let request =
    Request.create ~headers:(Headers.of_list [ "host", "localhost" ]) `GET "/"

  let get ~on_read ~on_eof =
    let conn = Client_connection.create () in
    let rec read body =
      Body.Reader.schedule_read
        body
        ~on_eof
        ~on_error:(fun () -> failwith "response body error")
        ~on_read:(fun _ ~off:_ ~len ->
          on_read len;
          read body)
    in
    let body =
      Client_connection.request
        conn
        request
        ~error_handler:(fun _ -> failwith "response error")
        ~response_handler:(fun _ body -> read body)
    in
    Body.Writer.close body;
    conn
end

module Httpun_lib : LIB = struct
  open Httpun

  let name = "httpun"

  module Server = struct
    include Server_connection

    type iovec = Bigstringaf.t IOVec.t

    let lengthv = IOVec.lengthv

    let capture b =
      List.iter (fun { IOVec.buffer; off; len } ->
        Buffer.add_string b (Bigstringaf.substring buffer ~off ~len))
  end

  module Client = struct
    include Client_connection

    type iovec = Bigstringaf.t IOVec.t

    let lengthv = IOVec.lengthv

    let capture b =
      List.iter (fun { IOVec.buffer; off; len } ->
        Buffer.add_string b (Bigstringaf.substring buffer ~off ~len))
  end

  let server () =
    Server_connection.create
      ~error_handler:Vs_httpun.error_handler
      Vs_httpun.Handler.handle

  let request =
    Request.create ~headers:(Headers.of_list [ "host", "localhost" ]) `GET "/"

  let get ~on_read ~on_eof =
    let conn = Client_connection.create () in
    let rec read body =
      Body.Reader.schedule_read body ~on_eof ~on_read:(fun _ ~off:_ ~len ->
        on_read len;
        read body)
    in
    let body =
      Client_connection.request
        conn
        request
        ~error_handler:(fun _ -> failwith "response error")
        ~response_handler:(fun _ body -> read body)
    in
    Body.Writer.close body;
    conn
end

module Httpaf_lib : LIB = struct
  open Httpaf

  let name = "httpaf"

  module Server = struct
    include Server_connection

    type iovec = Bigstringaf.t IOVec.t

    let lengthv = IOVec.lengthv

    let capture b =
      List.iter (fun { IOVec.buffer; off; len } ->
        Buffer.add_string b (Bigstringaf.substring buffer ~off ~len))
  end

  module Client = struct
    include Client_connection

    type iovec = Bigstringaf.t IOVec.t

    let next_read_operation t =
      (next_read_operation t :> [ `Read | `Yield | `Close ])

    let lengthv = IOVec.lengthv

    let capture b =
      List.iter (fun { IOVec.buffer; off; len } ->
        Buffer.add_string b (Bigstringaf.substring buffer ~off ~len))
  end

  let server () =
    Server_connection.create
      ~error_handler:Vs_httpaf.error_handler
      Vs_httpaf.Handler.handle

  let request =
    Request.create ~headers:(Headers.of_list [ "host", "localhost" ]) `GET "/"

  let get ~on_read ~on_eof =
    let rec read body =
      Body.schedule_read body ~on_eof ~on_read:(fun _ ~off:_ ~len ->
        on_read len;
        read body)
    in
    let body, conn =
      Client_connection.request
        request
        ~error_handler:(fun _ -> failwith "response error")
        ~response_handler:(fun _ body -> read body)
    in
    Body.close_writer body;
    conn
end

let libs : (module LIB) list =
  [ (module Ache_lib); (module Httpun_lib); (module Httpaf_lib) ]

let fields n =
  String.concat
    ""
    (List.init n (fun i ->
       Printf.sprintf "X-Bench-%02d: value-%02d-abcdefghij\r\n" (i + 1) (i + 1)))

let get target extra =
  Printf.sprintf "GET %s HTTP/1.1\r\nhost: localhost\r\n%s\r\n" target extra

let chunked_body size chunk =
  let one = Printf.sprintf "%x\r\n%s\r\n" chunk (String.make chunk 'x') in
  String.concat "" (List.init (size / chunk) (fun _ -> one)) ^ "0\r\n\r\n"

type side =
  | Server_side of string
  | Client_side of string

type scenario =
  { id : string
  ; side : side
  ; expect_suffix : string
  ; expect_body : int
  }

let kib64 = 65536

let scenarios =
  [ { id = "get_small"
    ; side = Server_side (get "/fixed/12" "")
    ; expect_suffix = String.make 12 'x'
    ; expect_body = 0
    }
  ; { id = "get_32_fields"
    ; side = Server_side (get "/fixed/12" (fields 31))
    ; expect_suffix = String.make 12 'x'
    ; expect_body = 0
    }
  ; { id = "post_64k_content_length"
    ; side =
        Server_side
          (Printf.sprintf
             "POST /sink HTTP/1.1\r\n\
              host: localhost\r\n\
              content-length: %d\r\n\
              \r\n\
              %s"
             kib64
             (String.make kib64 'x'))
    ; expect_suffix = "\r\n\r\n65536"
    ; expect_body = 0
    }
  ; { id = "post_64k_chunked"
    ; side =
        Server_side
          ("POST /sink HTTP/1.1\r\n\
            host: localhost\r\n\
            transfer-encoding: chunked\r\n\
            \r\n"
          ^ chunked_body kib64 4096)
    ; expect_suffix = "\r\n\r\n65536"
    ; expect_body = 0
    }
  ; { id = "get_fixed_64k"
    ; side = Server_side (get "/fixed/65536" "")
    ; expect_suffix = "\r\n\r\n" ^ String.make kib64 'x'
    ; expect_body = 0
    }
  ; { id = "get_stream_chunked_16x4k"
    ; side = Server_side (get "/stream/4096/16" "")
    ; expect_suffix = "x\r\n0\r\n\r\n"
    ; expect_body = 0
    }
  ; { id = "client_fixed_12"
    ; side =
        Client_side "HTTP/1.1 200 OK\r\ncontent-length: 12\r\n\r\nxxxxxxxxxxxx"
    ; expect_suffix = "\r\n\r\n"
    ; expect_body = 12
    }
  ; { id = "client_fixed_64k"
    ; side =
        Client_side
          (Printf.sprintf
             "HTTP/1.1 200 OK\r\ncontent-length: %d\r\n\r\n%s"
             kib64
             (String.make kib64 'x'))
    ; expect_suffix = "\r\n\r\n"
    ; expect_body = kib64
    }
  ; { id = "client_chunked_16x4k"
    ; side =
        Client_side
          ("HTTP/1.1 200 OK\r\ntransfer-encoding: chunked\r\n\r\n"
          ^ chunked_body kib64 4096)
    ; expect_suffix = "\r\n\r\n"
    ; expect_body = kib64
    }
  ]

let bigstring_of_string s =
  Bigstringaf.of_string s ~off:0 ~len:(String.length s)

let ends_with ~suffix s =
  let n = String.length suffix
  and m = String.length s in
  m >= n && String.sub s (m - n) n = suffix

let head_of s =
  match Str.search_forward (Str.regexp_string "\r\n\r\n") s 0 with
  | i -> String.sub s 0 i
  | exception Not_found -> s

type runner =
  { iteration : unit -> int
  ; check : unit -> string
  }

let runner (module L : LIB) scenario =
  match scenario.side with
  | Server_side input ->
    let module D = Drive (L.Server) in
    let input = bigstring_of_string input in
    let conn = L.server () in
    { iteration = (fun () -> D.feed conn input None)
    ; check =
        (fun () ->
          let b = Buffer.create 4096 in
          ignore (D.feed conn input (Some b) : int);
          Buffer.contents b)
    }
  | Client_side input ->
    let module D = Drive (L.Client) in
    let input = bigstring_of_string input in
    let received = ref 0
    and finished = ref false in
    let on_read len = received := !received + len
    and on_eof () = finished := true in
    let exchange capture =
      received := 0;
      finished := false;
      let conn = L.get ~on_read ~on_eof in
      let written = D.drain conn capture 0 in
      let _ = D.feed conn input None in
      if not !finished then failwith "response not complete";
      written + !received
    in
    { iteration = (fun () -> exchange None)
    ; check =
        (fun () ->
          let b = Buffer.create 256 in
          let _ = exchange (Some b) in
          Printf.bprintf b "\r\n\r\n[body %d bytes]" !received;
          if !received <> scenario.expect_body
          then
            failwith
              (Printf.sprintf
                 "body %d bytes, expected %d"
                 !received
                 scenario.expect_body);
          Buffer.contents b)
    }

let check_output lib scenario r =
  let first = r.check () in
  let second = r.check () in
  (match scenario.side with
  | Server_side _ ->
    if
      not
        (String.starts_with ~prefix:"HTTP/1.1 200 OK\r\n" first
        && ends_with ~suffix:scenario.expect_suffix first)
    then failwith (Printf.sprintf "%s %s: unexpected response" lib scenario.id)
  | Client_side _ -> ());
  if first <> second
  then
    failwith
      (Printf.sprintf
         "%s %s: output differs between iterations"
         lib
         scenario.id);
  first

let time_block r n =
  Gc.full_major ();
  let w0 = Gc.minor_words () in
  let t0 = Unix.gettimeofday () in
  let sink = ref 0 in
  for _ = 1 to n do
    sink := !sink + r.iteration ()
  done;
  let t1 = Unix.gettimeofday () in
  let w1 = Gc.minor_words () in
  (t1 -. t0) *. 1e9 /. float n, (w1 -. w0) /. float n, !sink

let calibrate r target =
  let rec go n =
    let ns, _, _ = time_block r n in
    if ns *. float n >= 0.05e9 || n >= 1 lsl 24
    then max 1 (int_of_float (target *. 1e9 /. ns))
    else go (n * 4)
  in
  go 16

let median l =
  let a = Array.of_list l in
  Array.sort compare a;
  let n = Array.length a in
  if n mod 2 = 1 then a.(n / 2) else (a.((n / 2) - 1) +. a.(n / 2)) /. 2.

let minimum l = List.fold_left min infinity l

let json_string s =
  let b = Buffer.create (String.length s + 2) in
  Buffer.add_char b '"';
  String.iter
    (function
      | '"' -> Buffer.add_string b "\\\""
      | '\\' -> Buffer.add_string b "\\\\"
      | c when Char.code c < 0x20 || Char.code c >= 0x7f ->
        Printf.bprintf b "\\u%04x" (Char.code c)
      | c -> Buffer.add_char b c)
    s;
  Buffer.add_char b '"';
  Buffer.contents b

let json_floats l =
  "[" ^ String.concat "," (List.map (Printf.sprintf "%.2f") l) ^ "]"

let () =
  let reps = ref 7
  and target = ref 0.3
  and json = ref false
  and only = ref ""
  and only_libs = ref "" in
  Arg.parse
    [ "--reps", Arg.Set_int reps, " Repetitions per scenario and library (7)"
    ; "--time", Arg.Set_float target, " Seconds per repetition (0.3)"
    ; "--json", Arg.Set json, " Print JSON instead of a table"
    ; "--only", Arg.Set_string only, " Regexp on scenario ids (Str syntax)"
    ; "--libs", Arg.Set_string only_libs, " Regexp on library names"
    ]
    (fun arg -> raise (Arg.Bad arg))
    "In-process benchmark of ache, httpun and httpaf connections.";
  let matches re s = re = "" || Str.string_match (Str.regexp re) s 0 in
  let scenarios = List.filter (fun s -> matches !only s.id) scenarios in
  let libs =
    List.filter (fun (module L : LIB) -> matches !only_libs L.name) libs
  in
  let prepared =
    List.map
      (fun scenario ->
         ( scenario
         , List.map
             (fun ((module L : LIB) as lib) ->
                let r = runner lib scenario in
                let output = check_output L.name scenario r in
                let n = calibrate r !target in
                L.name, r, output, n, ref [], ref [])
             libs ))
      scenarios
  in
  for rep = 1 to !reps do
    List.iter
      (fun (_, runs) ->
         let k = List.length runs in
         List.iteri
           (fun i _ ->
              let _, r, _, n, ns, words = List.nth runs ((i + rep) mod k) in
              let t, w, _ = time_block r n in
              ns := t :: !ns;
              words := w :: !words)
           runs)
      prepared
  done;
  List.iter
    (fun (scenario, runs) ->
       List.iter
         (fun (lib, r, output, _, _, _) ->
            if check_output lib scenario r <> output
            then
              failwith
                (Printf.sprintf "%s %s: end state differs" lib scenario.id))
         runs)
    prepared;
  if !json
  then
    let rows =
      List.concat_map
        (fun (scenario, runs) ->
           List.map
             (fun (lib, _, output, n, ns, words) ->
                Printf.sprintf
                  {|{"scenario":%s,"lib":%s,"iterations":%d,"ns":%s,"words":%s,"ns_median":%.2f,"ns_min":%.2f,"words_median":%.2f,"bytes":%d,"head":%s}|}
                  (json_string scenario.id)
                  (json_string lib)
                  n
                  (json_floats !ns)
                  (json_floats !words)
                  (median !ns)
                  (minimum !ns)
                  (median !words)
                  (String.length output)
                  (json_string (head_of output)))
             runs)
        prepared
    in
    Printf.printf
      "{\"reps\":%d,\"seconds_per_rep\":%g,\"window\":%d,\"results\":[%s]}\n"
      !reps
      !target
      window
      (String.concat "," rows)
  else (
    Printf.printf
      "%-26s %-7s %12s %12s %12s\n"
      "scenario"
      "lib"
      "ns/msg med"
      "ns/msg min"
      "words/msg";
    List.iter
      (fun (scenario, runs) ->
         List.iter
           (fun (lib, _, _, _, ns, words) ->
              Printf.printf
                "%-26s %-7s %12.0f %12.0f %12.1f\n"
                scenario.id
                lib
                (median !ns)
                (minimum !ns)
                (median !words))
           runs)
      prepared)
