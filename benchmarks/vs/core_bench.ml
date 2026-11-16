module type CONN = sig
  type t
  type iovec

  val next_read_operation : t -> [ `Read | `Yield | `Close ]
  val read : t -> Ache.Bigstring.t -> off:int -> len:int -> int

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

module Drive (Connection : CONN) = struct
  let rec drain conn capture written =
    match Connection.next_write_operation conn with
    | `Write iovecs ->
      let n = Connection.lengthv iovecs in
      (match capture with Some b -> Connection.capture b iovecs | None -> ());
      Connection.report_write_result conn (`Ok n);
      drain conn capture (written + n)
    | `Yield | `Close _ -> written

  let feed ~first conn input capture =
    let total = Ache.Bigstring.length input in
    let rec go off want stalled written =
      if off >= total
      then drain conn capture written
      else
        match Connection.next_read_operation conn with
        | `Read ->
          let len = min want (total - off) in
          let n = Connection.read conn input ~off ~len in
          let written = drain conn capture written in
          if n > 0
          then go (off + n) window 0 written
          else if len < total - off
          then go off (if want < window then window else want * 2) 0 written
          else failwith "the connection does not consume its input"
        | `Yield ->
          let written = drain conn capture written in
          if stalled > 2
          then failwith "the connection does not read"
          else go off want (stalled + 1) written
        | `Close -> failwith "the connection closed"
    in
    go 0 first 0 0
end

module Ache_lib : LIB = struct
  open Ache

  let name = "ache"

  module Server = struct
    include Server_connection

    type iovec = Ache.Bigstring.t IOVec.t

    let lengthv = IOVec.lengthv

    let capture b =
      List.iter (fun { IOVec.buffer; off; len } ->
        Buffer.add_string b (Ache.Bigstring.substring buffer ~off ~len))
  end

  module Client = struct
    include Client_connection

    type iovec = Ache.Bigstring.t IOVec.t

    let lengthv = IOVec.lengthv

    let capture b =
      List.iter (fun { IOVec.buffer; off; len } ->
        Buffer.add_string b (Ache.Bigstring.substring buffer ~off ~len))
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

    type iovec = Ache.Bigstring.t IOVec.t

    let lengthv = IOVec.lengthv

    let capture b =
      List.iter (fun { IOVec.buffer; off; len } ->
        Buffer.add_string b (Ache.Bigstring.substring buffer ~off ~len))
  end

  module Client = struct
    include Client_connection

    type iovec = Ache.Bigstring.t IOVec.t

    let lengthv = IOVec.lengthv

    let capture b =
      List.iter (fun { IOVec.buffer; off; len } ->
        Buffer.add_string b (Ache.Bigstring.substring buffer ~off ~len))
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

    type iovec = Ache.Bigstring.t IOVec.t

    let lengthv = IOVec.lengthv

    let capture b =
      List.iter (fun { IOVec.buffer; off; len } ->
        Buffer.add_string b (Ache.Bigstring.substring buffer ~off ~len))
  end

  module Client = struct
    include Client_connection

    type iovec = Ache.Bigstring.t IOVec.t

    let next_read_operation t =
      (next_read_operation t :> [ `Read | `Yield | `Close ])

    let lengthv = IOVec.lengthv

    let capture b =
      List.iter (fun { IOVec.buffer; off; len } ->
        Buffer.add_string b (Ache.Bigstring.substring buffer ~off ~len))
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
  ; first_read : int
  ; responses : int
  }

let kib64 = 65536

let browser_head =
  let path = "/wp-content/uploads/2010/03/hello-kitty-darth-vader-pink.jpg" in
  Printf.sprintf
    "GET /fixed/12?%s HTTP/1.1\r\n\
     Host: www.kittyhell.com\r\n\
     User-Agent: Mozilla/5.0 (Macintosh; U; Intel Mac OS X 10.6; ja-JP-mac; \
     rv:1.9.2.3) Gecko/20100401 Firefox/3.6.3 Pathtraq/0.9\r\n\
     Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8\r\n\
     Accept-Language: ja,en-us;q=0.7,en;q=0.3\r\n\
     Accept-Encoding: gzip,deflate\r\n\
     Accept-Charset: Shift_JIS,utf-8;q=0.7,*;q=0.7\r\n\
     Keep-Alive: 115\r\n\
     Connection: keep-alive\r\n\
     \r\n"
    (String.sub path 10 (String.length path - 10))

let times n s = String.concat "" (List.init n (fun _ -> s))

let post_content_length size =
  Printf.sprintf
    "POST /sink HTTP/1.1\r\nhost: localhost\r\ncontent-length: %d\r\n\r\n%s"
    size
    (String.make size 'x')

let scenarios =
  [ { id = "get_small"
    ; side = Server_side (get "/fixed/12" "")
    ; expect_suffix = String.make 12 'x'
    ; expect_body = 0
    ; first_read = window
    ; responses = 1
    }
  ; { id = "get_32_fields"
    ; side = Server_side (get "/fixed/12" (fields 31))
    ; expect_suffix = String.make 12 'x'
    ; expect_body = 0
    ; first_read = window
    ; responses = 1
    }
  ; { id = "get_small_split_head"
    ; side = Server_side (get "/fixed/12" "")
    ; expect_suffix = String.make 12 'x'
    ; expect_body = 0
    ; first_read = String.length (get "/fixed/12" "") / 2
    ; responses = 1
    }
  ; { id = "get_small_pipelined_8"
    ; side = Server_side (times 8 (get "/fixed/12" ""))
    ; expect_suffix = String.make 12 'x'
    ; expect_body = 0
    ; first_read = window
    ; responses = 8
    }
  ; { id = "get_browser_head"
    ; side = Server_side browser_head
    ; expect_suffix = String.make 12 'x'
    ; expect_body = 0
    ; first_read = window
    ; responses = 1
    }
  ; { id = "get_browser_head_pipelined_8"
    ; side = Server_side (times 8 browser_head)
    ; expect_suffix = String.make 12 'x'
    ; expect_body = 0
    ; first_read = window
    ; responses = 8
    }
  ; { id = "post_1k_content_length"
    ; side = Server_side (post_content_length 1024)
    ; expect_suffix = "\r\n\r\n1024"
    ; expect_body = 0
    ; first_read = window
    ; responses = 1
    }
  ; { id = "post_64k_content_length"
    ; side = Server_side (post_content_length kib64)
    ; expect_suffix = "\r\n\r\n65536"
    ; expect_body = 0
    ; first_read = window
    ; responses = 1
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
    ; first_read = window
    ; responses = 1
    }
  ; { id = "get_fixed_64k"
    ; side = Server_side (get "/fixed/65536" "")
    ; expect_suffix = "\r\n\r\n" ^ String.make kib64 'x'
    ; expect_body = 0
    ; first_read = window
    ; responses = 1
    }
  ; { id = "get_stream_chunked_16x4k"
    ; side = Server_side (get "/stream/4096/16" "")
    ; expect_suffix = "x\r\n0\r\n\r\n"
    ; expect_body = 0
    ; first_read = window
    ; responses = 1
    }
  ; { id = "client_fixed_12"
    ; side =
        Client_side "HTTP/1.1 200 OK\r\ncontent-length: 12\r\n\r\nxxxxxxxxxxxx"
    ; expect_suffix = "\r\n\r\n"
    ; expect_body = 12
    ; first_read = window
    ; responses = 1
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
    ; first_read = window
    ; responses = 1
    }
  ; { id = "client_chunked_16x4k"
    ; side =
        Client_side
          ("HTTP/1.1 200 OK\r\ntransfer-encoding: chunked\r\n\r\n"
          ^ chunked_body kib64 4096)
    ; expect_suffix = "\r\n\r\n"
    ; expect_body = kib64
    ; first_read = window
    ; responses = 1
    }
  ]

let bigstring_of_string s =
  Ache.Bigstring.of_string s ~off:0 ~len:(String.length s)

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

let runner (module Lib : LIB) scenario =
  let first = scenario.first_read in
  match scenario.side with
  | Server_side input ->
    let module Bench = Drive (Lib.Server) in
    let input = bigstring_of_string input in
    let conn = Lib.server () in
    { iteration = (fun () -> Bench.feed ~first conn input None)
    ; check =
        (fun () ->
          let b = Buffer.create 4096 in
          ignore (Bench.feed ~first conn input (Some b) : int);
          Buffer.contents b)
    }
  | Client_side input ->
    let module Bench = Drive (Lib.Client) in
    let input = bigstring_of_string input in
    let received = ref 0
    and finished = ref false in
    let on_read len = received := !received + len
    and on_eof () = finished := true in
    let exchange capture =
      received := 0;
      finished := false;
      let conn = Lib.get ~on_read ~on_eof in
      let written = Bench.drain conn capture 0 in
      let _ = Bench.feed ~first conn input None in
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

let count_occurrences sub s =
  let re = Str.regexp_string sub in
  let rec go i n =
    match Str.search_forward re s i with
    | j -> go (j + 1) (n + 1)
    | exception Not_found -> n
  in
  go 0 0

let check_output lib scenario r =
  let first = r.check () in
  let second = r.check () in
  (match scenario.side with
  | Server_side _ ->
    let ok = "HTTP/1.1 200 OK\r\n" in
    if
      not
        (String.starts_with ~prefix:ok first
        && ends_with ~suffix:scenario.expect_suffix first)
    then failwith (Printf.sprintf "%s %s: unexpected response" lib scenario.id);
    let n = count_occurrences ok first in
    if n <> scenario.responses
    then
      failwith
        (Printf.sprintf
           "%s %s: %d responses, expected %d"
           lib
           scenario.id
           n
           scenario.responses)
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

type measured =
  { lib : string
  ; r : runner
  ; output : string
  ; n : int
  ; ns : float list ref
  ; words : float list ref
  }

let attempt f =
  match f () with
  | v -> Ok v
  | exception Failure m -> Error m
  | exception e -> Error (Printexc.to_string e)

let exit_with_usage_error fmt =
  Printf.ksprintf
    (fun m ->
       prerr_endline m;
       exit 2)
    fmt

let read_bounds file =
  let lines =
    try In_channel.with_open_text file In_channel.input_lines with
    | Sys_error m -> exit_with_usage_error "%s" m
  in
  List.concat
    (List.mapi
       (fun i line ->
          match Str.split (Str.regexp "[ \t]+") line with
          | [] -> []
          | word :: _ when word.[0] = '#' -> []
          | [ id; bound ] ->
            (match float_of_string_opt bound with
            | Some b when Float.is_finite b && b > 0. -> [ id, b ]
            | Some _ | None ->
              exit_with_usage_error
                "%s:%d: %S is not a positive number"
                file
                (i + 1)
                bound)
          | _ ->
            exit_with_usage_error
              "%s:%d: expected \"scenario max_ratio\""
              file
              (i + 1))
       lines)

let check_bounds file bounds =
  let ids = List.map (fun s -> s.id) scenarios in
  let problems =
    List.filter_map
      (fun id ->
         if List.mem_assoc id bounds
         then None
         else Some (Printf.sprintf "no bound for scenario %s" id))
      ids
    @ List.filter_map
        (fun (id, _) ->
           if List.mem id ids
           then None
           else Some (Printf.sprintf "bound for unknown scenario %s" id))
        bounds
    @ List.filter_map
        (fun id ->
           if List.length (List.filter (fun (i, _) -> i = id) bounds) > 1
           then Some (Printf.sprintf "more than one bound for scenario %s" id)
           else None)
        ids
  in
  if problems <> []
  then (
    List.iter (fun p -> Printf.eprintf "%s: %s\n" file p) problems;
    exit 2)

let paired_ratio ache other = median (List.map2 ( /. ) !(ache.ns) !(other.ns))

let print_check bounds prepared =
  Printf.printf
    "%-30s %9s %9s %9s %6s  %s\n"
    "scenario"
    "ache ns"
    "vs httpun"
    "vs httpaf"
    "bound"
    "verdict";
  let faster = ref 0
  and known = ref 0
  and failed = ref 0 in
  List.iter
    (fun (scenario, runs) ->
       let result lib = !(List.assoc lib runs) in
       let ache = result "ache" in
       let against lib =
         match ache, result lib with
         | Ok a, Ok b -> Some (paired_ratio a b)
         | _ -> None
       in
       let httpun = against "httpun"
       and httpaf = against "httpaf" in
       let bound = List.assoc scenario.id bounds in
       let ratio =
         match httpun, httpaf with
         | Some a, Some b -> Some (Float.max a b)
         | _ -> None
       in
       let verdict =
         match ratio with
         | Some r when r > bound ->
           incr failed;
           "FAIL"
         | None ->
           incr failed;
           "FAIL (no ratio)"
         | Some r when r < 1. ->
           incr faster;
           "faster"
         | Some _ ->
           incr known;
           "slower, known"
       in
       let cell = function
         | Some r -> Printf.sprintf "%.2f" r
         | None -> "failed"
       in
       Printf.printf
         "%-30s %9s %9s %9s %6.2f  %s\n"
         scenario.id
         (match ache with
         | Ok a -> Printf.sprintf "%.0f" (median !(a.ns))
         | Error _ -> "failed")
         (cell httpun)
         (cell httpaf)
         bound
         verdict)
    prepared;
  Printf.printf
    "%d faster, %d slower (known), %d failed\n"
    !faster
    !known
    !failed;
  !failed = 0

let print_failures prepared =
  List.iter
    (fun (scenario, runs) ->
       List.iter
         (fun (lib, result) ->
            match !result with
            | Ok _ -> ()
            | Error reason ->
              Printf.printf "%s %s failed: %s\n" lib scenario.id reason)
         runs)
    prepared

let () =
  let reps = ref 7
  and target = ref 0.3
  and json = ref false
  and check = ref ""
  and only = ref ""
  and only_libs = ref "" in
  Arg.parse
    [ "--reps", Arg.Set_int reps, " Repetitions per scenario and library (7)"
    ; "--time", Arg.Set_float target, " Seconds per repetition (0.3)"
    ; "--json", Arg.Set json, " Print JSON instead of a table"
    ; ( "--check"
      , Arg.Set_string check
      , "FILE Compare ache with the faster baseline against the bounds in FILE"
      )
    ; "--only", Arg.Set_string only, " Regexp on scenario ids (Str syntax)"
    ; "--libs", Arg.Set_string only_libs, " Regexp on library names"
    ]
    (fun arg -> raise (Arg.Bad arg))
    "In-process benchmark of ache, httpun and httpaf connections.";
  let bounds =
    if !check = ""
    then []
    else (
      if !only_libs <> ""
      then
        exit_with_usage_error "--check needs all three libraries; drop --libs";
      let bounds = read_bounds !check in
      check_bounds !check bounds;
      bounds)
  in
  let matches re s = re = "" || Str.string_match (Str.regexp re) s 0 in
  let scenarios = List.filter (fun s -> matches !only s.id) scenarios in
  let libs =
    List.filter (fun (module Lib : LIB) -> matches !only_libs Lib.name) libs
  in
  let prepared =
    List.map
      (fun scenario ->
         ( scenario
         , List.map
             (fun ((module Lib : LIB) as lib) ->
                ( Lib.name
                , ref
                    (attempt (fun () ->
                       let r = runner lib scenario in
                       let output = check_output Lib.name scenario r in
                       { lib = Lib.name
                       ; r
                       ; output
                       ; n = calibrate r !target
                       ; ns = ref []
                       ; words = ref []
                       })) ))
             libs ))
      scenarios
  in
  for rep = 1 to !reps do
    List.iter
      (fun (_, runs) ->
         let k = List.length runs in
         List.iteri
           (fun i _ ->
              let _, result = List.nth runs ((i + rep) mod k) in
              match !result with
              | Error _ -> ()
              | Ok m ->
                (match attempt (fun () -> time_block m.r m.n) with
                | Ok (t, w, _) ->
                  m.ns := t :: !(m.ns);
                  m.words := w :: !(m.words)
                | Error reason -> result := Error reason))
           runs)
      prepared
  done;
  List.iter
    (fun (scenario, runs) ->
       List.iter
         (fun (_, result) ->
            match !result with
            | Error _ -> ()
            | Ok m ->
              (match attempt (fun () -> check_output m.lib scenario m.r) with
              | Ok output when output = m.output -> ()
              | Ok _ -> result := Error "end state differs"
              | Error reason -> result := Error reason))
         runs)
    prepared;
  if !check <> ""
  then (
    if prepared = []
    then exit_with_usage_error "--only %S matches no scenario" !only;
    let passed = print_check bounds prepared in
    print_failures prepared;
    if not passed then exit 1)
  else if !json
  then
    let rows =
      List.concat_map
        (fun (scenario, runs) ->
           List.filter_map
             (fun (_, result) ->
                match !result with
                | Error _ -> None
                | Ok { lib; output; n; ns; words; _ } ->
                  Some
                    (Printf.sprintf
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
                       (json_string (head_of output))))
             runs)
        prepared
    in
    let failed =
      List.concat_map
        (fun (scenario, runs) ->
           List.filter_map
             (fun (lib, result) ->
                match !result with
                | Ok _ -> None
                | Error reason ->
                  Some
                    (Printf.sprintf
                       {|{"scenario":%s,"lib":%s,"reason":%s}|}
                       (json_string scenario.id)
                       (json_string lib)
                       (json_string reason)))
             runs)
        prepared
    in
    Printf.printf
      "{\"reps\":%d,\"seconds_per_rep\":%g,\"window\":%d,\"results\":[%s],\"failed\":[%s]}\n"
      !reps
      !target
      window
      (String.concat "," rows)
      (String.concat "," failed)
  else (
    Printf.printf
      "%-30s %-7s %12s %12s %12s\n"
      "scenario"
      "lib"
      "ns/msg med"
      "ns/msg min"
      "words/msg";
    List.iter
      (fun (scenario, runs) ->
         List.iter
           (fun (_, result) ->
              match !result with
              | Error _ -> ()
              | Ok { lib; ns; words; _ } ->
                Printf.printf
                  "%-30s %-7s %12.0f %12.0f %12.1f\n"
                  scenario.id
                  lib
                  (median !ns)
                  (minimum !ns)
                  (median !words))
           runs)
      prepared;
    print_failures prepared)
