type bigstring =
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

module type HTTP = sig
  type reqd
  type reader
  type writer
  type response
  type flushed

  val target : reqd -> string
  val request_body : reqd -> reader

  val schedule_read :
     reader
    -> on_eof:(unit -> unit)
    -> on_read:(bigstring -> off:int -> len:int -> unit)
    -> unit

  val response : [ `OK | `Not_found ] -> (string * string) list -> response
  val respond_with_string : reqd -> response -> string -> unit
  val respond_with_bigstring : reqd -> response -> bigstring -> unit
  val respond_with_streaming : reqd -> response -> writer
  val write_bigstring : writer -> ?off:int -> ?len:int -> bigstring -> unit
  val flush : writer -> (flushed -> unit) -> unit
  val close_writer : writer -> unit
  val writer_is_closed : writer -> bool

  (* Upgrades [GET /ws] to a WebSocket that echoes each message and counts it in
     [requests]; [None] where the library has no WebSocket. *)
  val websocket_echo : (reqd -> unit) option
end

(* Requests the handler has started in each domain, for [cost.sh] and
   [scale.sh]. A counter per domain, so that domains do not share a cache line
   on every request. *)
let counters = Atomic.make []

let new_counter () =
  let r = ref 0 in
  let rec register () =
    let old = Atomic.get counters in
    if not (Atomic.compare_and_set counters old (r :: old)) then register ()
  in
  register ();
  r

(* The counter of the calling domain, for handlers outside [Make] (the WebSocket
   echo counts each message as a request). *)
let domain_requests = Domain.DLS.new_key new_counter

(* Domains of the server, set by [settings]. *)
let domains = ref 1

(* On [SIGUSR1], one line on stderr with the request count and the GC counters.
   [cost.sh] reads two such lines around its window and divides by the requests
   between them. [Gc.minor_words] counts this domain only; with several domains,
   [Gc.quick_stat] sums them, as sampled at each domain's last minor collection.
   [per_domain] lists the requests of each domain. *)
let print_counters () =
  let s = Gc.quick_stat () in
  let per_domain = List.rev_map (fun r -> !r) (Atomic.get counters) in
  Printf.eprintf
    "vs-counters requests=%d minor_words=%.0f promoted_words=%.0f \
     minor_collections=%d major_collections=%d per_domain=%s\n\
     %!"
    (List.fold_left ( + ) 0 per_domain)
    (if !domains > 1 then s.minor_words else Gc.minor_words ())
    s.promoted_words
    s.minor_collections
    s.major_collections
    (String.concat "/" (List.map string_of_int per_domain))

let () =
  match
    Sys.set_signal Sys.sigusr1 (Sys.Signal_handle (fun _ -> print_counters ()))
  with
  | () -> ()
  | exception Invalid_argument _ -> ()

type settings =
  { port : int
  ; read_buffer_size : int
  ; request_body_buffer_size : int
  ; response_buffer_size : int
  ; response_body_buffer_size : int
  ; timeouts : bool
  ; domains : int option
  }

let settings () =
  let port = ref 8080
  and read_buffer_size = ref 4096
  and request_body_buffer_size = ref 4096
  and response_buffer_size = ref 1024
  and response_body_buffer_size = ref 4096
  and timeouts = ref true
  and domains_flag = ref None in
  Arg.parse
    [ "--port", Arg.Set_int port, " Port on 127.0.0.1 (8080)"
    ; "--read-buffer-size", Arg.Set_int read_buffer_size, " (4096)"
    ; ( "--request-body-buffer-size"
      , Arg.Set_int request_body_buffer_size
      , " (4096)" )
    ; "--response-buffer-size", Arg.Set_int response_buffer_size, " (1024)"
    ; ( "--response-body-buffer-size"
      , Arg.Set_int response_body_buffer_size
      , " (4096)" )
    ; ( "--timeouts"
      , Arg.Symbol ([ "on"; "off" ], fun mode -> timeouts := mode = "on")
      , " Timeouts of the ache servers: the defaults (on), or all off" )
    ; ( "--domains"
      , Arg.Int (fun n -> domains_flag := Some n)
      , " Serve with Server.run on this many domains (ache eio and miou)" )
    ]
    (fun arg -> raise (Arg.Bad arg))
    "Benchmark server. Routes: GET /fixed/<bytes>, GET /string/<bytes>, GET \
     /close/<bytes>, GET /stream/<chunk>/<count>, GET \
     /streamcl/<chunk>/<count>, POST /sink, POST /echo, GET /gc, GET /ws \
     (WebSocket echo).";
  Option.iter (fun n -> domains := n) !domains_flag;
  { port = !port
  ; read_buffer_size = !read_buffer_size
  ; request_body_buffer_size = !request_body_buffer_size
  ; response_buffer_size = !response_buffer_size
  ; response_body_buffer_size = !response_body_buffer_size
  ; timeouts = !timeouts
  ; domains = !domains_flag
  }

module Int_table = Hashtbl.Make (Int)
module String_table = Hashtbl.Make (String)

(* The tables are per domain: a Hashtbl must not be written by two domains at
   once. *)
let bigstrings : bigstring Int_table.t Domain.DLS.key =
  Domain.DLS.new_key (fun () -> Int_table.create 16)

let bigstring size =
  let bigstrings = Domain.DLS.get bigstrings in
  match Int_table.find bigstrings size with
  | b -> b
  | exception Not_found ->
    let b = Bigarray.Array1.create Bigarray.char Bigarray.c_layout size in
    Bigarray.Array1.fill b 'x';
    Int_table.add bigstrings size b;
    b

let strings : string Int_table.t Domain.DLS.key =
  Domain.DLS.new_key (fun () -> Int_table.create 16)

let string size =
  let strings = Domain.DLS.get strings in
  match Int_table.find strings size with
  | s -> s
  | exception Not_found ->
    let s = String.make size 'x' in
    Int_table.add strings size s;
    s

let gc_json () =
  let s = Gc.quick_stat () in
  Printf.sprintf
    {|{"minor_words":%.0f,"promoted_words":%.0f,"major_words":%.0f,"minor_collections":%d,"major_collections":%d,"heap_words":%d,"top_heap_words":%d}|}
    (Gc.minor_words ())
    s.promoted_words
    s.major_words
    s.minor_collections
    s.major_collections
    s.heap_words
    s.top_heap_words

module Make (Http : HTTP) = struct
  type action =
    | Fixed of Http.response * bigstring
    | Copied of Http.response * string
    | Stream of Http.response * bigstring * int
    | Sink
    | Echo
    | Gc_stat
    | Websocket of (Http.reqd -> unit)
    | Missing

  let with_length n = Http.response `OK [ "content-length", string_of_int n ]
  let chunked = Http.response `OK [ "transfer-encoding", "chunked" ]
  let not_found = Http.response `Not_found [ "content-length", "0" ]
  let by_length = Array.init 24 with_length

  let positive s =
    match int_of_string_opt s with Some n when n > 0 -> Some n | _ -> None

  let action_of_target target =
    let path =
      match String.index_opt target '?' with
      | Some i -> String.sub target 0 i
      | None -> target
    in
    match String.split_on_char '/' path with
    | [ ""; "fixed"; n ] ->
      (match int_of_string_opt n with
      | Some n when n >= 0 -> Fixed (with_length n, bigstring n)
      | _ -> Missing)
    | [ ""; "string"; n ] ->
      (match int_of_string_opt n with
      | Some n when n >= 0 -> Copied (with_length n, string n)
      | _ -> Missing)
    | [ ""; "close"; n ] ->
      (match int_of_string_opt n with
      | Some n when n >= 0 ->
        Fixed
          ( Http.response
              `OK
              [ "content-length", string_of_int n; "connection", "close" ]
          , bigstring n )
      | _ -> Missing)
    | [ ""; "stream"; c; k ] ->
      (match positive c, positive k with
      | Some c, Some k -> Stream (chunked, bigstring c, k)
      | _ -> Missing)
    | [ ""; "streamcl"; c; k ] ->
      (match positive c, positive k with
      | Some c, Some k -> Stream (with_length (c * k), bigstring c, k)
      | _ -> Missing)
    | [ ""; "sink" ] -> Sink
    | [ ""; "echo" ] -> Echo
    | [ ""; "gc" ] -> Gc_stat
    | [ ""; "ws" ] ->
      (match Http.websocket_echo with Some f -> Websocket f | None -> Missing)
    | _ -> Missing

  (* State of a domain, behind one [Domain.DLS.get] per request. *)
  type local =
    { requests : int ref
    ; actions : action String_table.t
    }

  let local =
    Domain.DLS.new_key (fun () ->
      { requests = new_counter (); actions = String_table.create 64 })

  let action actions target =
    match String_table.find actions target with
    | a -> a
    | exception Not_found ->
      let a = action_of_target target in
      String_table.add actions target a;
      a

  let stream writer chunk count =
    let rec go i =
      if i = count || Http.writer_is_closed writer
      then Http.close_writer writer
      else (
        Http.write_bigstring writer chunk;
        Http.flush writer (fun _ -> go (i + 1)))
    in
    go 0

  let sink reqd =
    let body = Http.request_body reqd in
    let total = ref 0 in
    let rec on_read _ ~off:_ ~len =
      total := !total + len;
      Http.schedule_read body ~on_eof ~on_read
    and on_eof () =
      let s = string_of_int !total in
      Http.respond_with_string reqd by_length.(String.length s) s
    in
    Http.schedule_read body ~on_eof ~on_read

  let echo reqd =
    let body = Http.request_body reqd in
    let writer = Http.respond_with_streaming reqd chunked in
    let rec on_read bs ~off ~len =
      Http.write_bigstring writer ~off ~len bs;
      Http.schedule_read body ~on_eof ~on_read
    and on_eof () = Http.close_writer writer in
    Http.schedule_read body ~on_eof ~on_read

  let handle reqd =
    let local = Domain.DLS.get local in
    incr local.requests;
    match action local.actions (Http.target reqd) with
    | Fixed (response, body) -> Http.respond_with_bigstring reqd response body
    | Copied (response, body) -> Http.respond_with_string reqd response body
    | Stream (response, chunk, count) ->
      stream (Http.respond_with_streaming reqd response) chunk count
    | Sink -> sink reqd
    | Echo -> echo reqd
    | Gc_stat ->
      let s = gc_json () in
      Http.respond_with_string reqd (with_length (String.length s)) s
    | Websocket upgrade -> upgrade reqd
    | Missing -> Http.respond_with_string reqd not_found ""
end
