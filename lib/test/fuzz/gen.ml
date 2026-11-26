(* Random generators of HTTP/1.1 bytes. A generator is a function of a
   [Random.State.t], so that a test case is reproducible from its seed. *)

type 'a t = Random.State.t -> 'a

let int st n = if n <= 0 then 0 else Random.State.int st n
let range st lo hi = lo + int st (hi - lo + 1)
let bool st = Random.State.bool st
let chance st n = int st n = 0
let pick st l = List.nth l (int st (List.length l))

let weighted st l =
  let total = List.fold_left (fun acc (w, _) -> acc + w) 0 l in
  let rec go n = function
    | [] -> assert false
    | (w, x) :: rest -> if n < w then x else go (n - w) rest
  in
  go (int st total) l

let string_of st chars len =
  String.init len (fun _ -> chars.[int st (String.length chars)])

let bytes st len = String.init len (fun _ -> Char.chr (int st 256))

(* Seeds: the head corpus of the grammar tests, and the inputs of the snapshot
   oracle. *)

let oracle_inputs path =
  let requests = ref []
  and responses = ref [] in
  In_channel.with_open_bin path (fun ic ->
    let rec go () =
      match In_channel.input_line ic with
      | None -> ()
      | Some line ->
        (if String.starts_with ~prefix:"case " line
         then
           try
             Scanf.sscanf line "case %s %d %S" (fun kind _limit input ->
               if kind = "request"
               then requests := input :: !requests
               else if String.starts_with ~prefix:"response:" kind
               then
                 responses :=
                   (String.sub kind 9 (String.length kind - 9), input)
                   :: !responses)
           with
           | Scanf.Scan_failure _ | Failure _ | End_of_file -> ());
        go ()
    in
    go ());
  Array.of_list (List.rev !requests), Array.of_list (List.rev !responses)

(* The test runs in its build directory; [dune exec] runs it elsewhere. *)
let oracle_path =
  let near_exe =
    Filename.concat
      (Filename.dirname Sys.executable_name)
      "../oracle/oracle.data"
  in
  if Sys.file_exists near_exe then near_exe else "../oracle/oracle.data"

let oracle = lazy (oracle_inputs oracle_path)

let request_seeds =
  lazy
    (Array.append
       (Array.of_list Grammar_corpus.requests)
       (fst (Lazy.force oracle)))

let response_seeds =
  lazy
    (Array.append
       (Array.of_list Grammar_corpus.responses)
       (Array.map snd (snd (Lazy.force oracle))))

let seed st seeds =
  let seeds = Lazy.force seeds in
  seeds.(int st (Array.length seeds))

(* Mutations: byte flips, insertion of bytes that framing depends on, deletion,
   duplication, truncation and splicing. *)

let specials = "\r\n \t\000\x80\xff:;,=\"0123456789aAfF-+"

let mutate ~seeds st s =
  let len = String.length s in
  let at () = int st (len + 1) in
  let insert p x = String.sub s 0 p ^ x ^ String.sub s p (len - p) in
  match int st 9 with
  | 0 when len > 0 ->
    let p = int st len in
    String.mapi (fun i c -> if i = p then Char.chr (int st 256) else c) s
  | 1 ->
    insert (at ()) (String.make 1 specials.[int st (String.length specials)])
  | 2 when len > 0 ->
    let p = int st len in
    String.sub s 0 p ^ String.sub s (p + 1) (len - p - 1)
  | 3 when len > 0 ->
    let a = int st len in
    let b = range st a (min len (a + 64)) in
    insert b (String.sub s a (b - a))
  | 4 ->
    let other = seed st seeds in
    let p = at () in
    let q = int st (String.length other + 1) in
    String.sub s 0 p ^ String.sub other q (String.length other - q)
  | 5 when len > 0 -> String.sub s 0 (at ())
  | 6 -> insert (at ()) (pick st [ "\r\n"; "\n"; "\r"; "\r\n\r\n"; " \r\n" ])
  | 7 ->
    insert
      (at ())
      (pick
         st
         [ "ffffffffffffffff"; "0000000000000000001"; "99999999999999999999" ])
  | _ -> s

let rec mutations ~seeds st n s =
  if n = 0 then s else mutations ~seeds st (n - 1) (mutate ~seeds st s)

(* Message bodies. *)

let body st =
  weighted
    st
    [ (3, fun () -> string_of st "abc0\r\n xyz" (int st 40))
    ; (1, fun () -> bytes st (int st 40))
    ; (1, fun () -> "")
    ]
    ()

let hex_size st n =
  let s = Printf.sprintf (if bool st then "%x" else "%X") n in
  if chance st 6 then String.make (range st 1 4) '0' ^ s else s

let valid_extension st =
  pick
    st
    [ ""; ""; ";a"; ";a=b"; ";name=\"q d\""; " ;a=b"; ";a=\"\\\"\""; ";x;y=1" ]

let extension st =
  if chance st 5
  then
    pick
      st
      [ ";"; ";a=\"open"; ";\x00"; ";a=b c"; ";=b"; ";" ^ String.make 100 'e' ]
  else valid_extension st

let valid_trailers st =
  pick st [ ""; ""; "X-T: v\r\n"; "X-T: v\r\nY: \r\n"; "Expires: 0\r\n" ]

let trailers st =
  if chance st 6
  then pick st [ "X-T v\r\n"; " a\r\n"; "X: a\nb\r\n" ]
  else valid_trailers st

let chunked_with ~extension ~trailers st data =
  let b = Buffer.create (String.length data * 2) in
  let rec go p =
    if p < String.length data
    then (
      let n = range st 1 (String.length data - p) in
      Buffer.add_string b (hex_size st n);
      Buffer.add_string b (extension st);
      Buffer.add_string b "\r\n";
      Buffer.add_string b (String.sub data p n);
      Buffer.add_string b "\r\n";
      go (p + n))
  in
  go 0;
  Buffer.add_string b (if chance st 8 then "00" else "0");
  Buffer.add_string b (extension st);
  Buffer.add_string b "\r\n";
  Buffer.add_string b (trailers st);
  Buffer.add_string b "\r\n";
  Buffer.contents b

let chunked ~valid st data =
  if valid
  then chunked_with ~extension:valid_extension ~trailers:valid_trailers st data
  else chunked_with ~extension ~trailers st data

(* [choose ~valid st good bad]: an element of [good], or when [valid] is false,
   sometimes one of [bad]. *)
let choose ~valid st good bad =
  if valid || bad = [] || bool st then pick st good else pick st bad

(* Framing fields and the body bytes that go with them. *)

let framing ?(chunked_ok = true) ~valid st ~close_delimited =
  let data = body st in
  let n = String.length data in
  let fixed s = [ "Content-Length: " ^ s ], data in
  let good =
    [ (3, fun () -> [], if close_delimited then data else "")
    ; (4, fun () -> fixed (string_of_int n))
    ; (1, fun () -> fixed (Printf.sprintf "%d, %d" n n))
    ]
    @
    if chunked_ok
    then
      [ (4, fun () -> [ "Transfer-Encoding: chunked" ], chunked ~valid st data)
      ]
    else []
  in
  let bad =
    [ ( 1
      , fun () ->
          (* Many small chunks with long extensions: more framing than data,
             near the chunked overhead budget. *)
          let ext = ";" ^ String.make (range st 0 40) 'e' in
          let count = range st 100 1500 in
          let b = Buffer.create (count * (String.length ext + 8)) in
          for _ = 1 to count do
            Buffer.add_string b "1";
            Buffer.add_string b ext;
            Buffer.add_string b "\r\nx\r\n"
          done;
          Buffer.add_string b "0\r\n\r\n";
          [ "Transfer-Encoding: chunked" ], Buffer.contents b )
    ; (1, fun () -> fixed (string_of_int (n + range st (-2) 2)))
    ; (1, fun () -> fixed (pick st [ ""; "+1"; "-1"; "1 2"; "0x10"; "007" ]))
    ; ( 1
      , fun () -> [ "Transfer-Encoding: gzip, chunked" ], chunked ~valid st data
      )
    ; ( 1
      , fun () ->
          ( [ "Transfer-Encoding: chunked"
            ; "Content-Length: " ^ string_of_int n
            ]
          , chunked ~valid st data ) )
    ; ( 1
      , fun () ->
          ( [ pick
                st
                [ "Transfer-Encoding: chunked, chunked"
                ; "Transfer-Encoding: gzip"
                ; "Transfer-Encoding: "
                ; "Transfer-Encoding: chunked, gzip"
                ; "Transfer-Encoding: Chunked"
                ]
            ]
          , chunked ~valid st data ) )
    ]
  in
  weighted st (if valid || bool st then good else good @ bad) ()

let field_value ~valid st =
  let core = string_of st "abc \t!~\x80\xff;=,\"" (int st 12) in
  if (not valid) && chance st 4
  then core ^ pick st [ "\000"; "\r"; "\n"; "\x7f" ]
  else core

let extra_headers ~valid st =
  List.init (int st 4) (fun _ ->
    choose
      ~valid
      st
      [ "Connection: close"
      ; "Connection: keep-alive"
      ; "Connection: upgrade"
      ; "Upgrade: websocket"
      ; "Expect: 100-continue"
      ; "Accept: */*"
      ; "X-A: b"
      ; "Keep-Alive: timeout=5"
      ; "X: " ^ field_value ~valid st
      ]
      [ "X-A : b"; "X-A b"; " X: b"; "X: " ^ field_value ~valid st ])

let head line fields =
  String.concat "" (List.map (fun f -> f ^ "\r\n") (line :: fields)) ^ "\r\n"

(* Two thirds of the messages use only valid choices, so that most of them reach
   the request handler; the others mix in invalid ones. *)
let request st =
  let valid = int st 3 > 0 in
  let meth =
    pick
      st
      [ "GET"
      ; "GET"
      ; "POST"
      ; "POST"
      ; "HEAD"
      ; "PUT"
      ; "DELETE"
      ; "OPTIONS"
      ; "CONNECT"
      ; "PATCH"
      ; "X"
      ]
  in
  let target =
    match meth with
    | "CONNECT" ->
      choose
        ~valid
        st
        [ "example.com:443"; "h:80" ]
        [ "/"; "[::1]:80"; "h:99999" ]
    | "OPTIONS" -> pick st [ "*"; "/" ]
    | _ ->
      choose
        ~valid
        st
        [ "/"; "/"; "/a/b?c=d"; "http://example.com/x"; "/%41" ]
        [ "/a#f"; "/\xc3\xa9"; "example.com:80"; "http://u@h/"; "/a{b}"; "*" ]
  in
  let version =
    if valid
    then weighted st [ 6, "HTTP/1.1"; 2, "HTTP/1.0" ]
    else
      weighted st [ 6, "HTTP/1.1"; 2, "HTTP/1.0"; 1, "HTTP/1.2"; 1, "HTTP/2.0" ]
  in
  let host =
    if valid
    then [ "Host: example.com" ]
    else
      weighted
        st
        [ 1, []
        ; 4, [ "Host: example.com" ]
        ; ( 1
          , [ "Host: " ^ pick st [ "example.com:80"; ":80"; ""; "a b"; "[::1]" ]
            ] )
        ; 1, [ "Host: a"; "Host: b" ]
        ]
  in
  (* RFC 9112 §6.1: an HTTP/1.0 request with Transfer-Encoding is rejected. *)
  let chunked_ok = version <> "HTTP/1.0" in
  let fields, body = framing ~chunked_ok ~valid st ~close_delimited:false in
  head
    (String.concat " " [ meth; target; version ])
    (host @ extra_headers ~valid st @ fields)
  ^ body

let response st =
  let valid = int st 3 > 0 in
  let status =
    choose
      ~valid
      st
      [ "100 Continue"
      ; "101 Switching Protocols"
      ; "103 Early Hints"
      ; "200 OK"
      ; "200 OK"
      ; "200 OK"
      ; "201 Created"
      ; "204 No Content"
      ; "206 Partial"
      ; "301 "
      ; "304 Not Modified"
      ; "404 Not Found"
      ; "500 Error"
      ; "199 x"
      ; "999 x"
      ]
      [ "301"; "200  OK"; "20 OK"; "2000 OK" ]
  in
  let version =
    choose
      ~valid
      st
      [ "HTTP/1.1"; "HTTP/1.1"; "HTTP/1.0" ]
      [ "HTTP/1.2"; "HTTP/2.0" ]
  in
  let fields, body = framing ~valid st ~close_delimited:true in
  head (version ^ " " ^ status) (extra_headers ~valid st @ fields) ^ body

let some_bytes st =
  string_of
    st
    "GET /HTP1.0\r\n: ;\t\000\x80\xffchunkedContent-Lgth"
    (int st 120)

let messages st one ~seeds =
  let base () =
    weighted
      st
      [ (2, fun () -> seed st seeds)
      ; (7, fun () -> one st)
      ; (1, fun () -> some_bytes st)
      ]
      ()
  in
  let n = weighted st [ 6, 1; 3, 2; 1, 3 ] in
  let s = String.concat "" (List.init n (fun _ -> base ())) in
  mutations ~seeds st (weighted st [ 8, 0; 2, 1; 1, 2; 1, 3 ]) s

(* Splits: the sizes of successive reads. *)

let chunks_of_sizes s sizes =
  let len = String.length s in
  let rec go p sizes acc =
    if p >= len
    then List.rev acc
    else
      match sizes with
      | [] -> List.rev (String.sub s p (len - p) :: acc)
      | n :: rest ->
        let n = min n (len - p) in
        go (p + n) rest (String.sub s p n :: acc)
  in
  if s = "" then [ "" ] else go 0 sizes []

let every_byte s = chunks_of_sizes s (List.init (String.length s) (fun _ -> 1))

let split st s =
  let len = String.length s in
  if len < 600 && chance st 6
  then every_byte s
  else
    chunks_of_sizes
      s
      (List.init (range st 1 12) (fun _ ->
         if bool st then range st 1 8 else range st 1 (max 1 len)))
