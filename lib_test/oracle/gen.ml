let read_file path = In_channel.with_open_bin path In_channel.input_all

let string_literals src =
  let n = String.length src in
  let out = ref [] in
  let buf = Buffer.create 64 in
  let digit c = Char.code c - 48 in
  let rec plain i =
    if i >= n
    then ()
    else
      match src.[i] with
      | '"' ->
        Buffer.clear buf;
        quoted (i + 1)
      | '{' ->
        let j = ref (i + 1) in
        while
          !j < n && match src.[!j] with 'a' .. 'z' | '_' -> true | _ -> false
        do
          incr j
        done;
        if !j < n && src.[!j] = '|'
        then (
          let close = "|" ^ String.sub src (i + 1) (!j - i - 1) ^ "}" in
          let rec find k =
            if k + String.length close > n
            then n
            else if String.sub src k (String.length close) = close
            then k
            else find (k + 1)
          in
          let e = find (!j + 1) in
          out := String.sub src (!j + 1) (e - !j - 1) :: !out;
          plain (e + String.length close))
        else plain (i + 1)
      | '\'' when i + 2 < n && src.[i + 1] = '\\' ->
        let j = ref (i + 2) in
        while !j < n && src.[!j] <> '\'' do
          incr j
        done;
        plain (!j + 1)
      | '\'' when i + 2 < n && src.[i + 2] = '\'' -> plain (i + 3)
      | _ -> plain (i + 1)
  and quoted i =
    if i >= n
    then ()
    else
      match src.[i] with
      | '"' ->
        out := Buffer.contents buf :: !out;
        plain (i + 1)
      | '\\' when i + 1 < n ->
        (match src.[i + 1] with
        | 'n' ->
          Buffer.add_char buf '\n';
          quoted (i + 2)
        | 'r' ->
          Buffer.add_char buf '\r';
          quoted (i + 2)
        | 't' ->
          Buffer.add_char buf '\t';
          quoted (i + 2)
        | 'b' ->
          Buffer.add_char buf '\b';
          quoted (i + 2)
        | ' ' ->
          Buffer.add_char buf ' ';
          quoted (i + 2)
        | '\n' ->
          let j = ref (i + 2) in
          while !j < n && (src.[!j] = ' ' || src.[!j] = '\t') do
            incr j
          done;
          quoted !j
        | 'x' ->
          Buffer.add_char
            buf
            (Char.chr (int_of_string ("0x" ^ String.sub src (i + 2) 2)));
          quoted (i + 4)
        | 'o' ->
          Buffer.add_char
            buf
            (Char.chr (int_of_string ("0o" ^ String.sub src (i + 2) 3)));
          quoted (i + 5)
        | '0' .. '9' ->
          Buffer.add_char
            buf
            (Char.chr
               ((100 * digit src.[i + 1])
               + (10 * digit src.[i + 2])
               + digit src.[i + 3]));
          quoted (i + 4)
        | c ->
          Buffer.add_char buf c;
          quoted (i + 2))
      | c ->
        Buffer.add_char buf c;
        quoted (i + 1)
  in
  plain 0;
  !out

let sources dirs =
  List.concat_map
    (fun dir ->
       Sys.readdir dir
       |> Array.to_list
       |> List.filter (fun f -> Filename.check_suffix f ".ml")
       |> List.sort compare
       |> List.map (Filename.concat dir))
    dirs

let corpus dirs =
  let seen = Hashtbl.create 1024 in
  List.concat_map
    (fun path ->
       List.rev (string_literals (read_file path))
       |> List.filter (fun s ->
         let keep =
           String.length s <= 2048
           && (String.contains s '\r' || String.contains s '\n')
           && not (Hashtbl.mem seen s)
         in
         if keep then Hashtbl.add seen s ();
         keep))
    (sources dirs)

let kind_of input =
  let rec skip i =
    if i + 1 < String.length input && input.[i] = '\r' && input.[i + 1] = '\n'
    then skip (i + 2)
    else i
  in
  let i = skip 0 in
  if i + 5 <= String.length input && String.sub input i 5 = "HTTP/"
  then Oracle.Response `GET
  else Oracle.Request

let bytes = List.init 256 Char.chr
let with_byte f = List.map (fun c -> f (String.make 1 c)) bytes
let get_head = "GET / HTTP/1.1\r\n"
let ok_head = "HTTP/1.1 200 OK\r\n"
let req fields = get_head ^ fields ^ "\r\n"
let res fields = ok_head ^ fields ^ "\r\n"
let chunked_req = "POST / HTTP/1.1\r\nTransfer-Encoding: chunked\r\n\r\n"
let chunked_res = "HTTP/1.1 200 OK\r\nTransfer-Encoding: chunked\r\n\r\n"

let chunk_lines =
  [ "0"
  ; "5"
  ; "00005"
  ; String.make 20 '0'
  ; "fffffffffffffff"
  ; "0000fffffffffffffff"
  ; "10000000000000000"
  ; "ffffffffffffffff"
  ; "7fffffffffffffff"
  ; ""
  ; "g"
  ; "5 "
  ; "5\t"
  ; "5;"
  ; "5;a"
  ; "5;a=b"
  ; "5;a=\"b\""
  ; "5;a=\"b\\\"c\""
  ; "5 ;a"
  ; "5\t;\ta\t=\tb"
  ; "5;a="
  ; "5;=b"
  ; "5;a=b;c"
  ; "5;a=b ;c=\"d\""
  ; "5;a=\"\x7f\""
  ; "5;a=\"\\\x00\""
  ; "5;a=\"\\\x7f\""
  ; "5;a=\"\xff\""
  ; "5;a=\"unterminated"
  ; "5;a=b c"
  ; "5;a b"
  ; "5 ; a = \"x\" ; b"
  ; "-5"
  ; "+5"
  ; "0x5"
  ; "5\n"
  ; "5\r"
  ; "5\r\r"
  ; "5;a=b\n"
  ; "A"
  ; "a"
  ; "Ff"
  ; "5;a=@"
  ; "5;a@=b"
  ; "5 \t "
  ; "5;"
  ]

let trailer_sections =
  [ ""
  ; "X: y\r\n"
  ; "X: y\r\n z\r\n"
  ; "X: y\r\n\tz\r\n"
  ; "X : y\r\n"
  ; "X: y\n"
  ; "X: y\r\nZ: w\r\n"
  ; ": y\r\n"
  ; "X\x00: y\r\n"
  ; "X: y\x00\r\n"
  ; " X: y\r\n"
  ; "X:y\r\n"
  ; "X: y \t\r\n"
  ]

let chunk_bodies =
  List.map (fun l -> l ^ "\r\nhello\r\n0\r\n\r\n") chunk_lines
  @ List.map (fun t -> "5\r\nhello\r\n0\r\n" ^ t ^ "\r\n") trailer_sections
  @ [ "5\r\nhello\r\n"
    ; "5\r\nhelloXX0\r\n\r\n"
    ; "5\r\nhello\n0\r\n\r\n"
    ; "5\r\nhello\r\n0\r\n"
    ; "5\r\nhello\r\n0\r\n\r"
    ; "3\r\nabc\r\n2\r\nde\r\n0\r\n\r\n"
    ; "0\r\n\r\nGET / HTTP/1.1\r\n\r\n"
    ]

let standalone =
  List.map (fun l -> l ^ "\r\n") chunk_lines
  @ chunk_lines
  @ List.map (fun t -> t ^ "\r\n") trailer_sections
  @ trailer_sections

let rules =
  let r s = Oracle.Request, s
  and p s = Oracle.Response `GET, s in
  let both fields = [ r (req fields); p (res fields) ] in
  List.concat
    [ [ r "GET / HTTP/1.1\nHost: a\r\n\r\n"
      ; r "GET / HTTP/1.1\r\nHost: a\n\r\n"
      ; r "GET / HTTP/1.1\r\nHost: a\r\n\n"
      ; r "GET / HTTP/1.1\rHost: a\r\n\r\n"
      ; r "GET / HTTP/1.1\r\nHost: a\r\r\n\r\n"
      ; r "GET / HTTP/1.1\r\nHost: a\r\n\r"
      ; p "HTTP/1.1 200 OK\nX: a\r\n\r\n"
      ; p "HTTP/1.1 200 OK\r\nX: a\n\r\n"
      ; p "HTTP/1.1 200 OK\r\nX: a\r\n\n"
      ; p "HTTP/1.1 200 OK\rX: a\r\n\r\n"
      ; p "HTTP/1.1 200 OK\r\n\r"
      ; p "HTTP/1.1 200 OK\r\r\n\r\n"
      ; r "\r\nGET / HTTP/1.1\r\n\r\n"
      ; r "\r\n\r\nGET / HTTP/1.1\r\n\r\n"
      ; r "\nGET / HTTP/1.1\r\n\r\n"
      ; r "\rGET / HTTP/1.1\r\n\r\n"
      ; r "!#$%&'*+-.^_`|~09azAZ / HTTP/1.1\r\n\r\n"
      ; r "GET  / HTTP/1.1\r\n\r\n"
      ; r "GET\t/ HTTP/1.1\r\n\r\n"
      ; r "GET / HTTP/1.1 \r\n\r\n"
      ; r "GET/ HTTP/1.1\r\n\r\n"
      ; r "GET /HTTP/1.1\r\n\r\n"
      ; r "GET /\r\n\r\n"
      ; r "GET\r\n\r\n"
      ; r " GET / HTTP/1.1\r\n\r\n"
      ]
    ; List.map
        (fun v -> r ("GET / " ^ v ^ "\r\n\r\n"))
        [ "HTTP/1.1"
        ; "HTTP/1.0"
        ; "HTTP/0.9"
        ; "HTTP/2.0"
        ; "HTTP/9.9"
        ; "HTTP/1.10"
        ; "HTTP/1"
        ; "HTTP/1."
        ; "HTTP/.1"
        ; "HTTP/a.1"
        ; "http/1.1"
        ; "HTTP/1.1x"
        ; "HTTX/1.1"
        ; "HTTP 1.1"
        ; "HTTP/11"
        ; ""
        ]
    ; List.map
        (fun n ->
           p
             (String.concat "" (List.init n (fun _ -> "\r\n"))
             ^ "HTTP/1.1 200 OK\r\n\r\n"))
        [ 1; 2; 10 ]
    ; [ p "\nHTTP/1.1 200 OK\r\n\r\n"; p "\r\r\nHTTP/1.1 200 OK\r\n\r\n" ]
    ; List.map
        (fun v -> p (v ^ " 200 OK\r\n\r\n"))
        [ "HTTP/1.0"
        ; "HTTP/1.9"
        ; "HTTP/2.0"
        ; "HTTP/0.9"
        ; "HTTP/3.0"
        ; "HTTP/2"
        ; "HTTP/2."
        ; "HTTP/1"
        ; "HTTP/12.0"
        ; "HTTP/1.1x"
        ]
    ; [ p "HTTP/2.0"; p "HTTP/2.0\r\n"; p "HTTP/1.1 99"; p "HTTP/1.1 9999" ]
    ; List.map
        (fun s -> p ("HTTP/1.1 " ^ s ^ "\r\n\r\n"))
        [ "200 OK"
        ; "000 x"
        ; "099 x"
        ; "100 x"
        ; "999 x"
        ; "20 x"
        ; "2 x"
        ; " x"
        ; "2000 x"
        ; "20a x"
        ; "a00 x"
        ; "1000000 x"
        ; "200"
        ; "200 "
        ; "200  OK"
        ; "200\tOK"
        ; "200OK"
        ; ""
        ; "200 OK "
        ]
    ; with_byte (fun b -> r ("G" ^ b ^ "T / HTTP/1.1\r\n\r\n"))
    ; with_byte (fun b -> r ("GET /a" ^ b ^ "b HTTP/1.1\r\n\r\n"))
    ; with_byte (fun b -> p ("HTTP/1.1 200 O" ^ b ^ "K\r\n\r\n"))
    ; List.concat (with_byte (fun b -> both ("X" ^ b ^ "Y: v\r\n")))
    ; List.concat (with_byte (fun b -> both ("X: a" ^ b ^ "b\r\n")))
    ; List.concat_map
        both
        [ "X : v\r\n"
        ; "X\t: v\r\n"
        ; ": v\r\n"
        ; "X:v\r\n"
        ; "X:  v  \r\n"
        ; "X:\tv\t\r\n"
        ; "X: v\x00\r\n"
        ; "X: a\r\n b\r\n"
        ; "X: a\r\n\tb\r\n"
        ; "X: a\r\n  \r\n"
        ; " X: a\r\n"
        ; "\tX: a\r\n"
        ; "X: a\r\n b\r\n c\r\n"
        ; "X: a\r\n b \r\nY: c\r\n"
        ; "X: a\r\n b\x00\r\n"
        ; "X: a\r\n b\nY: c\r\n"
        ; "X: 1\r\nX: 2\r\nY: 3\r\n"
        ; "X\r\n"
        ; "X;: v\r\n"
        ; "X: a\r\n\r\n"
        ]
    ; List.map (fun b -> r (chunked_req ^ b)) chunk_bodies
    ; List.map (fun b -> p (chunked_res ^ b)) chunk_bodies
    ; [ r "GET / HTTP/1.1\r\n\r\nGET /b HTTP/1.1\r\n\r\n"
      ; r
          "POST / HTTP/1.1\r\n\
           Content-Length: 5\r\n\
           \r\n\
           helloGET / HTTP/1.1\r\n\
           \r\n"
      ; r
          "GET / HTTP/1.1\r\n\
           Upgrade: websocket\r\n\
           Connection: upgrade\r\n\
           \r\n\
           tunnel"
      ; r "CONNECT a:443 HTTP/1.1\r\nHost: a:443\r\n\r\ntunnel bytes"
      ; r
          "POST / HTTP/1.1\r\n\
           Content-Length: 5\r\n\
           Content-Length: 6\r\n\
           \r\n\
           hello"
      ; r "POST / HTTP/1.1\r\nTransfer-Encoding: gzip\r\n\r\n"
      ; r "GET / HTTP/1.0\r\n\r\n"
      ; r "GET / HTTP/2.0\r\n\r\n"
      ; p
          "HTTP/1.1 100 Continue\r\n\
           \r\n\
           HTTP/1.1 200 OK\r\n\
           Content-Length: 2\r\n\
           \r\n\
           ok"
      ; p "HTTP/1.1 101 Switching Protocols\r\nUpgrade: x\r\n\r\ntunnel"
      ; p "HTTP/1.1 200 OK\r\n\r\nclose delimited"
      ; p
          "HTTP/1.1 200 OK\r\n\
           Content-Length: 2\r\n\
           \r\n\
           okHTTP/1.1 204 No Content\r\n\
           \r\n"
      ; p "HTTP/1.1 200 OK\r\nContent-Length: x\r\n\r\n"
      ; p
          "HTTP/1.1 200 OK\r\n\
           Content-Length: 2\r\n\
           \r\n\
           okHTTP/1.1 200 OK\r\n\
           Content-Length: 0\r\n\
           \r\n\
           HTTP/1.1 200 OK\r\n\
           Content-Length: 0\r\n\
           \r\n\
           HTTP/1.1 200 OK\r\n\
           \r\n"
      ; ( Oracle.Response `HEAD
        , "HTTP/1.1 200 OK\r\nContent-Length: 5\r\n\r\nHTTP/1.1 200 OK\r\n\r\n"
        )
      ; Oracle.Response `CONNECT, "HTTP/1.1 200 OK\r\n\r\ntunnel"
      ; ( Oracle.Response `CONNECT
        , "HTTP/1.1 407 No\r\nContent-Length: 1\r\n\r\nx" )
      ]
    ]

let sweeps =
  let r s = Oracle.Request, s
  and p s = Oracle.Response `GET, s in
  let both fields = [ r (req fields); p (res fields) ] in
  let chunk line = [ r line; r (chunked_req ^ line ^ "x\r\n0\r\n\r\n") ] in
  let sweep f = List.concat (with_byte f) in
  List.concat
    [ sweep (fun b -> chunk ("1" ^ b ^ "\r\n"))
    ; sweep (fun b -> chunk ("0" ^ b ^ "\r\n"))
    ; sweep (fun b -> chunk (b ^ "1\r\n"))
    ; sweep (fun b -> chunk ("1" ^ b ^ ";a\r\n"))
    ; sweep (fun b -> chunk ("1;" ^ b ^ "\r\n"))
    ; sweep (fun b -> chunk ("1;a" ^ b ^ "\r\n"))
    ; sweep (fun b -> chunk ("1;a" ^ b ^ "=b\r\n"))
    ; sweep (fun b -> chunk ("1;a=" ^ b ^ "\r\n"))
    ; sweep (fun b -> chunk ("1;a=b" ^ b ^ "\r\n"))
    ; sweep (fun b -> chunk ("1;a=\"x" ^ b ^ "y\"\r\n"))
    ; sweep (fun b -> chunk ("1;a=\"\\" ^ b ^ "\"\r\n"))
    ; sweep (fun b -> both ("X:" ^ b ^ "v\r\n"))
    ; sweep (fun b -> both ("X: v" ^ b ^ "\r\n"))
    ; sweep (fun b -> both ("X: a\r\n" ^ b ^ "b\r\n"))
    ; sweep (fun b ->
        [ r ("0\r\nX:" ^ b ^ "v\r\n\r\n"); r ("X: v" ^ b ^ "\r\n\r\n") ])
    ; sweep (fun b ->
        [ r ("GET / HTTP/" ^ b ^ ".1\r\n\r\n")
        ; r ("GET / HTTP/1" ^ b ^ "1\r\n\r\n")
        ; r ("GET / HTTP/1." ^ b ^ "\r\n\r\n")
        ; p ("HTTP/" ^ b ^ ".1 200 OK\r\n\r\n")
        ; p ("HTTP/1." ^ b ^ " 200 OK\r\n\r\n")
        ])
    ; sweep (fun b ->
        [ p ("HTTP/1.1 " ^ b ^ "00 OK\r\n\r\n")
        ; p ("HTTP/1.1 2" ^ b ^ "0 OK\r\n\r\n")
        ; p ("HTTP/1.1 20" ^ b ^ " OK\r\n\r\n")
        ; p ("HTTP/1.1 200" ^ b ^ "OK\r\n\r\n")
        ])
    ; sweep (fun b ->
        [ r ("GET / HTTP/1.1\r\nHost: a\r\n\r\n" ^ b)
        ; p ("HTTP/1.1 200 OK\r\nContent-Length: 0\r\n\r\n" ^ b)
        ; r ("5\r\n" ^ b)
        ; r ("\r\n" ^ b)
        ; r ("X: v\r\n\r\n" ^ b)
        ])
    ]

let at_limit limit =
  let r s = Oracle.Request, limit, s
  and p s = Oracle.Response `GET, limit, s in
  let near f = List.init 7 (fun i -> f (limit - 3 + i)) in
  let pad n s = if n > 0 then String.make n s else "" in
  List.concat
    [ near (fun n -> r (pad n 'M' ^ " / HTTP/1.1\r\n\r\n"))
    ; near (fun n -> r ("GET /" ^ pad (n - 5) 'p' ^ " HTTP/1.1\r\n\r\n"))
    ; near (fun n -> r (pad (n - 4) 'M' ^ " /a HTTP/1.1\r\n\r\n"))
    ; near (fun n ->
        r ("GET / HTTP/1.1\r\nX: " ^ pad (n - 23) 'v' ^ "\r\n\r\n"))
    ; near (fun n ->
        r ("\r\nGET / HTTP/1.1\r\nX: " ^ pad (n - 25) 'v' ^ "\r\n\r\n"))
    ; near (fun n -> r ("GET / HTTP/1.1\r\nX: " ^ pad (n - 21) 'v' ^ "\r\n"))
    ; near (fun n ->
        p ("HTTP/1.1 200 OK\r\nX: " ^ pad (n - 24) 'v' ^ "\r\n\r\n"))
    ; near (fun n -> p ("HTTP/1.1 200 " ^ pad (n - 17) 'r' ^ "\r\n\r\n"))
    ; near (fun n ->
        p
          (String.concat "" (List.init (max 0 ((n - 19) / 2)) (fun _ -> "\r\n"))
          ^ "HTTP/1.1 200 OK\r\n\r\n"))
    ; near (fun n ->
        r (chunked_req ^ "5;" ^ pad (n - 5) 'e' ^ "\r\nhello\r\n0\r\n\r\n"))
    ; near (fun n ->
        p (chunked_res ^ "5;" ^ pad (n - 5) 'e' ^ "\r\nhello\r\n0\r\n\r\n"))
    ; near (fun n ->
        r (chunked_req ^ "0\r\nX: " ^ pad (n - 7) 't' ^ "\r\n\r\n"))
    ; near (fun n ->
        p (chunked_res ^ "0\r\nX: " ^ pad (n - 7) 't' ^ "\r\n\r\n"))
    ; near (fun n ->
        r (chunked_req ^ pad (n - 3) '0' ^ "5\r\nhello\r\n0\r\n\r\n"))
    ]

let specials =
  [ '\r'; '\n'; '\x00'; ' '; '\t'; '\x7f'; '\x80'; '\xff'; ':'; ';' ]

let mutate s =
  let rng = Random.State.make [| 87; Hashtbl.hash s |] in
  fun () ->
    let n = String.length s in
    let pick l = List.nth l (Random.State.int rng (List.length l)) in
    let byte () =
      if Random.State.bool rng
      then pick specials
      else Char.chr (Random.State.int rng 256)
    in
    if n = 0
    then String.make 1 (byte ())
    else
      let i = Random.State.int rng n in
      match Random.State.int rng 4 with
      | 0 -> String.mapi (fun j c -> if j = i then byte () else c) s
      | 1 -> String.sub s 0 i ^ String.make 1 (byte ()) ^ String.sub s i (n - i)
      | 2 -> String.sub s 0 i ^ String.sub s (i + 1) (n - i - 1)
      | _ -> String.sub s 0 i

let default_limit = 0x4000

let () =
  let dirs = List.tl (Array.to_list Sys.argv) in
  let limits = List.concat_map at_limit [ 64; 100 ] in
  let base =
    List.map (fun s -> kind_of s, default_limit, s) (corpus dirs)
    @ List.map (fun (k, s) -> k, default_limit, s) rules
  in
  let mutations =
    List.concat_map
      (fun (k, l, s) ->
         let mutate = mutate s in
         List.init 3 (fun _ -> k, l, mutate ()))
      (base @ limits)
  in
  let standalone =
    List.map (fun s -> Oracle.Request, default_limit, s) standalone
  in
  let seen = Hashtbl.create 4096 in
  let module R = Oracle.Run (Ache.Ache_private.Parse_v1) in
  List.iter
    (fun (kind, limit, input) ->
       if not (Hashtbl.mem seen (kind, limit, input))
       then (
         Hashtbl.add seen (kind, limit, input) ();
         Oracle.output_case stdout (R.case kind ~limit input)))
    (base
    @ standalone
    @ List.map (fun (k, s) -> k, default_limit, s) sweeps
    @ limits
    @ mutations)
