module Encoder = Ache.Ache_private.Encoder

let bigstring_of_string s =
  let b = Ache.Bigstring.create (String.length s) in
  Ache.Bigstring.blit_from_string
    s
    ~src_off:0
    b
    ~dst_off:0
    ~len:(String.length s);
  b

let string_of_iovecs iovecs =
  String.concat
    ""
    (List.map
       (fun { Encoder.buffer; off; len } ->
          Ache.Bigstring.substring buffer ~off ~len)
       iovecs)

let pending t =
  match Encoder.operation t with
  | `Writev iovecs -> iovecs
  | `Yield | `Close -> Alcotest.fail "expected a Writev"

(* Writes everything pending; returns the iovecs and their bytes. *)
let write_all t =
  let iovecs = pending t in
  let s = string_of_iovecs iovecs in
  Encoder.shift t (String.length s);
  Alcotest.(check bool) "nothing pending" false (Encoder.has_pending_output t);
  iovecs, s

let head = String.make 100 'h'
let body = String.init 12288 (fun i -> Char.chr (Char.code 'a' + (i mod 26)))

let test_reuse () =
  let t = Encoder.create 0x400 in
  Encoder.write_string t head;
  Encoder.write_string t body;
  let iovecs, s = write_all t in
  Alcotest.(check string) "first response" (head ^ body) s;
  let kept =
    match iovecs with
    | [ _; { buffer; _ } ] -> buffer
    | _ -> Alcotest.fail "expected the head and the body in two buffers"
  in
  Alcotest.(check int)
    "grown to hold the old buffer and the body"
    (0x400 + String.length body)
    (Ache.Bigstring.length kept);
  for i = 2 to 5 do
    Encoder.write_string t head;
    Encoder.write_string t body;
    let iovecs, s = write_all t in
    let name = Printf.sprintf "response %d" i in
    Alcotest.(check string) name (head ^ body) s;
    match iovecs with
    | [ { buffer; off = 0; _ } ] ->
      Alcotest.(check bool) (name ^ ": same buffer") true (buffer == kept)
    | _ -> Alcotest.fail (name ^ ": expected one iovec")
  done

let test_oversized ~write () =
  let t = Encoder.create 0x400 in
  Encoder.write_string t head;
  Encoder.write_string t body;
  let iovecs, _ = write_all t in
  let kept = (List.nth iovecs 1).buffer in
  let size = Ache.Bigstring.length kept in
  let large = String.init 100_000 (fun i -> Char.chr (i land 0xff)) in
  for i = 1 to 2 do
    let name = Printf.sprintf "large response %d" i in
    Encoder.write_string t head;
    write t large;
    Encoder.write_string t "tail";
    let iovecs, s = write_all t in
    Alcotest.(check string) name (head ^ large ^ "tail") s;
    (match iovecs with
    | [ a; copy; b ] ->
      Alcotest.(check bool) (name ^ ": head in kept") true (a.buffer == kept);
      Alcotest.(check bool) (name ^ ": tail in kept") true (b.buffer == kept);
      Alcotest.(check int)
        (name ^ ": own buffer")
        (String.length large)
        (Ache.Bigstring.length copy.buffer)
    | _ -> Alcotest.fail (name ^ ": expected three iovecs"));
    Alcotest.(check int)
      (name ^ ": kept buffer size")
      size
      (Ache.Bigstring.length kept)
  done;
  Encoder.write_string t head;
  Encoder.write_string t body;
  match write_all t with
  | [ { buffer; _ } ], s ->
    Alcotest.(check string) "after" (head ^ body) s;
    Alcotest.(check bool) "after: same buffer" true (buffer == kept)
  | _ -> Alcotest.fail "after: expected one iovec"

let test_cap () =
  (* Growth stops at 64 KiB; writes up to it still fit a replacement. *)
  let t = Encoder.create 0x400 in
  let chunk = String.make 40_000 'c' in
  Encoder.write_string t chunk;
  Encoder.write_string t chunk;
  let iovecs, s = write_all t in
  Alcotest.(check string) "output" (chunk ^ chunk) s;
  Alcotest.(check (list int))
    "buffer sizes"
    [ 40_000 + 0x400; 0x10000 ]
    (List.map
       (fun { Encoder.buffer; _ } -> Ache.Bigstring.length buffer)
       iovecs)

(* Random writes of every kind and partial shifts give the same bytes as writing
   them to a Buffer. *)
let test_bytes () =
  let rng = Random.State.make [| 40 |] in
  let t = Encoder.create 0x40 in
  let expected = Buffer.create 0x10000 in
  let written = Buffer.create 0x10000 in
  let random_string () =
    let len =
      match Random.State.int rng 4 with
      | 0 -> Random.State.int rng 16
      | 1 -> Random.State.int rng 0x400
      | 2 -> Random.State.int rng 0x4000
      | _ -> 0x10000 + Random.State.int rng 0x8000
    in
    String.init len (fun _ -> Char.chr (Random.State.int rng 256))
  in
  for _ = 1 to 2000 do
    match Random.State.int rng 6 with
    | 0 ->
      let c = Char.chr (Random.State.int rng 256) in
      Buffer.add_char expected c;
      Encoder.write_char t c
    | 1 ->
      let s = random_string () in
      let off = if s = "" then 0 else Random.State.int rng (String.length s) in
      let len = String.length s - off in
      Buffer.add_substring expected s off len;
      Encoder.write_string t ~off ~len s
    | 2 ->
      let s = random_string () in
      Buffer.add_string expected s;
      Encoder.write_bigstring t (bigstring_of_string s)
    | 3 ->
      let s = random_string () in
      Buffer.add_string expected s;
      Encoder.schedule_bigstring t (bigstring_of_string s)
    | _ ->
      (match Encoder.operation t with
      | `Writev iovecs ->
        let s = string_of_iovecs iovecs in
        let n = Random.State.int rng (String.length s + 1) in
        Buffer.add_substring written s 0 n;
        Encoder.shift t n
      | `Yield | `Close -> ())
  done;
  Encoder.close t;
  (match Encoder.operation t with
  | `Writev iovecs ->
    let s = string_of_iovecs iovecs in
    Buffer.add_string written s;
    Encoder.shift t (String.length s)
  | `Yield | `Close -> ());
  Alcotest.(check bool) "closed" true (Encoder.operation t = `Close);
  Alcotest.(check int) "length" (Buffer.length expected) (Buffer.length written);
  Alcotest.(check bool)
    "bytes"
    true
    (String.equal (Buffer.contents expected) (Buffer.contents written))

let tests =
  [ "repeated large writes reuse one buffer", `Quick, test_reuse
  ; ( "a string over the cap gets its own buffer"
    , `Quick
    , test_oversized ~write:(fun t s -> Encoder.write_string t s) )
  ; ( "a bigstring over the cap gets its own buffer"
    , `Quick
    , test_oversized ~write:(fun t s ->
        Encoder.write_bigstring t (bigstring_of_string s)) )
  ; "growth stops at the cap", `Quick, test_cap
  ; "output bytes are the written bytes", `Quick, test_bytes
  ]
