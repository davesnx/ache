module B = Ache.Bigstring

let filled n c =
  let t = B.create n in
  for i = 0 to n - 1 do
    B.unsafe_set t i c
  done;
  t

let contents t = B.substring t ~off:0 ~len:(B.length t)
let pattern n = String.init n (fun i -> Char.chr (i * 7 mod 256))

let raises_invalid name f =
  match f () with
  | _ -> Alcotest.failf "%s: expected Invalid_argument" name
  | exception Invalid_argument _ -> ()

let test_create () =
  Alcotest.(check int) "empty" 0 (B.length B.empty);
  Alcotest.(check int) "create" 17 (B.length (B.create 17))

let test_of_string_substring () =
  let t = B.of_string ~off:2 ~len:5 "abcdefghij" in
  Alcotest.(check int) "length" 5 (B.length t);
  Alcotest.(check string) "contents" "cdefg" (contents t);
  Alcotest.(check string) "substring" "def" (B.substring t ~off:1 ~len:3);
  Alcotest.(check string) "empty substring" "" (B.substring t ~off:5 ~len:0);
  let s = pattern 1000 in
  Alcotest.(check string) "long" s (contents (B.of_string ~off:0 ~len:1000 s))

let test_get_set () =
  let t = B.of_string ~off:0 ~len:4 "ab\xffd" in
  Alcotest.(check char) "get" 'b' (B.get t 1);
  Alcotest.(check char) "unsafe_get" '\xff' (B.unsafe_get t 2);
  B.unsafe_set t 3 'z';
  Alcotest.(check string) "unsafe_set" "ab\xffz" (contents t)

let test_ints () =
  let t =
    B.of_string ~off:0 ~len:10 "\x01\x02\x03\x04\x05\x06\x07\x08\x09\x0a"
  in
  let le32 = 0x05040302l
  and le64 = 0x0908070605040302L in
  Alcotest.(check int32)
    "get int32"
    (if Sys.big_endian then 0x02030405l else le32)
    (B.unsafe_get_int32_ne t 1);
  Alcotest.(check int64)
    "get int64"
    (if Sys.big_endian then 0x0203040506070809L else le64)
    (B.unsafe_get_int64_ne t 1);
  let t = filled 12 '.' in
  B.unsafe_set_int16_ne t 1 (if Sys.big_endian then 0x4142 else 0x4241);
  B.unsafe_set_int64_ne
    t
    3
    (if Sys.big_endian then 0x434445464748494aL else 0x4a49484746454443L);
  Alcotest.(check string) "set int16 and int64" ".ABCDEFGHIJ." (contents t)

let test_blit () =
  let src = B.of_string ~off:0 ~len:10 "0123456789" in
  let dst = filled 10 '.' in
  B.blit src ~src_off:2 dst ~dst_off:5 ~len:4;
  Alcotest.(check string) "blit" ".....2345." (contents dst);
  B.unsafe_blit src ~src_off:0 dst ~dst_off:0 ~len:2;
  Alcotest.(check string) "unsafe_blit" "01...2345." (contents dst);
  B.blit src ~src_off:10 dst ~dst_off:10 ~len:0;
  Alcotest.(check string) "empty blit" "01...2345." (contents dst)

let test_blit_overlap () =
  let n = 4096 in
  let s = pattern n in
  let t = B.of_string ~off:0 ~len:n s in
  B.blit t ~src_off:0 t ~dst_off:3 ~len:(n - 3);
  Alcotest.(check string)
    "forward overlap"
    (String.sub s 0 3 ^ String.sub s 0 (n - 3))
    (contents t);
  let t = B.of_string ~off:0 ~len:n s in
  B.blit t ~src_off:3 t ~dst_off:0 ~len:(n - 3);
  Alcotest.(check string)
    "backward overlap"
    (String.sub s 3 (n - 3) ^ String.sub s (n - 3) 3)
    (contents t)

let test_blit_from_string () =
  let dst = filled 8 '.' in
  B.blit_from_string "abcdef" ~src_off:1 dst ~dst_off:3 ~len:4;
  Alcotest.(check string) "blit_from_string" "...bcde." (contents dst);
  B.unsafe_blit_from_string "xyz" ~src_off:2 dst ~dst_off:0 ~len:1;
  Alcotest.(check string) "unsafe_blit_from_string" "z..bcde." (contents dst)

let test_blit_to_bytes () =
  let src = B.of_string ~off:0 ~len:6 "abcdef" in
  let dst = Bytes.make 8 '.' in
  B.unsafe_blit_to_bytes src ~src_off:2 dst ~dst_off:1 ~len:3;
  Alcotest.(check string)
    "unsafe_blit_to_bytes"
    ".cde...."
    (Bytes.to_string dst)

let test_memchr () =
  let t = B.of_string ~off:0 ~len:10 "ab\nde\n\xffhij" in
  Alcotest.(check int) "first" 2 (B.memchr t 0 '\n' 10);
  Alcotest.(check int) "absolute index" 5 (B.memchr t 3 '\n' 7);
  Alcotest.(check int) "high byte" 6 (B.memchr t 1 '\xff' 9);
  Alcotest.(check int) "not found" (-1) (B.memchr t 0 'z' 10);
  Alcotest.(check int) "outside len" (-1) (B.memchr t 6 '\n' 4);
  Alcotest.(check int) "before off" (-1) (B.memchr t 3 'a' 7);
  Alcotest.(check int) "len stops search" (-1) (B.memchr t 3 '\n' 2);
  Alcotest.(check int) "zero len" (-1) (B.memchr t 2 '\n' 0)

let test_bounds () =
  let t = B.create 8 in
  let s = "abcdefgh" in
  raises_invalid "blit src" (fun () -> B.blit t ~src_off:5 t ~dst_off:0 ~len:4);
  raises_invalid "blit dst" (fun () -> B.blit t ~src_off:0 t ~dst_off:5 ~len:4);
  raises_invalid "blit negative" (fun () ->
    B.blit t ~src_off:(-1) t ~dst_off:0 ~len:1);
  raises_invalid "blit len" (fun () ->
    B.blit t ~src_off:0 t ~dst_off:0 ~len:(-1));
  raises_invalid "blit_from_string src" (fun () ->
    B.blit_from_string s ~src_off:6 t ~dst_off:0 ~len:3);
  raises_invalid "blit_from_string dst" (fun () ->
    B.blit_from_string s ~src_off:0 t ~dst_off:6 ~len:3);
  raises_invalid "substring" (fun () -> B.substring t ~off:7 ~len:2);
  raises_invalid "substring negative" (fun () -> B.substring t ~off:(-1) ~len:1);
  raises_invalid "of_string" (fun () -> B.of_string ~off:4 ~len:5 s);
  raises_invalid "memchr" (fun () -> B.memchr t 4 'a' 5);
  raises_invalid "memchr negative off" (fun () -> B.memchr t (-1) 'a' 1);
  raises_invalid "memchr negative len" (fun () -> B.memchr t 0 'a' (-1));
  raises_invalid "get" (fun () -> B.get t 8)

let tests =
  [ "create and empty", `Quick, test_create
  ; "of_string and substring", `Quick, test_of_string_substring
  ; "get and set", `Quick, test_get_set
  ; "integer accessors", `Quick, test_ints
  ; "blit", `Quick, test_blit
  ; "overlapping blit", `Quick, test_blit_overlap
  ; "blit_from_string", `Quick, test_blit_from_string
  ; "unsafe_blit_to_bytes", `Quick, test_blit_to_bytes
  ; "memchr", `Quick, test_memchr
  ; "bounds", `Quick, test_bounds
  ]
