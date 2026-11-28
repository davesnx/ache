(* The parser records which framing and connection fields a request has, so the
   server skips the walk over the fields for one that is absent. The names it
   recognises byte for byte take one path, the other spellings another: each
   request below gives the same output whatever the case of its field names, and
   when it arrives one byte at a time. *)

open Ache

(* "cOnNeCtIoN" *)
let odd_case name =
  String.mapi
    (fun i c ->
       if i mod 2 = 0 then Char.lowercase_ascii c else Char.uppercase_ascii c)
    name

let spellings =
  [ "as sent by clients", Fun.id
  ; "lowercase", String.lowercase_ascii
  ; "uppercase", String.uppercase_ascii
  ; "odd case", odd_case
  ]

(* [spell i name] spells the [i]th tracked name of a request; mixed spellings
   give duplicates different spellings. *)
let variants =
  List.map (fun (label, f) -> label, fun _ name -> f name) spellings
  @ [ ( "mixed"
      , fun i name ->
          (snd (List.nth spellings (i mod List.length spellings))) name )
    ]

let read_all body k =
  let received = Buffer.create 16 in
  let rec loop () =
    Body.Reader.schedule_read
      body
      ~on_error:(fun () -> k ("incomplete " ^ Buffer.contents received))
      ~on_eof:(fun () -> k (Buffer.contents received))
      ~on_read:(fun bs ~off ~len ->
        Buffer.add_string received (Bigstring.substring bs ~off ~len);
        loop ())
  in
  loop ()

(* /stream answers with a chunked body and a trailer; the rest with the request
   body. *)
let handler reqd =
  let request = Request_descriptor.request reqd in
  read_all (Request_descriptor.request_body reqd) (fun body ->
    if request.target = "/stream"
    then (
      let writer =
        Request_descriptor.respond_with_streaming reqd (Response.create `OK)
      in
      Body.Writer.write_string writer body;
      Body.Writer.close ~trailers:(Headers.of_list [ "x-t", "1" ]) writer)
    else
      Request_descriptor.respond_with_string
        reqd
        (Response.create
           ~headers:
             (Headers.of_list
                [ "content-length", string_of_int (String.length body + 4) ])
           `OK)
        ("got " ^ body))

let config = { Config.default with auto_continue = true }
let serve input = Testing.serve ~config handler input

let serve_bytewise input =
  let t = Testing.create ~config handler in
  String.iter (fun c -> Testing.feed t (String.make 1 c)) input;
  Testing.feed_eof t;
  Testing.output t

let rec count_from s sub i n =
  if i + String.length sub > String.length s
  then n
  else if String.sub s i (String.length sub) = sub
  then count_from s sub (i + 1) (n + 1)
  else count_from s sub (i + 1) n

let count sub s = count_from s sub 0 0
let contains sub s = count sub s > 0

(* Each case: a request built from a speller of its tracked names, and what the
   output holds. The speller numbers the names in order of appearance. *)
let cases :
  (string * ((int -> string -> string) -> string) * (string -> bool)) list
  =
  let ok n out = count "HTTP/1.1 200 OK" out = n in
  let status code out = contains ("HTTP/1.1 " ^ code) out && ok 0 out in
  [ ( "one Host"
    , (fun f -> "GET / HTTP/1.1\r\n" ^ f 0 "Host" ^ ": a\r\n\r\n")
    , ok 1 )
  ; ( "one Host in each of two requests"
    , (fun f ->
        "GET / HTTP/1.1\r\n"
        ^ f 0 "Host"
        ^ ": a\r\n\r\nGET / HTTP/1.1\r\n"
        ^ f 1 "Host"
        ^ ": a\r\n\r\n")
    , ok 2 )
  ; ( "two Host fields"
    , (fun f ->
        "GET / HTTP/1.1\r\n"
        ^ f 0 "Host"
        ^ ": a\r\n"
        ^ f 1 "Host"
        ^ ": a\r\n\r\n")
    , status "400" )
  ; ( "an invalid Host"
    , (fun f -> "GET / HTTP/1.1\r\n" ^ f 0 "Host" ^ ": a b\r\n\r\n")
    , status "400" )
  ; ( "Host after other fields"
    , (fun f ->
        "GET / HTTP/1.1\r\nAccept: */*\r\nX-Hast: b\r\n"
        ^ f 0 "Host"
        ^ ": a\r\nCookie: c=d\r\n\r\n")
    , ok 1 )
  ; ( "Content-Length"
    , (fun f ->
        "POST / HTTP/1.1\r\nHost: a\r\n"
        ^ f 0 "Content-Length"
        ^ ": 5\r\n\r\nhello")
    , fun out -> ok 1 out && contains "got hello" out )
  ; ( "two equal Content-Length fields"
    , (fun f ->
        "POST / HTTP/1.1\r\nHost: a\r\n"
        ^ f 0 "Content-Length"
        ^ ": 5\r\n"
        ^ f 1 "Content-Length"
        ^ ": 5\r\n\r\nhello")
    , fun out -> ok 1 out && contains "got hello" out )
  ; ( "two different Content-Length fields"
    , (fun f ->
        "POST / HTTP/1.1\r\nHost: a\r\n"
        ^ f 0 "Content-Length"
        ^ ": 5\r\n"
        ^ f 1 "Content-Length"
        ^ ": 6\r\n\r\nhello!")
    , status "400" )
  ; ( "Transfer-Encoding"
    , (fun f ->
        "POST / HTTP/1.1\r\nHost: a\r\n"
        ^ f 0 "Transfer-Encoding"
        ^ ": chunked\r\n\r\n5\r\nhello\r\n0\r\n\r\n")
    , fun out -> ok 1 out && contains "got hello" out )
  ; ( "two Transfer-Encoding fields"
    , (fun f ->
        "POST / HTTP/1.1\r\nHost: a\r\n"
        ^ f 0 "Transfer-Encoding"
        ^ ": gzip\r\n"
        ^ f 1 "Transfer-Encoding"
        ^ ": chunked\r\n\r\n5\r\nhello\r\n0\r\n\r\n")
    , status "501" )
  ; ( "Transfer-Encoding and Content-Length"
    , (fun f ->
        "POST / HTTP/1.1\r\nHost: a\r\n"
        ^ f 0 "Content-Length"
        ^ ": 5\r\n"
        ^ f 1 "Transfer-Encoding"
        ^ ": chunked\r\n\r\n5\r\nhello\r\n0\r\n\r\n")
    , status "400" )
  ; ( "Transfer-Encoding in HTTP/1.0"
    , (fun f ->
        "POST / HTTP/1.0\r\n"
        ^ f 0 "Transfer-Encoding"
        ^ ": chunked\r\n\r\n5\r\nhello\r\n0\r\n\r\n")
    , status "400" )
  ; ( "Connection: close"
    , (fun f ->
        "GET / HTTP/1.1\r\nHost: a\r\n"
        ^ f 0 "Connection"
        ^ ": close\r\n\r\nGET / HTTP/1.1\r\nHost: a\r\n\r\n")
    , fun out -> ok 1 out && contains "connection: close" out )
  ; ( "two Connection fields, the second with close"
    , (fun f ->
        "GET / HTTP/1.1\r\nHost: a\r\n"
        ^ f 0 "Connection"
        ^ ": keep-alive\r\n"
        ^ f 1 "Connection"
        ^ ": close\r\n\r\nGET / HTTP/1.1\r\nHost: a\r\n\r\n")
    , ok 1 )
  ; ( "Connection: keep-alive in HTTP/1.0"
    , (fun f ->
        "GET / HTTP/1.0\r\n"
        ^ f 0 "Connection"
        ^ ": keep-alive\r\n\r\nGET / HTTP/1.0\r\n\r\n")
    , fun out -> count "200 OK" out = 2 )
  ; ( "HTTP/1.0 without Connection"
    , (fun _ -> "GET / HTTP/1.0\r\n\r\nGET / HTTP/1.0\r\n\r\n")
    , fun out -> count "200 OK" out = 1 )
  ; ( "Expect: 100-continue"
    , (fun f ->
        "POST / HTTP/1.1\r\nHost: a\r\n"
        ^ f 0 "Expect"
        ^ ": 100-continue\r\n"
        ^ f 1 "Content-Length"
        ^ ": 5\r\n\r\nhello")
    , fun out -> contains "100 Continue" out && contains "got hello" out )
  ; ( "two Expect fields"
    , (fun f ->
        "POST / HTTP/1.1\r\nHost: a\r\n"
        ^ f 0 "Expect"
        ^ ": x\r\n"
        ^ f 1 "Expect"
        ^ ": 100-continue\r\n"
        ^ f 2 "Content-Length"
        ^ ": 5\r\n\r\nhello")
    , fun out -> contains "100 Continue" out && contains "got hello" out )
  ; ( "no Expect"
    , (fun f ->
        "POST / HTTP/1.1\r\nHost: a\r\n"
        ^ f 0 "Content-Length"
        ^ ": 5\r\n\r\nhello")
    , fun out -> (not (contains "100 Continue" out)) && ok 1 out )
  ; ( "TE: trailers"
    , (fun f ->
        "GET /stream HTTP/1.1\r\nHost: a\r\n" ^ f 0 "TE" ^ ": trailers\r\n\r\n")
    , fun out -> contains "\r\nx-t: 1\r\n" out )
  ; ( "two TE fields"
    , (fun f ->
        "GET /stream HTTP/1.1\r\nHost: a\r\n"
        ^ f 0 "TE"
        ^ ": deflate\r\n"
        ^ f 1 "TE"
        ^ ": trailers\r\n\r\n")
    , fun out -> contains "\r\nx-t: 1\r\n" out )
  ; ( "no TE"
    , (fun _ -> "GET /stream HTTP/1.1\r\nHost: a\r\n\r\n")
    , fun out -> ok 1 out && not (contains "x-t" out) )
  ; ( "a trailer section does not reach the next request"
    , (fun f ->
        "POST / HTTP/1.1\r\nHost: a\r\nTransfer-Encoding: chunked\r\n\r\n0\r\n"
        ^ f 0 "Host"
        ^ ": b\r\n"
        ^ f 1 "Connection"
        ^ ": close\r\n\r\nGET /stream HTTP/1.1\r\nHost: a\r\n\r\n")
    , fun out -> ok 2 out && not (contains "x-t" out) )
  ]

let test_cases () =
  List.iter
    (fun (label, request, expected) ->
       let reference = serve (request (fun _ name -> name)) in
       if not (expected reference)
       then Alcotest.failf "%s: unexpected output %S" label reference;
       List.iter
         (fun (spelling, f) ->
            let input = request f in
            Alcotest.(check string)
              (Printf.sprintf "%s, %s" label spelling)
              reference
              (serve input);
            Alcotest.(check string)
              (Printf.sprintf "%s, %s, byte by byte" label spelling)
              reference
              (serve_bytewise input))
         variants)
    cases

module Recorder = struct
  type t = { mutable got : string }

  let next_read_operation _ = `Read

  let read t bs ~off ~len =
    t.got <- t.got ^ Bigstring.substring bs ~off ~len;
    len

  let read_eof = read
  let yield_reader _ _ = ()
  let next_write_operation _ = `Yield
  let report_write_result _ _ = ()
  let yield_writer _ _ = ()
  let report_exn _ _ = ()
  let is_closed _ = false
  let shutdown _ = ()
end

(* After an upgrade request, the reader stops at the end of its head, so the new
   protocol gets the next bytes. *)
let test_upgrade () =
  let upgrade ~bytewise input =
    let recorder = { Recorder.got = "" } in
    let t =
      Testing.create (fun reqd ->
        Request_descriptor.respond_with_upgrade
          reqd
          Headers.empty
          (Runtime.make (module Recorder) recorder))
    in
    if bytewise
    then String.iter (fun c -> Testing.feed t (String.make 1 c)) input
    else Testing.feed t input;
    ignore (Testing.output t : string);
    recorder.got
  in
  List.iter
    (fun (label, request) ->
       List.iter
         (fun (spelling, f) ->
            List.iter
              (fun bytewise ->
                 Alcotest.(check string)
                   (Printf.sprintf
                      "%s, %s%s"
                      label
                      spelling
                      (if bytewise then ", byte by byte" else ""))
                   "ping\r\n"
                   (upgrade ~bytewise (request f)))
              [ false; true ])
         variants)
    [ ( "Upgrade"
      , fun f ->
          "GET / HTTP/1.1\r\nHost: a\r\n"
          ^ f 0 "Connection"
          ^ ": upgrade\r\n"
          ^ f 1 "Upgrade"
          ^ ": echo\r\n\r\nping\r\n" )
    ; ( "Upgrade before two Connection fields"
      , fun f ->
          "GET / HTTP/1.1\r\nHost: a\r\n"
          ^ f 0 "Upgrade"
          ^ ": echo\r\n"
          ^ f 1 "Connection"
          ^ ": keep-alive\r\n"
          ^ f 2 "Connection"
          ^ ": upgrade\r\n\r\nping\r\n" )
    ]

let tests =
  [ "framing and connection fields in any case, duplicated", `Quick, test_cases
  ; "upgrade fields in any case", `Quick, test_upgrade
  ]
