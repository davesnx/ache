(* Target 5, WebSocket: frames after an upgrade. The frame header parser is
   split invariant, and a text message is delivered exactly when it is valid
   UTF-8. *)

open Ache

let handshake =
  "GET /chat HTTP/1.1\r\n\
   Host: example.com\r\n\
   Upgrade: websocket\r\n\
   Connection: Upgrade\r\n\
   Sec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ==\r\n\
   Sec-WebSocket-Version: 13\r\n\
   \r\n"

let mask_key = "\x37\xfa\x21\x3d"

type length_form =
  | Minimal
  | Two
  | Eight

let frame
      ?(fin = true)
      ?(rsv = 0)
      ?(masked = true)
      ?(form = Minimal)
      opcode
      payload
  =
  let len = String.length payload in
  let b = Buffer.create (len + 14) in
  Buffer.add_char
    b
    (Char.chr ((if fin then 0x80 else 0) lor (rsv lsl 4) lor opcode));
  let m = if masked then 0x80 else 0 in
  (match form with
  | Minimal when len < 126 -> Buffer.add_char b (Char.chr (m lor len))
  | (Minimal | Two) when len < 0x10000 ->
    Buffer.add_char b (Char.chr (m lor 126));
    Buffer.add_uint16_be b len
  | Minimal | Two | Eight ->
    Buffer.add_char b (Char.chr (m lor 127));
    Buffer.add_int64_be b (Int64.of_int len));
  if masked
  then (
    Buffer.add_string b mask_key;
    String.iteri
      (fun i c ->
         Buffer.add_char
           b
           (Char.chr (Char.code c lxor Char.code mask_key.[i land 3])))
      payload)
  else Buffer.add_string b payload;
  Buffer.contents b

(* Mostly UTF-8, with invalid sequences: lone continuation and lead bytes,
   surrogates, overlong forms, code points past U+10FFFF. *)
let utf8ish st =
  String.concat
    ""
    (List.init (Gen.int st 10) (fun _ ->
       Gen.weighted
         st
         [ 6, "a"
         ; 2, "\xc3\xa9"
         ; 2, "\xe2\x82\xac"
         ; 2, "\xf0\x9d\x84\x9e"
         ; 1, "\xc3"
         ; 1, "\x80"
         ; 1, "\xed\xa0\x80"
         ; 1, "\xc0\x80"
         ; 1, "\xf4\x90\x80\x80"
         ; 1, String.make 1 (Char.chr (Gen.int st 256))
         ]))

let gen_frame st =
  let opcode =
    Gen.weighted st [ 5, 0x1; 2, 0x0; 2, 0x9; 1, 0xA; 1, 0x8; 1, 0x2; 1, 0xB ]
  in
  let payload =
    if opcode = 0x8
    then
      Gen.pick
        st
        [ ""; "\x03"; "\x03\xe8"; "\x03\xe8bye"; "\x03\xe8\xff"; "\x00\x01" ]
    else utf8ish st
  in
  frame
    ~fin:(Gen.weighted st [ 4, true; 1, false ])
    ~rsv:(if Gen.chance st 20 then 1 else 0)
    ~masked:(not (Gen.chance st 20))
    ~form:(Gen.weighted st [ 8, Minimal; 1, Two; 1, Eight ])
    opcode
    payload

let seeds =
  lazy
    [| frame 0x1 "hello"
     ; frame 0x9 "p"
     ; frame ~fin:false 0x1 "a" ^ frame 0x0 "b"
    |]

type conn =
  { messages : string list ref
  ; closes : int option list ref
  ; feeder : Harness.Feeder.t
  ; output : Buffer.t
  }

let connect () =
  let messages = ref []
  and closes = ref [] in
  let conn =
    Server_connection.create (fun reqd ->
      match
        Websocket.upgrade reqd (fun _ ->
          { on_message = (fun m -> messages := m :: !messages)
          ; on_close = (fun code -> closes := code :: !closes)
          })
      with
      | Ok () -> ()
      | Error _ ->
        Request_descriptor.respond_with_string
          reqd
          (Response.create `Bad_request)
          "")
  in
  let output = Buffer.create 64 in
  let feeder =
    Harness.Feeder.make
      ~next_read_operation:(fun () ->
        Server_connection.next_read_operation conn)
      ~read:(Server_connection.read conn)
      ~read_eof:(Server_connection.read_eof conn)
      ~drain:(fun () ->
        Roundtrip.drain_all
          ~next:(fun () -> Server_connection.next_write_operation conn)
          ~report:(Server_connection.report_write_result conn)
          output)
  in
  { messages; closes; feeder; output }

let run ~eof chunks =
  let c = connect () in
  if eof
  then Harness.Feeder.run c.feeder chunks
  else List.iter (Harness.Feeder.feed c.feeder) chunks;
  { Harness.events =
      List.rev_map (Printf.sprintf "message %S") !(c.messages)
      @ List.rev_map
          (function
            | None -> "close" | Some code -> Printf.sprintf "close %d" code)
          !(c.closes)
  ; output = Buffer.contents c.output
  ; (* The connection drops the bytes after a close. *)
    consumed = 0
  ; read_op = ""
  ; write_op = ""
  }

type case =
  { input : string
  ; chunks : string list
  ; eof : bool
  }

let gen st =
  let frames =
    String.concat "" (List.init (Gen.range st 1 6) (fun _ -> gen_frame st))
  in
  let frames =
    Gen.mutations ~seeds st (Gen.weighted st [ 6, 0; 2, 1; 1, 2 ]) frames
  in
  let input = handshake ^ frames in
  { input; chunks = Gen.split st input; eof = Gen.bool st }

let print c =
  Printf.sprintf
    "input %S\nreads [%s]\nend of input %b"
    c.input
    (Prop.sizes c.chunks)
    c.eof

let split_prop c =
  Prop.split_invariant ~run:(run ~eof:c.eof) ~input:c.input ~chunks:c.chunks

(* A text message in fragments, with pings between them. *)
type message_case =
  { payload : string
  ; cuts : int list
  ; pings : bool
  ; mreads : int list
  }

let gen_message st =
  let payload = utf8ish st in
  { payload
  ; cuts =
      List.sort
        compare
        (List.init (Gen.int st 3) (fun _ ->
           Gen.int st (String.length payload + 1)))
  ; pings = Gen.bool st
  ; mreads = List.init (Gen.range st 1 8) (fun _ -> Gen.range st 1 20)
  }

let print_message c =
  Printf.sprintf
    "payload %S, valid %b\ncuts [%s], pings %b, reads [%s]"
    c.payload
    (String.is_valid_utf_8 c.payload)
    (String.concat "," (List.map string_of_int c.cuts))
    c.pings
    (String.concat "," (List.map string_of_int c.mreads))

let fragments c =
  let rec go p cuts acc =
    match cuts with
    | [] ->
      List.rev (String.sub c.payload p (String.length c.payload - p) :: acc)
    | q :: rest -> go q rest (String.sub c.payload p (q - p) :: acc)
  in
  go 0 c.cuts []

let close_1007 = "\x88\x02\x03\xef"

let message_prop c =
  let parts = fragments c in
  let n = List.length parts in
  let frames =
    List.mapi
      (fun i part ->
         frame ~fin:(i = n - 1) (if i = 0 then 0x1 else 0x0) part
         ^ if c.pings && i < n - 1 then frame 0x9 "p" else "")
      parts
  in
  let t =
    run
      ~eof:false
      (Gen.chunks_of_sizes (handshake ^ String.concat "" frames) c.mreads)
  in
  let messages = List.filter (String.starts_with ~prefix:"message") t.events in
  if String.is_valid_utf_8 c.payload
  then (
    if messages <> [ Printf.sprintf "message %S" c.payload ]
    then
      Prop.fail "valid message not delivered: %s" (String.concat "; " t.events);
    true)
  else if messages <> []
  then Prop.fail "invalid message delivered: %s" (String.concat "; " messages)
  else if not (String.ends_with ~suffix:close_1007 t.output)
  then Prop.fail "no close frame with code 1007; output %S" t.output
  else true

let tests =
  [ Prop.test
      ~name:"websocket: split invariance"
      ~count:5000
      ~print
      gen
      split_prop
  ; Prop.test
      ~name:"websocket: text delivered iff valid UTF-8"
      ~count:5000
      ~print:print_message
      gen_message
      message_prop
  ]
