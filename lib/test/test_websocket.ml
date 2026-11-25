open Ache

let rfc_key = "dGhlIHNhbXBsZSBub25jZQ=="

let handshake_headers =
  [ "host", "server.example.com"
  ; "upgrade", "websocket"
  ; "connection", "Upgrade"
  ; "sec-websocket-key", rfc_key
  ; "sec-websocket-version", "13"
  ]

let request_string headers =
  "GET /chat HTTP/1.1\r\n"
  ^ String.concat "" (List.map (fun (k, v) -> k ^ ": " ^ v ^ "\r\n") headers)
  ^ "\r\n"

let response_101 =
  "HTTP/1.1 101 Switching Protocols\r\n\
   upgrade: websocket\r\n\
   connection: Upgrade\r\n\
   sec-websocket-accept: s3pPLMBiTxaQ9kYGzzhZRbK+xOo=\r\n\
   \r\n"

type conn =
  { server : Server_connection.t
  ; mutable leftover : string
  ; messages : Websocket.message list ref
  ; pongs : string list ref
  ; closed : Websocket.close option list ref
  ; ws : Websocket.t option ref
  }

let feed c s =
  let data = c.leftover ^ s in
  let len = String.length data in
  let n =
    Server_connection.read
      c.server
      (Bigstring.of_string ~off:0 ~len data)
      ~off:0
      ~len
  in
  c.leftover <- String.sub data n (len - n)

let feed_eof c =
  let len = String.length c.leftover in
  let (_ : int) =
    Server_connection.read_eof
      c.server
      (Bigstring.of_string ~off:0 ~len c.leftover)
      ~off:0
      ~len
  in
  c.leftover <- ""

let drain c =
  let buf = Stdlib.Buffer.create 64 in
  let rec go () =
    match Server_connection.next_write_operation c.server with
    | `Write iovecs ->
      let n =
        List.fold_left
          (fun acc { IOVec.buffer; off; len } ->
             Stdlib.Buffer.add_string buf (Bigstring.substring buffer ~off ~len);
             acc + len)
          0
          iovecs
      in
      Server_connection.report_write_result c.server (`Ok n);
      go ()
    | `Yield -> `Yield
    | `Close _ -> `Close
  in
  let state = go () in
  Stdlib.Buffer.contents buf, state

let write_state =
  Alcotest.testable
    (fun fmt s ->
       Format.pp_print_string
         fmt
         (match s with `Yield -> "`Yield" | `Close -> "`Close"))
    ( = )

let hex s =
  String.concat
    " "
    (List.map
       (fun c -> Printf.sprintf "%02x" (Char.code c))
       (List.of_seq (String.to_seq s)))

let check_output ?(msg = "output") c expected state =
  let output, state' = drain c in
  Alcotest.(check string) msg (hex expected) (hex output);
  Alcotest.check write_state (msg ^ ": write state") state state'

let connect ?max_message_size ?(on_open = fun (_ : Websocket.t) -> ()) () =
  let messages = ref []
  and pongs = ref []
  and closed = ref []
  and ws = ref None in
  let server =
    Server_connection.create (fun reqd ->
      match
        Websocket.upgrade ?max_message_size reqd (fun t ->
          ws := Some t;
          on_open t;
          { on_message = (fun m -> messages := m :: !messages)
          ; on_pong = (fun p -> pongs := p :: !pongs)
          ; on_close = (fun close -> closed := close :: !closed)
          })
      with
      | Ok () -> ()
      | Error _ -> Alcotest.fail "handshake rejected")
  in
  let c = { server; leftover = ""; messages; pongs; closed; ws } in
  feed c (request_string handshake_headers);
  c

let upgraded ?max_message_size ?on_open () =
  let c = connect ?max_message_size ?on_open () in
  let output, state = drain c in
  Alcotest.(check string) "101 response" response_101 output;
  Alcotest.check write_state "101 written" `Yield state;
  c

let ws c = Option.get !(c.ws)
let mask_key = "\x37\xfa\x21\x3d"

let client_frame
      ?(fin = true)
      ?(rsv = 0)
      ?(masked = true)
      ?length
      opcode
      payload
  =
  let len = String.length payload in
  let b = Stdlib.Buffer.create (len + 14) in
  Stdlib.Buffer.add_char
    b
    (Char.chr ((if fin then 0x80 else 0) lor (rsv lsl 4) lor opcode));
  let m = if masked then 0x80 else 0 in
  let length = Option.value length ~default:(Int64.of_int len) in
  if Int64.compare length 126L < 0
  then Stdlib.Buffer.add_char b (Char.chr (m lor Int64.to_int length))
  else if Int64.compare length 0x10000L < 0
  then (
    Stdlib.Buffer.add_char b (Char.chr (m lor 126));
    Stdlib.Buffer.add_uint16_be b (Int64.to_int length))
  else (
    Stdlib.Buffer.add_char b (Char.chr (m lor 127));
    Stdlib.Buffer.add_int64_be b length);
  if masked
  then (
    Stdlib.Buffer.add_string b mask_key;
    String.iteri
      (fun i c ->
         Stdlib.Buffer.add_char
           b
           (Char.chr (Char.code c lxor Char.code mask_key.[i land 3])))
      payload)
  else Stdlib.Buffer.add_string b payload;
  Stdlib.Buffer.contents b

let close_frame_out code =
  Printf.sprintf
    "\x88\x02%c%c"
    (Char.chr (code lsr 8))
    (Char.chr (code land 0xff))

let close_payload code =
  Printf.sprintf "%c%c" (Char.chr (code lsr 8)) (Char.chr (code land 0xff))

let message =
  Alcotest.testable
    (fun fmt m ->
       match m with
       | Websocket.Text s -> Format.fprintf fmt "Text %S" s
       | Binary s -> Format.fprintf fmt "Binary %S" s)
    ( = )

let check_message_list c expected =
  Alcotest.(check (list message)) "messages" expected (List.rev !(c.messages))

let check_messages c expected =
  check_message_list c (List.map (fun s -> Websocket.Text s) expected)

let check_closed c expected =
  Alcotest.(check (list (option int)))
    "on_close calls"
    expected
    (List.map (Option.map (fun { Websocket.code; _ } -> code)) !(c.closed))

let check_close_reasons c expected =
  Alcotest.(check (list (option (pair int string))))
    "on_close calls"
    expected
    (List.map
       (Option.map (fun { Websocket.code; reason } -> code, reason))
       !(c.closed))

let check_reader_closed c =
  Alcotest.(check bool)
    "reader closed"
    true
    (Server_connection.next_read_operation c.server = `Close)

let test_accept_key () =
  Alcotest.(check string)
    "RFC 6455 §1.3"
    "s3pPLMBiTxaQ9kYGzzhZRbK+xOo="
    (Websocket.accept_key rfc_key);
  Alcotest.(check string)
    "empty key, one SHA-1 block"
    "Kfh9QIsMVZcl6xEPYxPHzW8SZ8w="
    (Websocket.accept_key "");
  Alcotest.(check string)
    "100-byte key, three SHA-1 blocks"
    "DJTE+uYDnPxiT+W6VvIG/iPUxv8="
    (Websocket.accept_key (String.make 100 'x'))

let handshake_error =
  Alcotest.testable
    (fun fmt e ->
       Format.pp_print_string
         fmt
         (match e with
         | `Not_get -> "`Not_get"
         | `Not_websocket -> "`Not_websocket"
         | `Unsupported_version -> "`Unsupported_version"
         | `Invalid_key -> "`Invalid_key"))
    ( = )

let check_handshake
      name
      ?(meth = `GET)
      ?(version = Version.v1_1)
      headers
      expected
  =
  Alcotest.(check (result string handshake_error))
    name
    expected
    (Websocket.check_handshake
       (Request.create ~version ~headers:(Headers.of_list headers) meth "/"))

let replace name value =
  List.map (fun (k, v) -> if k = name then k, value else k, v) handshake_headers

let remove name = List.filter (fun (k, _) -> k <> name) handshake_headers
let accept = Ok "s3pPLMBiTxaQ9kYGzzhZRbK+xOo="

let test_handshake_accept () =
  check_handshake "RFC 6455 §1.3 request" handshake_headers accept;
  check_handshake
    "tokens in lists, any case"
    (replace "connection" "keep-alive, UPGRADE"
    |> List.map (fun (k, v) ->
      if k = "upgrade" then k, "foo, WebSocket" else k, v))
    accept;
  check_handshake
    "spaces around the key and version"
    (replace "sec-websocket-key" (" " ^ rfc_key ^ " "))
    accept;
  check_handshake
    "Connection in two fields"
    (remove "connection"
    @ [ "connection", "keep-alive"; "connection", "upgrade" ])
    accept

let test_handshake_reject () =
  check_handshake "POST" ~meth:`POST handshake_headers (Error `Not_get);
  check_handshake
    "HTTP/1.0"
    ~version:Version.v1_0
    handshake_headers
    (Error `Not_get);
  check_handshake "no Upgrade" (remove "upgrade") (Error `Not_websocket);
  check_handshake
    "Upgrade without websocket"
    (replace "upgrade" "h2c")
    (Error `Not_websocket);
  check_handshake "no Connection" (remove "connection") (Error `Not_websocket);
  check_handshake
    "Connection without upgrade"
    (replace "connection" "keep-alive")
    (Error `Not_websocket);
  check_handshake
    "no version"
    (remove "sec-websocket-version")
    (Error `Unsupported_version);
  check_handshake
    "version 8"
    (replace "sec-websocket-version" "8")
    (Error `Unsupported_version);
  check_handshake "no key" (remove "sec-websocket-key") (Error `Invalid_key);
  check_handshake
    "key of 15 bytes"
    (replace "sec-websocket-key" "dGhlIHNhbXBsZSBub25j")
    (Error `Invalid_key);
  check_handshake
    "key with a character outside base64"
    (replace "sec-websocket-key" "dGhlIHNhbXBsZSBub25jZ*==")
    (Error `Invalid_key);
  check_handshake
    "key whose padding bits are not zero"
    (replace "sec-websocket-key" "AAAAAAAAAAAAAAAAAAAAAB==")
    (Error `Invalid_key);
  check_handshake
    "key with the RFC sample's last data character changed"
    (replace "sec-websocket-key" "dGhlIHNhbXBsZSBub25jZR==")
    (Error `Invalid_key);
  check_handshake
    "two keys"
    (handshake_headers @ [ "sec-websocket-key", rfc_key ])
    (Error `Invalid_key)

let test_canonical_keys () =
  String.iter
    (fun last ->
       let key = String.make 21 'A' ^ String.make 1 last ^ "==" in
       Alcotest.(check bool)
         key
         (String.contains "AQgw" last)
         (Result.is_ok
            (Websocket.check_handshake
               (Request.create
                  ~headers:(Headers.of_list (replace "sec-websocket-key" key))
                  `GET
                  "/"))))
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"

let test_upgrade_rejected_responds_nothing () =
  let result = ref None in
  let server =
    Server_connection.create (fun reqd ->
      result :=
        Some (Websocket.upgrade reqd (fun _ -> Alcotest.fail "handler called"));
      Request_descriptor.respond_with_string
        reqd
        (Response.create
           ~headers:(Headers.of_list [ "content-length", "0" ])
           `Bad_request)
        "")
  in
  let c =
    { server
    ; leftover = ""
    ; messages = ref []
    ; pongs = ref []
    ; closed = ref []
    ; ws = ref None
    }
  in
  feed c (request_string (remove "sec-websocket-version"));
  Alcotest.(check (option (result unit handshake_error)))
    "error"
    (Some (Error `Unsupported_version))
    !result;
  let output, (_ : [ `Yield | `Close ]) = drain c in
  Alcotest.(check string)
    "only the caller's response"
    "HTTP/1.1 400 Bad Request\r\ncontent-length: 0\r\n\r\n"
    output

let hello_masked = "\x81\x85\x37\xfa\x21\x3d\x7f\x9f\x4d\x51\x58"

let test_masked_text () =
  let c = upgraded () in
  Alcotest.(check string)
    "client_frame matches RFC 6455 §5.7"
    hello_masked
    (client_frame 0x1 "Hello");
  feed c hello_masked;
  check_messages c [ "Hello" ];
  check_output c "" `Yield

let test_send_text () =
  let c = upgraded () in
  Websocket.send_text (ws c) "Hello";
  Websocket.send_text (ws c) (String.make 300 'a');
  Websocket.send_text (ws c) (String.make 70000 'b');
  check_output
    c
    ("\x81\x05Hello"
    ^ "\x81\x7e\x01\x2c"
    ^ String.make 300 'a'
    ^ "\x81\x7f\x00\x00\x00\x00\x00\x01\x11\x70"
    ^ String.make 70000 'b')
    `Yield

let test_send_before_101 () =
  let c = connect ~on_open:(fun t -> Websocket.send_text t "hi") () in
  let output, state = drain c in
  Alcotest.(check string)
    "101 then the frame"
    (response_101 ^ "\x81\x02hi")
    output;
  Alcotest.check write_state "state" `Yield state

let test_flush () =
  let c = upgraded () in
  let results = ref [] in
  let record r = results := r :: !results in
  Websocket.flush (ws c) record;
  Alcotest.(check int) "nothing queued: at once" 1 (List.length !results);
  Websocket.send_text (ws c) "Hello";
  Websocket.flush (ws c) record;
  Alcotest.(check int) "not written yet" 1 (List.length !results);
  check_output c "\x81\x05Hello" `Yield;
  Alcotest.(check bool) "both `Written" true (!results = [ `Written; `Written ])

let test_fragmented () =
  let c = upgraded () in
  feed c (client_frame ~fin:false 0x1 "Hel");
  check_messages c [];
  feed c (client_frame 0x9 "p");
  check_output ~msg:"pong between fragments" c "\x8a\x01p" `Yield;
  feed c (client_frame ~fin:false 0x0 "l");
  feed c (client_frame 0x0 "o");
  check_messages c [ "Hello" ];
  feed c (client_frame 0x1 "next");
  check_messages c [ "Hello"; "next" ]

let test_split_reads () =
  let c = upgraded () in
  let bytes =
    client_frame 0x1 "Hello"
    ^ client_frame 0x1 (String.make 300 'x')
    ^ client_frame ~length:70000L 0x1 (String.make 70000 'y')
  in
  String.iter (fun ch -> feed c (String.make 1 ch)) bytes;
  check_messages c [ "Hello"; String.make 300 'x'; String.make 70000 'y' ];
  let c = upgraded () in
  feed c (bytes ^ client_frame 0x9 "");
  check_messages c [ "Hello"; String.make 300 'x'; String.make 70000 'y' ];
  check_output c "\x8a\x00" `Yield

let test_ping_pong () =
  let c = upgraded () in
  feed c (client_frame 0x9 "Hello");
  check_output c "\x8a\x05Hello" `Yield;
  feed c (client_frame 0xA "unsolicited");
  check_output ~msg:"no answer to a pong" c "" `Yield;
  Alcotest.(check (list string)) "on_pong" [ "unsolicited" ] !(c.pongs)

let test_close_echo () =
  let c = upgraded () in
  feed c (client_frame 0x8 (close_payload 1000 ^ "bye"));
  check_closed c [];
  check_output c (close_frame_out 1000) `Close;
  check_close_reasons c [ Some (1000, "bye") ];
  check_reader_closed c;
  Alcotest.(check bool) "is_closed" true (Websocket.is_closed (ws c));
  let c = upgraded () in
  feed c (client_frame 0x8 "");
  check_output ~msg:"empty close" c "\x88\x00" `Close;
  check_close_reasons c [ Some (1005, "") ]

let test_data_after_close_ignored () =
  let c = upgraded () in
  feed c (client_frame 0x8 (close_payload 1001) ^ client_frame 0x1 "late");
  check_messages c [];
  check_output c (close_frame_out 1001) `Close;
  check_closed c [ Some 1001 ]

let fails_with ?max_message_size code bytes () =
  let c = upgraded ?max_message_size () in
  feed c bytes;
  check_messages c [];
  check_reader_closed c;
  check_output c (close_frame_out code) `Close;
  check_closed c [ None ];
  feed c (client_frame 0x1 "after");
  check_messages c [];
  check_output ~msg:"nothing after close" c "" `Close

let protocol_errors =
  [ "unmasked frame", client_frame ~masked:false 0x1 "Hello"
  ; "opcode 3", client_frame 0x3 ""
  ; "opcode 0xB", client_frame 0xB ""
  ; "RSV1", client_frame ~rsv:4 0x1 "Hello"
  ; "RSV3", client_frame ~rsv:1 0x1 "Hello"
  ; "fragmented ping", client_frame ~fin:false 0x9 ""
  ; "ping of 126 bytes", client_frame 0x9 (String.make 126 'p')
  ; ( "close of 126 bytes"
    , client_frame 0x8 (close_payload 1000 ^ String.make 124 'r') )
  ; "continuation without start", client_frame 0x0 "lo"
  ; ( "text inside a fragmented message"
    , client_frame ~fin:false 0x1 "Hel" ^ client_frame 0x1 "lo" )
  ; ( "binary inside a fragmented message"
    , client_frame ~fin:false 0x1 "Hel" ^ client_frame 0x2 "lo" )
  ; ( "16-bit length below 126"
    , "\x81\xfe\x00\x7d" ^ mask_key ^ String.make 125 'a' )
  ; ( "64-bit length below 65536"
    , "\x81\xff\x00\x00\x00\x00\x00\x00\xff\xff"
      ^ mask_key
      ^ String.make 65535 'a' )
  ; ( "64-bit length with the high bit set"
    , "\x81\xff\x80\x00\x00\x00\x00\x00\x00\x00" ^ mask_key )
  ; "close with 1 byte", client_frame 0x8 "\x03"
  ; "close code 999", client_frame 0x8 (close_payload 999)
  ; "close code 1005", client_frame 0x8 (close_payload 1005)
  ; "close code 1006", client_frame 0x8 (close_payload 1006)
  ; "close code 1015", client_frame 0x8 (close_payload 1015)
  ; "close code 5000", client_frame 0x8 (close_payload 5000)
  ]

let test_invalid_utf8 = fails_with 1007 (client_frame 0x1 "\xff\xfe")

let test_invalid_utf8_split =
  fails_with
    1007
    (client_frame ~fin:false 0x1 "caf\xc3" ^ client_frame 0x0 "\xa9\xff")

let test_utf8_split_across_fragments () =
  let c = upgraded () in
  feed c (client_frame ~fin:false 0x1 "caf\xc3" ^ client_frame 0x0 "\xa9");
  check_messages c [ "caf\xc3\xa9" ];
  check_output c "" `Yield

let test_invalid_utf8_close_reason =
  fails_with 1007 (client_frame 0x8 (close_payload 1000 ^ "\xff"))

let test_too_large_header_only =
  fails_with ~max_message_size:10 1009 ("\x81\x8b" ^ mask_key)

let test_too_large_fragments =
  fails_with
    ~max_message_size:10
    1009
    (client_frame ~fin:false 0x1 "123456" ^ client_frame 0x0 "12345")

let test_too_large_64bit =
  fails_with 1009 ("\x81\xff\x00\x00\x01\x00\x00\x00\x00\x00" ^ mask_key)

let test_max_size_exact () =
  let c = upgraded ~max_message_size:10 () in
  feed c (client_frame ~fin:false 0x1 "12345" ^ client_frame 0x0 "67890");
  check_messages c [ "1234567890" ];
  feed c (client_frame 0x9 (String.make 125 'p'));
  check_output
    ~msg:"control frames do not count"
    c
    ("\x8a\x7d" ^ String.make 125 'p')
    `Yield

let test_binary () =
  let c = upgraded () in
  feed c (client_frame 0x2 "\x00\xff");
  feed
    c
    (client_frame ~fin:false 0x2 "\xc3"
    ^ client_frame 0x9 "p"
    ^ client_frame 0x0 "\xff\xfe");
  feed c (client_frame 0x1 "text");
  check_message_list c [ Binary "\x00\xff"; Binary "\xc3\xff\xfe"; Text "text" ];
  check_output c "\x8a\x01p" `Yield

let test_send_binary () =
  let c = upgraded () in
  Websocket.send_binary (ws c) "\xff";
  Websocket.send_binary (ws c) (String.make 300 '\000');
  check_output
    c
    ("\x82\x01\xff" ^ "\x82\x7e\x01\x2c" ^ String.make 300 '\000')
    `Yield

let test_ping () =
  let c = upgraded () in
  Websocket.ping (ws c) "x";
  check_output c "\x89\x01x" `Yield;
  feed c (client_frame 0xA "x");
  Alcotest.(check (list string)) "on_pong" [ "x" ] !(c.pongs);
  Alcotest.check_raises
    "126 bytes"
    (Invalid_argument "Ache.Websocket.ping: payload longer than 125 bytes")
    (fun () -> Websocket.ping (ws c) (String.make 126 'p'));
  check_output ~msg:"nothing written by the raise" c "" `Yield;
  Websocket.close (ws c) ~code:1000;
  Websocket.ping (ws c) "late";
  check_output ~msg:"no ping after close" c (close_frame_out 1000) `Yield

let test_close_reason () =
  let c = upgraded () in
  Alcotest.check_raises
    "reason not UTF-8"
    (Invalid_argument "Ache.Websocket.close: reason is not valid UTF-8")
    (fun () -> Websocket.close ~reason:"\xff" (ws c) ~code:1000);
  Alcotest.check_raises
    "reason of 124 bytes"
    (Invalid_argument "Ache.Websocket.close: reason longer than 123 bytes")
    (fun () -> Websocket.close ~reason:(String.make 124 'r') (ws c) ~code:1000);
  check_output ~msg:"nothing written by the raises" c "" `Yield;
  Websocket.close ~reason:"bye" (ws c) ~code:1000;
  check_output c "\x88\x05\x03\xe8bye" `Yield;
  let c = upgraded () in
  let reason = String.make 123 'r' in
  Websocket.close ~reason (ws c) ~code:4000;
  check_output c ("\x88\x7d\x0f\xa0" ^ reason) `Yield

let test_length_boundaries () =
  let c = upgraded () in
  feed c (client_frame 0x2 (String.make 126 'a'));
  feed c (client_frame 0x2 (String.make 65536 'b'));
  check_message_list
    c
    [ Binary (String.make 126 'a'); Binary (String.make 65536 'b') ];
  check_output c "" `Yield

(* RFC 3629 §4 boundaries. *)
let valid_utf8 =
  [ "\xc2\x80"
  ; "\xdf\xbf"
  ; "\xe0\xa0\x80"
  ; "\xed\x9f\xbf"
  ; "\xee\x80\x80"
  ; "\xef\xbf\xbf"
  ; "\xf0\x90\x80\x80"
  ; "\xf4\x8f\xbf\xbf"
  ]

let invalid_utf8 =
  [ "overlong 2-byte", "\xc0\x80"
  ; "overlong 2-byte C1", "\xc1\xbf"
  ; "overlong 3-byte", "\xe0\x80\x80"
  ; "surrogate", "\xed\xa0\x80"
  ; "overlong 4-byte", "\xf0\x80\x80\x80"
  ; "above U+10FFFF", "\xf4\x90\x80\x80"
  ; "F5", "\xf5\x80\x80\x80"
  ; "lone continuation", "\x80"
  ; "ASCII inside a sequence", "\xe2\x82a"
  ; "truncated at the end", "caf\xc3"
  ]

let test_utf8_boundaries () =
  let c = upgraded () in
  List.iter (fun s -> feed c (client_frame 0x1 s)) valid_utf8;
  check_messages c valid_utf8;
  check_output c "" `Yield;
  let c = upgraded () in
  let text = "caf\xc3\xa9 \xe2\x82\xac \xf0\x9f\x98\x80" in
  String.iter
    (fun ch -> feed c (String.make 1 ch))
    (client_frame ~fin:false 0x1 (String.sub text 0 4)
    ^ client_frame ~fin:false 0x0 (String.sub text 4 5)
    ^ client_frame 0x0 (String.sub text 9 (String.length text - 9)));
  check_messages c [ text ]

let test_utf8_state_across_control_frames () =
  let c = upgraded () in
  feed
    c
    (client_frame ~fin:false 0x1 "\xe2"
    ^ client_frame 0x9 "\xff"
    ^ client_frame 0xA "\xff"
    ^ client_frame 0x0 "\x82\xac");
  check_messages c [ "\xe2\x82\xac" ];
  check_output c "\x8a\x01\xff" `Yield;
  fails_with
    1007
    (client_frame ~fin:false 0x1 "\xe2"
    ^ client_frame 0xA ""
    ^ client_frame 0x0 "a")
    ()

let test_utf8_fails_in_first_fragment () =
  let c = upgraded () in
  feed c (client_frame ~fin:false 0x1 "ok\xff");
  check_reader_closed c;
  check_output c (close_frame_out 1007) `Close;
  check_messages c []

let test_utf8_fails_inside_a_frame () =
  let c = upgraded () in
  let frame =
    client_frame 0x1 (String.make 10 'a' ^ "\xff" ^ String.make 989 'a')
  in
  feed c (String.sub frame 0 28);
  check_reader_closed c;
  check_output c (close_frame_out 1007) `Close;
  check_messages c []

let test_eof_without_close () =
  let c = upgraded () in
  feed c (String.sub hello_masked 0 4);
  feed_eof c;
  check_messages c [];
  check_reader_closed c;
  check_output c "" `Close;
  check_closed c [ None ]

let test_write_closed_no_spin () =
  let c = upgraded () in
  Websocket.send_text (ws c) "Hello";
  let flushed = ref [] in
  Websocket.flush (ws c) (fun r -> flushed := r :: !flushed);
  (match Server_connection.next_write_operation c.server with
  | `Write _ -> Server_connection.report_write_result c.server `Closed
  | _ -> Alcotest.fail "expected a write");
  Alcotest.(check bool) "flush got `Closed" true (!flushed = [ `Closed ]);
  check_closed c [ None ];
  check_reader_closed c;
  for _ = 1 to 3 do
    Websocket.send_text (ws c) "more";
    match Server_connection.next_write_operation c.server with
    | `Close _ -> ()
    | `Write _ -> Alcotest.fail "`Write after `Closed: the IO loop would spin"
    | `Yield -> Alcotest.fail "`Yield after `Closed"
  done;
  Websocket.flush (ws c) (fun r -> flushed := r :: !flushed);
  Alcotest.(check bool)
    "later flush `Closed"
    true
    (!flushed = [ `Closed; `Closed ]);
  Alcotest.(check bool) "is_closed" true (Websocket.is_closed (ws c));
  check_closed c [ None ]

let read_operation c =
  match Server_connection.next_read_operation c.server with
  | `Read -> "`Read"
  | `Yield -> "`Yield"
  | `Close -> "`Close"

let test_server_close () =
  let c = upgraded () in
  Websocket.close (ws c) ~code:1001;
  Websocket.send_text (ws c) "dropped";
  Websocket.close (ws c) ~code:1000;
  check_output
    ~msg:"one close frame, then wait for the client's"
    c
    (close_frame_out 1001)
    `Yield;
  Alcotest.(check string) "still reading" "`Read" (read_operation c);
  feed
    c
    (client_frame 0x1 "dropped"
    ^ client_frame 0x9 "no pong"
    ^ client_frame 0x8 (close_payload 4000 ^ "done"));
  check_messages c [];
  check_reader_closed c;
  check_output ~msg:"no pong, no second close frame" c "" `Close;
  check_close_reasons c [ Some (4000, "done") ];
  Alcotest.check_raises
    "reserved code"
    (Invalid_argument "Ache.Websocket.close: invalid close code")
    (fun () -> Websocket.close (ws c) ~code:1006)

let test_server_close_eof () =
  let c = upgraded () in
  Websocket.close (ws c) ~code:1001;
  check_output c (close_frame_out 1001) `Yield;
  feed_eof c;
  check_reader_closed c;
  check_output ~msg:"closes at the end of input" c "" `Close;
  check_closed c [ None ]

let test_server_close_resumes_reading () =
  let c = upgraded () in
  Websocket.pause_reading (ws c);
  Alcotest.(check string) "paused" "`Yield" (read_operation c);
  Websocket.close (ws c) ~code:1000;
  check_output c (close_frame_out 1000) `Yield;
  Alcotest.(check string) "reads the client's close" "`Read" (read_operation c);
  feed c (client_frame 0x8 (close_payload 1000));
  check_output c "" `Close;
  check_closed c [ Some 1000 ]

let test_server_close_protocol_error () =
  let c = upgraded () in
  Websocket.close (ws c) ~code:1000;
  check_output c (close_frame_out 1000) `Yield;
  feed c (client_frame ~rsv:1 0x1 "x");
  check_reader_closed c;
  check_output ~msg:"no second close frame" c "" `Close;
  check_closed c [ None ]

let test_101_not_written () =
  let c = connect () in
  check_closed c [];
  (match Server_connection.next_write_operation c.server with
  | `Write _ -> Server_connection.report_write_result c.server `Closed
  | _ -> Alcotest.fail "expected the 101");
  check_closed c [ None ];
  Alcotest.(check bool) "is_closed" true (Websocket.is_closed (ws c))

let test_pong_backpressure () =
  let c = upgraded () in
  let ping = client_frame 0x9 (String.make 125 'p') in
  let pings = ref 0 in
  while read_operation c = "`Read" && !pings < 10_000 do
    feed c ping;
    incr pings
  done;
  Alcotest.(check string)
    (Printf.sprintf "reader yields (after %d pings)" !pings)
    "`Yield"
    (read_operation c);
  let woken = ref false in
  Server_connection.yield_reader c.server (fun () -> woken := true);
  let output, (_ : [ `Yield | `Close ]) = drain c in
  Alcotest.(check bool)
    (Printf.sprintf
       "unwritten output %d <= 64 KiB + one pong"
       (String.length output))
    true
    (String.length output <= 65536 + 127);
  Alcotest.(check int)
    "every ping answered"
    (!pings * 127)
    (String.length output);
  Alcotest.(check bool) "reader woken after the writes" true !woken;
  Alcotest.(check string) "reads again" "`Read" (read_operation c)

let test_pause_reading () =
  let c = upgraded () in
  Websocket.pause_reading (ws c);
  Alcotest.(check string) "paused" "`Yield" (read_operation c);
  let woken = ref false in
  Server_connection.yield_reader c.server (fun () -> woken := true);
  Websocket.resume_reading (ws c);
  Alcotest.(check bool) "resume wakes the reader" true !woken;
  Alcotest.(check string) "reads again" "`Read" (read_operation c);
  feed c hello_masked;
  check_messages c [ "Hello" ]

let tests =
  [ "accept key", `Quick, test_accept_key
  ; "handshake accepted", `Quick, test_handshake_accept
  ; "handshake rejected", `Quick, test_handshake_reject
  ; "canonical keys only", `Quick, test_canonical_keys
  ; ( "rejected upgrade responds nothing"
    , `Quick
    , test_upgrade_rejected_responds_nothing )
  ; "masked text frame", `Quick, test_masked_text
  ; "send_text with 7, 16 and 64-bit lengths", `Quick, test_send_text
  ; "send before the 101 is written", `Quick, test_send_before_101
  ; "flush", `Quick, test_flush
  ; "fragmented message with a ping inside", `Quick, test_fragmented
  ; "reads split at every byte", `Quick, test_split_reads
  ; "ping gets pong", `Quick, test_ping_pong
  ; "client close is echoed", `Quick, test_close_echo
  ; "data after a close frame is ignored", `Quick, test_data_after_close_ignored
  ; "invalid UTF-8 closes 1007", `Quick, test_invalid_utf8
  ; ( "invalid UTF-8 across fragments closes 1007"
    , `Quick
    , test_invalid_utf8_split )
  ; "UTF-8 split across fragments", `Quick, test_utf8_split_across_fragments
  ; ( "invalid UTF-8 close reason closes 1007"
    , `Quick
    , test_invalid_utf8_close_reason )
  ; ( "too large, before the payload, closes 1009"
    , `Quick
    , test_too_large_header_only )
  ; "too large fragments close 1009", `Quick, test_too_large_fragments
  ; "too large 64-bit length closes 1009", `Quick, test_too_large_64bit
  ; "message of max_message_size", `Quick, test_max_size_exact
  ; "binary messages", `Quick, test_binary
  ; "send_binary", `Quick, test_send_binary
  ; "ping and on_pong", `Quick, test_ping
  ; "close with a reason", `Quick, test_close_reason
  ; "shortest lengths at 126 and 65536", `Quick, test_length_boundaries
  ; "UTF-8 boundaries and splits", `Quick, test_utf8_boundaries
  ; ( "control frames keep the UTF-8 state of a message"
    , `Quick
    , test_utf8_state_across_control_frames )
  ; ( "invalid UTF-8 in the first fragment closes 1007"
    , `Quick
    , test_utf8_fails_in_first_fragment )
  ; ( "invalid UTF-8 closes 1007 before the end of the frame"
    , `Quick
    , test_utf8_fails_inside_a_frame )
  ; "end of input without a close frame", `Quick, test_eof_without_close
  ; ( "no write after the write side reports `Closed"
    , `Quick
    , test_write_closed_no_spin )
  ; "server close", `Quick, test_server_close
  ; "server close, then end of input", `Quick, test_server_close_eof
  ; "server close resumes reading", `Quick, test_server_close_resumes_reading
  ; ( "server close, then a protocol error"
    , `Quick
    , test_server_close_protocol_error )
  ; "101 not written shuts the connection down", `Quick, test_101_not_written
  ; "pongs to a client that does not read", `Quick, test_pong_backpressure
  ; "pause and resume reading", `Quick, test_pause_reading
  ]
  @ List.map
      (fun (name, bytes) -> "1002: " ^ name, `Quick, fails_with 1002 bytes)
      protocol_errors
  @ List.map
      (fun (name, bytes) ->
         "1007: " ^ name, `Quick, fails_with 1007 (client_frame 0x1 bytes))
      invalid_utf8
