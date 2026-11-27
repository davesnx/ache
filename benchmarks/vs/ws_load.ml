(* WebSocket echo load for [cost.sh --ws]: each connection sends [depth]
   messages of [size] bytes, reads their echoes, and starts again, until
   [duration] seconds have passed. It prints wrk's "Requests/sec:" line, where a
   request is one echoed message.

   The client frames are masked with the key 0, so the payload is sent as is
   (RFC 6455 §5.3 allows any key); the server still unmasks it. Each domain runs
   its share of the connections in blocking threads. *)

let port = ref 8080
let path = ref "/ws"
let connections = ref 64
let domains = ref 4
let size = ref 64
let depth = ref 1
let duration = ref 10.
let text = ref false

let frame () =
  let n = !size in
  let b = Buffer.create (n + 14) in
  Buffer.add_char b (if !text then '\x81' else '\x82');
  if n < 126
  then Buffer.add_uint8 b (0x80 lor n)
  else if n < 65536
  then (
    Buffer.add_uint8 b 0xfe;
    Buffer.add_uint16_be b n)
  else (
    Buffer.add_uint8 b 0xff;
    Buffer.add_int64_be b (Int64.of_int n));
  Buffer.add_string b "\000\000\000\000";
  Buffer.add_string b (String.make n 'x');
  Buffer.contents b

let handshake =
  Printf.sprintf
    "GET %s HTTP/1.1\r\n\
     Host: 127.0.0.1:%d\r\n\
     Upgrade: websocket\r\n\
     Connection: Upgrade\r\n\
     Sec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ==\r\n\
     Sec-WebSocket-Version: 13\r\n\
     \r\n"

let read_head ic =
  let rec go () = if input_line ic <> "\r" then go () in
  let status = input_line ic in
  if String.length status < 12 || String.sub status 9 3 <> "101"
  then failwith ("ws_load: handshake answered " ^ status);
  go ()

let skip ic buf n =
  let rec go n =
    if n > 0
    then (
      let k = min n (Bytes.length buf) in
      really_input ic buf 0 k;
      go (n - k))
  in
  go n

(* Reads one unmasked frame from the server and checks its length. *)
let read_frame ic buf =
  let b0 = input_byte ic in
  let b1 = input_byte ic in
  if b0 land 0x0f = 8 then failwith "ws_load: the server closed";
  let n =
    match b1 land 0x7f with
    | 126 ->
      let hi = input_byte ic in
      (hi lsl 8) lor input_byte ic
    | 127 ->
      let n = ref 0 in
      for _ = 1 to 8 do
        n := (!n lsl 8) lor input_byte ic
      done;
      !n
    | n -> n
  in
  if n <> !size then failwith "ws_load: unexpected frame length";
  skip ic buf n

let stop = Atomic.make false

let connection batch count () =
  let socket = Unix.socket PF_INET SOCK_STREAM 0 in
  Unix.setsockopt socket TCP_NODELAY true;
  Unix.connect socket (ADDR_INET (Unix.inet_addr_loopback, !port));
  let ic = Unix.in_channel_of_descr socket in
  let oc = Unix.out_channel_of_descr socket in
  output_string oc (handshake !path !port);
  flush oc;
  read_head ic;
  let buf = Bytes.create 65536 in
  while not (Atomic.get stop) do
    output_string oc batch;
    flush oc;
    for _ = 1 to !depth do
      read_frame ic buf
    done;
    count := !count + !depth
  done;
  Unix.close socket

let domain_run share batch () =
  let counts = List.init share (fun _ -> ref 0) in
  let threads =
    List.map (fun count -> Thread.create (connection batch count) ()) counts
  in
  List.iter Thread.join threads;
  List.fold_left (fun acc c -> acc + !c) 0 counts

let () =
  Arg.parse
    [ "--port", Arg.Set_int port, " Port on 127.0.0.1 (8080)"
    ; "--path", Arg.Set_string path, " Target (/ws)"
    ; "--connections", Arg.Set_int connections, " Connections (64)"
    ; "--domains", Arg.Set_int domains, " Domains (4)"
    ; "--size", Arg.Set_int size, " Message bytes (64)"
    ; "--depth", Arg.Set_int depth, " Messages in flight per connection (1)"
    ; "--duration", Arg.Set_float duration, " Seconds (10)"
    ; "--text", Arg.Set text, " Text messages instead of binary"
    ]
    (fun arg -> raise (Arg.Bad arg))
    "WebSocket echo load generator for cost.sh.";
  Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
  let one = frame () in
  let batch = String.concat "" (List.init !depth (fun _ -> one)) in
  let start = Unix.gettimeofday () in
  let workers =
    List.init !domains (fun i ->
      let share =
        (!connections / !domains)
        + if i < !connections mod !domains then 1 else 0
      in
      Domain.spawn (domain_run share batch))
  in
  Unix.sleepf !duration;
  Atomic.set stop true;
  let total = List.fold_left (fun acc d -> acc + Domain.join d) 0 workers in
  let elapsed = Unix.gettimeofday () -. start in
  Printf.printf
    "%d messages in %.2fs\nRequests/sec: %.2f\n"
    total
    elapsed
    (float total /. elapsed)
