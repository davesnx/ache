let sha1 s =
  let mask = 0xffffffff in
  let rol x n = (x lsl n) lor (x lsr (32 - n)) land mask in
  let len = String.length s in
  let padded_len = (len + 9 + 63) / 64 * 64 in
  let msg = Bytes.make padded_len '\000' in
  Bytes.blit_string s 0 msg 0 len;
  Bytes.set msg len '\x80';
  Bytes.set_int64_be msg (padded_len - 8) (Int64.mul (Int64.of_int len) 8L);
  let h = [| 0x67452301; 0xEFCDAB89; 0x98BADCFE; 0x10325476; 0xC3D2E1F0 |] in
  let w = Array.make 80 0 in
  for chunk = 0 to (padded_len / 64) - 1 do
    for i = 0 to 15 do
      w.(i) <-
        Int32.to_int (Bytes.get_int32_be msg ((chunk * 64) + (i * 4))) land mask
    done;
    for i = 16 to 79 do
      w.(i) <- rol (w.(i - 3) lxor w.(i - 8) lxor w.(i - 14) lxor w.(i - 16)) 1
    done;
    let a = ref h.(0)
    and b = ref h.(1)
    and c = ref h.(2)
    and d = ref h.(3)
    and e = ref h.(4) in
    for i = 0 to 79 do
      let f, k =
        if i < 20
        then !b land !c lor (lnot !b land !d), 0x5A827999
        else if i < 40
        then !b lxor !c lxor !d, 0x6ED9EBA1
        else if i < 60
        then !b land !c lor (!b land !d) lor (!c land !d), 0x8F1BBCDC
        else !b lxor !c lxor !d, 0xCA62C1D6
      in
      let temp = (rol !a 5 + f + !e + k + w.(i)) land mask in
      e := !d;
      d := !c;
      c := rol !b 30;
      b := !a;
      a := temp
    done;
    h.(0) <- (h.(0) + !a) land mask;
    h.(1) <- (h.(1) + !b) land mask;
    h.(2) <- (h.(2) + !c) land mask;
    h.(3) <- (h.(3) + !d) land mask;
    h.(4) <- (h.(4) + !e) land mask
  done;
  let out = Bytes.create 20 in
  Array.iteri (fun i v -> Bytes.set_int32_be out (i * 4) (Int32.of_int v)) h;
  Bytes.unsafe_to_string out

let base64_alphabet =
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"

let base64 s =
  let len = String.length s in
  let out = Stdlib.Buffer.create ((len + 2) / 3 * 4) in
  let i = ref 0 in
  while !i < len do
    let byte n = if !i + n < len then Char.code s.[!i + n] else 0 in
    let v = (byte 0 lsl 16) lor (byte 1 lsl 8) lor byte 2 in
    let rem = len - !i in
    let add shift =
      Stdlib.Buffer.add_char out base64_alphabet.[(v lsr shift) land 63]
    in
    add 18;
    add 12;
    if rem > 1 then add 6 else Stdlib.Buffer.add_char out '=';
    if rem > 2 then add 0 else Stdlib.Buffer.add_char out '=';
    i := !i + 3
  done;
  Stdlib.Buffer.contents out

let accept_key key =
  base64 (sha1 (key ^ "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"))

type handshake_error =
  [ `Not_get
  | `Not_websocket
  | `Unsupported_version
  | `Invalid_key
  ]

let tokens headers name =
  List.concat_map
    (fun value -> List.map String.trim (String.split_on_char ',' value))
    (Headers.get_multi headers name)

let has_token headers name token =
  List.exists (Headers.ci_equal token) (tokens headers name)

(* RFC 6455 §4.1: the key is the base64 encoding of 16 bytes. The last character
   before the padding carries 2 bits of data and 4 padding bits, which RFC 4648
   §3.5 requires to be zero: one of A, Q, g, w. *)
let valid_key key =
  String.length key = 24
  && key.[22] = '='
  && key.[23] = '='
  && String.contains "AQgw" key.[21]
  &&
  let ok = ref true in
  for i = 0 to 21 do
    if not (String.contains base64_alphabet key.[i]) then ok := false
  done;
  !ok

(* RFC 6455 §4.2.1 *)
let check_handshake (request : Request.t) =
  let headers = request.headers in
  if
    request.meth <> `GET
    || Version.compare request.version Version.{ major = 1; minor = 1 } < 0
  then Error `Not_get
  else if
    not
      (has_token headers "upgrade" "websocket"
      && has_token headers "connection" "upgrade")
  then Error `Not_websocket
  else
    match Headers.get_multi headers "sec-websocket-version" with
    | [ version ] when String.trim version = "13" ->
      (match Headers.get_multi headers "sec-websocket-key" with
      | [ key ] when valid_key (String.trim key) ->
        Ok (accept_key (String.trim key))
      | _ -> Error `Invalid_key)
    | _ -> Error `Unsupported_version

type message =
  | Text of string
  | Binary of string

type close =
  { code : int
  ; reason : string
  }

type handler =
  { on_message : message -> unit
  ; on_pong : string -> unit
  ; on_close : close option -> unit
  }

type reading =
  | Header
  | Payload
  | Stopped

type writing =
  | Open
  | Closing
  | Closed

type t =
  { max_message_size : int
  ; mutable handler : handler
  ; mutable reading : reading
  ; mutable paused : bool
  ; mutable writing : writing
  ; mutable peer_close : close option
  ; mutable closed_reported : bool
  ; mutable opcode : int
  ; mutable fin : bool
  ; mutable remaining : int
  ; mask : Bytes.t
  ; mutable mask_pos : int
  ; mutable fragmented : bool
  ; mutable text : bool
  ; mutable utf8_need : int
  ; mutable utf8_lo : int
  ; mutable utf8_hi : int
  ; message : Stdlib.Buffer.t
  ; control : Stdlib.Buffer.t
  ; pending : Stdlib.Buffer.t
  ; mutable inflight : Bigstring.t
  ; mutable inflight_off : int
  ; mutable queued : int
  ; mutable written : int
  ; flushes : (int * ([ `Written | `Closed ] -> unit)) Queue.t
  ; mutable reader_k : (unit -> unit) option
  ; mutable writer_k : (unit -> unit) option
  }

let wake_reader t =
  let k = t.reader_k in
  t.reader_k <- None;
  Option.iter (fun k -> k ()) k

let wake_writer t =
  let k = t.writer_k in
  t.writer_k <- None;
  Option.iter (fun k -> k ()) k

let frame t ~opcode payload =
  let len = String.length payload in
  let before = Stdlib.Buffer.length t.pending in
  Stdlib.Buffer.add_char t.pending (Char.chr (0x80 lor opcode));
  if len < 126
  then Stdlib.Buffer.add_char t.pending (Char.chr len)
  else if len < 0x10000
  then (
    Stdlib.Buffer.add_char t.pending '\126';
    Stdlib.Buffer.add_uint16_be t.pending len)
  else (
    Stdlib.Buffer.add_char t.pending '\127';
    Stdlib.Buffer.add_int64_be t.pending (Int64.of_int len));
  Stdlib.Buffer.add_string t.pending payload;
  t.queued <- t.queued + Stdlib.Buffer.length t.pending - before;
  wake_writer t

let close_payload ?(reason = "") code =
  let b = Bytes.create (2 + String.length reason) in
  Bytes.set_uint16_be b 0 code;
  Bytes.blit_string reason 0 b 2 (String.length reason);
  Bytes.unsafe_to_string b

let stop_reading t =
  t.reading <- Stopped;
  wake_reader t;
  wake_writer t

let send_close t payload =
  if t.writing = Open
  then (
    frame t ~opcode:0x8 payload;
    t.writing <- Closing;
    wake_writer t)

let fail t code =
  stop_reading t;
  send_close t (close_payload code)

let finish t =
  t.writing <- Closed;
  Stdlib.Buffer.clear t.pending;
  t.inflight <- Bigstring.empty;
  t.inflight_off <- 0;
  stop_reading t;
  Queue.iter (fun (_, k) -> k `Closed) t.flushes;
  Queue.clear t.flushes;
  wake_writer t;
  if not t.closed_reported
  then (
    t.closed_reported <- true;
    t.handler.on_close t.peer_close)

(* RFC 6455 §7.4.1, §7.4.2: codes a peer may send in a close frame. *)
let valid_close_code code =
  (code >= 1000 && code <= 1003)
  || (code >= 1007 && code <= 1014)
  || (code >= 3000 && code <= 4999)

(* [peer_close] is set before [stop_reading]: that wakes the writer, which
   finishes the connection when our close frame is already written. *)
let on_close_frame t payload =
  match String.length payload with
  | 0 ->
    t.peer_close <- Some { code = 1005; reason = "" };
    stop_reading t;
    send_close t ""
  | 1 -> fail t 1002
  | n ->
    let code = String.get_uint16_be payload 0 in
    let reason = String.sub payload 2 (n - 2) in
    if not (valid_close_code code)
    then fail t 1002
    else if not (String.is_valid_utf_8 reason)
    then fail t 1007
    else (
      t.peer_close <- Some { code; reason };
      stop_reading t;
      send_close t (close_payload code))

(* RFC 3629 §4: the next byte of a text message, given the bytes before it.
   [false] when the message cannot be valid UTF-8 any more. *)
let utf8_byte t c =
  if t.utf8_need > 0
  then
    if c < t.utf8_lo || c > t.utf8_hi
    then false
    else (
      t.utf8_need <- t.utf8_need - 1;
      t.utf8_lo <- 0x80;
      t.utf8_hi <- 0xbf;
      true)
  else if c < 0x80
  then true
  else if c >= 0xc2 && c <= 0xdf
  then (
    t.utf8_need <- 1;
    true)
  else if c = 0xe0
  then (
    t.utf8_need <- 2;
    t.utf8_lo <- 0xa0;
    true)
  else if (c >= 0xe1 && c <= 0xec) || c = 0xee || c = 0xef
  then (
    t.utf8_need <- 2;
    true)
  else if c = 0xed
  then (
    t.utf8_need <- 2;
    t.utf8_hi <- 0x9f;
    true)
  else if c = 0xf0
  then (
    t.utf8_need <- 3;
    t.utf8_lo <- 0x90;
    true)
  else if c >= 0xf1 && c <= 0xf3
  then (
    t.utf8_need <- 3;
    true)
  else if c = 0xf4
  then (
    t.utf8_need <- 3;
    t.utf8_hi <- 0x8f;
    true)
  else false

let start_message t opcode =
  t.text <- opcode = 0x1;
  t.utf8_need <- 0;
  t.utf8_lo <- 0x80;
  t.utf8_hi <- 0xbf

let end_frame t =
  match t.opcode with
  | 0x8 -> on_close_frame t (Stdlib.Buffer.contents t.control)
  | 0x9 ->
    if t.writing = Open
    then frame t ~opcode:0xA (Stdlib.Buffer.contents t.control);
    t.reading <- Header
  | 0xA ->
    t.reading <- Header;
    t.handler.on_pong (Stdlib.Buffer.contents t.control)
  | _ ->
    t.reading <- Header;
    if not t.fin
    then t.fragmented <- true
    else if t.text && t.utf8_need > 0
    then fail t 1007
    else
      let message = Stdlib.Buffer.contents t.message in
      Stdlib.Buffer.clear t.message;
      t.fragmented <- false;
      (* After our close frame, messages are dropped while we wait for the
         peer's (RFC 6455 §1.4). *)
      if t.writing = Open
      then
        t.handler.on_message (if t.text then Text message else Binary message)

let read_header t bs ~off ~len =
  if len < 2
  then 0
  else
    let b0 = Char.code (Bigstring.unsafe_get bs off)
    and b1 = Char.code (Bigstring.unsafe_get bs (off + 1)) in
    let len7 = b1 land 0x7f in
    let ext = if len7 = 126 then 2 else if len7 = 127 then 8 else 0 in
    let size = 2 + ext + 4 in
    let opcode = b0 land 0x0f in
    let fin = b0 land 0x80 <> 0 in
    let control = opcode >= 0x8 in
    if
      b0 land 0x70 <> 0
      || (not (List.mem opcode [ 0x0; 0x1; 0x2; 0x8; 0x9; 0xA ]))
      || b1 land 0x80 = 0
      || (control && ((not fin) || len7 > 125))
      || (opcode = 0x0 && not t.fragmented)
      || ((opcode = 0x1 || opcode = 0x2) && t.fragmented)
    then (
      fail t 1002;
      len)
    else if len < size
    then 0
    else
      let payload_len =
        if ext = 0
        then Int64.of_int len7
        else
          let v = ref 0L in
          for i = 0 to ext - 1 do
            v :=
              Int64.logor
                (Int64.shift_left !v 8)
                (Int64.of_int
                   (Char.code (Bigstring.unsafe_get bs (off + 2 + i))))
          done;
          !v
      in
      (* RFC 6455 §5.2: a payload length that has the high bit set, or that is
         not in its shortest form. *)
      if
        Int64.compare payload_len 0L < 0
        || (ext = 2 && Int64.compare payload_len 126L < 0)
        || (ext = 8 && Int64.compare payload_len 0x10000L < 0)
      then (
        fail t 1002;
        len)
      else if
        (not control)
        && Int64.compare
             payload_len
             (Int64.of_int
                (t.max_message_size - Stdlib.Buffer.length t.message))
           > 0
      then (
        fail t 1009;
        len)
      else (
        for i = 0 to 3 do
          Bytes.set t.mask i (Bigstring.unsafe_get bs (off + 2 + ext + i))
        done;
        if opcode = 0x1 || opcode = 0x2 then start_message t opcode;
        t.opcode <- opcode;
        t.fin <- fin;
        t.remaining <- Int64.to_int payload_len;
        t.mask_pos <- 0;
        Stdlib.Buffer.clear t.control;
        t.reading <- Payload;
        if t.remaining = 0 then end_frame t;
        size)

let read_payload t bs ~off ~len =
  let n = min len t.remaining in
  let control = t.opcode >= 0x8 in
  let dst = if control then t.control else t.message in
  let check = (not control) && t.text in
  let i = ref 0 in
  let valid = ref true in
  while !valid && !i < n do
    let c =
      Char.code (Bigstring.unsafe_get bs (off + !i))
      lxor Char.code (Bytes.unsafe_get t.mask ((t.mask_pos + !i) land 3))
    in
    if check && (c >= 0x80 || t.utf8_need > 0) && not (utf8_byte t c)
    then valid := false
    else (
      Stdlib.Buffer.add_char dst (Char.unsafe_chr c);
      incr i)
  done;
  if not !valid
  then (
    (* Invalid text fails at once, without reading the rest of the message (RFC
       6455 §8.1). *)
    fail t 1007;
    len)
  else (
    t.mask_pos <- (t.mask_pos + n) land 3;
    t.remaining <- t.remaining - n;
    if t.remaining = 0 then end_frame t;
    n)

let read t bs ~off ~len =
  let rec go consumed =
    let off' = off + consumed
    and len' = len - consumed in
    match t.reading with
    | Stopped -> len
    | Header ->
      let n = read_header t bs ~off:off' ~len:len' in
      if n = 0 then consumed else go (consumed + n)
    | Payload ->
      if len' = 0
      then consumed
      else go (consumed + read_payload t bs ~off:off' ~len:len')
  in
  go 0

let read_eof t bs ~off ~len =
  let n = read t bs ~off ~len in
  stop_reading t;
  if t.writing = Open then t.writing <- Closing;
  wake_writer t;
  n

let max_unwritten_output = 65536
let read_blocked t = t.paused || t.queued - t.written > max_unwritten_output
let wake_reader_unless_blocked t = if not (read_blocked t) then wake_reader t

let next_read_operation t =
  match t.reading with
  | Stopped -> `Close
  | (Header | Payload) when read_blocked t -> `Yield
  | Header | Payload -> `Read

let read_timeout t =
  match t.reading, t.writing with
  | (Header | Payload), Open when not (read_blocked t) ->
    Some (`Idle, fun () -> fail t 1001)
  | (Header | Payload), Closing ->
    (* RFC 6455 §7.1.1: the client did not answer our close frame in time. *)
    Some (`Close, fun () -> finish t)
  | (Header | Payload), (Open | Closed) | Stopped, _ -> None

let yield_reader t k =
  if t.reading = Stopped then k () else t.reader_k <- Some k

let next_write_operation t =
  if
    t.writing <> Closed
    && t.inflight_off = Bigstring.length t.inflight
    && Stdlib.Buffer.length t.pending > 0
  then (
    let s = Stdlib.Buffer.contents t.pending in
    Stdlib.Buffer.clear t.pending;
    t.inflight <- Bigstring.of_string s ~off:0 ~len:(String.length s);
    t.inflight_off <- 0);
  let len = Bigstring.length t.inflight - t.inflight_off in
  match t.writing with
  | Closed -> `Close 0
  | (Open | Closing) when len > 0 ->
    `Write [ { IOVec.buffer = t.inflight; off = t.inflight_off; len } ]
  | Open -> `Yield
  | Closing when t.reading <> Stopped ->
    (* Our close frame is written: wait for the peer's (RFC 6455 §7.1.1). *)
    `Yield
  | Closing ->
    finish t;
    `Close 0

let report_write_result t = function
  | `Closed -> finish t
  | `Ok n ->
    t.inflight_off <- t.inflight_off + n;
    t.written <- t.written + n;
    let rec fire () =
      match Queue.peek_opt t.flushes with
      | Some (target, k) when target <= t.written ->
        ignore (Queue.pop t.flushes);
        k `Written;
        fire ()
      | _ -> ()
    in
    fire ();
    wake_reader_unless_blocked t

let yield_writer t k = if t.writing = Closed then k () else t.writer_k <- Some k
let report_exn t _ = finish t
let is_closed t = t.writing = Closed
let pause_reading t = t.paused <- true

let resume_reading t =
  t.paused <- false;
  wake_reader_unless_blocked t

let shutdown t = finish t
let send_text t s = if t.writing = Open then frame t ~opcode:0x1 s

let flush t k =
  if t.writing <> Open
  then k `Closed
  else if t.queued <= t.written
  then k `Written
  else Queue.push (t.queued, k) t.flushes

let send_binary t s = if t.writing = Open then frame t ~opcode:0x2 s

let ping t payload =
  if String.length payload > 125
  then invalid_arg "Ache.Websocket.ping: payload longer than 125 bytes";
  if t.writing = Open then frame t ~opcode:0x9 payload

let close ?(reason = "") t ~code =
  if not (valid_close_code code)
  then invalid_arg "Ache.Websocket.close: invalid close code";
  if String.length reason > 123
  then invalid_arg "Ache.Websocket.close: reason longer than 123 bytes";
  if not (String.is_valid_utf_8 reason)
  then invalid_arg "Ache.Websocket.close: reason is not valid UTF-8";
  if t.writing = Open
  then (
    (* Keep reading until the peer's close frame or the end of input; the
       messages before it are dropped, so the queue no longer pauses. *)
    send_close t (close_payload ~reason code);
    t.paused <- false;
    wake_reader_unless_blocked t)

let default_max_message_size = 1 lsl 20

let requested_protocols (request : Request.t) =
  List.filter
    (fun member -> member <> "")
    (Message.list_members
       (Headers.get_multi request.headers "sec-websocket-protocol"))

let handshake_fields = [ "upgrade"; "connection"; "sec-websocket-accept" ]

let check_extra_headers headers =
  Headers.iter headers ~f:(fun name _ ->
    if List.exists (Headers.ci_equal name) handshake_fields
    then
      invalid_arg
        (Printf.sprintf
           "Ache.Websocket.upgrade: the handshake owns the field %S"
           name));
  Serialize.check_headers headers

let upgrade
      ?(max_message_size = default_max_message_size)
      ?(headers = Headers.empty)
      reqd
      make_handler
  =
  if max_message_size < 1
  then invalid_arg "Ache.Websocket.upgrade: max_message_size must be positive";
  check_extra_headers headers;
  match check_handshake (Request_descriptor.request reqd) with
  | Error _ as error -> error
  | Ok accept ->
    let t =
      { max_message_size
      ; handler = { on_message = ignore; on_pong = ignore; on_close = ignore }
      ; reading = Header
      ; paused = false
      ; writing = Open
      ; peer_close = None
      ; closed_reported = false
      ; opcode = 0
      ; fin = false
      ; remaining = 0
      ; mask = Bytes.create 4
      ; mask_pos = 0
      ; fragmented = false
      ; text = false
      ; utf8_need = 0
      ; utf8_lo = 0x80
      ; utf8_hi = 0xbf
      ; message = Stdlib.Buffer.create 256
      ; control = Stdlib.Buffer.create 128
      ; pending = Stdlib.Buffer.create 256
      ; inflight = Bigstring.empty
      ; inflight_off = 0
      ; queued = 0
      ; written = 0
      ; flushes = Queue.create ()
      ; reader_k = None
      ; writer_k = None
      }
    in
    t.handler <- make_handler t;
    let module Engine = struct
      type nonrec t = t

      let next_read_operation = next_read_operation
      let read = read
      let read_eof = read_eof
      let yield_reader = yield_reader
      let next_write_operation = next_write_operation
      let report_write_result = report_write_result
      let yield_writer = yield_writer
      let report_exn = report_exn
      let is_closed = is_closed
      let shutdown = shutdown
    end
    in
    Request_descriptor.respond_with_upgrade
      reqd
      (Headers.of_list
         ([ "upgrade", "websocket"
          ; "connection", "Upgrade"
          ; "sec-websocket-accept", accept
          ]
         @ Headers.to_list headers))
      (Runtime.make_with_hooks
         (module Engine)
         t
         { drain = (fun t -> fail t 1001); read_timeout });
    Ok ()
