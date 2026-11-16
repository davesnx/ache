(*----------------------------------------------------------------------------
  Copyright (c) 2018 Inhabited Type LLC.

  All rights reserved.

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions are met:

  1. Redistributions of source code must retain the above copyright notice, this
  list of conditions and the following disclaimer.

  2. Redistributions in binary form must reproduce the above copyright notice,
  this list of conditions and the following disclaimer in the documentation
  and/or other materials provided with the distribution.

  3. Neither the name of the author nor the names of his contributors may be
  used to endorse or promote products derived from this software without
  specific prior written permission.

  THIS SOFTWARE IS PROVIDED BY THE CONTRIBUTORS ``AS IS'' AND ANY EXPRESS OR
  IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
  MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO
  EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
  INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
  LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
  PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
  LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
  NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,
  EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
  ----------------------------------------------------------------------------*)

module Reader = struct
  type t =
    { mutable buffer : Bigstring.t
    ; mutable off : int
    ; mutable len : int
    ; mutable closed : bool
    ; mutable read_scheduled : bool
    ; mutable on_eof : unit -> unit
    ; mutable on_error : unit -> unit
    ; mutable failed : bool
    ; mutable discarding : bool
    ; mutable eof_has_been_called : bool
    ; mutable on_read : Bigstring.t -> off:int -> len:int -> unit
    ; when_ready_to_read : Optional_thunk.t
    }

  let default_on_eof = Sys.opaque_identity (fun () -> ())
  let default_on_error = Sys.opaque_identity (fun () -> ())
  let default_on_read = Sys.opaque_identity (fun _ ~off:_ ~len:_ -> ())

  let create ~when_ready_to_read =
    { buffer = Bigstring.empty
    ; off = 0
    ; len = 0
    ; closed = false
    ; read_scheduled = false
    ; eof_has_been_called = false
    ; on_eof = default_on_eof
    ; on_error = default_on_error
    ; failed = false
    ; discarding = false
    ; on_read = default_on_read
    ; when_ready_to_read
    }

  let create_empty () =
    let t = create ~when_ready_to_read:Optional_thunk.none in
    t.closed <- true;
    t

  let is_closed t = t.closed
  let ready_to_read t = Optional_thunk.call_if_some t.when_ready_to_read

  (* The application may keep bytes passed to [on_read]: never reuse them. *)
  let write t bs ~off ~len =
    if (not t.closed) && (not t.discarding) && len > 0
    then (
      if Bigstring.length t.buffer - (t.off + t.len) < len
      then (
        let buffer = Bigstring.create (Int.max 0x1000 ((2 * t.len) + len)) in
        Bigstring.blit t.buffer ~src_off:t.off buffer ~dst_off:0 ~len:t.len;
        t.buffer <- buffer;
        t.off <- 0);
      Bigstring.blit bs ~src_off:off t.buffer ~dst_off:(t.off + t.len) ~len;
      t.len <- t.len + len)

  let reset_callbacks t =
    t.read_scheduled <- false;
    if t.on_eof != default_on_eof then t.on_eof <- default_on_eof;
    if t.on_error != default_on_error then t.on_error <- default_on_error;
    if t.on_read != default_on_read then t.on_read <- default_on_read

  let rec do_execute_read t ~on_error on_eof on_read =
    if t.len > 0
    then (
      reset_callbacks t;
      let off = t.off
      and len = t.len in
      t.off <- off + len;
      t.len <- 0;
      on_read t.buffer ~off ~len;
      execute_read t)
    else if t.closed
    then (
      reset_callbacks t;
      if not t.eof_has_been_called
      then (
        t.eof_has_been_called <- true;
        if t.failed then on_error () else on_eof ()))

  and execute_read t =
    if t.read_scheduled
    then do_execute_read t ~on_error:t.on_error t.on_eof t.on_read

  let schedule_read t ~on_eof ~on_error ~on_read =
    if t.read_scheduled
    then failwith "Body.schedule_read: reader already scheduled";
    if not (is_closed t)
    then (
      t.read_scheduled <- true;
      t.on_eof <- on_eof;
      t.on_error <- on_error;
      t.on_read <- on_read);
    do_execute_read t ~on_error on_eof on_read;
    ready_to_read t

  let close t =
    t.closed <- true;
    execute_read t;
    ready_to_read t

  let discard t =
    t.discarding <- true;
    t.buffer <- Bigstring.empty;
    t.off <- 0;
    t.len <- 0

  let fail t =
    if not t.closed
    then (
      t.failed <- true;
      close t)

  let has_pending_output t = t.len > 0
  let is_read_scheduled t = t.read_scheduled
end

module Writer = struct
  type encoding =
    | Identity
    | Fixed of { mutable remaining : Int64.t }
    | Chunked of { mutable written_final_chunk : bool }
    | Discard

  type t =
    { encoder : Encoder.t
    ; encoding : encoding
    ; writer : Serialize.Writer.t
    ; mutable buffered_bytes : int
    }

  let of_encoder encoder ~encoding ~writer =
    let encoding =
      match encoding with
      | `Fixed remaining -> Fixed { remaining }
      | `Close_delimited -> Identity
      | `Chunked -> Chunked { written_final_chunk = false }
      | `No_body -> Discard
    in
    { encoder; encoding; writer; buffered_bytes = 0 }

  let create buffer ~encoding =
    of_encoder (Encoder.of_bigstring buffer) ~encoding

  let create_empty ~writer =
    let t = create Bigstring.empty ~encoding:(`Fixed 0L) ~writer in
    Encoder.close t.encoder;
    t

  let reserve t len =
    match t.encoding with
    | Fixed fixed ->
      let len = Int64.of_int len in
      if Int64.compare len fixed.remaining > 0
      then
        invalid_arg
          (Printf.sprintf
             "ache.Body.Writer: %Ld bytes written, %Ld left of the \
              content-length"
             len
             fixed.remaining);
      fixed.remaining <- Int64.sub fixed.remaining len
    | Identity | Chunked _ | Discard -> ()

  let length ?(off = 0) ?len total =
    match len with Some len -> len | None -> total - off

  let write_char t c =
    if not (Encoder.is_closed t.encoder)
    then (
      reserve t 1;
      Encoder.write_char t.encoder c)

  let write_string t ?off ?len s =
    if not (Encoder.is_closed t.encoder)
    then (
      reserve t (length ?off ?len (String.length s));
      Encoder.write_string ?off ?len t.encoder s)

  let write_bigstring t ?off ?len b =
    if not (Encoder.is_closed t.encoder)
    then (
      reserve t (length ?off ?len (Bigstring.length b));
      Encoder.write_bigstring ?off ?len t.encoder b)

  let schedule_bigstring t ?off ?len (b : Bigstring.t) =
    if not (Encoder.is_closed t.encoder)
    then (
      reserve t (length ?off ?len (Bigstring.length b));
      Encoder.schedule_bigstring ?off ?len t.encoder b)

  let ready_to_write t = Serialize.Writer.wakeup t.writer

  let flush t kontinue =
    if Serialize.Writer.is_closed t.writer
    then kontinue `Closed
    else (
      Encoder.flush_with_reason t.encoder (function
        | Drain -> kontinue `Closed
        | Nothing_pending | Shift -> Serialize.Writer.flush t.writer kontinue);
      ready_to_write t)

  let is_closed t = Encoder.is_closed t.encoder

  let is_incomplete t =
    is_closed t
    &&
    match t.encoding with
    | Fixed { remaining } -> Int64.compare remaining 0L > 0
    | Identity | Chunked _ | Discard -> false

  let close_and_drain t =
    Encoder.close t.encoder;
    (* Resolve all pending flushes *)
    ignore (Encoder.drain t.encoder : int)

  let close t =
    Serialize.Writer.unyield t.writer;
    Encoder.close t.encoder;
    ready_to_write t

  let force_close t =
    (match t.encoding with
    | Chunked t -> t.written_final_chunk <- true
    | Identity | Fixed _ | Discard -> ());
    close t

  let has_pending_output t =
    (* Force another write poll to make sure that the final chunk is emitted for
       chunk-encoded bodies. *)
    let encoder_has_output = Encoder.has_pending_output t.encoder in
    let additional_encoding_output =
      match t.encoding with
      | Identity | Fixed _ | Discard -> false
      | Chunked { written_final_chunk } ->
        Encoder.is_closed t.encoder && not written_final_chunk
    in
    encoder_has_output || additional_encoding_output

  let requires_output t = (not (is_closed t)) || has_pending_output t

  let transfer_to_writer t =
    let encoder = t.encoder in
    if Serialize.Writer.is_closed t.writer
    then close_and_drain t
    else
      match Encoder.operation encoder with
      | `Yield -> ()
      | `Close ->
        (match t.encoding with
        | Identity | Fixed _ | Discard -> ()
        | Chunked ({ written_final_chunk } as chunked) ->
          if not written_final_chunk
          then (
            chunked.written_final_chunk <- true;
            Serialize.Writer.schedule_chunk t.writer []))
      | `Writev iovecs when t.encoding = Discard ->
        Encoder.shift encoder (IOVec.lengthv iovecs)
      | `Writev iovecs ->
        (match IOVec.shiftv iovecs t.buffered_bytes with
        | [] -> ()
        | iovecs ->
          let lengthv = IOVec.lengthv iovecs in
          t.buffered_bytes <- t.buffered_bytes + lengthv;
          (match t.encoding with
          | Identity | Fixed _ | Discard ->
            Serialize.Writer.schedule_fixed t.writer iovecs
          | Chunked _ -> Serialize.Writer.schedule_chunk t.writer iovecs);
          Serialize.Writer.flush t.writer (function
            | `Closed -> close_and_drain t
            | `Written ->
              Encoder.shift encoder lengthv;
              t.buffered_bytes <- t.buffered_bytes - lengthv))
end
