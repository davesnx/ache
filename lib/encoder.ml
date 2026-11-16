(*----------------------------------------------------------------------------
  Copyright (c) 2016 Inhabited Type LLC.

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

type bigstring = Bigstring.t

type 'a iovec =
  { buffer : 'a
  ; off : int
  ; len : int
  }

exception Dequeue_empty

module Deque (T : sig
    type t

    val sentinel : t
  end) : sig
  type elem = T.t
  type t

  val create : int -> t
  val is_empty : t -> bool
  val enqueue : elem -> t -> unit
  val dequeue_exn : t -> elem
  val enqueue_front : elem -> t -> unit
  val map_to_list : t -> f:(elem -> 'b) -> 'b list
end = struct
  type elem = T.t

  type t =
    { mutable elements : elem array
    ; mutable front : int
    ; mutable back : int
    }

  let sentinel = T.sentinel
  let create size = { elements = Array.make size sentinel; front = 0; back = 0 }
  let is_empty t = t.front = t.back

  let ensure_space t =
    if t.back = Array.length t.elements - 1
    then begin
      let len = t.back - t.front in
      if t.front > 0
      then begin
        (* Shift everything to the front of the array and then clear out
         * dangling pointers to elements from their previous locations. *)
        Array.blit t.elements t.front t.elements 0 len;
        Array.fill t.elements len t.front sentinel
      end
      else begin
        let old = t.elements in
        let new_ = Array.(make (2 * length old) sentinel) in
        Array.blit old t.front new_ 0 len;
        t.elements <- new_
      end;
      t.front <- 0;
      t.back <- len
    end

  let enqueue e t =
    ensure_space t;
    t.elements.(t.back) <- e;
    t.back <- t.back + 1

  let dequeue_exn t =
    if is_empty t
    then raise_notrace Dequeue_empty
    else
      let result = Array.unsafe_get t.elements t.front in
      Array.unsafe_set t.elements t.front sentinel;
      t.front <- t.front + 1;
      result

  let enqueue_front e t =
    (* This is in general not true for Deque data structures, but the usage
     * below ensures that there is always space to push an element back on the
     * front. An [enqueue_front] is always preceded by a [dequeue], with no
     * intervening operations. *)
    assert (t.front > 0);
    t.front <- t.front - 1;
    t.elements.(t.front) <- e

  let map_to_list t ~f =
    let result = ref [] in
    for i = t.back - 1 downto t.front do
      result := f t.elements.(i) :: !result
    done;
    !result
end

module IOVec = struct
  let create buffer ~off ~len = { buffer; off; len }
  let length t = t.len

  let shift { buffer; off; len } n =
    assert (n < len);
    { buffer; off = off + n; len = len - n }

  let lengthv ts =
    let rec loop ts acc =
      match ts with [] -> acc | iovec :: ts -> loop ts (length iovec + acc)
    in
    loop ts 0
end

module Flushed_reason = struct
  type t =
    | Shift
    | Drain
    | Nothing_pending
end

module Buffers = Deque (struct
    type t = bigstring iovec

    let sentinel =
      let deadbeef = "\222\173\190\239" in
      let len = String.length deadbeef in
      let buffer = Bigstring.create len in
      String.iteri (Bigstring.unsafe_set buffer) deadbeef;
      { buffer; off = 0; len }
  end)

module Flushes = Deque (struct
    type t = int * (Flushed_reason.t -> unit)

    let sentinel = 0, fun _ -> ()
  end)

type t =
  { mutable buffer : bigstring
  ; mutable scheduled_pos : int
  ; mutable write_pos : int
  ; scheduled : Buffers.t
  ; flushed : Flushes.t
  ; mutable bytes_received : int
  ; mutable bytes_written : int
  ; mutable closed : bool
  ; mutable yield : bool
  }

type operation =
  [ `Writev of bigstring iovec list
  | `Yield
  | `Close
  ]

let of_bigstring buffer =
  { buffer
  ; write_pos = 0
  ; scheduled_pos = 0
  ; scheduled = Buffers.create 4
  ; flushed = Flushes.create 1
  ; bytes_received = 0
  ; bytes_written = 0
  ; closed = false
  ; yield = false
  }

let create size = of_bigstring (Bigstring.create size)
let writable_exn t = if t.closed then failwith "cannot write to closed writer"

let schedule_iovec t ?(off = 0) ~len buffer =
  t.bytes_received <- t.bytes_received + len;
  Buffers.enqueue (IOVec.create buffer ~off ~len) t.scheduled

let flush_buffer t =
  let len = t.write_pos - t.scheduled_pos in
  if len > 0
  then begin
    let off = t.scheduled_pos in
    schedule_iovec t ~off ~len t.buffer;
    t.scheduled_pos <- t.write_pos
  end

let flush_with_reason t f =
  t.yield <- false;
  flush_buffer t;
  if Buffers.is_empty t.scheduled
  then f Flushed_reason.Nothing_pending
  else Flushes.enqueue (t.bytes_received, f) t.flushed

let free_bytes_in_buffer t =
  let buf_len = Bigstring.length t.buffer in
  buf_len - t.write_pos

let schedule_bigstring t ?(off = 0) ?len a =
  writable_exn t;
  flush_buffer t;
  let len =
    match len with None -> Bigstring.length a - off | Some len -> len
  in
  if len > 0 then schedule_iovec t ~off ~len a

let ensure_space t len =
  if free_bytes_in_buffer t < len
  then begin
    flush_buffer t;
    t.buffer <- Bigstring.create (Int.max (Bigstring.length t.buffer) len);
    t.write_pos <- 0;
    t.scheduled_pos <- 0
  end

let write_string t ?(off = 0) ?len a =
  writable_exn t;
  let len = match len with None -> String.length a - off | Some len -> len in
  ensure_space t len;
  Bigstring.unsafe_blit_from_string
    a
    ~src_off:off
    t.buffer
    ~dst_off:t.write_pos
    ~len;
  t.write_pos <- t.write_pos + len

let write_bigstring t ?(off = 0) ?len a =
  writable_exn t;
  let len =
    match len with None -> Bigstring.length a - off | Some len -> len
  in
  ensure_space t len;
  Bigstring.unsafe_blit a ~src_off:off t.buffer ~dst_off:t.write_pos ~len;
  t.write_pos <- t.write_pos + len

let write_char t c =
  writable_exn t;
  ensure_space t 1;
  Bigstring.unsafe_set t.buffer t.write_pos c;
  t.write_pos <- t.write_pos + 1

module LE = struct
  external bswap16 : int -> int = "%bswap16"
  external bswap_int64 : int64 -> int64 = "%bswap_int64"

  let write_uint16 t i =
    writable_exn t;
    ensure_space t 2;
    if Sys.big_endian
    then Bigstring.unsafe_set_int16_ne t.buffer t.write_pos (bswap16 i)
    else Bigstring.unsafe_set_int16_ne t.buffer t.write_pos i;
    t.write_pos <- t.write_pos + 2

  let write_uint64 t i =
    writable_exn t;
    ensure_space t 8;
    if Sys.big_endian
    then Bigstring.unsafe_set_int64_ne t.buffer t.write_pos (bswap_int64 i)
    else Bigstring.unsafe_set_int64_ne t.buffer t.write_pos i;
    t.write_pos <- t.write_pos + 8
end

let close t =
  t.closed <- true;
  flush_buffer t

let is_closed t = t.closed

let pending_bytes t =
  t.write_pos - t.scheduled_pos + (t.bytes_received - t.bytes_written)

let has_pending_output t = pending_bytes t <> 0
let yield t = t.yield <- true

let rec shift_buffers t written =
  match Buffers.dequeue_exn t.scheduled with
  | exception Dequeue_empty ->
    assert (written = 0);
    if t.scheduled_pos = t.write_pos
    then begin
      t.scheduled_pos <- 0;
      t.write_pos <- 0
    end
  | { len; _ } as iovec ->
    if len <= written
    then begin
      shift_buffers t (written - len)
    end
    else Buffers.enqueue_front (IOVec.shift iovec written) t.scheduled

let rec shift_flushes t ~reason =
  match Flushes.dequeue_exn t.flushed with
  | exception Dequeue_empty -> ()
  | (threshold, f) as flush ->
    (* Edited notes from @dinosaure:
     *
     * The quantities [t.bytes_written] and [threshold] are always going to be
     * positive integers. Therefore, we can treat them as unsinged integers for
     * the purposes of comparision. Doing so allows us to handle overflows in
     * either quantity as long as they're both within one overflow of each other.
     * We can accomplish this by subracting [min_int] from both quantities before
     * comparision. This shift a quantity that has not overflowed into the
     * negative integer range while shifting a quantity that has overflow into
     * the positive integer range.
     *
     * This effectively restablishes the relative difference when an overflow
     * has occurred, and otherwise just compares numbers that haven't
     * overflowed as similarly, just shifted down a bit.
     *)
    if t.bytes_written - min_int >= threshold - min_int
    then begin
      f reason;
      shift_flushes t ~reason
    end
    else Flushes.enqueue_front flush t.flushed

let shift_internal t written ~reason =
  shift_buffers t written;
  t.bytes_written <- t.bytes_written + written;
  shift_flushes t ~reason

let shift t written = shift_internal t written ~reason:Shift

let operation t =
  if t.closed
  then begin
    t.yield <- false
  end;
  flush_buffer t;
  let nothing_to_do = not (has_pending_output t) in
  if t.closed && nothing_to_do
  then `Close
  else if t.yield || nothing_to_do
  then begin
    t.yield <- false;
    `Yield
  end
  else begin
    let iovecs = Buffers.map_to_list t.scheduled ~f:(fun x -> x) in
    `Writev iovecs
  end

let serialize_to_string t =
  close t;
  match operation t with
  | `Writev iovecs ->
    let len = IOVec.lengthv iovecs in
    let bytes = Bytes.create len in
    let pos = ref 0 in
    List.iter
      (function
        | { buffer; off; len } ->
          Bigstring.unsafe_blit_to_bytes
            buffer
            ~src_off:off
            bytes
            ~dst_off:!pos
            ~len;
          pos := !pos + len)
      iovecs;
    shift t len;
    assert (operation t = `Close);
    Bytes.unsafe_to_string bytes
  | `Close -> ""
  | `Yield -> assert false

let drain =
  let rec loop t acc =
    match operation t with
    | `Writev iovecs ->
      let len = IOVec.lengthv iovecs in
      shift_internal t len ~reason:Drain;
      loop t (len + acc)
    | `Close -> acc
    | `Yield -> loop t acc
  in
  fun t -> loop t 0
