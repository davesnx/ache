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

(** Serialization primitives built for speed an memory-efficiency.


    Encoder is ache's buffered writer for fast and memory-efficient
    serializers. Its core type and related operation gives the user
    fine-grained control over copying and allocation behavior while serializing user-defined types, and
    presents the output in a form that makes it possible to use vectorized
    write operations, such as the [writev][] system call, or any other platform
    or application-specific output APIs.

    A serializer manages an internal buffer and a queue of output
    buffers. The output bufferes may be a sub range of the serializer's
    internal buffer or one that is user-provided. Buffered writes such as
    {!write_string}, {!write_char} and {!write_bigstring} copy the source
    bytes into the serializer's internal buffer. The unbuffered write
    {!schedule_bigstring}, on the other hand, performs no copying. Instead, it
    enqueues the source bytes into the serializer's write queue directly. *)

type bigstring =
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

type t
(** The type of a serializer. *)

(** {2 Constructors} *)

val create : int -> t
(** [create len] creates a serializer with a fixed-length internal buffer of
    length [len]. See the Buffered writes section for details about what happens
    when [len] is not large enough to support a write. *)

val of_bigstring : bigstring -> t
(** [of_bigstring buf] creates a serializer, using [buf] as its internal
    buffer. The serializer takes ownership of [buf] until the serializer has
    been closed and flushed of all output. *)

(** {2 Buffered Writes}

    A serializer manages an internal buffer for coalescing small writes. The
    size of this buffer is determined when the serializer is created. If the
    buffer does not contain sufficient space to service a caller's buffered
    write, the serializer allocates a new buffer, at least twice as large and
    with room for the old buffer's contents plus the write, up to 64 KiB, and
    uses it for the current and subsequent writes. Once all of its output has
    been {!shift}ed out, writes start again at its beginning, so later output
    of the same size needs no new buffer. The old buffer will be garbage
    collected once all of its contents have been {!shift}ed out.

    A write that does not fit and is larger than both 64 KiB and the current
    buffer is copied into a buffer of its own, scheduled like
    {!schedule_bigstring}; the serializer keeps its current buffer. *)

val write_string : t -> ?off:int -> ?len:int -> string -> unit
(** [write_string t ?off ?len str] copies [str] into the serializer's
    internal buffer. *)

val write_bigstring : t -> ?off:int -> ?len:int -> bigstring -> unit
(** [write_bigstring t ?off ?len bigstring] copies [bigstring] into the
    serializer's internal buffer. It is safe to modify [bigstring] after this
    call returns.  *)

val write_char : t -> char -> unit
(** [write_char t char] copies [char] into the serializer's internal buffer. *)

(** Little endian serializers *)
module LE : sig
  val write_uint16 : t -> int -> unit
  (** [write_uint16 t n] copies the lower 16 bits of [n] into the
      serializer's internal buffer in little-endian byte order. *)

  val write_uint64 : t -> int64 -> unit
  (** [write_uint64 t n] copies [n] into the serializer's internal buffer in
      little-endian byte order. *)
end

(** {2 Unbuffered Writes}

    Unbuffered writes do not involve copying bytes to the serializers internal
    buffer. *)

val schedule_bigstring : t -> ?off:int -> ?len:int -> bigstring -> unit
(** [schedule_bigstring t ?off ?len bigstring] schedules [bigstring] to
    be written the next time the serializer surfaces writes to the user.
    [bigstring] is not copied in this process, so [bigstring] should only be
    modified after [t] has been flushed. *)

(** {2 Querying A Serializer's State} *)

val has_pending_output : t -> bool
(** [has_pending_output t] is [true] if [t]'s output queue is non-empty. It may
    be the case that [t]'s queued output is being serviced by some other thread
    of control, but has not yet completed. *)

val pending_bytes : t -> int
(** [pending_bytes t] is the size of the next write, in bytes, that [t] will
    surface to the caller as a [`Writev]. *)

(** {2 Control Operations} *)

val yield : t -> unit
(** [yield t] causes [t] to delay surfacing writes to the user, instead
    returning a [`Yield]. This gives the serializer an opportunity to collect
    additional writes before sending them to the underlying device, which will
    increase the write batch size.

    As one example, code may want to call this function if it's about to
    release the OCaml lock and perform a blocking system call, but would like
    to batch output across that system call. To hint to the thread of control
    that is performing the writes on behalf of the serializer, the code might
    call [yield t] before releasing the lock. *)

module Flushed_reason : sig
  (** Indicates why a flush callback was called. *)
  type t =
    | Shift
    (** [shift t] was called, normally indicating that bytes were written
        successfully. *)
    | Drain
    (** [drain t] was called, normally indicating that the downstream
        consumer of [t]'s bytes stopped accepting new input. *)
    | Nothing_pending
    (** Passed to [f] when [flush_with_reason t f] is called when there is
        not any pending output, so [t] is considered immediately flushed. *)
end

val flush_with_reason : t -> (Flushed_reason.t -> unit) -> unit
(** [flush_with_reason t f] registers [f] to be called when all prior writes
    have been successfully completed. If [t] has no pending writes, then [f]
    will be called immediately. If {!yield} was recently called on [t], then
    the effect of the [yield] will be ignored so that client code has an
    opportunity to write pending output, regardless of how it handles [`Yield]
    operations. [f] is supplied with the reason that the callback was
    triggered. *)

val close : t -> unit
(** [close t] closes [t]. All subsequent write calls will raise, and any
    pending or subsequent {!yield} calls will be ignored. If the serializer has
    any pending writes, user code will have an opportunity to service them
    before it receives the [Close] operation. Flush callbacks will continue to
    be invoked while output is {!shift}ed out of [t] as needed. *)

val is_closed : t -> bool
(** [is_closed t] is [true] if [close] has been called on [t] and [false]
    otherwise. A closed [t] may still have pending output. *)

val shift : t -> int -> unit
(** [shift t n] removes the first [n] bytes in [t]'s write queue. Any flush
    callbacks registered with [t] within this span of the write queue will be
    called. *)

val drain : t -> int
(** [drain t] removes all pending writes from [t], returning the number of
    bytes that were enqueued to be written and freeing any scheduled
    buffers in the process. *)

(** {2 Running}

    Low-level operations for running a serializer. *)

type 'a iovec =
  { buffer : 'a
  ; off : int
  ; len : int
  }
(** A view into {!iovec.buffer} starting at {!iovec.off} and with length
    {!iovec.len}. *)

type operation =
  [ `Writev of bigstring iovec list
  | `Yield
  | `Close
  ]
(** The type of operations that the serialier may wish to perform.
  {ul

  {li [`Writev iovecs]: Write the bytes in {!iovecs}s reporting the actual
  number of bytes written by calling {!shift}. You must accurately report the
  number of bytes written. Failure to do so will result in the same bytes being
  surfaced in a [`Writev] operation multiple times.}

  {li [`Yield]: Yield to other threads of control, waiting for additional
  output before procedding. The method for achieving this is
  application-specific, but once complete, the caller can proceed with
  serialization by simply making another call to {!val:operation}.}

  {li [`Close]: Serialization is complete. No further output will generated.
  The action to take as a result, if any, is application-specific.}} *)

val operation : t -> operation
(** [operation t] is the next operation that the caller must perform on behalf
    of the serializer [t]. See the documentation for the {!type:operation}
    type for details on how callers should handle these operations. *)

(** {2 Convenience Functions}

    These functions are included for testing, debugging, and general
    development. They are not the suggested way of driving a serializer in a
    production setting. *)

val serialize_to_string : t -> string
(** [serialize_to_string t] runs [t], collecting the output into a string and
    returning it. [serialzie_to_string t] immediately closes [t] and ignores
    any calls to {!yield} on [t]. *)
