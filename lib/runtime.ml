(*----------------------------------------------------------------------------
 *  Copyright (c) 2019-2020 António Nuno Monteiro
 *
 *  All rights reserved.
 *
 *  Redistribution and use in source and binary forms, with or without
 *  modification, are permitted provided that the following conditions are met:
 *
 *  1. Redistributions of source code must retain the above copyright notice,
 *  this list of conditions and the following disclaimer.
 *
 *  2. Redistributions in binary form must reproduce the above copyright
 *  notice, this list of conditions and the following disclaimer in the
 *  documentation and/or other materials provided with the distribution.
 *
 *  3. Neither the name of the copyright holder nor the names of its
 *  contributors may be used to endorse or promote products derived from this
 *  software without specific prior written permission.
 *
 *  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 *  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 *  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 *  ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
 *  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 *  CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 *  SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 *  INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 *  CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 *  ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 *  POSSIBILITY OF SUCH DAMAGE.
 *---------------------------------------------------------------------------*)

module type ENGINE = sig
  type t

  val next_read_operation : t -> [ `Read | `Yield | `Close ]
  val read : t -> Bigstringaf.t -> off:int -> len:int -> int
  val read_eof : t -> Bigstringaf.t -> off:int -> len:int -> int
  val yield_reader : t -> (unit -> unit) -> unit

  val next_write_operation :
     t
    -> [ `Write of Bigstringaf.t IOVec.t list | `Yield | `Close of int ]

  val report_write_result : t -> [ `Ok of int | `Closed ] -> unit
  val yield_writer : t -> (unit -> unit) -> unit
  val report_exn : t -> exn -> unit
  val is_closed : t -> bool
  val shutdown : t -> unit
end

type impl = Impl : (module ENGINE with type t = 't) * 't -> impl

let make runtime t = Impl (runtime, t)

module Upgradable = struct
  type t = { mutable impl : impl }

  let create impl = { impl }

  let upgrade t impl' =
    let (Impl ((module Engine), engine)) = t.impl in
    t.impl <- impl';
    Engine.shutdown engine

  let next_read_operation { impl = Impl ((module Engine), engine) } =
    Engine.next_read_operation engine

  let read { impl = Impl ((module Engine), engine) } = Engine.read engine

  let read_eof { impl = Impl ((module Engine), engine) } =
    Engine.read_eof engine

  let yield_reader { impl = Impl ((module Engine), engine) } =
    Engine.yield_reader engine

  let next_write_operation { impl = Impl ((module Engine), engine) } =
    Engine.next_write_operation engine

  let report_write_result { impl = Impl ((module Engine), engine) } =
    Engine.report_write_result engine

  let yield_writer { impl = Impl ((module Engine), engine) } =
    Engine.yield_writer engine

  let report_exn { impl = Impl ((module Engine), engine) } =
    Engine.report_exn engine

  let is_closed { impl = Impl ((module Engine), engine) } =
    Engine.is_closed engine

  let shutdown { impl = Impl ((module Engine), engine) } =
    Engine.shutdown engine
end

module Buffer = struct
  type t =
    { buffer : Bigstringaf.t
    ; mutable off : int
    ; mutable len : int
    ; cap : int
    ; mutable unread : bool
    }

  let create size =
    let buffer = Bigstringaf.create size in
    { buffer; off = 0; len = 0; cap = size; unread = false }

  let compress t =
    if t.len = 0
    then (
      t.off <- 0;
      t.len <- 0)
    else if t.off > 0
    then (
      Bigstringaf.blit t.buffer ~src_off:t.off t.buffer ~dst_off:0 ~len:t.len;
      t.off <- 0)

  let get t ~f =
    let n = f t.buffer ~off:t.off ~len:t.len in
    t.off <- t.off + n;
    t.len <- t.len - n;
    if t.len = 0 then t.off <- 0;
    t.unread <- n > 0 && t.len > 0;
    n

  let unread t = t.unread

  let put t ~f k =
    compress t;
    let off = t.off + t.len in
    let len = t.cap - t.len - t.off in
    f t.buffer ~off ~len (fun n ->
      t.len <- t.len + n;
      k n)
end
