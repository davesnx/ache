(* Free bigstrings of a connection, per domain and by size, so that a closing
   connection hands its buffers to the next one. A bigstring is a custom block:
   its memory paces major cycles, and a connection per request would start one
   every few hundred connections (runtime-cost-2 H4).

   A pool belongs to one domain ([Domain.DLS]); a buffer given back on another
   domain joins that domain's pool. Only the code that owns a buffer gives it
   back, once nothing reads or writes it any more. The next owner reads only the
   bytes it wrote itself, so the buffer is not cleared. *)

(* Bounds of what a domain keeps: [max_classes] sizes, each up to [max_size]
   bytes, and per size at most [max_buffers] buffers and [max_bytes] bytes. *)
let max_size = 0x10000
let max_bytes = 0x40000
let max_buffers = 64
let max_classes = 4

type pool =
  { size : int
  ; mutable count : int
  ; buffers : Bigstring.t array
  }

let key : pool array ref Domain.DLS.key =
  Domain.DLS.new_key (fun () -> ref [||])

(* The pool of no size: empty, and full. *)
let absent = { size = -1; count = 0; buffers = [||] }

let rec find pools size i =
  if i = Array.length pools
  then absent
  else
    let pool = Array.unsafe_get pools i in
    if pool.size = size then pool else find pools size (i + 1)

let take size =
  let pool = find !(Domain.DLS.get key) size 0 in
  if pool.count > 0
  then (
    let n = pool.count - 1 in
    let buffer = pool.buffers.(n) in
    pool.buffers.(n) <- Bigstring.empty;
    pool.count <- n;
    buffer)
  else Bigstring.create size

let give buffer =
  let size = Bigstring.length buffer in
  if size > 0 && size <= max_size
  then
    let pools = Domain.DLS.get key in
    let pool =
      let pool = find !pools size 0 in
      if pool == absent && Array.length !pools < max_classes
      then (
        let capacity = Int.min max_buffers (max_bytes / size) in
        let pool =
          { size; count = 0; buffers = Array.make capacity Bigstring.empty }
        in
        pools := Array.append !pools [| pool |];
        pool)
      else pool
    in
    if pool.count < Array.length pool.buffers
    then (
      pool.buffers.(pool.count) <- buffer;
      pool.count <- pool.count + 1)

let pooled size = (find !(Domain.DLS.get key) size 0).count
