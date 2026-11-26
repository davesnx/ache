(*----------------------------------------------------------------------------
  Copyright (c) 2018 Inhabited Type LLC. Copyright (c) 2018 Anton Bachin
  Copyright (c) 2019-2020 António Nuno Monteiro

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

open Lwt.Infix
include Ache_lwt_intf
module Buffer = Ache.Ache_private.Runtime.Buffer
module Upgradable = Ache.Ache_private.Runtime.Upgradable

module IO_loop = struct
  (* Ends a read that waits, without reading the socket: the read loop goes on
     without its bytes. *)
  exception Interrupted

  (* The read of the read loop. A timer or a drain ends a read that waits with
     [interrupt]; the socket read stays, as [left]. *)
  type reader =
    { mutable reading : int Lwt.t
    ; mutable resolver : int Lwt.u
    ; (* The socket read of an interrupted read, while it waits. *)
      mutable left : (int Lwt.t * int Lwt.u) option
    }

  let reader () =
    { reading = Lwt.return 0; resolver = snd (Lwt.wait ()); left = None }

  (* The hooks of a server around each socket read of the read loop. *)
  type timing =
    { reader : reader
    ; (* Before the read starts. *)
      starts : unit -> unit
    ; (* The read waits. *)
      waits : unit -> unit
    ; (* The read returned this many bytes. *)
      returned : int -> unit
    }

  let untimed () =
    { reader = reader (); starts = ignore; waits = ignore; returned = ignore }

  let waiting r = Lwt.is_sleeping r.reading

  let interrupt r exn =
    if Lwt.is_sleeping r.reading
    then (
      r.left <- Some (Lwt.wait ());
      Lwt.wakeup_later_exn r.resolver exn)

  let pending_left r =
    match r.left with
    | Some (left, _) when Lwt.is_sleeping left -> Some left
    | Some _ | None -> None

  let read (type fd) (module Io : IO with type socket = fd) socket read_buffer =
    let p, u = Lwt.wait () in
    Buffer.put
      ~f:(fun buf ~off ~len k ->
        if len = 0
        then Lwt.wakeup_later_exn u (Failure "ache-lwt: read buffer full")
        else
          Lwt.on_any (Io.read socket buf ~off ~len) k (Lwt.wakeup_later_exn u))
      read_buffer
      (fun n -> Lwt.wakeup_later u n);
    p

  (* A read that [interrupt] can end. *)
  let read_interruptible
        (type fd)
        (module Io : IO with type socket = fd)
        r
        socket
        read_buffer
    =
    let p, u = Lwt.wait () in
    r.reading <- p;
    r.resolver <- u;
    let finish n =
      if Lwt.is_sleeping p
      then Lwt.wakeup_later u n
      else
        match r.left with
        | Some (left, u) when Lwt.is_sleeping left -> Lwt.wakeup_later u n
        | Some _ | None -> ()
    and fail exn =
      if Lwt.is_sleeping p
      then Lwt.wakeup_later_exn u exn
      else
        match r.left with
        | Some (left, u) when Lwt.is_sleeping left -> Lwt.wakeup_later_exn u exn
        | Some _ | None -> ()
    in
    Buffer.put
      ~f:(fun buf ~off ~len k ->
        if len = 0
        then fail (Failure "ache-lwt: read buffer full")
        else Lwt.on_any (Io.read socket buf ~off ~len) k fail)
      read_buffer
      finish;
    p

  let close_once close socket =
    let closing = ref None in
    fun () ->
      match !closing with
      | Some closing -> closing
      | None ->
        let p =
          Lwt.catch (fun () -> close socket) (fun (_ : exn) -> Lwt.return_unit)
        in
        closing := Some p;
        p

  let start : type t fd.
    (module IO with type socket = fd)
    -> (module Ache.Runtime.ENGINE with type t = t)
    -> t
    -> read_buffer_size:int
    -> ?timing:timing
    -> close:(unit -> unit Lwt.t)
    -> ?writer_waits:(unit -> unit)
    -> ?linger:(unit -> ((unit -> unit) -> unit) option)
    -> ?writev:
         (Ache.Bigstring.t Ache.IOVec.t list -> [ `Closed | `Ok of int ] Lwt.t)
    -> fd
    -> unit
    =
   fun (module Io)
     (module Runtime)
     t
     ~read_buffer_size
     ?(timing = untimed ())
     ~close
     ?(writer_waits = ignore)
     ?linger
     ?writev
     socket ->
    let read_buffer = Buffer.create read_buffer_size in
    (* [eof]: a read saw the end of input, so a linger would read nothing. *)
    let eof = ref false
    (* [send_shut]: the write loop shut the send side. [on_send_shut]: the
       linger waits for it to arm its deadline. *)
    and send_shut = ref false
    and on_send_shut = ref ignore
    and reader = timing.reader in
    (* The socket closes when both loops have exited, or at once when one fails.
       [close] does not fail. *)
    let closing = ref false
    and running = ref 2 in
    let close_socket () =
      if not !closing
      then (
        closing := true;
        let (_ : unit Lwt.t) = close () in
        ())
    in
    (* Called once by each loop. *)
    let loop_exited () =
      decr running;
      if !running = 0 then close_socket ()
    in
    let fail exn =
      (try Runtime.report_exn t exn with _ -> ());
      close_socket ();
      Lwt.return_unit
    in
    let read_loop_exited = ref false in
    let read_loop_done () =
      if not !read_loop_exited
      then (
        read_loop_exited := true;
        loop_exited ());
      (try Io.shutdown_receive socket with _ -> ());
      Lwt.return_unit
    in
    let read_failed exn =
      match Runtime.report_exn t exn with
      | () -> read_loop_done ()
      | exception _ ->
        close_socket ();
        Lwt.return_unit
    in
    let rec read_loop () =
      let rec read_loop_step () =
        match Runtime.next_read_operation t with
        | `Read ->
          Lwt.catch
            (fun () ->
               (if Buffer.unread read_buffer
                then Lwt.return 0
                else
                  match pending_left reader with
                  (* An interrupted read whose socket read still waits: its
                     bytes are the next input. *)
                  | Some left -> left
                  | None ->
                    timing.starts ();
                    let reading =
                      read_interruptible (module Io) reader socket read_buffer
                    in
                    if Lwt.is_sleeping reading
                    then (
                      timing.waits ();
                      reading)
                    else
                      reading >>= fun n ->
                      Lwt.pause () >|= fun () -> n)
               >|= fun n ->
               timing.returned n;
               let (_ : int) = Buffer.get read_buffer ~f:(Runtime.read t) in
               ())
            (function
              | Interrupted -> Lwt.return_unit
              | End_of_file ->
                eof := true;
                let (_ : int) =
                  Buffer.get read_buffer ~f:(Runtime.read_eof t)
                in
                Lwt.return_unit
              | exn -> Lwt.fail exn)
          >>= read_loop_step
        | `Yield ->
          Runtime.yield_reader t read_loop;
          Lwt.return_unit
        | `Close ->
          (match linger with
          | Some lingers when not !eof ->
            (match lingers () with
            | Some arm -> lingering arm
            | None -> read_loop_done ())
          | Some _ | None -> read_loop_done ())
      (* Lingering close (RFC 9112 9.6): reads and drops input until the end of
         input, or until the send side is shut and the deadline that [arm] sets
         passes, so that the close does not reset the connection under unread
         input and the peer can read the last response. The drain runs on
         callbacks; at the deadline the read loop ends at once, and the socket
         read in progress is left to the close. *)
      and lingering arm =
        let drop () =
          let (_ : int) =
            Buffer.get read_buffer ~f:(fun _ ~off:_ ~len -> len)
          in
          ()
        in
        let read () =
          match read (module Io) socket read_buffer with
          | reading -> reading
          | exception exn -> Lwt.fail exn
        in
        let ended = ref false in
        let finish () =
          if not !ended
          then (
            ended := true;
            drop ();
            let (_ : unit Lwt.t) = read_loop_done () in
            ())
        in
        let rec drain reading =
          match Lwt.state reading with
          | Lwt.Sleep ->
            Lwt.on_any
              reading
              (fun (_ : int) ->
                 if not !ended
                 then (
                   drop ();
                   drain (read ())))
              (fun (_ : exn) -> finish ())
          | Lwt.Return (_ : int) ->
            (* Input was ready at once: let other connections and the timers run
               before the next read, or a client that always has input ready
               would keep the drain from returning to [Lwt_main], and its
               deadline from passing. *)
            drop ();
            Lwt.on_success (Lwt.pause ()) (fun () ->
              if not !ended then drain (read ()))
          | Lwt.Fail (_ : exn) -> finish ()
        in
        drop ();
        let reading =
          match pending_left reader with Some left -> left | None -> read ()
        in
        (match Lwt.state reading with
        | Lwt.Fail (_ : exn) ->
          (* The end of input, or an error: no deadline. *)
          finish ()
        | Lwt.Sleep | Lwt.Return (_ : int) ->
          let arm () = if not !ended then arm finish in
          if !send_shut then arm () else on_send_shut := arm;
          drain reading);
        Lwt.return_unit
      in
      Lwt.async (fun () -> Lwt.catch read_loop_step read_failed)
    in
    let writev = Option.value writev ~default:(Io.writev socket) in
    let write_loop_exited = ref false in
    let rec write_loop () =
      let rec write_loop_step () =
        match Runtime.next_write_operation t with
        | `Write io_vectors ->
          writev io_vectors >>= fun result ->
          Runtime.report_write_result t result;
          write_loop_step ()
        | `Yield ->
          writer_waits ();
          Runtime.yield_writer t write_loop;
          Lwt.return_unit
        | `Close _ ->
          writer_waits ();
          if Option.is_some linger
          then (
            (try Io.shutdown_send socket with _ -> ());
            send_shut := true;
            let on_send_shut' = !on_send_shut in
            on_send_shut := ignore;
            on_send_shut' ());
          if not !write_loop_exited
          then (
            write_loop_exited := true;
            loop_exited ());
          Lwt.return_unit
      in
      Lwt.async (fun () -> Lwt.catch write_loop_step fail)
    in
    read_loop ();
    write_loop ()
end

module Timeout = struct
  (* Deadlines are in microseconds, so that a [set] neither allocates nor calls
     [caml_modify]. A [set] of a queued timeout only moves [at]; the queue keeps
     it at [queued_at] until [run] meets it and puts it back in order. All
     timeouts of a queue have one length, so a [set] never moves a deadline
     earlier. *)
  type t =
    { queue : queue
    ; mutable at : int
    ; mutable queued_at : int
    ; mutable prev : t
    ; mutable next : t
    ; (* The promise of [expired], made by its first call. *)
      mutable expired : (unit Lwt.t * unit Lwt.u) option
    ; (* Called when the deadline passes. *)
      on_expire : t -> unit
    }

  and queue =
    { seconds : float
    ; span : int
    ; root : t
    ; mutable sleeping : bool
    }

  (* Keyed on the bits of the length, so that a lookup neither calls the
     polymorphic hash nor compares boxed floats. *)
  module Queues = Hashtbl.Make (struct
      type t = float

      let equal = Float.equal

      let hash seconds =
        let bits = Int64.bits_of_float seconds in
        Int64.to_int (Int64.logxor bits (Int64.shift_right_logical bits 32))
        land max_int
    end)

  let queues : queue Queues.t = Queues.create 2

  (* The queue of the last [create]: a server creates timeouts of a few lengths
     over and over. *)
  let last_queue = ref None
  let no_callback (_ : t) = ()

  (* [expired] of a timeout that expired before anyone asked for it. *)
  let resolved = Some (Lwt.return_unit, snd (Lwt.wait ()))

  let find_queue seconds =
    match !last_queue with
    | Some queue when Float.equal queue.seconds seconds -> queue
    | Some _ | None ->
      let queue =
        match Queues.find_opt queues seconds with
        | Some queue -> queue
        | None ->
          let rec queue =
            { seconds
            ; span = int_of_float (Float.ceil (seconds *. 1e6))
            ; root
            ; sleeping = false
            }
          and root =
            { queue
            ; at = max_int
            ; queued_at = max_int
            ; prev = root
            ; next = root
            ; expired = None
            ; on_expire = no_callback
            }
          in
          Queues.add queues seconds queue;
          queue
      in
      last_queue := Some queue;
      queue

  let create_with seconds on_expire =
    if not (seconds > 0.)
    then invalid_arg "Ache_lwt.Timeout.create: seconds must be positive";
    let queue = find_queue seconds in
    let rec t =
      { queue
      ; at = 0
      ; queued_at = 0
      ; prev = t
      ; next = t
      ; expired = None
      ; on_expire
      }
    in
    t

  let create seconds = create_with seconds no_callback

  (* The deadline of [t] is set and has not passed. *)
  let pending t = t.next != t

  let stop t =
    t.prev.next <- t.next;
    t.next.prev <- t.prev;
    t.prev <- t;
    t.next <- t

  let now () = int_of_float (Unix.gettimeofday () *. 1e6)

  (* Queues [t] in deadline order, from the back: [t] is usually last. *)
  let enqueue queue t =
    t.queued_at <- t.at;
    let rec after p =
      if p != queue.root && p.queued_at > t.at then after p.prev else p
    in
    let p = after queue.root.prev in
    t.prev <- p;
    t.next <- p.next;
    p.next.prev <- t;
    p.next <- t

  let rec run queue () =
    let now = now () in
    let rec expire () =
      let t = queue.root.next in
      if t != queue.root && t.at <> t.queued_at
      then (
        (* Set again since it was queued. *)
        stop t;
        enqueue queue t;
        expire ())
      else if t != queue.root && t.at <= now
      then (
        stop t;
        (match t.expired with
        | Some (p, u) when Lwt.is_sleeping p -> Lwt.wakeup_later u ()
        | Some _ -> ()
        | None -> t.expired <- resolved);
        (try t.on_expire t with exn -> !Lwt.async_exception_hook exn);
        expire ())
    in
    expire ();
    let front = queue.root.next in
    if front == queue.root
    then queue.sleeping <- false
    else
      Lwt.on_success
        (Lwt_unix.sleep (float (front.at - now) /. 1e6))
        (run queue)

  let set t =
    let queue = t.queue in
    (match t.expired with
    | Some (p, _) when not (Lwt.is_sleeping p) -> t.expired <- None
    | Some _ | None -> ());
    t.at <- now () + queue.span;
    if t.next == t then enqueue queue t;
    if not queue.sleeping
    then (
      queue.sleeping <- true;
      Lwt.on_success (Lwt_unix.sleep queue.seconds) (run queue))

  let expired t =
    match t.expired with
    | Some (p, _) -> Lwt.protected p
    | None ->
      let ((p, _) as expired) = Lwt.wait () in
      t.expired <- Some expired;
      Lwt.protected p
end

module Websocket = struct
  type t =
    { connection : Ache.Websocket.t
    ; messages : Ache.Websocket.message Queue.t
    ; receivers :
        (Ache.Websocket.message option Lwt.t
        * Ache.Websocket.message option Lwt.u)
          Queue.t
    ; pingers : (unit Lwt.t * unit Lwt.u) Queue.t
    ; ended : bool ref
    ; client_close : Ache.Websocket.close option ref
    ; closed : unit Lwt.t
    }

  exception Closed

  let max_queued_messages = 8

  let rec take_waiter waiters =
    match Queue.take_opt waiters with
    | Some (p, u) when Lwt.is_sleeping p -> Some u
    | Some _ -> take_waiter waiters
    | None -> None

  let prune waiters =
    let pending = Queue.of_seq (Queue.to_seq waiters) in
    Queue.clear waiters;
    Queue.iter
      (fun ((p, _) as waiter) ->
         if Lwt.is_sleeping p then Queue.push waiter waiters)
      pending

  let create ?max_message_size ?headers reqd =
    let messages = Queue.create ()
    and receivers = Queue.create ()
    and pingers = Queue.create ()
    and ended = ref false
    and client_close = ref None in
    let closed, notify_closed = Lwt.wait () in
    let connection = ref None in
    match
      Ache.Websocket.upgrade ?max_message_size ?headers reqd (fun ws ->
        connection := Some ws;
        { on_message =
            (fun message ->
              match take_waiter receivers with
              | Some u -> Lwt.wakeup_later u (Some message)
              | None ->
                Queue.push message messages;
                if Queue.length messages >= max_queued_messages
                then Ache.Websocket.pause_reading ws)
        ; on_pong =
            (fun (_ : string) ->
              let rec wake_all () =
                match take_waiter pingers with
                | Some u ->
                  Lwt.wakeup_later u ();
                  wake_all ()
                | None -> ()
              in
              wake_all ())
        ; on_close =
            (fun close ->
              ended := true;
              client_close := close;
              let rec wake_receivers () =
                match take_waiter receivers with
                | Some u ->
                  Lwt.wakeup_later u None;
                  wake_receivers ()
                | None -> ()
              in
              wake_receivers ();
              let rec fail_pingers () =
                match take_waiter pingers with
                | Some u ->
                  Lwt.wakeup_later_exn u Closed;
                  fail_pingers ()
                | None -> ()
              in
              fail_pingers ();
              Lwt.wakeup_later notify_closed ())
        })
    with
    | Error _ as error -> error
    | Ok () ->
      Ok
        { connection = Option.get !connection
        ; messages
        ; receivers
        ; pingers
        ; ended
        ; client_close
        ; closed
        }

  let upgrade ?max_message_size ?headers reqd session =
    match create ?max_message_size ?headers reqd with
    | Error _ as error -> error
    | Ok t ->
      Lwt.async (fun () ->
        Lwt.catch
          (fun () ->
             session t >|= fun () ->
             (* Nothing reads the messages any more: close, unless the
                connection already ended. *)
             Ache.Websocket.close t.connection ~code:1000)
          (function
            | Closed ->
              Ache.Websocket.close t.connection ~code:1000;
              Lwt.return_unit
            | exn ->
              Ache.Websocket.close t.connection ~code:1011;
              Lwt.reraise exn));
      Ok ()

  let written t =
    let p, u = Lwt.wait () in
    Ache.Websocket.flush t.connection (function
      | `Written -> Lwt.wakeup_later u ()
      | `Closed -> Lwt.wakeup_later_exn u Closed);
    p

  let send_text t message =
    Ache.Websocket.send_text t.connection message;
    written t

  let send_binary t message =
    Ache.Websocket.send_binary t.connection message;
    written t

  let receive t =
    match Queue.take_opt t.messages with
    | Some message ->
      if Queue.length t.messages < max_queued_messages
      then Ache.Websocket.resume_reading t.connection;
      Lwt.return_some message
    | None when !(t.ended) -> Lwt.return_none
    | None ->
      prune t.receivers;
      let p, u = Lwt.task () in
      Queue.push (p, u) t.receivers;
      Ache.Websocket.resume_reading t.connection;
      p

  let ping t payload =
    Ache.Websocket.ping t.connection payload;
    if !(t.ended)
    then Lwt.fail Closed
    else (
      prune t.pingers;
      let p, u = Lwt.task () in
      Queue.push (p, u) t.pingers;
      p)

  let client_close t = !(t.client_close)

  let close ?reason t code =
    Ache.Websocket.close ?reason t.connection ~code;
    t.closed
end

let current_date = Ache.Ache_private.Http_date.now

(* A client checks an idle connection for response bytes before it writes a
   request (undici GHSA-35p6-xmwp-9g52). *)
module Idle_input = struct
  let rec has_response_byte buf i stop =
    i < stop
    &&
    match Ache.Bigstring.unsafe_get buf i with
    | '\r' | '\n' -> has_response_byte buf (i + 1) stop
    | _ -> true

  (* Bytes other than empty lines, or the end of input, that wait on the
     non-blocking [fd]. [fd] must not be read by another thread. *)
  let waiting fd =
    let buf = Bytes.create 64 in
    match Unix.recv fd buf 0 (Bytes.length buf) [ Unix.MSG_PEEK ] with
    | n ->
      n = 0
      || Bytes.exists
           (function '\r' | '\n' -> false | _ -> true)
           (Bytes.sub buf 0 n)
    | exception Unix.Unix_error ((ECONNRESET | ECONNABORTED | EPIPE), _, _) ->
      true
    | exception Unix.Unix_error _ -> false
end

module Make_with_idle_check
    (Io : IO)
    (Idle : sig
       val input_waiting : Io.socket -> bool
     end) =
struct
  module Server = struct
    type socket = Io.socket
    type addr = Io.addr

    let check_timeout name = function
      | Some seconds when not (seconds > 0.) ->
        invalid_arg
          (Printf.sprintf
             "Ache_lwt.Server.create_connection_handler: %s must be positive"
             name)
      | _ -> ()

    (* The body and write clocks restart after this many bytes, so that a peer
       that sends or reads a byte at a time cannot keep them alive. *)
    let progress_bytes = 2048

    (* The body clock counts in ticks of a tenth of [body_timeout], so that all
       connections share one [Timeout] queue for each [body_timeout]. *)
    let body_ticks = 10

    (* The linger limit when [idle_timeout] is [None]. *)
    let linger_without_idle_timeout = 2.0

    (* WebSocket state of a connection, made when a WebSocket needs it. *)
    type websocket =
      { (* Close timer of a WebSocket: a fixed deadline from the moment the
           server's close frame is queued. The messages that still arrive do not
           restart it, and it is not stopped: a connection closes only once.
           Made, and set, when the close frame is queued. *)
        mutable close_timer : Timeout.t option
      ; (* The close timer expired: the connection closes without a linger. *)
        mutable close_expired : bool
      }

    (* The phase of the read in progress, from
       [Ache.Server_connection.read_timeout] when it started. *)
    type phase =
      [ `Idle
      | `Head
      | `Body
      | `Close
      | `None
      ]

    (* The timeouts of a connection and their state. Each timer is made on its
       first [set], so that a connection pays only for the timers it uses.

       A read that waits has at most one deadline: the timer in [armed]. The
       timer's callback runs the action that [read_timeout] gives at that
       moment, then ends the read with [IO_loop.interrupt]. The socket read
       stays: the next read, or the linger, takes its bytes. *)
    type timers =
      { socket : socket
      ; idle_timeout : float option
      ; head_timeout : float option
      ; (* The length of a body clock tick. *)
        tick_timeout : float option
      ; close_timeout : float option
      ; (* Responses get a [Keep-Alive] hint. *)
        keep_alive_hint : bool
      ; reader : IO_loop.reader
      ; mutable phase : phase
      ; mutable armed : Timeout.t option
      ; mutable head_timer : Timeout.t option
      ; mutable head_armed : bool
      ; mutable idle_timer : Timeout.t option
      ; (* Between requests the idle timer runs 1 s longer than the [Keep-Alive]
           hint, so that a client that trusts the hint does not write into a
           connection that is closing. Without a hint, it is the idle timer. *)
        mutable between_requests_timer : Timeout.t option
      ; (* Body clock: ticks counted while a read waits in the [`Body] phase,
           and bytes read in that phase since the last restart. *)
        mutable tick_timer : Timeout.t option
      ; mutable ticks : int
      ; mutable body_progress : int
      ; mutable ticking : bool
      ; (* Write clock: runs while output waits to be written; restarts when all
           of it is written or after [progress_bytes]. *)
        mutable write_timer : Timeout.t option
      ; mutable written : int
      ; mutable write_clock : bool
      ; (* Made when the connection lingers, so that a keep-alive connection
           does not pay for it. *)
        mutable linger_timer : Timeout.t option
      ; mutable websocket : websocket option
      ; (* Resolves when the socket is closed and the timers are stopped. *)
        ended : unit Lwt.t
      ; notify_ended : unit Lwt.u
      ; mutable closing : bool
      }

    type connection =
      { connection : Ache.Server_connection.t
      ; timers : timers
      ; upgraded : unit -> bool
      ; is_websocket : unit -> bool
      }

    let websocket t =
      match t.websocket with
      | Some websocket -> websocket
      | None ->
        let websocket = { close_timer = None; close_expired = false } in
        t.websocket <- Some websocket;
        websocket

    let close_timer t =
      match t.websocket with
      | Some { close_timer; _ } -> close_timer
      | None -> None

    let stop_timers t =
      Option.iter Timeout.stop t.head_timer;
      Option.iter Timeout.stop (close_timer t);
      Option.iter Timeout.stop t.idle_timer;
      Option.iter Timeout.stop t.between_requests_timer;
      Option.iter Timeout.stop t.tick_timer;
      Option.iter Timeout.stop t.write_timer;
      Option.iter Timeout.stop t.linger_timer

    (* Closes the socket once, then stops the timers and resolves [ended]. *)
    let close t () =
      if not t.closing
      then (
        t.closing <- true;
        let closed () =
          stop_timers t;
          Lwt.wakeup_later t.notify_ended ()
        in
        match Io.close t.socket with
        | closing -> Lwt.on_termination closing closed
        | exception (_ : exn) -> closed ());
      t.ended

    let restart_body_clock t =
      t.ticks <- 0;
      t.body_progress <- 0

    (* The head timer runs only while it is armed. *)
    let stop_head_timer t =
      if t.head_armed
      then (
        t.head_armed <- false;
        Option.iter Timeout.stop t.head_timer)

    let phase_of = function
      | Some (((`Idle | `Head | `Body | `Close) as phase), _) ->
        (phase :> phase)
      | None -> `None

    let same_phase (a : phase) (b : phase) =
      match a, b with
      | `Idle, `Idle | `Head, `Head | `Body, `Body | `Close, `Close -> true
      | (`Idle | `Head | `Body | `Close | `None), _ -> false

    (* [armed] is the deadline of the last read that waited; it counts only
       while the read loop waits ([IO_loop.waiting]). Writing it only when it
       changes saves a [caml_modify] for each read. *)
    let arm t armed = if t.armed != armed then t.armed <- armed

    let is_armed t timer =
      match t.armed with
      | Some armed -> armed == timer && IO_loop.waiting t.reader
      | None -> false

    (* The deadline of the waiting read passed: run the action of [phase], if
       the connection is still in it, and end the read. The action stops the
       reads of the connection, so the read loop goes on to close. *)
    let rec expire c (phase : phase) =
      let t = c.timers in
      t.armed <- None;
      (match phase with
      | `Body -> restart_body_clock t
      | `Idle | `Head | `Close | `None -> ());
      let timeout = Ache.Server_connection.read_timeout c.connection in
      match timeout with
      | Some (now, action) when same_phase (now :> phase) phase ->
        (match phase with
        | `Close -> (websocket t).close_expired <- true
        | `Idle | `Head | `Body | `None -> ());
        (match action () with
        | () -> IO_loop.interrupt t.reader IO_loop.Interrupted
        | exception exn -> IO_loop.interrupt t.reader exn)
      | Some _ | None ->
        (match Ache.Server_connection.next_read_operation c.connection with
        | `Read ->
          (* The read waits in another phase: it takes that phase's deadline. *)
          start c timeout;
          wait c
        | `Yield | `Close -> IO_loop.interrupt t.reader IO_loop.Interrupted)

    and on_expire c phase timer = if is_armed c.timers timer then expire c phase

    (* One tick of the body clock. A tick that ends while no read waits in the
       [`Body] phase is not counted, and the clock stops until the next such
       read. *)
    and tick c timer =
      let t = c.timers in
      if is_armed t timer
      then (
        t.ticks <- t.ticks + 1;
        if t.ticks >= body_ticks
        then (
          t.ticking <- false;
          expire c `Body)
        else Timeout.set timer)
      else t.ticking <- false

    and head_timer c seconds =
      let t = c.timers in
      match t.head_timer with
      | Some timer -> timer
      | None ->
        let timer = Timeout.create_with seconds (on_expire c `Head) in
        t.head_timer <- Some timer;
        timer

    and idle_timer c =
      let t = c.timers in
      match t.idle_timer, t.idle_timeout with
      | (Some _ as timer), _ -> timer
      | None, None -> None
      | None, Some seconds ->
        let timer = Some (Timeout.create_with seconds (on_expire c `Idle)) in
        t.idle_timer <- timer;
        timer

    and between_requests_timer c =
      let t = c.timers in
      match t.between_requests_timer, t.idle_timeout with
      | (Some _ as timer), _ -> timer
      | None, Some seconds when t.keep_alive_hint ->
        let timer =
          Some (Timeout.create_with (seconds +. 1.) (on_expire c `Idle))
        in
        t.between_requests_timer <- timer;
        timer
      | None, (Some _ | None) -> idle_timer c

    and tick_timer c seconds =
      let t = c.timers in
      match t.tick_timer with
      | Some _ as timer -> timer
      | None ->
        let timer = Some (Timeout.create_with seconds (tick c)) in
        t.tick_timer <- timer;
        timer

    (* Arms the close timer when the server's close frame is queued. The frame
       wakes the writer, whoever queued it: [Ache_lwt.Websocket] or the callback
       API. A read that waits in the [`Idle] phase takes it as its deadline. *)
    and observe_close c timeout =
      let t = c.timers in
      match timeout, t.close_timeout with
      | Some (`Close, _), Some seconds
        when Option.is_none (close_timer t) && c.is_websocket () ->
        let websocket = websocket t in
        let timer = Timeout.create_with seconds (on_expire c `Close) in
        websocket.close_timer <- Some timer;
        Timeout.set timer;
        (match t.phase with
        | `Idle when IO_loop.waiting t.reader ->
          Option.iter Timeout.stop t.idle_timer;
          t.armed <- websocket.close_timer
        | `Idle | `Head | `Body | `Close | `None -> ())
      | Some ((`Idle | `Head | `Body | `Close), _), _ | None, _ -> ()

    (* A read starts in the phase of [timeout]: the head clock starts with the
       first read of the [`Head] phase and stops outside it. *)
    and start c timeout =
      let t = c.timers in
      (match t.head_timeout with
      | None -> ()
      | Some seconds ->
        (match timeout with
        | Some (`Head, _) ->
          if not t.head_armed
          then (
            t.head_armed <- true;
            Timeout.set (head_timer c seconds))
        | Some ((`Idle | `Body | `Close), _) | None -> stop_head_timer t));
      observe_close c timeout;
      t.phase <- phase_of timeout

    (* The read waits: arm the deadline of its phase. The head and close timers
       are not set again; when they already expired, so does the read. *)
    and wait c =
      let t = c.timers in
      match t.phase with
      | `Idle ->
        let armed =
          if c.upgraded () then idle_timer c else between_requests_timer c
        in
        Option.iter Timeout.set armed;
        arm t armed
      | `Head ->
        (match t.head_timer with
        | Some timer as armed when t.head_armed ->
          if Timeout.pending timer then arm t armed else expire c `Head
        | Some _ | None -> arm t None)
      | `Body ->
        (match t.tick_timeout with
        | Some seconds ->
          let armed = tick_timer c seconds in
          arm t armed;
          if not t.ticking
          then (
            t.ticking <- true;
            Option.iter Timeout.set armed)
        | None -> arm t None)
      | `Close ->
        (match close_timer t with
        | Some timer as armed ->
          if Timeout.pending timer then arm t armed else expire c `Close
        | None -> arm t None)
      | `None -> arm t None

    let write_timer t seconds =
      match t.write_timer with
      | Some timer -> timer
      | None ->
        let timer = Timeout.create seconds in
        t.write_timer <- Some timer;
        timer

    let observe_close_on_write c =
      match c.timers.close_timeout with
      | Some _ when Option.is_none (close_timer c.timers) && c.is_websocket ()
        ->
        observe_close c (Ache.Server_connection.read_timeout c.connection)
      | Some _ | None -> ()

    let timing c =
      { IO_loop.reader = c.timers.reader
      ; starts =
          (fun () -> start c (Ache.Server_connection.read_timeout c.connection))
      ; waits = (fun () -> wait c)
      ; returned =
          (fun n ->
            let t = c.timers in
            match t.phase, t.tick_timeout with
            | `Body, Some _ ->
              t.body_progress <- t.body_progress + n;
              if t.body_progress >= progress_bytes then restart_body_clock t
            | (`Idle | `Head | `Body | `Close | `None), _ -> ())
      }

    module Observed = struct
      type t = connection

      let next_read_operation c =
        Ache.Server_connection.next_read_operation c.connection

      let read c bs ~off ~len =
        Ache.Server_connection.read c.connection bs ~off ~len

      let read_eof c bs ~off ~len =
        Ache.Server_connection.read_eof c.connection bs ~off ~len

      let yield_reader c k = Ache.Server_connection.yield_reader c.connection k

      let next_write_operation c =
        let operation =
          Ache.Server_connection.next_write_operation c.connection
        in
        observe_close_on_write c;
        operation

      let report_write_result c result =
        Ache.Server_connection.report_write_result c.connection result

      let yield_writer c k = Ache.Server_connection.yield_writer c.connection k
      let report_exn c exn = Ache.Server_connection.report_exn c.connection exn
      let is_closed c = Ache.Server_connection.is_closed c.connection
      let shutdown c = Ache.Server_connection.shutdown c.connection
    end

    (* Lingering close: after the send side is shut, input is read and dropped
       for at most [idle_timeout] (or [linger_without_idle_timeout]) more. Not
       after the close timeout of a WebSocket: that client already had its
       time. *)
    let linger t () =
      match t.websocket with
      | Some { close_expired = true; _ } -> None
      | Some { close_expired = false; _ } | None ->
        Some
          (fun on_expire ->
            let timer =
              Timeout.create_with
                (Option.value
                   t.idle_timeout
                   ~default:linger_without_idle_timeout)
                (fun (_ : Timeout.t) -> on_expire ())
            in
            t.linger_timer <- Some timer;
            Timeout.set timer)

    let cancel c =
      (try Ache.Server_connection.shutdown c.connection with _ -> ());
      let (_ : unit Lwt.t) = close c.timers () in
      ()

    let stop_write_clock t () =
      if t.write_clock
      then (
        t.write_clock <- false;
        Option.iter Timeout.stop t.write_timer)

    (* The write clock starts when a write waits: a write that completes at once
       leaves it as it is. *)
    let writev c seconds iovecs =
      let t = c.timers in
      let writing = Io.writev t.socket iovecs in
      if not (Lwt.is_sleeping writing)
      then (
        (if t.write_clock
         then
           match Lwt.state writing with
           | Lwt.Return (`Ok n) ->
             t.written <- t.written + n;
             if t.written >= progress_bytes
             then (
               t.written <- 0;
               Option.iter Timeout.set t.write_timer)
           | Lwt.Return `Closed | Lwt.Fail _ | Lwt.Sleep -> ());
        writing)
      else
        let timer = write_timer t seconds in
        if not t.write_clock
        then (
          t.write_clock <- true;
          t.written <- 0;
          Timeout.set timer);
        Lwt.pick
          [ ( writing >|= fun result ->
              (match result with
              | `Ok n ->
                t.written <- t.written + n;
                if t.written >= progress_bytes
                then (
                  t.written <- 0;
                  Timeout.set timer)
              | `Closed -> ());
              result )
          ; ( Timeout.expired timer >|= fun () ->
              t.write_clock <- false;
              cancel c;
              `Closed )
          ]

    let drain c ~grace ~ended =
      Ache.Server_connection.drain c.connection;
      (* A drained idle connection reads nothing more. A drain ends the reads of
         a WebSocket at once (it sends 1001 and stops reading): only an idle
         read needs waking. *)
      (match c.timers.phase with
      | `Idle -> IO_loop.interrupt c.timers.reader IO_loop.Interrupted
      | `Head | `Body | `Close | `None -> ());
      match grace with
      | None -> ()
      | Some seconds ->
        let timer = Timeout.create seconds in
        Timeout.set timer;
        Lwt.on_termination
          (Lwt.choose [ Timeout.expired timer; ended ])
          (fun () ->
             Timeout.stop timer;
             if Lwt.is_sleeping ended then cancel c)

    let create_connection_handler
          ?(config = Ache.Config.default)
          ?(idle_timeout = Some 60.)
          ?(head_timeout = Some 30.)
          ?(body_timeout = Some 60.)
          ?(write_timeout = Some 60.)
          ?(close_timeout = Some 10.)
          ?(date = true)
          ?stop
          ?grace
          ~request_handler
          ~error_handler
          client_addr
          socket
      =
      check_timeout "idle_timeout" idle_timeout;
      check_timeout "head_timeout" head_timeout;
      check_timeout "body_timeout" body_timeout;
      check_timeout "write_timeout" write_timeout;
      check_timeout "close_timeout" close_timeout;
      check_timeout "grace" grace;
      match stop with
      | Some stop when not (Lwt.is_sleeping stop) ->
        IO_loop.close_once Io.close socket ()
      | _ ->
        let keep_alive_timeout =
          match idle_timeout with
          | Some seconds when seconds >= 1. -> Some (int_of_float seconds)
          | Some _ | None -> None
        in
        let ended, notify_ended = Lwt.wait () in
        let timers =
          { socket
          ; idle_timeout
          ; head_timeout
          ; tick_timeout =
              Option.map
                (fun seconds -> seconds /. float body_ticks)
                body_timeout
          ; close_timeout
          ; keep_alive_hint = Option.is_some keep_alive_timeout
          ; reader = IO_loop.reader ()
          ; phase = `None
          ; armed = None
          ; head_timer = None
          ; head_armed = false
          ; idle_timer = None
          ; between_requests_timer = None
          ; tick_timer = None
          ; ticks = 0
          ; body_progress = 0
          ; ticking = false
          ; write_timer = None
          ; written = 0
          ; write_clock = false
          ; linger_timer = None
          ; websocket = None
          ; ended
          ; notify_ended
          ; closing = false
          }
        in
        let restart_body_clock () = restart_body_clock timers in
        let stop_head_timer () = stop_head_timer timers in
        let connection =
          Ache.Server_connection.create
            ~config
            ~error_handler:(error_handler client_addr)
            ?date:(if date then Some current_date else None)
            ?keep_alive_timeout
            (fun reqd ->
               stop_head_timer ();
               restart_body_clock ();
               request_handler client_addr reqd)
        in
        let upgraded () =
          match Ache.Server_connection.upgrade_state connection with
          | `Websocket | `Other -> true
          | `None -> false
        and is_websocket () =
          match Ache.Server_connection.upgrade_state connection with
          | `Websocket -> true
          | `None | `Other -> false
        in
        let c = { connection; timers; upgraded; is_websocket } in
        IO_loop.start
          (module Io)
          (module Observed)
          c
          ~read_buffer_size:config.read_buffer_size
          ~timing:(timing c)
          ~close:(close timers)
          ~writer_waits:(stop_write_clock timers)
          ~linger:(linger timers)
          ?writev:(Option.map (writev c) write_timeout)
          socket;
        (* [Lwt.choose] removes its callback from [stop] when [ended] resolves
           first, so a long-lived [stop] does not keep one callback for each
           connection that ended. *)
        Option.iter
          (fun stop ->
             Lwt.on_termination
               (Lwt.choose [ stop; ended ])
               (fun () -> if Lwt.is_sleeping ended then drain c ~grace ~ended))
          stop;
        ended
  end

  module Client = struct
    type socket = Io.socket

    type t =
      { connection : Ache.Client_connection.t
      ; runtime : Upgradable.t
      ; close : unit -> unit Lwt.t
      ; socket : socket
      ; unread : bool ref
      ; reuse : reuse
      }

    (* The deadline after which the connection is not reused, from the
       [Keep-Alive] hint of the last response. *)
    and reuse =
      { mutable deadline : float option
      ; mutable timer : Timeout.t option
      ; mutable upgraded : bool
      }

    let shutdown_runtime runtime close =
      Upgradable.shutdown runtime;
      close ()

    let arm reuse connection runtime close =
      if (not reuse.upgraded) && Option.is_none reuse.deadline
      then
        match Ache.Client_connection.reuse_timeout connection with
        | None -> ()
        | Some seconds when not (seconds > 0.) ->
          reuse.deadline <- Some (Unix.gettimeofday ());
          Lwt.async (fun () -> shutdown_runtime runtime close)
        | Some seconds ->
          reuse.deadline <- Some (Unix.gettimeofday () +. seconds);
          (* [seconds] is a whole number of at most 598: at most 598 queues. *)
          let timer = Timeout.create seconds in
          reuse.timer <- Some timer;
          Timeout.set timer;
          Lwt.on_success (Timeout.expired timer) (fun () ->
            match reuse.timer with
            | Some armed when armed == timer ->
              reuse.timer <- None;
              if
                Option.is_some (Ache.Client_connection.reuse_timeout connection)
              then Lwt.async (fun () -> shutdown_runtime runtime close)
            | Some _ | None -> ())

    let create_connection ?(config = Ache.Config.default) socket =
      let connection = Ache.Client_connection.create ~config () in
      let runtime =
        Upgradable.create
          (Ache.Runtime.make (module Ache.Client_connection) connection)
      in
      let close = IO_loop.close_once Io.close socket in
      (* [unread]: a read returned response bytes that the connection has not
         been given yet: the read loop can pause between the two. *)
      let unread = ref false in
      let module Io = struct
        include Io

        let read socket buf ~off ~len =
          Io.read socket buf ~off ~len >|= fun n ->
          if Idle_input.has_response_byte buf off (off + n) then unread := true;
          n
      end
      in
      let reuse = { deadline = None; timer = None; upgraded = false } in
      let module Observed = struct
        include Upgradable

        let next_read_operation t =
          let operation = next_read_operation t in
          arm reuse connection runtime close;
          operation

        let read t bs ~off ~len =
          unread := false;
          let n = read t bs ~off ~len in
          arm reuse connection runtime close;
          n

        let read_eof t bs ~off ~len =
          unread := false;
          let n = read_eof t bs ~off ~len in
          arm reuse connection runtime close;
          n
      end
      in
      IO_loop.start
        (module Io)
        (module Observed)
        runtime
        ~read_buffer_size:config.read_buffer_size
        ~close:(fun () ->
          let closing = close () in
          Lwt.on_termination closing (fun () ->
            Option.iter Timeout.stop reuse.timer;
            reuse.timer <- None);
          closing)
        socket;
      Lwt.return { connection; runtime; close; socket; unread; reuse }

    let request
          t
          ?flush_headers_immediately
          request
          ~error_handler
          ~response_handler
      =
      (* Past the [Keep-Alive] deadline, the request gets [`Connection_closed]
         and nothing is written. *)
      (match t.reuse.deadline with
      | Some deadline when Unix.gettimeofday () >= deadline ->
        Upgradable.shutdown t.runtime;
        Lwt.async t.close
      | Some _ | None -> ());
      t.reuse.deadline <- None;
      Option.iter Timeout.stop t.reuse.timer;
      t.reuse.timer <- None;
      (* Response bytes that wait on an idle connection would be taken as the
         response to this request: close the connection instead. *)
      if
        Ache.Ache_private.Client_connection.is_waiting t.connection
        && (!(t.unread) || Idle.input_waiting t.socket)
      then Upgradable.shutdown t.runtime;
      Ache.Client_connection.request
        t.connection
        ?flush_headers_immediately
        request
        ~error_handler
        ~response_handler

    let shutdown t = shutdown_runtime t.runtime t.close
    let is_closed t = Upgradable.is_closed t.runtime

    let upgrade t impl =
      t.reuse.upgraded <- true;
      t.reuse.deadline <- None;
      Option.iter Timeout.stop t.reuse.timer;
      t.reuse.timer <- None;
      Upgradable.upgrade t.runtime impl
  end
end

module Io = struct
  type socket = Lwt_unix.file_descr
  type addr = Unix.sockaddr

  let close socket =
    match Lwt_unix.state socket with
    | Closed -> Lwt.return_unit
    | _ ->
      (try Lwt_unix.shutdown socket SHUTDOWN_ALL with Unix.Unix_error _ -> ());
      Lwt.catch (fun () -> Lwt_unix.close socket) (fun _exn -> Lwt.return_unit)

  let read socket bigstring ~off ~len =
    Lwt.catch
      (fun () -> Lwt_bytes.read socket bigstring off len)
      (function
        | Unix.Unix_error ((Unix.EBADF | ECONNRESET | ENOTCONN), _, _) ->
          Lwt.return 0
        | exn -> Lwt.fail exn)
    >>= function
    | 0 -> Lwt.fail End_of_file
    | n -> Lwt.return n

  let writev socket iovecs =
    let lwt_iovecs = Lwt_unix.IO_vectors.create () in
    List.iter
      (fun { Ache.IOVec.buffer; off; len } ->
         Lwt_unix.IO_vectors.append_bigarray lwt_iovecs buffer off len)
      iovecs;
    Lwt.catch
      (fun () -> Lwt_unix.writev socket lwt_iovecs >|= fun n -> `Ok n)
      (function
        | Unix.Unix_error (Unix.EBADF, "check_descriptor", _)
        | Unix.Unix_error ((Unix.EPIPE | ECONNRESET | ENOTCONN), _, _) ->
          Lwt.return `Closed
        | exn -> Lwt.fail exn)

  let shutdown_receive socket =
    if Lwt_unix.state socket <> Lwt_unix.Closed
    then
      try Lwt_unix.shutdown socket Unix.SHUTDOWN_RECEIVE with
      | Unix.Unix_error (Unix.ENOTCONN, _, _) -> ()

  let shutdown_send socket =
    if Lwt_unix.state socket <> Lwt_unix.Closed
    then
      try Lwt_unix.shutdown socket Unix.SHUTDOWN_SEND with
      | Unix.Unix_error _ -> ()
end

module Make (Io : IO) =
  Make_with_idle_check
    (Io)
    (struct
      let input_waiting _ = false
    end)

include
  Make_with_idle_check
    (Io)
    (struct
      (* A blocking descriptor may be read by a Lwt thread at the same time. *)
      let input_waiting socket =
        Lwt_unix.state socket = Lwt_unix.Opened
        && (match Lwt.state (Lwt_unix.blocking socket) with
          | Lwt.Return false -> true
          | Lwt.Return true | Lwt.Fail _ | Lwt.Sleep -> false)
        && Idle_input.waiting (Lwt_unix.unix_file_descr socket)
    end)
