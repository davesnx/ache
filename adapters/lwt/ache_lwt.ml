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

  let untimed_read read = read () >|= Result.ok

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
    -> timed_read:((unit -> int Lwt.t) -> (int, [ `Timeout ]) result Lwt.t)
    -> close:(unit -> unit Lwt.t)
    -> ?writer_waits:(unit -> unit)
    -> ?linger:(unit -> (unit -> unit Lwt.t) option)
    -> ?writev:
         (Ache.Bigstring.t Ache.IOVec.t list -> [ `Closed | `Ok of int ] Lwt.t)
    -> fd
    -> unit Lwt.t
    =
   fun (module Io)
     (module Runtime)
     t
     ~read_buffer_size
     ~timed_read
     ~close
     ?(writer_waits = ignore)
     ?linger
     ?writev
     socket ->
    let read_buffer = Buffer.create read_buffer_size in
    (* [eof]: a read saw the end of input, so a linger would read nothing. *)
    let eof = ref false
    and send_shut, notify_send_shut = Lwt.wait ()
    (* The last read on the socket. A timed read that timed out leaves it
       pending: a linger waits for it rather than read the socket twice. *)
    and last_read = ref (Lwt.return 0) in
    let failed, notify_failed = Lwt.wait () in
    let fail exn =
      (try Runtime.report_exn t exn with _ -> ());
      if Lwt.is_sleeping failed then Lwt.wakeup_later notify_failed ();
      Lwt.return_unit
    in
    let read_loop_exited, notify_read_loop_exited = Lwt.wait () in
    let read_loop_done () =
      if Lwt.is_sleeping read_loop_exited
      then Lwt.wakeup_later notify_read_loop_exited ();
      (try Io.shutdown_receive socket with _ -> ());
      Lwt.return_unit
    in
    let read_failed exn =
      match Runtime.report_exn t exn with
      | () -> read_loop_done ()
      | exception _ ->
        if Lwt.is_sleeping failed then Lwt.wakeup_later notify_failed ();
        Lwt.return_unit
    in
    let rec read_loop () =
      let rec read_loop_step () =
        match Runtime.next_read_operation t with
        | `Read ->
          Lwt.catch
            (fun () ->
               (if Buffer.unread read_buffer
                then Lwt.return (Ok 0)
                else
                  let reading =
                    timed_read (fun () ->
                      let reading = read (module Io) socket read_buffer in
                      if Option.is_some linger then last_read := reading;
                      reading)
                  in
                  if Lwt.is_sleeping reading
                  then reading
                  else
                    reading >>= fun result ->
                    Lwt.pause () >|= fun () -> result)
               >|= function
               | Ok (_ : int) ->
                 let (_ : int) = Buffer.get read_buffer ~f:(Runtime.read t) in
                 ()
               | Error `Timeout -> ())
            (function
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
            | Some deadline -> lingering deadline >>= read_loop_done
            | None -> read_loop_done ())
          | Some _ | None -> read_loop_done ())
      (* Lingering close (RFC 9112 9.6): reads and drops input until the end of
         input, or until the send side is shut and [deadline ()] resolves, so
         that the close does not reset the connection under unread input and the
         peer can read the last response. *)
      and lingering deadline =
        let drop () =
          let (_ : int) =
            Buffer.get read_buffer ~f:(fun _ ~off:_ ~len -> len)
          in
          ()
        in
        let rec drain reading =
          (if Lwt.is_sleeping reading
           then reading >|= ignore
           else reading >>= fun (_ : int) -> Lwt.pause ())
          >>= fun () ->
          drop ();
          drain (read (module Io) socket read_buffer)
        in
        drop ();
        let reading =
          if Lwt.is_sleeping !last_read
          then !last_read
          else read (module Io) socket read_buffer
        in
        Lwt.pick
          [ Lwt.catch
              (fun () -> drain reading)
              (fun (_ : exn) -> Lwt.return_unit)
          ; send_shut >>= deadline
          ]
        >|= drop
      in
      Lwt.async (fun () -> Lwt.catch read_loop_step read_failed)
    in
    let writev = Option.value writev ~default:(Io.writev socket) in
    let write_loop_exited, notify_write_loop_exited = Lwt.wait () in
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
            Lwt.wakeup_later notify_send_shut ());
          Lwt.wakeup_later notify_write_loop_exited ();
          Lwt.return_unit
      in
      Lwt.async (fun () -> Lwt.catch write_loop_step fail)
    in
    read_loop ();
    write_loop ();
    Lwt.choose [ Lwt.join [ read_loop_exited; write_loop_exited ]; failed ]
    >>= close
end

module Timeout = struct
  type t =
    { queue : queue
    ; mutable at : float
    ; mutable prev : t
    ; mutable next : t
    ; mutable expired : unit Lwt.t * unit Lwt.u
    }

  and queue =
    { seconds : float
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

  let find_queue seconds =
    match !last_queue with
    | Some queue when Float.equal queue.seconds seconds -> queue
    | Some _ | None ->
      let queue =
        match Queues.find_opt queues seconds with
        | Some queue -> queue
        | None ->
          let rec queue = { seconds; root; sleeping = false }
          and root =
            { queue
            ; at = infinity
            ; prev = root
            ; next = root
            ; expired = Lwt.wait ()
            }
          in
          Queues.add queues seconds queue;
          queue
      in
      last_queue := Some queue;
      queue

  let create seconds =
    if not (seconds > 0.)
    then invalid_arg "Ache_lwt.Timeout.create: seconds must be positive";
    let queue = find_queue seconds in
    let rec t = { queue; at = 0.; prev = t; next = t; expired = Lwt.wait () } in
    t

  let stop t =
    t.prev.next <- t.next;
    t.next.prev <- t.prev;
    t.prev <- t;
    t.next <- t

  let rec run queue () =
    let now = Unix.gettimeofday () in
    let rec expire () =
      let t = queue.root.next in
      if t != queue.root && t.at <= now
      then (
        stop t;
        Lwt.wakeup_later (snd t.expired) ();
        expire ())
    in
    expire ();
    let front = queue.root.next in
    if front == queue.root
    then queue.sleeping <- false
    else Lwt.on_success (Lwt_unix.sleep (front.at -. now)) (run queue)

  let set t =
    let queue = t.queue in
    stop t;
    if not (Lwt.is_sleeping (fst t.expired)) then t.expired <- Lwt.wait ();
    t.at <- Unix.gettimeofday () +. queue.seconds;
    t.prev <- queue.root.prev;
    t.next <- queue.root;
    queue.root.prev.next <- t;
    queue.root.prev <- t;
    if not queue.sleeping
    then (
      queue.sleeping <- true;
      Lwt.on_success (Lwt_unix.sleep queue.seconds) (run queue))

  let expired t = Lwt.protected (fst t.expired)
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
      ; (* The WebSocket read that waits in the [`Idle] phase. When the server
           queues its close frame meanwhile, that read stays (the socket read
           cannot be cancelled), and its limit becomes the close timeout. *)
        mutable closing_wait : Timeout.t Lwt.u option
      }

    (* The timeouts of a connection and their state. Each timer is made on its
       first [set], so that a connection pays only for the timers it uses. *)
    type timers =
      { socket : socket
      ; close : unit -> unit Lwt.t
      ; idle_timeout : float option
      ; head_timeout : float option
      ; (* The length of a body clock tick. *)
        tick_timeout : float option
      ; close_timeout : float option
      ; (* Responses get a [Keep-Alive] hint. *)
        keep_alive_hint : bool
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
      ; mutable body_wait : unit Lwt.u option
      ; (* Write clock: runs while output waits to be written; restarts when all
           of it is written or after [progress_bytes]. *)
        mutable write_timer : Timeout.t option
      ; mutable written : int
      ; mutable write_clock : bool
      ; (* Made when the connection lingers, so that a keep-alive connection
           does not pay for it. *)
        mutable linger_timer : Timeout.t option
      ; (* The idle read in progress, which [drain] ends: a drained idle
           connection reads nothing more. *)
        mutable idle_read :
          ((int, [ `Timeout ]) result Lwt.t * (int, [ `Timeout ]) result Lwt.u)
            option
      ; mutable websocket : websocket option
      ; cancelled : unit Lwt.t
      ; notify_cancelled : unit Lwt.u
      }

    type connection =
      { connection : Ache.Server_connection.t
      ; timers : timers
      ; upgraded : unit -> bool
      ; is_websocket : unit -> bool
      }

    let head_timer t seconds =
      match t.head_timer with
      | Some timer -> timer
      | None ->
        let timer = Timeout.create seconds in
        t.head_timer <- Some timer;
        timer

    let idle_timer t =
      match t.idle_timer, t.idle_timeout with
      | (Some _ as timer), _ -> timer
      | None, None -> None
      | None, Some seconds ->
        let timer = Some (Timeout.create seconds) in
        t.idle_timer <- timer;
        timer

    let between_requests_timer t =
      match t.between_requests_timer, t.idle_timeout with
      | (Some _ as timer), _ -> timer
      | None, Some seconds when t.keep_alive_hint ->
        let timer = Some (Timeout.create (seconds +. 1.)) in
        t.between_requests_timer <- timer;
        timer
      | None, (Some _ | None) -> idle_timer t

    let tick_timer t seconds =
      match t.tick_timer with
      | Some timer -> timer
      | None ->
        let timer = Timeout.create seconds in
        t.tick_timer <- Some timer;
        timer

    let write_timer t seconds =
      match t.write_timer with
      | Some timer -> timer
      | None ->
        let timer = Timeout.create seconds in
        t.write_timer <- Some timer;
        timer

    let websocket t =
      match t.websocket with
      | Some websocket -> websocket
      | None ->
        let websocket =
          { close_timer = None; close_expired = false; closing_wait = None }
        in
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

    let restart_body_clock t =
      t.ticks <- 0;
      t.body_progress <- 0

    (* The head timer runs only while it is armed. *)
    let stop_head_timer t =
      if t.head_armed
      then (
        t.head_armed <- false;
        Option.iter Timeout.stop t.head_timer)

    (* Arms the close timer when the server's close frame is queued. The frame
       wakes the writer, whoever queued it: [Ache_lwt.Websocket] or the callback
       API. *)
    let observe_close c =
      match c.timers.close_timeout with
      | Some seconds
        when Option.is_none (close_timer c.timers) && c.is_websocket () ->
        (match Ache.Server_connection.read_timeout c.connection with
        | Some (`Close, _) ->
          let websocket = websocket c.timers in
          let timer = Timeout.create seconds in
          websocket.close_timer <- Some timer;
          Timeout.set timer;
          (match websocket.closing_wait with
          | Some u ->
            websocket.closing_wait <- None;
            Lwt.wakeup_later u timer
          | None -> ())
        | Some ((`Idle | `Head | `Body), _) | None -> ())
      | Some _ | None -> ()

    let observe c =
      (match c.timers.head_timeout with
      | None -> ()
      | Some seconds ->
        (match Ache.Server_connection.read_timeout c.connection with
        | Some (`Head, _) ->
          if not c.timers.head_armed
          then (
            c.timers.head_armed <- true;
            Timeout.set (head_timer c.timers seconds))
        | Some ((`Idle | `Body | `Close), _) | None -> stop_head_timer c.timers));
      observe_close c

    module Observed = struct
      type t = connection

      let next_read_operation c =
        Ache.Server_connection.next_read_operation c.connection

      let read c bs ~off ~len =
        let n = Ache.Server_connection.read c.connection bs ~off ~len in
        observe c;
        n

      let read_eof c bs ~off ~len =
        let n = Ache.Server_connection.read_eof c.connection bs ~off ~len in
        observe c;
        n

      let yield_reader c k = Ache.Server_connection.yield_reader c.connection k

      let next_write_operation c =
        let operation =
          Ache.Server_connection.next_write_operation c.connection
        in
        observe_close c;
        operation

      let report_write_result c result =
        Ache.Server_connection.report_write_result c.connection result

      let yield_writer c k = Ache.Server_connection.yield_writer c.connection k
      let report_exn c exn = Ache.Server_connection.report_exn c.connection exn
      let is_closed c = Ache.Server_connection.is_closed c.connection
      let shutdown c = Ache.Server_connection.shutdown c.connection
    end

    let expire timer on_timeout =
      Timeout.expired timer >|= fun () ->
      on_timeout ();
      Error `Timeout

    (* One tick of the body clock. A tick that ends while no read waits in the
       [`Body] phase is not counted, and the clock stops until the next such
       read. *)
    let rec tick t timer () =
      match t.body_wait with
      | Some u ->
        t.ticks <- t.ticks + 1;
        if t.ticks >= body_ticks
        then (
          t.ticking <- false;
          t.body_wait <- None;
          Lwt.wakeup_later u ())
        else (
          Timeout.set timer;
          Lwt.on_success (Timeout.expired timer) (tick t timer))
      | None -> t.ticking <- false

    let body_limit c seconds =
      let t = c.timers in
      let p, u = Lwt.wait () in
      t.body_wait <- Some u;
      if not t.ticking
      then (
        t.ticking <- true;
        let timer = tick_timer t seconds in
        Timeout.set timer;
        Lwt.on_success (Timeout.expired timer) (tick t timer));
      p >|= fun () ->
      restart_body_clock t;
      (* The action depends on the state when the clock runs out. *)
      (match Ache.Server_connection.read_timeout c.connection with
      | Some (`Body, expire) -> expire ()
      | Some ((`Idle | `Head | `Close), _) | None -> ());
      Error `Timeout

    let close_limit c timer =
      Timeout.expired timer >|= fun () ->
      (* The client may have answered since the read started. *)
      (match Ache.Server_connection.read_timeout c.connection with
      | Some (`Close, abort) ->
        (websocket c.timers).close_expired <- true;
        abort ()
      | Some ((`Idle | `Head | `Body), _) | None -> ());
      Error `Timeout

    (* Lingering close: after the send side is shut, input is read and dropped
       for at most [idle_timeout] (or [linger_without_idle_timeout]) more. Not
       after the close timeout of a WebSocket: that client already had its
       time. *)
    let linger t () =
      match t.websocket with
      | Some { close_expired = true; _ } -> None
      | Some { close_expired = false; _ } | None ->
        Some
          (fun () ->
            let timer =
              Timeout.create
                (Option.value
                   t.idle_timeout
                   ~default:linger_without_idle_timeout)
            in
            t.linger_timer <- Some timer;
            Timeout.set timer;
            Timeout.expired timer)

    let drainable t =
      let p, u = Lwt.wait () in
      t.idle_read <- Some (p, u);
      p

    let closing_limit c =
      let t = c.timers in
      let p, u = Lwt.wait () in
      (websocket t).closing_wait <- Some u;
      p >>= fun timer ->
      Option.iter Timeout.stop t.idle_timer;
      close_limit c timer

    let closing_wait t =
      match t.websocket with
      | Some { closing_wait; _ } -> closing_wait
      | None -> None

    let timed_read c ~stop read =
      let t = c.timers in
      observe c;
      let reading = IO_loop.untimed_read read in
      let phase = Ache.Server_connection.read_timeout c.connection in
      let counted =
        match phase, t.tick_timeout with
        | Some (`Body, _), Some _ ->
          reading >|= fun result ->
          (match result with
          | Ok n ->
            t.body_progress <- t.body_progress + n;
            if t.body_progress >= progress_bytes then restart_body_clock t
          | Error `Timeout -> ());
          result
        | (Some ((`Idle | `Head | `Body | `Close), _) | None), _ -> reading
      in
      let limits =
        if not (Lwt.is_sleeping reading)
        then []
        else
          match phase with
          | Some (`Idle, on_timeout) ->
            (match
               if c.upgraded () then idle_timer t else between_requests_timer t
             with
            | Some timer ->
              Timeout.set timer;
              [ expire timer on_timeout ]
            | None -> [])
          | Some (`Head, on_timeout) when t.head_armed ->
            Option.to_list
              (Option.map (fun timer -> expire timer on_timeout) t.head_timer)
          | Some (`Head, _) -> []
          | Some (`Body, _) ->
            Option.to_list (Option.map (body_limit c) t.tick_timeout)
          | Some (`Close, _) ->
            Option.to_list (Option.map (close_limit c) (close_timer t))
          | None -> []
      in
      let limits =
        match phase, t.close_timeout with
        | Some (`Idle, _), Some _
          when Lwt.is_sleeping reading && c.is_websocket () ->
          closing_limit c :: limits
        | (Some _ | None), (Some _ | None) -> limits
      in
      let limits =
        match stop, phase with
        | Some _, Some (`Idle, _) when Lwt.is_sleeping reading ->
          drainable t :: limits
        (* A drain ends the reads of a WebSocket at once (it sends 1001 and
           stops reading): only an idle read needs waking. *)
        | (Some _ | None), (Some ((`Idle | `Head | `Body | `Close), _) | None)
          ->
          limits
      in
      match limits with
      | [] -> counted
      | limits ->
        let waiting = t.body_wait
        and closing_waiting = closing_wait t in
        let result = Lwt.pick (counted :: limits) in
        (* The next read can start before this callback runs. *)
        Lwt.on_termination result (fun () ->
          (match t.body_wait, waiting with
          | Some u, Some mine when u == mine -> t.body_wait <- None
          | _ -> ());
          match t.websocket, closing_waiting with
          | Some ({ closing_wait = Some u; _ } as websocket), Some mine
            when u == mine ->
            websocket.closing_wait <- None
          | _ -> ());
        result

    let cancel c =
      (try Ache.Server_connection.shutdown c.connection with _ -> ());
      Lwt.on_termination (c.timers.close ()) (fun () ->
        if Lwt.is_sleeping c.timers.cancelled
        then Lwt.wakeup_later c.timers.notify_cancelled ())

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
      (match c.timers.idle_read with
      | Some (p, u) when Lwt.is_sleeping p ->
        Lwt.wakeup_later u (Error `Timeout)
      | Some _ | None -> ());
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
      let close = IO_loop.close_once Io.close socket in
      match stop with
      | Some stop when not (Lwt.is_sleeping stop) -> close ()
      | _ ->
        let keep_alive_timeout =
          match idle_timeout with
          | Some seconds when seconds >= 1. -> Some (int_of_float seconds)
          | Some _ | None -> None
        in
        let cancelled, notify_cancelled = Lwt.wait () in
        let timers =
          { socket
          ; close
          ; idle_timeout
          ; head_timeout
          ; tick_timeout =
              Option.map
                (fun seconds -> seconds /. float body_ticks)
                body_timeout
          ; close_timeout
          ; keep_alive_hint = Option.is_some keep_alive_timeout
          ; head_timer = None
          ; head_armed = false
          ; idle_timer = None
          ; between_requests_timer = None
          ; tick_timer = None
          ; ticks = 0
          ; body_progress = 0
          ; ticking = false
          ; body_wait = None
          ; write_timer = None
          ; written = 0
          ; write_clock = false
          ; linger_timer = None
          ; idle_read = None
          ; websocket = None
          ; cancelled
          ; notify_cancelled
          }
        in
        let restart_body_clock () = restart_body_clock timers
        and last_reqd = ref None in
        let upgraded () =
          match Option.bind !last_reqd Ache.Request_descriptor.response with
          | Some { Ache.Response.status = `Switching_protocols; _ } -> true
          | Some _ | None -> false
        in
        (* [true] once the connection is upgraded with [Ache.Websocket.upgrade],
           whose [101] carries [Upgrade: websocket]. The answer is cached once
           there is a [101]: other protocols never reach the [`Close] phase. *)
        let websocket = ref false
        and checked_reqd = ref None in
        let is_websocket () =
          !websocket
          ||
          match !last_reqd with
          | Some reqd
            when match !checked_reqd with
                 | Some checked -> checked != reqd
                 | None -> true ->
            (* Each request's response is checked once it is known. *)
            (match Ache.Request_descriptor.response reqd with
            | Some { Ache.Response.status; headers; _ } ->
              checked_reqd := Some reqd;
              (websocket :=
                 status = `Switching_protocols
                 &&
                 match Ache.Headers.get headers "upgrade" with
                 | Some value -> String.lowercase_ascii value = "websocket"
                 | None -> false);
              !websocket
            | None -> false)
          | Some _ | None -> false
        in
        let stop_head_timer () = stop_head_timer timers in
        let connection =
          Ache.Server_connection.create
            ~config
            ~error_handler:(error_handler client_addr)
            ?date:(if date then Some current_date else None)
            ?keep_alive_timeout
            (fun reqd ->
               last_reqd := Some reqd;
               stop_head_timer ();
               restart_body_clock ();
               request_handler client_addr reqd)
        in
        let c = { connection; timers; upgraded; is_websocket } in
        let serving =
          IO_loop.start
            (module Io)
            (module Observed)
            c
            ~read_buffer_size:config.read_buffer_size
            ~timed_read:(timed_read c ~stop)
            ~close
            ~writer_waits:(stop_write_clock timers)
            ~linger:(linger timers)
            ?writev:(Option.map (writev c) write_timeout)
            socket
        in
        let ended =
          Lwt.finalize
            (fun () -> Lwt.choose [ serving; cancelled ])
            (fun () ->
               stop_timers timers;
               Lwt.return_unit)
        in
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
      Lwt.async (fun () ->
        Lwt.finalize
          (fun () ->
             IO_loop.start
               (module Io)
               (module Observed)
               runtime
               ~read_buffer_size:config.read_buffer_size
               ~timed_read:IO_loop.untimed_read
               ~close
               socket)
          (fun () ->
             Option.iter Timeout.stop reuse.timer;
             reuse.timer <- None;
             Lwt.return_unit));
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
