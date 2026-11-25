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
     ?writev
     socket ->
    let read_buffer = Buffer.create read_buffer_size in
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
                    timed_read (fun () -> read (module Io) socket read_buffer)
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
                let (_ : int) =
                  Buffer.get read_buffer ~f:(Runtime.read_eof t)
                in
                Lwt.return_unit
              | exn -> Lwt.fail exn)
          >>= read_loop_step
        | `Yield ->
          Runtime.yield_reader t read_loop;
          Lwt.return_unit
        | `Close -> read_loop_done ()
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

  let queues : (float, queue) Hashtbl.t = Hashtbl.create 2

  let create seconds =
    if not (seconds > 0.)
    then invalid_arg "Ache_lwt.Timeout.create: seconds must be positive";
    let queue =
      match Hashtbl.find_opt queues seconds with
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
        Hashtbl.add queues seconds queue;
        queue
    in
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

  let create ?max_message_size reqd =
    let messages = Queue.create ()
    and receivers = Queue.create ()
    and pingers = Queue.create ()
    and ended = ref false
    and client_close = ref None in
    let closed, notify_closed = Lwt.wait () in
    let connection = ref None in
    match
      Ache.Websocket.upgrade ?max_message_size reqd (fun ws ->
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

  let upgrade ?max_message_size reqd session =
    match create ?max_message_size reqd with
    | Error _ as error -> error
    | Ok t ->
      Lwt.async (fun () ->
        Lwt.catch
          (fun () -> session t)
          (function
            | Closed -> Lwt.return_unit
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

module Make (Io : IO) = struct
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

    let create_connection_handler
          ?(config = Ache.Config.default)
          ?(idle_timeout = Some 60.)
          ?(head_timeout = Some 30.)
          ?(body_timeout = Some 60.)
          ?(write_timeout = Some 60.)
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
      check_timeout "grace" grace;
      let close = IO_loop.close_once Io.close socket in
      match stop with
      | Some stop when not (Lwt.is_sleeping stop) -> close ()
      | _ ->
        let head_timer = Option.map Timeout.create head_timeout
        and head_armed = ref false
        and idle_timer = Option.map Timeout.create idle_timeout
        and keep_alive_timeout =
          match idle_timeout with
          | Some seconds when seconds >= 1. -> Some (int_of_float seconds)
          | Some _ | None -> None
        and tick_timer =
          Option.map
            (fun seconds -> Timeout.create (seconds /. float body_ticks))
            body_timeout
        and write_timer = Option.map Timeout.create write_timeout in
        (* Body clock: ticks counted while a read waits in the [`Body] phase,
           and bytes read in that phase since the last restart. *)
        let ticks = ref 0
        and body_progress = ref 0
        and ticking = ref false
        and body_wait = ref None in
        let restart_body_clock () =
          ticks := 0;
          body_progress := 0
        in
        (* Between requests the idle timer runs 1 s longer than the [Keep-Alive]
           hint, so that a client that trusts the hint does not write into a
           connection that is closing. *)
        let between_requests_timer =
          match keep_alive_timeout, idle_timeout with
          | Some _, Some seconds -> Some (Timeout.create (seconds +. 1.))
          | None, _ -> idle_timer
          | Some _, None -> None
        and last_reqd = ref None in
        let upgraded () =
          match Option.bind !last_reqd Ache.Request_descriptor.response with
          | Some { Ache.Response.status = `Switching_protocols; _ } -> true
          | Some _ | None -> false
        in
        let stop_head_timer () =
          head_armed := false;
          Option.iter Timeout.stop head_timer
        in
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
        let observe () =
          match head_timer with
          | None -> ()
          | Some timer ->
            (match Ache.Server_connection.read_timeout connection with
            | Some (`Head, _) ->
              if not !head_armed
              then (
                head_armed := true;
                Timeout.set timer)
            | Some ((`Idle | `Body), _) | None -> stop_head_timer ())
        in
        let module Observed = struct
          include Ache.Server_connection

          let read t bs ~off ~len =
            let n = read t bs ~off ~len in
            observe ();
            n

          let read_eof t bs ~off ~len =
            let n = read_eof t bs ~off ~len in
            observe ();
            n
        end
        in
        let expire timer on_timeout =
          Timeout.expired timer >|= fun () ->
          on_timeout ();
          Error `Timeout
        in
        (* One tick of the body clock. A tick that ends while no read waits in
           the [`Body] phase is not counted, and the clock stops until the next
           such read. *)
        let rec tick timer () =
          match !body_wait with
          | Some u ->
            incr ticks;
            if !ticks >= body_ticks
            then (
              ticking := false;
              body_wait := None;
              Lwt.wakeup_later u ())
            else (
              Timeout.set timer;
              Lwt.on_success (Timeout.expired timer) (tick timer))
          | None -> ticking := false
        in
        let body_limit timer =
          let p, u = Lwt.wait () in
          body_wait := Some u;
          if not !ticking
          then (
            ticking := true;
            Timeout.set timer;
            Lwt.on_success (Timeout.expired timer) (tick timer));
          p >|= fun () ->
          restart_body_clock ();
          (* The action depends on the state when the clock runs out. *)
          (match Ache.Server_connection.read_timeout connection with
          | Some (`Body, expire) -> expire ()
          | Some ((`Idle | `Head), _) | None -> ());
          Error `Timeout
        in
        (* The idle read in progress, which [drain] ends: a drained idle
           connection reads nothing more. *)
        let idle_read = ref None in
        let drainable () =
          let p, u = Lwt.wait () in
          idle_read := Some (p, u);
          p
        in
        let timed_read read =
          observe ();
          let reading = IO_loop.untimed_read read in
          let phase = Ache.Server_connection.read_timeout connection in
          let counted =
            match phase, tick_timer with
            | Some (`Body, _), Some _ ->
              reading >|= fun result ->
              (match result with
              | Ok n ->
                body_progress := !body_progress + n;
                if !body_progress >= progress_bytes then restart_body_clock ()
              | Error `Timeout -> ());
              result
            | _ -> reading
          in
          let limits =
            if not (Lwt.is_sleeping reading)
            then []
            else
              match phase with
              | Some (`Idle, on_timeout) ->
                (match
                   if upgraded () then idle_timer else between_requests_timer
                 with
                | Some timer ->
                  Timeout.set timer;
                  [ expire timer on_timeout ]
                | None -> [])
              | Some (`Head, on_timeout) when !head_armed ->
                Option.to_list
                  (Option.map (fun timer -> expire timer on_timeout) head_timer)
              | Some (`Head, _) -> []
              | Some (`Body, _) ->
                Option.to_list (Option.map body_limit tick_timer)
              | None -> []
          in
          let limits =
            match stop, phase with
            | Some _, Some (`Idle, _) when Lwt.is_sleeping reading ->
              drainable () :: limits
            | _ -> limits
          in
          match limits with
          | [] -> counted
          | limits ->
            let waiting = !body_wait in
            let result = Lwt.pick (counted :: limits) in
            (* The next read can start before this callback runs. *)
            Lwt.on_termination result (fun () ->
              match !body_wait, waiting with
              | Some u, Some mine when u == mine -> body_wait := None
              | _ -> ());
            result
        in
        let cancelled, notify_cancelled = Lwt.wait () in
        let cancel () =
          (try Ache.Server_connection.shutdown connection with _ -> ());
          Lwt.on_termination (close ()) (fun () ->
            if Lwt.is_sleeping cancelled
            then Lwt.wakeup_later notify_cancelled ())
        in
        (* Write clock: runs while output is pending; restarts when all of it is
           written or after [progress_bytes]. *)
        let written = ref 0
        and write_clock = ref false in
        let stop_write_clock () =
          if !write_clock
          then (
            write_clock := false;
            Option.iter Timeout.stop write_timer)
        in
        let writev =
          match write_timer with
          | None -> None
          | Some timer ->
            Some
              (fun iovecs ->
                if not !write_clock
                then (
                  write_clock := true;
                  written := 0;
                  Timeout.set timer);
                let account = function
                  | `Ok n ->
                    written := !written + n;
                    if !written >= progress_bytes
                    then (
                      written := 0;
                      Timeout.set timer)
                  | `Closed -> ()
                in
                let writing = Io.writev socket iovecs in
                if not (Lwt.is_sleeping writing)
                then (
                  Lwt.on_success writing account;
                  writing)
                else
                  Lwt.pick
                    [ ( writing >|= fun result ->
                        account result;
                        result )
                    ; ( Timeout.expired timer >|= fun () ->
                        write_clock := false;
                        cancel ();
                        `Closed )
                    ])
        in
        let serving =
          IO_loop.start
            (module Io)
            (module Observed)
            connection
            ~read_buffer_size:config.read_buffer_size
            ~timed_read
            ~close
            ~writer_waits:stop_write_clock
            ?writev
            socket
        in
        let ended =
          Lwt.finalize
            (fun () -> Lwt.choose [ serving; cancelled ])
            (fun () ->
               Option.iter Timeout.stop head_timer;
               Option.iter Timeout.stop idle_timer;
               Option.iter Timeout.stop between_requests_timer;
               Option.iter Timeout.stop tick_timer;
               Option.iter Timeout.stop write_timer;
               Lwt.return_unit)
        in
        let drain () =
          Ache.Server_connection.drain connection;
          (match !idle_read with
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
                 if Lwt.is_sleeping ended then cancel ())
        in
        (* [Lwt.choose] removes its callback from [stop] when [ended] resolves
           first, so a long-lived [stop] does not keep one callback for each
           connection that ended. *)
        Option.iter
          (fun stop ->
             Lwt.on_termination
               (Lwt.choose [ stop; ended ])
               (fun () -> if Lwt.is_sleeping ended then drain ()))
          stop;
        ended
  end

  module Client = struct
    type socket = Io.socket

    type t =
      { connection : Ache.Client_connection.t
      ; runtime : Upgradable.t
      ; close : unit -> unit Lwt.t
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
      let reuse = { deadline = None; timer = None; upgraded = false } in
      let module Observed = struct
        include Upgradable

        let next_read_operation t =
          let operation = next_read_operation t in
          arm reuse connection runtime close;
          operation

        let read t bs ~off ~len =
          let n = read t bs ~off ~len in
          arm reuse connection runtime close;
          n

        let read_eof t bs ~off ~len =
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
      Lwt.return { connection; runtime; close; reuse }

    let request
          t
          ?flush_headers_immediately
          request
          ~error_handler
          ~response_handler
      =
      (match t.reuse.deadline with
      | Some deadline when Unix.gettimeofday () >= deadline ->
        Upgradable.shutdown t.runtime;
        Lwt.async t.close
      | Some _ | None -> ());
      t.reuse.deadline <- None;
      Option.iter Timeout.stop t.reuse.timer;
      t.reuse.timer <- None;
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
end

include Make (Io)
