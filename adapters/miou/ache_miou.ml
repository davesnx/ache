module Buffer = Ache.Ache_private.Runtime.Buffer
module Upgradable = Ache.Ache_private.Runtime.Upgradable

let await_or_raise computation =
  match Miou.Computation.await computation with
  | Ok value -> value
  | Error (exn, bt) -> Printexc.raise_with_backtrace exn bt

module Signal = struct
  type t = { mutable waiter : unit Miou.Computation.t option }

  let create () = { waiter = None }

  let wake t =
    match t.waiter with
    | None -> ()
    | Some waiter ->
      t.waiter <- None;
      ignore (Miou.Computation.try_return waiter () : bool)

  let rec wait_until t ready =
    if not (ready ())
    then (
      let waiter = Miou.Computation.create () in
      t.waiter <- Some waiter;
      await_or_raise waiter;
      wait_until t ready)
end

module Read_closed = struct
  type t =
    { mutable closed : bool
    ; ready : Signal.t
    }

  let create () = { closed = false; ready = Signal.create () }

  let close t =
    if not t.closed
    then (
      t.closed <- true;
      Signal.wake t.ready)
end

(* The timers of a domain: one heap of connection deadlines, and one task, the
   ticker, that sleeps until the earliest and marks the connections whose
   deadline passed as [due], and wakes their main task. So a domain has one
   sleeper in Miou's timer heap, whatever its number of connections: Miou 0.10.2
   walks that heap on each poll. The ticker never runs a connection's action
   (which may call the user's [error_handler] and wait): each connection runs
   its own, so a slow action delays only its connection.

   A task must be cancelled by the task that started it, and a connection may
   end at any time, so the ticker is a child of the main task of one of the
   connections of the domain, the leader. When the leader ends, it cancels its
   ticker and elects another connection that has a deadline. A new leader starts
   its ticker one round of the scheduler later. The domain cannot wait for
   events meanwhile, since the leader's main task is ready to run; while leaders
   keep ending before they start a ticker, each leader that ends looks at the
   heap.

   A connection's key in the heap is a time at which to look at it again. A
   deadline that moves later does not change it: at the key, the connection runs
   the action of the deadlines that passed, then schedules itself again at the
   earliest of its deadlines. *)
module Timers = struct
  type entry =
    { mutable read : float
    ; mutable write : float
    ; mutable key : float (* [Float.infinity] when not in the heap *)
    ; mutable index : int
    ; mutable due : bool (* the key passed: the connection looks at it *)
    ; main : Signal.t (* wakes the connection's main task *)
    ; mutable lead : bool
    ; mutable restart : bool (* the leader's ticker sleeps too long *)
    ; mutable removed : bool
    }

  type t =
    { mutable heap : entry array
    ; mutable size : int
    ; mutable leader : entry option
    ; mutable target : float (* the ticker sleeps until then *)
    ; parked : Signal.t (* the ticker waits there while the heap is empty *)
    }

  (* A cancelled sleep stays in Miou's timer heap until it ends: the ticker
     sleeps at most this long, so that one cancelled leaves the heap soon. *)
  let longest_sleep = 0.5

  let entry main =
    { read = Float.infinity
    ; write = Float.infinity
    ; key = Float.infinity
    ; index = -1
    ; due = false
    ; main
    ; lead = false
    ; restart = false
    ; removed = false
    }

  let empty = entry (Signal.create ())

  let domain =
    Domain.DLS.new_key (fun () ->
      { heap = [||]
      ; size = 0
      ; leader = None
      ; target = Float.infinity
      ; parked = Signal.create ()
      })

  let get () = Domain.DLS.get domain

  let[@inline] place t i e =
    t.heap.(i) <- e;
    e.index <- i

  let rec sift_up t i e =
    if i = 0
    then place t 0 e
    else
      let parent = (i - 1) / 2 in
      let p = t.heap.(parent) in
      if e.key < p.key
      then (
        place t i p;
        sift_up t parent e)
      else place t i e

  let rec sift_down t i e =
    let left = (2 * i) + 1 in
    if left >= t.size
    then place t i e
    else
      let right = left + 1 in
      let child =
        if right < t.size && t.heap.(right).key < t.heap.(left).key
        then right
        else left
      in
      let c = t.heap.(child) in
      if c.key < e.key
      then (
        place t i c;
        sift_down t child e)
      else place t i e

  let remove t e =
    let i = e.index in
    if i >= 0
    then (
      e.index <- -1;
      e.key <- Float.infinity;
      t.size <- t.size - 1;
      let last = t.heap.(t.size) in
      t.heap.(t.size) <- empty;
      if i < t.size
      then
        if i > 0 && last.key < t.heap.((i - 1) / 2).key
        then sift_up t i last
        else sift_down t i last)

  let elect t e =
    t.leader <- Some e;
    e.lead <- true;
    Signal.wake e.main

  (* [schedule t e at] makes sure that the ticker looks at [e] at [at] at the
     latest. *)
  let schedule t e at =
    if at < e.key && not e.removed
    then (
      e.key <- at;
      if e.index < 0
      then (
        if t.size = Array.length t.heap
        then (
          let heap = Array.make (Int.max 16 (2 * t.size)) empty in
          Array.blit t.heap 0 heap 0 t.size;
          t.heap <- heap);
        t.size <- t.size + 1;
        sift_up t (t.size - 1) e)
      else sift_up t e.index e;
      match t.leader with
      | None -> elect t e
      | Some leader ->
        if at < t.target
        then
          if t.target = Float.infinity
          then Signal.wake t.parked
          else (
            (* Only the leader can cancel the sleep of its ticker. *)
            t.target <- Float.infinity;
            leader.restart <- true;
            Signal.wake leader.main))

  let expire t now =
    while t.size > 0 && t.heap.(0).key <= now do
      let e = t.heap.(0) in
      remove t e;
      e.due <- true;
      Signal.wake e.main
    done

  let tick t () =
    let rec loop () =
      let now = Unix.gettimeofday () in
      expire t now;
      (if t.size = 0
       then (
         t.target <- Float.infinity;
         Signal.wait_until t.parked (fun () -> t.size > 0))
       else
         let target = Float.min t.heap.(0).key (now +. longest_sleep) in
         t.target <- target;
         Miou_unix.sleep (target -. now));
      loop ()
    in
    match loop () with
    | () -> ()
    | exception (Miou.Cancelled as exn) ->
      (* The leader cancelled it, and starts another one or abdicated. *)
      raise exn
    | exception exn ->
      (* Not expected: the leader starts another ticker. *)
      t.target <- Float.infinity;
      (match t.leader with
      | Some leader ->
        leader.restart <- true;
        Signal.wake leader.main
      | None -> ());
      raise exn

  (* [abdicate t e]: the leader [e] no longer runs the ticker. Another
     connection that has a deadline takes over: the last of the heap, a leaf,
     which has one of the latest keys, so that it is not the next to end. *)
  let abdicate t e =
    if e.lead
    then (
      e.lead <- false;
      e.restart <- false;
      t.leader <- None;
      t.target <- Float.infinity;
      if t.size > 0
      then (
        expire t (Unix.gettimeofday ());
        if t.size > 0 then elect t t.heap.(t.size - 1)))
end

let sleep_until at =
  let rec loop () =
    let left = at -. Unix.gettimeofday () in
    if left > 0.
    then (
      Miou_unix.sleep (Float.min Timers.longest_sleep left);
      loop ())
  in
  loop ()

(* [await_stop stop] returns when [stop] is resolved, returned or cancelled.
   When the calling task is cancelled first, it raises [Miou.Cancelled]. *)
let await_stop stop =
  match Miou.Computation.await stop with
  | Error (exn, bt) when Miou.Computation.is_running stop ->
    Printexc.raise_with_backtrace exn bt
  | Ok () | Error _ -> ()

module Sessions = struct
  type t =
    { mutable pending : (unit -> unit) list
    ; mutable running : unit Miou.t list
    ; mutable live : int
    ; mutable aborted : bool
    ; added : Signal.t
    }

  let create () =
    { pending = []
    ; running = []
    ; live = 0
    ; aborted = false
    ; added = Signal.create ()
    }

  type _ Effect.t += Current : t Effect.t

  (* [with_current t] runs request handlers with [t] as their sessions. The
     handler record is made once for each connection, not for each request. *)
  let with_current (t : t) : (unit -> unit) -> unit =
    let effc : type a.
      a Effect.t -> ((a, unit) Effect.Deep.continuation -> unit) option
      = function
      | Current -> Some (fun k -> Effect.Deep.continue k t)
      | _ -> None
    in
    let handler = { Effect.Deep.retc = Fun.id; exnc = raise; effc } in
    fun f -> Effect.Deep.match_with f () handler

  let current () =
    match Effect.perform Current with
    | t -> Some t
    | exception Effect.Unhandled _ -> None

  let add t session =
    t.pending <- session :: t.pending;
    Signal.wake t.added

  let start t ~on_exn =
    List.iter
      (fun session ->
         t.live <- t.live + 1;
         t.running <-
           Miou.async (fun () ->
             Fun.protect
               ~finally:(fun () ->
                 t.live <- t.live - 1;
                 Signal.wake t.added)
               (fun () ->
                  match session () with
                  | () -> ()
                  | exception (Miou.Cancelled as exn) -> raise exn
                  | exception exn -> on_exn exn (Printexc.get_raw_backtrace ())))
           :: t.running)
      (List.rev t.pending);
    t.pending <- []

  let finish t ~on_exn ~shutdown =
    start t ~on_exn;
    if t.running <> []
    then (
      shutdown ();
      Signal.wait_until t.added (fun () -> t.live = 0 || t.aborted);
      if t.aborted then List.iter Miou.cancel t.running;
      let (_ : (unit, exn) result list) = Miou.await_all t.running in
      t.running <- [])
end

module type IO = Ache_miou_intf.IO
module type Server = Ache_miou_intf.Server
module type Client = Ache_miou_intf.Client

module Make_runtime
    (Io : IO)
    (Idle : sig
       (* Response bytes or the end of input wait on the socket. *)
       val input_waiting : Io.socket -> bool
     end)
    (Write_now : sig
       (* [try_writev socket iovecs] writes what the socket takes now, without
          waiting; [`Would_block] when it takes nothing now. With it, the main
          task of a connection runs its writes (see [IO_loop.start]). *)
       val try_writev :
         (Io.socket
          -> Ache.Bigstring.t Ache.IOVec.t list
          -> [ `Ok of int | `Closed | `Would_block ])
           option
     end) =
struct
  module IO_loop = struct
    let writev = Io.writev

    let read socket buffer =
      let result = ref 0 in
      Buffer.put
        ~f:(fun buf ~off ~len k ->
          if len = 0 then failwith "ache-miou: read buffer full";
          k (Io.read socket buf ~off ~len))
        buffer
        (fun n -> result := n);
      !result

    let shutdown = Io.shutdown

    (* The body and write clocks restart after this many bytes, so that a
       trickle of bytes cannot keep a connection alive. *)
    let progress_bytes = 2048

    (* The time limits on reads, without allocation to set one. [deadline now
       `Read] is the deadline of the read that starts at [now], [Float.infinity]
       for none; it is not called when not [timed]. [ended n] runs when a timed
       read ends, with the bytes read (0 when it timed out), and [expire] after
       it when its deadline passed first: it runs the action for the phase that
       the deadline was for, and is [false] when that phase no longer applies.
       [deadline now `Linger] bounds the linger. *)
    type timeouts =
      { timed : bool
      ; deadline : float -> [ `Read | `Linger ] -> float
      ; ended : int -> unit
      ; expire : unit -> bool
      }

    type graceful =
      { stop : unit Miou.Computation.t
      ; grace : float option
      ; drain : unit -> unit
      }

    (* The main task, which calls [start], waits on [main]: for the sessions to
       start, for the end of both loops ([stopped]), for the ticker of the
       domain while the connection leads, and for output to write. Who resolves
       [stopped] wakes [main].

       The main task runs the write loop as long as the socket takes each write
       at once ([Write_now.try_writev]): a write task costs a [Miou.async] per
       connection. The first write that would wait starts a write task, which
       runs the write loop from then on, so the main task never waits in a
       write: the timers, the sessions and the write timeout do not wait behind
       a peer that stops reading. Without [try_writev], the write task starts at
       once. *)
    let start : type t.
      (module Ache.Runtime.ENGINE with type t = t)
      -> read_buffer:Buffer.t
      -> read_closed:Read_closed.t
      -> interruptible:bool
      -> timeout:timeouts
      -> linger:(unit -> bool)
      -> ?write_timeout:float
      -> ?interrupt:(unit -> unit) ref
      -> ?sessions:Sessions.t
      -> ?graceful:graceful
      -> main:Signal.t
      -> stopped:[ `Done | `Abort ] Miou.Computation.t
      -> t
      -> Io.socket
      -> unit
      =
     fun (module Runtime)
       ~read_buffer
       ~read_closed
       ~interruptible
       ~timeout
       ~linger:lingers
       ?write_timeout
       ?interrupt
       ?sessions
       ?graceful
       ~main
       ~stopped
       t
       socket ->
      let receive_shut = ref false
      and send_shut = ref false
      and running = ref 2
      and unreported = ref None in
      let shutdown_once command =
        let receive =
          (match command with
            | Unix.SHUTDOWN_RECEIVE | SHUTDOWN_ALL -> true
            | SHUTDOWN_SEND -> false)
          && not !receive_shut
        and send =
          (match command with
            | Unix.SHUTDOWN_SEND | SHUTDOWN_ALL -> true
            | SHUTDOWN_RECEIVE -> false)
          && not !send_shut
        in
        if receive then receive_shut := true;
        if send then send_shut := true;
        match receive, send with
        | true, true -> shutdown socket SHUTDOWN_ALL
        | true, false -> shutdown socket SHUTDOWN_RECEIVE
        | false, true -> shutdown socket SHUTDOWN_SEND
        | false, false -> ()
      in
      let complete outcome =
        ignore (Miou.Computation.try_return stopped outcome : bool);
        Signal.wake main
      in
      let abort () = complete `Abort in
      let report_exn exn =
        match Runtime.report_exn t exn with
        | () -> true
        | exception (Miou.Cancelled as exn) -> raise exn
        | exception exn ->
          if Option.is_none !unreported
          then unreported := Some (exn, Printexc.get_raw_backtrace ());
          abort ();
          false
      in
      let fail = function
        | Miou.Cancelled as exn -> raise exn
        | exn ->
          let (_ : bool) = report_exn exn in
          abort ()
      in
      let yield register =
        let waker = Miou.Computation.create () in
        register t (fun () ->
          ignore (Miou.Computation.try_return waker () : bool));
        await_or_raise waker
      in
      (* The read and write deadlines live in [entry], in the timers of the
         domain. Setting one later than its key costs nothing: no task wakes. *)
      let timers = Timers.get () in
      let entry = Timers.entry main in
      let set_read deadline =
        entry.read <- deadline;
        Timers.schedule timers entry deadline
      and set_write deadline =
        entry.write <- deadline;
        Timers.schedule timers entry deadline
      in
      (* [reading]: the read loop waits in a read. [lingering]: reading ended
         for good, and reads only drop input until it ends. *)
      let reading = ref false
      and lingering = ref false
      and expire_failed = ref None in
      (* Reading ended for good while a read waits: a timeout action ran. The
         read is not cancelled, which a TLS read would keep as an error: it and
         the reads after it drop their input, and the linger deadline ends them
         by shutting down the receive side. *)
      let stop_reading () =
        lingering := true;
        if lingers ()
        then set_read (timeout.deadline (Unix.gettimeofday ()) `Linger)
        else shutdown_once SHUTDOWN_RECEIVE
      in
      (* The write clock runs while output waits to be written. It restarts when
         all of it is written, and after each [progress_bytes] bytes written. *)
      let write_clock = ref false
      and written = ref 0
      and in_writev = ref false in
      (* [fire now] runs the action of the deadlines that passed, in a task of
         the connection (see [run_due]). *)
      let fire now =
        if entry.read <= now
        then (
          entry.read <- Float.infinity;
          if !lingering
          then
            (* The linger ends, unless the response is still being written. *)
            if !send_shut
            then shutdown_once SHUTDOWN_RECEIVE
            else set_read (timeout.deadline now `Linger)
          else
            match
              timeout.ended 0;
              timeout.expire ()
            with
            | true -> stop_reading ()
            | false ->
              (* The phase changed while the read waited: the read goes on, with
                 the deadline of the phase now. *)
              set_read (timeout.deadline now `Read)
            | exception (Miou.Cancelled as exn) -> raise exn
            | exception exn ->
              (* The read loop raises it when its read ends. *)
              expire_failed := Some (exn, Printexc.get_raw_backtrace ());
              lingering := true;
              shutdown_once SHUTDOWN_RECEIVE);
        if entry.write <= now
        then (
          entry.write <- Float.infinity;
          if !write_clock then abort ())
      in
      (* A read in a reader task, so that [read_closed] can end the wait for it
         (the client). *)
      let pending_read = ref false
      and read_requested = ref false
      and read_result = ref None
      and request_read = Signal.create ()
      and reader = ref None in
      let start_reader () =
        if Option.is_none !reader
        then
          reader :=
            Some
              (Miou.async (fun () ->
                 let rec loop () =
                   Signal.wait_until request_read (fun () -> !read_requested);
                   read_requested := false;
                   (read_result :=
                      match read socket read_buffer with
                      | n -> Some (Ok n)
                      | exception (Miou.Cancelled as exn) -> raise exn
                      | exception exn -> Some (Error exn));
                   Signal.wake read_closed.ready;
                   loop ()
                 in
                 loop ()))
      in
      let await_read () =
        if Option.is_none !read_result
        then (
          if read_closed.closed then raise End_of_file;
          if not !pending_read
          then (
            pending_read := true;
            read_requested := true;
            start_reader ();
            Signal.wake request_read);
          Signal.wait_until read_closed.ready (fun () ->
            Option.is_some !read_result || read_closed.closed));
        match !read_result with
        | None -> raise End_of_file
        | Some result ->
          read_result := None;
          pending_read := false;
          (match result with Ok n -> n | Error exn -> raise exn)
      in
      let read_input () =
        if interruptible then await_read () else read socket read_buffer
      in
      let drop () =
        let (_ : int) = Buffer.get read_buffer ~f:(fun _ ~off:_ ~len -> len) in
        ()
      in
      let raise_expire_failed () =
        match !expire_failed with
        | Some (exn, bt) ->
          expire_failed := None;
          Printexc.raise_with_backtrace exn bt
        | None -> ()
      in
      Option.iter
        (fun interrupt ->
           interrupt :=
             fun () ->
               (* The core's read state changed while a read waits: a new phase
                  (the server's close frame starts [`Close]) gives it its
                  deadline at once. *)
               if !reading && not !lingering
               then
                 match Runtime.next_read_operation t with
                 | `Close ->
                   entry.read <- Float.infinity;
                   stop_reading ()
                 | `Read | `Yield ->
                   if timeout.timed
                   then (
                     let now = Unix.gettimeofday () in
                     timeout.ended 0;
                     entry.read <- Float.infinity;
                     set_read (timeout.deadline now `Read)))
        interrupt;
      (* [Error `Timeout]: the read gave the core neither input nor its end. *)
      let timed_read () =
        (* The core reads again after a timeout action (none does): the reads
           give it their input again. *)
        if !lingering
        then (
          lingering := false;
          entry.read <- Float.infinity);
        if not timeout.timed
        then Ok (read_input ())
        else
          let start = Unix.gettimeofday () in
          let deadline = timeout.deadline start `Read in
          if deadline <= start
          then (
            timeout.ended 0;
            let (_ : bool) = timeout.expire () in
            Error `Timeout)
          else (
            set_read deadline;
            reading := true;
            match read_input () with
            | n ->
              reading := false;
              if !lingering
              then (
                raise_expire_failed ();
                drop ();
                Error `Timeout)
              else (
                entry.read <- Float.infinity;
                timeout.ended n;
                Ok n)
            | exception exn ->
              reading := false;
              if !lingering
              then (
                raise_expire_failed ();
                match exn with
                | End_of_file -> Error `Timeout
                | exn -> raise exn)
              else (
                entry.read <- Float.infinity;
                raise exn))
      in
      let linger () =
        if not !lingering
        then (
          lingering := true;
          set_read (timeout.deadline (Unix.gettimeofday ()) `Linger));
        let rec drop_input () =
          match read_input () with
          | (_ : int) ->
            drop ();
            drop_input ()
          | exception End_of_file -> ()
        in
        if not !receive_shut then drop_input ();
        entry.read <- Float.infinity
      in
      let rec read_loop () =
        match Runtime.next_read_operation t with
        | `Read ->
          (match if Buffer.unread read_buffer then Ok 0 else timed_read () with
          | Ok (_ : int) ->
            let (_ : int) = Buffer.get read_buffer ~f:(Runtime.read t) in
            ()
          | Error `Timeout -> ()
          | exception End_of_file ->
            let (_ : int) =
              Buffer.get read_buffer ~f:(fun buf ~off ~len ->
                let n = Runtime.read_eof t buf ~off ~len in
                if !pending_read then 0 else n)
            in
            ());
          read_loop ()
        | `Yield ->
          yield Runtime.yield_reader;
          read_loop ()
        | `Close ->
          if not read_closed.closed
          then (
            if lingers () then linger ();
            shutdown_once SHUTDOWN_RECEIVE;
            Read_closed.close read_closed)
      in
      let restart_write_clock limit =
        set_write (Unix.gettimeofday () +. limit);
        written := 0
      in
      let rec write_loop () =
        match Runtime.next_write_operation t with
        | `Write io_vectors ->
          (match write_timeout with
          | Some limit when not !write_clock ->
            write_clock := true;
            restart_write_clock limit
          | Some _ | None -> ());
          in_writev := true;
          let result = writev socket io_vectors in
          in_writev := false;
          (match write_timeout, result with
          | Some limit, `Ok n ->
            written := !written + n;
            if !written >= progress_bytes then restart_write_clock limit
          | _ -> ());
          Runtime.report_write_result t result;
          write_loop ()
        | `Yield ->
          write_clock := false;
          entry.write <- Float.infinity;
          yield Runtime.yield_writer;
          write_loop ()
        | `Close _ ->
          write_clock := false;
          entry.write <- Float.infinity;
          shutdown_once SHUTDOWN_SEND
      in
      let read_task () =
        (match read_loop () with
        | () -> ()
        | exception (Miou.Cancelled as exn) -> raise exn
        | exception exn ->
          if report_exn exn
          then (
            shutdown_once SHUTDOWN_RECEIVE;
            Read_closed.close read_closed));
        entry.read <- Float.infinity;
        Option.iter Miou.cancel !reader
      and write_task () =
        match write_loop () with () -> () | exception exn -> fail exn
      in
      let stop () =
        decr running;
        if !running = 0 then complete `Done
      in
      let reading =
        Miou.async (fun () ->
          read_task ();
          stop ())
      in
      let writing = ref None in
      let start_writing () =
        writing :=
          Some
            (Miou.async (fun () ->
               write_task ();
               stop ()))
      in
      (* [write_ready]: the main task runs the write loop when it is set; it is
         clear while the loop waits for the core's wake-up, or when the loop
         runs in the write task or ended. *)
      let write_ready = ref false in
      let write_now =
        match Write_now.try_writev with
        | None ->
          start_writing ();
          fun () -> ()
        | Some try_writev ->
          write_ready := true;
          let woken () =
            write_ready := true;
            Signal.wake main
          in
          let rec write_loop () =
            match Runtime.next_write_operation t with
            | `Write io_vectors ->
              (match try_writev socket io_vectors with
              | `Would_block -> start_writing ()
              | (`Ok _ | `Closed) as result ->
                Runtime.report_write_result t result;
                write_loop ())
            | `Yield -> Runtime.yield_writer t woken
            | `Close _ ->
              shutdown_once SHUTDOWN_SEND;
              stop ()
          in
          fun () ->
            if !write_ready
            then (
              write_ready := false;
              match write_loop () with () -> () | exception exn -> fail exn)
      in
      let watcher =
        Option.map
          (fun { stop; grace; drain } ->
             Miou.async (fun () ->
               await_stop stop;
               (match drain () with
               | () ->
                 (* An idle connection is closed now: end the read that
                    waits. *)
                 if Runtime.is_closed t then shutdown_once SHUTDOWN_RECEIVE
               | exception (Miou.Cancelled as exn) -> raise exn
               | exception exn -> fail exn);
               Option.iter
                 (fun grace ->
                    sleep_until (Unix.gettimeofday () +. grace);
                    Option.iter
                      (fun sessions -> sessions.Sessions.aborted <- true)
                      sessions;
                    abort ())
                 grace))
          graceful
      in
      let session_failed exn bt =
        if Option.is_none !unreported then unreported := Some (exn, bt)
      in
      let ticker = ref None in
      let lead () =
        if entry.lead
        then
          match !ticker with
          | None ->
            (* See [Timers]: the ticker starts a round later, if this connection
               still leads then. *)
            Miou.yield ();
            (* The response may have come meanwhile: writing it may end the
               connection. *)
            write_now ();
            if
              entry.lead
              && Option.is_none !ticker
              && Miou.Computation.is_running stopped
            then (
              entry.restart <- false;
              ticker := Some (Miou.async (Timers.tick timers)))
          | Some old when entry.restart ->
            entry.restart <- false;
            Miou.cancel old;
            ticker := Some (Miou.async (Timers.tick timers))
          | Some _ -> ()
      in
      (* The action of a deadline runs in its own task: it may wait (the
         [error_handler] of a 408), while the main task goes on leading the
         ticker and starting sessions. One runs at a time. *)
      let action = ref None
      and action_ended = ref false in
      let reschedule () =
        Timers.schedule timers entry (Float.min entry.read entry.write)
      in
      let run_due () =
        (match !action with
        | Some task when !action_ended ->
          action := None;
          action_ended := false;
          let (_ : (unit, exn) result) = Miou.await task in
          reschedule ()
        | Some _ | None -> ());
        if entry.due && Option.is_none !action
        then (
          entry.due <- false;
          let now = Unix.gettimeofday () in
          if entry.read <= now || entry.write <= now
          then
            action :=
              Some
                (Miou.async (fun () ->
                   Fun.protect
                     ~finally:(fun () ->
                       action_ended := true;
                       Signal.wake main)
                     (fun () -> fire now)))
          else reschedule ())
      in
      (* The main task wakes for each response: a round allocates nothing. *)
      let ready () =
        (not (Miou.Computation.is_running stopped))
        || (match sessions with
          | Some sessions -> sessions.Sessions.pending <> []
          | None -> false)
        || (entry.lead && (Option.is_none !ticker || entry.restart))
        || (entry.due && Option.is_none !action)
        || !action_ended
        || !write_ready
      in
      let rec serve () =
        Signal.wait_until main ready;
        write_now ();
        (match sessions with
        | Some ({ pending = _ :: _; _ } as sessions) ->
          Sessions.start sessions ~on_exn:session_failed
        | Some _ | None -> ());
        lead ();
        run_due ();
        if Miou.Computation.is_running stopped
        then serve ()
        else await_or_raise stopped
      in
      (* The connection leaves the timers of the domain before its socket is
         closed. *)
      let leave () =
        entry.removed <- true;
        Timers.remove timers entry;
        Timers.abdicate timers entry
      in
      Fun.protect ~finally:leave @@ fun () ->
      (match serve () with
      | `Abort ->
        Miou.cancel reading;
        Option.iter Miou.cancel !writing;
        (* A write that was cancelled before it returned is lost: tell the core,
           so that the flushes that wait for it get [`Closed]. *)
        if !in_writev
        then (
          in_writev := false;
          match Runtime.report_write_result t `Closed with
          | () -> ()
          | exception (Miou.Cancelled as exn) -> raise exn
          | exception exn ->
            let (_ : bool) = report_exn exn in
            ());
        shutdown_once SHUTDOWN_ALL
      | `Done ->
        let (_ : (unit, exn) result list) =
          Miou.await_all (reading :: Option.to_list !writing)
        in
        ());
      leave ();
      Option.iter Miou.cancel !ticker;
      ticker := None;
      Option.iter Miou.cancel !action;
      action := None;
      Option.iter
        (fun sessions ->
           Sessions.finish sessions ~on_exn:session_failed ~shutdown:(fun () ->
             try Runtime.shutdown t with
             | Miou.Cancelled as exn -> raise exn
             | _ -> ()))
        sessions;
      Option.iter Miou.cancel watcher;
      Option.iter
        (fun (exn, bt) -> Printexc.raise_with_backtrace exn bt)
        !unreported
  end

  module Server = struct
    type socket = Io.socket

    let linger_without_idle_timeout = 2.0
    let default_idle_timeout = Some 60.
    let default_head_timeout = Some 30.
    let default_body_timeout = Some 60.
    let default_write_timeout = Some 60.
    let default_close_timeout = Some 10.
    let current_date = Ache.Ache_private.Http_date.now

    let check_timeout name = function
      | Some seconds when not (seconds > 0.) ->
        invalid_arg
          (Printf.sprintf
             "Ache_miou.Server.create_connection_handler: %s must be positive"
             name)
      | _ -> ()

    let create_connection_handler
          ?(config = Ache.Config.default)
          ?(idle_timeout = default_idle_timeout)
          ?(head_timeout = default_head_timeout)
          ?(body_timeout = default_body_timeout)
          ?(write_timeout = default_write_timeout)
          ?(close_timeout = default_close_timeout)
          ?(date = true)
          ?stop
          ?grace
          ~request_handler
          ~error_handler
      =
      check_timeout "idle_timeout" idle_timeout;
      check_timeout "head_timeout" head_timeout;
      check_timeout "body_timeout" body_timeout;
      check_timeout "write_timeout" write_timeout;
      check_timeout "close_timeout" close_timeout;
      check_timeout "grace" grace;
      (* With an idle timeout of at least 1 s, persistent responses announce it
         in [Keep-Alive: timeout=n], and the idle timer between requests runs 1
         s longer, so that a client that trusts the hint does not write into a
         connection that is closing. *)
      let keep_alive_timeout =
        match idle_timeout with
        | Some seconds when seconds >= 1. -> Some (int_of_float seconds)
        | Some _ | None -> None
      in
      let timed =
        Option.is_some idle_timeout
        || Option.is_some head_timeout
        || Option.is_some body_timeout
        || Option.is_some close_timeout
      in
      let serve socket client_addr =
        let head_deadline = ref None
        and close_deadline = ref None
        and close_aborted = ref false
        and read_closed = Read_closed.create ()
        and body_spent = ref 0.
        and body_received = ref 0
        and body_start = ref 0.
        and error_handler_failure = ref None
        and sessions = Sessions.create () in
        let with_sessions = Sessions.with_current sessions in
        let error_handler ?request error start_response =
          try error_handler client_addr ?request error start_response with
          | Miou.Cancelled as exn -> raise exn
          | exn ->
            let bt = Printexc.get_raw_backtrace () in
            if Option.is_none !error_handler_failure
            then error_handler_failure := Some (exn, bt);
            Printexc.raise_with_backtrace exn bt
        in
        let connection =
          Ache.Server_connection.create
            ~config
            ~error_handler
            ?date:(if date then Some current_date else None)
            ?keep_alive_timeout
            (fun reqd ->
               head_deadline := None;
               body_spent := 0.;
               body_received := 0;
               with_sessions (fun () -> request_handler client_addr reqd))
        in
        (* [`Idle] after an upgrade is the idle timeout of a WebSocket or
           another protocol, which announces nothing. *)
        let upgraded () =
          match Ache.Server_connection.upgrade_state connection with
          | `Websocket | `Other -> true
          | `None -> false
        (* Only a WebSocket ([Ache.Websocket.upgrade]) reaches [`Close]. *)
        and is_websocket () =
          match Ache.Server_connection.upgrade_state connection with
          | `Websocket -> true
          | `None | `Other -> false
        in
        let interrupt = ref ignore in
        (* The close deadline is set the first time the connection is seen
           waiting for the client's close frame, and never moves after. A read
           that waits takes it at once. *)
        let start_close now =
          match close_timeout, !close_deadline with
          | Some seconds, None ->
            close_deadline := Some (now +. seconds);
            !interrupt ()
          | Some _, Some _ | None, _ -> ()
        in
        (* The close clock starts when the server's close frame is queued,
           whatever queued it ({!Ache.Websocket.close} from the session, or the
           close at the end of the session). The read task may wait with the
           idle limit then, so the write task, which the frame wakes, sets the
           deadline. Drain and the idle timeout close through the core's [fail],
           which stops reading: the connection ends at once, without a close
           deadline. *)
        let observe_write () =
          if
            Option.is_some close_timeout
            && Option.is_none !close_deadline
            && is_websocket ()
          then
            match Ache.Server_connection.read_timeout connection with
            | Some (`Close, _) -> start_close (Unix.gettimeofday ())
            | Some ((`Idle | `Head | `Body), _) | None -> ()
        in
        let module Observed = struct
          include Ache.Server_connection

          let next_write_operation t =
            let operation = next_write_operation t in
            observe_write ();
            operation
        end
        in
        (* The phase whose deadline the read in progress has. Its action depends
           on the state when the deadline passes. *)
        let armed : [ `None | `Idle | `Head | `Body | `Close ] ref =
          ref `None
        in
        let deadline now = function
          | `Linger ->
            now
            +. Option.value idle_timeout ~default:linger_without_idle_timeout
          | `Read ->
            let phase = Ache.Server_connection.read_timeout connection in
            armed := `None;
            (* The head clock starts when a read starts in the [`Head] phase,
               right after the read that brought the first bytes of a head. *)
            (match head_timeout, phase with
            | Some seconds, Some (`Head, _) ->
              if Option.is_none !head_deadline
              then head_deadline := Some (now +. seconds)
            | Some _, Some ((`Idle | `Body | `Close), _) | Some _, None ->
              head_deadline := None
            | None, _ -> ());
            (match phase with
            | Some (`Idle, _) ->
              (match idle_timeout with
              | Some seconds ->
                armed := `Idle;
                (* The idle timeout of a WebSocket announces nothing. *)
                if Option.is_some keep_alive_timeout && not (upgraded ())
                then now +. seconds +. 1.
                else now +. seconds
              | None -> Float.infinity)
            | Some (`Head, _) ->
              (match !head_deadline with
              | Some deadline ->
                armed := `Head;
                deadline
              | None -> Float.infinity)
            | Some (`Body, _) ->
              (* The body clock counts only the time spent waiting for body
                 bytes, and restarts after [progress_bytes] bytes. *)
              (match body_timeout with
              | Some limit ->
                armed := `Body;
                body_start := now;
                now +. limit -. !body_spent
              | None -> Float.infinity)
            | Some (`Close, _) ->
              start_close now;
              (match !close_deadline with
              | Some deadline ->
                armed := `Close;
                deadline
              | None -> Float.infinity)
            | None -> Float.infinity)
        in
        let ended n =
          match !armed with
          | `Body ->
            body_spent := !body_spent +. (Unix.gettimeofday () -. !body_start);
            body_received := !body_received + n;
            if !body_received >= IO_loop.progress_bytes
            then (
              body_spent := 0.;
              body_received := 0)
          | `Idle | `Head | `Close | `None -> ()
        in
        let expire () =
          match Ache.Server_connection.read_timeout connection with
          | Some (phase, action)
            when (phase :> [ `None | `Idle | `Head | `Body | `Close ]) == !armed
            ->
            (match phase with
            | `Body ->
              body_spent := 0.;
              body_received := 0
            | `Close ->
              (* Our close frame is written: close without lingering. *)
              close_aborted := true
            | `Idle | `Head -> ());
            action ();
            true
          | Some _ | None -> false
        in
        let read_buffer = Buffer.acquire config.read_buffer_size in
        IO_loop.start
          (module Observed)
          ~read_buffer
          ~read_closed
          ~interruptible:false
          ~timeout:{ IO_loop.timed; deadline; ended; expire }
          ~linger:(fun () ->
            (not !close_aborted) && Ache.Server_connection.lingers connection)
          ?write_timeout
          ~interrupt
          ~sessions
          ~main:sessions.added
          ?graceful:
            (Option.map
               (fun stop ->
                  { IO_loop.stop
                  ; grace
                  ; drain = (fun () -> Ache.Server_connection.drain connection)
                  })
               stop)
          ~stopped:(Miou.Computation.create ())
          connection
          socket;
        (* [IO_loop.start] returns once its tasks ended or were cancelled (a
           cancelled task does not run again): no read or write runs, and none
           starts. The application keeps no bytes of the read buffer (the parser
           and [Ache.Websocket] copy them), unless it took over the connection
           with a protocol of its own. *)
        (match Ache.Server_connection.upgrade_state connection with
        | `None | `Websocket -> Buffer.release read_buffer
        | `Other -> ());
        Ache.Ache_private.Server_connection.release connection;
        Option.iter
          (fun (exn, bt) -> Printexc.raise_with_backtrace exn bt)
          !error_handler_failure
      in
      fun socket client_addr ->
        match stop with
        | Some stop when not (Miou.Computation.is_running stop) -> ()
        | _ -> serve socket client_addr
  end

  module Client = struct
    type socket = Io.socket

    type t =
      { connection : Ache.Client_connection.t
      ; runtime : Upgradable.t
      ; read_closed : Read_closed.t
      ; stopped : [ `Done | `Abort ] Miou.Computation.t
      ; main : Signal.t
      ; task : unit Miou.t
      ; mutable awaited : bool
      ; socket : socket
      ; reuse : reuse
      }

    (* The connection is not reused after [deadline]: the server announced with
       [Keep-Alive: timeout=n] that it may close it then. *)
    and reuse =
      { mutable deadline : float option
      ; mutable upgraded : bool
      }

    (* [close_idle] closes the connection as {!shutdown} does, from any task of
       the connection, without waiting for its task. *)
    let close_idle runtime read_closed =
      Read_closed.close read_closed;
      Upgradable.shutdown runtime

    let create_connection ?(config = Ache.Config.default) socket =
      let connection = Ache.Client_connection.create ~config () in
      let runtime =
        Upgradable.create
          (Ache.Runtime.make (module Ache.Client_connection) connection)
      in
      let read_closed = Read_closed.create ()
      and stopped = Miou.Computation.create ()
      and main = Signal.create ()
      and reuse = { deadline = None; upgraded = false } in
      (* Before each read, arm the reuse deadline once a response has given a
         hint; the read then waits at most until the deadline. *)
      let armed = ref Float.infinity in
      let deadline now = function
        | `Linger -> Float.infinity
        | `Read when reuse.upgraded ->
          armed := Float.infinity;
          Float.infinity
        | `Read ->
          armed := Float.infinity;
          (match Ache.Client_connection.reuse_timeout connection with
          | None -> ()
          | Some seconds ->
            armed :=
              (match reuse.deadline with
                | Some deadline -> deadline
                | None ->
                  let deadline = now +. seconds in
                  reuse.deadline <- Some deadline;
                  deadline));
          !armed
      and expire () =
        match reuse.deadline with
        | Some current
          when Float.equal current !armed
               && Option.is_some
                    (Ache.Client_connection.reuse_timeout connection) ->
          close_idle runtime read_closed;
          true
        | Some _ | None -> false
      in
      let task =
        Miou.async (fun () ->
          IO_loop.start
            (module Upgradable)
            ~read_buffer:(Buffer.create config.read_buffer_size)
            ~read_closed
            ~interruptible:true
            ~timeout:{ IO_loop.timed = true; deadline; ended = ignore; expire }
            ~linger:(fun () -> true)
            ~main
            ~stopped
            runtime
            socket)
      in
      { connection
      ; runtime
      ; read_closed
      ; stopped
      ; main
      ; task
      ; awaited = false
      ; socket
      ; reuse
      }

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
      | Some deadline when deadline <= Unix.gettimeofday () ->
        close_idle t.runtime t.read_closed
      | Some _ | None -> ());
      t.reuse.deadline <- None;
      (* Response bytes that wait on an idle connection would be taken as the
         response to this request (undici GHSA-35p6-xmwp-9g52): close the
         connection instead. The read loop gives the bytes of a read to the
         connection before it yields, so the socket holds all of them. *)
      if
        Ache.Ache_private.Client_connection.is_waiting t.connection
        && Idle.input_waiting t.socket
      then (
        Read_closed.close t.read_closed;
        Upgradable.shutdown t.runtime);
      Ache.Client_connection.request
        t.connection
        ?flush_headers_immediately
        request
        ~error_handler
        ~response_handler

    let shutdown t =
      Read_closed.close t.read_closed;
      Upgradable.shutdown t.runtime;
      ignore (Miou.Computation.try_return t.stopped `Abort : bool);
      Signal.wake t.main;
      if not t.awaited
      then (
        t.awaited <- true;
        let (_ : (unit, exn) result) = Miou.await t.task in
        ())

    let is_closed t = Upgradable.is_closed t.runtime

    let upgrade t impl =
      t.reuse.upgraded <- true;
      t.reuse.deadline <- None;
      Upgradable.upgrade t.runtime impl
  end
end

module Io = struct
  type socket = Miou_unix.file_descr

  let read socket buffer ~off ~len =
    match Miou_unix.read_bigstring socket buffer ~off ~len with
    | 0 -> raise End_of_file
    | n -> n
    | exception Unix.Unix_error ((ECONNRESET | ENOTCONN), _, _) ->
      raise End_of_file

  external sendmsg :
     Unix.file_descr
    -> Ache.Bigstring.t Ache.IOVec.t list
    -> int
    = "ache_miou_sendmsg"

  let write_each socket iovecs =
    List.fold_left
      (fun total { Ache.IOVec.buffer; off; len } ->
         Miou_unix.write_bigstring socket buffer ~off ~len;
         total + len)
      0
      iovecs

  let writev socket iovecs =
    let fd = Miou_unix.to_file_descr socket in
    let rec send () =
      match sendmsg fd iovecs with
      | -1 ->
        Miou_unix.blocking_write fd;
        send ()
      | n -> n
    in
    match send () with
    | n -> `Ok n
    | exception Unix.Unix_error (ENOTSOCK, _, _) ->
      (match write_each socket iovecs with
      | n -> `Ok n
      | exception Unix.Unix_error ((EPIPE | ECONNRESET | ENOTCONN), _, _) ->
        `Closed)
    | exception Unix.Unix_error ((EPIPE | ECONNRESET | ENOTCONN), _, _) ->
      `Closed

  let shutdown socket command =
    try Unix.shutdown (Miou_unix.to_file_descr socket) command with
    | Unix.Unix_error _ -> ()

  (* [writev] without its wait: [`Would_block] also when the descriptor is not a
     socket, so that [writev] writes it. *)
  let try_writev socket iovecs =
    match sendmsg (Miou_unix.to_file_descr socket) iovecs with
    | -1 -> `Would_block
    | n -> `Ok n
    | exception Unix.Unix_error ((EPIPE | ECONNRESET | ENOTCONN), _, _) ->
      `Closed
    | exception Unix.Unix_error (ENOTSOCK, _, _) -> `Would_block
end

(* Bytes other than empty lines, or the end of input, that wait on [fd], without
   blocking. *)
let input_waiting fd =
  match Unix.select [ fd ] [] [] 0. with
  | [], _, _ -> false
  | _ :: _, _, _ ->
    let buf = Bytes.create 64 in
    (match Unix.recv fd buf 0 (Bytes.length buf) [ Unix.MSG_PEEK ] with
    | n ->
      n = 0
      || Bytes.exists
           (function '\r' | '\n' -> false | _ -> true)
           (Bytes.sub buf 0 n)
    | exception Unix.Unix_error ((ECONNRESET | ECONNABORTED | EPIPE), _, _) ->
      true
    | exception Unix.Unix_error _ -> false)
  | exception Unix.Unix_error _ -> false

module Make_with_idle_check
    (Io : IO)
    (Idle : sig
       val input_waiting : Io.socket -> bool
     end) =
  Make_runtime (Io) (Idle)
    (struct
      let try_writev = None
    end)

module Make (Io : IO) =
  Make_with_idle_check
    (Io)
    (struct
      let input_waiting _ = false
    end)

include
  Make_runtime
    (Io)
    (struct
      let input_waiting socket = input_waiting (Miou_unix.to_file_descr socket)
    end)
    (struct
      let try_writev = Some Io.try_writev
    end)

module Server = struct
  include Server

  let rec clean_up orphans =
    match Miou.care orphans with
    | None | Some None -> ()
    | Some (Some connection) ->
      let (_ : (unit, exn) result) = Miou.await connection in
      clean_up orphans

  (* [await_orphans orphans] awaits the tasks of [orphans]; they have all
     returned or are about to. *)
  let rec await_orphans orphans =
    match Miou.care orphans with
    | None -> ()
    | Some None ->
      Miou.yield ();
      await_orphans orphans
    | Some (Some connection) ->
      let (_ : (unit, exn) result) = Miou.await connection in
      await_orphans orphans

  (* [accept_until stop listening] accepts a connection, or returns [None] once
     [stop] is resolved. *)
  let accept_until stop listening =
    let fd = Miou_unix.to_file_descr listening in
    let rec go () =
      if not (Miou.Computation.is_running stop)
      then None
      else
        match Unix.accept fd with
        | client, client_addr ->
          Some (Miou_unix.of_file_descr ~non_blocking:true client, client_addr)
        | exception Unix.Unix_error ((EAGAIN | EWOULDBLOCK | EINTR), _, _) ->
          let (_ : (unit, exn) result) =
            Miou.await_first
              [ Miou.async (fun () -> Miou_unix.blocking_read fd)
              ; Miou.async (fun () -> await_stop stop)
              ]
          in
          go ()
    in
    go ()

  let accept_pause = 0.1

  let run
        ?config
        ?idle_timeout
        ?head_timeout
        ?body_timeout
        ?write_timeout
        ?close_timeout
        ?date
        ?domains
        ?(backlog = 128)
        ?(tcp_nodelay = false)
        ?stop
        ?grace
        ~on_error
        ~request_handler
        ~error_handler
        addr
    =
    let self = Miou.Domain.self () in
    let others =
      List.filter
        (fun domain -> not (Miou.Domain.Uid.equal domain self))
        (Miou.Domain.all ())
    in
    let available = List.length others + 1 in
    let domains = Option.value domains ~default:available in
    if domains < 1 || domains > available
    then
      invalid_arg
        (Printf.sprintf
           "Ache_miou.Server.run: domains must be between 1 and %d"
           available);
    let socket =
      match addr with
      | Unix.ADDR_UNIX _ ->
        invalid_arg "Ache_miou.Server.run: addr must be an Internet address"
      | ADDR_INET (inet_addr, _) ->
        if Unix.is_inet6_addr inet_addr
        then Miou_unix.tcpv6
        else Miou_unix.tcpv4
    in
    let handler =
      create_connection_handler
        ?config
        ?idle_timeout
        ?head_timeout
        ?body_timeout
        ?write_timeout
        ?close_timeout
        ?date
        ?stop
        ?grace
        ~request_handler
        ~error_handler
    in
    let handler =
      if tcp_nodelay
      then (
        fun client addr ->
          Unix.setsockopt (Miou_unix.to_file_descr client) TCP_NODELAY true;
          handler client addr)
      else handler
    in
    let failed = Miou.Computation.create () in
    let next_client =
      match stop with
      | None -> fun listening -> Some (Miou_unix.accept listening)
      | Some stop -> accept_until stop
    in
    let serve () =
      let listening = socket () in
      let closed = ref false in
      let close () =
        if not !closed
        then (
          closed := true;
          Miou_unix.close listening)
      in
      Fun.protect ~finally:close @@ fun () ->
      Miou_unix.bind_and_listen
        ~backlog
        ~reuseaddr:true
        ~reuseport:true
        listening
        addr;
      let orphans = Miou.orphans ()
      and live = ref 0
      and ended = Signal.create () in
      let rec accept () =
        clean_up orphans;
        match next_client listening with
        | None -> ()
        | exception
            (Unix.Unix_error ((EMFILE | ENFILE | ENOBUFS | ENOMEM), _, _) as exn)
          ->
          on_error exn;
          Miou_unix.sleep accept_pause;
          accept ()
        | exception
            (Unix.Unix_error
               ( ( ECONNABORTED | EPERM | ENETDOWN | ENETUNREACH | EHOSTDOWN
                 | EHOSTUNREACH | ENOPROTOOPT )
               , _
               , _ ) as exn) ->
          on_error exn;
          accept ()
        | Some (client, client_addr) ->
          incr live;
          ignore
            (Miou.async ~orphans (fun () ->
               Fun.protect
                 ~finally:(fun () ->
                   Miou_unix.close client;
                   decr live;
                   Signal.wake ended)
                 (fun () ->
                    match handler client client_addr with
                    | () -> ()
                    | exception (Miou.Cancelled as exn) -> raise exn
                    | exception exn ->
                      (try on_error exn with
                      | exn ->
                        let bt = Printexc.get_raw_backtrace () in
                        ignore
                          (Miou.Computation.try_cancel failed (exn, bt) : bool))))
             : unit Miou.t);
          accept ()
      in
      accept ();
      close ();
      Signal.wait_until ended (fun () -> !live = 0);
      await_orphans orphans
    in
    let serve () =
      match serve () with
      | () -> ()
      | exception (Miou.Cancelled as exn) -> raise exn
      | exception exn ->
        let bt = Printexc.get_raw_backtrace () in
        ignore (Miou.Computation.try_cancel failed (exn, bt) : bool)
    in
    let loops () =
      let (_ : (unit, exn) result list) =
        Miou.await_all
          (Miou.async serve
          :: List.map
               (fun pin -> Miou.call ~pin serve)
               (List.filteri (fun i _ -> i < domains - 1) others))
      in
      ()
    in
    match
      Miou.await_first
        [ Miou.async (fun () -> Miou.Computation.await_exn failed)
        ; Miou.async loops
        ]
    with
    | Ok () -> ()
    | Error exn -> raise exn
end

module Websocket = struct
  type t =
    { connection : Ache.Websocket.t
    ; messages : Ache.Websocket.message Queue.t
    ; receivers : unit Miou.Computation.t Queue.t
    ; pingers : unit Miou.Computation.t Queue.t
    ; mutable ended : bool
    ; mutable client_close : Ache.Websocket.close option
    ; closed : unit Miou.Computation.t
    }

  exception Closed

  let max_queued_messages = 8

  let rec wake_one waiters =
    match Queue.take_opt waiters with
    | Some waiter ->
      if not (Miou.Computation.try_return waiter ()) then wake_one waiters
    | None -> ()

  let end_all waiters result =
    Queue.iter
      (fun waiter ->
         ignore
           ((match result with
            | Ok () -> Miou.Computation.try_return waiter ()
            | Error error -> Miou.Computation.try_cancel waiter error)
            : bool))
      waiters;
    Queue.clear waiters

  let wait waiters =
    let pending = Queue.of_seq (Queue.to_seq waiters) in
    Queue.clear waiters;
    Queue.iter
      (fun waiter ->
         if Miou.Computation.is_running waiter then Queue.push waiter waiters)
      pending;
    let waiter = Miou.Computation.create () in
    Queue.push waiter waiters;
    let fail exn bt =
      ignore (Miou.Computation.try_cancel waiter (exn, bt) : bool);
      Printexc.raise_with_backtrace exn bt
    in
    match Miou.Computation.await waiter with
    | Ok () -> ()
    | Error (exn, bt) -> fail exn bt
    | exception exn -> fail exn (Printexc.get_raw_backtrace ())

  let upgrade ?max_message_size ?headers reqd session =
    let sessions =
      match Sessions.current () with
      | Some sessions -> sessions
      | None ->
        invalid_arg
          "Ache_miou.Websocket.upgrade: call it from the request handler of an \
           Ache_miou server"
    in
    let messages = Queue.create ()
    and receivers = Queue.create ()
    and pingers = Queue.create ()
    and closed = Miou.Computation.create () in
    let t = ref None in
    match
      Ache.Websocket.upgrade ?max_message_size ?headers reqd (fun connection ->
        let ws =
          { connection
          ; messages
          ; receivers
          ; pingers
          ; ended = false
          ; client_close = None
          ; closed
          }
        in
        t := Some ws;
        { on_message =
            (fun message ->
              Queue.push message messages;
              if Queue.length messages >= max_queued_messages
              then Ache.Websocket.pause_reading connection;
              wake_one receivers)
        ; on_pong = (fun (_ : string) -> end_all pingers (Ok ()))
        ; on_close =
            (fun close ->
              ws.ended <- true;
              ws.client_close <- close;
              end_all receivers (Ok ());
              end_all pingers (Error (Closed, Printexc.get_callstack 0));
              ignore (Miou.Computation.try_return closed () : bool))
        })
    with
    | Error _ as error -> error
    | Ok () ->
      let t = Option.get !t in
      Sessions.add sessions (fun () ->
        match session t with
        | () | (exception Closed) ->
          (* Nothing reads the messages any more: close, unless the connection
             already ended. *)
          Ache.Websocket.close t.connection ~code:1000
        | exception (Miou.Cancelled as exn) -> raise exn
        | exception exn ->
          let bt = Printexc.get_raw_backtrace () in
          Ache.Websocket.close t.connection ~code:1011;
          Printexc.raise_with_backtrace exn bt);
      Ok ()

  let written t =
    let flushed = Miou.Computation.create () in
    Ache.Websocket.flush t.connection (fun result ->
      ignore (Miou.Computation.try_return flushed result : bool));
    match await_or_raise flushed with `Written -> () | `Closed -> raise Closed

  let send_text t message =
    Ache.Websocket.send_text t.connection message;
    written t

  let send_binary t message =
    Ache.Websocket.send_binary t.connection message;
    written t

  let rec receive t =
    match Queue.take_opt t.messages with
    | Some message ->
      if Queue.length t.messages < max_queued_messages
      then Ache.Websocket.resume_reading t.connection;
      Some message
    | None when t.ended -> None
    | None ->
      Ache.Websocket.resume_reading t.connection;
      (match wait t.receivers with
      | () -> ()
      | exception exn ->
        let bt = Printexc.get_raw_backtrace () in
        if not (Queue.is_empty t.messages) then wake_one t.receivers;
        Printexc.raise_with_backtrace exn bt);
      receive t

  let ping t payload =
    Ache.Websocket.ping t.connection payload;
    if t.ended then raise Closed;
    wait t.pingers

  let client_close t = t.client_close

  let close ?reason t code =
    Ache.Websocket.close ?reason t.connection ~code;
    await_or_raise t.closed
end
