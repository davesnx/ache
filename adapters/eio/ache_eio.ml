(*----------------------------------------------------------------------------
  Copyright (c) 2018 Inhabited Type LLC. Copyright (c) 2018 Anton Bachin
  Copyright (c) 2019 António Nuno Monteiro. Copyright (c) 2022 António Nuno
  Monteiro

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

open Eio.Std
module Buffer = Ache.Ache_private.Runtime.Buffer
module Upgradable = Ache.Ache_private.Runtime.Upgradable

module Time = struct
  type t =
    { now : unit -> float
    ; sleep : float -> unit
    ; system : bool
    }

  let system =
    { now = Unix.gettimeofday; sleep = Eio_unix.sleep; system = true }

  let of_clock clock =
    { now = (fun () -> Eio.Time.now clock)
    ; sleep = Eio.Time.sleep clock
    ; system = false
    }

  (* [t.now ()], without allocating for the system clock. *)
  let[@inline] current t = if t.system then Unix.gettimeofday () else t.now ()
end

module IO_loop = struct
  (* A progress clock restarts after this many bytes, not after each read or
     write, so that a peer that trickles bytes cannot keep it alive. *)
  let progress_bytes = 2048

  (* The most that one timed write hands to the flow, so that a flow that writes
     all it is given (tls-eio) shows progress. *)
  let max_timed_write = 65536

  let rec take n = function
    | [] -> []
    | buf :: rest ->
      let len = Cstruct.length buf in
      if len >= n then [ Cstruct.sub buf 0 n ] else buf :: take (n - len) rest

  (* The time limits on reads, without allocation to set one. [deadline now
     `Read] is the deadline of the read that starts at [now], [Float.infinity]
     for none; it is not called when not [timed]. [ended n] runs when a read
     with a deadline ends, with the bytes read (0 when it timed out), and
     [expire] after it when its deadline passed first: it runs the core's action
     for the phase that the deadline was for, and is [false] when that phase no
     longer applies. [deadline now `Linger] bounds the linger. *)
  type timeouts =
    { timed : bool
    ; deadline : float -> [ `Read | `Linger ] -> float
    ; ended : int -> unit
    ; expire : unit -> bool
    }

  (* Floats only, so that setting one does not allocate. *)
  type deadlines =
    { mutable read : float
    ; mutable write : float
    ; mutable watchdog : float (* the deadline the watchdog sleeps to *)
    }

  exception Write_timeout

  (* The watchdog cancelled a write that had already ended. *)
  exception Late_cancel

  (* While no timeout applies to a read that waits, the watchdog looks again
     this often, so that one that applies again later starts counting. *)
  let phase_recheck = 1.0

  let iovecs_to_cstructs iovecs =
    List.fold_left_map
      (fun acc { Ache.IOVec.buffer; off; len } ->
         acc + len, Cstruct.of_bigarray buffer ~off ~len)
      0
      iovecs

  let read_once ~on_read flow buffer =
    let result = ref 0 in
    Buffer.put
      ~f:(fun buf ~off ~len k ->
        let cstruct = Cstruct.of_bigarray buf ~off ~len in
        let n = Eio.Flow.single_read flow cstruct in
        on_read buf ~off ~len:n;
        k n)
      buffer
      (fun n -> result := n);
    !result

  let read ~on_read flow buffer =
    match read_once ~on_read flow buffer with
    | r -> r
    | exception Eio.Cancel.Cancelled _ ->
      (* tls-eio raises the exception of a cancelled write (a write timeout)
         again in each later read: it is the end of the input, unless this fiber
         is cancelled. *)
      Fiber.check ();
      raise End_of_file
    | exception
        ( Unix.Unix_error (ENOTCONN, _, _)
        | Eio.Io (Eio.Exn.X (Eio_unix.Unix_error (ENOTCONN, _, _)), _)
        | Eio.Io (Eio.Net.E (Connection_reset _), _) ) ->
      raise End_of_file

  (* A deadline earlier than the one the watchdog sleeps to wakes it. *)
  let[@inline] set_read deadlines rearm deadline =
    deadlines.read <- deadline;
    if deadline < deadlines.watchdog then Eio.Condition.broadcast rearm

  let[@inline] set_write deadlines rearm deadline =
    deadlines.write <- deadline;
    if deadline < deadlines.watchdog then Eio.Condition.broadcast rearm

  let[@inline] now = function
    | Some time -> Time.current time
    | None -> Float.infinity

  let shutdown flow cmd =
    try Eio.Flow.shutdown flow cmd with
    | Eio.Cancel.Cancelled _ as exn -> raise exn
    | _ -> ()

  let start : type t.
    (module Ache.Runtime.ENGINE with type t = t)
    -> read_buffer_size:int
    -> ?read_closed:unit Promise.t * unit Promise.u
    -> ?time:Time.t
    -> timeout:timeouts
    -> linger:bool
    -> ?write_timeout:float
    -> ?interrupt:(unit -> unit) ref
    -> ?on_read:(Ache.Bigstring.t -> off:int -> len:int -> unit)
    -> t
    -> _ Eio.Flow.two_way
    -> unit
    =
   fun (module Runtime)
     ~read_buffer_size
     ?read_closed
     ?time
     ~timeout
     ~linger:lingers
     ?write_timeout
     ?interrupt
     ?(on_read = fun _ ~off:_ ~len:_ -> ())
     t
     socket ->
    Switch.run @@ fun sw ->
    let interruptible = Option.is_some read_closed in
    let read_closed, resolve_read_closed =
      match read_closed with Some p -> p | None -> Promise.create ()
    and receive_shut = ref false
    and send_shut = ref false
    and failed, resolve_failed = Promise.create ()
    and unreported = ref None in
    let shutdown_once cmd =
      let receive =
        (match cmd with `Receive | `All -> true | `Send -> false)
        && not !receive_shut
      and send =
        (match cmd with `Send | `All -> true | `Receive -> false)
        && not !send_shut
      in
      if receive then receive_shut := true;
      if send then send_shut := true;
      match receive, send with
      | true, true -> shutdown socket `All
      | true, false -> shutdown socket `Receive
      | false, true -> shutdown socket `Send
      | false, false -> ()
    in
    let report_exn exn =
      match Runtime.report_exn t exn with
      | () -> true
      | exception (Eio.Cancel.Cancelled _ as exn) -> raise exn
      | exception exn ->
        let bt = Printexc.get_raw_backtrace () in
        if Option.is_none !unreported then unreported := Some (exn, bt);
        ignore (Promise.try_resolve resolve_failed () : bool);
        false
    in
    let fail = function
      | Eio.Cancel.Cancelled _ as exn -> raise exn
      | exn ->
        let (_ : bool) = report_exn exn in
        ignore (Promise.try_resolve resolve_failed () : bool)
    in
    let read_buffer = Buffer.create read_buffer_size in
    (* The watchdog: one fiber that sleeps until the nearest deadline, so that a
       read or a write that ends in time costs no timer. A deadline that moves
       later does not wake it; it sleeps again when it wakes. *)
    let deadlines =
      { read = Float.infinity
      ; write = Float.infinity
      ; watchdog = Float.infinity
      }
    and rearm = Eio.Condition.create ()
    (* [reading]: the read loop waits in a read. [lingering]: reading ended for
       good, and reads only drop input until it ends. *)
    and reading = ref false
    and lingering = ref false
    (* The watchdog's timer ended, and it has yet to run. *)
    and timer_fired = ref false
    and expire_failed = ref None
    and write_cancel = ref None
    and write_expired = ref false in
    (* Reading ended for good while a read waits: a timeout action ran, or a
       drain closed the connection. The read is not cancelled, which tls-eio
       would keep as an error: it and the reads after it drop their input, and
       the linger deadline ends them by shutting down the receive side. *)
    let stop_reading () =
      lingering := true;
      if lingers
      then set_read deadlines rearm (timeout.deadline (now time) `Linger)
      else shutdown_once `Receive
    in
    Option.iter
      (fun time ->
         Fiber.fork_daemon ~sw (fun () ->
           let rec watch () =
             let deadline = Float.min deadlines.read deadlines.write in
             deadlines.watchdog <- deadline;
             (if deadline = Float.infinity
              then Eio.Condition.await_no_mutex rearm
              else
                let seconds = deadline -. time.Time.now () in
                if seconds > 0.
                then
                  Fiber.first
                    (fun () ->
                       time.sleep seconds;
                       timer_fired := true)
                    (fun () -> Eio.Condition.await_no_mutex rearm));
             deadlines.watchdog <- Float.infinity;
             timer_fired := false;
             let now = time.now () in
             if deadlines.read <= now
             then (
               deadlines.read <- Float.infinity;
               if !lingering
               then
                 (* The linger ends, unless the response is still being
                    written. *)
                 if !send_shut
                 then shutdown_once `Receive
                 else set_read deadlines rearm (timeout.deadline now `Linger)
               else
                 match
                   timeout.ended 0;
                   timeout.expire ()
                 with
                 | true -> stop_reading ()
                 | false ->
                   (* The phase changed while the read waited: the read goes on,
                      with the deadline of the phase now. *)
                   let deadline = timeout.deadline now `Read in
                   set_read
                     deadlines
                     rearm
                     (if deadline = Float.infinity
                      then now +. phase_recheck
                      else deadline)
                 | exception (Eio.Cancel.Cancelled _ as exn) -> raise exn
                 | exception exn ->
                   (* The read loop raises it when its read ends. *)
                   expire_failed := Some (exn, Printexc.get_raw_backtrace ());
                   lingering := true;
                   shutdown_once `Receive);
             if deadlines.write <= now
             then (
               deadlines.write <- Float.infinity;
               match !write_cancel with
               | Some cc ->
                 write_expired := true;
                 Eio.Cancel.cancel cc Write_timeout
               | None -> ());
             watch ()
           in
           watch ()))
      time;
    (* Writes [bufs], [len] bytes, with a clock that counts while the write
       waits, and restarts after [progress_bytes] written. At the deadline the
       watchdog cancels [write_cancel], around the whole write loop. *)
    let rec write_timed limit bufs len progress =
      let n =
        Eio.Flow.single_write
          socket
          (if len <= max_timed_write then bufs else take max_timed_write bufs)
      in
      if n < len
      then
        if progress + n >= progress_bytes
        then (
          set_write deadlines rearm (now time +. limit);
          write_timed limit (Cstruct.shiftv bufs n) (len - n) 0)
        else write_timed limit (Cstruct.shiftv bufs n) (len - n) (progress + n)
    in
    let write_timed limit bufs len =
      set_write deadlines rearm (now time +. limit);
      match write_timed limit bufs len 0 with
      | () -> deadlines.write <- Float.infinity
      | exception exn ->
        deadlines.write <- Float.infinity;
        raise exn
    in
    let writev iovecs =
      let lenv, cstructs = iovecs_to_cstructs iovecs in
      match
        match write_timeout with
        | None -> Eio.Flow.write socket cstructs
        | Some limit -> write_timed limit cstructs lenv
      with
      | () -> `Ok lenv
      | exception
          ( End_of_file
          | Unix.Unix_error (ENOTCONN, _, _)
          | Eio.Io (Eio.Exn.X (Eio_unix.Unix_error (ENOTCONN, _, _)), _)
          | Eio.Io (Eio.Net.E (Connection_reset _), _) ) ->
        `Closed
    in
    (* A client's read runs in a reader fiber, so that [read_closed] can end the
       wait for it: tls-eio keeps a cancelled read as a permanent error. *)
    let pending_read = ref false
    and read_requested = ref false
    and read_result = ref None
    and request_read = Eio.Condition.create ()
    and read_ready = Eio.Condition.create () in
    if interruptible
    then (
      Fiber.fork_daemon ~sw (fun () ->
        let requested () = if !read_requested then Some () else None in
        let rec reader () =
          if not !read_requested
          then Eio.Condition.loop_no_mutex request_read requested;
          read_requested := false;
          (read_result :=
             match read ~on_read socket read_buffer with
             | n -> Some (Ok n)
             | exception (Eio.Cancel.Cancelled _ as exn) -> raise exn
             | exception exn -> Some (Error exn));
          Eio.Condition.broadcast read_ready;
          reader ()
        in
        reader ());
      Fiber.fork_daemon ~sw (fun () ->
        Promise.await read_closed;
        Eio.Condition.broadcast read_ready;
        `Stop_daemon));
    Option.iter
      (fun interrupt ->
         interrupt :=
           fun () ->
             (* A drain that closes the connection ends the read that waits. *)
             if !reading && not !lingering
             then
               match Runtime.next_read_operation t with
               | `Close ->
                 deadlines.read <- Float.infinity;
                 stop_reading ()
               | `Read | `Yield -> ())
      interrupt;
    let read_loop =
      (* Waits for the reader fiber's result. *)
      let await_read () =
        if Option.is_none !read_result
        then (
          if Promise.is_resolved read_closed then raise End_of_file;
          if not !pending_read
          then (
            pending_read := true;
            read_requested := true;
            Eio.Condition.broadcast request_read);
          Eio.Condition.loop_no_mutex read_ready (fun () ->
            if Option.is_some !read_result || Promise.is_resolved read_closed
            then Some ()
            else None));
        match !read_result with
        | None -> raise End_of_file
        | Some result ->
          read_result := None;
          pending_read := false;
          (match result with Ok n -> n | Error exn -> raise exn)
      in
      let read_input () =
        if interruptible
        then await_read ()
        else read ~on_read socket read_buffer
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
      (* [Error `Timeout]: the read gave the core neither input nor its end. *)
      let timed_read () =
        (* The core reads again after a timeout action (none does): the reads
           give it their input again. *)
        if !lingering
        then (
          lingering := false;
          deadlines.read <- Float.infinity);
        let start = if timeout.timed then now time else Float.infinity in
        let deadline =
          if timeout.timed then timeout.deadline start `Read else Float.infinity
        in
        let timed = deadline < Float.infinity in
        if timed && deadline <= start
        then (
          timeout.ended 0;
          let (_ : bool) = timeout.expire () in
          Error `Timeout)
        else (
          if timed then set_read deadlines rearm deadline;
          reading := true;
          match read_input () with
          | n ->
            reading := false;
            if !lingering
            then (
              raise_expire_failed ();
              drop ();
              Error `Timeout)
            else if not timed
            then Ok n
            else
              let deadline = deadlines.read in
              deadlines.read <- Float.infinity;
              if !timer_fired && deadline <= now time
              then (
                (* Input that ends the wait after the deadline comes too late,
                   even before the watchdog runs. *)
                timeout.ended 0;
                if timeout.expire ()
                then (
                  drop ();
                  Error `Timeout)
                else Ok n)
              else (
                timeout.ended n;
                Ok n)
          | exception exn ->
            reading := false;
            if !lingering
            then (
              raise_expire_failed ();
              match exn with End_of_file -> Error `Timeout | exn -> raise exn)
            else (
              deadlines.read <- Float.infinity;
              raise exn))
      in
      let linger () =
        if not !lingering
        then (
          lingering := true;
          set_read deadlines rearm (timeout.deadline (now time) `Linger));
        let rec drop_input () =
          match read_input () with
          | (_ : int) ->
            drop ();
            drop_input ()
          | exception End_of_file -> ()
        in
        if not !receive_shut then drop_input ();
        deadlines.read <- Float.infinity
      in
      fun () ->
        let rec read_loop_step () =
          match Runtime.next_read_operation t with
          | `Read ->
            (match
               if Buffer.unread read_buffer then Ok 0 else timed_read ()
             with
            | Ok (_ : int) ->
              let (_ : int) =
                Buffer.get read_buffer ~f:(fun buf ~off ~len ->
                  Runtime.read t buf ~off ~len)
              in
              ()
            | Error `Timeout -> ()
            | exception End_of_file ->
              let (_ : int) =
                Buffer.get read_buffer ~f:(fun buf ~off ~len ->
                  let n = Runtime.read_eof t buf ~off ~len in
                  if !pending_read then 0 else n)
              in
              ());
            read_loop_step ()
          | `Yield ->
            let p, u = Promise.create () in
            Runtime.yield_reader t (fun () -> Promise.resolve u ());
            Promise.await p;
            read_loop_step ()
          | `Close ->
            if not (Promise.is_resolved read_closed)
            then (
              if lingers then linger ();
              shutdown_once `Receive;
              ignore (Promise.try_resolve resolve_read_closed () : bool))
        in
        match read_loop_step () with
        | () -> ()
        | exception (Eio.Cancel.Cancelled _ as exn) -> raise exn
        | exception exn ->
          if report_exn exn
          then (
            shutdown_once `Receive;
            ignore (Promise.try_resolve resolve_read_closed () : bool))
    in
    let write_loop () =
      let rec write_loop_step () =
        match Runtime.next_write_operation t with
        | `Write io_vectors ->
          (match writev io_vectors with
          | (`Ok _ | `Closed) as result -> Runtime.report_write_result t result);
          if !write_expired then raise Late_cancel;
          write_loop_step ()
        | `Yield ->
          let p, u = Promise.create () in
          Runtime.yield_writer t (fun () -> Promise.resolve u ());
          Promise.await p;
          write_loop_step ()
        | `Close _ -> shutdown_once `Send
      in
      (* One cancel context for all the writes, so that a write costs none. *)
      let rec timed_writes () =
        match
          Eio.Cancel.sub (fun cc ->
            write_cancel := Some cc;
            write_loop_step ())
        with
        | () -> write_cancel := None
        | exception Late_cancel ->
          (* The context stays cancelled: go on in a new one. *)
          write_cancel := None;
          write_expired := false;
          timed_writes ()
        | exception (Eio.Cancel.Cancelled _ as exn) ->
          write_cancel := None;
          Fiber.check ();
          if not !write_expired then raise exn;
          (* The write was cancelled, so nothing more is written: end the reads
             too. A TLS flow does not send close_notify after a cancelled
             write. *)
          Runtime.report_write_result t `Closed;
          shutdown_once `Receive;
          shutdown_once `Send;
          write_loop_step ()
        | exception exn ->
          write_cancel := None;
          raise exn
      in
      match
        match write_timeout with
        | None -> write_loop_step ()
        | Some _ -> timed_writes ()
      with
      | () -> ()
      | exception exn -> fail exn
    in
    Fiber.first
      (fun () -> Fiber.both read_loop write_loop)
      (fun () ->
         Promise.await failed;
         shutdown_once `All);
    Option.iter
      (fun (exn, bt) -> Printexc.raise_with_backtrace exn bt)
      !unreported

  let no_timeout =
    { timed = false
    ; deadline = (fun _ _ -> Float.infinity)
    ; ended = ignore
    ; expire = (fun () -> false)
    }
end

type connection_context =
  { sw : Switch.t
  ; session_failed : exn -> Printexc.raw_backtrace -> unit
  }

let connection_context : connection_context Fiber.key = Fiber.create_key ()

module Websocket = struct
  type t =
    { connection : Ache.Websocket.t
    ; messages : Ache.Websocket.message Queue.t
    ; changed : Eio.Condition.t
    ; mutable pongs : int
    ; mutable ended : bool
    ; mutable client_close : Ache.Websocket.close option
    }

  exception Closed

  let max_queued_messages = 8

  let upgrade ?max_message_size ?headers reqd session =
    let context =
      match Fiber.get connection_context with
      | Some context -> context
      | None ->
        invalid_arg
          "Ache_eio.Websocket.upgrade: not called in a request handler of \
           Ache_eio.Server"
    in
    let state = ref None in
    match
      Ache.Websocket.upgrade ?max_message_size ?headers reqd (fun connection ->
        let t =
          { connection
          ; messages = Queue.create ()
          ; changed = Eio.Condition.create ()
          ; pongs = 0
          ; ended = false
          ; client_close = None
          }
        in
        state := Some t;
        { on_message =
            (fun message ->
              Queue.push message t.messages;
              if Queue.length t.messages >= max_queued_messages
              then Ache.Websocket.pause_reading connection;
              Eio.Condition.broadcast t.changed)
        ; on_pong =
            (fun (_ : string) ->
              t.pongs <- t.pongs + 1;
              Eio.Condition.broadcast t.changed)
        ; on_close =
            (fun close ->
              t.ended <- true;
              t.client_close <- close;
              Eio.Condition.broadcast t.changed)
        })
    with
    | Error _ as error -> error
    | Ok () ->
      let t = Option.get !state in
      Fiber.fork ~sw:context.sw (fun () ->
        (* The fork runs at once: wait until [upgrade] has returned. *)
        Fiber.yield ();
        match session t with
        | () | (exception Closed) ->
          (* Nothing reads the messages any more: close, unless the connection
             already ended. *)
          Ache.Websocket.close t.connection ~code:1000
        | exception (Eio.Cancel.Cancelled _ as exn) -> raise exn
        | exception exn ->
          context.session_failed exn (Printexc.get_raw_backtrace ());
          Ache.Websocket.close t.connection ~code:1011);
      Ok ()

  let written t =
    let p, u = Promise.create () in
    Ache.Websocket.flush t.connection (Promise.resolve u);
    match Promise.await p with `Written -> () | `Closed -> raise Closed

  let send_text t message =
    Ache.Websocket.send_text t.connection message;
    written t

  let send_binary t message =
    Ache.Websocket.send_binary t.connection message;
    written t

  let receive t =
    Eio.Condition.loop_no_mutex t.changed (fun () ->
      Fiber.check ();
      match Queue.take_opt t.messages with
      | Some message ->
        if Queue.length t.messages < max_queued_messages
        then Ache.Websocket.resume_reading t.connection;
        Some (Some message)
      | None when t.ended -> Some None
      | None ->
        Ache.Websocket.resume_reading t.connection;
        None)

  let ping t payload =
    Ache.Websocket.ping t.connection payload;
    let pongs = t.pongs in
    Eio.Condition.loop_no_mutex t.changed (fun () ->
      if t.pongs > pongs
      then Some ()
      else if t.ended
      then raise Closed
      else None)

  let client_close t = t.client_close

  let close ?reason t code =
    Ache.Websocket.close ?reason t.connection ~code;
    Eio.Condition.loop_no_mutex t.changed (fun () ->
      if t.ended then Some () else None)
end

module Server = struct
  let linger_without_idle_timeout = 2.0
  let current_date = Ache.Ache_private.Http_date.now

  let check_timeout name = function
    | Some seconds when not (seconds > 0.) ->
      invalid_arg
        (Printf.sprintf
           "Ache_eio.Server.create_connection_handler: %s must be positive"
           name)
    | _ -> ()

  let create_connection_handler
        ?(config = Ache.Config.default)
        ?clock
        ?(idle_timeout = Some 60.)
        ?(head_timeout = Some 30.)
        ?(body_timeout = Some 60.)
        ?(write_timeout = Some 60.)
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
    check_timeout "grace" grace;
    let time =
      match clock with Some clock -> Time.of_clock clock | None -> Time.system
    in
    let keep_alive_timeout =
      match idle_timeout with
      | Some seconds when seconds >= 1. -> Some (int_of_float seconds)
      | Some _ | None -> None
    in
    fun socket client_addr ->
      let last_reqd = ref None in
      let upgraded () =
        match !last_reqd with
        | Some reqd ->
          (* [response_exn], which does not allocate: this runs per request. *)
          (match Ache.Request_descriptor.response_exn reqd with
          | { Ache.Response.status = `Switching_protocols; _ } -> true
          | _ -> false
          | exception Failure _ -> false)
        | None -> false
      in
      let head_deadline = ref None
      and body_spent = ref 0.
      and body_progress = ref 0
      and failure = ref None in
      let record_failure exn bt =
        if Option.is_none !failure then failure := Some (exn, bt)
      in
      let error_handler ?request error start_response =
        try error_handler client_addr ?request error start_response with
        | Eio.Cancel.Cancelled _ as exn -> raise exn
        | exn ->
          let bt = Printexc.get_raw_backtrace () in
          record_failure exn bt;
          Printexc.raise_with_backtrace exn bt
      in
      let connection =
        Ache.Server_connection.create
          ~config
          ~error_handler
          ?date:(if date then Some current_date else None)
          ?keep_alive_timeout
          (fun reqd ->
             last_reqd := Some reqd;
             head_deadline := None;
             body_spent := 0.;
             body_progress := 0;
             request_handler client_addr reqd)
      in
      (* The head clock starts when a read starts in the [`Head] phase, right
         after the read that brought the first bytes of a head. *)
      let observe now phase =
        match head_timeout with
        | Some seconds ->
          (match phase with
          | Some (`Head, _) ->
            if Option.is_none !head_deadline
            then head_deadline := Some (now +. seconds)
          | Some ((`Idle | `Body), _) | None ->
            if Option.is_some !head_deadline then head_deadline := None)
        | None -> ()
      in
      (* The phase whose deadline the read in progress has. Its action depends
         on the state when the deadline passes. *)
      let armed : [ `None | `Idle | `Head | `Body ] ref = ref `None in
      let expire () =
        match Ache.Server_connection.read_timeout connection with
        | Some (phase, action)
          when (phase :> [ `None | `Idle | `Head | `Body ]) = !armed ->
          action ();
          true
        | Some _ | None -> false
      in
      (* The body clock counts only while a read waits for body bytes, and
         restarts after [progress_bytes] and with each request. *)
      let body_start = ref 0. in
      let ended n =
        match !armed with
        | `Body ->
          body_spent := !body_spent +. (time.now () -. !body_start);
          body_progress := !body_progress + n;
          if !body_progress >= IO_loop.progress_bytes
          then (
            body_spent := 0.;
            body_progress := 0)
        | `Idle | `Head | `None -> ()
      in
      let interrupt = ref ignore in
      let drained = ref false in
      let drain () =
        if not !drained
        then (
          drained := true;
          Ache.Server_connection.drain connection;
          (* An idle connection closes: its read stops waiting. *)
          !interrupt ())
      in
      let deadline now = function
        | `Linger ->
          now +. Option.value idle_timeout ~default:linger_without_idle_timeout
        | `Read ->
          let phase = Ache.Server_connection.read_timeout connection in
          observe now phase;
          armed := `None;
          (match phase with
          | Some (`Idle, _) ->
            (match idle_timeout with
            | Some seconds ->
              armed := `Idle;
              (* The hint promises [keep_alive_timeout]: wait 1 s longer, so
                 that a client that trusts it does not write into a closing
                 connection. Not after an upgrade (WebSocket). *)
              let seconds =
                if Option.is_some keep_alive_timeout && not (upgraded ())
                then seconds +. 1.
                else seconds
              in
              now +. seconds
            | None -> Float.infinity)
          | Some (`Head, _) ->
            (match !head_deadline with
            | Some deadline ->
              armed := `Head;
              deadline
            | None -> Float.infinity)
          | Some (`Body, _) ->
            (match body_timeout with
            | Some seconds ->
              armed := `Body;
              body_start := now;
              now +. seconds -. !body_spent
            | None -> Float.infinity)
          | None -> Float.infinity)
      in
      let serve () =
        Switch.run (fun sw ->
          Fiber.with_binding
            connection_context
            { sw; session_failed = record_failure }
            (fun () ->
               IO_loop.start
                 (module Ache.Server_connection)
                 ~read_buffer_size:config.read_buffer_size
                 ~time
                 ~timeout:
                   { IO_loop.timed =
                       Option.is_some idle_timeout
                       || Option.is_some head_timeout
                       || Option.is_some body_timeout
                   ; deadline
                   ; ended
                   ; expire
                   }
                 ~linger:true
                 ?write_timeout
                 ~interrupt
                 connection
                 socket))
      in
      (match stop with
      | None -> serve ()
      | Some stop ->
        if Promise.is_resolved stop then drain ();
        Fiber.first serve (fun () ->
          Promise.await stop;
          drain ();
          match grace with
          | Some seconds -> time.sleep seconds
          | None -> Fiber.await_cancel ()));
      Option.iter
        (fun (exn, bt) -> Printexc.raise_with_backtrace exn bt)
        !failure

  let accept_retry_delay = 0.05

  let lasting_accept_error = function
    | Eio.Io
        ( Eio.Exn.X
            (Eio_unix.Unix_error ((EBADF | EINVAL | ENOTSOCK | EFAULT), _, _))
        , _ )
    | Eio.Io (Eio.Exn.Not_available _, _) ->
      true
    | _ -> false

  let run
        ?config
        ?clock
        ?idle_timeout
        ?head_timeout
        ?body_timeout
        ?write_timeout
        ?date
        ?stop
        ?grace
        ?(domains = Domain.recommended_domain_count ())
        ?(backlog = 128)
        ?(tcp_nodelay = false)
        ~on_error
        ~net
        ~domain_mgr
        ~request_handler
        ~error_handler
        (addr : [ `Tcp of Eio.Net.Ipaddr.v4v6 * int ])
    =
    if domains < 1
    then invalid_arg "Ache_eio.Server.run: domains must be at least 1";
    let handler =
      create_connection_handler
        ?config
        ?clock
        ?idle_timeout
        ?head_timeout
        ?body_timeout
        ?write_timeout
        ?date
        ?stop
        ?grace
        ~request_handler
        ~error_handler
    in
    let handler =
      if tcp_nodelay
      then (
        fun flow addr ->
          Option.iter
            (fun fd ->
               Eio_unix.Fd.use_exn "setsockopt" fd (fun fd ->
                 Unix.setsockopt fd TCP_NODELAY true))
            (Eio_unix.Resource.fd_opt flow);
          handler flow addr)
      else handler
    in
    let serve () =
      Switch.run @@ fun sw ->
      let socket =
        Eio.Net.listen
          ~sw
          ~reuse_addr:true
          ~reuse_port:true
          ~backlog
          net
          (addr :> Eio.Net.Sockaddr.stream)
      in
      let rec accept () =
        (match Eio.Net.accept_fork ~sw socket ~on_error handler with
        | () -> ()
        | exception (Eio.Io _ as exn) when not (lasting_accept_error exn) ->
          on_error exn;
          Eio_unix.sleep accept_retry_delay);
        accept ()
      in
      match stop with
      | None -> accept ()
      | Some stop ->
        Fiber.first accept (fun () -> Promise.await stop);
        Eio.Resource.close socket
    in
    Fiber.all
      (serve
      :: List.init (domains - 1) (fun _ () ->
        Eio.Domain_manager.run domain_mgr serve))
end

module Client = struct
  type t =
    { connection : Ache.Client_connection.t
    ; runtime : Upgradable.t
    ; shutdown_reader : unit -> unit
    ; shutdown_complete : unit Promise.t
    ; input_waiting : unit -> bool
    ; unread : bool ref
    ; reuse_deadline : float ref
    }

  let rec has_response_byte buf i stop =
    i < stop
    &&
    match Ache.Bigstring.unsafe_get buf i with
    | '\r' | '\n' -> has_response_byte buf (i + 1) stop
    | _ -> true

  (* Bytes other than empty lines, or the end of input, that wait on [fd]. Only
     for a non-blocking [fd], so that [recv] does not wait. *)
  let socket_input_waiting fd =
    Eio_unix.Fd.use
      fd
      ~if_closed:(fun () -> false)
      (fun fd ->
         let buf = Bytes.create 64 in
         match Unix.recv fd buf 0 (Bytes.length buf) [ Unix.MSG_PEEK ] with
         | n ->
           n = 0
           || Bytes.exists
                (function '\r' | '\n' -> false | _ -> true)
                (Bytes.sub buf 0 n)
         | exception Unix.Unix_error ((ECONNRESET | ECONNABORTED | EPIPE), _, _)
           ->
           true
         | exception Unix.Unix_error _ -> false)

  let non_blocking fd =
    match Eio_unix.Fd.is_blocking fd with
    | blocking -> not blocking
    | exception _ -> false

  let close_now t =
    t.shutdown_reader ();
    Upgradable.shutdown t.runtime

  let create_connection
        ?(config = Ache.Config.default)
        ?input_waiting
        ~sw
        socket
    =
    let connection = Ache.Client_connection.create ~config () in
    let runtime =
      Upgradable.create
        (Ache.Runtime.make (module Ache.Client_connection) connection)
    in
    let shutdown_p, shutdown_u = Promise.create () in
    let read_closed = Promise.create () in
    (* [unread]: a read returned response bytes that the connection has not been
       given yet. The read runs in its own fiber, so the read loop gets them
       later. *)
    let unread = ref false in
    let reuse_deadline = ref Float.infinity
    and watchdog_target = ref Float.infinity
    and rearm = Eio.Condition.create () in
    let t =
      { connection
      ; runtime
      ; shutdown_reader =
          (fun () ->
            let cancel_reader, resolve_cancel_reader = read_closed in
            if not (Promise.is_resolved cancel_reader)
            then Promise.resolve resolve_cancel_reader ())
      ; shutdown_complete = shutdown_p
      ; input_waiting =
          (match input_waiting, Eio_unix.Resource.fd_opt socket with
          | Some input_waiting, _ -> input_waiting
          | None, Some fd when non_blocking fd ->
            fun () -> socket_input_waiting fd
          | None, (Some _ | None) -> fun () -> false)
      ; unread
      ; reuse_deadline
      }
    in
    (* The server's [Keep-Alive: timeout] hint: the connection is not reused
       once [reuse_timeout] seconds have passed since the response ended. One
       watchdog fiber closes it then. *)
    let observe () =
      match
        if Ache.Client_connection.is_closed connection
        then None (* closed, or upgraded *)
        else Ache.Client_connection.reuse_timeout connection
      with
      | Some seconds when Float.equal !reuse_deadline Float.infinity ->
        reuse_deadline := Unix.gettimeofday () +. seconds;
        if !reuse_deadline < !watchdog_target then Eio.Condition.broadcast rearm
      | Some _ -> ()
      | None -> reuse_deadline := Float.infinity
    in
    let module Observed = struct
      include Upgradable

      let next_read_operation t =
        let operation = next_read_operation t in
        observe ();
        operation

      let read t bs ~off ~len =
        unread := false;
        let n = read t bs ~off ~len in
        observe ();
        n

      let read_eof t bs ~off ~len =
        unread := false;
        let n = read_eof t bs ~off ~len in
        observe ();
        n
    end
    in
    Fiber.fork_daemon ~sw (fun () ->
      let rec watch () =
        let deadline = !reuse_deadline in
        watchdog_target := deadline;
        (if Float.equal deadline Float.infinity
         then Eio.Condition.await_no_mutex rearm
         else
           let seconds = deadline -. Unix.gettimeofday () in
           if seconds > 0.
           then
             Fiber.first
               (fun () -> Eio_unix.sleep seconds)
               (fun () -> Eio.Condition.await_no_mutex rearm));
        watchdog_target := Float.infinity;
        if !reuse_deadline <= Unix.gettimeofday ()
        then close_now t
        else watch ()
      in
      Fiber.first watch (fun () -> Promise.await shutdown_p);
      `Stop_daemon);
    Fiber.fork ~sw (fun () ->
      Fun.protect ~finally:(Promise.resolve shutdown_u) (fun () ->
        match
          IO_loop.start
            (module Observed)
            ~read_buffer_size:config.read_buffer_size
            ~read_closed
            ~timeout:IO_loop.no_timeout
            ~linger:true
            ~on_read:(fun buf ~off ~len ->
              if has_response_byte buf off (off + len) then unread := true)
            runtime
            socket
        with
        | () -> ()
        | exception (Eio.Cancel.Cancelled _ as exn) -> raise exn
        | exception _ -> ()));
    t

  let request
        t
        ?flush_headers_immediately
        request
        ~error_handler
        ~response_handler
    =
    (* Past the [Keep-Alive] deadline, the request gets [`Connection_closed] and
       nothing is written. *)
    if !(t.reuse_deadline) <= Unix.gettimeofday () then close_now t;
    (* Response bytes that wait on an idle connection would be taken as the
       response to this request (undici GHSA-35p6-xmwp-9g52): close the
       connection instead. io_uring completes a read before its fiber runs:
       yield so that a complete read returns, then look at what reads returned
       and at the socket. *)
    let waiting () =
      Ache.Ache_private.Client_connection.is_waiting t.connection
    in
    if waiting ()
    then (
      Fiber.yield ();
      Fiber.yield ();
      if waiting () && (!(t.unread) || t.input_waiting ()) then close_now t);
    t.reuse_deadline := Float.infinity;
    Ache.Client_connection.request
      t.connection
      ?flush_headers_immediately
      request
      ~error_handler
      ~response_handler

  let shutdown t =
    close_now t;
    t.shutdown_complete

  let is_closed t = Upgradable.is_closed t.runtime
  let upgrade t impl = Upgradable.upgrade t.runtime impl
end
