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
    }

  let system = { now = Unix.gettimeofday; sleep = Eio_unix.sleep }

  let of_clock clock =
    { now = (fun () -> Eio.Time.now clock); sleep = Eio.Time.sleep clock }
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

  (* A limit on one wait for input: [ended n] runs when the wait ends, with the
     bytes read (0 when none), and [expire] after it when [deadline] passed
     first. *)
  type limit =
    { deadline : float
    ; expire : unit -> unit
    ; ended : int -> unit
    }

  exception Write_timeout

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

  let shutdown flow cmd =
    try Eio.Flow.shutdown flow cmd with
    | Eio.Cancel.Cancelled _ as exn -> raise exn
    | _ -> ()

  let start : type t.
    (module Ache.Runtime.ENGINE with type t = t)
    -> read_buffer_size:int
    -> ?read_closed:unit Promise.t * unit Promise.u
    -> ?time:Time.t
    -> timeout:([ `Read | `Linger ] -> limit option)
    -> linger:(unit -> bool)
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
    let pending_read = ref false
    and read_requested = ref false
    and read_result = ref None
    and request_read = Eio.Condition.create ()
    and read_ready = Eio.Condition.create () in
    (* The watchdog: one fiber that sleeps until the nearest deadline, so that a
       read or a write that ends in time costs no timer. A deadline that moves
       later does not wake it; it sleeps again when it wakes. *)
    let read_deadline = ref Float.infinity
    and write_deadline = ref Float.infinity
    and read_expired = ref false
    and read_interrupted = ref false
    and write_cancel = ref None
    and write_expired = ref false
    and watchdog_target = ref Float.infinity
    and rearm = Eio.Condition.create () in
    let arm deadline_ref deadline =
      deadline_ref := deadline;
      if deadline < !watchdog_target then Eio.Condition.broadcast rearm
    in
    Option.iter
      (fun time ->
         Fiber.fork_daemon ~sw (fun () ->
           let rec watch () =
             let deadline = Float.min !read_deadline !write_deadline in
             watchdog_target := deadline;
             (if deadline = Float.infinity
              then Eio.Condition.await_no_mutex rearm
              else
                let seconds = deadline -. time.Time.now () in
                if seconds > 0.
                then
                  Fiber.first
                    (fun () -> time.sleep seconds)
                    (fun () -> Eio.Condition.await_no_mutex rearm));
             watchdog_target := Float.infinity;
             let now = time.now () in
             if !read_deadline <= now
             then (
               read_deadline := Float.infinity;
               read_expired := true;
               Eio.Condition.broadcast read_ready);
             if !write_deadline <= now
             then (
               write_deadline := Float.infinity;
               match !write_cancel with
               | Some cc ->
                 write_expired := true;
                 Eio.Cancel.cancel cc Write_timeout
               | None -> ());
             watch ()
           in
           watch ()))
      time;
    let now () =
      match time with Some time -> time.now () | None -> Float.infinity
    in
    (* Writes [bufs] with a clock that counts while the write waits, and
       restarts after [progress_bytes] written. *)
    let write_timed limit bufs =
      write_expired := false;
      match
        Eio.Cancel.sub (fun cc ->
          write_cancel := Some cc;
          arm write_deadline (now () +. limit);
          let progress = ref 0 in
          let rec loop = function
            | [] -> ()
            | bufs ->
              let n =
                Eio.Flow.single_write socket (take max_timed_write bufs)
              in
              progress := !progress + n;
              if !progress >= progress_bytes
              then (
                progress := 0;
                arm write_deadline (now () +. limit));
              loop (Cstruct.shiftv bufs n)
          in
          loop bufs)
      with
      | () ->
        write_cancel := None;
        write_deadline := Float.infinity
      | exception (Eio.Cancel.Cancelled _ as exn) ->
        write_cancel := None;
        write_deadline := Float.infinity;
        Fiber.check ();
        if !write_expired then raise Write_timeout else raise exn
      | exception exn ->
        write_cancel := None;
        write_deadline := Float.infinity;
        raise exn
    in
    let writev iovecs =
      let lenv, cstructs = iovecs_to_cstructs iovecs in
      match
        match write_timeout with
        | None -> Eio.Flow.write socket cstructs
        | Some limit -> write_timed limit cstructs
      with
      | () -> `Ok lenv
      | exception Write_timeout -> `Timeout
      | exception
          ( End_of_file
          | Unix.Unix_error (ENOTCONN, _, _)
          | Eio.Io (Eio.Exn.X (Eio_unix.Unix_error (ENOTCONN, _, _)), _)
          | Eio.Io (Eio.Net.E (Connection_reset _), _) ) ->
        `Closed
    in
    (* tls-eio keeps a cancelled read as a permanent error. *)
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
      `Stop_daemon);
    Option.iter
      (fun interrupt ->
         interrupt :=
           fun () ->
             read_interrupted := true;
             Eio.Condition.broadcast read_ready)
      interrupt;
    let read_loop =
      (* Waits for the reader fiber's result, or for the watchdog or an
         interrupt when [limited]. *)
      let await_read ~limited =
        if Option.is_none !read_result
        then (
          if Promise.is_resolved read_closed then raise End_of_file;
          if not !pending_read
          then (
            pending_read := true;
            read_requested := true;
            Eio.Condition.broadcast request_read);
          Eio.Condition.loop_no_mutex read_ready (fun () ->
            if
              (limited && (!read_expired || !read_interrupted))
              || Option.is_some !read_result
              || Promise.is_resolved read_closed
            then Some ()
            else None));
        if limited && !read_expired
        then `Expired
        else if limited && !read_interrupted
        then `Interrupted
        else
          match !read_result with
          | None -> raise End_of_file
          | Some result ->
            read_result := None;
            pending_read := false;
            (match result with Ok n -> `Read n | Error exn -> raise exn)
      in
      let daemon_read () =
        match await_read ~limited:false with
        | `Read n -> n
        | `Expired | `Interrupted -> assert false
      in
      let timed limit =
        if limit.deadline <= now ()
        then (
          limit.ended 0;
          limit.expire ();
          Error `Timeout)
        else (
          read_expired := false;
          read_interrupted := false;
          arm read_deadline limit.deadline;
          match await_read ~limited:true with
          | `Read n ->
            read_deadline := Float.infinity;
            limit.ended n;
            Ok n
          | `Expired ->
            limit.ended 0;
            limit.expire ();
            Error `Timeout
          | `Interrupted ->
            read_deadline := Float.infinity;
            limit.ended 0;
            Error `Timeout
          | exception exn ->
            read_deadline := Float.infinity;
            raise exn)
      in
      let read_within limit =
        match limit with
        | Some limit -> timed limit
        | None when interruptible || !pending_read -> Ok (daemon_read ())
        | None -> Ok (read ~on_read socket read_buffer)
      in
      let timed_read phase = read_within (timeout phase) in
      let rec linger limit =
        match read_within limit with
        | Ok (_ : int) ->
          let (_ : int) =
            Buffer.get read_buffer ~f:(fun _ ~off:_ ~len -> len)
          in
          linger limit
        | Error `Timeout -> if not !send_shut then linger (timeout `Linger)
        | exception End_of_file -> ()
      in
      fun () ->
        let rec read_loop_step () =
          match Runtime.next_read_operation t with
          | `Read ->
            (match
               if Buffer.unread read_buffer then Ok 0 else timed_read `Read
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
              if lingers () then linger (timeout `Linger);
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
          | (`Ok _ | `Closed) as result -> Runtime.report_write_result t result
          | `Timeout ->
            (* The write was cancelled, so nothing more is written: end the
               reads too. A TLS flow does not send close_notify after a
               cancelled write. *)
            Runtime.report_write_result t `Closed;
            shutdown_once `Receive;
            shutdown_once `Send);
          write_loop_step ()
        | `Yield ->
          let p, u = Promise.create () in
          Runtime.yield_writer t (fun () -> Promise.resolve u ());
          Promise.await p;
          write_loop_step ()
        | `Close _ -> shutdown_once `Send
      in
      match write_loop_step () with () -> () | exception exn -> fail exn
    in
    Fiber.first
      (fun () -> Fiber.both read_loop write_loop)
      (fun () ->
         Promise.await failed;
         shutdown_once `All);
    Option.iter
      (fun (exn, bt) -> Printexc.raise_with_backtrace exn bt)
      !unreported

  let no_timeout _ = None
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
        ?(close_timeout = Some 10.)
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
    check_timeout "close_timeout" close_timeout;
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
          (match Ache.Request_descriptor.response reqd with
          | Some { Ache.Response.status = `Switching_protocols; _ } -> true
          | Some _ | None -> false)
        | None -> false
      in
      (* Only a WebSocket ([Ache.Websocket.upgrade]) reaches [`Close]: plain
         HTTP and other protocols skip the close timeout. *)
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
          (match Ache.Request_descriptor.response reqd with
          | Some { Ache.Response.status; headers; _ } ->
            checked_reqd := Some reqd;
            (websocket :=
               status = `Switching_protocols
               &&
               match Ache.Headers.get headers "upgrade" with
               | Some value ->
                 String.lowercase_ascii (String.trim value) = "websocket"
               | None -> false);
            !websocket
          | None -> false)
        | Some _ | None -> false
      in
      let head_deadline = ref None
      and close_deadline = ref None
      and close_expired = ref false
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
      let interrupt = ref ignore in
      (* A fixed deadline: the messages that the client still sends do not
         extend it, and a connection closes only once. A read that waits starts
         again, to take it. *)
      let start_close () =
        match close_timeout, !close_deadline with
        | Some seconds, None ->
          close_deadline := Some (time.now () +. seconds);
          !interrupt ()
        | Some _, Some _ | None, _ -> ()
      in
      let watch_close () = Option.is_some close_timeout && is_websocket () in
      let observe_close () =
        match Ache.Server_connection.read_timeout connection with
        | Some (`Close, _) -> start_close ()
        | Some ((`Idle | `Head | `Body), _) | None -> ()
      in
      (* An upgraded connection has no request head. *)
      let observe () =
        if watch_close ()
        then observe_close ()
        else
          match head_timeout with
          | Some seconds ->
            (match Ache.Server_connection.read_timeout connection with
            | Some (`Head, _) ->
              if Option.is_none !head_deadline
              then head_deadline := Some (time.now () +. seconds)
            | Some ((`Idle | `Body | `Close), _) | None -> head_deadline := None)
          | None -> ()
      in
      (* The server's close frame is queued from any fiber (a session, a
         callback) while a read may wait: the writer sees it first. *)
      let observe_write () = if watch_close () then observe_close () in
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

        let next_write_operation t =
          let operation = next_write_operation t in
          observe_write ();
          operation
      end
      in
      (* The action of a timeout depends on the state when it fires. *)
      let expire phase () =
        match Ache.Server_connection.read_timeout connection with
        | Some (phase', action) when phase' = phase ->
          (* Our close frame is written: close at once, without the linger. *)
          if phase = `Close then close_expired := true;
          action ()
        | Some _ | None -> ()
      in
      let limit deadline phase =
        { IO_loop.deadline; expire = expire phase; ended = ignore }
      in
      (* The body clock counts only while a read waits for body bytes, and
         restarts after [progress_bytes] and with each request. *)
      let body_limit seconds =
        let start = time.now () in
        { IO_loop.deadline = start +. seconds -. !body_spent
        ; expire = expire `Body
        ; ended =
            (fun n ->
              body_spent := !body_spent +. (time.now () -. start);
              body_progress := !body_progress + n;
              if !body_progress >= IO_loop.progress_bytes
              then (
                body_spent := 0.;
                body_progress := 0))
        }
      in
      let drained = ref false in
      let drain () =
        if not !drained
        then (
          drained := true;
          Ache.Server_connection.drain connection;
          (* An idle connection closes: its read stops waiting. *)
          !interrupt ())
      in
      let timeout = function
        | `Linger ->
          let deadline =
            time.now ()
            +. Option.value idle_timeout ~default:linger_without_idle_timeout
          in
          Some { IO_loop.deadline; expire = ignore; ended = ignore }
        | `Read ->
          observe ();
          (match Ache.Server_connection.read_timeout connection with
          | Some (`Idle, _) ->
            (match idle_timeout with
            | Some seconds ->
              (* The hint promises [keep_alive_timeout]: wait 1 s longer, so
                 that a client that trusts it does not write into a closing
                 connection. Not after an upgrade (WebSocket). *)
              let seconds =
                if Option.is_some keep_alive_timeout && not (upgraded ())
                then seconds +. 1.
                else seconds
              in
              Some (limit (time.now () +. seconds) `Idle)
            | None when (Option.is_some stop && not !drained) || watch_close ()
              ->
              (* Only so that a drain, or the server's close frame that starts
                 the close timeout, can stop the wait. *)
              Some (limit Float.infinity `Idle)
            | None -> None)
          | Some (`Head, _) ->
            Option.map (fun deadline -> limit deadline `Head) !head_deadline
          | Some (`Body, _) -> Option.map body_limit body_timeout
          | Some (`Close, _) ->
            Option.map (fun deadline -> limit deadline `Close) !close_deadline
          | None -> None)
      in
      let serve () =
        Switch.run (fun sw ->
          Fiber.with_binding
            connection_context
            { sw; session_failed = record_failure }
            (fun () ->
               IO_loop.start
                 (module Observed)
                 ~read_buffer_size:config.read_buffer_size
                 ~time
                 ~timeout
                 ~linger:(fun () -> not !close_expired)
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
        ?close_timeout
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
        ?close_timeout
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
    ; fd : Eio_unix.Fd.t option
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
  let input_waiting fd =
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

  let create_connection ?(config = Ache.Config.default) ~sw socket =
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
      ; fd =
          (match Eio_unix.Resource.fd_opt socket with
          | Some fd when non_blocking fd -> Some fd
          | Some _ | None -> None)
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
            ~linger:(fun () -> true)
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
      if
        waiting ()
        && (!(t.unread)
           || match t.fd with Some fd -> input_waiting fd | None -> false)
      then close_now t);
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
