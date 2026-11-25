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

module IO_loop = struct
  let writev socket iovecs =
    let lenv, cstructs =
      List.fold_left_map
        (fun acc { Ache.IOVec.buffer; off; len } ->
           acc + len, Cstruct.of_bigarray buffer ~off ~len)
        0
        iovecs
    in
    match Eio.Flow.write socket cstructs with
    | () -> `Ok lenv
    | exception
        ( End_of_file
        | Unix.Unix_error (ENOTCONN, _, _)
        | Eio.Io (Eio.Exn.X (Eio_unix.Unix_error (ENOTCONN, _, _)), _)
        | Eio.Io (Eio.Net.E (Connection_reset _), _) ) ->
      `Closed

  let read_once flow buffer =
    let result = ref 0 in
    Buffer.put
      ~f:(fun buf ~off ~len k ->
        let cstruct = Cstruct.of_bigarray buf ~off ~len in
        k (Eio.Flow.single_read flow cstruct))
      buffer
      (fun n -> result := n);
    !result

  let read flow buffer =
    match read_once flow buffer with
    | r -> r
    | exception
        ( Unix.Unix_error (ENOTCONN, _, _)
        | Eio.Io (Eio.Exn.X (Eio_unix.Unix_error (ENOTCONN, _, _)), _)
        | Eio.Io (Eio.Net.E (Connection_reset _), _) ) ->
      raise End_of_file

  let shutdown flow cmd =
    try Eio.Flow.shutdown flow cmd with
    | Eio.Cancel.Cancelled _ as exn -> raise exn
    | _ -> ()

  type body_read =
    { stop : unit -> (unit -> unit) option
    ; wait : unit -> unit
    ; linger : unit -> unit
    }

  let no_body_read = { stop = (fun () -> None); wait = ignore; linger = ignore }

  let start : type t.
    (module Ache.Runtime.ENGINE with type t = t)
    -> read_buffer_size:int
    -> ?read_closed:unit Promise.t * unit Promise.u
    -> timeout:
         ([ `Read | `Linger ]
          -> ((unit -> int) -> (int, [ `Timeout ]) result) option)
    -> linger:bool
    -> ?body_read:body_read
    -> t
    -> _ Eio.Flow.two_way
    -> unit
    =
   fun (module Runtime)
     ~read_buffer_size
     ?read_closed
     ~timeout
     ~linger:lingers
     ?(body_read = no_body_read)
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
    let direct_pending = ref false
    and direct_read_ends = ref None
    and stop_failure = ref None in
    let direct_read_ended () =
      direct_pending := false;
      (match !direct_read_ends with
      | None -> ()
      | Some end_read ->
        direct_read_ends := None;
        Promise.resolve end_read ());
      match !stop_failure with
      | None -> ()
      | Some exn ->
        stop_failure := None;
        raise exn
    in
    let stop_body_read () =
      if
        !direct_pending
        && Option.is_none !direct_read_ends
        && Option.is_some (body_read.stop ())
      then (
        let ended, end_read = Promise.create () in
        direct_read_ends := Some end_read;
        Fiber.fork_daemon ~sw (fun () ->
          Fiber.first
            (fun () -> Promise.await ended)
            (fun () ->
               body_read.wait ();
               match body_read.stop () with
               | None -> ()
               | Some on_timeout ->
                 (match on_timeout () with
                 | () ->
                   if lingers then body_read.linger ();
                   shutdown_once `Receive
                 | exception (Eio.Cancel.Cancelled _ as exn) -> raise exn
                 | exception exn ->
                   stop_failure := Some exn;
                   shutdown_once `Receive));
          `Stop_daemon))
    in
    let pending_read = ref false
    and read_requested = ref false
    and read_result = ref None
    and request_read = Eio.Condition.create ()
    and read_ready = Eio.Condition.create () in
    (* tls-eio keeps a cancelled read as a permanent error. *)
    Fiber.fork_daemon ~sw (fun () ->
      let requested () = if !read_requested then Some () else None in
      let rec reader () =
        if not !read_requested
        then Eio.Condition.loop_no_mutex request_read requested;
        read_requested := false;
        (read_result :=
           match read socket read_buffer with
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
    let read_loop =
      let ready () =
        if Option.is_some !read_result || Promise.is_resolved read_closed
        then Some ()
        else None
      in
      let daemon_read () =
        if Option.is_none !read_result
        then (
          if Promise.is_resolved read_closed then raise End_of_file;
          if not !pending_read
          then (
            pending_read := true;
            read_requested := true;
            Eio.Condition.broadcast request_read);
          Eio.Condition.loop_no_mutex read_ready ready);
        match !read_result with
        | None -> raise End_of_file
        | Some result ->
          read_result := None;
          pending_read := false;
          (match result with Ok n -> n | Error exn -> raise exn)
      in
      let read_within limit =
        match limit with
        | Some limited -> limited daemon_read
        | None when interruptible || !pending_read -> Ok (daemon_read ())
        | None ->
          direct_pending := true;
          (match read socket read_buffer with
          | n ->
            direct_read_ended ();
            Ok n
          | exception exn ->
            direct_read_ended ();
            raise exn)
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
              if lingers then linger (timeout `Linger);
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
          let write_result = writev socket io_vectors in
          Runtime.report_write_result t write_result;
          write_loop_step ()
        | `Yield ->
          stop_body_read ();
          let p, u = Promise.create () in
          Runtime.yield_writer t (fun () -> Promise.resolve u ());
          Promise.await p;
          write_loop_step ()
        | `Close _ ->
          stop_body_read ();
          shutdown_once `Send
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

  let upgrade ?max_message_size reqd session =
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
      Ache.Websocket.upgrade ?max_message_size reqd (fun connection ->
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
        | () | (exception Closed) -> ()
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
        ?idle_timeout
        ?head_timeout
        ?(date = true)
        ~request_handler
        ~error_handler
    =
    check_timeout "idle_timeout" idle_timeout;
    check_timeout "head_timeout" head_timeout;
    (match clock, idle_timeout, head_timeout with
    | None, Some _, _ | None, _, Some _ ->
      invalid_arg
        "Ache_eio.Server.create_connection_handler: a timeout needs ~clock"
    | _ -> ());
    fun socket client_addr ->
      let head_deadline = ref None
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
          (fun reqd ->
             head_deadline := None;
             request_handler client_addr reqd)
      in
      let observe () =
        match clock, head_timeout with
        | Some clock, Some seconds ->
          (match Ache.Server_connection.read_timeout connection with
          | Some (`Head, _) ->
            if Option.is_none !head_deadline
            then head_deadline := Some (Eio.Time.now clock +. seconds)
          | Some ((`Idle | `Body), _) | None -> head_deadline := None)
        | _ -> ()
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
      let timeout =
        match clock with
        | None -> IO_loop.no_timeout
        | Some clock ->
          let limited seconds on_timeout read =
            match
              Eio.Time.with_timeout clock seconds (fun () -> Ok (read ()))
            with
            | Ok n -> Ok n
            | Error `Timeout ->
              on_timeout ();
              Error `Timeout
          in
          (function
            | `Linger ->
              let deadline =
                Eio.Time.now clock
                +. Option.value
                     idle_timeout
                     ~default:linger_without_idle_timeout
              in
              Some
                (fun read ->
                  let seconds = deadline -. Eio.Time.now clock in
                  if seconds > 0.
                  then limited seconds ignore read
                  else Error `Timeout)
            | `Read ->
              observe ();
              (match Ache.Server_connection.read_timeout connection with
              | Some ((`Idle | `Body), on_timeout) ->
                Option.map (fun s -> limited s on_timeout) idle_timeout
              | Some (`Head, on_timeout) ->
                Option.map
                  (fun deadline ->
                     limited (deadline -. Eio.Time.now clock) on_timeout)
                  !head_deadline
              | None -> None))
      in
      let timeout phase =
        match phase, Ache.Server_connection.read_timeout connection with
        | `Read, Some (`Body, on_timeout) when Option.is_none idle_timeout ->
          Some
            (fun _ ->
              on_timeout ();
              Error `Timeout)
        | _ -> timeout phase
      in
      Switch.run (fun sw ->
        Fiber.with_binding
          connection_context
          { sw; session_failed = record_failure }
          (fun () ->
             IO_loop.start
               (module Observed)
               ~read_buffer_size:config.read_buffer_size
               ~timeout
               ~linger:(Option.is_some clock)
               ~body_read:
                 { stop =
                     (fun () ->
                       match Ache.Server_connection.read_timeout connection with
                       | Some (`Body, on_timeout) -> Some on_timeout
                       | Some ((`Idle | `Head), _) | None -> None)
                 ; wait =
                     (match clock, idle_timeout with
                     | Some clock, Some seconds ->
                       fun () -> Eio.Time.sleep clock seconds
                     | _ -> ignore)
                 ; linger =
                     (match clock with
                     | Some clock ->
                       fun () ->
                         Eio.Time.sleep
                           clock
                           (Option.value
                              idle_timeout
                              ~default:linger_without_idle_timeout)
                     | None -> ignore)
                 }
               connection
               socket));
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
        ?date
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
        ?date
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
      accept ()
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
    }

  let create_connection ?(config = Ache.Config.default) ~sw socket =
    let connection = Ache.Client_connection.create ~config () in
    let runtime =
      Upgradable.create
        (Ache.Runtime.make (module Ache.Client_connection) connection)
    in
    let shutdown_p, shutdown_u = Promise.create () in
    let read_closed = Promise.create () in
    Fiber.fork ~sw (fun () ->
      Fun.protect ~finally:(Promise.resolve shutdown_u) (fun () ->
        match
          IO_loop.start
            (module Upgradable)
            ~read_buffer_size:config.read_buffer_size
            ~read_closed
            ~timeout:IO_loop.no_timeout
            ~linger:true
            runtime
            socket
        with
        | () -> ()
        | exception (Eio.Cancel.Cancelled _ as exn) -> raise exn
        | exception _ -> ()));
    { connection
    ; runtime
    ; shutdown_reader =
        (fun () ->
          let cancel_reader, resolve_cancel_reader = read_closed in
          if not (Promise.is_resolved cancel_reader)
          then Promise.resolve resolve_cancel_reader ())
    ; shutdown_complete = shutdown_p
    }

  let request t = Ache.Client_connection.request t.connection

  let shutdown t =
    t.shutdown_reader ();
    Upgradable.shutdown t.runtime;
    t.shutdown_complete

  let is_closed t = Upgradable.is_closed t.runtime
  let upgrade t impl = Upgradable.upgrade t.runtime impl
end
