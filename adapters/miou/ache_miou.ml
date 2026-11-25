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

module Deadline = struct
  type t =
    { mutable at : float
    ; moved : Signal.t
    ; mutable timer : unit Miou.t option
    }

  let longest_sleep = 0.5
  let create () = { at = 0.; moved = Signal.create (); timer = None }
  let passed t = t.at <= Unix.gettimeofday ()

  let set t at ~expired =
    t.at <- at;
    Signal.wake t.moved;
    if Option.is_none t.timer
    then
      t.timer <-
        Some
          (Miou.async (fun () ->
             let rec loop () =
               Signal.wait_until t.moved (fun () -> not (passed t));
               while not (passed t) do
                 Miou_unix.sleep
                   (Float.min longest_sleep (t.at -. Unix.gettimeofday ()))
               done;
               Signal.wake expired;
               loop ()
             in
             loop ()))

  let stop t = Option.iter Miou.cancel t.timer
end

module Sessions = struct
  type t =
    { mutable pending : (unit -> unit) list
    ; mutable running : unit Miou.t list
    ; added : Signal.t
    }

  let create () = { pending = []; running = []; added = Signal.create () }

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
         t.running <-
           Miou.async (fun () ->
             match session () with
             | () -> ()
             | exception (Miou.Cancelled as exn) -> raise exn
             | exception exn -> on_exn exn (Printexc.get_raw_backtrace ()))
           :: t.running)
      (List.rev t.pending);
    t.pending <- []

  let rec serve t ~stopped ~on_exn =
    Signal.wait_until t.added (fun () ->
      t.pending <> [] || not (Miou.Computation.is_running stopped));
    start t ~on_exn;
    if Miou.Computation.is_running stopped
    then serve t ~stopped ~on_exn
    else await_or_raise stopped

  let finish t ~on_exn ~shutdown =
    start t ~on_exn;
    if t.running <> []
    then (
      shutdown ();
      let (_ : (unit, exn) result list) = Miou.await_all t.running in
      t.running <- [])
end

module type IO = Ache_miou_intf.IO
module type Server = Ache_miou_intf.Server
module type Client = Ache_miou_intf.Client

module Make_with_idle_check
    (Io : IO)
    (Idle : sig
       (* Response bytes or the end of input wait on the socket. *)
       val input_waiting : Io.socket -> bool
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

    type body_read =
      { stop : unit -> (unit -> unit) option
      ; wait : float
      ; linger : float
      }

    let no_body_read = { stop = (fun () -> None); wait = 0.; linger = 0. }

    let start : type t.
      (module Ache.Runtime.ENGINE with type t = t)
      -> read_buffer_size:int
      -> read_closed:Read_closed.t
      -> interruptible:bool
      -> timeout:
           ([ `Read | `Linger ]
            -> ([ `Idle | `Head ] * float * (unit -> unit)) option)
      -> linger:bool
      -> ?body_read:body_read
      -> ?sessions:Sessions.t
      -> stopped:[ `Done | `Abort ] Miou.Computation.t
      -> t
      -> Io.socket
      -> unit
      =
     fun (module Runtime)
       ~read_buffer_size
       ~read_closed
       ~interruptible
       ~timeout
       ~linger:lingers
       ?(body_read = no_body_read)
       ?sessions
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
        Option.iter
          (fun sessions -> Signal.wake sessions.Sessions.added)
          sessions
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
      let read_buffer = Buffer.create read_buffer_size in
      let idle_deadline = Deadline.create ()
      and head_deadline = Deadline.create () in
      let direct_reads = ref 0
      and direct_pending = ref false
      and stop_armed = ref 0
      and stop_at = ref None
      and stop_failure = ref None
      and body_stop = Signal.create () in
      let live read = !direct_pending && !direct_reads = read in
      let stop read = function
        | `Linger ->
          shutdown_once SHUTDOWN_RECEIVE;
          None
        | `Body ->
          (match body_read.stop () with
          | None -> None
          | Some on_timeout ->
            (match on_timeout () with
            | () when lingers ->
              Some (read, Unix.gettimeofday () +. body_read.linger, `Linger)
            | () ->
              shutdown_once SHUTDOWN_RECEIVE;
              None
            | exception (Miou.Cancelled as exn) -> raise exn
            | exception exn ->
              stop_failure := Some exn;
              shutdown_once SHUTDOWN_RECEIVE;
              None))
      in
      let stopper =
        if lingers
        then
          Some
            (Miou.async (fun () ->
               let rec loop () =
                 Signal.wait_until body_stop (fun () -> Option.is_some !stop_at);
                 (match !stop_at with
                 | Some (read, _, _) when not (live read) -> stop_at := None
                 | Some (read, at, stage) ->
                   let left = at -. Unix.gettimeofday () in
                   if left > 0.
                   then Miou_unix.sleep (Float.min Deadline.longest_sleep left)
                   else stop_at := stop read stage
                 | None -> ());
                 loop ()
               in
               loop ()))
        else None
      in
      let stop_body_read () =
        if
          !direct_pending
          && !stop_armed <> !direct_reads
          && Option.is_some (body_read.stop ())
        then (
          let read = !direct_reads in
          stop_armed := read;
          match stopper with
          | Some _ ->
            stop_at := Some (read, Unix.gettimeofday () +. body_read.wait, `Body);
            Signal.wake body_stop
          | None -> ignore (stop read `Body : _ option))
      in
      let direct_read_ended () =
        direct_pending := false;
        match !stop_failure with
        | None -> ()
        | Some exn ->
          stop_failure := None;
          raise exn
      in
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
      let timed_read limit =
        if (not interruptible) && Option.is_none limit && not !pending_read
        then (
          incr direct_reads;
          direct_pending := true;
          match read socket read_buffer with
          | n ->
            direct_read_ended ();
            Ok n
          | exception exn ->
            direct_read_ended ();
            raise exn)
        else (
          if Option.is_none !read_result
          then (
            if read_closed.closed then raise End_of_file;
            if not !pending_read
            then (
              pending_read := true;
              read_requested := true;
              start_reader ();
              Signal.wake request_read);
            let deadline =
              Option.bind limit (fun (kind, at, _) ->
                let deadline =
                  match kind with
                  | `Idle -> Some idle_deadline
                  | `Head -> Some head_deadline
                in
                Option.iter
                  (fun deadline ->
                     Deadline.set deadline at ~expired:read_closed.ready)
                  deadline;
                deadline)
            in
            Signal.wait_until read_closed.ready (fun () ->
              Option.is_some !read_result
              || read_closed.closed
              || Option.fold ~none:false ~some:Deadline.passed deadline));
          match !read_result with
          | Some result ->
            read_result := None;
            pending_read := false;
            (match result with Ok n -> Ok n | Error exn -> raise exn)
          | None when read_closed.closed -> raise End_of_file
          | None ->
            Option.iter (fun (_, _, on_timeout) -> on_timeout ()) limit;
            Error `Timeout)
      in
      let rec linger limit =
        match timed_read limit with
        | Ok (_ : int) ->
          let (_ : int) =
            Buffer.get read_buffer ~f:(fun _ ~off:_ ~len -> len)
          in
          linger limit
        | Error `Timeout -> if not !send_shut then linger (timeout `Linger)
        | exception End_of_file -> ()
      in
      let rec read_loop () =
        match Runtime.next_read_operation t with
        | `Read ->
          (match
             if Buffer.unread read_buffer
             then Ok 0
             else timed_read (timeout `Read)
           with
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
            if lingers then linger (timeout `Linger);
            shutdown_once SHUTDOWN_RECEIVE;
            Read_closed.close read_closed)
      in
      let rec write_loop () =
        match Runtime.next_write_operation t with
        | `Write io_vectors ->
          Runtime.report_write_result t (writev socket io_vectors);
          write_loop ()
        | `Yield ->
          stop_body_read ();
          yield Runtime.yield_writer;
          write_loop ()
        | `Close _ ->
          stop_body_read ();
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
        Option.iter Miou.cancel !reader;
        Deadline.stop idle_deadline;
        Deadline.stop head_deadline
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
      and writing =
        Miou.async (fun () ->
          write_task ();
          stop ())
      in
      let session_failed exn bt =
        if Option.is_none !unreported then unreported := Some (exn, bt)
      in
      (match
         match sessions with
         | None -> await_or_raise stopped
         | Some sessions ->
           Sessions.serve sessions ~stopped ~on_exn:session_failed
       with
      | `Abort ->
        Miou.cancel reading;
        Miou.cancel writing;
        shutdown_once SHUTDOWN_ALL
      | `Done ->
        let (_ : (unit, exn) result list) =
          Miou.await_all [ reading; writing ]
        in
        ());
      Option.iter
        (fun sessions ->
           Sessions.finish sessions ~on_exn:session_failed ~shutdown:(fun () ->
             try Runtime.shutdown t with
             | Miou.Cancelled as exn -> raise exn
             | _ -> ()))
        sessions;
      Option.iter Miou.cancel stopper;
      Option.iter
        (fun (exn, bt) -> Printexc.raise_with_backtrace exn bt)
        !unreported
  end

  module Server = struct
    type socket = Io.socket

    let linger_without_idle_timeout = 2.0

    let check_timeout name = function
      | Some seconds when not (seconds > 0.) ->
        invalid_arg
          (Printf.sprintf
             "Ache_miou.Server.create_connection_handler: %s must be positive"
             name)
      | _ -> ()

    let create_connection_handler
          ?(config = Ache.Config.default)
          ?idle_timeout
          ?head_timeout
          ~request_handler
          ~error_handler
      =
      check_timeout "idle_timeout" idle_timeout;
      check_timeout "head_timeout" head_timeout;
      let untimed =
        Option.is_none idle_timeout && Option.is_none head_timeout
      in
      fun socket client_addr ->
        let head_deadline = ref None
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
          Ache.Server_connection.create ~config ~error_handler (fun reqd ->
            head_deadline := None;
            with_sessions (fun () -> request_handler client_addr reqd))
        in
        let observe () =
          match head_timeout with
          | None -> ()
          | Some seconds ->
            (match Ache.Server_connection.read_timeout connection with
            | Some (`Head, _) ->
              if Option.is_none !head_deadline
              then head_deadline := Some (Unix.gettimeofday () +. seconds)
            | Some ((`Idle | `Body), _) | None -> head_deadline := None)
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
        let timeout = function
          | `Linger ->
            Some
              ( `Idle
              , Unix.gettimeofday ()
                +. Option.value
                     idle_timeout
                     ~default:linger_without_idle_timeout
              , ignore )
          | `Read ->
            observe ();
            (match Ache.Server_connection.read_timeout connection with
            | Some (`Idle, on_timeout) ->
              Option.map
                (fun seconds ->
                   `Idle, Unix.gettimeofday () +. seconds, on_timeout)
                idle_timeout
            | Some (`Head, on_timeout) ->
              Option.map
                (fun deadline -> `Head, deadline, on_timeout)
                !head_deadline
            | Some (`Body, on_timeout) ->
              Some
                ( `Idle
                , Unix.gettimeofday () +. Option.value idle_timeout ~default:0.
                , on_timeout )
            | None -> None)
        in
        IO_loop.start
          (module Observed)
          ~read_buffer_size:config.read_buffer_size
          ~read_closed:(Read_closed.create ())
          ~interruptible:false
          ~timeout
          ~linger:(not untimed)
          ~body_read:
            { stop =
                (fun () ->
                  match Ache.Server_connection.read_timeout connection with
                  | Some (`Body, on_timeout) -> Some on_timeout
                  | Some ((`Idle | `Head), _) | None -> None)
            ; wait = Option.value idle_timeout ~default:0.
            ; linger =
                Option.value idle_timeout ~default:linger_without_idle_timeout
            }
          ~sessions
          ~stopped:(Miou.Computation.create ())
          connection
          socket;
        Option.iter
          (fun (exn, bt) -> Printexc.raise_with_backtrace exn bt)
          !error_handler_failure
  end

  module Client = struct
    type socket = Io.socket

    type t =
      { connection : Ache.Client_connection.t
      ; runtime : Upgradable.t
      ; read_closed : Read_closed.t
      ; stopped : [ `Done | `Abort ] Miou.Computation.t
      ; task : unit Miou.t
      ; mutable awaited : bool
      ; socket : socket
      }

    let create_connection ?(config = Ache.Config.default) socket =
      let connection = Ache.Client_connection.create ~config () in
      let runtime =
        Upgradable.create
          (Ache.Runtime.make (module Ache.Client_connection) connection)
      in
      let read_closed = Read_closed.create ()
      and stopped = Miou.Computation.create () in
      let task =
        Miou.async (fun () ->
          IO_loop.start
            (module Upgradable)
            ~read_buffer_size:config.read_buffer_size
            ~read_closed
            ~interruptible:true
            ~timeout:(fun _ -> None)
            ~linger:true
            ~stopped
            runtime
            socket)
      in
      { connection
      ; runtime
      ; read_closed
      ; stopped
      ; task
      ; awaited = false
      ; socket
      }

    let request
          t
          ?flush_headers_immediately
          request
          ~error_handler
          ~response_handler
      =
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
      if not t.awaited
      then (
        t.awaited <- true;
        let (_ : (unit, exn) result) = Miou.await t.task in
        ())

    let is_closed t = Upgradable.is_closed t.runtime
    let upgrade t impl = Upgradable.upgrade t.runtime impl
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
      let input_waiting socket = input_waiting (Miou_unix.to_file_descr socket)
    end)

module Server = struct
  include Server

  let rec clean_up orphans =
    match Miou.care orphans with
    | None | Some None -> ()
    | Some (Some connection) ->
      let (_ : (unit, exn) result) = Miou.await connection in
      clean_up orphans

  let accept_pause = 0.1

  let run
        ?config
        ?idle_timeout
        ?head_timeout
        ?domains
        ?(backlog = 128)
        ?(tcp_nodelay = false)
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
    let serve () =
      let listening = socket () in
      Fun.protect ~finally:(fun () -> Miou_unix.close listening) @@ fun () ->
      Miou_unix.bind_and_listen
        ~backlog
        ~reuseaddr:true
        ~reuseport:true
        listening
        addr;
      let orphans = Miou.orphans () in
      let rec accept () =
        clean_up orphans;
        match Miou_unix.accept listening with
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
        | client, client_addr ->
          ignore
            (Miou.async ~orphans (fun () ->
               Fun.protect
                 ~finally:(fun () -> Miou_unix.close client)
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
      accept ()
    in
    let loops =
      Miou.async (fun () -> Miou.Computation.await_exn failed)
      :: Miou.async serve
      :: List.map
           (fun pin -> Miou.call ~pin serve)
           (List.filteri (fun i _ -> i < domains - 1) others)
    in
    match Miou.await_first loops with Ok () -> () | Error exn -> raise exn
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

  let upgrade ?max_message_size reqd session =
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
      Ache.Websocket.upgrade ?max_message_size reqd (fun connection ->
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
        | () | (exception Closed) -> ()
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
