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

module type IO = Ache_miou_intf.IO
module type Server = Ache_miou_intf.Server
module type Client = Ache_miou_intf.Client

module Make (Io : IO) = struct
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

    let start : type t.
      (module Ache.Runtime.S with type t = t)
      -> read_buffer_size:int
      -> read_closed:Read_closed.t
      -> interruptible:bool
      -> timeout:([ `Read | `Linger ] -> (float * (unit -> unit)) option)
      -> linger:bool
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
       t
       socket ->
      let receive_shut = ref false
      and send_shut = ref false
      and failed = ref false
      and running = ref 2
      and stopped = Miou.Computation.create () in
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
      let report_exn exn =
        try Runtime.report_exn t exn with
        | Miou.Cancelled as exn -> raise exn
        | _ -> ()
      in
      let fail = function
        | Miou.Cancelled as exn -> raise exn
        | exn ->
          report_exn exn;
          failed := true;
          ignore (Miou.Computation.try_return stopped () : bool)
      in
      let yield register =
        let waker = Miou.Computation.create () in
        register t (fun () ->
          ignore (Miou.Computation.try_return waker () : bool));
        await_or_raise waker
      in
      let read_buffer = Buffer.create read_buffer_size in
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
        then Ok (read socket read_buffer)
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
            let timed_out = ref false in
            let timer =
              Option.map
                (fun (seconds, _) ->
                   Miou.async (fun () ->
                     Miou_unix.sleep seconds;
                     timed_out := true;
                     Signal.wake read_closed.ready))
                limit
            in
            Signal.wait_until read_closed.ready (fun () ->
              Option.is_some !read_result || read_closed.closed || !timed_out);
            Option.iter Miou.cancel timer);
          match !read_result with
          | Some result ->
            read_result := None;
            pending_read := false;
            (match result with Ok n -> Ok n | Error exn -> raise exn)
          | None when read_closed.closed -> raise End_of_file
          | None ->
            Option.iter (fun (_, on_timeout) -> on_timeout ()) limit;
            Error `Timeout)
      in
      let rec linger () =
        match timed_read (timeout `Linger) with
        | Ok (_ : int) ->
          let (_ : int) =
            Buffer.get read_buffer ~f:(fun _ ~off:_ ~len -> len)
          in
          linger ()
        | Error `Timeout -> if !send_shut then `Stopped else linger ()
        | exception End_of_file -> `Eof
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
            let result = if lingers then linger () else `Stopped in
            shutdown_once SHUTDOWN_RECEIVE;
            Read_closed.close read_closed;
            if result = `Eof && not !send_shut then report_exn End_of_file)
      in
      let rec write_loop () =
        match Runtime.next_write_operation t with
        | `Write io_vectors ->
          Runtime.report_write_result t (writev socket io_vectors);
          write_loop ()
        | `Yield ->
          yield Runtime.yield_writer;
          write_loop ()
        | `Close _ -> shutdown_once SHUTDOWN_SEND
      in
      let read_task () =
        (match read_loop () with () -> () | exception exn -> fail exn);
        Option.iter Miou.cancel !reader
      and write_task () =
        match write_loop () with () -> () | exception exn -> fail exn
      in
      let stop () =
        decr running;
        if !running = 0
        then ignore (Miou.Computation.try_return stopped () : bool)
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
      await_or_raise stopped;
      if !failed
      then (
        Miou.cancel reading;
        Miou.cancel writing;
        shutdown_once SHUTDOWN_ALL)
      else
        let (_ : (unit, exn) result list) =
          Miou.await_all [ reading; writing ]
        in
        ()
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
        let head_deadline = ref None in
        let connection =
          Ache.Server_connection.create
            ~config
            ~error_handler:(error_handler client_addr)
            (fun reqd ->
               head_deadline := None;
               request_handler client_addr reqd)
        in
        let observe () =
          match head_timeout with
          | None -> ()
          | Some seconds ->
            (match Ache.Server_connection.read_timeout connection with
            | Some (`Head, _) ->
              if Option.is_none !head_deadline
              then head_deadline := Some (Unix.gettimeofday () +. seconds)
            | Some (`Idle, _) | None -> head_deadline := None)
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
              ( Option.value idle_timeout ~default:linger_without_idle_timeout
              , ignore )
          | `Read when untimed -> None
          | `Read ->
            observe ();
            (match Ache.Server_connection.read_timeout connection with
            | Some (`Idle, on_timeout) ->
              Option.map (fun seconds -> seconds, on_timeout) idle_timeout
            | Some (`Head, on_timeout) ->
              Option.map
                (fun deadline ->
                   Float.max 0. (deadline -. Unix.gettimeofday ()), on_timeout)
                !head_deadline
            | None -> None)
        in
        IO_loop.start
          (module Observed)
          ~read_buffer_size:config.read_buffer_size
          ~read_closed:(Read_closed.create ())
          ~interruptible:false
          ~timeout
          ~linger:(not untimed)
          connection
          socket
  end

  module Client = struct
    type socket = Io.socket

    type t =
      { connection : Ache.Client_connection.t
      ; runtime : Upgradable.t
      ; read_closed : Read_closed.t
      ; task : unit Miou.t
      ; mutable awaited : bool
      }

    let create_connection ?(config = Ache.Config.default) socket =
      let connection = Ache.Client_connection.create ~config () in
      let runtime =
        Upgradable.create
          (Ache.Runtime.make (module Ache.Client_connection) connection)
      in
      let read_closed = Read_closed.create () in
      let task =
        Miou.async (fun () ->
          IO_loop.start
            (module Upgradable)
            ~read_buffer_size:config.read_buffer_size
            ~read_closed
            ~interruptible:true
            ~timeout:(fun _ -> None)
            ~linger:true
            runtime
            socket)
      in
      { connection; runtime; read_closed; task; awaited = false }

    let request t = Ache.Client_connection.request t.connection

    let shutdown t =
      Read_closed.close t.read_closed;
      Upgradable.shutdown t.runtime;
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

  let writev socket iovecs =
    match
      List.fold_left
        (fun total { Ache.IOVec.buffer; off; len } ->
           Miou_unix.write_bigstring socket buffer ~off ~len;
           total + len)
        0
        iovecs
    with
    | n -> `Ok n
    | exception Unix.Unix_error ((EPIPE | ECONNRESET | ENOTCONN), _, _) ->
      `Closed

  let shutdown socket command =
    try Unix.shutdown (Miou_unix.to_file_descr socket) command with
    | Unix.Unix_error _ -> ()
end

include Make (Io)
