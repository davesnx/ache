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
    let p, u = Promise.create () in
    Buffer.put
      ~f:(fun buf ~off ~len k ->
        let cstruct = Cstruct.of_bigarray buf ~off ~len in
        k (Eio.Flow.single_read flow cstruct))
      buffer
      (Promise.resolve u);
    Promise.await p

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

  let start : type t.
    (module Ache.Runtime.S with type t = t)
    -> read_buffer_size:int
    -> read_closed:unit Promise.t * unit Promise.u
    -> timed_read:
         ([ `Read | `Linger ] -> (unit -> int) -> (int, [ `Timeout ]) result)
    -> linger:bool
    -> t
    -> _ Eio.Flow.two_way
    -> unit
    =
   fun (module Runtime)
     ~read_buffer_size
     ~read_closed
     ~timed_read
     ~linger:lingers
     t
     socket ->
    Switch.run @@ fun sw ->
    let read_closed, resolve_read_closed = read_closed
    and receive_shut = ref false
    and send_shut = ref false
    and failed, resolve_failed = Promise.create () in
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
      try Runtime.report_exn t exn with
      | Eio.Cancel.Cancelled _ as exn -> raise exn
      | _ -> ()
    in
    let fail = function
      | Eio.Cancel.Cancelled _ as exn -> raise exn
      | exn ->
        report_exn exn;
        ignore (Promise.try_resolve resolve_failed () : bool)
    in
    let read_buffer = Buffer.create read_buffer_size in
    let pending_read = ref None in
    let read_socket () =
      let result =
        match !pending_read with
        | Some result -> result
        | None ->
          let result, resolve = Promise.create () in
          pending_read := Some result;
          Fiber.fork_daemon ~sw (fun () ->
            (match read socket read_buffer with
            | n -> Promise.resolve_ok resolve n
            | exception (Eio.Cancel.Cancelled _ as exn) -> raise exn
            | exception exn -> Promise.resolve_error resolve exn);
            `Stop_daemon);
          result
      in
      let result = Promise.await result in
      pending_read := None;
      match result with Ok n -> n | Error exn -> raise exn
    in
    let rec read_loop =
      let read () =
        Fiber.first read_socket (fun () ->
          Promise.await read_closed;
          raise End_of_file)
      in
      let rec linger () =
        match timed_read `Linger read with
        | Ok (_ : int) ->
          let (_ : int) =
            Buffer.get read_buffer ~f:(fun _ ~off:_ ~len -> len)
          in
          linger ()
        | Error `Timeout -> if !send_shut then `Stopped else linger ()
        | exception End_of_file -> `Eof
      in
      fun () ->
        let rec read_loop_step () =
          match Runtime.next_read_operation t with
          | `Read ->
            (match
               if Buffer.unread read_buffer then Ok 0 else timed_read `Read read
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
                  if Option.is_some !pending_read then 0 else n)
              in
              ());
            read_loop_step ()
          | `Yield ->
            let p, u = Promise.create () in
            Runtime.yield_reader t (fun () -> Promise.resolve u ());
            Promise.await p;
            read_loop ()
          | `Close ->
            if not (Promise.is_resolved read_closed)
            then (
              let result = if lingers then linger () else `Stopped in
              shutdown_once `Receive;
              ignore (Promise.try_resolve resolve_read_closed () : bool);
              if result = `Eof && not !send_shut then report_exn End_of_file)
        in
        match read_loop_step () with () -> () | exception exn -> fail exn
    in
    let rec write_loop () =
      let rec write_loop_step () =
        match Runtime.next_write_operation t with
        | `Write io_vectors ->
          let write_result = writev socket io_vectors in
          Runtime.report_write_result t write_result;
          write_loop_step ()
        | `Yield ->
          let p, u = Promise.create () in
          Runtime.yield_writer t (fun () -> Promise.resolve u ());
          Promise.await p;
          write_loop ()
        | `Close _ -> shutdown_once `Send
      in
      match write_loop_step () with () -> () | exception exn -> fail exn
    in
    Fiber.first
      (fun () -> Fiber.both read_loop write_loop)
      (fun () ->
         Promise.await failed;
         shutdown_once `All)

  let untimed_read _ read = Ok (read ())
end

module Server = struct
  let linger_without_idle_timeout = 2.0

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
        match clock, head_timeout with
        | Some clock, Some seconds ->
          (match Ache.Server_connection.read_timeout connection with
          | Some (`Head, _) ->
            if Option.is_none !head_deadline
            then head_deadline := Some (Eio.Time.now clock +. seconds)
          | Some (`Idle, _) | None -> head_deadline := None)
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
      let timed_read =
        match clock with
        | None -> IO_loop.untimed_read
        | Some clock ->
          fun phase read ->
            let limit =
              match phase with
              | `Linger ->
                Some
                  ( Option.value
                      idle_timeout
                      ~default:linger_without_idle_timeout
                  , ignore )
              | `Read ->
                observe ();
                (match Ache.Server_connection.read_timeout connection with
                | Some (`Idle, on_timeout) ->
                  Option.map (fun s -> s, on_timeout) idle_timeout
                | Some (`Head, on_timeout) ->
                  Option.map
                    (fun deadline -> deadline -. Eio.Time.now clock, on_timeout)
                    !head_deadline
                | None -> None)
            in
            (match limit with
            | None -> Ok (read ())
            | Some (seconds, on_timeout) ->
              (match
                 Eio.Time.with_timeout clock seconds (fun () -> Ok (read ()))
               with
              | Ok n -> Ok n
              | Error `Timeout ->
                on_timeout ();
                Error `Timeout))
      in
      IO_loop.start
        (module Observed)
        ~read_buffer_size:config.read_buffer_size
        ~read_closed:(Promise.create ())
        ~timed_read
        ~linger:(Option.is_some clock)
        connection
        socket
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
        IO_loop.start
          (module Upgradable)
          ~read_buffer_size:config.read_buffer_size
          ~read_closed
          ~timed_read:IO_loop.untimed_read
          ~linger:true
          runtime
          socket));
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
