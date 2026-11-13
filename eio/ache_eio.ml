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
    | Unix.Unix_error (ENOTCONN, _, _)
    | Eio.Io (Eio.Exn.X (Eio_unix.Unix_error (ENOTCONN, _, _)), _) ->
      ()

  let start : type t.
    (module Ache.Runtime.S with type t = t)
    -> read_buffer_size:int
    -> read_closed:unit Promise.t * unit Promise.u
    -> timed_read:
         ([ `Read | `Linger ] -> (unit -> int) -> (int, [ `Timeout ]) result)
    -> t
    -> _ Eio.Flow.two_way
    -> unit
    =
   fun (module Runtime) ~read_buffer_size ~read_closed ~timed_read t socket ->
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
    let fail = function
      | Eio.Cancel.Cancelled _ as exn -> raise exn
      | exn ->
        Runtime.report_exn t exn;
        ignore (Promise.try_resolve resolve_failed () : bool)
    in
    let read_buffer = Buffer.create read_buffer_size in
    let rec read_loop =
      let read () =
        Fiber.first
          (fun () -> read socket read_buffer)
          (fun () ->
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
                  Runtime.read_eof t buf ~off ~len)
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
              let result = linger () in
              shutdown_once `Receive;
              ignore (Promise.try_resolve resolve_read_closed () : bool);
              if result = `Eof && not !send_shut
              then Runtime.report_exn t End_of_file)
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
    let timed_read =
      match clock, idle_timeout, head_timeout with
      | _, None, None -> fun _ -> IO_loop.untimed_read
      | None, _, _ ->
        invalid_arg
          "Ache_eio.Server.create_connection_handler: a timeout needs ~clock"
      | Some clock, _, _ ->
        fun connection ->
          let head_deadline = ref None in
          fun phase read ->
            let limit =
              match phase with
              | `Linger -> Option.map (fun s -> s, ignore) idle_timeout
              | `Read ->
                (match Ache.Server_connection.read_timeout connection with
                | Some (`Idle, on_timeout) ->
                  head_deadline := None;
                  Option.map (fun s -> s, on_timeout) idle_timeout
                | Some (`Head, on_timeout) ->
                  Option.map
                    (fun s ->
                       let deadline =
                         match !head_deadline with
                         | Some deadline -> deadline
                         | None ->
                           let deadline = Eio.Time.now clock +. s in
                           head_deadline := Some deadline;
                           deadline
                       in
                       deadline -. Eio.Time.now clock, on_timeout)
                    head_timeout
                | None ->
                  head_deadline := None;
                  None)
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
    fun socket client_addr ->
      let connection =
        Ache.Server_connection.create
          ~config
          ~error_handler:(error_handler client_addr)
          (request_handler client_addr)
      in
      IO_loop.start
        (module Ache.Server_connection)
        ~read_buffer_size:config.read_buffer_size
        ~read_closed:(Promise.create ())
        ~timed_read:(timed_read connection)
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
