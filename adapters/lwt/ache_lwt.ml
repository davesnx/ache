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
    -> fd
    -> unit Lwt.t
    =
   fun (module Io)
     (module Runtime)
     t
     ~read_buffer_size
     ~timed_read
     ~close
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
                else timed_read (fun () -> read (module Io) socket read_buffer))
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
    let writev = Io.writev socket in
    let write_loop_exited, notify_write_loop_exited = Lwt.wait () in
    let rec write_loop () =
      let rec write_loop_step () =
        match Runtime.next_write_operation t with
        | `Write io_vectors ->
          writev io_vectors >>= fun result ->
          Runtime.report_write_result t result;
          write_loop_step ()
        | `Yield ->
          Runtime.yield_writer t write_loop;
          Lwt.return_unit
        | `Close _ ->
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

    let create_connection_handler
          ?(config = Ache.Config.default)
          ?idle_timeout
          ?head_timeout
          ~request_handler
          ~error_handler
          client_addr
          socket
      =
      check_timeout "idle_timeout" idle_timeout;
      check_timeout "head_timeout" head_timeout;
      let head_timer = Option.map Timeout.create head_timeout
      and head_armed = ref false
      and idle_timer = Option.map Timeout.create idle_timeout in
      let stop_head_timer () =
        head_armed := false;
        Option.iter Timeout.stop head_timer
      in
      let connection =
        Ache.Server_connection.create
          ~config
          ~error_handler:(error_handler client_addr)
          (fun reqd ->
             stop_head_timer ();
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
          | Some (`Idle, _) | None -> stop_head_timer ())
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
      let timed_read read =
        observe ();
        let reading = IO_loop.untimed_read read in
        let limit =
          if not (Lwt.is_sleeping reading)
          then None
          else
            match
              Ache.Server_connection.read_timeout connection, idle_timer
            with
            | Some (`Idle, on_timeout), Some timer ->
              Timeout.set timer;
              Some (timer, on_timeout)
            | Some (`Head, on_timeout), _ when !head_armed ->
              Option.map (fun timer -> timer, on_timeout) head_timer
            | (Some ((`Idle | `Head), _) | None), _ -> None
        in
        match limit with
        | None -> reading
        | Some (timer, on_timeout) ->
          Lwt.pick
            [ reading
            ; ( Timeout.expired timer >|= fun () ->
                on_timeout ();
                Error `Timeout )
            ]
      in
      let timed_read =
        match idle_timeout, head_timeout with
        | None, None -> IO_loop.untimed_read
        | _ -> timed_read
      in
      Lwt.finalize
        (fun () ->
           IO_loop.start
             (module Io)
             (module Observed)
             connection
             ~read_buffer_size:config.read_buffer_size
             ~timed_read
             ~close:(IO_loop.close_once Io.close socket)
             socket)
        (fun () ->
           Option.iter Timeout.stop head_timer;
           Option.iter Timeout.stop idle_timer;
           Lwt.return_unit)
  end

  module Client = struct
    type socket = Io.socket

    type t =
      { connection : Ache.Client_connection.t
      ; runtime : Upgradable.t
      ; close : unit -> unit Lwt.t
      }

    let create_connection ?(config = Ache.Config.default) socket =
      let connection = Ache.Client_connection.create ~config () in
      let runtime =
        Upgradable.create
          (Ache.Runtime.make (module Ache.Client_connection) connection)
      in
      let close = IO_loop.close_once Io.close socket in
      Lwt.async (fun () ->
        IO_loop.start
          (module Io)
          (module Upgradable)
          runtime
          ~read_buffer_size:config.read_buffer_size
          ~timed_read:IO_loop.untimed_read
          ~close
          socket);
      Lwt.return { connection; runtime; close }

    let request t = Ache.Client_connection.request t.connection

    let shutdown t =
      Upgradable.shutdown t.runtime;
      t.close ()

    let is_closed t = Upgradable.is_closed t.runtime
    let upgrade t impl = Upgradable.upgrade t.runtime impl
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
