(* curl -v -p -x http://localhost:8080 http://example.com *)
open Ache

type tunnel =
  { upstream : Eio.Flow.two_way_ty Eio.Resource.t
  ; mutable to_client : string
  ; mutable client_done : bool
  ; mutable upstream_done : bool
  ; mutable writer : (unit -> unit) option
  ; written : Eio.Condition.t
  ; finished : unit Eio.Promise.t * unit Eio.Promise.u
  }

module Tunnel = struct
  type t = tunnel

  let wake_writer t =
    match t.writer with
    | Some k ->
      t.writer <- None;
      k ()
    | None -> ()

  let finish t =
    t.client_done <- true;
    t.upstream_done <- true;
    ignore (Eio.Promise.try_resolve (snd t.finished) () : bool);
    wake_writer t;
    Eio.Condition.broadcast t.written

  let next_read_operation t = if t.client_done then `Close else `Read

  let read t bs ~off ~len =
    Eio.Flow.copy_string (Bigstring.substring bs ~off ~len) t.upstream;
    len

  let read_eof t bs ~off ~len =
    let n = read t bs ~off ~len in
    t.client_done <- true;
    (try Eio.Flow.shutdown t.upstream `Send with Eio.Io _ -> ());
    if t.upstream_done then finish t;
    n

  let yield_reader _ _ = ()

  let next_write_operation t =
    let len = String.length t.to_client in
    if len > 0
    then
      `Write
        [ { IOVec.buffer = Bigstring.of_string t.to_client ~off:0 ~len
          ; off = 0
          ; len
          }
        ]
    else if t.upstream_done
    then `Close 0
    else `Yield

  let report_write_result t = function
    | `Ok n ->
      t.to_client <- String.sub t.to_client n (String.length t.to_client - n);
      if t.to_client = "" then Eio.Condition.broadcast t.written
    | `Closed -> finish t

  let yield_writer t k = t.writer <- Some k
  let report_exn t _ = finish t
  let is_closed t = t.client_done && t.upstream_done && t.to_client = ""
  let shutdown = finish
end

let tunnel reqd upstream =
  let t =
    { upstream :> Eio.Flow.two_way_ty Eio.Resource.t
    ; to_client = ""
    ; client_done = false
    ; upstream_done = false
    ; writer = None
    ; written = Eio.Condition.create ()
    ; finished = Eio.Promise.create ()
    }
  in
  Request_descriptor.respond_with_upgrade
    reqd
    Headers.empty
    (Runtime.make (module Tunnel) t);
  let buf = Cstruct.create 4096 in
  let rec pump () =
    match Eio.Flow.single_read t.upstream buf with
    | n ->
      t.to_client <- t.to_client ^ Cstruct.to_string ~len:n buf;
      Tunnel.wake_writer t;
      while t.to_client <> "" && not t.upstream_done do
        Eio.Condition.await_no_mutex t.written
      done;
      if not t.upstream_done then pump ()
    | exception End_of_file ->
      t.upstream_done <- true;
      Tunnel.wake_writer t;
      if t.client_done then Tunnel.finish t
  in
  pump ();
  Eio.Promise.await (fst t.finished)

let error_handler (_ : Eio.Net.Sockaddr.stream) =
  Ache_examples.Server.error_handler

let request_handler ~sw net (_ : Eio.Net.Sockaddr.stream) reqd =
  let respond status =
    Request_descriptor.respond_with_string reqd (Response.create status) ""
  in
  match Request_descriptor.request reqd with
  | { Request.meth = `CONNECT; target; _ } ->
    let i = String.rindex target ':' in
    let host = String.sub target 0 i
    and service = String.sub target (i + 1) (String.length target - i - 1) in
    Eio.Fiber.fork ~sw (fun () ->
      match Eio.Net.with_tcp_connect net ~host ~service (tunnel reqd) with
      | () -> ()
      | exception (Eio.Io _ as exn) ->
        Eio.traceln "CONNECT %s: %a" target Eio.Exn.pp exn;
        if Option.is_none (Request_descriptor.response reqd)
        then respond `Bad_gateway)
  | _ ->
    let headers = Headers.of_list [ "connection", "close" ] in
    Request_descriptor.respond_with_string
      reqd
      (Response.create ~headers `Method_not_allowed)
      ""

let main port =
  Eio_main.run (fun env ->
    let net = Eio.Stdenv.net env in
    Eio.Switch.run (fun sw ->
      let socket =
        Eio.Net.listen
          ~reuse_addr:true
          ~backlog:5
          ~sw
          net
          (`Tcp (Eio.Net.Ipaddr.V4.loopback, port))
      in
      Printf.printf "Listening on port %i as a CONNECT proxy. Try:\n\n" port;
      Printf.printf
        "  curl -v -p -x http://localhost:%d http://example.com\n%!"
        port;
      Eio.Net.run_server
        socket
        ~on_error:(fun exn ->
          Eio.traceln "Uncaught exception handling client: %a" Fmt.exn exn)
        (fun client_socket client_addr ->
           Eio.Switch.run (fun sw ->
             Ache_eio.Server.create_connection_handler
               ~request_handler:(request_handler ~sw net)
               ~error_handler
               client_socket
               client_addr))))

let () =
  let port = ref 8080 in
  Arg.parse
    [ "-p", Arg.Set_int port, " Listening port number (8080 by default)" ]
    ignore
    "A CONNECT proxy. Runs forever.";
  main !port
