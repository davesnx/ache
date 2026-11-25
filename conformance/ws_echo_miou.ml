let rec clean_up orphans =
  match Miou.care orphans with
  | None | Some None -> ()
  | Some (Some prm) ->
    (match Miou.await prm with
    | Ok () -> ()
    | Error exn -> prerr_endline (Printexc.to_string exn));
    clean_up orphans

let () =
  let port = Ws_echo.port () in
  Sys.set_signal Sys.sigpipe Signal_ignore;
  Sys.set_signal Sys.sigterm (Signal_handle (fun _ -> exit 0));
  Sys.set_signal Sys.sigint (Signal_handle (fun _ -> exit 0));
  let handler =
    Ache_miou.Server.create_connection_handler
      ~request_handler:(fun _ -> Ws_echo.request_handler)
      ~error_handler:(fun _ -> Ws_echo.error_handler)
  in
  Miou_unix.run ~domains:0 @@ fun () ->
  let listening = Miou_unix.tcpv4 () in
  Miou_unix.bind_and_listen
    ~backlog:128
    listening
    (Unix.ADDR_INET (Unix.inet_addr_loopback, port));
  (match Unix.getsockname (Miou_unix.to_file_descr listening) with
  | ADDR_INET (_, port) -> Ws_echo.print_listening "ache-miou" port
  | ADDR_UNIX _ -> assert false);
  let rec accept orphans =
    clean_up orphans;
    let socket, addr = Miou_unix.accept listening in
    ignore
      (Miou.async ~orphans (fun () ->
         Fun.protect
           ~finally:(fun () -> Miou_unix.close socket)
           (fun () -> handler socket addr))
       : unit Miou.t);
    accept orphans
  in
  accept (Miou.orphans ())
