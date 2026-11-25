let () =
  let port = Ws_echo.port () in
  Sys.set_signal Sys.sigpipe Signal_ignore;
  let stop, wake = Lwt.wait () in
  let on_signal _ = if Lwt.is_sleeping stop then Lwt.wakeup wake () in
  ignore (Lwt_unix.on_signal Sys.sigterm on_signal : Lwt_unix.signal_handler_id);
  ignore (Lwt_unix.on_signal Sys.sigint on_signal : Lwt_unix.signal_handler_id);
  let server_fd = Lwt_unix.socket PF_INET SOCK_STREAM 0 in
  Lwt_unix.setsockopt server_fd SO_REUSEADDR true;
  Lwt_main.run
    (let open Lwt.Syntax in
     let* server =
       Lwt_io.establish_server_with_client_socket
         ~server_fd
         Unix.(ADDR_INET (inet_addr_loopback, port))
         (Ache_lwt.Server.create_connection_handler
            ~request_handler:(fun _ -> Ws_echo.request_handler)
            ~error_handler:(fun _ -> Ws_echo.error_handler))
     in
     (match Lwt_unix.getsockname server_fd with
     | ADDR_INET (_, port) -> Ws_echo.print_listening "ache-lwt" port
     | ADDR_UNIX _ -> assert false);
     let* () = stop in
     Lwt_io.shutdown_server server)
