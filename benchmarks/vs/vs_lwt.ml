let run ~port handler =
  Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
  (Lwt.async_exception_hook :=
     fun exn -> prerr_endline ("async exception: " ^ Printexc.to_string exn));
  let address = Unix.(ADDR_INET (inet_addr_loopback, port)) in
  Lwt_main.run
    (Lwt.bind
       (Lwt_io.establish_server_with_client_socket
          ~backlog:4096
          ~set_tcp_nodelay:true
          address
          handler)
       (fun _server -> fst (Lwt.wait ())))
