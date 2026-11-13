let () =
  let port = Echo.port () in
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let stop = Eio.Condition.create () in
  let on_signal _ = Eio.Condition.broadcast stop in
  Sys.set_signal Sys.sigterm (Signal_handle on_signal);
  Sys.set_signal Sys.sigint (Signal_handle on_signal);
  let socket =
    Eio.Net.listen
      ~reuse_addr:true
      ~backlog:128
      ~sw
      (Eio.Stdenv.net env)
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, port))
  in
  (match Eio.Net.listening_addr socket with
  | `Tcp (_, port) -> Echo.print_listening "ache-eio" port
  | `Unix _ -> assert false);
  Eio.Fiber.first
    (fun () -> Eio.Condition.await_no_mutex stop)
    (fun () ->
       Eio.Net.run_server
         ~on_error:(fun exn ->
           Eio.traceln "connection error: %s" (Printexc.to_string exn))
         socket
         (fun flow addr ->
            Eio.Switch.run @@ fun sw ->
            Ache_eio.Server.create_connection_handler
              ~request_handler:(fun _ -> Echo.request_handler)
              ~error_handler:(fun _ -> Echo.error_handler)
              ~sw
              addr
              flow))
