let run ~port handler =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let socket =
    Eio.Net.listen
      ~sw
      ~reuse_addr:true
      ~backlog:4096
      (Eio.Stdenv.net env)
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, port))
  in
  Eio.Net.run_server
    socket
    ~on_error:(fun exn -> prerr_endline (Printexc.to_string exn))
    (fun flow addr ->
       Eio.Net.setsockopt flow Eio.Net.Sockopt.TCP_NODELAY true;
       handler ~sw flow addr)
