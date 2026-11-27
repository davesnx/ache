let () =
  let s = Vs.settings () in
  (* [--timeouts off]: every server timeout [None]. *)
  let off = if s.timeouts then None else Some None in
  let config = Vs_ache.config s in
  let request_handler _ reqd = Vs_ache.Handler.handle reqd
  and error_handler _ = Vs_ache.error_handler in
  match s.domains with
  | None ->
    let handler =
      Ache_eio.Server.create_connection_handler
        ~config
        ?idle_timeout:off
        ?head_timeout:off
        ?close_timeout:off
        ?body_timeout:off
        ?write_timeout:off
        ~request_handler
        ~error_handler
    in
    Vs_eio.run ~port:s.port (fun ~sw:_ flow addr -> handler flow addr)
  | Some domains ->
    Eio_main.run @@ fun env ->
    Ache_eio.Server.run
      ~config
      ?idle_timeout:off
      ?head_timeout:off
      ?close_timeout:off
      ?body_timeout:off
      ?write_timeout:off
      ~domains
      ~backlog:4096
      ~tcp_nodelay:true
      ~on_error:(fun exn -> prerr_endline (Printexc.to_string exn))
      ~net:(Eio.Stdenv.net env)
      ~domain_mgr:(Eio.Stdenv.domain_mgr env)
      ~request_handler
      ~error_handler
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, s.port))
