let () =
  let s = Vs.settings () in
  let handler =
    Ache_eio.Server.create_connection_handler
      ~config:(Vs_ache.config s)
      ~request_handler:(fun _ reqd -> Vs_ache.Handler.handle reqd)
      ~error_handler:(fun _ -> Vs_ache.error_handler)
  in
  Vs_eio.run ~port:s.port (fun ~sw:_ flow addr -> handler flow addr)
