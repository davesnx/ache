let () =
  let s = Vs.settings () in
  Vs_lwt.run
    ~port:s.port
    (Ache_lwt.Server.create_connection_handler
       ~config:(Vs_ache.config s)
       ~request_handler:(fun _ reqd -> Vs_ache.Handler.handle reqd)
       ~error_handler:(fun _ -> Vs_ache.error_handler))
