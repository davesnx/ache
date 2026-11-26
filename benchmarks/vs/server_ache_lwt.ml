let () =
  let s = Vs.settings () in
  (* [--timeouts off]: every server timeout [None]. *)
  let off = if s.timeouts then None else Some None in
  Vs_lwt.run
    ~port:s.port
    (Ache_lwt.Server.create_connection_handler
       ~config:(Vs_ache.config s)
       ?idle_timeout:off
       ?head_timeout:off
       ?close_timeout:off
       ?body_timeout:off
       ?write_timeout:off
       ~request_handler:(fun _ reqd -> Vs_ache.Handler.handle reqd)
       ~error_handler:(fun _ -> Vs_ache.error_handler))
