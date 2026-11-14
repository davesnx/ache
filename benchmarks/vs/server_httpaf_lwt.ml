let () =
  let s = Vs.settings () in
  Vs_lwt.run
    ~port:s.port
    (Httpaf_lwt_unix.Server.create_connection_handler
       ~config:(Vs_httpaf.config s)
       ~request_handler:(fun _ reqd -> Vs_httpaf.Handler.handle reqd)
       ~error_handler:(fun _ -> Vs_httpaf.error_handler))
