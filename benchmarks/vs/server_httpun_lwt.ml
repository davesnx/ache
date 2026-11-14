let () =
  let s = Vs.settings () in
  Vs_lwt.run
    ~port:s.port
    (Httpun_lwt_unix.Server.create_connection_handler
       ~config:(Vs_httpun.config s)
       ~request_handler:(fun _ { Gluten.reqd; _ } ->
         Vs_httpun.Handler.handle reqd)
       ~error_handler:(fun _ -> Vs_httpun.error_handler))
