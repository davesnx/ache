let () =
  let s = Vs.settings () in
  let handler =
    Httpun_eio.Server.create_connection_handler
      ~config:(Vs_httpun.config s)
      ~request_handler:(fun _ { Gluten.reqd; _ } ->
        Vs_httpun.Handler.handle reqd)
      ~error_handler:(fun _ -> Vs_httpun.error_handler)
  in
  Vs_eio.run ~port:s.port (fun ~sw flow addr -> handler ~sw addr flow)
