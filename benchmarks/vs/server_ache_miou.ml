let rec clean_up orphans =
  match Miou.care orphans with
  | None | Some None -> ()
  | Some (Some prm) ->
    (match Miou.await prm with
    | Ok () -> ()
    | Error exn -> prerr_endline (Printexc.to_string exn));
    clean_up orphans

let () =
  let s = Vs.settings () in
  Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
  let handler =
    Ache_miou.Server.create_connection_handler
      ~config:(Vs_ache.config s)
      ~request_handler:(fun _ reqd -> Vs_ache.Handler.handle reqd)
      ~error_handler:(fun _ -> Vs_ache.error_handler)
  in
  Miou_unix.run ~domains:0 @@ fun () ->
  let listening = Miou_unix.tcpv4 () in
  Miou_unix.bind_and_listen
    ~backlog:4096
    listening
    (Unix.ADDR_INET (Unix.inet_addr_loopback, s.port));
  let rec accept orphans =
    clean_up orphans;
    match Miou_unix.accept listening with
    | exception Unix.Unix_error ((EMFILE | ENFILE), _, _) ->
      Miou.yield ();
      accept orphans
    | socket, addr ->
      (try
         Unix.setsockopt (Miou_unix.to_file_descr socket) Unix.TCP_NODELAY true
       with
      | Unix.Unix_error _ -> ());
      ignore
        (Miou.async ~orphans (fun () ->
           Fun.protect
             ~finally:(fun () -> Miou_unix.close socket)
             (fun () -> handler socket addr))
         : unit Miou.t);
      accept orphans
  in
  accept (Miou.orphans ())
