open Ache

let request_handler _ reqd =
  Body.Reader.close (Request_descriptor.request_body reqd);
  Request_descriptor.respond_with_string
    reqd
    (Response.create ~headers:(Headers.of_list [ "content-length", "2" ]) `OK)
    "ok"

let error_handler _ ?request:_ _ start_response =
  Body.Writer.close (start_response Headers.empty)

let () =
  let port = int_of_string Sys.argv.(1) in
  Eio_main.run @@ fun env ->
  match
    Ache_eio.Server.run
      ~domains:2
      ~backlog:256
      ~on_error:(fun exn ->
        Printf.printf "on_error %s\n%!" (Printexc.to_string exn))
      ~net:(Eio.Stdenv.net env)
      ~domain_mgr:(Eio.Stdenv.domain_mgr env)
      ~request_handler
      ~error_handler
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, port))
  with
  | () -> print_endline "returned"
  | exception exn -> Printf.printf "raised %s\n%!" (Printexc.to_string exn)
