open Ache

let request_handler _client_address reqd =
  let body = "Hello, TLS!\n" in
  let headers = Headers.of_list [ "content-type", "text/plain" ] in
  Request_descriptor.respond_with_string
    reqd
    (Response.create ~headers `OK)
    body

let error_handler _client_address ?request:_ error start_response =
  let body = start_response Headers.empty in
  (match error with
  | `Exn exn -> Body.Writer.write_string body (Printexc.to_string exn)
  | #Status.standard as status ->
    Body.Writer.write_string body (Status.default_reason_phrase status));
  Body.Writer.close body

let () =
  Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
  let port = int_of_string Sys.argv.(1) in
  let cert = Sys.argv.(2)
  and key = Sys.argv.(3) in
  let listen_address = Unix.(ADDR_INET (inet_addr_loopback, port)) in
  Lwt_main.run
    (let open Lwt.Syntax in
     let* certificate = X509_lwt.private_of_pems ~cert ~priv_key:key in
     let tls =
       match Tls.Config.server ~certificates:(`Single certificate) () with
       | Ok tls -> tls
       | Error (`Msg message) -> failwith message
     in
     let* _server =
       Lwt_io.establish_server_with_client_socket
         listen_address
         (Ache_lwt_tls.Server.create_connection_handler_with_fd
            ~tls
            ~request_handler
            ~error_handler)
     in
     Printf.printf "Listening on https://127.0.0.1:%d\n%!" port;
     fst (Lwt.wait ()))
