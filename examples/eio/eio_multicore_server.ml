open Ache

let request_handler _client_address reqd =
  let body = "Hello, world!\n" in
  let headers = Headers.of_list [ "content-type", "text/plain" ] in
  Request_descriptor.respond_with_string
    reqd
    (Response.create ~headers `OK)
    body

let error_handler _client_address ?request:_ error start_response =
  let body = start_response Headers.empty in
  (match error with
  | `Exn exn -> Body.Writer.write_string body (Printexc.to_string exn)
  | error ->
    Body.Writer.write_string
      body
      (Status.to_string (Server_connection.error_status error)));
  Body.Writer.close body

let () =
  let port = int_of_string Sys.argv.(1) in
  let domains = int_of_string Sys.argv.(2) in
  Eio_main.run @@ fun env ->
  Printf.printf "Listening on 127.0.0.1:%d with %d domains\n%!" port domains;
  Ache_eio.Server.run
    ~domains
    ~on_error:(Eio.traceln "connection error: %a" Fmt.exn)
    ~net:(Eio.Stdenv.net env)
    ~domain_mgr:(Eio.Stdenv.domain_mgr env)
    ~request_handler
    ~error_handler
    (`Tcp (Eio.Net.Ipaddr.V4.loopback, port))
