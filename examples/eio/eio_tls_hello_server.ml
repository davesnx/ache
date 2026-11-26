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
  | error ->
    Body.Writer.write_string
      body
      (Status.to_string (Server_connection.error_status error)));
  Body.Writer.close body

let () =
  let port = int_of_string Sys.argv.(1) in
  let cert = Sys.argv.(2)
  and key = Sys.argv.(3) in
  Mirage_crypto_rng_unix.use_default ();
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let certificate =
    X509_eio.private_of_pems
      ~cert:Eio.Path.(fs / cert)
      ~priv_key:Eio.Path.(fs / key)
  in
  let tls =
    match Tls.Config.server ~certificates:(`Single certificate) () with
    | Ok tls -> tls
    | Error (`Msg message) -> failwith message
  in
  Eio.Switch.run @@ fun sw ->
  let socket =
    Eio.Net.listen
      ~sw
      ~backlog:128
      ~reuse_addr:true
      (Eio.Stdenv.net env)
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, port))
  in
  Printf.printf "Listening on https://127.0.0.1:%d\n%!" port;
  Eio.Net.run_server
    socket
    ~on_error:(Eio.traceln "connection error: %a" Fmt.exn)
    (Ache_eio_tls.Server.create_connection_handler
       ~tls
       ~request_handler
       ~error_handler)
