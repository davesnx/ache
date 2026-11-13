open Ache

let request_handler _client_address reqd =
  let body = "Hello, world!\n" in
  let headers =
    Headers.of_list
      [ "content-type", "text/plain"
      ; "content-length", Int.to_string (String.length body)
      ]
  in
  Reqd.respond_with_string reqd (Response.create ~headers `OK) body

let error_handler _client_address ?request:_ error start_response =
  let body = start_response Headers.empty in
  (match error with
  | `Exn exn -> Body.Writer.write_string body (Printexc.to_string exn)
  | #Status.standard as status ->
    Body.Writer.write_string body (Status.default_reason_phrase status));
  Body.Writer.close body

let () =
  let port = int_of_string Sys.argv.(1) in
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let socket =
    Eio.Net.listen
      ~sw
      ~backlog:128
      ~reuse_addr:true
      (Eio.Stdenv.net env)
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, port))
  in
  Printf.printf "Listening on 127.0.0.1:%d\n%!" port;
  Eio.Net.run_server
    socket
    ~on_error:(Eio.traceln "connection error: %a" Fmt.exn)
    (fun flow client_address ->
       Eio.Switch.run @@ fun sw ->
       Ache_eio.Server.create_connection_handler
         ~sw
         ~request_handler
         ~error_handler
         client_address
         flow)
