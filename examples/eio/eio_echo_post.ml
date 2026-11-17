open Ache_eio
open Ache

let error_handler (_ : Eio.Net.Sockaddr.stream) =
  Ache_examples.Server.error_handler

let request_handler ~u (_ : Eio.Net.Sockaddr.stream) reqd =
  match Request_descriptor.request reqd with
  | { Request.meth = `POST; headers; _ } ->
    let response =
      let content_type =
        match Headers.get headers "content-type" with
        | None -> "application/octet-stream"
        | Some x -> x
      in
      Response.create
        ~headers:
          (Headers.of_list
             [ "content-type", content_type
             ; "transfer-encoding", "chunked" (* ; "connection", "close" *)
             ])
        `OK
    in
    let request_body = Request_descriptor.request_body reqd in
    let response_body =
      Request_descriptor.respond_with_streaming reqd response
    in
    let rec on_read buffer ~off ~len =
      Body.Writer.write_bigstring response_body buffer ~off ~len;
      Body.Reader.schedule_read request_body ~on_eof ~on_error ~on_read
    and on_eof () =
      Stdlib.Format.eprintf "EOF@.";
      Body.Writer.close response_body;
      Eio.Promise.resolve_ok u ()
    and on_error () =
      Eio.Promise.resolve_error u (Failure "request body cut short")
    in
    Body.Reader.schedule_read request_body ~on_eof ~on_error ~on_read
  | _ ->
    let headers = Headers.of_list [ "connection", "close" ] in
    Request_descriptor.respond_with_string
      reqd
      (Response.create ~headers `Method_not_allowed)
      ""

let log_connection_error ex =
  Eio.traceln "Uncaught exception handling client: %a" Fmt.exn ex

let main port =
  Eio_main.run (fun env ->
    let listen_address = `Tcp (Eio.Net.Ipaddr.V4.loopback, port) in
    let network = Eio.Stdenv.net env in
    let handler ~u =
      Server.create_connection_handler
        ~request_handler:(request_handler ~u)
        ~error_handler
    in
    Eio.Switch.run (fun sw ->
      let socket =
        Eio.Net.listen
          ~reuse_addr:true
          ~reuse_port:true
          ~backlog:5
          ~sw
          network
          listen_address
      in
      Printf.printf "Listening on port %i and echoing POST requests.\n" port;
      Printf.printf "To send a POST request, try one of the following\n\n";
      Printf.printf
        "  echo \"Testing echo POST\" | dune exec examples/lwt/lwt_post.exe\n";
      Printf.printf
        "  echo \"Testing echo POST\" | curl -XPOST --data @- \
         http://localhost:%d\n\n\
         %!"
        port;
      Eio.Net.run_server
        socket
        ~additional_domains:
          ( Eio.Stdenv.domain_mgr env
          , Stdlib.Domain.recommended_domain_count () - 1 )
        ~on_error:log_connection_error
        (fun client_sock client_addr ->
           let p, u = Eio.Promise.create () in
           handler ~u client_sock client_addr;
           Eio.Promise.await_exn p)))

let () =
  let port = ref 8080 in
  Arg.parse
    [ "-p", Arg.Set_int port, " Listening port number (8080 by default)" ]
    ignore
    "Echoes POST requests. Runs forever.";
  main !port
