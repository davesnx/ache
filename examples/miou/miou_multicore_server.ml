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
  | `Bad_request _ ->
    Body.Writer.write_string body (Status.default_reason_phrase `Bad_request)
  | ( #Status.server_error
    | `Uri_too_long | `Request_header_fields_too_large | `Request_timeout ) as
    status ->
    Body.Writer.write_string body (Status.default_reason_phrase status));
  Body.Writer.close body

let () =
  Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
  let port = int_of_string Sys.argv.(1) in
  let domains = int_of_string Sys.argv.(2) in
  Miou_unix.run ~domains:(domains - 1) @@ fun () ->
  Printf.printf "Listening on 127.0.0.1:%d with %d domains\n%!" port domains;
  Ache_miou.Server.run
    ~on_error:(fun exn ->
      prerr_endline ("connection error: " ^ Printexc.to_string exn))
    ~request_handler
    ~error_handler
    Unix.(ADDR_INET (inet_addr_loopback, port))
