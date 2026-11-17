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

let get_ok = function
  | Ok value -> value
  | Error (`Msg message) -> failwith message

let read_file path = In_channel.with_open_bin path In_channel.input_all

let rec clean_up orphans =
  match Miou.care orphans with
  | None | Some None -> ()
  | Some (Some connection) ->
    (match Miou.await connection with
    | Ok () -> ()
    | Error exn -> prerr_endline ("connection error: " ^ Printexc.to_string exn));
    clean_up orphans

let () =
  Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
  let port = int_of_string Sys.argv.(1) in
  let cert = Sys.argv.(2)
  and key = Sys.argv.(3) in
  Mirage_crypto_rng_unix.use_default ();
  let certificates =
    get_ok (X509.Certificate.decode_pem_multiple (read_file cert))
  and private_key = get_ok (X509.Private_key.decode_pem (read_file key)) in
  let tls =
    get_ok
      (Tls.Config.server ~certificates:(`Single (certificates, private_key)) ())
  in
  let handler =
    Ache_miou_tls.Server.create_connection_handler_with_fd
      ~tls
      ~request_handler
      ~error_handler
  in
  Miou_unix.run @@ fun () ->
  let socket = Miou_unix.tcpv4 () in
  Miou_unix.bind_and_listen
    ~backlog:128
    socket
    Unix.(ADDR_INET (inet_addr_loopback, port));
  Printf.printf "Listening on https://127.0.0.1:%d\n%!" port;
  let orphans = Miou.orphans () in
  let rec accept () =
    clean_up orphans;
    let client, client_address = Miou_unix.accept socket in
    ignore
      (Miou.async ~orphans (fun () ->
         Fun.protect
           ~finally:(fun () -> Miou_unix.close client)
           (fun () -> handler client client_address))
       : unit Miou.t);
    accept ()
  in
  accept ()
