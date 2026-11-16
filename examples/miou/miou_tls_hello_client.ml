open Ache

let error_handler error =
  let message =
    match error with
    | `Malformed_response message -> message
    | `Invalid_response_body_length _ -> "invalid response body length"
    | `Connection_closed -> "connection closed before the response"
    | `Exn exn -> Printexc.to_string exn
  in
  prerr_endline ("request failed: " ^ message);
  exit 1

let get_ok = function
  | Ok value -> value
  | Error (`Msg message) -> failwith message

let () =
  Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
  let port = int_of_string Sys.argv.(1) in
  let ca = Sys.argv.(2) in
  Mirage_crypto_rng_unix.use_default ();
  let authenticator =
    let cas =
      get_ok
        (X509.Certificate.decode_pem_multiple
           (In_channel.with_open_bin ca In_channel.input_all))
    in
    X509.Authenticator.chain_of_trust
      ~time:(fun () -> Some (Ptime_clock.now ()))
      cas
  in
  let tls = get_ok (Tls.Config.client ~authenticator ()) in
  Miou_unix.run @@ fun () ->
  let socket = Miou_unix.tcpv4 () in
  Miou_unix.connect socket Unix.(ADDR_INET (inet_addr_loopback, port));
  let host = Domain_name.(host_exn (of_string_exn "localhost")) in
  let client =
    Ache_miou_tls.Client.create_connection_with_fd
      ~tls
      ~peer:(`Host host)
      socket
  in
  let finished = Miou.Computation.create () in
  let on_error () =
    ignore
      (Miou.Computation.try_cancel
         finished
         (Failure "response body cut short", Printexc.get_callstack 0)
       : bool)
  in
  let response_handler response body =
    Format.printf "%a@." Response.pp_hum response;
    let rec on_read buffer ~off ~len =
      print_string (Bigstringaf.substring buffer ~off ~len);
      Body.Reader.schedule_read body ~on_read ~on_eof ~on_error
    and on_eof () = ignore (Miou.Computation.try_return finished () : bool) in
    Body.Reader.schedule_read body ~on_read ~on_eof ~on_error
  in
  let headers =
    Headers.of_list [ "host", Printf.sprintf "localhost:%d" port ]
  in
  let request_body =
    Ache_miou_tls.Client.request
      client
      (Request.create ~headers `GET "/")
      ~error_handler
      ~response_handler
  in
  Body.Writer.close request_body;
  Miou.Computation.await_exn finished;
  Ache_miou_tls.Client.shutdown client;
  Miou_unix.close socket
