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

let () =
  let port = int_of_string Sys.argv.(1) in
  Lwt_main.run
    (let open Lwt.Syntax in
     let socket = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
     let* () =
       Lwt_unix.connect socket Unix.(ADDR_INET (inet_addr_loopback, port))
     in
     let* client = Ache_lwt.Client.create_connection socket in
     let finished, notify_finished = Lwt.wait () in
     let response_handler response body =
       Format.printf "%a@." Response.pp_hum response;
       let rec on_read buffer ~off ~len =
         print_string (Ache.Bigstring.substring buffer ~off ~len);
         Body.Reader.schedule_read body ~on_read ~on_eof ~on_error
       and on_eof () = Lwt.wakeup_later notify_finished ()
       and on_error () =
         Lwt.wakeup_later_exn
           notify_finished
           (Failure "response body cut short")
       in
       Body.Reader.schedule_read body ~on_read ~on_eof ~on_error
     in
     let headers =
       Headers.of_list [ "host", Printf.sprintf "127.0.0.1:%d" port ]
     in
     let request_body =
       Ache_lwt.Client.request
         client
         (Request.create ~headers `GET "/")
         ~error_handler
         ~response_handler
     in
     Body.Writer.close request_body;
     let* () = finished in
     Ache_lwt.Client.shutdown client)
