open Lwt.Infix
open Ache
open Ache_lwt

let error_handler _ = assert false

let main port host =
  Lwt_unix.getaddrinfo host (Int.to_string port) [ Unix.(AI_FAMILY PF_INET) ]
  >>= fun addresses ->
  let socket = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Lwt_unix.connect socket (List.hd addresses).Unix.ai_addr >>= fun () ->
  let finished, notify_finished = Lwt.wait () in
  let response_handler =
    Ache_examples.Client.print ~on_eof:(Lwt.wakeup_later notify_finished)
  in
  let request_headers =
    Request.create ~headers:(Headers.of_list [ "host", host ]) `GET "/"
  in
  let config = { Config.default with max_pipelined_requests = 2 } in
  Client.create_connection ~config socket >>= fun connection ->
  let request_body =
    Client.request connection ~response_handler ~error_handler request_headers
  in
  let finished', notify_finished' = Lwt.wait () in
  let response_handler' =
    Ache_examples.Client.print ~on_eof:(Lwt.wakeup_later notify_finished')
  in
  let request_body' =
    Client.request
      connection
      ~response_handler:response_handler'
      ~error_handler
      request_headers
  in
  Body.Writer.close request_body';
  Body.Writer.close request_body;
  Lwt.join [ finished; finished' ] >>= fun () -> Client.shutdown connection

let () =
  let host = ref None in
  let port = ref 80 in
  Arg.parse
    [ "-p", Set_int port, " Port number (80 by default)" ]
    (fun host_argument -> host := Some host_argument)
    "lwt_get.exe [-p N] HOST";
  let host =
    match !host with
    | None -> failwith "No hostname provided"
    | Some host -> host
  in
  Lwt_main.run (main !port host)
