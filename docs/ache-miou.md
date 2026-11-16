# ache-miou

ache on Miou. The protocol behavior, the general limits and the TLS notes
are in [ache.md](ache.md).

## Quick start

The same server and client as in [ache-eio](ache-eio.md#quick-start), with
`ache-miou`. As with Lwt, the application
must ignore `SIGPIPE`. The server runs each connection in its own task, made
with `Miou.async ~orphans`, closes the socket when the handler returns, and
collects ended tasks with `Miou.care`. The client calls `Client.shutdown`
from the task that called `Client.create_connection`, because Miou requires
a task to await the tasks it created.

<!-- example: examples/miou/miou_hello_server.ml -->
```ocaml
open Ache

let request_handler _client_address reqd =
  let body = "Hello, world!\n" in
  let headers = Headers.of_list [ "content-type", "text/plain" ] in
  Reqd.respond_with_string reqd (Response.create ~headers `OK) body

let error_handler _client_address ?request:_ error start_response =
  let body = start_response Headers.empty in
  (match error with
  | `Exn exn -> Body.Writer.write_string body (Printexc.to_string exn)
  | #Status.standard as status ->
    Body.Writer.write_string body (Status.default_reason_phrase status));
  Body.Writer.close body

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
  let handler =
    Ache_miou.Server.create_connection_handler ~request_handler ~error_handler
  in
  Miou_unix.run @@ fun () ->
  let socket = Miou_unix.tcpv4 () in
  Miou_unix.bind_and_listen
    ~backlog:128
    socket
    Unix.(ADDR_INET (inet_addr_loopback, port));
  Printf.printf "Listening on 127.0.0.1:%d\n%!" port;
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
```

<!-- example: examples/miou/miou_hello_client.ml -->
```ocaml
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
  Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
  let port = int_of_string Sys.argv.(1) in
  Miou_unix.run @@ fun () ->
  let socket = Miou_unix.tcpv4 () in
  Miou_unix.connect socket Unix.(ADDR_INET (inet_addr_loopback, port));
  let client = Ache_miou.Client.create_connection socket in
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
      print_string (Ache.Bigstring.substring buffer ~off ~len);
      Body.Reader.schedule_read body ~on_read ~on_eof ~on_error
    and on_eof () = ignore (Miou.Computation.try_return finished () : bool) in
    Body.Reader.schedule_read body ~on_read ~on_eof ~on_error
  in
  let headers =
    Headers.of_list [ "host", Printf.sprintf "127.0.0.1:%d" port ]
  in
  let request_body =
    Ache_miou.Client.request
      client
      (Request.create ~headers `GET "/")
      ~error_handler
      ~response_handler
  in
  Body.Writer.close request_body;
  Miou.Computation.await_exn finished;
  Ache_miou.Client.shutdown client;
  Miou_unix.close socket
```

The output is the same as with [Eio](ache-eio.md#quick-start).

## Runtime notes

The application must ignore `SIGPIPE`. `Ache_miou.Server` and
`Ache_miou.Client` use `Miou_unix.file_descr`, and `ache-miou-tls` a TLS
socket. For another socket type, implement `Ache_miou.IO` and apply
`Ache_miou.Make`. Run the server handler in
its own task for each accepted socket, and close the socket when it
returns. Timeouts use `Miou_unix.sleep` and need no clock. With
`?idle_timeout` or `?head_timeout`, when the server stops reading it reads
and drops input until the peer closes, for at most `idle_timeout` (2 s
without one). Without a timeout it closes at once. Handlers and body
callbacks run in the connection tasks and must not leave a child task that
they do not await. `Client.shutdown` must be called by the task that
created the connection.

## TLS

To make the test certificates, and for the TLS notes, see
[TLS in ache.md](ache.md#tls).

The same as the [Eio TLS example](ache-eio.md#tls), with Miou. As with
`ache-miou`, the caller owns the socket and
closes it after the server handler returns, or after `Client.shutdown`; this
is also true when the handshake fails. `create_connection_handler_with_fd`
returns when the handshake fails, and `create_connection_with_fd` raises
`` Tls_miou_unix.Tls_failure (`Error (`AuthenticationFailure _)) `` when the
certificate is rejected.

<!-- example: examples/miou/miou_tls_hello_server.ml -->
```ocaml
open Ache

let request_handler _client_address reqd =
  let body = "Hello, TLS!\n" in
  let headers = Headers.of_list [ "content-type", "text/plain" ] in
  Reqd.respond_with_string reqd (Response.create ~headers `OK) body

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
```

<!-- example: examples/miou/miou_tls_hello_client.ml -->
```ocaml
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
      print_string (Ache.Bigstring.substring buffer ~off ~len);
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
```
