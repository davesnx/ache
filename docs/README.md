# ache documentation

## Install

ache is not on opam yet. Pin it from GitHub:

```sh
opam pin add ache https://github.com/davesnx/ache.git
opam pin add ache-eio https://github.com/davesnx/ache.git  # or ache-lwt, ache-miou
opam pin add ache-eio-tls https://github.com/davesnx/ache.git  # or ache-lwt-tls, ache-miou-tls, for TLS
```

ache needs OCaml 5.3 or later.

## Quick start with Eio

A server that answers every request with `Hello, world!`. It takes the port
as its first argument. ache adds the `Content-Length` field.

<!-- example: examples/eio/eio_hello_server.ml -->
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
    (Ache_eio.Server.create_connection_handler ~request_handler ~error_handler)
```

A client that sends `GET /` to that server and prints the response. An
HTTP/1.1 request must have a `Host` field; without it the server answers 400.

<!-- example: examples/eio/eio_hello_client.ml -->
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
  let port = int_of_string Sys.argv.(1) in
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let flow =
    Eio.Net.connect
      ~sw
      (Eio.Stdenv.net env)
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, port))
  in
  let client = Ache_eio.Client.create_connection ~sw flow in
  let finished, resolve_finished = Eio.Promise.create () in
  let on_error () =
    Eio.Promise.resolve_error
      resolve_finished
      (Failure "response body cut short")
  in
  let response_handler response body =
    Format.printf "%a@." Response.pp_hum response;
    let rec on_read buffer ~off ~len =
      print_string (Bigstringaf.substring buffer ~off ~len);
      Body.Reader.schedule_read body ~on_read ~on_eof ~on_error
    and on_eof () = Eio.Promise.resolve_ok resolve_finished () in
    Body.Reader.schedule_read body ~on_read ~on_eof ~on_error
  in
  let headers =
    Headers.of_list [ "host", Printf.sprintf "127.0.0.1:%d" port ]
  in
  let request_body =
    Ache_eio.Client.request
      client
      (Request.create ~headers `GET "/")
      ~error_handler
      ~response_handler
  in
  Body.Writer.close request_body;
  Eio.Promise.await_exn finished;
  Eio.Promise.await (Ache_eio.Client.shutdown client)
```

```console
$ dune exec examples/eio/eio_hello_server.exe -- 8080 &
Listening on 127.0.0.1:8080
$ dune exec examples/eio/eio_hello_client.exe -- 8080
((version "HTTP/1.1") (status 200) (reason "OK") (headers (("content-type" "text/plain")
                                                          ("content-length" "14"))))
Hello, world!
```

## Quick start with Lwt

The same server and client with `ache-lwt`. A Lwt server must ignore
`SIGPIPE`, as the server below does, or the process exits when it writes to a
peer that has closed the connection (ache installs no signal handlers;
`Eio_main.run` already ignores `SIGPIPE`).

<!-- example: examples/lwt/lwt_hello_server.ml -->
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

let () =
  Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
  let port = int_of_string Sys.argv.(1) in
  let listen_address = Unix.(ADDR_INET (inet_addr_loopback, port)) in
  Lwt_main.run
    (let open Lwt.Syntax in
     let* _server =
       Lwt_io.establish_server_with_client_socket
         listen_address
         (Ache_lwt.Server.create_connection_handler
            ~request_handler
            ~error_handler)
     in
     Printf.printf "Listening on 127.0.0.1:%d\n%!" port;
     fst (Lwt.wait ()))
```

<!-- example: examples/lwt/lwt_hello_client.ml -->
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
         print_string (Bigstringaf.substring buffer ~off ~len);
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
```

The output is the same as with Eio. `examples/` has more: streaming request
and response bodies, pipelined requests, and a CONNECT proxy.

## Quick start with Miou

The same server and client with `ache-miou`. As with Lwt, the application
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
      print_string (Bigstringaf.substring buffer ~off ~len);
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

The output is the same as with Eio.

## TLS

TLS uses [ocaml-tls](https://github.com/mirleft/ocaml-tls): `ache-eio-tls`
runs `ache-eio` over `tls-eio`, `ache-lwt-tls` runs `ache-lwt` over
`tls-lwt`, and `ache-miou-tls` runs `ache-miou` over the protocol engine of
ocaml-tls (`Tls.Engine`), with the exceptions of `tls-miou-unix`. OpenSSL
(`lwt_ssl`) is not supported.

The examples load a certificate from PEM files. To make a test CA and a
certificate for `localhost` and `127.0.0.1` (the repository has no key
material):

```console
$ dune exec examples/certs/gen_test_certs.exe -- /tmp
```

An Eio server. The handler does the handshake in the connection fiber; when
it fails, it raises and `run_server` gives the exception to `on_error`. The
time limit of the handshake is under "Timeouts" below.

<!-- example: examples/eio/eio_tls_hello_server.ml -->
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
```

An Eio client that trusts the test CA. A client for public servers uses the
system trust anchors, for example `Ca_certs.authenticator ()` from the
`ca-certs` package. `~peer` is required: `` `Host name `` or `` `Ip address ``
is the identity that the certificate must prove. `` `Insecure_no_name_check ``
accepts any certificate from a trusted CA for any name; use it only with an
authenticator that checks the identity itself. When the certificate is
rejected, `create_connection` raises
`` Tls_eio.Tls_failure (`Error (`AuthenticationFailure _)) `` (Lwt: the promise
fails with `Tls_lwt.Tls_failure` and the socket is closed).

<!-- example: examples/eio/eio_tls_hello_client.ml -->
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
  let port = int_of_string Sys.argv.(1) in
  let ca = Sys.argv.(2) in
  Mirage_crypto_rng_unix.use_default ();
  Eio_main.run @@ fun env ->
  let authenticator =
    X509_eio.authenticator (`Ca_file Eio.Path.(Eio.Stdenv.fs env / ca))
  in
  let tls =
    match Tls.Config.client ~authenticator () with
    | Ok tls -> tls
    | Error (`Msg message) -> failwith message
  in
  Eio.Switch.run @@ fun sw ->
  let socket =
    Eio.Net.connect
      ~sw
      (Eio.Stdenv.net env)
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, port))
  in
  let host = Domain_name.(host_exn (of_string_exn "localhost")) in
  let client =
    Ache_eio_tls.Client.create_connection ~sw ~tls ~peer:(`Host host) socket
  in
  let finished, resolve_finished = Eio.Promise.create () in
  let on_error () =
    Eio.Promise.resolve_error
      resolve_finished
      (Failure "response body cut short")
  in
  let response_handler response body =
    Format.printf "%a@." Response.pp_hum response;
    let rec on_read buffer ~off ~len =
      print_string (Bigstringaf.substring buffer ~off ~len);
      Body.Reader.schedule_read body ~on_read ~on_eof ~on_error
    and on_eof () = Eio.Promise.resolve_ok resolve_finished () in
    Body.Reader.schedule_read body ~on_read ~on_eof ~on_error
  in
  let headers =
    Headers.of_list [ "host", Printf.sprintf "localhost:%d" port ]
  in
  let request_body =
    Ache_eio_tls.Client.request
      client
      (Request.create ~headers `GET "/")
      ~error_handler
      ~response_handler
  in
  Body.Writer.close request_body;
  Eio.Promise.await_exn finished;
  Eio.Promise.await (Ache_eio_tls.Client.shutdown client)
```

```console
$ dune exec examples/eio/eio_tls_hello_server.exe -- 8443 /tmp/server.pem /tmp/server.key &
Listening on https://127.0.0.1:8443
$ dune exec examples/eio/eio_tls_hello_client.exe -- 8443 /tmp/ca.pem
((version "HTTP/1.1") (status 200) (reason "OK") (headers (("content-type" "text/plain")
                                                          ("content-length" "12"))))
Hello, TLS!
$ curl --cacert /tmp/ca.pem https://127.0.0.1:8443/
Hello, TLS!
```

The same with Lwt. `create_connection_handler_with_fd` does the handshake and
closes the socket when it fails.

<!-- example: examples/lwt/lwt_tls_hello_server.ml -->
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

let () =
  Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
  let port = int_of_string Sys.argv.(1) in
  let cert = Sys.argv.(2)
  and key = Sys.argv.(3) in
  let listen_address = Unix.(ADDR_INET (inet_addr_loopback, port)) in
  Lwt_main.run
    (let open Lwt.Syntax in
     let* certificate = X509_lwt.private_of_pems ~cert ~priv_key:key in
     let tls =
       match Tls.Config.server ~certificates:(`Single certificate) () with
       | Ok tls -> tls
       | Error (`Msg message) -> failwith message
     in
     let* _server =
       Lwt_io.establish_server_with_client_socket
         listen_address
         (Ache_lwt_tls.Server.create_connection_handler_with_fd
            ~tls
            ~request_handler
            ~error_handler)
     in
     Printf.printf "Listening on https://127.0.0.1:%d\n%!" port;
     fst (Lwt.wait ()))
```

<!-- example: examples/lwt/lwt_tls_hello_client.ml -->
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
  let port = int_of_string Sys.argv.(1) in
  let ca = Sys.argv.(2) in
  Lwt_main.run
    (let open Lwt.Syntax in
     let* authenticator = X509_lwt.authenticator (`Ca_file ca) in
     let tls =
       match Tls.Config.client ~authenticator () with
       | Ok tls -> tls
       | Error (`Msg message) -> failwith message
     in
     let socket = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
     let* () =
       Lwt_unix.connect socket Unix.(ADDR_INET (inet_addr_loopback, port))
     in
     let host = Domain_name.(host_exn (of_string_exn "localhost")) in
     let* client =
       Ache_lwt_tls.Client.create_connection_with_fd
         ~tls
         ~peer:(`Host host)
         socket
     in
     let finished, notify_finished = Lwt.wait () in
     let response_handler response body =
       Format.printf "%a@." Response.pp_hum response;
       let rec on_read buffer ~off ~len =
         print_string (Bigstringaf.substring buffer ~off ~len);
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
       Headers.of_list [ "host", Printf.sprintf "localhost:%d" port ]
     in
     let request_body =
       Ache_lwt_tls.Client.request
         client
         (Request.create ~headers `GET "/")
         ~error_handler
         ~response_handler
     in
     Body.Writer.close request_body;
     let* () = finished in
     Ache_lwt_tls.Client.shutdown client)
```

The same with Miou. As with `ache-miou`, the caller owns the socket and
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
```

A response without `Content-Length` or `Transfer-Encoding: chunked` ends when
the connection closes. Over TLS, only a `close_notify` alert proves that the
peer closed the connection; without it, an attacker can cut the response.
The clients of `ache-eio-tls`, `ache-lwt-tls` and `ache-miou-tls` report
such a response as cut: the body gets `on_error`, and the error handler gets
`` `Exn Ache_eio_tls.Truncated ``, `` `Exn Ache_lwt_tls.Truncated `` or
`` `Exn Ache_miou_tls.Truncated ``. In
every other case the end of TCP without `close_notify` counts as the end of
the input, as without TLS: a complete response with `Content-Length` or
chunked framing is not an error, and a request that got no byte of its
response gets `` `Connection_closed ``, so that the application can retry
it. `Ache_eio` also accepts a `Tls_eio` flow directly,
but `tls-eio` 2.1.5 does not tell `close_notify` from the end of TCP, so a
client on such a flow reports a cut response as complete. A request body
always has an explicit length, so a server reports a cut body without this
check.

Timeouts: when `?idle_timeout` or `?head_timeout` is set (with `?clock` for
Eio), the TLS handshake must be complete within the smaller of them. Without
either, the handshake has no time limit. A client that sends plain HTTP to a
TLS port gets no answer, because ocaml-tls waits for the rest of what it
reads as a TLS record, so without a timeout the connection stays open until
the client closes it.

## Differences from httpun

ache forks httpun at commit `37fdcd8` (httpun 0.2.0 and 10 commits).
[CHANGES.md](../CHANGES.md) has the full list, with the httpun and httpaf issues
that each change fixes.

### Packages and internals

- `httpun-types` is part of `ache`. `httpun-lwt` and `httpun-lwt-unix` are
  one package, `ache-lwt`. The async and mirage runtimes are removed.
- Message heads, chunk-size lines and trailer sections are parsed by a
  hand-written parser in place of Angstrom. Bodies are framed by a small state machine.
- [Faraday](https://github.com/inhabitedtype/faraday) 0.8.2 is vendored in
  `vendor/faraday` and compiled inside `ache`. A program can also link the
  opam `faraday` package.
- There is no gluten. `ache-eio`, `ache-lwt` and `ache-miou` have their own
  I/O loops.
  `Ache.Runtime.S` is the interface that a runtime drives, and
  `Ache.Runtime.make` packs a protocol so that an HTTP upgrade can hand the
  connection to it.

### Requests the server rejects

The server calls the error handler, sends the error response with
`connection: close`, and closes the connection. Requests that came before it
on the same connection get their responses first.

| Request | Response |
|---|---|
| A bare LF or a bare CR as a line end, obs-fold, a field name that is not a token, a CTL (for example NUL) in a field value | 400 |
| A request line without a version | 400 |
| An HTTP/1.1 request without `Host`, two `Host` fields, a `Host` value that is not `uri-host [":" port]` | 400 |
| A target form that does not fit the method: `*` with a method other than OPTIONS; without CONNECT, a target that is neither `/...` nor `scheme:...` (an authority such as `example.com:80` reads as a URI with scheme `example.com` and is accepted; `127.0.0.1:80` or `[::1]:80` is not) | 400 |
| A CONNECT target that is not `host:port` with a port of 0 to 65535 | 400 |
| A `Content-Length` that is not one repeated `1*DIGIT` value, such as `+3` or `3, 4` | 400 |
| Both `Content-Length` and `Transfer-Encoding`; `Transfer-Encoding` in an HTTP/1.0 request; `chunked` that is not the final coding or is there twice | 400 |
| A transfer coding other than `chunked`, as in `gzip, chunked` | 501 |
| A method longer than `Config.max_header_size` | 501 |
| A target that makes the request line longer than `max_header_size` | 414 |
| A head longer than `max_header_size` (default 16 KiB) | 431 |
| A head that is not complete when `?head_timeout` expires (off by default) | 408 |
| A major version other than 1 | 505 |

ache accepts HTTP/1.x with x > 1 as HTTP/1.1, one empty line before the
request line, chunk extensions (ignored) and trailer fields (discarded).
httpun rejected chunk extensions and trailers.

### Responses

- ache adds `Content-Length` to a string or bigstring response, and
  `Transfer-Encoding: chunked` to a streaming response on an HTTP/1.1
  persistent connection. Otherwise a streaming body ends when the connection
  closes.
- A response to `HEAD`, and a 1xx, 204 or 304 response, has no body.
- Each final response after which the connection closes has
  `connection: close`. A response to an HTTP/1.0 request on a connection that
  stays open has `connection: keep-alive`.
- The `respond_with_*` functions raise `Invalid_argument`, before they write
  anything, for: a 1xx status; both `Content-Length` and `Transfer-Encoding`;
  either field in a 1xx, a 204 or a 2xx response to `CONNECT`;
  `Transfer-Encoding` in a string or bigstring response with a body; a
  `Content-Length` that differs from the string or bigstring; a field name
  that is not a token, or a CR, LF or NUL in a field value or reason phrase.
  The body writer of a streaming response raises `Invalid_argument` for a
  write past its `Content-Length`.
- `Reqd.respond_with_informational` sends `100 Continue`, `103 Early Hints`
  or another interim response before the final response.
- Only a 2xx response to `CONNECT` starts a tunnel. Another response to
  `CONNECT` lets the connection serve the next request.
- `Status` has the codes of the IANA registry that httpun did not have (102,
  103, 207, 208, 226, 308, 421, 422, 423, 424, 425, 451, 506, 507, 508).
  `` `Payload_too_large `` is now `` `Content_too_large ``, and 422 is
  `` `Unprocessable_content ``.

### Bodies

- `Body.Reader.schedule_read` takes a required `~on_error`. A body cut by the
  end of the input, by a framing error or by a closed connection calls
  `on_error`, not `on_eof`; exactly one of the two ends a body. httpun called
  `on_eof`, as for a complete body.
- `on_read` gets all the bytes that are available in one call, so a body of
  many small chunks is delivered in linear time. ache does not reuse the
  bytes it gives to `on_read`.
- When the response is complete, the server reads and drops the rest of a
  request body that the application did not read.

### Client

- The new error `` `Connection_closed `` goes to each request that got no
  byte of its response before the connection stopped reading. httpun called
  no handler for these requests, or gave them
  `` `Malformed_response "unexpected eof" ``.
- A request is pipelined only behind requests with an idempotent method. It
  waits behind other requests, and behind `CONNECT` and `Upgrade` requests,
  until their responses are complete.
- Interim 1xx responses other than 101 are dropped. After a 101, or a 2xx
  response to `CONNECT`, the client stops reading HTTP and `upgrade` gives
  the bytes after the head to the new protocol.
- An obs-fold in a response field value becomes one space. Empty lines
  before a response are ignored.
- `Client_connection.request` raises `Invalid_argument` for a method or field
  name that is not a token, a CR, LF or NUL in a field value, and a CR, LF,
  NUL or space in the target. Later requests are still sent.

## Limits

- HTTP/1.0 and HTTP/1.1 only. There is no HTTP/2 or HTTP/3.
- TLS is ocaml-tls only (see [TLS](#tls)): there is no OpenSSL runtime.
- Trailer fields are read and discarded. The API does not give them to the
  application.
- Server timeouts are off by default. `?idle_timeout` closes a connection
  that waits for a request. `?head_timeout` answers 408 to a request head
  that is not complete in time. A request body or a response in progress is
  never timed out.
- Once its write side has closed, the server reads and dispatches no new
  request; the request in progress still gets its body.
- The API is not stable before 1.0.

## Runtime notes

- **Eio.** `Ache_eio.Server.create_connection_handler ~request_handler
  ~error_handler` can be given to `Eio.Net.run_server`. The server and the
  client accept any `_ Eio.Flow.two_way`; `ache-eio-tls` gives TLS. Timeouts
  need `?clock`. With a clock, when the server stops reading (after a
  response that closes the connection) it reads and drops input until the
  peer closes, for at most `idle_timeout` (2 s without
  one), so that the peer can read the response before the connection is
  reset. Without a clock it closes at once.
- **Lwt.** The application must ignore `SIGPIPE`. `Ache_lwt.Server` and
  `Ache_lwt.Client` use `Lwt_unix.file_descr`, and `ache-lwt-tls` a TLS
  socket. For another socket type, implement `Ache_lwt.IO` and apply
  `Ache_lwt.Make`: `read`
  fails with `End_of_file` at the end of the input or when the peer is gone,
  and `writev` returns `` `Closed `` when the peer is gone. The `Ache_lwt.IO`
  documentation has the full contract. The Lwt server does not wait for the
  peer to close after the last response.
- **Miou.** The application must ignore `SIGPIPE`. `Ache_miou.Server` and
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

## Testing

`dune test` runs these suites (counts from `dune test --force` on
2026-11-15):

| Suite | Tests |
|---|---|
| `lib_test`: core unit tests | 280 |
| `rfc_test`: RFC 9110 | 159 |
| `rfc_test`: RFC 9112 | 178 |
| `eio_test`: Eio on loopback sockets | 33 |
| `eio_tls_test`: `ache-eio-tls` on loopback sockets | 34 |
| `lwt_test`: Lwt on loopback sockets | 25 |
| `lwt_tls_test`: `ache-lwt-tls` on loopback sockets | 24 |
| `miou_test`: Miou on loopback sockets | 28 |
| `miou_tls_test`: `ache-miou-tls` on loopback sockets | 33 |

The RFC suites start from each sentence of RFC 9110 and RFC 9112 §2–§11 that
has a requirement keyword (MUST, SHOULD, MAY and others), plus definitions
that a parser must follow. `rfc_test/COVERAGE.md` and
`rfc_test/COVERAGE_9112.md` give each requirement its tests, or the reason it
is out of scope:

| RFC | Requirements | Tested | Out of scope |
|---|---|---|---|
| 9110 | 445 | 93 | 352 |
| 9112 | 173 | 115 | 58 |

No test is marked as a known failure (`rfc_test/KNOWN_FAILURES*.md`). The
README check makes sure that the code in this file is the same as the
compiled examples.

`conformance/` runs the [h1spec](https://github.com/uNetworking/h1spec)
requests against Eio, Lwt and Miou echo servers. On 2026-11-16, h1spec at
`f0a5650` (under Node 22 with `conformance/deno_shim.mjs`) passed 33 of 33
checks on all three servers.

## License

BSD-3-Clause, with the copyright lines of httpaf and httpun. See
[LICENSE](../LICENSE). `vendor/faraday` is an unmodified copy of Faraday 0.8.2
under its own BSD-3-Clause license in `vendor/faraday/LICENSE`.
