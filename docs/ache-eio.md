# ache-eio

ache on Eio. The protocol behavior, the general limits and the TLS notes
are in [ache.md](ache.md).

## Quick start

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
      print_string (Ache.Bigstring.substring buffer ~off ~len);
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

## Serving on several cores

`Ache_eio.Server.run` runs one accept loop in each of `~domains` domains
(by default `Domain.recommended_domain_count ()`). Each domain listens on
its own socket with `SO_REUSEPORT`, so the domains share no socket and no
state, and the kernel spreads connections between them. The handlers run
in several domains at the same time. The address must have a fixed port.

<!-- example: examples/eio/eio_multicore_server.ml -->
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
  let domains = int_of_string Sys.argv.(2) in
  Eio_main.run @@ fun env ->
  Printf.printf "Listening on 127.0.0.1:%d with %d domains\n%!" port domains;
  Ache_eio.Server.run
    ~domains
    ~on_error:(Eio.traceln "connection error: %a" Fmt.exn)
    ~net:(Eio.Stdenv.net env)
    ~domain_mgr:(Eio.Stdenv.domain_mgr env)
    ~request_handler
    ~error_handler
    (`Tcp (Eio.Net.Ipaddr.V4.loopback, port))
```

## Runtime notes

`Ache_eio.Server.create_connection_handler ~request_handler
~error_handler` can be given to `Eio.Net.run_server`. The server and the
client accept any `_ Eio.Flow.two_way`; `ache-eio-tls` gives TLS. Timeouts
need `?clock`. With a clock, when the server stops reading (after a
response that closes the connection) it reads and drops input until the
peer closes, for at most `idle_timeout` (2 s without
one), so that the peer can read the response before the connection is
reset. Without a clock it closes at once.

## TLS

To make the test certificates, and for the TLS notes, see
[TLS in ache.md](ache.md#tls).

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
      print_string (Ache.Bigstring.substring buffer ~off ~len);
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
