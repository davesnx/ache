open Ache

(* The server streams a gRPC response and sends its status in trailers. *)
let request_handler reqd =
  let body =
    Request_descriptor.respond_with_streaming
      reqd
      (Response.create
         ~headers:(Headers.of_list [ "content-type", "application/grpc" ])
         `OK)
  in
  Body.Writer.write_string body "\000\000\000\000\002hi";
  Body.Writer.close
    ~trailers:(Headers.of_list [ "grpc-status", "0"; "grpc-message", "OK" ])
    body

(* The client reads grpc-status when the response body ends. *)
let response_handler status _response body =
  let rec read () =
    Body.Reader.schedule_read
      body
      ~on_read:(fun _ ~off:_ ~len:_ -> read ())
      ~on_error:ignore
      ~on_eof:(fun () ->
        status := Headers.get (Body.Reader.trailers body) "grpc-status")
  in
  read ()

let () =
  let request =
    Request.create
      ~headers:
        (Headers.of_list
           [ "host", "a"; "te", "trailers"; "content-length", "0" ])
      `POST
      "/helloworld.Greeter/SayHello"
  in
  let client = Client_connection.create () in
  let status = ref None in
  let _ : Body.Writer.t =
    Client_connection.request
      client
      request
      ~error_handler:ignore
      ~response_handler:(response_handler status)
  in
  let output =
    Testing.serve
      request_handler
      "POST /helloworld.Greeter/SayHello HTTP/1.1\r\n\
       Host: a\r\n\
       TE: trailers\r\n\
       Content-Length: 0\r\n\
       \r\n"
  in
  let len = String.length output in
  let read =
    Client_connection.read
      client
      (Bigstring.of_string output ~off:0 ~len)
      ~off:0
      ~len
  in
  assert (read = len);
  assert (!status = Some "0")
