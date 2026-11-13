open Ache

let request_handler reqd =
  let { Request.headers; _ } = Reqd.request reqd in
  let request_body = Reqd.request_body reqd in
  let body = Buffer.create 128 in
  let rec on_read buffer ~off ~len =
    Buffer.add_string body (Bigstringaf.substring buffer ~off ~len);
    Body.Reader.schedule_read request_body ~on_eof ~on_read
  and on_eof () =
    let content_type =
      match Headers.get headers "content-type" with
      | None -> []
      | Some value -> [ "content-type", value ]
    in
    let headers =
      Headers.of_list
        (("content-length", Int.to_string (Buffer.length body)) :: content_type)
    in
    Reqd.respond_with_string
      reqd
      (Response.create ~headers `OK)
      (Buffer.contents body)
  in
  Body.Reader.schedule_read request_body ~on_eof ~on_read

let error_handler = Ache_examples.Server.error_handler

let port () =
  let port = ref 8080 in
  Arg.parse
    [ "-p", Arg.Set_int port, " Listening port on 127.0.0.1 (default 8080)" ]
    ignore
    "Echoes the request body for every method. Stops on SIGTERM or SIGINT.";
  !port

let print_listening runtime port =
  Printf.printf "%s listening on 127.0.0.1:%d\n%!" runtime port
