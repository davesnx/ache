open Ache

module Client = struct
  exception Response_error

  let error_handler error =
    let error =
      match error with
      | `Malformed_response err -> Format.sprintf "Malformed response: %s" err
      | `Invalid_response_body_length _ -> "Invalid body length"
      | `Connection_closed -> "Connection closed before the response"
      | `Exn exn -> Format.sprintf "Exn raised: %s" (Printexc.to_string exn)
    in
    Format.eprintf "Error handling response: %s\n%!" error

  let print ~on_eof response response_body =
    match response with
    | { Response.status = `OK; _ } as response ->
      Format.fprintf Format.std_formatter "%a\n%!" Response.pp_hum response;
      let on_error () =
        prerr_endline "response body cut short";
        exit 1
      in
      let rec on_read bs ~off ~len =
        Ache.Bigstring.substring ~off ~len bs |> print_string;
        Body.Reader.schedule_read response_body ~on_read ~on_eof ~on_error
      in
      Body.Reader.schedule_read response_body ~on_read ~on_eof ~on_error
    | response ->
      Format.fprintf Format.err_formatter "%a\n%!" Response.pp_hum response;
      Stdlib.exit 1
end

module Server = struct
  let echo_post reqd =
    match Request_descriptor.request reqd with
    | { Request.meth = `POST; headers; _ } ->
      let response =
        let content_type =
          match Headers.get headers "content-type" with
          | None -> "application/octet-stream"
          | Some x -> x
        in
        Response.create
          ~headers:
            (Headers.of_list
               [ "content-type", content_type; "connection", "close" ])
          `OK
      in
      let request_body = Request_descriptor.request_body reqd in
      let response_body =
        Request_descriptor.respond_with_streaming reqd response
      in
      let rec on_read buffer ~off ~len =
        Body.Writer.write_bigstring response_body buffer ~off ~len;
        Body.Reader.schedule_read request_body ~on_eof ~on_error ~on_read
      and on_eof () = Body.Writer.close response_body
      and on_error () = prerr_endline "request body cut short" in
      Body.Reader.schedule_read request_body ~on_eof ~on_error ~on_read
    | _ ->
      let headers = Headers.of_list [ "connection", "close" ] in
      Request_descriptor.respond_with_string
        reqd
        (Response.create ~headers `Method_not_allowed)
        ""

  let error_handler ?request:_ error start_response =
    let response_body = start_response Headers.empty in
    (match error with
    | `Exn exn ->
      Body.Writer.write_string response_body (Printexc.to_string exn);
      Body.Writer.write_string response_body "\n"
    | #Status.standard as error ->
      Body.Writer.write_string
        response_body
        (Status.default_reason_phrase error));
    Body.Writer.close response_body
end
