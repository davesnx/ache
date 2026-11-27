open Ache

module Http = struct
  type reqd = Request_descriptor.t
  type reader = Body.Reader.t
  type writer = Body.Writer.t
  type response = Response.t

  type flushed =
    [ `Written
    | `Closed
    ]

  let target reqd = (Request_descriptor.request reqd).Request.target
  let request_body = Request_descriptor.request_body

  let schedule_read reader ~on_eof ~on_read =
    Body.Reader.schedule_read reader ~on_eof ~on_error:ignore ~on_read

  let response status headers =
    Response.create ~headers:(Headers.of_list headers) (status :> Status.t)

  let respond_with_string = Request_descriptor.respond_with_string
  let respond_with_bigstring = Request_descriptor.respond_with_bigstring

  let respond_with_streaming reqd response =
    Request_descriptor.respond_with_streaming reqd response

  let write_bigstring = Body.Writer.write_bigstring
  let flush = Body.Writer.flush
  let close_writer writer = Body.Writer.close writer
  let writer_is_closed = Body.Writer.is_closed

  let echo ws =
    { Websocket.on_message =
        (fun message ->
          incr (Domain.DLS.get Vs.domain_requests);
          match message with
          | Text s -> Websocket.send_text ws s
          | Binary s -> Websocket.send_binary ws s)
    ; on_pong = ignore
    ; on_close = ignore
    }

  let websocket_echo =
    Some
      (fun reqd ->
        match Websocket.upgrade reqd echo with
        | Ok () -> ()
        | Error _ ->
          Request_descriptor.respond_with_string
            reqd
            (Response.create
               ~headers:(Headers.of_list [ "content-length", "0" ])
               `Bad_request)
            "")
end

module Handler = Vs.Make (Http)

let config (s : Vs.settings) =
  { Config.default with
    read_buffer_size = s.read_buffer_size
  ; request_body_buffer_size = s.request_body_buffer_size
  ; response_buffer_size = s.response_buffer_size
  ; response_body_buffer_size = s.response_body_buffer_size
  }

let error_handler ?request:_ _ start_response =
  Body.Writer.close (start_response Headers.empty)
