open Httpaf

module Http = struct
  type reqd = Reqd.t
  type reader = [ `read ] Body.t
  type writer = [ `write ] Body.t
  type response = Response.t
  type flushed = unit

  let target reqd = (Reqd.request reqd).Request.target
  let request_body = Reqd.request_body
  let schedule_read = Body.schedule_read

  let response status headers =
    Response.create ~headers:(Headers.of_list headers) (status :> Status.t)

  let respond_with_string = Reqd.respond_with_string
  let respond_with_bigstring = Reqd.respond_with_bigstring

  let respond_with_streaming reqd response =
    Reqd.respond_with_streaming reqd response

  let write_bigstring = Body.write_bigstring
  let flush = Body.flush
  let close_writer = Body.close_writer
  let writer_is_closed = Body.is_closed
end

module Handler = Vs.Make (Http)

let config (s : Vs.settings) =
  { Config.read_buffer_size = s.read_buffer_size
  ; request_body_buffer_size = s.request_body_buffer_size
  ; response_buffer_size = s.response_buffer_size
  ; response_body_buffer_size = s.response_body_buffer_size
  }

let error_handler ?request:_ _ start_response =
  Body.close_writer (start_response Headers.empty)
