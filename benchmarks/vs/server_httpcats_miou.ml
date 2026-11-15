open H1

module Http = struct
  type reqd = Reqd.t
  type reader = Body.Reader.t
  type writer = Body.Writer.t
  type response = Response.t
  type flushed = unit

  let target reqd = (Reqd.request reqd).Request.target
  let request_body = Reqd.request_body
  let schedule_read = Body.Reader.schedule_read

  let response status headers =
    Response.create ~headers:(Headers.of_list headers) (status :> Status.t)

  let respond_with_string = Reqd.respond_with_string
  let respond_with_bigstring = Reqd.respond_with_bigstring

  let respond_with_streaming reqd response =
    Reqd.respond_with_streaming reqd response

  let write_bigstring = Body.Writer.write_bigstring
  let flush = Body.Writer.flush
  let close_writer = Body.Writer.close
  let writer_is_closed = Body.Writer.is_closed
end

module Handler = Vs.Make (Http)

let handler _flow _conn = function
  | `V1 reqd -> Handler.handle reqd
  | `V2 _ -> invalid_arg "h2 needs TLS, which this server does not offer"

let error_handler _version ?request:_ _error respond =
  match respond Httpcats.Server.Headers.empty with
  | `V1 body -> Body.Writer.close body
  | `V2 body -> H2.Body.Writer.close body

let () =
  let s = Vs.settings () in
  Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
  Logs.set_reporter (Logs.format_reporter ());
  let config =
    { Config.read_buffer_size = s.read_buffer_size
    ; request_body_buffer_size = s.request_body_buffer_size
    ; response_buffer_size = s.response_buffer_size
    ; response_body_buffer_size = s.response_body_buffer_size
    }
  in
  Miou_unix.run ~domains:0 @@ fun () ->
  Httpcats.Server.clear
    ~parallel:false
    ~config
    ~backlog:4096
    ~error_handler
    ~handler
    (Httpcats.Server.Bind Unix.(ADDR_INET (inet_addr_loopback, s.port)))
