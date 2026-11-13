type t =
  | Waiting
  | Fixed of Response.t
  | Streaming of Response.t * Body.Writer.t
  | Upgrade of Response.t

let output_state =
  let response_sent_state request_method (response : Response.t) =
    match request_method with
    | `CONNECT when Status.is_successful response.status -> Io_state.Wait
    | _ -> Complete
  in
  fun t ~request_method ~writer : Io_state.t ->
    match t with
    | Upgrade _ -> Wait
    | Waiting -> if Serialize.Writer.is_closed writer then Complete else Wait
    | Fixed response -> response_sent_state request_method response
    | Streaming (response, response_body) ->
      if Serialize.Writer.is_closed writer
      then response_sent_state request_method response
      else if Body.Writer.requires_output response_body
      then Ready
      else response_sent_state request_method response

let flush_response_body t =
  match t with
  | Streaming (_, response_body) -> Body.Writer.transfer_to_writer response_body
  | _ -> ()
