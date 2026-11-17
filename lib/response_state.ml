type t =
  | Waiting
  | Fixed of Response.t
  | Streaming of Response.t * Body.Writer.t
  | Upgrade of Response.t

let output_state t ~writer : Io_state.t =
  match t with
  | Upgrade _ -> Wait
  | Waiting -> if Serialize.Writer.is_closed writer then Complete else Wait
  | Fixed _ -> Complete
  | Streaming (_, response_body) ->
    if Serialize.Writer.is_closed writer
    then Complete
    else if Body.Writer.requires_output response_body
    then Ready
    else Complete

let flush_response_body t =
  match t with
  | Streaming (_, response_body) -> Body.Writer.transfer_to_writer response_body
  | _ -> ()
