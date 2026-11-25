type t =
  { read_buffer_size : int
  ; request_body_buffer_size : int
  ; response_buffer_size : int
  ; response_body_buffer_size : int
  ; max_header_size : int
  ; proxy : bool
  ; max_pending_output : int
  ; max_discard_body : int
  ; max_pipelined_requests : int
  }

let default =
  { read_buffer_size = 0x1000
  ; request_body_buffer_size = 0x1000
  ; response_buffer_size = 0x400
  ; response_body_buffer_size = 0x1000
  ; max_header_size = 0x4000
  ; proxy = false
  ; max_pending_output = 0x10000
  ; max_discard_body = 0x40000
  ; max_pipelined_requests = 1
  }
