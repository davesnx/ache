(*----------------------------------------------------------------------------
  Copyright (c) 2017-2019 Inhabited Type LLC. Copyright (c) 2019 Antonio Nuno
  Monteiro.

  All rights reserved.

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions are met:

  1. Redistributions of source code must retain the above copyright notice, this
  list of conditions and the following disclaimer.

  2. Redistributions in binary form must reproduce the above copyright notice,
  this list of conditions and the following disclaimer in the documentation
  and/or other materials provided with the distribution.

  3. Neither the name of the author nor the names of his contributors may be
  used to endorse or promote products derived from this software without
  specific prior written permission.

  THIS SOFTWARE IS PROVIDED BY THE CONTRIBUTORS ``AS IS'' AND ANY EXPRESS OR
  IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
  MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO
  EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
  INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
  LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
  PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
  LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
  NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,
  EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
  ----------------------------------------------------------------------------*)

module Reader = Parse.Reader
module Writer = Serialize.Writer

type error =
  [ `Malformed_response of string
  | `Invalid_response_body_length of Response.t
  | `Connection_closed
  | `Exn of exn
  ]

type response_handler = Response.t -> Body.Reader.t -> unit
type error_handler = error -> unit

type t =
  { config : Config.t
  ; reader : Reader.response
  ; writer : Writer.t
  ; request_queue : Response_descriptor.t Queue.t
    (* invariant: If [request_queue] is not empty, then the head of the queue
       has already written the request headers to the wire. *)
  }

let is_closed t = Reader.is_closed t.reader && Writer.is_closed t.writer
let is_waiting t = (not (is_closed t)) && Queue.is_empty t.request_queue
let is_active t = not (Queue.is_empty t.request_queue)
let current_respd_exn t = Queue.peek t.request_queue

let yield_reader t k =
  if Reader.is_closed t.reader then k () else Reader.on_wakeup t.reader k

let wakeup_reader t = Reader.wakeup t.reader

let yield_writer t k =
  if Writer.is_closed t.writer then k () else Writer.on_wakeup t.writer k

let wakeup_writer t = Writer.wakeup t.writer

let create ?(config = Config.default) () =
  let request_queue = Queue.create () in
  { config
  ; reader =
      Reader.response ~max_header_size:config.max_header_size request_queue
  ; writer = Writer.create ()
  ; request_queue
  }

let create_request_body ~request t =
  match Request.body_length request with
  | `Fixed 0L -> Body.Writer.create_empty ~writer:t.writer
  | (`Fixed _ | `Chunked) as encoding ->
    Body.Writer.create
      ~send_trailers:true
      (Bigstring.create t.config.request_body_buffer_size)
      ~encoding
      ~writer:t.writer
  | `Error `Bad_request ->
    failwith "ache.Client_connection.request: invalid body length"

let request
      t
      ?(flush_headers_immediately = false)
      request
      ~error_handler
      ~response_handler
  =
  Serialize.check_request request;
  let request_body = create_request_body ~request t in
  let respd =
    Response_descriptor.create
      ~error_handler
      ~writer:t.writer
      ~response_handler
      request
      request_body
  in
  let handle_now = Queue.is_empty t.request_queue in
  Queue.push respd t.request_queue;
  if handle_now then Response_descriptor.write_request respd;

  if not flush_headers_immediately then Writer.yield t.writer;

  (* Not handling the request now means it may be pipelined. *
     `advance_request_queue_if_necessary` will take care of it, but we still *
     wanna wake up the writer so that the function gets called. *)
  wakeup_writer t;
  request_body

let fail_unanswered t =
  Queue.iter
    (fun respd ->
       match respd.Response_descriptor.state with
       | Uninitialized | Awaiting_response ->
         Response_descriptor.report_error respd `Connection_closed
       | Received_response _ | Upgraded _ | Closed -> ())
    t.request_queue

let shutdown_reader t =
  if is_active t
  then Response_descriptor.fail_response_body (current_respd_exn t);
  Reader.force_close t.reader;
  fail_unanswered t;
  wakeup_reader t

let shutdown_writer t =
  if is_active t
  then Response_descriptor.close_request_body (current_respd_exn t);
  Writer.close t.writer;
  Queue.iter Response_descriptor.flush_request_body t.request_queue;
  wakeup_writer t

let shutdown t =
  shutdown_reader t;
  shutdown_writer t

let set_error_and_handle t error =
  let reading respd =
    match
      respd.Response_descriptor.state, Response_descriptor.input_state respd
    with
    | Awaiting_response, _ -> true
    | Received_response _, (Wait | Ready) -> true
    | Received_response _, Complete ->
      (match Reader.next t.reader with `Error _ | `Read -> true | _ -> false)
    | (Uninitialized | Upgraded _ | Closed), _ -> false
  in
  (match List.find_opt reading (List.of_seq (Queue.to_seq t.request_queue)) with
  | Some respd -> Response_descriptor.report_error respd error
  | None -> ());
  (* From RFC7230§6.5: * A client sending a message body SHOULD monitor the
     network connection * for an error response while it is transmitting the
     request. If the * client sees a response that indicates the server does not
     wish to * receive the message body and is closing the connection, the
     client * SHOULD immediately cease transmitting the body and close its side
     of the * connection. *)
  shutdown t

let report_exn t exn = set_error_and_handle t (`Exn exn)

exception Local

let pipelinable respd =
  let request = Response_descriptor.request respd in
  (not (Reader.may_switch_protocols request))
  &&
  match request.meth with
  | #Method.standard as meth -> Method.is_idempotent meth
  | `Other _ -> false

let maybe_pipeline_queued_requests t =
  (* Don't bother trying to pipeline if there aren't multiple requests in the *
     queue. *)
  if Queue.length t.request_queue > 1
  then
    try
      let _ =
        Queue.fold
          (fun prev respd ->
             (match prev with
             | None -> ()
             | Some prev ->
               (match
                  ( respd.Response_descriptor.state
                  , Response_descriptor.output_state prev )
                with
               | Uninitialized, Complete when pipelinable prev ->
                 Response_descriptor.write_request respd;
                 Response_descriptor.flush_request_body respd
               | _ ->
                 (* bail early. If we can't pipeline this request, we can't
                    write * next ones either. *)
                 raise Local));
             Some respd)
          None
          t.request_queue
      in
      ()
    with
    | _ -> ()

let advance_request_queue t =
  ignore (Queue.take t.request_queue);
  if not (Queue.is_empty t.request_queue)
  then
    (* write request to the wire *)
    let respd = current_respd_exn t in
    match respd.state with
    | Uninitialized ->
      (* Only write request if it hasn't been written to the wire yet (e.g. via
       * pipelining). *)
      Response_descriptor.write_request respd;
      wakeup_writer t
    | _ -> ()

let rec _next_read_operation t =
  if not (is_active t)
  then (
    if Reader.is_closed t.reader then shutdown t;
    Reader.next t.reader)
  else
    let respd = current_respd_exn t in
    match Response_descriptor.input_state respd with
    | Wait -> `Yield
    | Ready -> Reader.next t.reader
    | Complete -> _final_read_operation_for t respd

and _final_read_operation_for t respd =
  let next =
    if not (Response_descriptor.persistent_connection respd)
    then (
      shutdown t;
      Reader.next t.reader)
    else
      match Response_descriptor.output_state respd with
      | Wait | Ready -> `Yield
      | Complete ->
        (match Reader.next t.reader with
        | (`Error _ | `Read) as operation ->
          (* Keep reading when in a "partial" state (`Read). * Don't advance the
             request queue if in an error state. *)
          operation
        | _ ->
          advance_request_queue t;
          _next_read_operation t)
  in
  wakeup_writer t;
  next

let report_reader_error t = function
  | `Parse message -> set_error_and_handle t (`Malformed_response message)
  | `Header_too_large ->
    set_error_and_handle t (`Malformed_response "header fields too large")
  | `Invalid_response_body_length _ as error -> set_error_and_handle t error
  | `Unsolicited_response -> shutdown t

let next_read_operation t =
  match _next_read_operation t with
  | `Error error ->
    report_reader_error t error;
    `Close
  | `Start -> `Read
  | (`Read | `Yield | `Close) as operation -> operation

let read_with_more t bs ~off ~len more =
  let consumed = Reader.read_with_more t.reader bs ~off ~len more in
  if is_active t
  then Response_descriptor.flush_response_body (current_respd_exn t);
  consumed

let read t bs ~off ~len = read_with_more t bs ~off ~len Incomplete

let read_eof t bs ~off ~len =
  let bytes_read = read_with_more t bs ~off ~len Complete in
  (match Reader.next t.reader with
  | `Error error -> report_reader_error t error
  | _ -> if is_active t then shutdown t);
  bytes_read

let rec _next_write_operation t =
  if not (is_active t)
  then (
    if Reader.is_closed t.reader then shutdown t;
    Writer.next t.writer)
  else
    let respd = current_respd_exn t in
    match Response_descriptor.output_state respd with
    | Wait -> `Yield
    | Ready ->
      Response_descriptor.flush_request_body respd;
      Writer.next t.writer
    | Complete -> _final_write_operation_for t respd

and _final_write_operation_for t respd =
  if not (Response_descriptor.persistent_connection respd)
  then (
    shutdown_writer t;
    Writer.next t.writer)
  else (
    (* From RFC7230§6.3.2: * A client that supports persistent connections MAY
       "pipeline" its * requests (i.e., send multiple requests without waiting
       for each * response). *)
    maybe_pipeline_queued_requests t;
    match Response_descriptor.input_state respd with
    | Wait | Ready ->
      wakeup_reader t;
      Writer.next t.writer
    | Complete ->
      (match Reader.next t.reader with
      | `Error _ -> Writer.next t.writer
      | _ ->
        advance_request_queue t;
        wakeup_reader t;
        _next_write_operation t))

let next_write_operation t = _next_write_operation t

let report_write_result t result =
  Writer.report_result t.writer result;
  match result with
  | `Ok _ -> ()
  | `Closed -> Queue.iter Response_descriptor.flush_request_body t.request_queue
