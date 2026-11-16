(*----------------------------------------------------------------------------
  Copyright (c) 2017 Inhabited Type LLC.

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

type request_handler = Reqd.t -> unit

type error =
  [ `Bad_gateway
  | `Bad_request
  | `Not_implemented
  | `Http_version_not_supported
  | `Uri_too_long
  | `Internal_server_error
  | `Request_header_fields_too_large
  | `Request_timeout
  | `Exn of exn
  ]

type error_handler =
  ?request:Request.t -> error -> (Headers.t -> Body.Writer.t) -> unit

type error_code =
  | No_error
  | Error of
      { request : Request.t option
      ; mutable response_state : Response_state.t
      }

type t =
  { config : Config.t
  ; reader : Reader.request
  ; writer : Writer.t
  ; response_body_buffer : Bigstring.t
  ; request_handler : request_handler
  ; error_handler : error_handler
  ; request_queue : Reqd.t Queue.t
    (* invariant: If [request_queue] is not empty, then the head of the queue
       has already had [request_handler] called on it. *)
  ; mutable error_code : error_code
    (* Represents an unrecoverable error that will cause the connection to *
       shutdown. Holds on to the response body created by the error handler *
       that might be streaming to the client. *)
  ; mutable upgraded : Runtime.impl option
  }

let is_closed t = Reader.is_closed t.reader && Writer.is_closed t.writer
let is_active t = not (Queue.is_empty t.request_queue)
let current_reqd_exn t = Queue.peek t.request_queue

let yield_reader t k =
  if Reader.is_closed t.reader then k () else Reader.on_wakeup t.reader k

let wakeup_reader t =
  (if is_active t
   then
     let reqd = current_reqd_exn t in
     (* Before going through another read loop, give the body a chance to flush
        its buffered bytes to the application. This fixes a pathological case
        where the body could buffer too much without a chance of executing
        scheduled reads. *)
     Reqd.flush_request_body reqd);
  Reader.wakeup t.reader

let yield_writer t k =
  if Writer.is_closed t.writer then k () else Writer.on_wakeup t.writer k

let wakeup_writer t = Writer.wakeup t.writer

let default_error_handler ?request:_ error handle =
  let message =
    match error with
    | `Exn exn -> Printexc.to_string exn
    | (#Status.client_error | #Status.server_error) as error ->
      Status.to_string error
  in
  let body = handle Headers.empty in
  Body.Writer.write_string body message;
  Body.Writer.close body

let shutdown_reader t =
  if is_active t then Reqd.fail_request_body (current_reqd_exn t);
  Reader.force_close t.reader;
  wakeup_reader t

let shutdown_writer t =
  if is_active t
  then (
    let reqd = current_reqd_exn t in
    (* XXX(dpatti): I'm not sure I understand why we close the *request* body
       here. Maybe we can write a test such that removing this line causes it to
       fail? *)
    Reqd.fail_request_body reqd;
    Reqd.flush_response_body reqd);
  (match t.error_code with
  | Error { response_state; _ } ->
    Response_state.flush_response_body response_state
  | No_error -> ());
  Writer.close t.writer;
  Reader.pause t.reader;
  wakeup_writer t

let error_code t =
  if is_active t then Reqd.error_code (current_reqd_exn t) else None

let shutdown t =
  shutdown_reader t;
  shutdown_writer t

let upgrade t (Runtime.Impl ((module Engine), engine) as impl) =
  let eof = Reader.is_closed t.reader in
  t.upgraded <- Some impl;
  shutdown t;
  if eof
  then ignore (Engine.read_eof engine Bigstring.empty ~off:0 ~len:0 : int)

let create
      ?(config = Config.default)
      ?(error_handler = default_error_handler)
      request_handler
  =
  let { Config.response_buffer_size; response_body_buffer_size; _ } = config in
  let writer = Writer.create ~buffer_size:response_buffer_size () in
  let request_queue = Queue.create () in
  let response_body_buffer = Bigstring.create response_body_buffer_size in
  let self = ref None in
  let handler request request_body =
    match !self with
    | None -> assert false
    | Some (t, upgrade) ->
      let reqd =
        Reqd.create
          ~error_handler
          ~upgrade
          ~reader:t.reader
          ~writer
          ~response_body_buffer
          ~proxy:config.proxy
          request
          request_body
      in
      let call_handler = Queue.is_empty request_queue in
      Queue.push reqd request_queue;
      if call_handler then request_handler reqd
  in
  let t =
    { reader = Reader.request ~max_header_size:config.max_header_size handler
    ; writer
    ; response_body_buffer
    ; request_handler
    ; error_handler
    ; request_queue
    ; error_code = No_error
    ; config
    ; upgraded = None
    }
  in
  self := Some (t, upgrade t);
  t

let set_error_and_handle ?request t error =
  if is_active t
  then (
    assert (request = None);
    let reqd = current_reqd_exn t in
    Reqd.report_error reqd error)
  else
    let status =
      match (error :> [ error | Status.standard ]) with
      | `Exn _ -> `Internal_server_error
      | #Status.standard as status -> status
    in
    shutdown_reader t;
    let writer = t.writer in
    match t.error_code with
    | No_error ->
      t.error_code <- Error { request; response_state = Waiting };
      (match
         t.error_handler ?request error (fun headers ->
           (match t.error_code with
           | Error { response_state = Waiting; _ } -> ()
           | Error _ | No_error ->
             failwith
               "ache.Server_connection.error_handler: response already started");
           let response, encoding =
             (* If we haven't parsed the request method, just use GET as a
                standard placeholder. The method is only used for edge cases,
                like HEAD or CONNECT. *)
             let request_method, request_version =
               match request with
               | None -> `GET, Version.v1_1
               | Some (request : Request.t) -> request.meth, request.version
             in
             Response.frame
               ~fn:"ache.Server_connection.error_handler"
               ~proxy:t.config.proxy
               ~request_method
               ~request_version
               ~persistent:false
               ~length:None
               (Response.create ~headers status)
           in
           let response_body =
             (* The (shared) response body buffer can be used in this case *
                because in this conditional branch we're not sending a response
                * (is_active t == false), and are therefore not making use of
                that * buffer. *)
             Body.Writer.create
               t.response_body_buffer
               ~encoding
               ~writer:t.writer
           in
           Writer.write_response writer response;
           t.error_code <-
             Error
               { request; response_state = Streaming (response, response_body) };
           wakeup_writer t;
           response_body)
       with
      | () -> ()
      | exception _ ->
        Writer.close_and_drain t.writer;
        shutdown t)
    | Error _ ->
      (* When reading, this should be impossible: even if we try to read more, *
         the parser does not ingest it, and even if someone attempts to feed *
         more bytes to the parser when we already told them to [`Close], that's
         * really their own fault. * * We do, however, need to handle this case
         if any other exception is * reported (we're already handling an error
         and e.g. the writing channel * is closed). Just shut down the
         connection in that case. *)
      Writer.close_and_drain t.writer;
      shutdown t

let report_exn t exn = set_error_and_handle t (`Exn exn)

let advance_request_queue t =
  ignore (Queue.take t.request_queue);
  if Queue.is_empty t.request_queue && not (Writer.is_closed t.writer)
  then Reader.resume t.reader;
  if not (Queue.is_empty t.request_queue)
  then t.request_handler (Queue.peek t.request_queue)

let rec _next_read_operation t =
  if not (is_active t)
  then (
    match t.error_code with
    | Error _ -> `Close
    | No_error when Writer.is_closed t.writer ->
      shutdown t;
      `Close
    | No_error ->
      let next = Reader.next t.reader in
      (match next with
      | `Error _ -> shutdown_reader t
      | `Close -> shutdown t
      | _ -> ());
      next)
  else
    let reqd = current_reqd_exn t in
    match Reqd.input_state reqd with
    | Wait ->
      (match Reqd.output_state reqd with
      | Complete ->
        if Option.is_some (Reqd.response reqd)
        then Body.Reader.discard (Reqd.request_body reqd);
        Reader.next t.reader
      | Wait | Ready ->
        (* `Wait` signals that we should add backpressure to the read channel, *
           meaning the reader should tell the runtime to yield. * * The
           exception here is if there has been an error in the parser; in * that
           case, we need to return that exception and signal the runtime to *
           close. *)
        (match Reader.next t.reader with
        | `Error _ as operation
          when not (Body.Reader.is_closed (Reqd.request_body reqd)) ->
          operation
        | _ -> `Yield))
    | Ready -> Reader.next t.reader
    | Complete -> _final_read_operation_for t reqd

and _final_read_operation_for t reqd =
  if not (Reqd.persistent_connection reqd)
  then (
    shutdown_reader t;
    match Reader.next t.reader with
    | `Error _ -> `Close
    | operation -> operation)
  else
    match Reader.next t.reader, Reqd.output_state reqd with
    | `Close, _ ->
      shutdown_reader t;
      `Close
    | _, (Wait | Ready) -> `Yield
    | `Read, Complete ->
      advance_request_queue t;
      `Read
    | (`Start | `Error _), Complete ->
      advance_request_queue t;
      _next_read_operation t

let next_read_operation t =
  match _next_read_operation t with
  | `Error (`Parse _) ->
    set_error_and_handle t `Bad_request;
    `Close
  | `Error `Header_too_large ->
    set_error_and_handle t `Request_header_fields_too_large;
    `Close
  | `Error `Method_too_long ->
    set_error_and_handle t `Not_implemented;
    `Close
  | `Error `Target_too_long ->
    set_error_and_handle t `Uri_too_long;
    `Close
  | `Error (`Invalid_request (request, error)) ->
    set_error_and_handle ~request t (error :> error);
    `Close
  | `Start | `Read -> `Read
  | (`Yield | `Close) as operation -> operation

let read_with_more t bs ~off ~len more =
  let consumed = Reader.read_with_more t.reader bs ~off ~len more in
  (if is_active t
   then
     let reqd = current_reqd_exn t in
     Reqd.flush_request_body reqd);
  consumed

let read t bs ~off ~len = read_with_more t bs ~off ~len Incomplete
let read_eof t bs ~off ~len = read_with_more t bs ~off ~len Complete

let rec _next_write_operation t =
  if not (is_active t)
  then
    match t.error_code with
    | No_error when Reader.is_closed t.reader ->
      ignore (next_read_operation t : [ `Read | `Yield | `Close ]);
      (match t.error_code with
      | No_error -> Writer.next t.writer
      | Error _ -> _next_write_operation t)
    | No_error -> Writer.next t.writer
    | Error { request; response_state } ->
      (match
         let request_method =
           Option.value
             ~default:`GET
             (Option.map (fun (t : Request.t) -> t.meth) request)
         in
         Response_state.output_state
           response_state
           ~request_method
           ~writer:t.writer
       with
      | Wait -> `Yield
      | Ready ->
        Response_state.flush_response_body response_state;
        Writer.next t.writer
      | Complete ->
        shutdown_writer t;
        Writer.next t.writer)
  else
    let reqd = current_reqd_exn t in
    match Reqd.output_state reqd with
    | Wait -> Writer.next t.writer
    | Ready ->
      Reqd.flush_response_body reqd;
      Writer.next t.writer
    | Complete -> _final_write_operation_for t reqd

and _final_write_operation_for t reqd =
  if not (Reqd.persistent_connection reqd)
  then (
    shutdown_writer t;
    wakeup_reader t;
    Writer.next t.writer)
  else
    match Reqd.input_state reqd with
    | Wait ->
      wakeup_reader t;
      Writer.next t.writer
    | Ready ->
      (* we can't close the request body here, otherwise the reader loop is
         going to think that its "input state" is complete, and remove the
         request descriptor from the request queue, when in fact it needs to
         read the remainder of the request body. It needs to hang around because
         there could be a sudden EOF while discarding the request body, which we
         need to handle. *)
      wakeup_reader t;
      Writer.next t.writer
    | Complete ->
      advance_request_queue t;
      wakeup_reader t;
      _next_write_operation t

let next_write_operation t = _next_write_operation t

let report_write_result t result =
  Writer.report_result t.writer result;
  match result with
  | `Ok _ -> ()
  | `Closed ->
    Reader.pause t.reader;
    if is_active t then Reqd.flush_response_body (current_reqd_exn t);
    (match t.error_code with
    | Error { response_state; _ } ->
      Response_state.flush_response_body response_state
    | No_error -> ())

let next_read_operation t =
  match t.upgraded with
  | None -> next_read_operation t
  | Some (Runtime.Impl ((module Engine), engine)) ->
    Engine.next_read_operation engine

let read t bs ~off ~len =
  match t.upgraded with
  | None -> read t bs ~off ~len
  | Some (Runtime.Impl ((module Engine), engine)) ->
    Engine.read engine bs ~off ~len

let read_eof t bs ~off ~len =
  match t.upgraded with
  | None -> read_eof t bs ~off ~len
  | Some (Runtime.Impl ((module Engine), engine)) ->
    Engine.read_eof engine bs ~off ~len

let yield_reader t k =
  match t.upgraded with
  | None -> yield_reader t k
  | Some (Runtime.Impl ((module Engine), engine)) ->
    Engine.yield_reader engine k

let next_write_operation t =
  match t.upgraded with
  | None -> next_write_operation t
  | Some (Runtime.Impl ((module Engine), engine)) ->
    Engine.next_write_operation engine

let report_write_result t result =
  match t.upgraded with
  | None -> report_write_result t result
  | Some (Runtime.Impl ((module Engine), engine)) ->
    Engine.report_write_result engine result

let yield_writer t k =
  match t.upgraded with
  | None -> yield_writer t k
  | Some (Runtime.Impl ((module Engine), engine)) ->
    Engine.yield_writer engine k

let report_exn t exn =
  match t.upgraded with
  | None -> report_exn t exn
  | Some (Runtime.Impl ((module Engine), engine)) ->
    Engine.report_exn engine exn

let is_closed t =
  match t.upgraded with
  | None -> is_closed t
  | Some (Runtime.Impl ((module Engine), engine)) -> Engine.is_closed engine

let shutdown t =
  match t.upgraded with
  | None -> shutdown t
  | Some (Runtime.Impl ((module Engine), engine)) -> Engine.shutdown engine

let read_timeout t =
  match t.upgraded, t.error_code with
  | None, No_error when not (is_active t) ->
    (match Reader.next t.reader with
    | `Start -> Some (`Idle, fun () -> shutdown t)
    | `Read -> Some (`Head, fun () -> set_error_and_handle t `Request_timeout)
    | `Close | `Error _ -> None)
  | _ -> None
