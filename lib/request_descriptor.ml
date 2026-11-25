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

type error =
  [ `Bad_request
  | `Not_implemented
  | `Http_version_not_supported
  | `Uri_too_long
  | `Bad_gateway
  | `Internal_server_error
  | `Request_header_fields_too_large
  | `Request_timeout
  | `Exn of exn
  ]

type error_handler =
  ?request:Request.t -> error -> (Headers.t -> Body.Writer.t) -> unit

module Reader = Parse.Reader
module Writer = Serialize.Writer

(* XXX(seliopou): The current design assumes that a new [Request_descriptor.t] will be
 * allocated for each new request/response on a connection. This is wasteful,
 * as it creates garbage on persistent connections. A better approach would be
 * to allocate a single [Request_descriptor.t] per connection and reuse it across
 * request/responses. This would allow a single [Encoder.t] to be allocated for
 * the body and reused. The [response_state] type could then be inlined into
 * the [Request_descriptor.t] record, with dummy values occuping the fields for [response].
 * Something like this:
 *
 * {[
 *   type 'handle t =
 *     { mutable request        : Request.t
 *     ; mutable request_body   : Response.Body.Reader.t
 *     ; mutable response       : Response.t (* Starts off as a dummy value,
 *                                            * using [(==)] to identify it when
 *                                            * necessary *)
 *     ; mutable response_body  : Response.Body.Writer.t
 *     ; mutable persistent     : bool
 *     ; mutable response_state : [ `Waiting | `Started | `Streaming ]
 *     }
 *  ]}
 *
 *)
type t =
  { request : Request.t
  ; request_body : Body.Reader.t
  ; reader : Reader.request
  ; writer : Writer.t
  ; response_body_buffer : Bigstring.t
  ; error_handler : error_handler
  ; upgrade : Runtime.impl -> unit
  ; mutable persistent : bool
  ; mutable response_state : Response_state.t
  ; mutable error_code : [ `Ok | error ]
  ; proxy : bool
  ; date : (unit -> string) option
  ; keep_alive : string
  }

let create
      ~error_handler
      ~upgrade
      ~reader
      ~writer
      ~response_body_buffer
      ~proxy
      ~date
      ~keep_alive
      request
      request_body
  =
  { request
  ; request_body
  ; reader
  ; writer
  ; response_body_buffer
  ; error_handler
  ; upgrade
  ; persistent = Request.persistent_connection request
  ; response_state = Waiting
  ; error_code = `Ok
  ; proxy
  ; date
  ; keep_alive
  }

let request { request; _ } = request
let request_body { request_body; _ } = request_body

let response { response_state; _ } =
  match response_state with
  | Waiting -> None
  | Streaming (response, _) | Fixed response | Upgrade response -> Some response

let response_exn { response_state; _ } =
  match response_state with
  | Waiting ->
    failwith "ache.Request_descriptor.response_exn: response has not started"
  | Streaming (response, _) | Fixed response | Upgrade response -> response

let frame ~fn ~length t (response : Response.t) =
  if Status.is_informational response.status
  then
    invalid_arg
      (fn
      ^ ": a 1xx response is not final; use respond_with_informational, or \
         respond_with_upgrade for 101");
  Response.frame
    ~fn
    ~proxy:t.proxy
    ~request_method:t.request.meth
    ~request_version:t.request.version
    ~persistent:t.persistent
    ~length
    response

let needs_date (response : Response.t) =
  (not (Status.is_informational response.status))
  && not (Headers.mem response.headers "date")

let persists_after t (response : Response.t) =
  t.persistent
  && Response.persistent_connection response
  && not (t.request.meth = `CONNECT && Status.is_successful response.status)

let needs_keep_alive t (response : Response.t) =
  t.keep_alive <> ""
  && (not (Status.is_informational response.status))
  && persists_after t response
  && not (Headers.mem response.headers "keep-alive")

let write_response t response =
  if Writer.is_closed t.writer
  then Serialize.check_response response
  else
    let date =
      match t.date with
      | Some date when needs_date response -> date ()
      | _ -> ""
    in
    let keep_alive = if needs_keep_alive t response then t.keep_alive else "" in
    if date == "" && keep_alive == ""
    then Writer.write_response t.writer response
    else Writer.write_response_with t.writer response ~date ~keep_alive

let commit_persistence t (response : Response.t) =
  t.persistent <- persists_after t response;
  if not t.persistent then Reader.pause t.reader

let close_after t =
  match t.response_state with
  | Upgrade _ -> ()
  | Waiting | Fixed _ | Streaming _ ->
    t.persistent <- false;
    Reader.pause t.reader

let has_body t encoding =
  encoding <> `No_body && not (Writer.is_closed t.writer)

let respond_with_string t response str =
  if t.error_code <> `Ok
  then
    failwith
      "ache.Request_descriptor.respond_with_string: invalid state, currently \
       handling error";
  match t.response_state with
  | Waiting ->
    let response, encoding =
      frame
        ~fn:"ache.Request_descriptor.respond_with_string"
        ~length:(Some (String.length str))
        t
        response
    in
    write_response t response;
    if has_body t encoding then Writer.write_string t.writer str;
    commit_persistence t response;
    t.response_state <- Fixed response;
    Writer.wakeup t.writer
  | Streaming _ | Upgrade _ ->
    failwith
      "ache.Request_descriptor.respond_with_string: response already started"
  | Fixed _ ->
    failwith
      "ache.Request_descriptor.respond_with_string: response already complete"

let respond_with_bigstring t response (bstr : Bigstring.t) =
  if t.error_code <> `Ok
  then
    failwith
      "ache.Request_descriptor.respond_with_bigstring: invalid state, \
       currently handling error";
  match t.response_state with
  | Waiting ->
    let response, encoding =
      frame
        ~fn:"ache.Request_descriptor.respond_with_bigstring"
        ~length:(Some (Bigstring.length bstr))
        t
        response
    in
    write_response t response;
    if has_body t encoding then Writer.schedule_bigstring t.writer bstr;
    commit_persistence t response;
    t.response_state <- Fixed response;
    Writer.wakeup t.writer
  | Streaming _ | Upgrade _ ->
    failwith
      "ache.Request_descriptor.respond_with_bigstring: response already started"
  | Fixed _ ->
    failwith
      "ache.Request_descriptor.respond_with_bigstring: response already \
       complete"

let respond_with_informational t (response : Response.t) =
  if t.error_code <> `Ok
  then
    failwith
      "ache.Request_descriptor.respond_with_informational: invalid state, \
       currently handling error";
  if
    (not (Status.is_informational response.status))
    || Status.to_code response.status = 101
  then
    invalid_arg
      "ache.Request_descriptor.respond_with_informational: the status must be \
       1xx and not 101";
  if
    Headers.mem response.headers "content-length"
    || Headers.mem response.headers "transfer-encoding"
  then
    invalid_arg
      "ache.Request_descriptor.respond_with_informational: content-length or \
       transfer-encoding in a 1xx";
  match t.response_state with
  | Waiting ->
    if Message.at_least_v1_1 t.request.version
    then (
      write_response t response;
      Writer.wakeup t.writer)
  | Streaming _ | Upgrade _ ->
    failwith
      "ache.Request_descriptor.respond_with_informational: response already \
       started"
  | Fixed _ ->
    failwith
      "ache.Request_descriptor.respond_with_informational: response already \
       complete"

let unsafe_respond_with_streaming ~flush_headers_immediately t response =
  match t.response_state with
  | Waiting ->
    let response, encoding =
      frame
        ~fn:"ache.Request_descriptor.respond_with_streaming"
        ~length:None
        t
        response
    in
    let response_body =
      Body.Writer.create t.response_body_buffer ~encoding ~writer:t.writer
    in
    write_response t response;
    if Writer.is_closed t.writer then Body.Writer.close_and_drain response_body;
    commit_persistence t response;
    t.response_state <- Streaming (response, response_body);
    if flush_headers_immediately then Writer.wakeup t.writer;
    response_body
  | Streaming _ | Upgrade _ ->
    failwith
      "ache.Request_descriptor.respond_with_streaming: response already started"
  | Fixed _ ->
    failwith
      "ache.Request_descriptor.respond_with_streaming: response already \
       complete"

let respond_with_streaming ?(flush_headers_immediately = false) t response =
  if t.error_code <> `Ok
  then
    failwith
      "ache.Request_descriptor.respond_with_streaming: invalid state, \
       currently handling error";
  unsafe_respond_with_streaming ~flush_headers_immediately t response

let unsafe_respond_with_upgrade ?status t headers impl =
  let fn = "ache.Request_descriptor.respond_with_upgrade" in
  match t.response_state with
  | Waiting ->
    let response =
      match t.request.meth, status with
      | `CONNECT, status ->
        let status = Option.value status ~default:`OK in
        fst
          (frame
             ~fn
             ~length:None
             t
             (Response.create ~headers (status :> Status.t)))
      | _, None -> Response.create ~headers `Switching_protocols
      | _, Some _ -> invalid_arg (fn ^ ": ~status is for a CONNECT request")
    in
    let closed = Writer.is_closed t.writer in
    write_response t response;
    commit_persistence t response;
    t.response_state <- Upgrade response;
    Writer.flush t.writer (function
      | `Written when not closed -> t.upgrade impl
      | `Written | `Closed ->
        let (Runtime.Impl ((module Engine), engine, _)) = impl in
        Engine.shutdown engine;
        Reader.force_close t.reader;
        Reader.wakeup t.reader);
    Body.Reader.close t.request_body;
    Writer.wakeup t.writer
  | Streaming _ | Upgrade _ ->
    failwith
      "ache.Request_descriptor.respond_with_upgrade: response already started"
  | Fixed _ ->
    failwith
      "ache.Request_descriptor.respond_with_upgrade: response already complete"

let respond_with_upgrade ?status t headers impl =
  if t.error_code <> `Ok
  then
    failwith
      "ache.Request_descriptor.respond_with_upgrade: invalid state, currently \
       handling error";
  unsafe_respond_with_upgrade ?status t headers impl

let report_error t error =
  t.persistent <- false;
  match t.response_state, t.error_code with
  | Waiting, `Ok ->
    t.error_code <- (error :> [ `Ok | error ]);
    let status =
      match (error :> [ error | Status.standard ]) with
      | `Exn _ -> `Internal_server_error
      | #Status.standard as status -> status
    in
    (match
       t.error_handler ~request:t.request error (fun headers ->
         let response_body =
           unsafe_respond_with_streaming
             t
             ~flush_headers_immediately:true
             (Response.create ~headers status)
         in
         (* NOTE(anmonteiro): When reporting an error that calls the error
            handler, we can only deliver an EOF to the request body once the
            error response has started. Otherwise, the request body `on_eof`
            handler could erroneously send a successful response instead of
            letting us handle the error. *)
         Body.Reader.fail t.request_body;
         response_body)
     with
    | () -> ()
    | exception _ ->
      Body.Reader.fail t.request_body;
      Writer.close_and_drain t.writer;
      Response_state.flush_response_body t.response_state;
      Reader.force_close t.reader;
      Reader.wakeup t.reader)
  | other ->
    Body.Reader.fail t.request_body;
    (match other with
    | Waiting, _ ->
      Writer.close_and_drain t.writer;
      Reader.force_close t.reader;
      Reader.wakeup t.reader
    | Streaming (_response, response_body), `Ok ->
      Body.Writer.force_close response_body;
      Body.Writer.close_and_drain response_body;
      Reader.wakeup t.reader
    | Streaming (_response, response_body), `Exn _ ->
      Writer.close_and_drain t.writer;
      Body.Writer.close_and_drain response_body;
      Reader.wakeup t.reader
    | (Fixed _ | Streaming _ | Upgrade _), _ ->
      (* XXX(seliopou): Once additional logging support is added, log the error
       * in case it is not spurious. *)
      ())

let report_exn t exn = report_error t (`Exn exn)

let try_with t f : (unit, exn) result =
  try
    f ();
    Ok ()
  with
  | exn ->
    report_exn t exn;
    Error exn

(* Private API, not exposed to the user through ache.mli *)

let fail_request_body { request_body; _ } = Body.Reader.fail request_body

let error_code t =
  match t.error_code with #error as error -> Some error | `Ok -> None

let persistent_connection t =
  t.persistent
  && (not (Writer.is_closed t.writer))
  &&
  match t.response_state with
  | Streaming (_, response_body) ->
    not (Body.Writer.is_incomplete response_body)
  | Waiting | Fixed _ | Upgrade _ -> true

let input_state t : Io_state.t =
  match t.response_state, t.request.meth with
  | Upgrade _, _ when Reader.is_closed t.reader -> Complete
  | Upgrade _, _ -> Wait
  | Waiting, _
    when Reader.may_switch_protocols t.request
         && Body.Reader.is_closed t.request_body
         && not (Reader.is_closed t.reader) ->
    Wait
  | _ ->
    if Body.Reader.is_closed t.request_body
    then Complete
    else if Body.Reader.is_read_scheduled t.request_body
    then Ready
    else Wait

let output_state { response_state; writer; _ } =
  Response_state.output_state response_state ~writer

let flush_request_body t =
  if Body.Reader.has_pending_output t.request_body
  then
    try Body.Reader.execute_read t.request_body with exn -> report_exn t exn

let flush_response_body t = Response_state.flush_response_body t.response_state
