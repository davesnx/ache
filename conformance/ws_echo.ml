(* The server for autobahn.sh and wpt.sh. The same handler runs on Eio, Lwt and
   Miou: ws_echo_{eio,lwt,miou}.ml only listen and accept.

   Routes: - GET /ws: WebSocket echo, text as text and binary as binary. - GET
   /ws/close: WebSocket that closes with code 4001 on the first message, the
   message as reason. - GET /status: 200 with a JSON body. - /echo: 200 with the
   request body, the request method in x-ache-method. - GET /redirect: 302 to
   /status. - OPTIONS /cors: a CORS preflight, recorded under the query string.
   - PUT /cors: the preflight recorded under the query string, as JSON. Every
   response allows any origin, for the browser tests of wpt.sh. *)

open Ache

let max_message_size = 16 * 1024 * 1024

let cors =
  [ "access-control-allow-origin", "*"
  ; "access-control-expose-headers", "x-ache-method"
  ]

let respond reqd ?(headers = []) status body =
  let content_length =
    match status with
    | `No_content -> []
    | _ -> [ "content-length", Int.to_string (String.length body) ]
  in
  let headers = Headers.of_list (content_length @ headers @ cors) in
  Request_descriptor.respond_with_string
    reqd
    (Response.create ~headers status)
    body

let read_body reqd k =
  let request_body = Request_descriptor.request_body reqd in
  let body = Buffer.create 128 in
  let rec on_read buffer ~off ~len =
    Buffer.add_string body (Bigstring.substring buffer ~off ~len);
    Body.Reader.schedule_read request_body ~on_eof ~on_error ~on_read
  and on_error () = prerr_endline "request body cut short"
  and on_eof () = k (Buffer.contents body) in
  Body.Reader.schedule_read request_body ~on_eof ~on_error ~on_read

let echo ws =
  { Websocket.on_message =
      (function
        | Text message -> Websocket.send_text ws message
        | Binary message -> Websocket.send_binary ws message)
  ; on_pong = ignore
  ; on_close = ignore
  }

let close_on_message ws =
  { Websocket.on_message =
      (function
        | Text reason | Binary reason -> Websocket.close ~reason ws ~code:4001)
  ; on_pong = ignore
  ; on_close = ignore
  }

let upgrade reqd handler =
  match Websocket.upgrade ~max_message_size reqd handler with
  | Ok () -> ()
  | Error `Not_websocket ->
    respond reqd ~headers:[ "upgrade", "websocket" ] `Upgrade_required ""
  | Error `Unsupported_version ->
    respond reqd ~headers:[ "sec-websocket-version", "13" ] `Upgrade_required ""
  | Error (`Not_get | `Invalid_key) -> respond reqd `Bad_request ""

(* Preflight request headers, by the query string of /cors. *)
let preflights = Hashtbl.create 16

let json_string s =
  let b = Buffer.create (String.length s + 2) in
  Buffer.add_char b '"';
  String.iter
    (function
      | '"' -> Buffer.add_string b "\\\""
      | '\\' -> Buffer.add_string b "\\\\"
      | c when Char.code c < 0x20 ->
        Buffer.add_string b (Printf.sprintf "\\u%04x" (Char.code c))
      | c -> Buffer.add_char b c)
    s;
  Buffer.add_char b '"';
  Buffer.contents b

let request_handler reqd =
  let { Request.meth; target; headers; _ } = Request_descriptor.request reqd in
  let path, query =
    match String.index_opt target '?' with
    | None -> target, ""
    | Some i ->
      ( String.sub target 0 i
      , String.sub target (i + 1) (String.length target - i - 1) )
  in
  let header name = Option.value ~default:"" (Headers.get headers name) in
  match meth, path with
  | _, "/ws" -> upgrade reqd echo
  | _, "/ws/close" -> upgrade reqd close_on_message
  | (`GET | `HEAD), "/status" ->
    respond
      reqd
      ~headers:[ "content-type", "application/json" ]
      `OK
      {|{"status":"ok"}|}
  | _, "/echo" ->
    read_body reqd (fun body ->
      let content_type =
        match Headers.get headers "content-type" with
        | None -> []
        | Some value -> [ "content-type", value ]
      in
      respond
        reqd
        ~headers:(("x-ache-method", Method.to_string meth) :: content_type)
        `OK
        body)
  | `GET, "/redirect" ->
    respond reqd ~headers:[ "location", "/status" ] `Found ""
  | `OPTIONS, "/cors" ->
    Hashtbl.replace
      preflights
      query
      ( header "access-control-request-method"
      , header "access-control-request-headers" );
    respond
      reqd
      ~headers:
        [ "access-control-allow-methods", "PUT"
        ; "access-control-allow-headers", "x-ache-test"
        ; "access-control-max-age", "0"
        ]
      `No_content
      ""
  | `PUT, "/cors" ->
    let preflight =
      match Hashtbl.find_opt preflights query with
      | None -> "null"
      | Some (meth, names) ->
        Printf.sprintf
          {|{"method":%s,"headers":%s}|}
          (json_string meth)
          (json_string names)
    in
    respond
      reqd
      ~headers:[ "content-type", "application/json" ]
      `OK
      (Printf.sprintf
         {|{"preflight":%s,"x-ache-test":%s}|}
         preflight
         (json_string (header "x-ache-test")))
  | _, ("/status" | "/redirect" | "/cors") ->
    respond reqd `Method_not_allowed ""
  | _ -> respond reqd `Not_found ""

let error_handler = Ache_examples.Server.error_handler

let port () =
  let port = ref 8080 in
  Arg.parse
    [ "-p", Arg.Set_int port, " Listening port on 127.0.0.1 (default 8080)" ]
    ignore
    "WebSocket echo on /ws and HTTP routes for the browser tests. Stops on \
     SIGTERM or SIGINT.";
  !port

let print_listening = Echo.print_listening
