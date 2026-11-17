open Ache
open Rfc

let served_then_closed ?(version = "HTTP/1.1") connection () =
  let s =
    Server.run
      (req
         ~version
         ~headers:[ "Host", "a"; "Connection", connection ]
         "GET"
         "/")
  in
  check_status (Printf.sprintf "200 for Connection: %s" connection) [ 200 ] s;
  check_closed (Printf.sprintf "closed after Connection: %s" connection) s

let served_and_kept_open ?(version = "HTTP/1.1") connection () =
  let s =
    Server.run
      (req ~version ~headers:[ "Host", "a"; "Connection", connection ] "GET" "/"
      ^ get "/second")
  in
  check_status
    (Printf.sprintf "both requests served with Connection: %s" connection)
    [ 200; 200 ]
    s;
  check_open "connection still open" s

let client_get ?(headers = [ "Host", "a" ]) target =
  let c = Client.create () in
  let ex =
    Client.request
      c
      (Request.create ~headers:(Headers.of_list headers) `GET target)
  in
  c, ex

let client_closes_after connection () =
  let c, ex = client_get "/" in
  Client.feed
    c
    ("HTTP/1.1 200 OK\r\nContent-Length: 2\r\nConnection: "
    ^ connection
    ^ "\r\n\r\nok");
  Alcotest.(check (list int)) "response seen" [ 200 ] (Client.statuses ex);
  Alcotest.(check string) "body" "ok" ex.body;
  Alcotest.(check bool)
    (Printf.sprintf "client connection closed after Connection: %s" connection)
    true
    (Client.is_closed c)

module Tunnel = struct
  type t =
    { mutable received : string
    ; mutable closed : bool
    }

  let next_read_operation t = if t.closed then `Close else `Read

  let read t bs ~off ~len =
    t.received <- t.received ^ Ache.Bigstring.substring bs ~off ~len;
    len

  let read_eof t bs ~off ~len =
    t.closed <- true;
    read t bs ~off ~len

  let yield_reader _ _ = ()
  let next_write_operation t = if t.closed then `Close 0 else `Yield
  let report_write_result _ _ = ()
  let yield_writer _ _ = ()
  let report_exn t _ = t.closed <- true
  let is_closed t = t.closed
  let shutdown t = t.closed <- true
end

let upgrade_headers = [ "Upgrade", "echo"; "Connection", "Upgrade" ]

let upgrade ?(before = ignore) tunnel (ex : Server.exchange) =
  before ex;
  Request_descriptor.respond_with_upgrade
    ex.reqd
    (Headers.of_list upgrade_headers)
    (Runtime.make (module Tunnel) tunnel)

let upgrade_request ?(headers = []) () =
  req
    ~headers:
      ([ "Host", "a"; "Connection", "upgrade"; "Upgrade", "echo" ] @ headers)
    "GET"
    "/chat"

let switching_head =
  "HTTP/1.1 101 Switching Protocols\r\n\
   Upgrade: echo\r\n\
   Connection: Upgrade\r\n\
   \r\n"

let long_target = "/" ^ String.make 7999 'a'

let tests =
  [ test "§7.6.1 server DEF: Connection: close ends the connection" (fun () ->
      served_then_closed "close" ())
  ; test
      "§7.6.1 server DEF: connection option close matched case-insensitively"
      (fun () ->
         served_then_closed "Close" ();
         served_then_closed "CLOSE" ())
  ; test
      "§7.6.1 server DEF: close found in a list of connection options"
      (fun () ->
         served_then_closed "keep-alive, close" ();
         served_then_closed "close, upgrade" ();
         served_then_closed "foo ,close" ())
  ; test "§7.6.1 server DEF: close found among empty list elements" (fun () ->
      served_then_closed ", close" ();
      served_then_closed "close ," ())
  ; test
      "§7.6.1 server DEF: close in the first of two Connection field lines"
      (fun () ->
         let s =
           Server.run
             (req
                ~headers:
                  [ "Host", "a"
                  ; "Connection", "close"
                  ; "Connection", "upgrade"
                  ]
                "GET"
                "/")
         in
         check_status "200" [ 200 ] s;
         check_closed "closed" s)
  ; test
      "§7.6.1 server DEF: options other than close keep the connection open"
      (fun () ->
         served_and_kept_open "upgrade" ();
         served_and_kept_open "closed" ();
         served_and_kept_open "x-close" ())
  ; test
      "§7.6.1 server DEF: HTTP/1.0 Keep-Alive option matched case-insensitively"
      (fun () -> served_and_kept_open ~version:"HTTP/1.0" "Keep-Alive" ())
  ; test
      "§7.6.1 client DEF: response Connection: close closes the client"
      (fun () -> client_closes_after "close" ())
  ; test
      "§7.6.1 client DEF: response connection option close matched \
       case-insensitively and in a list"
      (fun () ->
         client_closes_after "Close" ();
         client_closes_after "keep-alive, close" ())
  ; test
      "§7.8 server MAY: ignore Upgrade and keep serving HTTP/1.1 on the \
       connection"
      (fun () ->
         let s =
           Server.run
             (req
                ~headers:
                  [ "Host", "a"
                  ; "Upgrade", "websocket"
                  ; "Connection", "upgrade"
                  ]
                "GET"
                "/chat"
             ^ get "/next")
         in
         check_status "both requests answered over HTTP/1.1" [ 200; 200 ] s;
         Alcotest.(check (list string))
           "both requests reached the handler"
           [ "/chat"; "/next" ]
           (List.map (fun (r : Request.t) -> r.target) (Server.requests s));
         check_open "connection still open" s)
  ; test
      "§7.8 server MUST: Upgrade in an HTTP/1.0 request is ignored"
      (fun () ->
         let s =
           Server.run
             (req
                ~version:"HTTP/1.0"
                ~headers:[ "Upgrade", "foo/2"; "Connection", "upgrade" ]
                "GET"
                "/")
         in
         let r = Server.response s in
         Alcotest.(check int) "normal final response" 200 (Wire.status r);
         Alcotest.(check bool)
           "no Upgrade field in the response"
           false
           (Wire.has_header r "upgrade");
         check_closed "HTTP/1.0 connection closed after the response" s)
  ; test
      "§7.8 client MAY: Upgrade request header section written as given"
      (fun () ->
         let c, _ =
           client_get
             ~headers:
               [ "Host", "a"; "Upgrade", "websocket"; "Connection", "Upgrade" ]
             "/chat"
         in
         Alcotest.(check string)
           "request bytes"
           "GET /chat HTTP/1.1\r\n\
            Host: a\r\n\
            Upgrade: websocket\r\n\
            Connection: Upgrade\r\n\
            \r\n"
           (Client.written c))
  ; test
      "§7.2 client SHOULD: Host stays the first field when given first"
      (fun () ->
         let c, _ =
           client_get
             ~headers:[ "Host", "www.example.org"; "Accept", "*/*" ]
             "/pub/WWW/"
         in
         Alcotest.(check string)
           "request bytes"
           "GET /pub/WWW/ HTTP/1.1\r\n\
            Host: www.example.org\r\n\
            Accept: */*\r\n\
            \r\n"
           (Client.written c))
  ; test
      "§7.5 client SHOULD: keep sending the request body after an early \
       response"
      (fun () ->
         let c = Client.create () in
         let ex =
           Client.request
             ~body:"abc"
             ~close_body:false
             c
             (Request.create
                ~headers:
                  (Headers.of_list [ "Host", "a"; "Content-Length", "6" ])
                `POST
                "/upload")
         in
         Client.feed c "HTTP/1.1 200 OK\r\nContent-Length: 0\r\n\r\n";
         Alcotest.(check (list int))
           "early response seen"
           [ 200 ]
           (Client.statuses ex);
         Body.Writer.write_string ex.writer "def";
         Body.Writer.close ex.writer;
         Client.pump c;
         Alcotest.(check string)
           "whole request written"
           "POST /upload HTTP/1.1\r\nHost: a\r\nContent-Length: 6\r\n\r\nabcdef"
           (Client.written c))
  ; test
      "§4.1 server RECOMMENDED: an 8000-octet request target reaches the \
       handler"
      (fun () ->
         let s = Server.run (get long_target) in
         check_status "200" [ 200 ] s;
         Alcotest.(check (list string))
           "target unchanged"
           [ long_target ]
           (List.map (fun (r : Request.t) -> r.target) (Server.requests s)))
  ; test
      "§4.1 client RECOMMENDED: an 8000-octet request target is written \
       unchanged"
      (fun () ->
         let c, _ = client_get long_target in
         Alcotest.(check string)
           "request bytes"
           ("GET " ^ long_target ^ " HTTP/1.1\r\nHost: a\r\n\r\n")
           (Client.written c))
  ; test
      "§7.8 server MUST: the 101 carries the Upgrade field and later bytes go \
       to the new protocol"
      (fun () ->
         let tunnel = { Tunnel.received = ""; closed = false } in
         let s =
           Server.run ~handler:(upgrade tunnel) (upgrade_request () ^ "ping")
         in
         Alcotest.(check string)
           "response bytes"
           switching_head
           (Server.output s);
         Server.feed s "pong";
         Alcotest.(check string)
           "bytes after the request reach the new protocol"
           "pingpong"
           tunnel.received;
         Alcotest.(check int) "one request handled" 1 (Server.handler_calls s))
  ; test
      "§7.8 server MUST: 100 Continue is sent before the 101 when the request \
       has Expect: 100-continue"
      (fun () ->
         let tunnel = { Tunnel.received = ""; closed = false } in
         let before (ex : Server.exchange) =
           Request_descriptor.respond_with_informational
             ex.reqd
             (Response.create `Continue)
         in
         let s =
           Server.run
             ~handler:(upgrade ~before tunnel)
             (upgrade_request ~headers:[ "Expect", "100-continue" ] ())
         in
         Alcotest.(check string)
           "response bytes"
           ("HTTP/1.1 100 Continue\r\n\r\n" ^ switching_head)
           (Server.output s))
  ]
