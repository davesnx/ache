open Ache
open Rfc

let strings = Alcotest.(list string)
let ok ex = Server.respond `OK ex
let serve ?config input = Server.run ?config ~handler:ok input

let only_request s =
  match Server.requests s with
  | [ r ] -> r
  | rs -> Alcotest.failf "expected one request, got %d" (List.length rs)

let values s name = Headers.get_multi (only_request s).headers name

let meth =
  Alcotest.of_pp (fun ppf m -> Format.pp_print_string ppf (Method.to_string m))

let check_rejected_each inputs =
  List.iter
    (fun input ->
       try check_rejected (serve input) with
       | exn -> Alcotest.failf "for %S: %s" input (Printexc.to_string exn))
    inputs

let check_accepted ~target input =
  let s = serve input in
  check_status (Printf.sprintf "200 for %S" input) [ 200 ] s;
  Alcotest.check strings "target" [ target ] (targets s);
  Alcotest.check meth "method" `GET (only_request s).meth

let check_never_seen ~name ~value s =
  List.iter
    (fun (r : Request.t) ->
       if List.mem value (Headers.get_multi r.headers name)
       then Alcotest.failf "handler saw %s: %s" name value)
    (Server.requests s)

let has_bare_cr s =
  let n = String.length s in
  let rec go i =
    i < n && ((s.[i] = '\r' && (i + 1 = n || s.[i + 1] <> '\n')) || go (i + 1))
  in
  go 0

let has_fold s = contains s "\r\n " || contains s "\r\n\t"

let head_of s =
  match Wire.index_from s 0 "\r\n\r\n" with
  | Some i -> String.sub s 0 (i + 4)
  | None -> s

let client_request ?body ?(headers = [ "Host", "a" ]) ?(meth = `GET) c target =
  Client.request
    ?body
    c
    (Request.create ~headers:(Headers.of_list headers) meth target)

let client_receive raw =
  let c = Client.create () in
  let ex = client_request c "/" in
  Client.feed c raw;
  ex

let check_malformed raw =
  let c = Client.create () in
  let ex = client_request c "/" in
  Client.feed c raw;
  Alcotest.check
    strings
    (Printf.sprintf "Malformed_response for %S" raw)
    [ "Malformed_response" ]
    ex.errors;
  Alcotest.(check int) "no response delivered" 0 (List.length ex.responses);
  Alcotest.(check bool) "client connection closed" true (Client.is_closed c)

let only_response (ex : Client.exchange) =
  Alcotest.check strings "no client error" [] ex.errors;
  match ex.responses with
  | [ r ] -> r
  | rs -> Alcotest.failf "expected one response, got %d" (List.length rs)

let server_writes headers ?(reason = "OK") () =
  let raised = ref false in
  let s =
    Server.run
      ~handler:(fun ex ->
        try
          Request_descriptor.respond_with_string
            ex.reqd
            (Response.create
               ~reason
               ~headers:(Headers.of_list (("Content-Length", "0") :: headers))
               `OK)
            ""
        with
        | Invalid_argument _ -> raised := true)
      (get "/")
  in
  !raised, head_of (Server.output s)

let client_writes ?(headers = []) target =
  let c = Client.create () in
  let raised =
    match client_request ~headers:(("Host", "a") :: headers) c target with
    | _ -> false
    | exception Invalid_argument _ -> true
  in
  raised, Client.written c

let check_app_rejected what bad (raised, out) =
  if not raised
  then Alcotest.failf "%s: no Invalid_argument at the application's call" what;
  if bad out then Alcotest.failf "%s: invalid bytes on the wire: %S" what out

let host_request ?(version = "HTTP/1.1") fields =
  "GET / " ^ version ^ "\r\n" ^ fields ^ "\r\n"

let tests =
  [ test
      "§2.1 server DEF: start-line, field lines, empty line, then the body"
      (fun () ->
         let s =
           Server.run
             "POST /p HTTP/1.1\r\nHost: a\r\nContent-Length: 3\r\n\r\nabc"
         in
         check_status "200" [ 200 ] s;
         let r = only_request s in
         Alcotest.check meth "method" `POST r.meth;
         Alcotest.(check string) "target" "/p" r.target;
         Alcotest.(check string) "body" "abc" (List.hd s.Server.exchanges).body)
  ; test
      "§2.1 server DEF: no dispatch before the empty line ends the head"
      (fun () ->
         let s = serve "GET / HTTP/1.1\r\nHost: a\r\n" in
         Alcotest.(check int) "handler not called" 0 (Server.handler_calls s);
         Alcotest.(check string) "nothing written" "" (Server.output s))
  ; test
      "§2.1 server DEF: a response sent to the server gets 400 and close"
      (fun () ->
         check_rejected (serve "HTTP/1.1 200 OK\r\nContent-Length: 0\r\n\r\n"))
  ; test
      "§2.2 server MUST: octets >= 0x80 in a field value are kept as bytes"
      (fun () ->
         let s =
           serve
             (host_request "Host: a\r\nX: \xe5\x98\x8a\xe5\x98\x8d\r\nY: 1\r\n")
         in
         Alcotest.check
           strings
           "X"
           [ "\xe5\x98\x8a\xe5\x98\x8d" ]
           (values s "x");
         Alcotest.check strings "Y" [ "1" ] (values s "y"))
  ; test
      "§2.2 client MUST: octets >= 0x80 in a field value are kept as bytes"
      (fun () ->
         let ex =
           client_receive
             "HTTP/1.1 200 OK\r\n\
              X: \xe5\x98\x8a\xe5\x98\x8d\r\n\
              Content-Length: 0\r\n\
              \r\n"
         in
         Alcotest.check
           strings
           "X"
           [ "\xe5\x98\x8a\xe5\x98\x8d" ]
           (Headers.get_multi (only_response ex).headers "x"))
  ; test
      "§2.2 server MAY: bare LF ending the request-line gets 400 and close"
      (fun () -> check_rejected (serve "GET / HTTP/1.1\nHost: a\n\n"))
  ; test
      "§2.2 server MAY: bare LF ending a field line gets 400 and close"
      (fun () ->
         check_rejected_each
           [ "GET / HTTP/1.1\r\nHost: a\nX: 1\r\n\r\n"
           ; "GET / HTTP/1.1\r\nHost: a\n\r\n"
           ; "GET / HTTP/1.1\r\nHost: a\r\n\n"
           ])
  ; test
      "§2.2 client MAY: bare LF line endings are a malformed response"
      (fun () ->
         List.iter
           check_malformed
           [ "HTTP/1.1 200 OK\nContent-Length: 0\n\n"
           ; "HTTP/1.1 200 OK\nContent-Length: 0\r\n\r\n"
           ; "HTTP/1.1 200 OK\r\nContent-Length: 0\n\r\n"
           ])
  ; test
      "§2.2 both MUST NOT: no bare CR from the app in a protocol element"
      (fun () ->
         let checks =
           [ "server field value", server_writes [ "X-A", "a\rb" ] ()
           ; "server reason", server_writes [] ~reason:"O\rK" ()
           ; "client field value", client_writes ~headers:[ "X-A", "a\rb" ] "/"
           ; "client target", client_writes "/a\rb"
           ]
         in
         List.iter
           (fun (what, result) -> check_app_rejected what has_bare_cr result)
           checks)
  ; test "§2.2 server MUST: bare CR before a line end gets 400" (fun () ->
      check_rejected_each
        [ "GET / HTTP/1.1\r\r\nHost: a\r\n\r\n"; host_request "Host: a\r\r\n" ])
  ; test "§2.2 server MUST: bare CR in the method or target gets 400" (fun () ->
      check_rejected_each
        [ "G\rET / HTTP/1.1\r\nHost: a\r\n\r\n"
        ; "GET /a\rb HTTP/1.1\r\nHost: a\r\n\r\n"
        ])
  ; test "§2.2 server MUST: bare CR in a field name gets 400" (fun () ->
      check_rejected_each [ host_request "Host: a\r\nX\rY: 1\r\n" ])
  ; test "§2.2 server MUST: bare CR in a field value gets 400" (fun () ->
      check_rejected_each [ host_request "Host: a\r\nX: a\rb\r\n" ])
  ; test
      "§2.2 client MUST: bare CR in the status-line is a malformed response"
      (fun () ->
         List.iter
           check_malformed
           [ "HTTP/1.1 200 O\rK\r\nContent-Length: 0\r\n\r\n"
           ; "HTTP/1.1 200\r OK\r\nContent-Length: 0\r\n\r\n"
           ])
  ; test
      "§2.2 client MUST: bare CR in a field name is a malformed response"
      (fun () ->
         check_malformed
           "HTTP/1.1 200 OK\r\nX\rY: 1\r\nContent-Length: 0\r\n\r\n")
  ; test "§2.2 client MUST NOT: no CRLF before or after a request" (fun () ->
      let c =
        Client.create
          ~config:{ Config.default with max_pipelined_requests = 16 }
          ()
      in
      let _ =
        client_request
          ~meth:`PUT
          ~headers:[ "Host", "a"; "Content-Length", "3" ]
          ~body:"abc"
          c
          "/p"
      in
      let _ = client_request c "/q" in
      Alcotest.(check string)
        "bytes written"
        "PUT /p HTTP/1.1\r\n\
         Host: a\r\n\
         Content-Length: 3\r\n\
         \r\n\
         abcGET /q HTTP/1.1\r\n\
         Host: a\r\n\
         \r\n"
        (Client.written c))
  ; test
      "§2.2 server SHOULD: one empty line before the request-line is ignored"
      (fun () -> check_accepted ~target:"/" ("\r\n" ^ get "/"))
  ; test
      "§2.2 server SHOULD: an empty line between pipelined requests is ignored"
      (fun () ->
         let s = serve (get "/a" ^ "\r\n" ^ get "/b") in
         check_status "two 200" [ 200; 200 ] s;
         Alcotest.check strings "targets" [ "/a"; "/b" ] (targets s);
         Alcotest.(check (list meth))
           "methods"
           [ `GET; `GET ]
           (List.map (fun (r : Request.t) -> r.meth) (Server.requests s)))
  ; test
      "§2.2 server SHOULD: two empty lines before the request-line are ignored \
       or get 400"
      (fun () ->
         let s = serve ("\r\n\r\n" ^ get "/") in
         match Server.requests s with
         | [] -> check_rejected s
         | [ r ] -> Alcotest.check meth "method" `GET r.meth
         | _ -> Alcotest.fail "more than one request")
  ; test
      "§2.2 server MUST: whitespace before the first field line is rejected"
      (fun () ->
         List.iter
           (fun input ->
              let s = serve input in
              check_never_seen ~name:"host" ~value:"evil" s;
              check_never_seen ~name:"transfer-encoding" ~value:"chunked" s;
              if Server.handler_calls s = 0 then check_rejected s)
           [ "GET / HTTP/1.1\r\n Host: evil\r\nHost: a\r\n\r\n"
           ; "GET / HTTP/1.1\r\n\tHost: evil\r\nHost: a\r\n\r\n"
           ; "POST / HTTP/1.1\r\n\
             \ Transfer-Encoding: chunked\r\n\
              Host: a\r\n\
              Content-Length: 3\r\n\
              \r\n\
              abc"
           ])
  ; test
      "§2.2 client MUST: whitespace before the first field line is rejected"
      (fun () ->
         check_malformed
           "HTTP/1.1 200 OK\r\n\
           \ Transfer-Encoding: chunked\r\n\
            Content-Length: 2\r\n\
            \r\n\
            ok")
  ; test
      "§2.2 both MUST NOT: no whitespace from the app before the first field"
      (fun () ->
         check_app_rejected "server" has_fold (server_writes [ " X-A", "1" ] ());
         check_app_rejected
           "client"
           has_fold
           (client_writes ~headers:[ "\tX-A", "1" ] "/"))
  ; test
      "§2.2 server SHOULD: 400 and close for octets that are not HTTP"
      (fun () ->
         check_rejected_each
           [ "\x16\x03\x01\x00\xa5\x01\x00\x00\xa1\x03\x03"; "hello\r\n\r\n" ])
  ; test "§2.3 server DEF: HTTP-version is case-sensitive" (fun () ->
      check_rejected_each
        (List.map
           (fun version -> req ~version "GET" "/")
           [ "http/1.1"; "Http/1.1"; "HTTP/01.1"; "HTTP/1.1 " ]))
  ; test "§2.3 client DEF: HTTP-version is case-sensitive" (fun () ->
      List.iter
        (fun version ->
           check_malformed (version ^ " 200 OK\r\nContent-Length: 0\r\n\r\n"))
        [ "http/1.1"; "HTTP/01.1"; "HTTP/1.10"; "HTTP/1" ])
  ; test
      "§3 server DEF: method SP request-target SP HTTP-version is accepted"
      (fun () -> check_accepted ~target:"/a/b" (get "/a/b"))
  ; test "§3 server SHOULD: invalid request-line gets 400 and close" (fun () ->
      check_rejected_each
        [ "GET  / HTTP/1.1\r\nHost: a\r\n\r\n"
        ; "GET /  HTTP/1.1\r\nHost: a\r\n\r\n"
        ; " GET / HTTP/1.1\r\nHost: a\r\n\r\n"
        ; "GET / HTTP/1.1 \r\nHost: a\r\n\r\n"
        ; "GET /a b HTTP/1.1\r\nHost: a\r\n\r\n"
        ; "GET /\r\nHost: a\r\n\r\n"
        ])
  ; test
      "§3 server SHOULD: 400 for a missing version, empty method or empty \
       target"
      (fun () ->
         check_rejected_each
           [ "GET / \r\n\r\n"
           ; "GET /\r\n\r\n"
           ; " / HTTP/1.1\r\nHost: a\r\n\r\n"
           ; "GET  HTTP/1.1\r\nHost: a\r\n\r\n"
           ])
  ; test
      "§3 server MAY: HTAB between request-line parts is rejected or read as SP"
      (fun () ->
         let s = serve "GET\t/ HTTP/1.1\r\nHost: a\r\n\r\n" in
         match Server.requests s with
         | [] -> check_rejected s
         | [ r ] ->
           Alcotest.check meth "method" `GET r.meth;
           Alcotest.(check string) "target" "/" r.target
         | _ -> Alcotest.fail "more than one request")
  ; test "§3.1 server DEF: the method is case-sensitive" (fun () ->
      let s = serve (req "get" "/") in
      Alcotest.check meth "method" (`Other "get") (only_request s).meth)
  ; test
      "§3 server SHOULD: a long unknown method reaches the handler for a 501"
      (fun () ->
         let name = String.make 200 'M' in
         let s =
           Server.run
             ~handler:(fun (ex : Server.exchange) ->
               match ex.request.meth with
               | `Other _ -> Server.respond `Not_implemented ex
               | _ -> Server.respond `OK ex)
             (req name "/")
         in
         check_status "501" [ 501 ] s)
  ; test
      "§3 server SHOULD: 501 for a method longer than the head limit"
      (fun () ->
         check_rejected
           ~allowed:[ 501 ]
           (serve (req (String.make 20000 'M') "/")))
  ; test
      "§3 server MUST: 414 for a request-target longer than the head limit"
      (fun () ->
         let config = { Config.default with max_header_size = 1024 } in
         check_rejected
           ~allowed:[ 414 ]
           (serve ~config (get ("/" ^ String.make 2000 'a'))))
  ; test
      "§3 server RECOMMENDED: a request-line of 8000 octets is accepted"
      (fun () ->
         let target =
           "/" ^ String.make (8000 - String.length "GET / HTTP/1.1") 'a'
         in
         Alcotest.(check int)
           "request-line length"
           8000
           (String.length ("GET " ^ target ^ " HTTP/1.1"));
         check_accepted ~target (get target))
  ; test
      "§3.2.1 server DEF: origin-form with a query reaches the handler"
      (fun () -> check_accepted ~target:"/a/b?x=1&y=%20" (get "/a/b?x=1&y=%20"))
  ; test
      "§3.2.2 server MUST: absolute-form is accepted, Host left to the app"
      (fun () ->
         let s =
           serve
             (req
                ~headers:[ "Host", "other.example" ]
                "GET"
                "http://example.org/p?q=1")
         in
         check_status "200" [ 200 ] s;
         Alcotest.check
           strings
           "target"
           [ "http://example.org/p?q=1" ]
           (targets s);
         Alcotest.check strings "Host" [ "other.example" ] (values s "host"))
  ; test
      "§3.2.3 server DEF: authority-form with CONNECT reaches the handler"
      (fun () ->
         let s =
           Server.run
             ~handler:ignore
             (req
                ~headers:[ "Host", "example.com:443" ]
                "CONNECT"
                "example.com:443")
         in
         Alcotest.check meth "method" `CONNECT (only_request s).meth;
         Alcotest.check strings "target" [ "example.com:443" ] (targets s))
  ; test
      "§3.2.4 server DEF: asterisk-form with OPTIONS reaches the handler"
      (fun () ->
         let s = serve (req "OPTIONS" "*") in
         check_status "200" [ 200 ] s;
         Alcotest.check meth "method" `OPTIONS (only_request s).meth;
         Alcotest.check strings "target" [ "*" ] (targets s))
  ; test
      "§3.2 server SHOULD: 400 for asterisk-form or authority-form outside \
       OPTIONS and CONNECT"
      (fun () ->
         check_rejected_each
           [ req "GET" "*"; req "GET" "127.0.0.1:80"; req "POST" "[::1]:8080" ])
  ; test "§3.2 server MUST: 400 for an HTTP/1.1 request without Host" (fun () ->
      check_rejected (serve (host_request "")))
  ; test
      "§3.2 server MUST: an HTTP/1.0 request without Host is accepted"
      (fun () ->
         check_accepted ~target:"/" (host_request ~version:"HTTP/1.0" ""))
  ; test "§3.2 server MUST: 400 for more than one Host field line" (fun () ->
      check_rejected_each
        (List.concat_map
           (fun version ->
              [ host_request ~version "Host: a\r\nHost: a\r\n"
              ; host_request ~version "Host: a\r\nHost: b\r\n"
              ; host_request ~version "Host: a\r\nhost: a\r\n"
              ])
           [ "HTTP/1.1"; "HTTP/1.0" ]))
  ; test
      "§3.2 server MUST: 400 for a Host field with an invalid value"
      (fun () ->
         check_rejected_each
           (List.map
              (fun v -> host_request ("Host: " ^ v ^ "\r\n"))
              [ "a b"; "a/b"; "a@b"; "a:port"; "a?b"; "a#b"; "[::1" ]))
  ; test
      "§3.2 server DEF: valid Host values are accepted, including empty"
      (fun () ->
         List.iter
           (fun v ->
              check_accepted ~target:"/" (host_request ("Host: " ^ v ^ "\r\n")))
           [ ""
           ; "a"
           ; "a:"
           ; "a:8080"
           ; "127.0.0.1:80"
           ; "[::1]:443"
           ; "xn--d-1ga.example"
           ; "a,b"
           ])
  ; test
      "§4 client DEF: an empty reason-phrase after the SP is accepted"
      (fun () ->
         let ex =
           client_receive "HTTP/1.1 200 \r\nContent-Length: 2\r\n\r\nok"
         in
         Alcotest.(check int)
           "status"
           200
           (Status.to_code (only_response ex).status);
         Alcotest.(check string) "body" "ok" ex.body)
  ; test
      "§4 client DEF: a reason-phrase with HTAB and obs-text is accepted"
      (fun () ->
         let ex =
           client_receive
             "HTTP/1.1 200 O\tK \xe9t\xe9\r\nContent-Length: 0\r\n\r\n"
         in
         Alcotest.(check int)
           "status"
           200
           (Status.to_code (only_response ex).status))
  ; test
      "§4 client SHOULD: the status comes from the code, not the reason"
      (fun () ->
         let ex =
           client_receive "HTTP/1.1 404 OK\r\nContent-Length: 0\r\n\r\n"
         in
         Alcotest.(check int)
           "status"
           404
           (Status.to_code (only_response ex).status))
  ; test
      "§4 client MAY: extra whitespace in the status-line is rejected or read \
       as SP"
      (fun () ->
         List.iter
           (fun raw ->
              let ex = client_receive raw in
              match ex.responses with
              | [] ->
                Alcotest.check strings raw [ "Malformed_response" ] ex.errors
              | [ r ] -> Alcotest.(check int) raw 200 (Status.to_code r.status)
              | _ -> Alcotest.fail "more than one response")
           [ "HTTP/1.1  200 OK\r\nContent-Length: 0\r\n\r\n"
           ; "HTTP/1.1\t200 OK\r\nContent-Length: 0\r\n\r\n"
           ])
  ; test
      "§4 server MUST: the SP after the status-code is sent with an empty \
       reason"
      (fun () ->
         let _, out = server_writes [] ~reason:"" () in
         Alcotest.(check string)
           "status-line"
           "HTTP/1.1 200 \r\n"
           (String.sub out 0 (min 15 (String.length out))))
  ; test
      "§5.1 server MUST: 400 for whitespace between field name and colon"
      (fun () ->
         check_rejected_each
           [ "GET / HTTP/1.1\r\n\
              Host : a\r\n\
              \r\n\
              GET /smuggled HTTP/1.1\r\n\
              Host: a\r\n\
              \r\n"
           ; host_request "Host: a\r\nTransfer-Encoding\t: chunked\r\n"
           ])
  ; test
      "§5 client DEF: whitespace between field name and colon is rejected"
      (fun () ->
         check_malformed "HTTP/1.1 200 OK\r\nContent-Length : 0\r\n\r\n")
  ; test "§5.2 server MUST: obs-fold in a request gets 400 and close" (fun () ->
      List.iter
        (fun input ->
           let s = serve input in
           List.iter
             (fun (r : Request.t) ->
                List.iter
                  (fun (_, v) ->
                     if String.contains v '\r' || String.contains v '\n'
                     then Alcotest.failf "handler saw CR or LF in %S" v)
                  (Headers.to_list r.headers))
             (Server.requests s);
           check_never_seen ~name:"transfer-encoding" ~value:"chunked" s;
           if Server.handler_calls s = 0 then check_rejected s)
        [ host_request "Host: a\r\nX-A: a\r\n b\r\n"
        ; host_request "Host: a\r\nX-A: a\r\n\tb\r\n"
        ; "POST / HTTP/1.1\r\n\
           Host: a\r\n\
           X-A: a\r\n\
          \ Transfer-Encoding: chunked\r\n\
           Content-Length: 3\r\n\
           \r\n\
           abc"
        ])
  ; test
      "§5.2 client MUST: obs-fold in a response is replaced with SP"
      (fun () ->
         let ex =
           client_receive
             "HTTP/1.1 200 OK\r\nX-A: a\r\n  b\r\nContent-Length: 0\r\n\r\n"
         in
         match Headers.get_multi (only_response ex).headers "x-a" with
         | [ v ]
           when String.length v >= 3
                && v.[0] = 'a'
                && v.[String.length v - 1] = 'b'
                && String.for_all
                     (( = ) ' ')
                     (String.sub v 1 (String.length v - 2)) ->
           ()
         | vs -> Alcotest.failf "X-A values: [%s]" (String.concat "; " vs))
  ; test
      "§5.2 both MUST NOT: no obs-fold from the app in a field value"
      (fun () ->
         check_app_rejected
           "server"
           has_fold
           (server_writes [ "X-A", "a\r\n b" ] ());
         check_app_rejected
           "client"
           has_fold
           (client_writes ~headers:[ "X-A", "a\r\n\tb" ] "/"))
  ]
