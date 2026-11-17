open Ache
open Rfc

let head fields = "GET / HTTP/1.1\r\nHost: example.com\r\n" ^ fields ^ "\r\n"

let ok_response fields =
  "HTTP/1.1 200 OK\r\n" ^ fields ^ "Content-Length: 0\r\n\r\n"

let strings = Alcotest.(list string)
let serve fields = Server.run ~handler:(Server.respond `OK) (head fields)

let only_request s =
  match Server.requests s with
  | [ r ] -> r
  | rs -> Alcotest.failf "expected one request, got %d" (List.length rs)

let server_values s name = Headers.get_multi (only_request s).headers name

let client_get () =
  let c = Client.create () in
  let ex =
    Client.request
      c
      (Request.create
         ~headers:(Headers.of_list [ "Host", "example.com" ])
         `GET
         "/")
  in
  c, ex

let client_receive raw =
  let c, ex = client_get () in
  Client.feed c raw;
  ex

let only_response (ex : Client.exchange) =
  Alcotest.check strings "no client error" [] ex.errors;
  match ex.responses with
  | [ r ] -> r
  | rs -> Alcotest.failf "expected one response, got %d" (List.length rs)

let client_values ex name = Headers.get_multi (only_response ex).headers name

let check_server_rejects_or_replaces ~name ~replaced fields =
  let s = serve fields in
  match Server.requests s with
  | [] -> check_bad_request ~msg:(Printf.sprintf "400 for %S" fields) s
  | _ ->
    Alcotest.check
      strings
      (Printf.sprintf "value after SP replacement for %S" fields)
      [ replaced ]
      (server_values s name)

let check_client_rejects_or_replaces ~name ~replaced fields =
  let ex = client_receive (ok_response fields) in
  match ex.responses with
  | [] ->
    Alcotest.check
      strings
      (Printf.sprintf "Malformed_response for %S" fields)
      [ "Malformed_response" ]
      ex.errors
  | _ ->
    Alcotest.check
      strings
      (Printf.sprintf "value after SP replacement for %S" fields)
      [ replaced ]
      (client_values ex name)

let check_server_rejects fields =
  check_bad_request ~msg:(Printf.sprintf "400 for %S" fields) (serve fields)

let check_client_rejects fields =
  let ex = client_receive (ok_response fields) in
  Alcotest.(check int)
    (Printf.sprintf "no response delivered for %S" fields)
    0
    (List.length ex.responses);
  Alcotest.check strings "Malformed_response" [ "Malformed_response" ] ex.errors

let delimiter_names =
  [ "X(y"
  ; "X)y"
  ; "X,y"
  ; "X/y"
  ; "X;y"
  ; "X<y"
  ; "X=y"
  ; "X>y"
  ; "X?y"
  ; "X@y"
  ; "X[y"
  ; "X\\y"
  ; "X]y"
  ; "X{y"
  ; "X}y"
  ; "X\"y"
  ]

let non_tchar_names = [ "X\x01y"; "X\x7fy"; "X\x80y"; "X\xc3\xa9" ]
let all_tchar_name = "!#$%&'*+-.^_`|~0123456789abcdefghijklmnopqrstuvwxyzABCZ"

let is_tchar = function
  | 'a' .. 'z'
  | 'A' .. 'Z'
  | '0' .. '9'
  | '!' | '#' | '$' | '%' | '&' | '\'' | '*' | '+' | '-' | '.' | '^' | '_' | '`'
  | '|' | '~' ->
    true
  | _ -> false

let is_field_byte = function
  | '\t' | ' ' .. '~' | '\128' .. '\255' -> true
  | _ -> false

let split_crlf s =
  let rec go acc i =
    match Wire.index_from s i "\r\n" with
    | None -> List.rev (String.sub s i (String.length s - i) :: acc)
    | Some j -> go (String.sub s i (j - i) :: acc) (j + 2)
  in
  go [] 0

let check_generated_head raw =
  let head =
    match Wire.index_from raw 0 "\r\n\r\n" with
    | Some i -> String.sub raw 0 i
    | None -> raw
  in
  match split_crlf head with
  | [] | [ "" ] -> ()
  | _start_line :: lines ->
    List.iter
      (fun line ->
         match String.index_opt line ':' with
         | None -> Alcotest.failf "field line without colon: %S" line
         | Some j ->
           let name = String.sub line 0 j in
           let value = String.sub line (j + 1) (String.length line - j - 1) in
           if name = "" || not (String.for_all is_tchar name)
           then Alcotest.failf "invalid field-name in %S" line;
           if not (String.for_all is_field_byte value)
           then Alcotest.failf "invalid field-value in %S" line;
           if Headers.ci_equal name "injected"
           then Alcotest.failf "injected field line %S" line)
      lines

let server_generates headers =
  let s =
    Server.run
      ~handler:(fun ex -> try Server.respond ~headers `OK ex with _ -> ())
      (head "")
  in
  Server.output s

let client_generates headers =
  let c = Client.create () in
  (try
     ignore
       (Client.request
          c
          (Request.create
             ~headers:(Headers.of_list (("Host", "example.com") :: headers))
             `GET
             "/"))
   with
  | _ -> ());
  Client.written c

let invalid_generated_names =
  [ "Bad Name", "v"; "X(y)", "v"; "", "v"; "X\r\nInjected", "v" ]

let invalid_generated_values =
  [ "X-A", "a\r\nInjected: 1"
  ; "X-A", "a\nInjected: 1"
  ; "X-A", "a\rInjected: 1"
  ; "X-A", "a\000b"
  ]

let tests =
  [ test
      "§2.2 server MUST NOT: no invalid field-name from the app on the wire"
      (fun () ->
         List.iter
           (fun h -> check_generated_head (server_generates [ h ]))
           invalid_generated_names)
  ; test
      "§2.2 server MUST NOT: no CR, LF or NUL from the app in a field value"
      (fun () ->
         List.iter
           (fun h -> check_generated_head (server_generates [ h ]))
           invalid_generated_values)
  ; test
      "§2.2 client MUST NOT: no invalid field-name from the app on the wire"
      (fun () ->
         List.iter
           (fun h -> check_generated_head (client_generates [ h ]))
           invalid_generated_names)
  ; test
      "§2.2 client MUST NOT: no CR, LF or NUL from the app in a field value"
      (fun () ->
         List.iter
           (fun h -> check_generated_head (client_generates [ h ]))
           invalid_generated_values)
  ; test
      "§2.2 both MUST NOT: valid fields are written as name colon SP value CRLF"
      (fun () ->
         let fields = [ "X-A", "1"; "X-B", "a, \"b\" (c)" ] in
         let out = server_generates fields in
         check_generated_head out;
         Alcotest.(check bool)
           "server writes X-A: 1"
           true
           (contains out "\r\nX-A: 1\r\n");
         Alcotest.(check bool)
           "server writes X-B unchanged"
           true
           (contains out "\r\nX-B: a, \"b\" (c)\r\n");
         let written = client_generates fields in
         check_generated_head written;
         Alcotest.(check bool)
           "client writes X-A: 1"
           true
           (contains written "\r\nX-A: 1\r\n");
         Alcotest.(check bool)
           "client writes X-B unchanged"
           true
           (contains written "\r\nX-B: a, \"b\" (c)\r\n"))
  ; test
      "§2.3 server SHOULD: 400 for field lines that do not match the grammar"
      (fun () ->
         List.iter
           check_server_rejects
           [ "NoColon\r\n"; "\x00\xff\x01garbage\r\n"; "X: 1\r\nNoColon\r\n" ])
  ; test
      "§2.3 client SHOULD: Malformed_response for field lines off the grammar"
      (fun () ->
         List.iter
           check_client_rejects
           [ "NoColon\r\n"; "\x00\xff\x01garbage\r\n"; "X: 1\r\nNoColon\r\n" ])
  ; test
      "§2.3 both MUST: an 8 KiB field value the library writes is parsed back"
      (fun () ->
         let big = String.make 8192 'v' in
         let c = Client.create () in
         let ex =
           Client.request
             c
             (Request.create
                ~headers:
                  (Headers.of_list [ "Host", "example.com"; "X-Big", big ])
                `GET
                "/")
         in
         let s =
           Server.run
             ~handler:(Server.respond ~headers:[ "X-Big", big ] `OK)
             (Client.written c)
         in
         Alcotest.check
           strings
           "server sees the value"
           [ big ]
           (server_values s "x-big");
         Client.feed c (Server.output s);
         Alcotest.check
           strings
           "client sees the value"
           [ big ]
           (client_values ex "x-big"))
  ; test
      "§5.1 server DEF: a field-name of every tchar reaches the handler"
      (fun () ->
         let s = serve (all_tchar_name ^ ": v\r\n") in
         check_status "200" [ 200 ] s;
         let names = List.map fst (Headers.to_list (only_request s).headers) in
         Alcotest.check strings "names" [ "Host"; all_tchar_name ] names)
  ; test "§5.1 client DEF: field-name may use every tchar" (fun () ->
      let ex = client_receive (ok_response (all_tchar_name ^ ": v\r\n")) in
      Alcotest.check strings "value" [ "v" ] (client_values ex all_tchar_name))
  ; test
      "§5.1 server DEF: 400 for a field-name that contains a delimiter"
      (fun () ->
         List.iter
           (fun n -> check_server_rejects (n ^ ": v\r\n"))
           delimiter_names)
  ; test
      "§5.1 server DEF: 400 for a field-name with a CTL, DEL or non-ASCII byte"
      (fun () ->
         List.iter
           (fun n -> check_server_rejects (n ^ ": v\r\n"))
           non_tchar_names)
  ; test "§5.1 server DEF: 400 for an empty field-name" (fun () ->
      check_server_rejects ": v\r\n")
  ; test
      "§5.1 client DEF: Malformed_response for an invalid field-name"
      (fun () ->
         List.iter
           (fun n -> check_client_rejects (n ^ ": v\r\n"))
           ("" :: (delimiter_names @ non_tchar_names)))
  ; test "§5.1 server DEF: field names are case-insensitive" (fun () ->
      let s =
        Server.run
          (req
             ~headers:[ "hOsT", "example.com"; "cOnTeNt-LeNgTh", "5" ]
             ~body:"hello"
             "POST"
             "/")
      in
      Alcotest.check int_list "one 200" [ 200 ] (Server.statuses s);
      Alcotest.(check string)
        "body framed by cOnTeNt-LeNgTh"
        "hello"
        (Server.response s).body;
      let r = only_request s in
      Alcotest.(check (option string))
        "lookup with other case"
        (Some "5")
        (Headers.get r.headers "CONTENT-LENGTH");
      check_open "connection kept" s)
  ; test "§5.1 client DEF: field names are case-insensitive" (fun () ->
      let c, ex = client_get () in
      Client.feed c "HTTP/1.1 200 OK\r\ncOnTeNt-LeNgTh: 3\r\n\r\nabc";
      Alcotest.(check string) "body framed by cOnTeNt-LeNgTh" "abc" ex.body;
      Alcotest.(check bool) "body complete" true ex.body_eof;
      Alcotest.(check (option string))
        "lookup with other case"
        (Some "3")
        (Headers.get (only_response ex).headers "Content-Length"))
  ; test "§5.1 server SHOULD: unrecognized fields are ignored" (fun () ->
      let s =
        Server.run (head "X-Unknown-Field: ???\r\nFoo: (bar)\r\n" ^ head "")
      in
      check_status "two 200" [ 200; 200 ] s;
      check_open "connection kept" s)
  ; test "§5.1 client SHOULD: unrecognized fields are ignored" (fun () ->
      let ex =
        client_receive
          "HTTP/1.1 200 OK\r\n\
           X-Unknown-Field: ???\r\n\
           Content-Length: 2\r\n\
           Foo: (bar)\r\n\
           \r\n\
           ok"
      in
      Alcotest.check int_list "status" [ 200 ] (Client.statuses ex);
      Alcotest.(check string) "body" "ok" ex.body;
      Alcotest.check strings "no error" [] ex.errors)
  ; test "§5.2 server DEF: repeated field lines keep their order" (fun () ->
      let s =
        serve "Accept: a\r\nX: 1\r\nAccept: b, c\r\naccept: d\r\nX: 2\r\n"
      in
      Alcotest.check
        strings
        "accept"
        [ "a"; "b, c"; "d" ]
        (server_values s "Accept");
      Alcotest.check strings "x" [ "1"; "2" ] (server_values s "x");
      Alcotest.check
        strings
        "all names in order"
        [ "Host"; "Accept"; "X"; "Accept"; "accept"; "X" ]
        (List.map fst (Headers.to_list (only_request s).headers)))
  ; test "§5.2 client DEF: repeated field lines keep their order" (fun () ->
      let ex =
        client_receive
          (ok_response "Vary: a\r\nX: 1\r\nVary: b, c\r\nvary: d\r\n")
      in
      Alcotest.check
        strings
        "vary"
        [ "a"; "b, c"; "d" ]
        (client_values ex "Vary"))
  ; test
      "§5.3 server MUST NOT: handler is not called before the empty line"
      (fun () ->
         let input = head "X-Late: 1\r\nX-Late: 2\r\n" in
         let s = Server.create ~handler:(Server.respond `OK) () in
         String.iteri
           (fun i ch ->
              if i < String.length input - 1
              then (
                Server.feed s (String.make 1 ch);
                Alcotest.(check int)
                  (Printf.sprintf "no handler call after %d bytes" (i + 1))
                  0
                  (Server.handler_calls s);
                Alcotest.(check string) "no output" "" (Server.output s)))
           input;
         Server.feed s "\n";
         Alcotest.(check int) "handler called once" 1 (Server.handler_calls s);
         Alcotest.check
           strings
           "late lines seen"
           [ "1"; "2" ]
           (server_values s "x-late");
         check_status "200" [ 200 ] s)
  ; test "§5.4 server MUST: 4xx for a header section of 1 MiB" (fun () ->
      let line i =
        Printf.sprintf "X-Fill-%05d: %s\r\n" i (String.make 500 'f')
      in
      let s = serve (String.concat "" (List.init 2048 line)) in
      let codes = Server.statuses s in
      Alcotest.(check bool)
        (Printf.sprintf
           "one 4xx, got [%s]"
           (String.concat ";" (List.map string_of_int codes)))
        true
        (match codes with [ c ] -> c >= 400 && c < 500 | _ -> false);
      Alcotest.(check int)
        "request handler not called"
        0
        (Server.handler_calls s))
  ; test "§5.4 server MUST: 4xx for one field line of 1 MiB" (fun () ->
      let s = serve ("X-Big: " ^ String.make (1 lsl 20) 'b' ^ "\r\n") in
      let codes = Server.statuses s in
      Alcotest.(check bool)
        (Printf.sprintf
           "one 4xx, got [%s]"
           (String.concat ";" (List.map string_of_int codes)))
        true
        (match codes with [ c ] -> c >= 400 && c < 500 | _ -> false);
      Alcotest.(check int)
        "request handler not called"
        0
        (Server.handler_calls s))
  ; test
      "§5.5 server MUST: leading and trailing whitespace is not in the value"
      (fun () ->
         let s = serve "A: \t a\tb \t\r\nB:c\r\nC:\r\nD: \t \r\n" in
         Alcotest.check strings "A" [ "a\tb" ] (server_values s "a");
         Alcotest.check strings "B" [ "c" ] (server_values s "b");
         Alcotest.check strings "C" [ "" ] (server_values s "c");
         Alcotest.check strings "D" [ "" ] (server_values s "d"))
  ; test
      "§5.5 client MUST: leading and trailing whitespace is not in the value"
      (fun () ->
         let ex =
           client_receive (ok_response "A: \t a\tb \t\r\nB:c\r\nC:\r\n")
         in
         Alcotest.check strings "A" [ "a\tb" ] (client_values ex "a");
         Alcotest.check strings "B" [ "c" ] (client_values ex "b");
         Alcotest.check strings "C" [ "" ] (client_values ex "c"))
  ; test
      "§5.5 server SHOULD: obs-text in a value reaches the handler unchanged"
      (fun () ->
         let s = serve "X: caf\xc3\xa9 \x80\xff\r\n" in
         Alcotest.check
           strings
           "value"
           [ "caf\xc3\xa9 \x80\xff" ]
           (server_values s "x"))
  ; test
      "§5.5 client SHOULD: obs-text in a value reaches the response unchanged"
      (fun () ->
         let ex = client_receive (ok_response "X: caf\xc3\xa9 \x80\xff\r\n") in
         Alcotest.check
           strings
           "value"
           [ "caf\xc3\xa9 \x80\xff" ]
           (client_values ex "x"))
  ; test
      "§5.5 server MUST: NUL in a value gives 400 or is replaced with SP"
      (fun () ->
         check_server_rejects_or_replaces
           ~name:"x"
           ~replaced:"a b"
           "X: a\000b\r\n")
  ; test
      "§5.5 server MUST: bare LF in a value gives 400 or is replaced with SP"
      (fun () ->
         check_server_rejects_or_replaces
           ~name:"x"
           ~replaced:"a b"
           "X: a\nb\r\n")
  ; test
      "§5.5 server MUST: bare CR in a value gives 400 or is replaced with SP"
      (fun () ->
         check_server_rejects_or_replaces
           ~name:"x"
           ~replaced:"a b"
           "X: a\rb\r\n")
  ; test
      "§5.5 client MUST: NUL in a value is rejected or replaced with SP"
      (fun () ->
         check_client_rejects_or_replaces
           ~name:"x"
           ~replaced:"a b"
           "X: a\000b\r\n")
  ; test
      "§5.5 client MUST: bare LF in a value is rejected or replaced with SP"
      (fun () ->
         check_client_rejects_or_replaces
           ~name:"x"
           ~replaced:"a b"
           "X: a\nb\r\n")
  ; test
      "§5.5 client MUST: bare CR in a value is rejected or replaced with SP"
      (fun () ->
         check_client_rejects_or_replaces
           ~name:"x"
           ~replaced:"a b"
           "X: a\rb\r\n")
  ; test
      "§5.5 server MAY: other CTL or DEL in a value gives 400 or is kept"
      (fun () ->
         List.iter
           (fun v ->
              let s = serve ("X: " ^ v ^ "\r\n") in
              match Server.requests s with
              | [] -> check_bad_request ~msg:(Printf.sprintf "400 for %S" v) s
              | _ ->
                Alcotest.check
                  strings
                  (Printf.sprintf "kept %S" v)
                  [ v ]
                  (server_values s "x"))
           [ "a\007b"; "a\027b"; "a\127b" ])
  ; test
      "§5.5 client MAY: other CTL or DEL in a value is rejected or kept"
      (fun () ->
         List.iter
           (fun v ->
              let ex = client_receive (ok_response ("X: " ^ v ^ "\r\n")) in
              match ex.responses with
              | [] ->
                Alcotest.check
                  strings
                  "Malformed_response"
                  [ "Malformed_response" ]
                  ex.errors
              | _ ->
                Alcotest.check
                  strings
                  (Printf.sprintf "kept %S" v)
                  [ v ]
                  (client_values ex "x"))
           [ "a\007b"; "a\027b"; "a\127b" ])
  ; test
      "§5.6.4 server DEF: quoted-string and comment values reach the handler"
      (fun () ->
         let v = "\"a, b\" ; q=\"x\\\"y\\\\z\", (c (nested) \\) d), \xe9" in
         let s = serve ("X: " ^ v ^ "\r\n") in
         Alcotest.check strings "value" [ v ] (server_values s "x"))
  ; test
      "§5.6.4 client DEF: quoted-string and comment values reach the client"
      (fun () ->
         let v = "\"a, b\" ; q=\"x\\\"y\\\\z\", (c (nested) \\) d), \xe9" in
         let ex = client_receive (ok_response ("X: " ^ v ^ "\r\n")) in
         Alcotest.check strings "value" [ v ] (client_values ex "x"))
  ]
