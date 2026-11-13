open Grammar_corpus

type outcome =
  | Accept of string
  | Reject

let outcome =
  Alcotest.of_pp (fun fmt -> function
    | Accept s -> Format.fprintf fmt "Accept %S" s
    | Reject -> Format.pp_print_string fmt "Reject")

(* Results of the head parsers of httpun 37fdcd8 on [Grammar_corpus.all], except
   four status codes that made httpun raise [Failure] and the rows where httpun
   accepted a method, field-name or field value outside RFC 9110 §9.1, §5.1 and
   §5.5. *)
let expected =
  [ Accept "\"GET\" \"/\" HTTP/1.1 []"
  ; Accept "\"some-other-verb\" \"/\" HTTP/1.1 []"
  ; Accept "\"GET\" \"/\" HTTP/1.1 [\"Link\":\"/path/to/some/website\"]"
  ; Accept "md5:5d59b2a9cb68f68156c4e602d3d3618b"
  ; Accept
      "\"POST\" \"/upload\" HTTP/1.1 [\"Host\":\"a\";\"Content-Length\":\"5\"]"
  ; Accept "\"GET\" \"/\" HTTP/1.0 []"
  ; Accept
      "\"GET\" \"http://example.com/x?y=1\" HTTP/1.1 [\"Host\":\"example.com\"]"
  ; Accept
      "\"CONNECT\" \"example.com:443\" HTTP/1.1 [\"Host\":\"example.com:443\"]"
  ; Accept "\"OPTIONS\" \"*\" HTTP/1.1 []"
  ; Accept "\"GET\" \"/\" HTTP/1.1 [\"X\":\"a\"]"
  ; Accept "\"GET\" \"/\" HTTP/1.1 [\"X\":\"\"]"
  ; Accept "\"GET\" \"/\" HTTP/1.1 [\"X\":\"\"]"
  ; Accept "\"GET\" \"/\" HTTP/1.1 [\"X\":\"val\"]"
  ; Accept "\"GET\" \"/\" HTTP/1.1 [\"X\":\"1\";\"X\":\"2\";\"Y\":\"3\"]"
  ; Accept "\"GET\" \"/\" HTTP/1.1 [\"X\":\"a:b\"]"
  ; Reject
  ; Accept "\"get\" \"/\" HTTP/1.1 []"
  ; Accept "\"GET\" \"/\" HTTP/2.0 []"
  ; Accept "\"GET\" \"/\" HTTP/1.1 []"
  ; Reject
  ; Reject
  ; Reject
  ; Reject
  ; Reject
  ; Reject
  ; Reject
  ; Reject
  ; Reject
  ; Reject
  ; Reject
  ; Reject
  ; Reject
  ; Reject
  ; Reject
  ; Reject
  ; Reject
  ; Reject
  ; Reject
  ; Reject
  ; Reject
  ; Reject
  ; Reject
  ; Accept "md5:b0d616918c3d1717ca32413df5a16752"
  ; Accept "md5:ecf8c5041016e0906cc3d2aac94a3c82"
  ; Accept "md5:555caf2783ecc303d87090628a1fb389"
  ; Accept "md5:6d36e436a7519fc9b1d1d86508dbe3ff"
  ; Accept "\"GET\" \"/\\195\\169\" HTTP/1.1 []"
  ; Accept "\"GET\" \"/\" HTTP/1.1 [\"X\":\"caf\\195\\169\"]"
  ; Reject
  ; Reject
  ; Reject
  ; Reject
  ; Reject
  ; Reject
  ; Reject
  ; Reject
  ; Reject
  ; Reject
  ; Reject
  ; Reject
  ; Reject
  ; Accept "\"GET\" \"/\" HTTP/1.2 []"
  ; Reject
  ; Reject
  ; Reject
  ; Reject
  ; Accept "\"GET\" \"/\" HTTP/1.1 [\"X\":\"a b\"]"
  ; Accept "HTTP/1.1 200 \"OK\" []"
  ; Accept "HTTP/1.1 404 \"Not Found\" [\"Content-Length\":\"0\"]"
  ; Accept "HTTP/1.0 200 \"OK\" []"
  ; Accept "HTTP/1.1 200 \"\" []"
  ; Accept "HTTP/1.1 999 \"Custom\" []"
  ; Accept "HTTP/1.1 600 \"x\" []"
  ; Accept "HTTP/1.1 100 \"Continue\" []"
  ; Accept "HTTP/1.1 200 \"OK\" [\"Transfer-Encoding\":\"chunked\"]"
  ; Accept
      "HTTP/1.1 101 \"Switching Protocols\" \
       [\"Upgrade\":\"websocket\";\"Connection\":\"Upgrade\"]"
  ; Accept "HTTP/1.1 200 \"OK\" [\"Set-Cookie\":\"a=1\";\"Set-Cookie\":\"b=2\"]"
  ; Accept "HTTP/1.1 304 \"Not Modified\" [\"ETag\":\"\\\"x\\\"\"]"
  ; Accept "HTTP/1.1 200 \"\\226\\156\\147\" []"
  ; Accept "HTTP/1.1 200 \"OK\\tfoo\" []"
  ; Accept "md5:3af961f8ea3aa8bb66d161d080ff6d97"
  ; Reject
  ; Reject
  ; Reject
  ; Reject
  ; Reject
  ; Reject
  ; Reject
  ; Reject
  ; Reject
  ; Reject
  ; Reject
  ; Reject
  ; Reject
  ; Reject
  ; Reject
  ; Reject
  ; Accept "HTTP/1.2 200 \"OK\" []"
  ; Reject
  ; Reject
  ; Reject
  ; Reject
  ]

let parse kind input =
  let module G = Ache.Ache_private.Parse.Grammar in
  match kind with
  | Request ->
    (match G.request input with
    | Ok r -> Accept (summary (request_to_string r))
    | Error _ -> Reject)
  | Response ->
    (match G.response input with
    | Ok r -> Accept (summary (response_to_string r))
    | Error _ -> Reject)

let test_corpus () =
  Alcotest.(check int) "corpus size" (List.length all) (List.length expected);
  List.iter2
    (fun (kind, input) expected ->
       Alcotest.check outcome (summary input) expected (parse kind input))
    all
    expected

let test_chunk_size () =
  List.iter
    (fun (line, expected) ->
       Alcotest.(check (result int64 string))
         line
         expected
         (Ache.Ache_private.Parse.Grammar.chunk_size line))
    [ "0000000000000005\r\n", Ok 5L
    ; String.make 20 '0' ^ "\r\n", Ok 0L
    ; "0000000000000000010\r\n", Ok 16L
    ; "fffffffffffffff\r\n", Ok 0xfffffffffffffffL
    ; ( "10000000000000000\r\n"
      , Error {|chunk-size too long: "10000000000000000"|} )
    ; "7fffffffffffffff\r\n", Error {|chunk-size too long: "7fffffffffffffff"|}
    ; "ffffffffffffffff\r\n", Error {|chunk-size too long: "ffffffffffffffff"|}
    ; "\r\n", Error "expected chunk-size, found '\\r'"
    ]

let tests =
  [ "head corpus", `Quick, test_corpus; "chunk-size", `Quick, test_chunk_size ]
