open Ache
open Request
open Helpers

let body_length = Alcotest.of_pp Request.Body_length.pp_hum

let check =
  let alco = Alcotest.result (Alcotest.of_pp pp_hum) Alcotest.string in
  fun message ~expect input ->
    let actual = Ache_private.Parse.Grammar.request input in
    Alcotest.check alco message expect actual

let test_parse_valid () =
  check
    "valid GET without headers"
    ~expect:(Ok (Request.create `GET "/"))
    "GET / HTTP/1.1\r\n\r\n";
  check
    "valid non-standard method without headers"
    ~expect:(Ok (Request.create (`Other "some-other-verb") "/"))
    "some-other-verb / HTTP/1.1\r\n\r\n";
  check
    "valid GET with headers"
    ~expect:
      (Ok
         (Request.create
            ~headers:(Headers.of_list [ "Link", "/path/to/some/website" ])
            `GET
            "/"))
    "GET / HTTP/1.1\r\nLink: /path/to/some/website\r\n\r\n"

let test_parse_invalid_errors () =
  check
    "doesn't end"
    ~expect:(Error "unexpected end of input")
    "GET / HTTP/1.1\r\n";
  check
    "invalid version"
    ~expect:(Error "unexpected '2' in request-line")
    "GET / HTTP/1.22\r\n\r\n";
  check
    "malformed header"
    ~expect:(Error "unexpected ' ' in field line")
    "GET / HTTP/1.1\r\nLink : /path/to/some/website\r\n\r\n"

let test_body_length () =
  let check message request ~expect =
    let actual = Request.body_length request in
    Alcotest.check body_length message expect actual
  in
  let req method_ headers = Request.create method_ ~headers "/" in
  check "no headers" ~expect:(`Fixed 0L) (req `GET Headers.empty);
  check
    "single fixed"
    ~expect:(`Fixed 10L)
    (req `GET Headers.(encoding_fixed 10));
  check
    "negative fixed"
    ~expect:(`Error `Bad_request)
    (req `GET Headers.(encoding_fixed (-10)));
  check
    "multiple fixed"
    ~expect:(`Error `Bad_request)
    (req `GET Headers.(encoding_fixed 10 @ encoding_fixed 20));
  check "chunked" ~expect:`Chunked (req `GET Headers.encoding_chunked);
  check
    "chunked multiple times"
    ~expect:`Chunked
    (req `GET Headers.(encoding_chunked @ encoding_chunked));
  let encoding_gzip = Headers.of_list [ "transfer-encoding", "gzip" ] in
  check
    "non-chunked transfer-encoding"
    ~expect:(`Error `Bad_request)
    (req `GET encoding_gzip);
  check
    "chunked after non-chunked"
    ~expect:`Chunked
    (req `GET Headers.(encoding_gzip @ encoding_chunked));
  check
    "chunked before non-chunked"
    ~expect:(`Error `Bad_request)
    (req `GET Headers.(encoding_chunked @ encoding_gzip));
  check
    "chunked case-insensitive"
    ~expect:`Chunked
    (req `GET Headers.(of_list [ "transfer-encoding", "CHUNKED" ]))

let check_rejected name ~expect request =
  let f = Encoder.create 0x100 in
  Alcotest.check_raises name (Invalid_argument expect) (fun () ->
    Ache_private.Serialize.write_request f request);
  Alcotest.(check int) (name ^ ": nothing written") 0 (Encoder.pending_bytes f)

let test_serialize_rejects_invalid_head () =
  List.iter
    (fun target ->
       check_rejected
         (Printf.sprintf "target %S" target)
         ~expect:(Printf.sprintf "ache: invalid request target %S" target)
         (Request.create `GET target))
    [ "/a b"; "/a\r\nInjected: 1"; "/a\n"; "/a\r"; "/a\000" ];
  List.iter
    (fun meth ->
       check_rejected
         (Printf.sprintf "method %S" meth)
         ~expect:(Printf.sprintf "ache: invalid method %S" meth)
         (Request.create (`Other meth) "/"))
    [ "GET /evil HTTP/1.1\r\n"; "A B"; "A\nB"; "A\rB"; "" ];
  check_rejected
    "field name"
    ~expect:{|ache: invalid field name "X\r\nInjected"|}
    (Request.create
       ~headers:(Headers.of_list [ "Host", "a"; "X\r\nInjected", "v" ])
       `GET
       "/");
  check_rejected
    "field value"
    ~expect:{|ache: invalid value for field "X-A"|}
    (Request.create
       ~headers:(Headers.of_list [ "Host", "a"; "X-A", "a\r\nInjected: 1" ])
       `GET
       "/");
  Alcotest.(check string)
    "valid head is written"
    "PURGE /a?b=c%20d HTTP/1.1\r\nHost: a\r\nX-A: 1\t2\r\n\r\n"
    (request_to_string
       (Request.create
          ~headers:(Headers.of_list [ "Host", "a"; "X-A", "1\t2" ])
          (`Other "PURGE")
          "/a?b=c%20d"))

let tests =
  [ "parse valid", `Quick, test_parse_valid
  ; "parse invalid errors", `Quick, test_parse_invalid_errors
  ; "body length", `Quick, test_body_length
  ; ( "serialize rejects an invalid head"
    , `Quick
    , test_serialize_rejects_invalid_head )
  ]
