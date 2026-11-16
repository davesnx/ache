let () =
  Alcotest.run
    "ache unit tests"
    [ "bigstring", Test_bigstring.tests
    ; "version", Test_version.tests
    ; "method", Test_method.tests
    ; "iovec", Test_iovec.tests
    ; "headers", Test_headers.tests
    ; "request", Test_request.tests
    ; "response", Test_response.tests
    ; "status", Test_status.tests
    ; "framing", Test_framing.tests
    ; "grammar", Test_grammar.tests
    ; "oracle", Test_oracle.tests
    ; "parse", Test_parse.tests
    ; "client connection", Test_client_connection.tests
    ; "client", Test_client.tests
    ; "server connection", Test_server_connection.tests
    ; "request syntax", Test_request_syntax.tests
    ; ( "bodies"
      , [ "no body expected to complete was cut", `Quick, Helpers.no_body_cut ]
      )
    ]
