let () =
  Alcotest.run
    "rfc9112"
    [ "s2-s5-message", Test_9112_message.tests
    ; "s6-s8-body", Test_9112_body.tests
    ; "s9-s11-connection", Test_9112_connection.tests
    ]
