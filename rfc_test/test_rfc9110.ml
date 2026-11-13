let () =
  Alcotest.run
    "rfc9110"
    [ "s5-fields", Test_s5_fields.tests
    ; "s6-message", Test_s6_message.tests
    ; "s7-routing", Test_s7_routing.tests
    ; "s8-representation", Test_s8_representation.tests
    ; "s9-methods", Test_s9_methods.tests
    ; "s10-context", Test_s10_context.tests
    ; "s15-status", Test_s15_status.tests
    ]
