# RFC 9110 known failures

No known failure remains. On 2026-11-13, every test in
`rfc_test/test_s*.ml` passes with no `known_failure` marker; `COVERAGE.md` lists the
requirements each test covers.

## How the marker works

`Rfc.known_failure ~ticket name f` (`rfc_test/rfc.ml`) registers a test for a requirement
that the library does not meet yet:

- While `f` raises (an assertion fails), the test passes and prints
  `KNOWN FAILURE (ticket NN) <name>` and the exception to the test log
  (`_build/default/rfc_test/_build/_tests/<suite>/*.output`).
- When `f` returns, the test fails with "known failure now passes: remove the
  known_failure marker". Check that it passes for the right reason, then replace
  `known_failure ~ticket:"NN"` with `test`.
- If `f` raises `Rfc.Api_changed`, the test fails and asks for a rewrite against the new API.

Use `~ticket:"new"` when no ticket covers the defect. When you add a marker, add a row here
(test, RFC section, observed behaviour from a real run, expected behaviour, ticket) and the
ticket label to the test's row in `COVERAGE.md`.
