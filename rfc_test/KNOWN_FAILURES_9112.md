# RFC 9112 known failures

Tests wrapped in `known_failure ~ticket` in `rfc_test/test_9112_*.ml`. Each one passes today
because the library fails the requirement, and fails (with a message to remove the marker)
once the library meets it. Tickets are in
`/home/me/workplace/davesnx/ache/.workplace/plans/ache-v1/` (`NN-*.md`).

"Observed" comes from a real run on v1 at c7102ad plus these test edits
(`_build/default/rfc_test/_build/_tests/rfc9112/*.output`, lines `KNOWN FAILURE ... observed:`
and the `ASSERT` lines before them).

## Counts

| Ticket | Known failures |
|---|---|
| 07 | 3 |
| 21 | 1 |
| 22 | 1 |
| 23 | 3 |
| 24 | 1 |
| 25 | 1 |
| 26 | 3 |
| 27 | 7 |
| Total | 20 |

## §2–§5 message syntax (`test_9112_message.ml`)

| Test | RFC section | Observed | Expected | Ticket |
|---|---|---|---|---|
| `§3 server SHOULD: 501 for a method longer than the head limit` | §3 | 20000-octet method gets 431 | 501 | 23 |
| `§3 server MUST: 414 for a request-target longer than the head limit` | §3 | 2001-octet target with `max_header_size = 1024` gets 431 | 414 | 23 |
| `§3.2 server SHOULD: 400 for asterisk-form or authority-form outside OPTIONS and CONNECT` | §3.2, §3.2.3, §3.2.4 | `GET * HTTP/1.1` gets 200 (first input of the list; the test stops there) | 400 and close | 23 |
| `§5.2 client MUST: obs-fold in a response is replaced with SP` | §5.2 | `X-A: a\r\n  b`: `Malformed_response`, no response | one response, X-A = `a` SP+ `b` | 24 |

## §6–§8 message body (`test_9112_body.ml`)

| Test | RFC section | Observed | Expected | Ticket |
|---|---|---|---|---|
| `§6.3 client MUST: connection not reused after a response with both Content-Length and Transfer-Encoding` | §6.3 rule 3 | body `abc` read by chunked (correct), then `connection closed` expected `true`, received `false` | connection closed after the response | 27 |
| `§6.1 client MUST: HTTP/1.0 response with Transfer-Encoding closes the connection` | §6.1 | HTTP/1.0 + `connection: keep-alive` + chunked: `closed` expected `true`, received `false` | connection closed after the response | 27 |
| `§6.1 server MUST: response with a non-chunked final coding closes the connection` | §6.1 | app streams with `transfer-encoding: gzip`: `gzip: connection closed` expected `true`, received `false` | close after the close-delimited response | 07 |
| `§6.1 server MUST NOT: streamed response to an HTTP/1.0 keep-alive request is close-delimited` | §6.1, §6.3 rule 8 | no Transfer-Encoding (correct), then `keep-alive: connection closed` expected `true`, received `false` | close after the close-delimited response | 07 |
| `§6.3 server SHOULD: streamed response without a length is chunked or closes the connection` | §6.3 | streamed 200 with no length to HTTP/1.1: no chunking, `close-delimited: connection closed` expected `true`, received `false` | chunked framing, or close after the response | 07 |
| `§8 server MAY: EOF in the head closes without calling the handler` | §8 | `Failure("cannot write to closed writer")` raised out of the connection | no exception; no response or 400; handler not called; closed | 21 |
| `§8 server MUST: an incomplete request body is not reported as complete` | §8, §6.3 rule 6 | `Content-Length: 5` + `ab` + EOF: `ab: body not complete` expected `false`, received `true` (the app's `on_eof` runs) | the app is not told the truncated body is complete | 25 |

The first two rows are the client side of RFC 9112 §6.1 and §6.3 rule 3; ticket 27 owns them.

## §9 and §11 connections and security (`test_9112_connection.ml`)

| Test | RFC section | Observed | Expected | Ticket |
|---|---|---|---|---|
| `§9.6 server SHOULD: close option in the response to a request with Connection: close` | §9.6#1, §9.6#4 | connection closed; response `Connection` field: `None` | `Connection: close` in the final response | 26 |
| `§9.6 server SHOULD: close option in the response to an HTTP/1.0 request` | §9.6#1 | connection closed; response `Connection` field: `None` | `Connection: close` in the final response | 26 |
| `§9.6 server SHOULD: close option in a 400 response` | §9.6#1 | 400, handler not called, reader closed; `Connection` field: `None` | `Connection: close` in the 400 | 26 |
| `§9.3.2 client SHOULD NOT: no request pipelined after a POST before its response` | §9.3.2#5 | written before the POST response: `POST /1 ...\r\n\r\nabcGET /2 HTTP/1.1\r\nHost: a\r\n\r\n` | only the POST written until its response | 27 |
| `§9.2 client MUST NOT: data with no outstanding request is not a response` | §9.2#2, §6.3#15 | `Client.feed` raises `File "lib/parse.ml", line 370 (now 391): Assertion failed` | no exception; data not a response; client closed | 27 |
| `§6.3 client MUST NOT: a second response after the last one is not delivered` | §6.3#15, §9.2#2 | `Client.feed` raises `File "lib/parse.ml", line 380 (now 401): Assertion failed` | first response delivered, extra response dropped, client closed | 27 |
| `§9.3.2 client SHOULD: unanswered pipelined request reported when the connection closes` | §9.6#8 (with §9.3.2 retry) | client closed, second exchange: no response and no error (`errors = []`) | an error reported on the unanswered request so the app can retry | 22 |
| `§9.5 client SHOULD: stop sending the body after an error response with Connection: close` | §9.5#5, §9.5#6 | after `413` with `Connection: close`, the rest of the body `"def"` is written | nothing more written; client closed | 27 |
| `§6.3 client MAY: CRLF after the last response is discarded` | §6.3#14, §9.2#2 | response `r1` delivered, then the trailing `\r\n` records an error on the completed exchange (`no error on the exchange` expected `false`, received `true`) | the CRLF is discarded with no error (RFC 9112 §9.2: data that is only CRLF can be discarded) | 27 |

This last row passed before `8d73ed5`. Cause: `Grammar.literal "HTTP/"` (`lib/parse.ml`) now
fails on the first byte, where `Parseff.consume` waited for five bytes; the reader then fails
on `\r\n` after a complete response and `Client_connection.set_error_and_handle` reports it to
the completed exchange. Ticket 27 (data with no outstanding request) owns it.
