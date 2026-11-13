# RFC 9112 known failures

Tests wrapped in `known_failure ~ticket` in `rfc_test/test_9112_*.ml`. Each one passes today
because the library fails the requirement, and fails (with a message to remove the marker)
once the library meets it. Tickets are in
`/home/me/workplace/davesnx/ache/.workplace/plans/ache-v1/` (`NN-*.md`); `new` means no
ticket covers the defect yet (causes below each table).

"Observed" comes from a real run (`_build/default/rfc_test/_build/_tests/rfc9112/*.output`,
lines `KNOWN FAILURE ... observed:` and the `ASSERT` lines before them).

## Counts

| Ticket | Known failures |
|---|---|
| 01 | 7 |
| 02 | 2 |
| 03 | 2 |
| 04 | 3 |
| 05 | 13 |
| 07 | 3 |
| 17 | 4 |
| 21 | 1 |
| new | 14 |
| Total | 49 |

## §2–§5 message syntax (`test_9112_message.ml`)

| Test | RFC section | Observed | Expected | Ticket |
|---|---|---|---|---|
| `§2.2 server MAY: bare LF ending a field line gets 400 and close` | §2.2 | `GET / HTTP/1.1\r\nHost: a\nX: 1\r\n\r\n` gets 200; the handler sees Host `a\nX: 1` | 400, handler not called, connection closed | 01 |
| `§2.2 client MAY: bare LF line endings are a malformed response` | §2.2 | `HTTP/1.1 200 OK\nContent-Length: 0\n\n`: no error and no response; the reason-phrase swallows the LFs and the client waits for CR | `Malformed_response` | 01 |
| `§2.2 both MUST NOT: no bare CR from the app in a protocol element` | §2.2 | server writes `X-A: a\rb` from the app value as given | no bare CR on the wire (value, reason, target) | 17 |
| `§2.2 server MUST: bare CR in the method or target gets 400` | §2.2 | `G\rET / HTTP/1.1` gets 200 (method `G\rET` reaches the handler) | 400, handler not called, closed | 01 |
| `§2.2 server MUST: bare CR in a field name gets 400` | §2.2 | `X\rY: 1` gets 200 | 400, handler not called, closed | 02 |
| `§2.2 client MUST: bare CR in a field name is a malformed response` | §2.2 | `X\rY: 1` in a response: no error reported | `Malformed_response` | 02 |
| `§2.2 server SHOULD: one empty line before the request-line is ignored` | §2.2 | `\r\nGET / HTTP/1.1` gets 200, but the handler sees method `\r\nGET` (`Other`) | handler sees `GET` | new |
| `§2.2 server SHOULD: an empty line between pipelined requests is ignored` | §2.2 | second request reaches the handler with method `\r\nGET` | methods `[GET; GET]` | new |
| `§2.2 server SHOULD: two empty lines before the request-line are ignored or get 400` | §2.2 | handler sees method `\r\n\r\nGET` | method `GET`, or 400 and close | new |
| `§2.2 both MUST NOT: no whitespace from the app before the first field` | §2.2 | server writes `\r\n X-A: 1` from the app field name ` X-A` | no field line that starts with SP or HTAB | 17 |
| `§2.2 server SHOULD: 400 and close for octets that are not HTTP` | §2.2 | TLS ClientHello bytes `\x16\x03\x01...`: nothing written, connection waits for more input | 400 and close | 01 |
| `§3 server SHOULD: 400 for a missing version, empty method or empty target` | §3, §3.2 | `GET / \r\n\r\n`: nothing written, connection waits | 400 and close for each | 01 |
| `§3 server SHOULD: 501 for a method longer than the head limit` | §3 | 20000-octet method gets 431 | 501 | 03 |
| `§3 server MUST: 414 for a request-target longer than the head limit` | §3 | 2001-octet target with `max_header_size = 1024` gets 431 | 414 | 03 |
| `§3.2 server SHOULD: 400 for asterisk-form or authority-form outside OPTIONS and CONNECT` | §3.2, §3.2.3, §3.2.4 | `GET * HTTP/1.1` gets 200 (first input of the list; the test stops there) | 400 and close | new |
| `§3.2 server MUST: 400 for an HTTP/1.1 request without Host` | §3.2 | `GET / HTTP/1.1\r\n\r\n` gets 200 | 400 and close | 04 |
| `§3.2 server MUST: 400 for more than one Host field line` | §3.2 | `Host: a\r\nHost: a` gets 200 | 400 and close (1.1 and 1.0, any name case) | 04 |
| `§3.2 server MUST: 400 for a Host field with an invalid value` | §3.2 | `Host: a b` gets 200 | 400 and close | 04 |
| `§5.2 client MUST: obs-fold in a response is replaced with SP` | §5.2 | `X-A: a\r\n  b`: `Malformed_response`, no response | one response, X-A = `a` SP+ `b` | new |
| `§5.2 both MUST NOT: no obs-fold from the app in a field value` | §5.2 | server writes `X-A: a\r\n b` from the app value | no obs-fold on the wire | 17 |

### Causes of the `new` failures

**Leading empty lines become part of the method** (§2.2 SHOULD).
`lib/parse.ml:94` `Grammar.request_head` reads the method with `take_while (not is_space)`, and nothing skips CRLF before it, so `\r\nGET` becomes `` `Other "\r\nGET" `` and is dispatched. Fix: skip at least one CRLF before the request-line (or with ticket 01's `1*tchar` method, at least do not dispatch).

**Request-target form is not checked against the method** (§3.2 SHOULD, §3.2.3/§3.2.4 DEF).
`lib/parse.ml:96` reads the target as any non-space bytes and `lib/parse.ml:102` builds the request without checking that `*` is used only with OPTIONS and authority-form only with CONNECT. Ticket 18 covers the CONNECT port only.

**Client rejects obs-fold instead of replacing it** (§5.2 MUST for a user agent).
`lib/parse.ml:76-91` `Grammar.header`/`headers`: a line that starts with SP or HTAB parses as an empty field name and `Parseff.char ':'` fails, so `lib/client_connection.ml:231` reports `Malformed_response`. The same grammar serves requests, where 400 is allowed; the response path must unfold instead.

## §6–§8 message body (`test_9112_body.ml`)

| Test | RFC section | Observed | Expected | Ticket |
|---|---|---|---|---|
| `§6.1 server SHOULD: unknown coding before chunked gets 400 or 501` | §6.1, §6.3 rule 4 | `Transfer-Encoding: gzip` line then `Transfer-Encoding: chunked` line: `[200; 200]`; the body is decoded as chunked, gzip is ignored, `/next` is dispatched (`gzip, chunked` on one line already gives 400) | 400 or 501, handler not called, connection closed | 05 |
| `§6.1 server MUST: close after a request with both Content-Length and Transfer-Encoding` | §6.1, §6.3 rule 3 | `Content-Length: 3` + `Transfer-Encoding: chunked`: `[200; 200]`, the pipelined `/next` is dispatched, connection open | 400, or 200 by Transfer-Encoding; then close; `/next` never dispatched | 05 |
| `§6.1 server MUST: HTTP/1.0 request with Transfer-Encoding closes the connection` | §6.1 | HTTP/1.0 + `Connection: keep-alive` + `Transfer-Encoding: chunked`: `[200; 200]`, `/next` dispatched | 400 or one 200, then close | 05 |
| `§6.3 server MUST: 400 then close for a Content-Length that is not 1*DIGIT` | §6.3 rule 5 | `Content-Length: 0x3`: `[200; 200]` (read as 3) | 400, handler not called, close | 05 |
| `§6.3 client MUST: connection not reused after a response with both Content-Length and Transfer-Encoding` | §6.3 rule 3 | body `abc` read by chunked (correct), then `connection closed` expected `true`, received `false` | connection closed after the response | 05 |
| `§6.1 client MUST: HTTP/1.0 response with Transfer-Encoding closes the connection` | §6.1 | HTTP/1.0 + `connection: keep-alive` + chunked: `closed` expected `true`, received `false` | connection closed after the response | 05 |
| `§6.1 server MUST: response with a non-chunked final coding closes the connection` | §6.1 | app streams with `transfer-encoding: gzip`: `gzip: connection closed` expected `true`, received `false` | close after the close-delimited response | 07 |
| `§6.1 server MUST NOT: streamed response to an HTTP/1.0 keep-alive request is close-delimited` | §6.1, §6.3 rule 8 | no Transfer-Encoding (correct), then `keep-alive: connection closed` expected `true`, received `false` | close after the close-delimited response | 07 |
| `§6.3 server SHOULD: streamed response without a length is chunked or closes the connection` | §6.3 | streamed 200 with no length to HTTP/1.1: no chunking, `close-delimited: connection closed` expected `true`, received `false` | chunked framing, or close after the response | 07 |
| `§8 server MAY: EOF in the head closes without calling the handler` | §8 | `Failure("cannot write to closed writer")` raised out of the connection | no exception; no response or 400; handler not called; closed | 21 |
| `§8 server MUST: an incomplete request body is not reported as complete` | §8, §6.3 rule 6 | `Content-Length: 5` + `ab` + EOF: `ab: body not complete` expected `false`, received `true` (the app's `on_eof` runs) | the app is not told the truncated body is complete | new |

### Cause of the `new` failure: incomplete request body ends with on_eof

On a body parse failure or an EOF inside the body, `Server_connection.next_read_operation` calls `set_error_and_handle`, and `Reqd.report_error` (`lib/reqd.ml:241`; also `lib/reqd.ml:244` and `Server_connection.shutdown_reader` at `lib/server_connection.ml:103`) calls `Body.Reader.close` on the request body. `Body.Reader.close` (`lib/body.ml:99-102`) calls the app's `on_eof`, the same signal as a complete body.
Effect: an app that answers in `on_eof` (as `Rfc.Server.echo` does) calls `respond_with_string` while the error response is open, and `lib/reqd.ml:119-122` raises `Failure "invalid state, currently handling error"` out of `next_read_operation`. Seen with `Server.run ~eof:true (post [ cl "5" ] "ab")` and the echo handler. The tests use a handler that only reads the body, so they can check the 400 and the close.
The client has the same `close` on error (`lib/respd.ml:77-80`), but it calls the error handler first, so the error records the message as incomplete before `on_eof`. The client tests accept this.

## §9 and §11 connections and security (`test_9112_connection.ml`)

| Test | RFC section | Observed | Expected | Ticket |
|---|---|---|---|---|
| `§9.6 server SHOULD: close option in the response to a request with Connection: close` | §9.6#1, §9.6#4 | connection closed; response `Connection` field: `None` | `Connection: close` in the final response | new |
| `§9.6 server SHOULD: close option in the response to an HTTP/1.0 request` | §9.6#1 | connection closed; response `Connection` field: `None` | `Connection: close` in the final response | new |
| `§9.6 server SHOULD: close option in a 400 response` | §9.6#1 | 400, handler not called, reader closed; `Connection` field: `None` | `Connection: close` in the 400 | new |
| `§9.3.2 client SHOULD NOT: no request pipelined after a POST before its response` | §9.3.2#5 | written before the POST response: `POST /1 ...\r\n\r\nabcGET /2 HTTP/1.1\r\nHost: a\r\n\r\n` | only the POST written until its response | new |
| `§9.2 client MUST NOT: data with no outstanding request is not a response` | §9.2#2, §6.3#15 | `Client.feed` raises `File "lib/parse.ml", line 370, characters 6-12: Assertion failed` | no exception; data not a response; client closed | new |
| `§6.3 client MUST NOT: a second response after the last one is not delivered` | §6.3#15, §9.2#2 | `Client.feed` raises `File "lib/parse.ml", line 380, characters 16-22: Assertion failed` | first response delivered, extra response dropped, client closed | new |
| `§9.3.2 client SHOULD: unanswered pipelined request reported when the connection closes` | §9.6#8 (with §9.3.2 retry) | client closed, second exchange: no response and no error (`errors = []`) | an error reported on the unanswered request so the app can retry | new |
| `§9.5 client SHOULD: stop sending the body after an error response with Connection: close` | §9.5#5, §9.5#6 | after `413` with `Connection: close`, the rest of the body `"def"` is written | nothing more written; client closed | new |
| `§11.1 server DEF: app field value cannot split the response` | §11.1#D1 | `the output is not one response: "HTTP/1.1 200 OK\r\nX-A: x\r\nContent-Length: 0\r\n\r\nHTTP/1.1 200 OK\r\nContent-Length: 4\r\n\r\nevil\r\ncontent-length: 2\r\n\r\nok"` | exactly one response on the wire | 17 |
| `§11.2 server DEF: no smuggling: Content-Length then Transfer-Encoding: chunked` | §11.2#D1, §6.1 | `/smuggled reached the request handler; requests: ["POST" /; "GET" /smuggled]` | 400 and close, or one 200 then close | 05 |
| `§11.2 server DEF: no smuggling: Transfer-Encoding: chunked then Content-Length` | §11.2#D1, §6.1 | `/smuggled reached the request handler; requests: ["POST" /; "GET" /smuggled]` | 400 and close, or one 200 then close | 05 |
| `§11.2 server DEF: no smuggling: chunked body with a short Content-Length that exposes a request` | §11.2#D1, §6.1 | `observed statuses [200], targets [/], closed false` | 400 and close, or one 200 then close | 05 |
| `§11.2 server DEF: no smuggling: Content-Length +3` | §11.2#D1, RFC 9110 §8.6 | `/smuggled reached the request handler; requests: ["POST" /; "GET" /smuggled]` | 400, handler not called, close | 05 |
| `§11.2 server DEF: no smuggling: Content-Length 0x3` | §11.2#D1, RFC 9110 §8.6 | `/smuggled reached the request handler; requests: ["POST" /; "GET" /smuggled]` | 400, handler not called, close | 05 |
| `§11.2 server DEF: no smuggling: Content-Length 1_0` | §11.2#D1, RFC 9110 §8.6 | `/smuggled reached the request handler; requests: ["POST" /; "GET" /smuggled]` | 400, handler not called, close | 05 |
| `§11.2 server DEF: no smuggling: Transfer-Encoding: chunked in HTTP/1.0 with keep-alive` | §11.2#D1, §6.1 | `/smuggled reached the request handler; requests: ["POST" /; "GET" /smuggled]` | 400 and close, or one 200 then close | 05 |
| `§11.2 server DEF: no smuggling: Transfer_Encoding with an underscore` | §11.2#D1, §3 | `/smuggled reached the request handler; requests: ["POST" /; "0\r\n\r\nGET" /smuggled]` | body length 0, then the line `0` is a bad request line: 400 and close (or 400 at once) | 01 |
| `§11.2 server DEF: no smuggling: bare LF before Transfer-Encoding in a field value` | §11.2#D1, §2.2, §3 | `/smuggled reached the request handler; requests: ["POST" /; "23\r\nGET" /smuggled]` | 400 for the bare LF, or the chunk line rejected as a request line | 01 |

### Causes of the `new` failures

- Server never adds `Connection: close` when it closes. `lib/reqd.ml:127` (and :146, :174) writes the app's response head as given, and the error path `lib/server_connection.ml:189` builds the 400 head from the error handler's headers only. Non-persistence is decided in `Reqd.persistent` and `Server_connection._final_write_operation_for` but never reflected in the head.
- Client pipelines after non-idempotent methods. `lib/client_connection.ml:153-175` (`maybe_pipeline_queued_requests`) writes the next queued request as soon as the previous one is fully written (`Uninitialized, Complete` at :166), with no check of the previous request's method.
- Unsolicited response data crashes the client. The response reader's `on_head` asserts a queued request (`lib/parse.ml:370`) and an `Awaiting_response` one (`lib/parse.ml:380`); bytes with no outstanding request raise `Assert_failure` out of `Client_connection.read` instead of a parse error and close.
- Unanswered pipelined requests are dropped silently. On a non-persistent response, `lib/client_connection.ml:207-212` (`_final_read_operation_for` → `shutdown_reader`, :111) closes only the head exchange; queued `Respd`s that were already written never get `report_error`, so the app cannot retry them.
- Client keeps sending the request body after an early `Connection: close` response. `lib/respd.ml:33` only clears `persistent`; `lib/client_connection.ml:264-265` keeps flushing the request body while its output state is `Ready`, and nothing closes the writer until the body is complete.
