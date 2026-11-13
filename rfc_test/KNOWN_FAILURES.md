# Known failures

Tests wrapped in `known_failure ~ticket` in `rfc_test/`. Each one passes today because
the library fails the requirement, and fails (with a message to remove the marker) once
the library meets it. Tickets are in
`/home/me/workplace/davesnx/ache/.workplace/plans/ache-v1/` (`NN-*.md`).

"Observed" comes from the test logs of a real run
(`_build/default/rfc_test/_build/_tests/rfc9110/*.output`, lines
`KNOWN FAILURE ... observed:`).

Tests that send `100 Continue` use `respond_with_string` with `` `Continue `` (the only
API today). Ticket 08 plans a dedicated function and to reject 1xx in
`respond_with_string` with `Invalid_argument`. The helper `Server.send_continue` turns
that `Invalid_argument` into `Api_changed`, and `known_failure` fails on `Api_changed`, so
these tests fail loudly and must be rewritten for the new function when ticket 08 lands.

## Counts

| Ticket | Known failures |
|---|---|
| 01 | 5 |
| 02 | 10 |
| 05 | 12 |
| 07 | 11 |
| 08 | 7 |
| 09 | 4 |
| 17 | 4 |
| 18 | 1 |
| 19 | 1 |
| Total | 55 |

## Failures

| Test | RFC section | Observed | Expected | Ticket |
|---|---|---|---|---|
| §2.2 server MUST NOT: no invalid field-name from the app on the wire | §2.2 | the server writes the application's field name verbatim: `Bad Name: v` appears in the response head | reject the field (exception or no output) instead of writing a name that is not a token | 17 |
| §2.2 server MUST NOT: no CR, LF or NUL from the app in a field value | §2.2 | the value `a\r\nInjected: 1` is written verbatim, so the response head gets an extra field line `Injected: 1` | reject the value or replace CR, LF and NUL so no extra field line is written | 17 |
| §2.2 client MUST NOT: no invalid field-name from the app on the wire | §2.2 | the client writes `Bad Name: v` in the request head | reject the field instead of writing a name that is not a token | 17 |
| §2.2 client MUST NOT: no CR, LF or NUL from the app in a field value | §2.2 | the value `a\r\nInjected: 1` is written verbatim, so the request head gets an extra field line `Injected: 1` | reject the value or replace CR, LF and NUL so no extra field line is written | 17 |
| §2.3 server SHOULD: 400 for field lines that do not match the grammar | §2.3 | for `NoColon\r\n\r\n` the server writes nothing and keeps reading: the name parser reads past CRLF looking for a colon | 400, request handler not called, connection closed | 02 |
| §2.3 client SHOULD: Malformed_response for field lines off the grammar | §2.3 | for `NoColon\r\nContent-Length: 0\r\n` the client delivers a response (the field name absorbs `NoColon\r\nContent-Length`) | `Malformed_response`, no response delivered | 02 |
| §5.1 server DEF: 400 for a field-name that contains a delimiter | §5.1 | `X(y: v` is accepted: 200, handler called | 400, handler not called | 02 |
| §5.1 server DEF: 400 for a field-name with a CTL, DEL or non-ASCII byte | §5.1 | `X\001y: v` is accepted: 200, handler called | 400, handler not called | 02 |
| §5.1 server DEF: 400 for an empty field-name | §5.1 | `: v` is accepted: 200, handler called | 400, handler not called | 02 |
| §5.1 client DEF: Malformed_response for an invalid field-name | §5.1 | `: v` in a response is accepted: one response delivered | `Malformed_response`, no response delivered | 02 |
| §5.5 server MUST: NUL in a value gives 400 or is replaced with SP | §5.5 | handler sees `a\000b` | 400, or handler sees `a b` | 02 |
| §5.5 server MUST: bare LF in a value gives 400 or is replaced with SP | §5.5 | handler sees `a\nb` | 400, or handler sees `a b` | 02 |
| §5.5 client MUST: NUL in a value is rejected or replaced with SP | §5.5 | response handler sees `a\000b` | `Malformed_response`, or value `a b` | 02 |
| §5.5 client MUST: bare LF in a value is rejected or replaced with SP | §5.5 | response handler sees `a\nb` | `Malformed_response`, or value `a b` | 02 |
| §2.5 server DEF: HTTP/2.0 request is not processed as HTTP/1.x | §2.5 | request accepted: request handler called (1 call) | 400 or 505, handler not called | 01 |
| §2.5 server DEF: HTTP/0.9 request is not processed as HTTP/1.x | §2.5 | request accepted: request handler called (1 call) | 400 or 505, handler not called | 01 |
| §2.5 server DEF: HTTP/9.9 request is not processed as HTTP/1.x | §2.5 | request accepted: request handler called (1 call) | 400 or 505, handler not called | 01 |
| §2.5 client DEF: HTTP/2.0 response is not processed as HTTP/1.x | §2.5 | response handler called with status 200, no error | error handler called, no response delivered | 01 |
| §6.4.1 server DEF: no body bytes in a 204 from respond_with_string | §6.4.1 | wrote `HTTP/1.1 204 No Content\r\n\r\nhello` before the next response | head only, next response right after it | 07 |
| §6.4.1 server DEF: no body bytes in a 304 from respond_with_string | §6.4.1 | wrote `HTTP/1.1 304 Not Modified\r\n\r\nhello` before the next response | head only, next response right after it | 07 |
| §6.4.1 server DEF: no body bytes in a streamed 204 | §6.4.1 | streamed `hello` written after the 204 head | head only; writes dropped | 07 |
| §6.4.1 server DEF: no body bytes in a streamed 304 | §6.4.1 | streamed `hello` written after the 304 head | head only; writes dropped | 07 |
| §6.4.1 server DEF: no body bytes after a 100 head | §6.4.1 | wrote `HTTP/1.1 100 Continue\r\n\r\nhello` | no body bytes after a 1xx head (or the call is rejected) | 08 |
| §6.4.1 client DEF: 100 response ends after the head | §6.4.1 | exception `Assertion failed` at lib/parse.ml:380 when the final 200 arrives | 100 skipped; response handler gets the 200 with body `ok` | 08 |
| §8.6 server DEF: 400 for Content-Length 0x3 | §8.6 | read as 3: handler called, 200 | 400, handler not called, reader closed | 05 |
| §8.6 server DEF: 400 for Content-Length +3 | §8.6 | read as 3: handler called, 200 | 400, handler not called, reader closed | 05 |
| §8.6 server DEF: 400 for Content-Length 1_0 | §8.6 | read as 10: handler called, waits for more body, no response | 400, handler not called, reader closed | 05 |
| §8.6 server DEF: 400 for Content-Length 0o3 | §8.6 | read as 3: handler called, 200 | 400, handler not called, reader closed | 05 |
| §8.6 client DEF: error for Content-Length 0x2 | §8.6 | read as 2: response handler called with 200 | error handler called, no response delivered | 05 |
| §8.6 client DEF: error for Content-Length +2 | §8.6 | read as 2: response handler called with 200 | error handler called, no response delivered | 05 |
| §8.6 server SHOULD: respond_with_string adds Content-Length | §8.6 | head has no Content-Length; body close-delimited on a kept-open connection | `content-length: 5` added for body `hello` | 07 |
| §7.6.1 server DEF: connection option close matched case-insensitively | §7.6.1 | `Connection: Close` and `Connection: CLOSE`: 200, then the connection stays open (reader `Read`) | 200, then reader and writer closed | 05 |
| §7.6.1 server DEF: close found in a list of connection options | §7.6.1 | `keep-alive, close`, `close, upgrade`, `foo ,close`: connection stays open | connection closed after the response | 05 |
| §7.6.1 server DEF: close found among empty list elements | §7.6.1 | `, close` and `close ,`: connection stays open | connection closed after the response | 05 |
| §7.6.1 server DEF: close in the first of two Connection field lines | §7.6.1 | `Connection: close` then `Connection: upgrade`: connection stays open (only the last line is read) | connection closed after the response | 05 |
| §7.6.1 server DEF: HTTP/1.0 Keep-Alive option matched case-insensitively | §7.6.1 | HTTP/1.0 request with `Connection: Keep-Alive`: connection closed after the first response, second request not served | both requests served, connection open | 05 |
| §7.6.1 client DEF: response connection option close matched case-insensitively and in a list | §7.6.1 | response with `Connection: Close`: client connection stays open | client connection closed after the response | 05 |
| §9.1 server DEF: method with a non-tchar byte gets 400 | §9.1 | `GE(T / HTTP/1.1` reaches the handler as a method and gets 200 | 400, handler not called, reader closed | 01 |
| §9.3.2 server MUST NOT: no content in a HEAD respond_with_string | §9.3.2 | `hello` written after the head of the HEAD response | only the response head | 07 |
| §9.3.2 server MUST NOT: no content in a streamed HEAD response | §9.3.2 | streamed `hello` written after the head of the HEAD response | only the response head | 07 |
| §9.3.2 server MUST NOT: no content in an error response to HEAD | §9.3.2 | 400 to a HEAD with `Content-Length: abc` carries the default error body `400` | 400 head with no content | 07 |
| §9.3.2 server MUST NOT: pipelined GET response follows the HEAD head | §9.3.2 | second status line reads `helloHTTP/1.1 200 OK`: the HEAD body bytes come before it | second response starts right after the HEAD head | 07 |
| §9.3.6 server MUST: 400 for CONNECT with an empty or invalid port | §9.3.6 | `CONNECT example.com: HTTP/1.1` reaches the handler; no 400 | 400, handler not called | 18 |
| §10.1.1 server MUST: final response follows the 100 | §10.1.1 | `Failure("ache.Reqd.respond_with_string: response already complete")` when the app sends the 200 after the 100 | `100 Continue` head, then the 200 with the body | 08 |
| §15 client MUST: unrecognized 199 is interim before the final | §15 | `Assertion failed` at lib/parse.ml:380 when the 200 arrives after the 199 | 199 skipped as interim; response handler gets the 200 and body `ok` | 08 |
| §15 client SHOULD: received status 600 is classed as a server error | §15 | `Status.is_server_error` of the received 600 is false | true (invalid code processed as 5xx) | 19 |
| §15.4 both DEF: 308 Permanent Redirect | §15.4.9 | reason phrase `Non-standard status code` | `Permanent Redirect` | 09 |
| §15.5 both DEF: 413 Content Too Large | §15.5.14 | reason phrase `Payload Too Large` | `Content Too Large` | 09 |
| §15.5 both DEF: 421 Misdirected Request | §15.5.20 | reason phrase `Non-standard status code` | `Misdirected Request` | 09 |
| §15.5 both DEF: 422 Unprocessable Content | §15.5.21 | reason phrase `Non-standard status code` | `Unprocessable Content` | 09 |
| §15.2 server MUST NOT: no 1xx response to an HTTP/1.0 client | §15.2 | the app's 100 is accepted for the HTTP/1.0 request; the following 200 raises `Failure("ache.Reqd.respond_with_string: response already complete")` | only the final 200 | 08 |
| §15.2 client MUST: 100 Continue before the final response is parsed | §15.2 | `Assertion failed` at lib/parse.ml:380 when the 200 arrives | response handler gets the 200 and body `ok` | 08 |
| §15.2 client MUST: 103 and 100 before the final response are parsed | §15.2 | `Assertion failed` at lib/parse.ml:380 | response handler gets the 200 and body `ok` | 08 |
| §15.3.5 server DEF: a 204 response ends after its header section | §15.3.5 | `hello` written after the 204 head | only the 204 head | 07 |
| §15.4.5 server DEF: a 304 response ends after its header section | §15.4.5 | streamed `hello` written after the 304 head | only the 304 head | 07 |

## Causes of the failures first marked `new` (now tickets 17-19)

- Ticket 17, §2.2 generator (4 tests): `lib/serialize.ml` `write_headers` writes each name
  and value with no check. `Request.create`, `Response.create` and `Headers` accept any
  string, so a value with CR LF injects a field line, on the server
  (`Reqd.respond_with_*`) and on the client (`Client_connection.request`).
- Ticket 18, §9.3.6 CONNECT port: nothing checks the authority-form `host:port` of a
  CONNECT request-target (RFC 9112 §3.2.3), so an empty or non-numeric port reaches the
  handler.
- Ticket 19, §15 status 600 class: `Status.is_server_error` matches only `` `Code ``
  500..599 while `Status.of_code` accepts 100..999, so a received 600..999 is in no class.
