type kind =
  | Request
  | Response

let browser_get =
  "GET /wp-content/uploads/2010/03/hello-kitty-darth-vader-pink.jpg HTTP/1.1\r\n\
   Host: www.kittyhell.com\r\n\
   User-Agent: Mozilla/5.0 (Macintosh; U; Intel Mac OS X 10.6; ja-JP-mac; \
   rv:1.9.2.3) Gecko/20100401 Firefox/3.6.3 Pathtraq/0.9\r\n\
   Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8\r\n\
   Accept-Language: ja,en-us;q=0.7,en;q=0.3\r\n\
   Accept-Encoding: gzip,deflate\r\n\
   Accept-Charset: Shift_JIS,utf-8;q=0.7,*;q=0.7\r\n\
   Keep-Alive: 115\r\n\
   Connection: keep-alive\r\n\
   \r\n"

let requests =
  [ "GET / HTTP/1.1\r\n\r\n"
  ; "some-other-verb / HTTP/1.1\r\n\r\n"
  ; "GET / HTTP/1.1\r\nLink: /path/to/some/website\r\n\r\n"
  ; browser_get
  ; "POST /upload HTTP/1.1\r\nHost: a\r\nContent-Length: 5\r\n\r\n"
  ; "GET / HTTP/1.0\r\n\r\n"
  ; "GET http://example.com/x?y=1 HTTP/1.1\r\nHost: example.com\r\n\r\n"
  ; "CONNECT example.com:443 HTTP/1.1\r\nHost: example.com:443\r\n\r\n"
  ; "OPTIONS * HTTP/1.1\r\n\r\n"
  ; "GET / HTTP/1.1\r\nX: a  \r\n\r\n"
  ; "GET / HTTP/1.1\r\nX:\r\n\r\n"
  ; "GET / HTTP/1.1\r\nX:   \r\n\r\n"
  ; "GET / HTTP/1.1\r\nX:\tval\r\n\r\n"
  ; "GET / HTTP/1.1\r\nX: 1\r\nX: 2\r\nY: 3\r\n\r\n"
  ; "GET / HTTP/1.1\r\nX: a:b\r\n\r\n"
  ; "GET / HTTP/1.1\r\n: empty-name\r\n\r\n"
  ; "get / HTTP/1.1\r\n\r\n"
  ; "GET / HTTP/2.0\r\n\r\n"
  ; "\r\nGET / HTTP/1.1\r\n\r\n"
  ; "GET / HTTP/1.1\r\nHost: a\n\r\n"
  ; "GET / HTTP/1.1\r\n"
  ; "GET / HTTP/1.22\r\n\r\n"
  ; "GET / HTTP/1.1\r\nLink : /path/to/some/website\r\n\r\n"
  ; "  X\r\n\r\n"
  ; "GET / HTTP/1\r\n\r\n"
  ; "GET / http/1.1\r\n\r\n"
  ; "GET / HTTP/1.1x\r\n\r\n"
  ; "GET / HTTP/a.b\r\n\r\n"
  ; "GET  / HTTP/1.1\r\n\r\n"
  ; "GET\t/ HTTP/1.1\r\n\r\n"
  ; "GET /\tHTTP/1.1\r\n\r\n"
  ; "GET /a b HTTP/1.1\r\n\r\n"
  ; "GET / HTTP/1.1 \r\n\r\n"
  ; "GET / HTTP/1.1\r\r\n\r\n"
  ; "GET / HTTP/1.1\r\nHost example.com\r\n\r\n"
  ; "GET / HTTP/1.1\r\nNoColon\r\n\r\n"
  ; "GET / HTTP/1.1\r\nHo st: x\r\n\r\n"
  ; "GET / HTTP/1.1\r\nHost\t: x\r\n\r\n"
  ; "GET / HTTP/1.1\n\n"
  ; "GET / HTTP/1.1\r\nHost: a\r\n\n"
  ; "GET / HTTP/1.1\r\nX: a\r\n b\r\n\r\n"
  ; "GET / HTTP/1.1\r\nX: a\r\n\tb\r\n\r\n"
  ; String.make 1000 'M' ^ " / HTTP/1.1\r\n\r\n"
  ; "GET / HTTP/1.1\r\nX: " ^ String.make 6000 'v' ^ "\r\n\r\n"
  ; "GET / HTTP/1.1\r\n" ^ String.make 300 'n' ^ ": v\r\n\r\n"
  ; "GET /" ^ String.make 4000 'p' ^ " HTTP/1.1\r\n\r\n"
  ; "GET /\xc3\xa9 HTTP/1.1\r\n\r\n"
  ; "GET / HTTP/1.1\r\nX: caf\xc3\xa9\r\n\r\n"
  ; "GET / HTTP/1.1\r\nX\xff: y\r\n\r\n"
  ; "GET / HTTP/1.1\r\nX: a\000b\r\n\r\n"
  ; "GET / HTTP/1.1\r\nHost: a\r\n"
  ; "GET / HTTP/1.1\r\nHost: a"
  ; "GET / HTTP/1.1\r\nHost: a\r\n\r"
  ; ""
  ; "\r\n"
  ; "GET / HTTP/1.1\r\n\r\nextra"
  ; "GET / \r\n\r\n"
  ; " / HTTP/1.1\r\n\r\n"
  ; "GET  HTTP/1.1\r\n\r\n"
  ; "GE(T / HTTP/1.1\r\n\r\n"
  ; "GET /\127 HTTP/1.1\r\n\r\n"
  ; "GET / HTTP/1.2\r\n\r\n"
  ; "GET / HTTP/1.1\r\nX-Invalid[]: t\r\n\r\n"
  ; "GET / HTTP/1.1\r\nX: a\nb\r\n\r\n"
  ; "GET / HTTP/1.1\r\nX: a\007b\r\n\r\n"
  ; "GET / HTTP/1.1\r\nX: a\127b\r\n\r\n"
  ; "GET / HTTP/1.1\r\nX:\t a b \t\r\n\r\n"
  ]

let responses =
  [ "HTTP/1.1 200 OK\r\n\r\n"
  ; "HTTP/1.1 404 Not Found\r\nContent-Length: 0\r\n\r\n"
  ; "HTTP/1.0 200 OK\r\n\r\n"
  ; "HTTP/1.1 200 \r\n\r\n"
  ; "HTTP/1.1 999 Custom\r\n\r\n"
  ; "HTTP/1.1 600 x\r\n\r\n"
  ; "HTTP/1.1 100 Continue\r\n\r\n"
  ; "HTTP/1.1 200 OK\r\nTransfer-Encoding: chunked\r\n\r\n"
  ; "HTTP/1.1 101 Switching Protocols\r\n\
     Upgrade: websocket\r\n\
     Connection: Upgrade\r\n\
     \r\n"
  ; "HTTP/1.1 200 OK\r\nSet-Cookie: a=1\r\nSet-Cookie: b=2\r\n\r\n"
  ; "HTTP/1.1 304 Not Modified\r\nETag: \"x\"\r\n\r\n"
  ; "HTTP/1.1 200 \xe2\x9c\x93\r\n\r\n"
  ; "HTTP/1.1 200 OK\tfoo\r\n\r\n"
  ; "HTTP/1.1 200 " ^ String.make 5000 'r' ^ "\r\n\r\n"
  ; "HTTP/1.1 200\r\n\r\n"
  ; "HTTP/1.1 OK\r\n\r\n"
  ; "HTTP/1.1 999999937377999999999200\r\n\r\n"
  ; "HTTP/1.1  200 OK\r\n\r\n"
  ; "HTTP/1.1 200 OK\n\n"
  ; "http/1.1 200 OK\r\n\r\n"
  ; "HTTP/1.1 200 OK\r\nX : y\r\n\r\n"
  ; "HTTP/1.1 200 OK\r\nX: y\r\n z\r\n\r\n"
  ; "HTTP/1.1 200 OK\r\nX: y\r\n"
  ; "HTTP/1.1 200 OK\r\n\r\nbody"
  ; "HTTP/1.1 2000 OK\r\n\r\n"
  ; "HTTP/1.1 200OK\r\n\r\n"
  ; "HTTP/1.1 99 x\r\n\r\n"
  ; "HTTP/1.1 099 x\r\n\r\n"
  ; "HTTP/1.1 1 x\r\n\r\n"
  ; "HTTP/1.1 20a OK\r\n\r\n"
  ; "HTTP/1.2 200 OK\r\n\r\n"
  ; "HTTP/2.0 200 OK\r\n\r\n"
  ; "HTTP/1.1 200 OK\r\nX(y): 1\r\n\r\n"
  ; "HTTP/1.1 200 O\nK\r\n\r\n"
  ; "HTTP/1.1 200 OK\r\nX: a\000b\r\n\r\n"
  ]

let all =
  List.map (fun s -> Request, s) requests
  @ List.map (fun s -> Response, s) responses

let headers_to_string headers =
  String.concat
    ";"
    (List.map
       (fun (name, value) -> Printf.sprintf "%S:%S" name value)
       (Ache.Headers.to_list headers))

let request_to_string { Ache.Request.meth; target; version; headers } =
  Printf.sprintf
    "%S %S %s [%s]"
    (Ache.Method.to_string meth)
    target
    (Ache.Version.to_string version)
    (headers_to_string headers)

let response_to_string { Ache.Response.version; status; reason; headers } =
  Printf.sprintf
    "%s %d %S [%s]"
    (Ache.Version.to_string version)
    (Ache.Status.to_code status)
    reason
    (headers_to_string headers)

let summary value =
  if String.length value <= 120
  then value
  else "md5:" ^ Digest.to_hex (Digest.string value)
