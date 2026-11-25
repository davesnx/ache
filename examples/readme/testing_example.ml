open Ache

let request_handler reqd =
  let request = Request_descriptor.request reqd in
  Request_descriptor.respond_with_string
    reqd
    (Response.create `OK)
    ("Hello, " ^ request.target)

let () =
  let output =
    Testing.serve request_handler "GET /a HTTP/1.1\r\nHost: a\r\n\r\n"
  in
  assert (output = "HTTP/1.1 200 OK\r\ncontent-length: 9\r\n\r\nHello, /a");
  let response, body =
    Testing.serve_request
      request_handler
      (Request.create ~headers:(Headers.of_list [ "host", "a" ]) `GET "/b")
  in
  assert (response.status = `OK);
  assert (body = "Hello, /b")
