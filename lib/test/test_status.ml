open Ache

let registered =
  [ 100, "Continue"
  ; 101, "Switching Protocols"
  ; 102, "Processing"
  ; 103, "Early Hints"
  ; 200, "OK"
  ; 201, "Created"
  ; 202, "Accepted"
  ; 203, "Non-Authoritative Information"
  ; 204, "No Content"
  ; 205, "Reset Content"
  ; 206, "Partial Content"
  ; 207, "Multi-Status"
  ; 208, "Already Reported"
  ; 226, "IM Used"
  ; 300, "Multiple Choices"
  ; 301, "Moved Permanently"
  ; 302, "Found"
  ; 303, "See Other"
  ; 304, "Not Modified"
  ; 305, "Use Proxy"
  ; 307, "Temporary Redirect"
  ; 308, "Permanent Redirect"
  ; 400, "Bad Request"
  ; 401, "Unauthorized"
  ; 402, "Payment Required"
  ; 403, "Forbidden"
  ; 404, "Not Found"
  ; 405, "Method Not Allowed"
  ; 406, "Not Acceptable"
  ; 407, "Proxy Authentication Required"
  ; 408, "Request Timeout"
  ; 409, "Conflict"
  ; 410, "Gone"
  ; 411, "Length Required"
  ; 412, "Precondition Failed"
  ; 413, "Content Too Large"
  ; 414, "URI Too Long"
  ; 415, "Unsupported Media Type"
  ; 416, "Range Not Satisfiable"
  ; 417, "Expectation Failed"
  ; 418, "I'm a teapot"
  ; 420, "Enhance Your Calm"
  ; 421, "Misdirected Request"
  ; 422, "Unprocessable Content"
  ; 423, "Locked"
  ; 424, "Failed Dependency"
  ; 425, "Too Early"
  ; 426, "Upgrade Required"
  ; 428, "Precondition Required"
  ; 429, "Too Many Requests"
  ; 431, "Request Header Fields Too Large"
  ; 451, "Unavailable For Legal Reasons"
  ; 500, "Internal Server Error"
  ; 501, "Not Implemented"
  ; 502, "Bad Gateway"
  ; 503, "Service Unavailable"
  ; 504, "Gateway Timeout"
  ; 505, "HTTP Version Not Supported"
  ; 506, "Variant Also Negotiates"
  ; 507, "Insufficient Storage"
  ; 508, "Loop Detected"
  ; 511, "Network Authentication Required"
  ]

let classes (s : Status.t) =
  List.filter_map
    (fun (c, p) -> if p s then Some c else None)
    [ 1, Status.is_informational
    ; 2, Status.is_successful
    ; 3, Status.is_redirection
    ; 4, Status.is_client_error
    ; 5, Status.is_server_error
    ]

let test_every_code () =
  List.iter
    (fun n ->
       let label = string_of_int n in
       let s = Status.of_code n in
       Alcotest.(check int) (label ^ " to_code") n (Status.to_code s);
       Alcotest.(check string) (label ^ " to_string") label (Status.to_string s);
       let expected_class = if n >= 600 then 5 else n / 100 in
       Alcotest.(check (list int))
         (label ^ " class")
         [ expected_class ]
         (classes s);
       Alcotest.(check bool)
         (label ^ " is_error")
         (expected_class >= 4)
         (Status.is_error s);
       match List.assoc_opt n registered, s with
       | Some phrase, (#Status.standard as standard) ->
         Alcotest.(check string)
           (label ^ " reason")
           phrase
           (Status.default_reason_phrase standard);
         Alcotest.(check string)
           (label ^ " status line")
           (Printf.sprintf "HTTP/1.1 %d %s\r\n\r\n" n phrase)
           (Helpers.response_to_string (Response.create s))
       | Some _, `Code _ -> Alcotest.failf "%d is registered, got `Code" n
       | None, `Code m -> Alcotest.(check int) (label ^ " `Code") n m
       | None, #Status.standard ->
         Alcotest.failf "%d is not registered, got a named status" n)
    (List.init 500 (fun i -> i + 100) @ [ 600; 999 ])

let test_named () =
  let status = Alcotest.of_pp Status.pp_hum in
  Alcotest.check status "308" `Permanent_redirect (Status.of_code 308);
  Alcotest.check status "421" `Misdirected_request (Status.of_code 421);
  Alcotest.check status "422" `Unprocessable_content (Status.of_code 422);
  Alcotest.check status "413" `Content_too_large (Status.of_code 413);
  Alcotest.check status "103" `Early_hints (Status.of_code 103);
  Alcotest.check status "425" `Too_early (Status.of_code 425);
  Alcotest.check
    status
    "451"
    `Unavailable_for_legal_reasons
    (Status.of_code 451)

let tests =
  [ "every code 100..599, 600, 999", `Quick, test_every_code
  ; "named RFC 9110 codes", `Quick, test_named
  ]
