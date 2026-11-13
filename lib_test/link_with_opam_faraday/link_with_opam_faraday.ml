let () =
  let opam = Faraday.create 0x100 in
  Faraday.write_string opam "opam";
  let opam = Faraday.serialize_to_string opam in
  let vendored = Ache.Ache_private.Faraday.create 0x100 in
  Ache.Ache_private.Serialize.write_request
    vendored
    (Ache.Request.create `GET "/");
  let vendored = Ache.Ache_private.Faraday.serialize_to_string vendored in
  if opam <> "opam" || vendored <> "GET / HTTP/1.1\r\n\r\n" then exit 1
