let () =
  let dir = Sys.argv.(1) in
  Ache_test_certs.write (Ache_test_certs.generate ~days:30 ()) ~dir;
  Printf.printf
    "Wrote %s, %s and %s: a test CA and a certificate for localhost and \
     127.0.0.1, valid for 30 days.\n"
    (Filename.concat dir "ca.pem")
    (Filename.concat dir "server.pem")
    (Filename.concat dir "server.key")
