type t =
  { ca : X509.Certificate.t
  ; certificate : X509.Certificate.t
  ; key : X509.Private_key.t
  }

let name cn =
  [ X509.Distinguished_name.(
      Relative_distinguished_name.singleton (CN (Common_name.v cn)))
  ]

let get_ok = function
  | Ok x -> x
  | Error _ -> failwith "Ache_test_certs: cannot create a certificate"

let generate ?(days = 1) () =
  Mirage_crypto_rng_unix.use_default ();
  let now = Ptime_clock.now () in
  let valid_from = Option.get (Ptime.sub_span now (Ptime.Span.of_int_s 3600)) in
  let valid_until =
    Option.get (Ptime.add_span now (Ptime.Span.of_int_s (days * 86400)))
  in
  let ca_name = name "ache test CA" in
  let ca_key = X509.Private_key.generate `P256 in
  let ca =
    let extensions =
      X509.Extension.(
        singleton Basic_constraints (true, (true, None))
        |> add Key_usage (true, [ `Key_cert_sign; `CRL_sign ]))
    in
    X509.Signing_request.sign
      (get_ok (X509.Signing_request.create ca_name ca_key))
      ~valid_from
      ~valid_until
      ~extensions
      ca_key
      ca_name
    |> get_ok
  in
  let key = X509.Private_key.generate `P256 in
  let certificate =
    let alt_names =
      X509.General_name.(
        singleton DNS [ "localhost" ] |> add IP [ "\127\000\000\001" ])
    in
    let extensions =
      X509.Extension.(
        singleton Basic_constraints (true, (false, None))
        |> add Key_usage (true, [ `Digital_signature ])
        |> add Ext_key_usage (false, [ `Server_auth ])
        |> add Subject_alt_name (false, alt_names))
    in
    X509.Signing_request.sign
      (get_ok (X509.Signing_request.create (name "localhost") key))
      ~valid_from
      ~valid_until
      ~extensions
      ca_key
      ca_name
    |> get_ok
  in
  { ca; certificate; key }

let server_config t =
  get_ok
    (Tls.Config.server ~certificates:(`Single ([ t.certificate ], t.key)) ())

let authenticator ca =
  X509.Authenticator.chain_of_trust
    ~time:(fun () -> Some (Ptime_clock.now ()))
    [ ca ]

let client_config t =
  get_ok (Tls.Config.client ~authenticator:(authenticator t.ca) ())

let write_file path contents =
  Out_channel.with_open_bin path (fun oc -> output_string oc contents)

let write t ~dir =
  write_file (Filename.concat dir "ca.pem") (X509.Certificate.encode_pem t.ca);
  write_file
    (Filename.concat dir "server.pem")
    (X509.Certificate.encode_pem t.certificate);
  write_file
    (Filename.concat dir "server.key")
    (X509.Private_key.encode_pem t.key)
