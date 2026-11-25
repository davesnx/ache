(lang dune 3.17)

(name ache)

(generate_opam_files true)

(source
 (github davesnx/ache))

(license BSD-3-Clause)

(authors
 "David Sancho <dsnxmoreno@gmail.com>"
 "Spiros Eliopoulos"
 "Antonio Nuno Monteiro")

(maintainers "David Sancho <dsnxmoreno@gmail.com>")

(package
 (name ache)
 (synopsis "High-performance, memory-efficient HTTP/1.1 for OCaml")
 (description
  "ache is an HTTP/1.1 client and server library. It parses and serializes messages and keeps the state of a connection, without doing I/O itself; the ache-eio, ache-lwt and ache-miou packages run it on sockets. ache is a fork of httpun and httpaf: it rejects requests that RFC 9110 and RFC 9112 do not allow, sets the framing of each response, limits head size, and parses heads with a hand-written parser.")
 (documentation "https://ocaml.org/p/ache/latest/doc/index.html")
 (tags
  (http http-server http-client web))
 (depends
  (ocaml
   (>= 5.3))
  (alcotest :with-test)
  (qcheck-core :with-test)
  (ocamlformat
   (and
    (= 0.29.0)
    :with-dev-setup))))

(package
 (name ache-eio)
 (synopsis "Eio runtime for ache")
 (description
  "Runs ache HTTP/1.1 servers and clients on any Eio two-way flow, such as a TCP socket or a TLS flow.")
 (documentation "https://ocaml.org/p/ache-eio/latest/doc/index.html")
 (tags
  (http http-server http-client web eio))
 (depends
  (ocaml
   (>= 5.3))
  (ache
   (= :version))
  cstruct
  eio
  (eio_main :with-test)
  (alcotest :with-test)))

(package
 (name ache-eio-tls)
 (synopsis "TLS for the Eio runtime of ache")
 (description
  "Runs ache HTTP/1.1 servers and clients over TLS on Eio flows, with ocaml-tls (tls-eio). A client reports a response that ends at the close of the connection without a TLS close_notify as an error.")
 (documentation "https://ocaml.org/p/ache-eio-tls/latest/doc/index.html")
 (tags
  (http http-server http-client web eio tls))
 (depends
  (ocaml
   (>= 5.3))
  (ache
   (= :version))
  (ache-eio
   (= :version))
  domain-name
  eio
  ipaddr
  (tls
   (>= 2.1.5))
  (tls-eio
   (>= 2.1.5))
  (eio_main :with-test)
  (x509 :with-test)
  (ptime :with-test)
  (mirage-crypto-rng :with-test)
  (alcotest :with-test)))

(package
 (name ache-lwt)
 (synopsis "Lwt runtime for ache")
 (description
  "Runs ache HTTP/1.1 servers and clients on Lwt_unix sockets. The Make functor accepts any other socket type, such as a TLS socket. The application must ignore SIGPIPE.")
 (documentation "https://ocaml.org/p/ache-lwt/latest/doc/index.html")
 (tags
  (http http-server http-client web lwt))
 (depends
  (ocaml
   (>= 5.3))
  (ache
   (= :version))
  lwt
  (alcotest :with-test)))

(package
 (name ache-miou)
 (synopsis "Miou runtime for ache")
 (description
  "Runs ache HTTP/1.1 servers and clients on Miou_unix sockets. The Make functor accepts any other socket type, such as a TLS socket. The application must ignore SIGPIPE.")
 (documentation "https://ocaml.org/p/ache-miou/latest/doc/index.html")
 (tags
  (http http-server http-client web miou))
 (depends
  (ocaml
   (>= 5.3))
  (ache
   (= :version))
  (miou
   (>= 0.10.0))
  (alcotest :with-test)))

(package
 (name ache-lwt-tls)
 (synopsis "TLS for the Lwt runtime of ache")
 (description
  "Runs ache HTTP/1.1 servers and clients over TLS on Lwt_unix sockets, with ocaml-tls (tls-lwt). A client reports a response that ends at the close of the connection without a TLS close_notify as an error.")
 (documentation "https://ocaml.org/p/ache-lwt-tls/latest/doc/index.html")
 (tags
  (http http-server http-client web lwt tls))
 (depends
  (ocaml
   (>= 5.3))
  (ache
   (= :version))
  (ache-lwt
   (= :version))
  domain-name
  ipaddr
  lwt
  (tls
   (>= 2.1.5))
  (tls-lwt
   (>= 2.1.5))
  (x509 :with-test)
  (ptime :with-test)
  (mirage-crypto-rng :with-test)
  (alcotest :with-test)))

(package
 (name ache-miou-tls)
 (synopsis "TLS for the Miou runtime of ache")
 (description
  "Runs ache HTTP/1.1 servers and clients over TLS on Miou_unix sockets, with ocaml-tls. A client reports a response that ends at the close of the connection without a TLS close_notify as an error. The application must ignore SIGPIPE.")
 (documentation "https://ocaml.org/p/ache-miou-tls/latest/doc/index.html")
 (tags
  (http http-server http-client web miou tls))
 (depends
  (ocaml
   (>= 5.3))
  (ache
   (= :version))
  (ache-miou
   (= :version))
  domain-name
  ipaddr
  (miou
   (>= 0.10.0))
  (tls
   (>= 2.1.5))
  (tls-miou-unix
   (>= 2.1.5))
  (x509 :with-test)
  (ptime :with-test)
  (mirage-crypto-rng :with-test)
  (alcotest :with-test)))
