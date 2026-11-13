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
  "ache is an HTTP/1.1 client and server library. It parses and serializes messages and keeps the state of a connection, without doing I/O itself; the ache-eio and ache-lwt packages run it on sockets. ache is a fork of httpun and httpaf with a parseff parser, strict RFC 9110 and RFC 9112 parsing, and limits on head size.")
 (documentation "https://ocaml.org/p/ache/latest/doc/index.html")
 (tags
  (http http-server http-client web))
 (depends
  (ocaml
   (>= 5.3))
  bigstringaf
  (parseff
   (and
    (>= 0.5.0)
    (< 0.6.0)))
  (alcotest :with-test)
  (faraday :with-test)
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
  bigstringaf
  cstruct
  eio
  (eio_main :with-test)
  (alcotest :with-test)))

(package
 (name ache-lwt)
 (synopsis "Lwt runtime for ache")
 (description
  "Runs ache HTTP/1.1 servers and clients on Lwt_unix sockets. A functor accepts any other socket type, such as a TLS socket.")
 (documentation "https://ocaml.org/p/ache-lwt/latest/doc/index.html")
 (tags
  (http http-server http-client web lwt))
 (depends
  (ocaml
   (>= 5.3))
  (ache
   (= :version))
  bigstringaf
  lwt
  (alcotest :with-test)))
