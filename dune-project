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
 (depends
  (ocaml
   (>= 5.3))
  angstrom
  bigstringaf
  (alcotest :with-test)
  (faraday :with-test)))

(package
 (name ache-eio)
 (synopsis "Eio runtime for ache")
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
 (depends
  (ocaml
   (>= 5.3))
  (ache
   (= :version))
  bigstringaf
  lwt
  (alcotest :with-test)))
