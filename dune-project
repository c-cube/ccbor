(lang dune 2.9)
(name ccbor)
(generate_opam_files true)

(license MIT)
(source (github c-cube/ccbor))

(package
  (name ccbor)
  (synopsis "Fast CBOR codec")
  (depends
    dune
    (ocaml (>= 4.08))
    (containers (and (>= 3.0) :with-test))
    (qcheck-core (and (>= 0.14) :with-test))))
