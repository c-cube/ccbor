(lang dune 2.9)
(name ccbor)

(license MIT)
(source (github c-cube/ccbor))

(package
  (name ccbor)
  (synopsis "Fast CBOR codec")
  (depends
    dune
    (ocaml (>= 4.08))
    (containers (and (>= 3.0) :with-test))))
