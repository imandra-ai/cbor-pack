(lang dune 2.9)
(name cbor-pack)
(version 0.1)
(using mdx 0.1)
(generate_opam_files true)
(source
 (github imandra-ai/cbor-pack))
(authors "Simon Cruanes")
(maintainers "Imandra team")
(license MIT)

;(documentation https://url/to/documentation)

(package
 (name cbor-pack)
 (synopsis "Library for CBOR-pack: a sharing-aware serialization format on top of CBOR")
 (depends (ocaml (>= 4.08)) (cbor (>= 0.5)))
 (tags
  (cbor ocaml)))

(package
 (name cbor-pack-ppx)
 (synopsis "Ppx deriver for cbor-pack")
 (depends (ocaml (>= 4.08)) cbor (cbor-pack (= :version))
          (ppxlib (>= 0.25.0))
          (ppx_deriving :with-test)
          (mdx (and :with-test (>= 1.9))))
 (tags
  (cbor ocaml ppx)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
