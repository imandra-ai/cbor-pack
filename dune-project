(lang dune 2.8)
(name cbor-pack)
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
 (synopsis "Library for CBOR-pack: a sharing aware serialization on top of CBOR")
 (depends (ocaml (>= 4.08)) cbor)
 (tags
  (cbor ocaml)))

(package
 (name cbor-pack-ppx)
 (synopsis "Ppx deriver for cbor-pack")
 (depends (ocaml (>= 4.08)) cbor (cbor-pack (= :version))
          ppxlib (ppx_deriving :with-test) (mdx :with-test))
 (tags
  (cbor ocaml ppx)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
