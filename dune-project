(lang dune 3.6)

(name act)

(using mdx 0.3)

(generate_opam_files true)

; (source (github username/reponame))

(authors "Henry Heffan")

(maintainers "Henry Heffan")

(license GPL-3.0-or-later)

; (documentation https://url/to/documentation)

(package
 (name act)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune act_ir core)
; (tags (topics "to describe" your project))
  )

(package
 (name act_ir)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune bignum core zarith)
; (tags (topics "to describe" your project))
)

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
