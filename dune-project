(lang dune 3.1)

(name interpreter)

(generate_opam_files true)

(source
 (github kimjaebyeog/interpreter_practice))

(authors "Jaebyeog Kim")

(maintainers "Jaebyeog Kim")

; (license LICENSE)

; (documentation https://url/to/documentation)

(package
 (name interpreter)
;  (synopsis "interpreter for a functional language")
;  (description "A longer description")
 (depends ocaml dune)
 (tags
  (ocaml interpreter "functional programming")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
