(lang dune 3.10)

(name codeforces_ocaml)

(generate_opam_files true)

(source
 (github GiorgosAthanasopoulos/codeforces_ocaml))

(authors "Giorgos Athanasopoulos")

(maintainers "Giorgos Athanasopoulos")

(license LICENSE)

(documentation https://github.com/giorgosathanasopoulos/codeforces_ocaml/tree/master/_build/default/_doc/_html/index.html)

(package
 (name codeforces_ocaml)
 (synopsis "Solutions to codeforces problems")
 (description "Solutions to codeforces problems")
 (depends ocaml dune)
 (tags
  (codeforces programming competitive problems problem_solving algorithms)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
