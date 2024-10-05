(lang dune 3.14)
(using menhir 3.0)
(name obsidian)
(version 0.0.1)
(generate_opam_files true)
(license BSD 3-Clause)
(authors "Codezz-ops")
(maintainers "Codezz-ops")

(package
 (name obsidian)
 (synopsis "A Programming Language")
 (description "A Programming Language")
 (depends ocaml dune menhir llvm ppx_deriving odoc))