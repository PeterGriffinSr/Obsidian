(lang dune 3.14)
(using menhir 3.0)

(name obsidian)
(version 0.1.0)
(generate_opam_files true)
(license "BSD-3-Clause")
(authors "Codezz-ops")
(maintainers "Codezz-ops")

(package
 (name obsidian)
 (synopsis "A Programming Language")
 (description "A Programming Language")
 (depends
  (ocaml (>= 4.14.2))
  "dune"
  "menhir"
  (llvm (= 14.0.6))
  "ppx_deriving"
  "odoc"))
