(lang dune 3.24)

(name fromage)

(generate_opam_files true)

(maintainers "Emile Trotignon <emile.trotignon@lexifi.com>")

(authors "Emile Trotignon <emile.trotignon@lexifi.com>")

; The tests: [with-test] are the dependencies of [@runtest] (what [opam
; install --with-test] runs), [with-dev-setup] those of [@runtest-full]. The
; fuzzing needs menhir and ocamlgrammarfuzzer, the coverage of ocst
; bisect_ppx, whose report needs cmdliner before 2.0 (see the .opam.template
; files for the versions pinned).

(package
 (name ocst)
 (synopsis "A lossless concrete syntax tree of OCaml, with its parser")
 (description
  "A parser of OCaml which keeps every token, comment and blank line of the source, and prints it back unchanged. Its syntax tree converts to the compiler's Parsetree.")
 (depends
  (ocaml
   (and
    (>= 5.5)
    (< 5.6)))
  (menhir :with-dev-setup)
  (ocamlgrammarfuzzer :with-dev-setup)
  (bisect_ppx :with-dev-setup)
  (cmdliner
   (and
    :with-dev-setup
    (< 2.0)))))

(package
 (name fromage)
 (synopsis "A formatter of OCaml, built on ocst")
 (description
  "A pretty printer of the concrete syntax trees of ocst, and the formatter of OCaml files which uses it, keeping the tokens, the comments and the blank lines of the source.")
 (depends
  (ocaml
   (and
    (>= 5.5)
    (< 5.6)))
  (ocst
   (= :version))
  pretty_expressive
  (menhir :with-test)
  (ocamlgrammarfuzzer :with-test)))
