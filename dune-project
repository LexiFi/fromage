(lang dune 3.24)

(name fromage)

(generate_opam_files true)

; dune fmt formats the OCaml sources with the executable ocamlformat of
; ocamlfromage (see the Makefile), not the dune files.

(formatting
 (enabled_for ocaml))

(maintainers "Emile Trotignon <emile.trotignon@lexifi.com>")

(authors "Emile Trotignon <emile.trotignon@lexifi.com>")

; The tests: [with-test] are the dependencies of [@runtest] (what [opam
; install --with-test] runs), [with-dev-setup] those of [@runtest-full]. The
; executables parse their command line with cmdliner, the fuzzing needs menhir
; and ocamlgrammarfuzzer (pinned in the .opam.template files), the coverage of
; ocst ppx_windtrap.

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
  (cmdliner
   (and
    :with-test
    (>= 2.0)))
  (menhir :with-dev-setup)
  (ocamlgrammarfuzzer :with-dev-setup)
  (ppx_windtrap
   (and
    :with-dev-setup
    (>= 0.2)))))

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
  (cmdliner
   (>= 2.0))
  (menhir :with-test)
  (ocamlgrammarfuzzer :with-test)))

(package
 (name ocamlfromage)
 (synopsis "fromage as ocamlformat, for dune fmt")
 (description
  "An executable named ocamlformat which formats with fromage, with the command line of ocamlformat which dune fmt and the editors use. Of the options of the .ocamlformat files, it honors margin and disable, with .ocamlformat-ignore and .ocamlformat-enable, and ignores the others. Unlike ocamlformat, it formats the files of a project without a .ocamlformat.")
 ; Both install an executable named ocamlformat.
 (conflicts ocamlformat)
 (depends
  (ocaml
   (and
    (>= 5.5)
    (< 5.6)))
  (ocst
   (= :version))
  (fromage
   (= :version))
  (cmdliner
   (>= 2.0))))
