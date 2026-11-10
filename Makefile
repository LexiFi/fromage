# Formats the OCaml sources with fromage: dune fmt runs the executable
# ocamlformat of the package ocamlfauxrmat, built in this workspace, which
# dune prefers to an installed one. The sources left as they are are listed
# in test/.ocamlformat-ignore.

.PHONY: fmt
fmt:
	dune fmt
