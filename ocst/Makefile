# Formats the sources of OCST with its own formatter (see HACKING.md): all
# its OCaml files, committed or not, except the inputs of the tests whose
# layout is tested or which come from elsewhere, and the vendored copies. A
# file a.ml with an a.ml.expected is left as it is, and its formatted version
# is written to a.ml.expected.
FORMATTER = ../_build/default/ocst/pretty/ocst_format.exe

.PHONY: fmt
fmt:
	dune build ./pretty/ocst_format.exe
	find . \( -path ./_build -o -path ./vendor -o -path ./test/errors/cases -o -path ./test/doc_comments/cases \
	  -o -path ./test/ocamlformat_tests \) -prune -o \( -name '*.ml' -o -name '*.mli' \) -print \
	  | while read -r file; do \
	      if [ -e "$$file.expected" ]; then \
	        { $(FORMATTER) "$$file" > "$$file.expected.new" && mv "$$file.expected.new" "$$file.expected"; } \
	          || rm -f "$$file.expected.new"; \
	      else echo "$$file"; fi; \
	    done \
	  | xargs -r $(FORMATTER) -i
