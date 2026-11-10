# Introduction

This repo contains both a new parser/syntax tree for OCaml called Ocst, and a
new formatter called fromage.

This is something intended to be used and useful, but its also an experiment in
LLM assisted programming. This is a good project to give a lot of freedom to an
LLM because a lot of properties are very easy to test: for the CST we have
checks that no information is lost and that it behaves the same as the upstream
parser, for the formatter we have checks that it does not change the source.

We also use [ocamlgrammarfuzzer](https://github.com/shogan-ai/ocamlgrammarfuzzer).
Without this none of this would be reasonable: fuzzing make sure every corner of
the grammar is tested. Big thanks to its authors.

Because of that, some parts the codebase are meant to be proof read by humans,
others not.

Part that are meant to be read include:

- The test suite
- The type of the syntax tree in Ocst
- The output of the formatter (this means every file, but only looking at
  formatting.)

Part the are not meant to be read by humans at all:

- The parser code for Ocst
- The formatting code for the formatter

This readme is not LLM-generated, its intended as human-to-human communication,
to express the intent of the authors.

During development the parts that are supposed to be human checked might not be,
but no release should be made until thats the case.

# Fromage

Fromage is a new formatter for ocaml. It intends to be opiniated but open
minded: the best formatting possible should be produced. I believe that
formatting are not just a matter of taste, but that some objective advantages
can be discovered and chosen. This means that I intend to document the
formatting choices I make and the reasoning behind them, and I am open to
discussing and improve them. I am also aware that its not always easy, and even
sometime impossible to pick the "best" formatting, but I also believe that
if you are writing a formatter you should at least try to do so.

I want to have as few user options as possible. maximum line length is an option
I want to support, and a few others but generally if there is a better
formatting we should just adopt it instead of making an option.

For now there is only one version, but I also intend the formatting to be
versioned so that we can improve it without forcing other devs to have large
diffs in their codebase.

Formatting choice are documented in `formatting.md`.

## ocamlfauxrmat

An opam package named `ocamlfauxrmat` is provided. It allows for fromage to
pretend its ocamlformat: this package installs a binary called ocamlformat, but
that is actually fromage.

This executable reads `.ocamlformat` files, including  `-enable` and `-disable`
ones, and honors which files should be formatted + the  chosen max line length.
It also accepts a subset of the ocamlformat cli args such as `--impl` and
`--intf`.

Its not possible to install on the same switch as ocamlformat.

When installed you can use `dune fmt` or the OCaml LSP format action, and it
will format your project with fromage.

# Ocst

Ocst is a concrete syntax tree for ocaml. It provides a parser that has been
implemented from scratch in recursive descent.

A concrete syntax tree is similar to an abstract syntax tree, but preserves a
lot more from the source. It can mean that there is a bijection with the
sequence of lexing tokens, but here we take it to mean bijection to the source.

This means all the whitespace and comments are preserved in the CST. This works
by having each CST node store each token, and each token stores the whitespace
and comments that precedes it. For comments and token that are at the end, they
are held by the `EOF` token.

Because we have a recursive descent parser, this means we get to define the type
of token ourselves, and it uses polymorphic variants so that which tokens CST
node can hold is enforced by the type-checker. This gives more confidence for
LLM-assisted programming and also makes the CST types very legible at a glance:

```ocaml
  | Exp_setfield of
      expr * [ `DOT ] tok * lident longident * [ `LESSMINUS ] tok * expr
```
Its quite obvious that this is `e.x <- f`, even without knowing the vocabulary.
Here "set field" is quite transparent, but its less the case for "send" :
```ocaml
 | Exp_send of expr * [ `HASH ] tok * lident
```
Which is a method call. Send is vocabulary from smalltalk, but not everyone
knows that, and even if knowing if you are out of context that this is about OO
it could not come to mind. The `` `HASH `` token make that impossible to miss
provided you know OCaml syntax.