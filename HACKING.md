# Hacking on ocst

ocst is a lossless concrete syntax tree (CST) for OCaml: every token of the
source, with its leading trivia (whitespace, comments), is kept in the tree, so
printing a CST gives back the exact source.

## Modules

- `cst_lexer.mll`, `token.ml`, `token_stream.ml`, `token_position.ml`: the lexer
  and the tokens, with their trivia.
- `cst.ml`: the CST types. It has no `.mli`, on purpose.
- `parser.ml`: a recursive-descent parser from tokens to the CST.
- `doc_comments.ml`: the doc comments, and the items they document.
- `syntax_error.ml`: the syntax errors, with their locations and messages.
- `printer.ml`: prints a CST back to its tokens or source text.
- `to_ast.ml`: converts a CST to the compiler's `Parsetree`, with the same
  locations, attributes and docstrings as the compiler's parser.
- `pretty/`: a pretty printer for the CST (see "The pretty printer").

The grammar of reference is the compiler's `parsing/parser.mly` (copied in
`test/parser.mly`, from OCaml 5.5.2, the version of `compiler-libs`), not
`parser.ml`. `to_ast.ml` exists to check that ocst agrees with it: the test
suite compares its output with `Parse.implementation`/`Parse.interface`.

## Building and testing

The repository has two opam packages, generated from `dune-project`:
`ocst`, the library (in `ocst/`), and `fromage`, the pretty printer (the
library `fromage`, module `Ocst_pretty`, in `pretty/`) and the formatter
(the executable `fromage`). Each test belongs to one of them: the tests of
the parser, the errors, the doc comments and the coverage to `ocst`, the
round-trips, the fuzzing and the layout to `fromage`. The dependencies of
the tests are `{with-test}` for `@runtest` (`cmdliner`, which parses the
command lines of the executables, and `menhir` and `ocamlgrammarfuzzer`, for
`fromage`), `{with-dev-setup}` for the rest of
`@runtest-full` (also `ppx_windtrap`, for the coverage, which is skipped
without it). `ocamlgrammarfuzzer`, which is not in the opam repository, is
pinned in the `.opam.template` files. The opam
files are checked by `@runtest`: after a change of `dune-project`,
`dune promote` updates them. From the repository root:

```bash
dune build                         # build, without running the tests
dune build @check                  # type-check
dune build @runtest                # run the quick tests (about 6 s)
dune build @runtest-full           # run all the tests (about 50 s)
dune build @runtest --auto-promote # update test/*.expected
```

`runtest` is for each change: the unit tests, the error and doc comment
cases, the formatter (`test/pretty_test.ml`), the layout of the sources, and
a quick fuzzing (the exhaustive sentences without their mutants, and the
random ones). `runtest-full` runs `runtest`, the round-trips on all the
sources of the repository, the fuzzing of all the configurations, and the
coverage: run it before a commit.

Each test stanza has explicit `(modules ...)` and `(libraries ...)`. The
tests are dune rules, run in dune's sandbox: they take the files they read
as arguments, and find the repository with `DUNE_SOURCEROOT`
(`git_root` in `test/roundtrip.ml`).

The main test is `test/codebase_roundtrip.ml`. For every `.ml`/`.mli`
file of the repository, including untracked files in `test/examples` and
`test/ocamlformat_tests`, it checks that:
- the tokens print back to the source, and are those of the compiler's
  lexer, at the same positions (`test/lexer_check.ml`);
- the printed CST is identical to the source;
- the AST built by `to_ast.ml` is equal to the compiler's.

When the ASTs differ, the first failing file is reported in detail: the
first differing top-level item, the path to the first difference and the
locations around it, including differences that Printast does not show
(ghost flags, location stacks). The other failing files get one line each.
Sources that both parsers reject pass. To check some files only, for
debugging:

```bash
dune build ./test/codebase_roundtrip.exe
_build/default/test/codebase_roundtrip.exe FILE...
```

The second test is `test/fuzz_roundtrip.ml`: it compiles `test/parser.mly`
with Menhir, generates sentences from it with ocamlgrammarfuzzer, and checks
each sentence like a file. The fuzzer prints identifiers, labels and
strings of about 15 characters (`names` in `test/fuzz_roundtrip.ml`), so that
some constructs fit on one line and others need several. Its configurations
include `--exhaust`,
which covers all the productions of the grammar; `runtest` runs those of
`--quick`, and `runtest-full` all of them.

Each configuration runs in its own dune rule, the exhaustive ones in 4
parts (`--configuration I --part I/N`), so that they run in parallel; the
outputs of the rules of a group are concatenated and compared with its
`.expected` file. These rules are generated from the configurations of
`fuzz_roundtrip.ml` in `test/fuzz.inc` and `test/coverage/fuzz.inc`
(`fuzz_roundtrip.exe --dune-rules test|coverage`): `runtest` checks that
they are up to date, and `dune promote` updates them after a change of the
configurations. Each check is done once: `runtest-full` reuses the outputs
of the quick rules for the configurations which they share, and checks only
the mutants (`--mutants-only`) of the configurations whose sentences
another rule checks. The fuzzer also runs once for each entry point and
arguments: a rule saves its sentences in `_build/default/test/*.sentences`
(`--generate`), and the rules of all the parts of the configurations with
them, in `test` and `test/coverage`, read them (`--sentences FILE`).

`--timings` prints, on the error output, the time of each configuration and
of each phase of the checks (parsing, formatting, comparisons...); `--help`
lists all the options. For larger runs, from the repository root (the
arguments after `--` are those of ocamlgrammarfuzzer):

```bash
_build/default/test/fuzz_roundtrip.exe --grammar test/parser.mly \
  -- interface --seed 4 --count 100000
```

The fuzzing test also puts a doc comment before every token of each sentence
of the exhaustive mode (`--doc-comments`), with random blank lines, to check
where doc comments are attached; the report gives, for each configuration,
the number of sentences and mutants which both parsers accept, which are the
ones compared in depth (most random sentences are rejected by both).

Random sentences avoid a few productions which the semantic actions of
`parser.mly` reject (`valid` in `test/fuzz_roundtrip.ml`), otherwise most long
sentences are invalid. To see why generated sentences are rejected by the
compiler, grouped by error, with the shortest example of each:

```bash
_build/default/test/fuzz_roundtrip.exe --grammar test/parser.mly --rejections \
  -- implementation --seed 1 --count 2000
```

`runtest-full` also measures the coverage of OCST by its tests (not the deep
fuzzing): `test/coverage` copies the sources of OCST and instruments them
with `ppx_windtrap.coverage`, runs the tests against this copy (each process
writing its own `.coverage` file), and compares the report of
`windtrap coverage` (the coverage of each module, and the source of the
uncovered lines) with `test/coverage/coverage.expected`. The coverage is
100%, and must stay so. For another view of the same files:
`windtrap coverage _build/default/test/coverage`.

- Cover new code with explicit cases: an example of `test/examples` (which
  must be valid: the codebase test checks it), a case of
  `test/errors/cases`, or a unit test (`test/parser_test.ml`,
  `test/random_test.ml`) for the functions which only the API reaches.
- windtrap counts the entry of every block (function body, match arm, `if`
  branch) and the return of every call, except the calls of `raise`,
  `failwith`, `invalid_arg` and `exit`. So the parser and the lexer raise
  their errors with `raise` at the call site (`raise (failure state error)`):
  the return of a function which raises would be uncovered.
- Remove the code which no source can reach, or restructure it (make the last
  case of a match the case which the caller checked). What remains (internal
  errors, the `_ -> None` of selectors after a checked token, consistency
  checks) is marked `[@coverage off]`, with a comment saying why it cannot be
  reached. The refill branches which ocamllex generates are excluded in the
  instrumented copy only (`test/coverage/exclude_refill.ml`).

A deeper fuzzing, too long for `runtest-full`, inserts every token of
`test/random_token.ml` in every sentence of the exhaustive mode (one token
per mutant), checks long random sentences with more mutants, and doc
comments before every token after each kind of separator. Its
configurations are `deep_configurations` in `test/fuzz_roundtrip.ml`, each
split in parts, in the rules of `test/fuzz.inc`:

```bash
dune build @test/fuzz-deep
```

Each failure is printed on the error output as soon as it is found, with a
smaller source failing in the same way (`minimized:`). Dune only shows the
error output of a rule when it ends: to see the failures during the run,
start the executable itself (as above), with the options
of the rules of `test/fuzz.inc` (`--deep NAME`, `--part I/N`, and
`--sentences FILE` with a file built by dune, or `--grammar GRAMMAR`
instead to generate the sentences).

Failures found by the fuzzer are LR artifacts of the grammar as often as
plain bugs: read the productions and precedences of `parser.mly`, check
variants with `ocamlc -stop-after parsing -dsource`, and turn them into
examples (see Workflow).

## Design rules for the CST

- Follow the productions of `parser.mly`. Keep every token, in source order,
  with its most specific token type (`` [ `LPAREN ] tok ``, not any token).
- Make invalid trees untypeable:
  - use `delimited` for pairs of opening and closing tokens;
  - never use a union of openers with a union of closers: write one case per
    kind of bracket, so that `(` can only be closed by `)`;
  - never store the same token twice.
- Nodes are tuples whose components follow the source order. Records are
  only used for the shared helper types (`delimited`,
  `non_empty_separated_list`, `with_attributes`).
- Labels in tuples (`post_attributes:attribute list`) are only used when they
  make the code clearer, for instance to tell two attribute lists apart.
  Tokens never get labels (no `in_:[ `IN ] tok`).
- When a construct appears several times, give it a shared type
  (`'a bar_list`, `'a and_list`, `('d, 'c) arrow`, `'a let_open`...).
- Use `non_empty_separated_list` instead of explicit first and rest
  components, when the first element and the others have the same shape.
- Document constructors and types with `(** *)` doc comments showing an
  example of the syntax, such as `(** [x :: xs] *)`. Doc comments appear in
  LSP hovers, so never turn them into regular comments: when one is
  ambiguous (warning 50), separate it with a blank line.
  - The example goes where the tokens are held directly: on each
    constructor of a variant, and on type abbreviations and records that
    hold tokens (such as `record_field`). A variant type whose constructors
    are documented does not get an example itself, but might get documentation
    that explain the general purpose.
  - When there is an optional part, show the example with and without it:
    `(** [x] or [x : t] *)`. When there are several optional parts, show two
    examples: one without any of them and one with all of them.
  - Lists are denoted with `...` after their first two elements, so that
    the repeated pattern is visible: `(** [x, y, ...] *)`,
    `(** [match e with | p -> e | q -> e | ...] *)`,
    `(** [struct let x = e let y = e ... end] *)`.

## Doc comments

Doc comments (`(** ... *)`, `(**)`) are tokens (`DOCSTRING`), not trivia.
They are in the CST where they document something, as decided by the
compiler (`ocaml/parsing/lexer.mll` and `docstrings.ml`):

- before or after an item of a structure, a signature or an object (the
  slots of `'a documented`), and before `and` in a group (`and_docs`);
- after a constructor, a record or object field, a variant tag or the domain
  of an arrow (info slots);
- as floating text between items (`Text`), or between two items which both
  claim it (`Ambiguous`, which the compiler warns about).

A doc comment which documents nothing becomes a `Warning` trivia of the next
token, as the compiler's warning 50 "unattached documentation comment".

`Doc_comments` replays the rules of the compiler on the boundaries between
tokens: the blank lines around a doc comment tell whether it documents the
token before it, the token after it, or is floating text. The parser, which
skips doc comments in `next` and `peek`, claims them as the semantic actions
of `parser.mly` do (`symbol_docs`, `symbol_info`, `symbol_text`,
`extra_text`, `mark_symbol_docs`), in the lists of items (`items`,
`and_list`) and the info slots (`info`), always before taking the token after
them: `take` turns the doc comments which nothing claimed into warnings.
`to_ast.ml` builds the `ocaml.doc` and `ocaml.text` attributes from the CST,
and emits the warnings with `Location.prerr_warning`.

The round-trip tests compare these warnings with the compiler's.
`test/doc_comments` shows, for each file of `cases`, the AST with its doc
attributes and the warnings: add a case for each new rule.

## Syntax errors

The parser raises `Syntax_error.Error` on invalid sources: never another
exception, nor an internal error (`internal`, raised when a token checked by
the parser itself is missing).

- Every error is declared once, as `syntax_error "id" "message"`, and
  identified by its `id`. An error raised in one place is written there,
  inline; the module `E` of `parser.ml` holds the errors raised in several
  places, and those computed from an argument (`E.operator_rhs`).
- A message names the illegal token, then says what was expected there:
  `fail_at` prefixes the message with "Illegal ')', " (with
  `Token.description`: "Illegal string literal, ", "Illegal identifier 'x', ")
  or "Unexpected end of input, ". When a specific token is expected, say so:
  "Illegal ')', '..' expected."; otherwise say what the construct expects:
  "Illegal ')', an index expects an expression.". Messages do not refer to the
  parser ("structure item"). Lexical errors have complete messages, with the
  illegal text.
- Say precisely what is accepted, in terms of tokens: "a lowercase
  identifier", "a capitalized identifier or a path, as in 'M' or 'M.N'",
  "an identifier, '(', '[' or '{' expected after '.'". Avoid words which the
  illegal token could seem to match, such as "a value" or "a name".
- Every place where the parser can fail gives its own error: the functions
  parsing a syntactic category (`core_type`, `pattern`, `expr`...) take the
  error to report when nothing of this category starts at the next token,
  `expect` takes the error for a missing token, and `delimited` and `closing`
  the error for a missing closing token, which points at the opening one too.
  `token` is only for tokens which the parser has already checked.
- When a construct cannot be followed by the next token (a `function` ending
  with an unreachable case before `;`), `stop` records why, and a later
  failure at this token reports this reason instead.

The errors are tested in `test/errors`: `errors.ml` prints, against
`errors.expected`, the error of OCST for every file of `cases`, with its id,
next to the error of the compiler. A file which OCST or the compiler accepts
is reported. The test lists the errors of `parser.ml` which no file raises,
and the ids declared twice: every error has at least one file, named after
its id (`let-in.ml`). The
fuzzing test also checks mutants of its sentences (a token removed, a
prefix, a random token inserted or replacing another one): OCST rejects
exactly the mutants that the compiler rejects, with a syntax error. The
random tokens come from `test/random_token.ml`, which has tokens of every
kind, and is shared with the lexer test `test/random_test.ml`.

## Layout of the sources

The sources are formatted by OCST's own pretty printer (see below):
`make fmt` runs it on them and on `test/examples` instead of `ocp-indent`,
except on the inputs of the tests (the `cases` of `test/errors` and
`test/doc_comments`, whose layout is what they test, and
`test/ocamlformat_tests`), which are left as they are. `runtest` checks their layout with
`test/layout.ml` (which lexes them with OCST; `cst_lexer.mll`, which neither
can read, follows the same rules by hand). The length of the lines is not
checked: pretty_expressive keeps them within the width of `make fmt` (80)
whenever a layout can.

- a bracket (`(`, `[`, `{`, `[|`...), a `begin` closed by `end`, a `let`
  closed by `in` and a `fun` closed by `->` are closed either on their line,
  or in their column: a bracket which spans several lines starts its line, and
  its closing token starts its line, below it (always: a closing bracket on
  another line than its opening one starts its line). A bracket closed on
  another line with content on its line has that content 2 columns after it
  (`( f`, `{ x = 1;`), so that the next lines of the content can be indented
  after its first token; else the content starts the next line (after `[|`,
  `M.(`). A keyword just after
  `begin` counts as two columns after `begin`, as after `( `; `then begin`
  and `else begin` count as opened where their line starts (`end` is in the
  column of `if` or `else`);
- the indentation is even, and increases by at most 4 from a line to the
  next: deeper indentation means that a line break is missing. An item of
  two columns at the start of a line (`| `, `( `, `[ `) is a collapsed line:
  the increase is measured from where the content of the line starts, as if
  each such item was on its own line (in `| ( f`, from the column of `f`);
- a separator (`;`, `,`) after an element on several lines is alone on its
  line, in the column of the element, except after the `)`, `end` or `done`
  which close the whole element and start its last line: `);`, `end;`,
  `done;`.
- in a bracket, the elements of a list start in the same column on all its
  lines: a line after a separator starts in the column of the first element
  (not checked after `->`, `in`, `then` or `else`, where the separators are
  those of a body). So a bracket glued after something (`matrix.%{`, `M.(`,
  `~(`), which does not start its line, has its content on the next lines,
  as after `[|`; such a bracket counts as opened where the path or label
  before it starts;
- there are never two empty lines in a row (outside strings and comments).

So a multi-line parenthesized expression is written `( f` (with a space, so
that its contents are indented by an even amount), a multi-line `match`
or `function` is in `begin`/`end` rather than in parentheses, a keyword
after `begin` stays on its line (`begin match`, `begin fun`, `begin if`...),
except `let`, which always starts the next line (`test/formatting/begin_end.ml`),
and `begin` stays on the line of `then` or `else`, with `end` in the column
of `if`:

```ocaml
let ext =
  preceded_option state `PERCENT
    ( fun state ->
        attr_id state (syntax_error "keyword-extension-name" "...")
    )
in
if is state `GREATER then begin
  ...
end
else None
```

## Layout choices

The layouts which may look unusual and are decided, with the tests where
they are shown (in `test/formatting/`).

### Decided

- A separator after an element on several lines is alone on its line, even
  in sequences (`;` after a call on several lines), except after the `)`,
  `end` or `done` which close the element (`separators.ml`).
- The infix operators are of three kinds (`pretty/operator_classification.ml`),
  which differ when an operand is on several lines: the operator is then
  alone on its line, and the operands which receive nothing are indented
  by 2 from it. The operands of a symmetric operator (`a + b`, `a = b`,
  `a && b`, `x :: l`) are all indented; for an operator whose left operand
  flows into the right one (`x |> f`), only the left one; for an operator
  whose right operand flows into the left one (`f @@ x`, and the
  assignments `r := v`, `r.x <- v`), only the right one, and the operator
  may also end the line of a left operand on one line (`r :=`, then `v`
  indented). In a bracket
  (`( a + b )`, `[ a + b; c ]`), where the first operand cannot be indented,
  the operators are in the column of the bracket instead. The kind of an
  operator comes from its text: a table of the usual ones, else the
  direction in which its characters point (`expressions_operators.ml`,
  `separators_after_long_parts.ml`).
- The formatter keeps the tokens: it does not add the `|` before the first
  case of `function` or `match` when the source has none; whoever wants it
  writes it (`expressions_applications.ml`, `expressions_matching.ml`).
- A doc comment between two items without blank lines (ambiguous: the
  compiler attaches it to both) starts its line, even when it was after its
  item in the source: the ambiguity is visible
  (`blank_lines_and_doc_comments.ml.expected`).
- `then begin` and `else begin` are on one line, with `end` in the column of
  `if` (`expressions_conditionals.ml`).

### To look at again

The layouts which are not decided yet, which may change, are listed in
`LAYOUTS_TO_REVISIT.md`.

## The pretty printer

`pretty/` prints a CST with new line breaks and indentation, following the
layout rules above, and keeps everything else: the tokens, the comments, and
the blank lines (several blank lines in a row become one). It adds a blank
line after an item on several lines of a structure or a signature (checked
in the round-trip: a blank line is never removed). The attributes of an
item (`[@@...]`) or of a node (`e [@attr]`) are on its line when both fit,
else on the next line, in its column. `fromage FILE...` (`ocst_format.exe` in `_build`) prints files
formatted, and `fromage -i FILE...` formats them in place (as
`make fmt` does). It
refuses to format a file when the compiler's AST of the formatted source
differs from the original one, except for the locations (`same_ast.ml`): its
message says which item changes, and whether its doc comments or its code
do; `--debug` also shows the item and the formatted source.

- `trivia.ml`: the comments and blank lines before each token. The comments
  on the line of the previous token stay after it; the others are printed
  before the token, on their own lines.
- `doc.ml`: the documents of pretty_expressive and their combinators. A
  document knows its first and last tokens: two tokens are printed without a
  space between them only when they are lexed as the same two tokens. Its
  leading comments are kept apart, so that they do not prevent the document
  from fitting on one line.
- `operator_classification.ml`: the kinds of the infix operators, from
  their text, which give their layouts (symmetric, left into right, right
  into left; tested by `test/operator_classification_test.ml`).
- `syntax_layouts.ml`: the layouts shared by several constructs: let-like
  bindings (`x = e`), match-like cases (`p -> e`), fun-like functions
  (`fun x -> e`, `functor (X : S) -> M`), chains of operators (with
  assignments, `r := v`, `r.x <- v`), type constraints (`(e : t :> u)`) and
  aliases (`p as x`). Its `tok` shadows the one of `doc.ml` and refuses `=`, `->`,
  `<-`, `:>` and `as`: a construct which prints them must use one of these
  layouts, so that constructs of the same kind share their layout. A new
  kind of construct gets a new layout there, which takes documents, not
  nodes of the CST.
- `ocst_pretty.ml`: the layouts of the nodes of the CST.

pretty_expressive chooses, among the layouts of a document, the one with the
fewest lines within the width of the page: 80 by default, set by
`fromage --width N` (and the optional argument `~width` of
`Ocst_pretty`). Of the layouts with the same number of lines, it chooses the
one whose lines have the most even lengths (the smallest sum of the squares
of their lengths, in the cost factory of `doc.ml`): a packed list fills its
lines evenly rather than leaving its last element alone. A layout which is
wanted only when it saves lines (`Doc.losing_ties`), such as a separator
alone on its line or a let pattern on several lines, loses to the others of
the same number of lines. Its function `flatten` keeps all the
documents which it flattens, so `doc.ml` builds the one-line version of each
document itself.

It is tested in two ways:

- The round-trip tests (`test/roundtrip.ml`), on all the sources of the
  repository and the fuzzed sentences, check that the formatted source has the
  same tokens and comments as the original one, and that a comment (or doc
  comment) which starts its line in the original one still starts a line: a
  comment on its own line always stays there, that the blank lines are kept,
  and that the compiler's AST is the same except for the locations
  (`Same_ast`), doc comments included (the library `test/pretty_check`
  installs the formatter in these tests).
- `test/pretty_test.ml` formats the sources of OCST, `test/examples` and
  `test/formatting`, and checks that they are already formatted (by
  `make fmt`), and that the result follows the layout rules and is not
  changed by formatting it again, for the default width and for 120. The
  files of `test/formatting/` are written in the expected layout, one per
  topic: they are the tests of the formatter. A change of the formatter
  starts with a test there, which fails before the change. An input whose
  own layout matters (blank lines, positions of doc comments) is left as it
  is when it has an `a.ml.expected`, its formatted version, which `make fmt`
  writes.

## Workflow

- Edit the existing modules in place. Do not move them aside to rewrite
  them from scratch.
- Tests may be added, never removed.
- Check a behaviour with tests in `test`, never with temporary programs or
  files: the check then also catches regressions.
- When a bug or a regression is found, add explicit tests to
  `test/examples` (`.ml` and/or `.mli`): simple reproductions of the bug, and
  edge cases around it (attributes, comments between tokens, `and` parts,
  structures and signatures...). Check that they fail without the fix.
  Write them as soon as the bug is found, before fixing it, including for
  bugs found by the fuzzer.
- Sources that the compiler rejects go in `test/errors/cases`, with one
  error per file, since a file is checked as a whole: OCST must reject them
  too. A new error gets a file named after its id.
- Debugging aids belong in the test suite, permanently, rather than in
  temporary scripts.
