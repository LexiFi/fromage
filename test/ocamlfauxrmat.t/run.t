The executable ocamlformat of ocamlfauxrmat, with the command line of
ocamlformat. A project, whose root is the directory with the dune-project:

  $ mkdir -p project/sub/deep project/disabled/enabled project/ignored
  $ cd project
  $ touch dune-project
  $ long='let f x = match x with Some value -> value + other_value + third_value | None -> 0'
  $ for file in a.ml sub/deep/b.ml disabled/c.ml disabled/enabled/d.ml ignored/e.ml ignored/f.ml; do
  >   echo "$long" > $file
  > done

Without a .ocamlformat, unlike ocamlformat, the files are formatted, for a
width of 80, as dune fmt runs it:

  $ ocamlformat --impl a.ml
  let f x =
    match x with
      Some value -> value + other_value + third_value
    | None -> 0

The margin of the .ocamlformat files, the deeper ones overriding the others,
and of the command line, overriding them; the other options are ignored:

  $ printf 'profile = default\nmargin = 40\n' > .ocamlformat
  $ printf 'margin = 60\n' > sub/.ocamlformat
  $ ocamlformat a.ml
  let f x =
    match x with
      Some value ->
        value + other_value + third_value
    | None -> 0
  $ ocamlformat sub/deep/b.ml
  let f x =
    match x with
      Some value -> value + other_value + third_value
    | None -> 0
  $ ocamlformat --margin 40 sub/deep/b.ml
  let f x =
    match x with
      Some value ->
        value + other_value + third_value
    | None -> 0

With disable = true, only the files of the .ocamlformat-enable files are
formatted; the others are printed as they are:

  $ printf 'disable = true\n' > disabled/.ocamlformat
  $ printf '# Formatted anyway.\nenabled/d.ml\n' > disabled/.ocamlformat-enable
  $ ocamlformat disabled/c.ml
  let f x = match x with Some value -> value + other_value + third_value | None -> 0
  $ ocamlformat disabled/enabled/d.ml
  let f x =
    match x with
      Some value ->
        value + other_value + third_value
    | None -> 0

The files of the .ocamlformat-ignore files, where * and ? match any
characters but /, are not formatted:

  $ printf 'ignored/e.*\n' > .ocamlformat-ignore
  $ ocamlformat ignored/e.ml
  let f x = match x with Some value -> value + other_value + third_value | None -> 0
  $ ocamlformat ignored/f.ml
  let f x =
    match x with
      Some value ->
        value + other_value + third_value
    | None -> 0

The standard input, with --name for its configuration and its kind, or
--impl and --intf:

  $ echo 'val f :   int ->   int' | ocamlformat --name sub/x.mli -
  val f : int -> int
  $ echo 'val f :   int ->   int' | ocamlformat --intf -
  val f : int -> int
  $ echo 'val f :   int ->   int' | ocamlformat -
  -: unknown extension, use --impl or --intf
  [1]

--check fails on the files which are not formatted, --inplace formats them,
and --output writes the formatted source to a file:

  $ ocamlformat --check a.ml sub/deep/b.ml
  a.ml: not formatted
  sub/deep/b.ml: not formatted
  [1]
  $ ocamlformat --inplace a.ml sub/deep/b.ml
  $ ocamlformat --check a.ml sub/deep/b.ml
  $ ocamlformat ignored/f.ml -o formatted.ml && cat formatted.ml
  let f x =
    match x with
      Some value ->
        value + other_value + third_value
    | None -> 0

Errors:

  $ echo 'let x =' > error.ml
  $ ocamlformat error.ml
  File "error.ml", line 2, characters 0-0:
  Error: Unexpected end of input, '=' in a binding expects an expression.
  [1]
  $ ocamlformat a.ml sub/deep/b.ml
  ocamlformat: Must specify exactly one input file without --inplace or --check
  [123]
  $ printf 'margin = wide\n' > sub/.ocamlformat
  $ ocamlformat sub/deep/b.ml
  $TESTCASE_ROOT/project/sub/.ocamlformat: invalid margin: "wide"
  [1]
