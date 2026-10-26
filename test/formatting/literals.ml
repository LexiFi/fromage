(* Literals: quoted strings, strings on several lines, numbers. *)

(* 1. Quoted strings. *)
let x1 = {|a quoted string|}
let x1_id =
  {id|a quoted string with an identifier, which contains |} and "quotes"|id}

let x1_lines =
  {|a quoted string
on several lines
  with indentation|}

let x1_argument =
  f
    {|a quoted string which is long enough to not fit on the line with f|}
    argument_number_one

(* 2. Strings on several lines. *)
let x2 =
  "a string on several lines\n\
          with an escaped newline"

let x2_raw =
  "a string
which contains a newline"

(* 3. Numbers. *)
let x3 =
  ( 0x7fff_ffff, 0o777, 0b1010, 1_000_000,
    1e10, 0x1p-3, 1l, 1L, 1n, 'a', '\n', '\x41'
  )
