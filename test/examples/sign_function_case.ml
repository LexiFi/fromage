(* The operand of a sign ([%prec prec_unary_minus]) does not extend over a
   comma or a bar: a [function] there has a single case. Found by the fuzzer,
   with [--exhaust]. *)

let _ =
  function
    a -> -function b -> x
  | c -> y

let _ =
  function
    a -> +function b -> x
  | c -> y

let _ =
  function
    a -> -.function | b -> x
  | c -> y

let _ =
  function
    a -> - -function b -> x
  | c -> y

let _ =
  match e with
    a -> -function b -> x
  | c -> y

let _ = -function b -> x, y
let _ = (-function b -> x), y
