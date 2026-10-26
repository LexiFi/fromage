(* The right operand of an operator is an [expr], which may be a tuple
   starting with a labelled element: [a + ~x, y] is [a + (~x, y)]. Found by
   the fuzzer. *)

let _ = a - ~l:x, y
let _ = a + ~x, y
let _ = a + ~(x : t), y
let _ = a + ~(x :> t), y
let _ = a + ~x:1, ~y:2, z
let _ = a :: ~x, y
let _ = a + b + ~x, y
let _ = a * b + ~x, y
let _ = a || ~x, y
let _ = a && ~x, y
let _ = a = ~x, y
let _ = a @ ~x, y
let _ = a ** ~x, y
let _ = a := ~x, y
let _ = r.f <- ~x, y
let _ = - ~x, y
let _ = -. ~x, y
let _ = a, b + ~x, y
let _ = a + (* a *) ~x (* b *) , (* c *) y
let _ = match e with p -> . - ~l:x, y
