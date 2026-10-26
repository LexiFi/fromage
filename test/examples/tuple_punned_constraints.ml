(* Punned labels with a type constraint or a coercion, in tuples and
   arguments. A first element [~(x : t)] is located up to the second
   element. *)

let _ = ~(x : int), ~y
let _ = ~(x :> int), ~y
let _ = ~(x : int :> float), ~y
let _ = ~(x : int), ~(y :> int), ~(z : int :> float)
let _ = a, ~(x : int)
let _ = a, ~(x :> int), b
let _ = (~(x : int), ~y)
let _ = ~(x : int), ~y, ~z
let _ = ~( (* a *) x (* b *) : (* c *) int (* d *) :> (* e *) float (* f *) ), y

let _ = f ~(x : int) ~(y :> int) ~(z : int :> float)

let f =
  function
  | ~(x : int), ~y -> x + y
  | ~(x : int), ~(y : int), .. -> x + y
