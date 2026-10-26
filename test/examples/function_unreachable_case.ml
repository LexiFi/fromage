(* A [function] whose last case is unreachable ([.]) cannot extend over a
   following comma: it becomes the first element of a tuple, which extends
   over the rest of the expression. Found by the fuzzer. *)

let _ = function x -> ., y
let _ = function x -> ., ~y
let _ = function x -> ., ~y:z, ~(w : t)
let _ =
  function
    x -> .
  | y -> .
  ,
  z

let _ = function | x -> ., y
let _ = function%ext x -> ., y
let _ = function [@attr] x -> ., y
let _ = function x -> ., y; z
let _ = function x -> ., y :: z
let _ = function x -> ., function y -> ., z
let _ = fun x -> function x -> ., y
let _ = fun x -> function x -> ., y; z
let _ = fun x -> function x -> .; y
let _ = a + function x -> ., y
let _ = a, function x -> ., y
let _ = a, function x -> ., y, z
let _ = f (function x -> ., y)
let _ =
  let x = function x -> ., y in
  x

let _ = match e with x -> ., y
let _ = try e with x -> ., y
let _ = (* a *) function (* b *) x (* c *) -> (* d *) . (* e *) , (* f *) y
let x = function x -> ., y
and z = w

let _ = [ function x -> ., y ]
