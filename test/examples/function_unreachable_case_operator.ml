(* A [function] whose last case is unreachable ([.]) is not a [fun_expr]: at
   the end of a tuple, a following operator or attribute applies to the whole
   tuple, with the usual precedences. Found by the fuzzer. *)

let _ = a, function y -> . ** z
let _ = a, function y -> . + z * w
let _ = a, function y -> . ** z ** w
let _ = a, function y -> . || z
let _ = a, function y -> . :: z
let _ = a, function y -> . := z
let _ = a, function y -> . [@attr]
let _ = a, function y -> . ** z, w
let _ = a, function y -> . ** z, function y -> . + w
let _ = a, function y -> .; z
let _ = ~x, function y -> . ** z
let _ = function x -> ., function y -> . ** z
let _ = (function x -> .), function y -> . ** z
let _ = b + function x -> ., function y -> . + z
let _ = b + function x -> ., function y -> . ** z
let _ = b ** function x -> ., function y -> . + z
let _ = f (a, function y -> . ** z)
let _ = fun x -> function x -> ., function y -> . ** z
let _ =
  a, (* a *) function (* b *) y (* c *) -> (* d *) . (* e *) ** (* f *) z
