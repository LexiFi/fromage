(* Signed constants are simple patterns, hence parameters. Found by the
   fuzzer, with [--exhaust]. *)

let f +2 = x
let f +2.0 = x
let f -2 = x
let f -2 -3.0 = x
let f x -1 = x
let _ = fun -1 -> x
let _ = fun x +1 -> x
let _ = fun (-1) -> x
let f ~x:-1 = x
let rec f -1 = x
and g +1 = y

class c -1 = object end
let _ = object method m -1 = x end
