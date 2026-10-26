(* A pattern starting with a labelled element is a tuple, even where a
   pattern of higher precedence is expected: after [exception], a constructor
   or a variant, and on the right of [::] and [|]. Found by the fuzzer. *)

let f =
  function
  | exception ~x, y -> ()
  | exception ~x:p, .. -> ()
  | exception ~(x : int), y -> ()
  | exception%ext ~x, y -> ()
  | C ~x, y -> ()
  | C ~x:p, ~y, .. -> ()
  | M.C ~(x : int), y -> ()
  | `A ~x, y -> ()
  | `A ~x:p, .. -> ()
  | a :: ~x, b -> ()
  | a :: ~x, b as z -> ()
  | a :: ~x, b :: c -> ()
  | a :: b :: ~x, .. -> ()
  | a | ~x, b -> ()
  | a | ~x:_, .. | c -> ()
  | (exception ~x, y) [@attr] -> ()
  | (* a *) exception (* b *) ~x (* c *) , (* d *) y -> ()
  | () ~x, .. -> ()
  | effect () ~x, .., k -> ()
  | effect C ~x, .., k -> ()

let C ~x, y = v
let `A ~x, .. = v
let a :: ~x, y = v

[@@@attr ? ~x, y]
[@@@attr ? exception ~x:p, .. when e]
[@@@attr ? `A ~x, y | C ~y, ..]
