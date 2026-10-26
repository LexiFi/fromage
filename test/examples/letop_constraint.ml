(* A [let*] binding of a name may have a type constraint, as a simple
   pattern: [simple_pattern COLON core_type EQUAL seq_expr]. Found by the
   fuzzer. *)

let _ =
  let* x : t = e in
  x

let _ =
  let* x : t = e
  and* y : u = f
  in
  x

let _ =
  let* x : t list = e
  and* y = f
  in
  x

let _ =
  let* ( + ) : t = e in
  x

let _ =
  let* _ : t = e in
  x

let _ =
  let* (x, y) : t = e in
  x

let _ =
  let* x y : t = e in
  x

let _ =
  let* (* a *) x (* b *) : (* c *) t (* d *) = (* e *) e in
  x
