(* The bases of records, and the labels which start like them. *)
let _ = { x }
let _ = { x = 1; y }
let _ = { M.x = 1; M.y : int = 2 }
let _ = { x with y = 1 }
let _ = { M.x with y = 1 }
let _ = { r.field with y = 1 }
let _ = { (f x) with y = 1 }
let _ = { { x = 1 } with y = 2 }
let _ = { x : int }
let (x : int) = 1
let (A | B) = x
let [ x ] : int list = l
let { x; _ } : t = r
let `A : t = x
let 'a' .. 'z' : char = c
let _ =
  let* (x : int) = y in
  let* (A | B) = x in
  x
