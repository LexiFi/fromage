(* Expression precedence and associativity cases checked against the compiler. *)
let _ = -a ** b
let _ = if c then a else b, d
let _ = a, b := c
let _ = a := b, c
let _ = -f x
let _ = -a.b
let _ = !a.b
let _ = f a ## b c
let _ = C x :: y
let _ = x.a <- y := z
let _ = a, b.x <- c
let _ = - -1
let _ = -1 + 2
let _ = f - 1
let _ = a || b && c
let _ =
  let x = 1 in
  x; y

let _ = f ~x:1 ~y ?z
let _ = a :: b :: c
let _ = a @ b @ c
let _ = `A x
let _ = f `A x
let _ = a + b [@a]
let _ = a @ b [@a]
let _ = f x [@a]
let _ = a := b [@x]
let _ = -x [@a]
let _ = a :: b [@a]
let _ = C x [@a]
let _ = if a then b, c
let _ = a + if c then d else e + f
