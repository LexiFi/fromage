(* Lists separated by [;] accept a trailing [;]; empty arrays and objects
   have no [;]. *)

let l = [ 1 ]
let l = [ 1; ]
let l = [ 1; 2 ]
let l = [ 1; 2; ]
let l = [ (* a *) 1 (* b *) ; (* c *) ]

let a = [||]
let a = [||]
let a = [| 1 |]
let a = [| 1; 2; |]

let r = { x = 1 }
let r = { x = 1; }
let r = { r with x = 1; y = 2; }

let o = {<>}
let o = {< x = 1 >}
let o = {< x = 1; y; >}

let i = a.%{1}
let i = a.%{1; 2}
let i = a.%{1; 2;}
let i = a.%(1;)

let f =
  function
  | [ x; ] -> x
  | [||] -> 0
  | [| x; y; |] -> x + y
  | { x; } -> x
  | { x; _ } -> x
  | { x; _; } -> x
  | { x = _; y; _ } -> y
