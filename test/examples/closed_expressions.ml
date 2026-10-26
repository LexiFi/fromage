(* Some expressions cannot be extended by a following operator, comma or
   semicolon: a [function] whose last case is unreachable ([.]) or ends with a
   [;], and the labelled elements of a tuple ([~x:e], [~x], [~(x : t)]). The
   following token then applies to the enclosing expression. Found by the
   fuzzer, with [--exhaust]. *)

(* Labelled elements at the end of a tuple. *)
let _ = (a, ~l:'a' ^ b)
let _ = a, ~l:x + b * c
let _ = a, ~x ** b
let _ = a, ~(x : t) || b
let _ = a, ~x :: b
let _ = a, ~x := b
let _ = a, ~l:x land b, c
let _ = [ a, ~l:x = b ]

(* [:=] and [::] after an unreachable case. *)
let _ = (a := function x -> . :: b)
let _ = [ a := function x -> . :: b ]
let _ = a := function x -> . + b
let _ = a :: function x -> . := b
let _ = (a := function x -> ., b)

(* Signs. *)
let _ = -function x -> ., b
let _ = +function x -> ., b
let _ = + -function x -> ., b
let _ = -function x -> . ** b
let _ = -.function | x -> ., b

(* A last case ending with [;]. *)
let _ = [ function x -> a;, b ]
let _ =
  [ function x -> a;, b; function x -> a;, b; function x -> a;, b;
    function x -> a;, b; function x -> a;, b; function x -> a;, b;
    function x -> a;, b; function x -> a;, b; function x -> a;, b
  ]

let _ = (function x -> a;), b
let _ = function x -> a;, b
let _ = function x -> a; b;, c

(* A semicolon after a [function] ending with an unreachable case ends the
   enclosing expression. *)
let _ =
  let* x in
  function x -> .
  ;
  b

let _ =
  let x = e in
  function x -> .
  ;
  b

let _ = fun x -> function x -> .; b
let _ =
  ( let* x in
    function x -> .
  );
  b

(* Last, since the semicolon would apply to the next item. *)
let _ =
  let* x in
  function x -> .
  ;
