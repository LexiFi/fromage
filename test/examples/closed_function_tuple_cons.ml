let x = function false -> ., function false -> . :: X1, 1
let x = function false -> ., function false -> . :: X1, 1, 2
let x = function false -> ., function false -> . :: X1 :: X2, 1
let x = function false -> ., function false -> . = X1, 1
let x = function false -> ., function false -> . :: X1, ~(x :> t)
let x = 1, function false -> . :: X1, 1
let x = function false -> ., (* c *) function false -> . :: X1, 1 [@attr]
let f x = function false -> ., function false -> . :: X1, 1
let x =
  let y = 1 in
  function false -> ., function false -> . :: X1, 1

let x =
  match y with A -> function false -> ., function false -> . :: X1, 1

let x = [ function false -> ., function false -> . :: X1, 1 ]
let x = function false -> ., function false -> . :: X1, 1; 2
let x =
  function false -> ., function false -> . :: X1, function false -> .
  :: X2
  ,
  1

let x = function false -> ., function false -> . ** X1, 1
let x = function false -> ., function false -> . := X1, 1
let x = function false -> ., ~x:1 :: X1, 1
let x = y := function false -> ., function false -> . :: X1, 1
[@@@a function false -> ., function false -> . :: X1, 1]
let x = -function false -> ., function false -> . :: X1, 1
let x = a + function false -> ., function false -> . :: X1, 1
