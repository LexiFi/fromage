(* A [fun] body which is a tuple starting with a closed [function] takes the
   operators [:=] and [::]. Found by the fuzzer, with [--exhaust]. *)

let _ = fun a -> function b -> ., x := y
let _ = fun a -> function b -> ., function c -> . := y
let _ = fun a -> function b -> ., x :: y
let f a = function b -> ., x := y
let f a = function b -> ., function c -> . := y
let _ = object method m a = function b -> ., x := y end
