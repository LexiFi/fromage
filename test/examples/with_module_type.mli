(* The module type after [with module type X =] ends before [->] and [with],
   since [=] has a higher precedence; after [:=], it ends before [with] only.
   Found by the fuzzer. *)

include S with module type X = T -> U
include S with module type X = sig end -> U
include S with module type X = T with type t = u
include S with module type X = T with module type Y = U
include S with module type X = T and type t = u
include S with module type X = T [@attr] -> U
include S with module type X = (T -> U) -> V
include S with module type X = functor (A : T) -> U with type t = u
include S with module type X = (A : T) -> U with type t = u
include S with module type X := T -> U
include S with module type X := T -> U with type t = u
include S with module type X := T with type t = u
include S with module type X := T -> U -> V
module type S := (sig end) with module type X = T with type t := u
include sig end with module type X = Y with module Z = W and module U := V
module M : S with module type X = T -> U
module type S = S with module type X = (* a *) T (* b *) -> (* c *) U
