(* Package types: a module type path, with [type] constraints that have no
   parameters, [private] or [constraint]. Found by the fuzzer. *)

type t = (module S)
type t = (module M.S)
type t = (module F(X).S)
type t = (module S with type t = int)
type t = (module S with type t = int and type M.u = 'a list)
type t = (module S [@attr])
type t = (module S with type t = int) -> int
type t = (module M : S with type t = int) -> M.t
let f (module M : S with type t = int) = ()
let m = (module M : S with type t = int)
module M = (val m : S with type t = int)
module M = (val m : S :> T with type t = int)
module M = (val m :> S)

(* Parentheses do not change the module type. *)
type t = (module (S))
type t = (module ((S)))
type t = (module (S) with type t = int)
type t = (module (S with type t = int))
let f (module M : (S)) = ()
let m = (module M : (S))
