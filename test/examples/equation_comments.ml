(* Type equations which cannot be on one line, because of a comment on a line
   of its own. *)

type t = u (* comment *) = A | B

type t' = u (* comment *) = private { x : int }

type t'' = (a, b) u (* comment *) = ..

(* From the fuzzer: doc comments before the name and before the [=] of the
   equation. *)

(** d0 *)
type

  (** d1 *) x1

  (** d2 *) = (** d3 *) {%ext|s2|} (** d4 *)
  =
  (** d5 *) private
  (** d6 *) |
  (** d7 *) false
