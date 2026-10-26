(* Type equations with comments on lines of their own, before the [=] after
   them: the equation cannot be on one line. *)

type t := u
  (* comment *)
  = true

type t' = u
  (* comment *)
  = private A | B

type t'' = u
  (* comment *)
  = private { x : int }

(* From the fuzzer: doc comments before the name and before the [=] of the
   equation, so that neither the head nor the equation is on one line. *)

(** d0 *)
type

  (** d1 *) x1
  (** d2 *) := (** d3 *) {%ext|s2|}
  (** d4 *) =
  (** d5 *) true
