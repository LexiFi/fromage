(* The module language of signatures which is not in the other tests. *)

(* 1. Functors. *)
module F1 : functor (X : S) -> S with type t = X.t
module F1_long :
  functor
    (Argument_number_one : Signature_number_one)
    (Argument_number_two : Signature_number_two)
  -> Result_signature

(* 2. [module type of] and [with]. *)
module M2 :
  module type of Module_with_a_long_name
    with type t = int and type u = string and module N := O

(* 3. [include]. *)
include S with type t := int
include
  module type of struct include Module_with_a_long_name end
    with type aaaaaaaaaaaaaaaa = int

(* 4. Module aliases and substitutions. *)
module M4 = N
module M4_substitution :=
  Module_with_a_long_name.Submodule_with_a_long_name.Third

(* 5. Recursive modules. *)
module rec A5 :
  sig
    type t
    val f : t -> B5.t
  end
and B5 :
  sig
    type t
    val of_int : int -> t
  end
