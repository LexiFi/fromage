(* The module language which is not in module_bindings.ml: functor
   applications, [module type of], [with module], [include]. *)

(* 1. Functor applications. *)
module M1 = F (X)
module M1_long =
  Functor_with_a_long_name
    (Argument_number_one) (Argument_number_two) (Argument_number_three)

module M1_struct =
  F
    ( struct
        let aaaaaaaaaaaaaaaa = 1
        let bbbbbbbbbbbbbbbbbbb = 2
      end
    )

module M1_nested =
  F
    (G (Argument_with_a_long_name) (Other_argument_with_a_long_name))
    (H (Third_argument_with_a_long_name))

(* 2. [module type of]. *)
module type S2 = module type of M2
module type S2_long =
  module type of
    struct
      include Module_with_a_long_name
      include Other_module_with_a_long_name
    end

(* 3. [with type] and [with module]. *)
module type S3 = S with module M = N
module type S3_long =
  Signature_with_a_long_name
    with module Mmmmmmmmmmmmmmmm = Nnnnnnnnnnnnnnnnnnnnnn
    and type t = Nnnnnnnnnnnnnnnnnnnnnn.t and module P := Q

(* 4. [include]. *)
include F (X)
include
  Functor_with_a_long_name
    (Argument_number_one) (Argument_number_two) (Argument_number_three)

(* 5. First-class modules. *)
let x5 = (module M : S)
let x5_long =
  ( module Module_with_a_long_name
  : Signature_with_a_long_name with type t = int and type u = string
  )

module M5 = (val x5)

(* 6. Recursive modules. *)
module rec A6
  : sig
      type t
      val f : t -> B6.t
    end
  =
  struct
    type t = int
    let f x = B6.of_int x
  end
and B6
  : sig
      type t
      val of_int : int -> t
    end
  =
  struct
    type t = int
    let of_int x = x
  end
