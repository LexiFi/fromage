type t =
  | A (** A. *)
  | B of int (** B. *)
(** After t. *)

type u = C (** C or u? *)

type t += D (** D. *) | E (** E. *)

exception F (** F. *)
