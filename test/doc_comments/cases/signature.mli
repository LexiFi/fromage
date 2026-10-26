(** At the start. *)

val x : int (** x. *)

type t (** t. *)
;;
(** After ;;. *)

module M : sig (** In an empty signature. *) end

[@@@attr]
(** After a floating attribute. *)
external f : int -> int = "f"
