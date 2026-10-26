module%ext M = N (** M. *)
;;
module%ext N (X : S) = X [@@attr] (** N. *);;
module%ext rec A : S = A (** A. *)
and B : S = B
type%ext t (** t. *)
let%ext x = 1 (** x. *)
