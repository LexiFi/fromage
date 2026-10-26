module %ext M : S (** M. *)
;;
module %ext N (X : S) : S [@@attr] (** N. *);;
module %ext rec A : S (** A. *)
and B : S
type%ext t (** t. *)
val%ext x : int (** x. *)
