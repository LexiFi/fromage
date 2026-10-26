let x = 1 (** x. *)

and y = 2
(** Ambiguous between y and z. *)
and z = 3

type t = int (** t. *)

(** Text before u. *)

and u = bool

module rec M : sig end = struct end (** M. *)

and N : sig end = struct end

class c = object end (** c. *)
and d = object end
