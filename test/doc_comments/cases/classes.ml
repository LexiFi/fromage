class c = object
  (** At the start. *)

  val x = 1 (** x. *)

  method m = x
  (** Ambiguous between m and n. *)
  method n = x

  [@@@attr] (** Marked by the floating attribute. *)

  (** At the end. *)
end

class type ct = object
  method m : int (** m. *)

  (** Text. *)

  method n : int
end
