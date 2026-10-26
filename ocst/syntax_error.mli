(** Syntax errors: what is wrong in the source, and how to fix it. *)

type t =
  { id : string;
    (** A stable identifier of the kind of error, used by the tests to check
        that every error is tested. *)
    location : Location.t; message : string;
    notes : (Location.t * string) list;
    (** Related locations, such as an opening parenthesis which is not
        closed. *)
  }

exception Error of t

val location_text : Location.t -> string
(** [File "f.ml", line 1, characters 4-5:], as printed by the compiler. *)

val to_string : t -> string
(** The error as reported by the compiler: [File "f.ml", line 1, characters
    4-5:] followed by [Error: ] and the message, then the notes. *)
