(** Checks that two sources have the same AST for the compiler's parser,
    except for the locations: formatting a source must not change its AST,
    nor the items its doc comments document ([ocaml.doc] attributes). *)

type difference =
  { summary : string;
    (** The first item which differs, its lines, and whether its doc
          comments or its code change. *)
    details : string;
    (** The item, printed from the two ASTs, and the formatted source
          around it. *)
  }

val implementation :
  filename:string -> string -> string -> (unit, difference) result
(** [implementation ~filename source formatted]: [Ok ()] when the two
    implementations have the same AST, or when the compiler's parser rejects
    [source] (there is no AST to compare); else [Error] with the first item
    which differs. *)

val interface : filename:string -> string -> string -> (unit, difference) result
(** As [implementation], for interfaces. *)

val equal_modulo_locations : 'a -> 'a -> bool
(** Structural equality, where any two locations ([Location.t]) are equal:
    their positions, their ghost flags. *)
