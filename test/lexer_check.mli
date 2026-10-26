(* The lexer round-trip: OCST lexes a source into tokens which print back to
   the same source, and which are the tokens of the lexer of the compiler, at
   the same positions. *)

val check : string -> string -> unit
(** [check filename source] fails with [Failure] when the tokens of [source]
    do not print back to [source], or differ from those of the compiler. *)
