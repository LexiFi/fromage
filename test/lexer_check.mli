(* The lexer round-trip: OCST lexes a source into tokens which print back to
   the same source, and which are the tokens of the lexer of the compiler, at
   the same positions. *)

val check : name:string -> string -> string -> unit
(** [check ~name filename source] fails with [Failure] when the tokens of
    [source] do not print back to [source], or differ from those of the
    compiler. [filename] is used in positions, [name] in error messages. *)
