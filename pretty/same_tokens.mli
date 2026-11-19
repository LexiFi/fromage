(** Checks that formatting a source only changes its whitespace: that the
    formatted source gives the same input to the compiler's parser. That
    input is the tokens of the compiler's lexer, so that the check does not
    depend on the lexer of OCST, which the formatter uses; with the doc
    comments, which the lexer attaches to the tokens around them, depending
    on the blank lines between them. The parser uses the positions of the
    tokens only for the locations and to find these doc comments. The
    comments are compared too: formatting keeps them. *)

type difference =
  { summary : string; (** The first token or comment which differs. *)
    details : string; (** The formatted source around it. *)
  }

val check : filename:string -> string -> string -> (unit, difference) result
(** [check ~filename source formatted]: [Ok ()] when the compiler's lexer
    gives the same tokens, comments and doc comments, with the same text, for
    the two sources, with blank lines at the same places for the attachment
    of the doc comments (and fails at the same token, if it fails); else
    [Error] with the first one which differs. [filename] is used in the
    errors. *)
