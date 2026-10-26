(** Recursive-descent parsing of lossless OCaml token streams. The parsers
    raise [Syntax_error.Error] on invalid sources, located in [filename]. *)

val implementation_tokens :
  ?filename:string -> Token.any_token list -> Cst.implementation

val interface_tokens : ?filename:string -> Token.any_token list -> Cst.interface
val implementation : ?filename:string -> Token_stream.t -> Cst.implementation
val interface : ?filename:string -> Token_stream.t -> Cst.interface
