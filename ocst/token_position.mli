(** Source positions of lossless tokens, as computed by the OCaml lexer. *)

type span = { start : Lexing.position; stop : Lexing.position }

val initial : filename:string -> Lexing.position

val advance : Lexing.position -> string -> Lexing.position
(** The position after the given text, which does not contain line
    directives. *)

val skip_trivia : Lexing.position -> Token.trivia -> Lexing.position
(** The position after a trivia, taking line directives into account. *)

val skip_leading : Lexing.position -> _ Token.token -> Lexing.position
(** The position after the leading trivia of a token. *)

val spans : filename:string -> Token.any_token list -> span array
(** The span of every token of a token stream. *)
