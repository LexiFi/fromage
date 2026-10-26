(** A printable token stream.  Its construction operations ensure that lexing
    the complete rendered stream produces the same tokens. *)

type t

exception Missing_trivia of
  { previous : Token.any_token; next : Token.any_token; }

exception Invalid_token of Token.any_token

val empty : t
val insert : t -> Token.any_token -> t
val insert_create_trivia : t -> Token.any_token -> t
val tokens : t -> Token.any_token list
val source : t -> string

(** Insert a lexer-emitted token group atomically. *)
val insert_group : t -> Token.any_token list -> t

(** Like [insert_group], inserting whitespace before the group when needed. *)
val insert_group_create_trivia : t -> Token.any_token list -> t
