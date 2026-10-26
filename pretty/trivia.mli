(** The comments and the blank lines before the tokens of a file, from their
    trivia. The comments on the line of the previous token are printed after
    it (its trailing comments); the others are printed before the token, each
    on its line, with the blank lines before them. *)

open Ocst

type comment =
  | Plain of string (** A comment, or a doc comment which documents nothing. *)
  | Directive of string (** A line directive. *)

type info =
  { mutable trailing : string list;
    (** The comments after the token, on its line, in reverse order. *)
    before : (int * comment) list;
    (** The comments before the token, with the number of newlines before each. *)
    newlines : int; (** The number of newlines just before the token. *)
  }

val set : Token.any_token list -> unit
(** Sets the tokens of the file being printed. *)

val clear : unit -> unit
(** Forgets the tokens of the file. *)

val info : Token.any_token -> info
(** The comments around a token of the file. *)

val can_glue : Token.any_token -> Token.any_token -> bool
(** Can two tokens of the file be printed without a space between them? They
    must be lexed as the same two tokens, without comments between them. *)
