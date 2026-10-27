(** The comments and the blank lines before a token, from its trivia. The
    comments on the line of the previous token stay at the end of that line;
    the others are printed before the token, each on its line, with the blank
    lines before them. *)

open Ocst

type elt =
  | Newline of int (** Newlines, as many as the number. *)
  | Space of int
  (** Blanks on a line, as many as the number: the indentation after
      newlines, or the blanks between two elements on a line. *)
  | Comment of string
  (** A comment, or a doc comment which documents nothing, with its
      delimiters. *)
  | Directive of string (** A line directive, as [# 12 "file.ml"]. *)

type t = elt list
(** The trivia before a token, for printing: the blanks before a newline are
    not kept. *)

val of_token : Token.any_token -> t
(** The trivia of a token ([Token.trivia]). *)

type info =
  { previous_line : string list;
    (** The comments on the line of the previous token, in order: they are
        printed at the end of that line. *)
    before : (int * elt) list;
    (** The comments and the line directives before the token, with the
        number of newlines before each. *)
    newlines : int; (** The number of newlines just before the token. *)
  }

val set : Token.any_token list -> unit
(** Sets the tokens of the file being printed: its first token has no
    previous line. *)

val clear : unit -> unit
(** Forgets the tokens of the file. *)

val info : Token.any_token -> info
(** The comments before a token of the file, from its own trivia
    ([of_token]). *)

val can_glue : Token.any_token -> Token.any_token -> bool
(** Can two tokens of the file be printed without a space between them? They
    must be lexed as the same two tokens, without comments between them. *)
