(** The doc comments of a token stream, and the items they document.

    The compiler attaches doc comments by position (ocaml/parsing/lexer.mll
    and docstrings.ml): the lexer files each doc comment as documenting the
    token before it ([post]) or after it ([pre]), as floating text, or as
    extra text at the beginning or the end of a list of items, according to
    the blank lines around it; the semantic actions of the parser then claim
    them, at the start and the end of the items. This module replays these
    rules on the boundaries between tokens: the boundary [k] is the place
    before the token of index [k] (doc comments excluded).

    The parser claims doc comments as the semantic actions do, then places
    them in the CST ({!place}, {!resolve}). The doc comments which no item
    claims become warnings, in the trivia of the next token
    ({!remaining}). *)

type docstring = [ `DOCSTRING of string ] Token.token

(** A doc comment of the stream. *)
type doc

type t

val split : Token.any_token list -> Token.any_token list * t
(** The tokens without their doc comments, and the doc comments. *)

(** {1 Claims}

    The functions of [Docstrings], at a boundary: they mark the doc comments
    as attached and associated in the same way. *)

val pre_docs : t -> int -> doc option
(** The doc comment of the item starting at the boundary ([get_pre_docs]). *)

val post_docs : t -> int -> doc option
(** The doc comment of the item ending at the boundary ([get_post_docs]). *)

val mark_pre : t -> int -> unit
(** [mark_pre_docs]: an item starting at the boundary, which takes no doc
    comment, such as a floating attribute. *)

val mark_post : t -> int -> unit
(** [mark_post_docs]. *)

val info : t -> int -> doc option
(** The doc comment of a constructor or a field ending at the boundary
    ([get_info]). *)

val text : t -> int -> doc list
(** The floating doc comments before an item ([get_text]). *)

val post_text : t -> int -> doc list
(** The doc comments at the end of an empty list ([get_post_text]). *)

val pre_extra_text : t -> int -> doc list
(** The doc comments at the start of a list ([get_pre_extra_text]). *)

val post_extra_text : t -> int -> doc list
(** The doc comments at the end of a list ([get_post_extra_text]). *)

val has_text : t -> int -> bool
(** Does {!text} claim a doc comment at the boundary? *)

(** {1 Placement in the CST} *)

val place : t -> doc -> docstring
(** The token of a claimed doc comment, with the doc comments before it at its
    boundary which are not placed yet, as warnings in its trivia. Doc
    comments are placed in source order. *)

val resolve :
  t -> int -> post:doc option -> texts:doc list -> pre:doc option
  -> docstring option * Cst.floating list * docstring option
(** Places the doc comments of a boundary between two items, once all the
    claims there are made: the doc comment of the previous item, the floating
    doc comments ([texts], and the ambiguous ones, which documents both items),
    and the doc comment of the next item. *)

val remaining : t -> int -> Token.trivia list
(** The doc comments of the boundary which are not placed, as warnings. *)
