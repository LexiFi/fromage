(** A pretty printer for the CST: it prints the same tokens and comments as
    the source, with new line breaks and indentation, following the layout
    rules of ocst/HACKING.md, for a width of [width] characters. *)

val default_width : int
(** 80. *)

val implementation : ?width:int -> Ocst.Cst.implementation -> string
val interface : ?width:int -> Ocst.Cst.interface -> string

(** Why a source is not formatted. *)
type error =
  | Syntax_error of Ocst.Syntax_error.t
  | Changes_tokens of Same_tokens.difference
  (** Formatting would change the input of the compiler's parser, not only
      the whitespace ([Same_tokens]): a bug of the formatter, or of the
      lexer of OCST. *)

val format :
  ?width:int -> interface:bool -> filename:string -> string
  -> (string, error) result
(** [format ?width ~interface ~filename source]: [source], an interface or
    an implementation, formatted, after checking that formatting only
    changes its whitespace ([Same_tokens]). [filename] is used in the
    errors. *)

(** The check that formatting only changes whitespace. *)
module Same_tokens = Same_tokens

(** The kinds of infix operators, which give their layouts. *)
module Operator_classification = Operator_classification
