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
  | Changes_ast of Ocst.Same_ast.difference
  (** Formatting would change the AST of the source for the compiler's
      parser (except for the locations): a bug of the formatter. *)

val format :
  ?width:int -> interface:bool -> filename:string -> string
  -> (string, error) result
(** [format ?width ~interface ~filename source]: [source], an interface or
    an implementation, formatted, after checking that its AST does not
    change ([Ocst.Same_ast]). [filename] is used in the errors. *)

(** The kinds of infix operators, which give their layouts. *)
module Operator_classification = Operator_classification
