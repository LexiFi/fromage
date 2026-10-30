(** A pretty printer for the CST: it prints the same tokens and comments as
    the source, with new line breaks and indentation, following the layout
    rules of ocst/HACKING.md, for a width of [width] characters. *)

val default_width : int
(** 80. *)

val implementation : ?width:int -> Ocst.Cst.implementation -> string
val interface : ?width:int -> Ocst.Cst.interface -> string

(** The kinds of infix operators, which give their layouts. *)
module Operator_classification = Operator_classification
