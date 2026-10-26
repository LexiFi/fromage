(* Comments which the lexer and the doc comments treat apart, rare in
   code: a comment starting with more than two stars, character literals
   and strings inside comments, and a stop comment after doc comments. *)

(*** A comment with three stars, which is not a doc comment. *)
let a = 1

(* An empty character literal '', an escape '\x41', and a string with an
   unknown escape "\q", inside a comment. *)
let b = 2

let c = 3
(** After c. *)

(** Before the stop comment. *)
(**/**)
let d = 4
