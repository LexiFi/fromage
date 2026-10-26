(* Lines which may be longer than the width: a doc comment after its item,
   when the item fits, stays on its line; a token longer than the width
   cannot fit anyway, and does not change the layout of the code around it. *)

type t =
  | First_constructor_with_a_long_name of int * string (** The line is too long with it. *)
  | Second_constructor (** Short. *)

let error state = syntax_error "unclosed-bracket" "a message which is longer than the width of the page, which is eighty characters wide"

let function_with_a_long_name first_argument_with_a_long_name second_argument =
  syntax_error "unclosed-bracket" "a message which is longer than the width of the page, which is eighty characters wide"
