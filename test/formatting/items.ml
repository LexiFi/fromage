(* Items with extensions, top-level expressions and [;;], doc comments
   (the attributes of items are in attributes.ml). *)

(* 1. Extension nodes. *)
let x3 = [%ext]
let x3_payload =
  [%ext
    function_with_a_long_name
      argument_number_one argument_number_two
      argument_number_three argument_number_four
  ]

let x3_sugar =
  let%ext x =
    function_with_a_long_name
      argument_number_one argument_number_two
      argument_number_three argument_number_four
  in
  x

let x3_match =
  match%ext x with
    A -> 1
  | B -> 2

[%%ext let x = 1]

(* 2. Top-level expressions and [;;]. *)
let () = ()
;;

function_with_a_long_name
  argument_number_one argument_number_two
  argument_number_three argument_number_four

;;
print_endline "done"

(* 3. Doc comments. *)

(** A doc comment before an item, which is long enough to take several
    lines. *)
let x5 = 1

let x5_after = 1
(** A doc comment after an item. *)

type t5 =
  | A (** A doc comment after a constructor. *)
  | B (** Another one, long enough to take several lines when it is placed
          after its constructor. *)
