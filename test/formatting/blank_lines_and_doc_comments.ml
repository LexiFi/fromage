(* An input left as it is (its formatted version is in the .expected file):
   its blank lines and the positions of its doc comments are part of the
   test. Formatting keeps the items each doc comment documents, keeps the
   blank lines, reduces several blank lines in a row to one, and adds a blank
   line after an item on several lines, except next to an ambiguous doc
   comment, which documents the items around it only without blank lines. *)

(* 1. Several blank lines in a row. *)
let a1 = 1



let b1 = 2


(* 2. An item on several lines, without a blank line after it. *)
let a2 = function_with_a_long_name argument_number_one argument_number_two argument_number_three argument_number_four
let b2 = 2

(* 3. Doc comments after an item, on its line. *)
let a3 = 1 (** a3. *)
let b3 = 2 (** b3. *)

(* 4. Doc comments between two items, without blank lines: ambiguous. *)
let a4 = 1 (** After a4. *) (** Between. *) (** Before b4. *)
let b4 = 2

(* 5. An ambiguous doc comment after an item on several lines. *)
let a5 = function_with_a_long_name argument_number_one argument_number_two argument_number_three argument_number_four (** a5 and b5. *)
let b5 = 2

(* 6. Doc comments in a group of [and] items. *)
let rec a6 = fun x -> b6 x (** a6. *)
and b6 = fun x -> a6 x (** b6. *)
let c6 = 3
