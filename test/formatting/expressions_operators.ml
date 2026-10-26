(* The operators which are not in shared_constructs.ml: boolean operators,
   unary operators, the operators of integers and strings. *)

(* 1. [&&] and [||]. *)

(* 1a. Short. *)
let x1 = a && b || c

(* 1b. Long chains. *)
let x1_and =
  condition_number_one && condition_number_two
  && condition_number_three && condition_number_four

let x1_or =
  condition_number_one || condition_number_two
  || condition_number_three || condition_number_four

(* 1c. Mixed, with long operands. *)
let x1_mixed =
  ( condition_with_a_long_name
      argument_number_one argument_number_two argument_number_three
  )
  &&
  ( condition_with_a_long_name
      argument_number_one argument_number_two argument_number_three
    ||
    condition_with_a_long_name
      argument_number_one argument_number_two argument_number_three
  )

(* 2. Unary operators. *)
let x2 = -x + (-1) - (-y) + (-.z) +. (-1.5)
let x2_bang = !reference_with_a_long_name := !reference_with_a_long_name + 1
let x2_not =
  not
    ( condition_with_a_long_name
        argument_number_one argument_number_two argument_number_three
    )

(* 3. Operators of integers. *)
let x3 = (a lsl 2) lor (b land 0xff) lxor (c lsr 1) asr d mod e
let x3_long =
  (aaaaaaaaaaaaaaaa lsl 2) lor (bbbbbbbbbbbbbbbbbbbbbbbbb land 0xff)
  lor (ccccccccccccccccccccccc lsr 1)

(* 4. String concatenation. *)
let x4 =
  "aaaaaaaaaaaaaaaa" ^ bbbbbbbbbbbbbbbbbbbbbbbbb
  ^ "ccccccccccccccccccccccc" ^ dddddddddddddddddd ^ "e"

(* 5. Comparison of long operands. *)
let x5 =
  function_with_a_long_name
    argument_number_one argument_number_two
    argument_number_three argument_number_four
  =
  other_function_with_a_long_name
    argument_number_five argument_number_six argument_number_seven

let x5_different =
  function_with_a_long_name
    argument_number_one argument_number_two
    argument_number_three argument_number_four
  <>
  other_function_with_a_long_name
    argument_number_five argument_number_six argument_number_seven

(* 6. [::] and [@]. *)
let x6 =
  aaaaaaaaaaaaaaaa :: bbbbbbbbbbbbbbbbbbbbbbbbb
  :: ccccccccccccccccccccccc :: dddddddddddddddddd :: []

let x6_append =
  list_number_one @ list_number_two @ list_number_three
  @ list_number_four @ list_number_five
