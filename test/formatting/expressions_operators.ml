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

(* 7. The kinds of operators (see operator_classification.ml), with operands
   on several lines. *)

(* 7a. Left into right: the left operand indented, not the right one. *)
let x7_pipe =
    List.combine
      first_list_with_a_long_name (Array.to_list second_array_with_a_long_name)
  |>
  List.filter_map
    (fun (first, second) -> some_function_with_a_long_name first second)

(* 7b. Right into left: the right operand indented, not the left one. *)
let x7_apply =
  some_function_with_a_long_name argument_number_one @@
    other_function_with_a_long_name
      argument_number_two argument_number_three argument_number_four

(* 7b'. A chain of right into left operators after operands on one line:
   each operator at the end of its line. *)
let x7_apply_chain =
  with_resource resource_one @@
    with_other_resource resource_two @@
      fun handle ->
        function_with_a_long_name handle argument_number_one argument_number_two

(* 7b''. A left side on several lines: the operator starts its line. *)
let () =
  ( function_with_a_long_name
      argument_number_one argument_number_two argument_number_three
  ).contents
  :=
    other_function_with_a_long_name
      argument_number_four argument_number_five argument_number_six

(* 7b'''. A value on one line, which does not fit after the left side: after
   the operator at the end of the line of the left side, rather than after
   the operator at the start of the next line. *)
let () =
  reference_with_a_long_name :=
    some_function_with_a_long_name argument_number_one argument_number_two

(* 7c. In a bracket: the operators in the column of the bracket. *)
let x7_bracket =
  f
    ( function_with_a_long_name
        argument_number_one argument_number_two argument_number_three
    + other_function_with_a_long_name argument_number_four argument_number_five
    )
    x

let x7_list =
  [ function_with_a_long_name
      argument_number_one argument_number_two argument_number_three
  @ other_function_with_a_long_name argument_number_four
  ;
    []
  ]

(* 7c'. In a bracket, a chain which is the first operand of the chain of
   the bracket: indented as outside a bracket, 4 columns after it. *)
let x7_first_operand =
  (   is_operator_kind (peek state (offset + 1))
    && peek state (offset + 2) = `RPAREN
  ||
    begin match peek state (offset + 1) with
      `DOTOP _ -> true
    | _ -> false
    end
  )

let x7_first_operand_list =
  [   is_operator_kind (peek state (offset + 1))
    && peek state (offset + 2) = `RPAREN
  || other_condition_with_a_long_name argument_number_one
  ;
    last
  ]

(* 7d. A chain of a lower precedence than its operand: the operators of the
   operand indented with it. *)
let x7_precedence =
    condition_with_a_long_name argument_number_one argument_number_two
  &&
      function_with_a_long_name argument_number_one argument_number_two
    = other_function_with_a_long_name argument_number_three argument_number_four
