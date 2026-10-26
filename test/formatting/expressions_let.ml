(* [let ... in] and its relatives: [let rec ... and], [let module],
   [let exception], [let open], local opens. *)

(* 1. Chains of [let ... in]. *)

(* 1a. Short. *)
let x1 =
  let a = 1 in
  let b = 2 in
  a + b

(* 1b. Long values. *)
let x1_long =
  let a =
    function_with_a_long_name
      argument_number_one argument_number_two
      argument_number_three argument_number_four
  in
  let b =
    other_function_with_a_long_name
      argument_number_five argument_number_six argument_number_seven
  in
  a + b

(* 1c. Long final expression. *)
let x1_final =
  let a = 1 in
  function_with_a_long_name
    argument_number_one argument_number_two
    argument_number_three argument_number_four

(* 2. [let rec ... and ... in]. *)
let x2 =
  let rec even n = n = 0 || odd (n - 1)
  and odd n = n <> 0 && even (n - 1)
  in
  even 10

let x2_long =
  let rec loop_with_a_long_name accumulator =
    function
      [] -> accumulator
    | x :: rest -> loop_with_a_long_name (x :: accumulator) rest
  in
  loop_with_a_long_name [] values

(* 3. [let module]. *)
let x3 =
  let module M = Map.Make (String) in
  M.empty

let x3_long =
  let module Mmmmmmmmmmmmmmmmmmmm =
    Functor_with_a_long_name (Argument_number_one) (Argument_number_two)
  in
  Mmmmmmmmmmmmmmmmmmmm.empty

let x3_struct =
  let module M =
    struct
      let aaaaaaaaaaaaaaaa = 1
      let bbbbbbbbbbbbbbbbbbb = 2
    end
  in
  M.aaaaaaaaaaaaaaaa

(* 4. [let exception]. *)
let x4 =
  let exception Found of int in
  try
    function_with_a_long_name
      argument_number_one argument_number_two
      argument_number_three argument_number_four
  with
    Found n -> n

(* 5. [let open] and local opens. *)
let x5 =
  let open Module_with_a_long_name in
  function_with_a_long_name
    argument_number_one argument_number_two
    argument_number_three argument_number_four

let x5_local =
  Module_with_a_long_name.(
    function_with_a_long_name
      argument_number_one argument_number_two
      argument_number_three argument_number_four
  )

let x5_record =
  Module_with_a_long_name.{
    field_number_one = 1; field_number_two = 2; field_number_three = 3
  }

let x5_list =
  Module_with_a_long_name.[
    aaaaaaaaaaaaaaaa; bbbbbbbbbbbbbbbbbbbbbbbbb; ccccccccccccccccccccccc
  ]

(* 6. Binding operators. *)
let x6 =
  let* a =
    function_with_a_long_name
      argument_number_one argument_number_two
      argument_number_three argument_number_four
  in
  let+ b =
    other_function_with_a_long_name
      argument_number_five argument_number_six argument_number_seven
  and+ c =
    function_with_a_long_name
      argument_number_one argument_number_two
      argument_number_three argument_number_four
  in
  a + b + c
