(* Comments in the positions where code is laid out: between cases, between
   arguments, after [=] and [->], at the end of lines. *)

(* 1. Between cases. *)
let f1 =
  function
  (* Before the first case. *)
  | A -> 1
  (* Between cases. *)
  | B -> 2 (* At the end of a case. *)
  | C ->
      (* Before a body. *)
      3

(* 2. Between arguments. *)
let () =
  f
    argument_number_one (* after the first *) argument_number_two
    (* on its own line *)
    argument_number_three

(* 3. After [=]. *)
let x3 = (* after = *) 1
let x3_own_line =
  (* on its own line after = *)
  function_with_a_long_name
    argument_number_one argument_number_two
    argument_number_three argument_number_four

(* 4. In brackets. *)
let x4 =
  [ aaaaaaaaaaaaaaaa; (* after an element *) bbbbbbbbbbbbbbbbbbbbbbbbb;
    bbbbbbbbbbbbbbbbbbbbbbbbb; bbbbbbbbbbbbbbbbbbbbbbbbb;
    bbbbbbbbbbbbbbbbbbbbbbbbb; bbbbbbbbbbbbbbbbbbbbbbbbb;
    (* on its own line *)
    bbbbbbbbbbbbbbbbbbbbbbbbb; cccccccccccccccccccccccbbbbbbbbbbbbbbbbbbbbbbbbb;
    bbbbbbbbbbbbbbbbbbbbbbbbb;
  ]

(* 5. Before [in] and [then]. *)
let x5 =
  let a = 1 (* before in *) in
  if a = 1 (* before then *) then a else (* before the else branch *) 2

(* 6. At the end of a sequence. *)
let () = f x; (* after a separator *) g y (* at the end *)
