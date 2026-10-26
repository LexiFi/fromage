(* [if c then a else b]: one case per shape, with subcases where each part
   is long (on several lines). *)

(* 1. [if c then a] without [else]. *)

(* 1a. Short. *)
let () = if c then a

(* 1b. Long condition. *)
let () =
  if
    condition_with_a_long_name
      argument_number_one argument_number_two argument_number_three
  then a

(* 1c. Long branch. *)
let () =
  if c then
    function_with_a_long_name
      argument_number_one argument_number_two
      argument_number_three argument_number_four

(* 1d. Branch which is a sequence. *)
let () =
  if c then begin
    function_with_a_long_name
      argument_number_one argument_number_two
      argument_number_three argument_number_four
    ;
    other_function_with_a_long_name
      argument_number_five argument_number_six argument_number_seven
  end

(* 2. [if c then a else b]. *)

(* 2a. Short. *)
let x2 = if c then a else b

(* 2b. Long condition. *)
let x2_condition =
  if
    condition_with_a_long_name
      argument_number_one argument_number_two argument_number_three
  then a
  else b

(* 2c. Long [then] branch. *)
let x2_then =
  if c then
    function_with_a_long_name
      argument_number_one argument_number_two
      argument_number_three argument_number_four
  else b

(* 2d. Long [else] branch. *)
let x2_else =
  if c then a
  else
    function_with_a_long_name
      argument_number_one argument_number_two
      argument_number_three argument_number_four

(* 2e. Both branches long. *)
let x2_both =
  if c then
    function_with_a_long_name
      argument_number_one argument_number_two
      argument_number_three argument_number_four
  else
    other_function_with_a_long_name
      argument_number_five argument_number_six argument_number_seven

(* 2f. Everything long. *)
let x2_all =
  if
    condition_with_a_long_name
      argument_number_one argument_number_two argument_number_three
  then
    function_with_a_long_name
      argument_number_one argument_number_two
      argument_number_three argument_number_four
  else
    other_function_with_a_long_name
      argument_number_five argument_number_six argument_number_seven

(* 2g. Branches which are sequences. *)
let () =
  if c then begin
    function_with_a_long_name
      argument_number_one argument_number_two
      argument_number_three argument_number_four
    ;
    other_function_with_a_long_name
      argument_number_five argument_number_six argument_number_seven
  end
  else begin
    other_function_with_a_long_name
      argument_number_five argument_number_six argument_number_seven
    ;
    function_with_a_long_name
      argument_number_one argument_number_two
      argument_number_three argument_number_four
  end

(* 3. [else if] chains. *)

(* 3a. Short. *)
let x3 = if a then 1 else if b then 2 else 3

(* 3b. Long branches. *)
let x3_long =
  if a then
    function_with_a_long_name
      argument_number_one argument_number_two
      argument_number_three argument_number_four
  else if b then
    other_function_with_a_long_name
      argument_number_five argument_number_six argument_number_seven
  else if c then
    function_with_a_long_name
      argument_number_one argument_number_two
      argument_number_three argument_number_four
  else
    other_function_with_a_long_name
      argument_number_five argument_number_six argument_number_seven

(* 3c. Long conditions. *)
let x3_conditions =
  if
    condition_with_a_long_name
      argument_number_one argument_number_two argument_number_three
  then 1
  else if
    condition_with_a_long_name
      argument_number_one argument_number_two argument_number_three
  then 2
  else 3

(* 4. Nested [if] in a [then] branch. *)
let x4 =
  if a then
    ( if b then
        function_with_a_long_name
          argument_number_one argument_number_two
          argument_number_three argument_number_four
      else
        other_function_with_a_long_name
          argument_number_five argument_number_six argument_number_seven
    )
  else c

(* 5. [if] as an argument and as an operand. *)
let x5 =
  f
    ( if c then
        function_with_a_long_name
          argument_number_one argument_number_two
          argument_number_three argument_number_four
      else b
    )
    x

let x5_operand =
  (if c then a else b)
  +
  ( if
      condition_with_a_long_name
        argument_number_one argument_number_two argument_number_three
    then 1
    else 2
  )
