(* [match], [try] and [function]: the shapes of their scrutinees and cases
   which are not in shared_constructs.ml (where the cases of the three
   constructs are). *)

(* 1. The scrutinee. *)

(* 1a. Long scrutinee. *)
let x1 =
  match
    function_with_a_long_name
      argument_number_one argument_number_two
      argument_number_three argument_number_four
  with
    A -> 1
  | B -> 2

(* 1b. Tuple scrutinee. *)
let x1_tuple =
  match
    aaaaaaaaaaaaaaaaaaaa, bbbbbbbbbbbbbbbbbbbbbbbbb, cccccccccccccccccccccc
  with
    A, B, C -> 1
  | _ -> 2

(* 2. Case bodies. *)

(* 2a. A body which is a sequence. *)
let x2 =
  match x with
    A ->
      function_with_a_long_name
        argument_number_one argument_number_two
        argument_number_three argument_number_four
      ;
      other_function_with_a_long_name
        argument_number_five argument_number_six argument_number_seven
  | B -> 2

(* 2b. A body which is a [match]: it needs parentheses or [begin]. *)
let x2_match =
  match x with
    A ->
      ( match y with
          C ->
            function_with_a_long_name
              argument_number_one argument_number_two
              argument_number_three argument_number_four
        | D ->
            other_function_with_a_long_name
              argument_number_five argument_number_six argument_number_seven
      )
  | B -> 2

let x2_begin =
  match x with
    A ->
      begin match y with
        C ->
          function_with_a_long_name
            argument_number_one argument_number_two
            argument_number_three argument_number_four
      | D ->
          other_function_with_a_long_name
            argument_number_five argument_number_six argument_number_seven
      end
  | B -> 2

(* 2c. A body which is a [let ... in]. *)
let x2_let =
  match x with
    A ->
      let y =
        function_with_a_long_name
          argument_number_one argument_number_two
          argument_number_three argument_number_four
      in
      y + 1
  | B -> 2

(* 2d. A body which is [fun]. *)
let x2_fun =
  match x with
    A ->
      fun y ->
        function_with_a_long_name
          argument_number_one argument_number_two
          argument_number_three argument_number_four
  | B -> fun _ -> 2

(* 3. Patterns of cases. *)

(* 3a. Or-patterns. *)
let x3 =
  match x with
    A | B | C -> 1
  | D -> 2

(* 3b. Long or-patterns. *)
let x3_long =
  match x with
    Aaaaaaaaaaaaaaaaaaaaaaa | Bbbbbbbbbbbbbbbbbbbbbbbbbbb
    | Cccccccccccccccccccccccccc | Ddddddddddd
  -> 1
  | _ -> 2

(* 3c. An or-pattern of long patterns. *)
let x3_long_patterns =
  match x with
    Constructor
      ( aaaaaaaaaaaaaaaa, bbbbbbbbbbbbbbbbbbbbbbbbb,
        ccccccccccccccccccccccc, dddddddddddddddddd, e
      )
    | Constructor
      ( aaaaaaaaaaaaaaaa, bbbbbbbbbbbbbbbbbbbbbbbbb,
        ccccccccccccccccccccccc, dddddddddddddddddd, e
      )
  -> 1
  | _ -> 2

(* 3d. An exception case. *)
let x3_exception =
  match
    function_with_a_long_name
      argument_number_one argument_number_two
      argument_number_three argument_number_four
  with
    exception Not_found -> 0
  | exception (Failure message) -> 1
  | value -> value

(* 4. [try]. *)

(* 4a. Short. *)
let x4 = try f x with Not_found -> 0

(* 4b. Long body. *)
let x4_body =
  try
    function_with_a_long_name
      argument_number_one argument_number_two
      argument_number_three argument_number_four
  with
    Not_found -> 0

(* 4c. Body which is a sequence. *)
let x4_sequence =
  try
    function_with_a_long_name
      argument_number_one argument_number_two
      argument_number_three argument_number_four
    ;
    other_function_with_a_long_name
      argument_number_five argument_number_six argument_number_seven
  with
    Not_found -> 0
  | Failure _ -> 1

(* 5. [function] as an argument. *)
let x5 =
  List.map
    ( function
        A ->
          function_with_a_long_name
            argument_number_one argument_number_two
            argument_number_three argument_number_four
      | B ->
          other_function_with_a_long_name
            argument_number_five argument_number_six argument_number_seven
    )
    values

(* 6. A [match] as an operand and as an argument. *)
let x6 =
    ( match x with
        A -> 1
      | B -> 2
    )
  +
    ( match y with
        C -> 3
      | D -> 4
    )

let x6_argument =
  f
    ( match x with
        A ->
          function_with_a_long_name
            argument_number_one argument_number_two
            argument_number_three argument_number_four
      | B ->
          other_function_with_a_long_name
            argument_number_five argument_number_six argument_number_seven
    )
    y
