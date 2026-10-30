(* Applications: functions as arguments, labelled and optional arguments,
   [@@] and [|>]. *)

(* 1. A function as the last argument. *)

(* 1a. Short. *)
let () = List.iter (fun x -> print x) values

(* 1b. Long body. *)
let () =
  List.iter
    ( fun x ->
        function_with_a_long_name
          argument_number_one argument_number_two
          argument_number_three argument_number_four
    )
    values

let () =
  List.iter
    ( fun value ->
        function_with_a_long_name
          argument_number_one argument_number_two
          argument_number_three argument_number_four
        ;
        other_function_with_a_long_name
          argument_number_five argument_number_six argument_number_seven
    )
    values_with_a_long_name

(* 1c. The function is the last argument. *)
let () =
  Hashtbl.iter
    table_with_a_long_name
    ( fun key value ->
        function_with_a_long_name
          argument_number_one argument_number_two
          argument_number_three argument_number_four
    )

(* 1d. [function] as the last argument. *)
let () =
  List.map
    values_with_a_long_name
    ( function
        A ->
          function_with_a_long_name
            argument_number_one argument_number_two
            argument_number_three argument_number_four
      | B ->
          other_function_with_a_long_name
            argument_number_five argument_number_six argument_number_seven
    )

(* 2. Labelled and optional arguments. *)

(* 2a. Short. *)
let () = f ~x ~y:1 ?z ?w:(Some 2) ()

(* 2b. Long labelled arguments. *)
let () =
  f
    ~first_label:
      ( function_with_a_long_name
          argument_number_one argument_number_two
          argument_number_three argument_number_four
      )
    ~second_label:
      ( other_function_with_a_long_name
          argument_number_five argument_number_six argument_number_seven
      )

(* 2c. A labelled function. *)
let () =
  List.iter
    ~f:
      ( fun x ->
          function_with_a_long_name
            argument_number_one argument_number_two
            argument_number_three argument_number_four
      )
    values

let () =
  List.fold_left
    ~init:accumulator
    ~f:
      ( fun accumulator value ->
          function_with_a_long_name
            argument_number_one argument_number_two
            argument_number_three argument_number_four
      )
    values_with_a_long_name

(* 2d. Punned labels, many of them. *)
let () =
  function_with_a_long_name
    ~argument_number_one ~argument_number_two
    ~argument_number_three ?argument_number_four ()

(* 3. [@@]. *)

(* 3a. Short. *)
let () = print @@ f x

(* 3b. Long argument. *)
let () =
  function_with_a_long_name @@
    other_function_with_a_long_name
      argument_number_five argument_number_six argument_number_seven

(* 3c. A function after [@@]. *)
let () =
  with_resource resource_with_a_long_name @@
    fun handle ->
      function_with_a_long_name
        argument_number_one argument_number_two
        argument_number_three argument_number_four

(* 4. Pipelines with [|>]. *)

(* 4a. Short. *)
let x4 = values |> List.map f |> List.filter g

(* 4b. Long steps. *)
let x4_long =
    values_with_a_long_name
  |>
  List.map
    ( fun x ->
        function_with_a_long_name
          argument_number_one argument_number_two
          argument_number_three argument_number_four
    )
  |>
  List.filter
    ( fun x ->
        other_function_with_a_long_name
          argument_number_five argument_number_six argument_number_seven
    )
  |> List.length

(* 5. Applications nested in arguments. *)
let x5 =
  f
    ( function_with_a_long_name
        argument_number_one argument_number_two
        argument_number_three argument_number_four
    )
    ( other_function_with_a_long_name
        argument_number_five argument_number_six argument_number_seven
    )

let x5_constructor =
  Some
    ( function_with_a_long_name
        argument_number_one argument_number_two
        argument_number_three argument_number_four
    )
