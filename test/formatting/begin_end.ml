(* [begin ... end]: one case per kind of body, short (on one line) and long
   (on several lines), and the places where [begin] is used. *)

(* 1. Empty. *)
let () = begin end

(* 2. A sequence. *)

(* 2a. Short. *)
let () = begin f x; g y end

(* 2b. Long. *)
let () =
  begin
    function_with_a_long_name argument_number_one argument_number_two;
    other_function_with_a_long_name argument_number_three
  end

(* 3. [match], [try] and [function]: [end] after the cases. *)

(* 3a. Short. *)
let () =
  begin match x with
    A -> a
  | B -> b
  end

(* 3b. Long. *)
let () =
  begin match function_with_a_long_name argument_number_one with
    A -> other_function_with_a_long_name argument_number_two
  | B -> b
  end

let () =
  begin
    try function_with_a_long_name argument_number_one argument_number_two with
      Not_found -> other_function_with_a_long_name
  end

let f =
  begin function
    A -> function_with_a_long_name argument_number_one argument_number_two
  | B -> b
  end

(* 4. A keyword on the line of [begin]. *)

(* 4a. [fun]. *)
let () =
  begin fun x ->
      function_with_a_long_name
        x argument_number_one argument_number_two argument_number_three
  end

(* 4b. [if]. *)
let () =
  begin if condition then
      function_with_a_long_name argument_number_one argument_number_two
    else other_function_with_a_long_name argument_number_three
  end

(* 4c. [while] and [for]. *)
let () =
  begin while condition do
      function_with_a_long_name
        argument_number_one argument_number_two argument_number_three
    done
  end

let () =
  begin for index = 0 to count do
      function_with_a_long_name argument_number_one argument_number_two index
    done
  end

(* 5. [let], always on the line after [begin]. *)

(* 5a. Short. *)
let () =
  begin
    let x = 1 in
    f x
  end

(* 5b. Long. *)
let () =
  begin
    let offset = skip_ext_attributes state (offset + 2) in
    is_uident (peek state offset) && peek state (offset + 1) = `COLON
  end

(* 5c. [let open], [let module], [let exception] and [let*]. *)
let () =
  begin
    let open Some_module_with_a_long_name in
    function_with_a_long_name argument_number_one argument_number_two
  end

let () =
  begin
    let module M = Some_functor_with_a_long_name (Argument_module) in
    M.function_with_a_long_name argument_number_one
  end

let () =
  begin
    let exception Exit_with_a_long_name of int in
    function_with_a_long_name argument_number_one argument_number_two
  end

let () =
  begin
    let* value = function_with_a_long_name argument_number_one in
    other_function_with_a_long_name value argument_number_two
  end

(* 6. [begin begin]: the second [begin] on the next line. *)
let () =
  begin
    begin
      function_with_a_long_name
        argument_number_one argument_number_two argument_number_three
    end
  end

(* 7. After [then] and [else]: [end] in the column of [if]. *)
let () =
  if condition then begin
    function_with_a_long_name argument_number_one;
    other_function_with_a_long_name argument_number_two
  end
  else begin
    function_with_a_long_name argument_number_three;
    other_function_with_a_long_name argument_number_four
  end

(* 8. In a sequence, in a case and as an argument. *)
let () =
  function_with_a_long_name argument_number_one;
  begin
    other_function_with_a_long_name argument_number_two;
    third_function argument_number_three
  end;
  last ()

let f x =
  match x with
    A ->
      begin
        function_with_a_long_name argument_number_one;
        other_function_with_a_long_name argument_number_two
      end
  | B -> b

let () =
  List.iter
    ( fun x ->
        begin
          function_with_a_long_name x argument_number_one;
          other_function_with_a_long_name x argument_number_two
        end
    )
    xs

(* 9. Comments. *)
let () =
  begin (* After begin. *)
    function_with_a_long_name argument_number_one argument_number_two;
    other_function argument_number_three
  end

let () =
  begin
    (* On its own line. *)
    match x with
      A -> function_with_a_long_name argument_number_one argument_number_two
    | B -> b
  end

let () =
  begin
    function_with_a_long_name argument_number_one argument_number_two;
    other_function argument_number_three (* Before end. *)
  end

(* 10. An extension and attributes. *)
let () =
  begin%ext [@attribute]
    function_with_a_long_name argument_number_one argument_number_two;
    other_function argument_number_three
  end
