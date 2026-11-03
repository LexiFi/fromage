(* Layouts of the formatter which do not seem to follow the principles of
   readme.md. Each item is the output of the formatter (the file is a fixed
   point of it), after a comment on the principle, the layout which would
   follow it, and where the layout comes from in the printer. The
   identifiers are made up: the file parses, but does not type. *)

(* 1. No diagonal separators: [->] after a pattern on several lines.

   The readme puts [->] in the column of the pattern, 2 columns after [|],
   alone on its line, with the expression below it:
   | looongg pattern
       pattern pattern
     ->
       expression
   The formatter puts [->] in the column of [|] instead, where it reads as
   the start of the next case ([case_after_bar] in syntax_layouts.ml:
   [below ~indent:(-2) head arrow_line]). The same for a [when] guard on
   several lines, and for an or-pattern (already discussed, postponed). *)
let readme_case x =
  match x with
  | Looongg_pattern
      (pattern_number_one, pattern_number_two, pattern_number_three)
  -> expression expression expression
  | _ -> 0

let case_guard x =
  match x with
  | Some value when
      value > some_threshold_with_a_long_name && value < another_threshold
  -> value
  | _ -> 0

let or_pattern x =
  match x with
    First_constructor | Second_constructor
    | Third_constructor | Fourth_constructor | Fifth
  -> 1
  | _ -> 0

(* 2. No diagonal separators, and similar constructs: [->] of a GADT
   constructor at the end of the line of its arguments.

   The result type is below the arguments, after [->] at the end of their
   line: a diagonal separator, unlike the arrows of any other arrow type,
   which start their lines ([val f :], then [a -> b], then [-> c]). The
   constructor is laid out as a case ([constructor_declaration] gives
   [case_after_bar] the result type as the body of the case). *)
type gadt =
    Constructor_with_a_long_name : first_argument_type * second_argument_type ->
      result_type gadt

(* 3. [begin] followed by a keyword is one unit, the parent of the rest.

   After [begin fun], [begin if] or [begin while], the body is indented by
   4 from [begin], as after [( fun]; as a unit, [begin fun x ->] would have
   its body indented by 2, as [begin match] has its cases. And [else] and
   [done] are not in the column of [if] and [while] which they close: they
   are diagonal to them ([Exp_begin] in ocst_pretty.ml: [inline left
   (nest 2 body)]). *)
let begin_fun () =
  begin fun x ->
      function_with_a_long_name
        x argument_number_one argument_number_two argument_number_three
  end

let begin_if () =
  begin if condition then
      function_with_a_long_name argument_number_one argument_number_two
    else other_function_with_a_long_name argument_number_three
  end

let begin_while () =
  begin while condition do
      function_with_a_long_name
        argument_number_one argument_number_two argument_number_three
    done
  end

(* 4. Infix operators: in a bracket, a chain packed on several lines has its
   operators in the column of its operands.

   A symmetric operator should be less indented than its operands, and the
   left operand of [|>] more indented than its right one. Outside a bracket
   a packed chain follows this (the first operand is indented); in a
   bracket, its operators are in the column of the first operand
   ([operator_chain] in syntax_layouts.ml: [packed] is not changed when
   [in_bracket]). *)
let packed_in_bracket =
  f
    ( aaaaaaaaaaaaaaaa + bbbbbbbbbbbbbbbbbbbbbbbb
      + cccccccccccccccccc + ddddddddddddd + eeeeeeeeeeeeeeeeeeeeee
    )
    x

let assert_value () =
  assert
    ( function_with_a_long_name argument_number_one argument_number_two
      = expected
    )

let packed_pipe_in_bracket =
  f
    ( xs |> List.map function_number_one
      |> List.filter function_number_two |> List.iter function_three
    )
    x

(* 5. Parent nodes less indented than their children: the right operand of
   [|>] in a bracket, in the column of the bracket.

   In a bracket, the operators of a chain on several lines are in the column
   of the bracket; the right operand of [|>], which is not indented from its
   operator, is then in the column of the bracket too, as if it was not
   inside it ([operator_chain]: [first_indent] 2 and [operand_indent] 0 for
   [Left_into_right]). *)
let pipe_in_bracket =
  f
    ( List.combine
        first_list_with_a_long_name
        (Array.to_list second_array_with_a_long_name)
    |>
    List.filter_map
      (fun (first, second) -> some_function_with_a_long_name first second)
    )
    x

(* 6. Parent nodes less indented than their children: the attribute of an
   expression in its column.

   An attribute which does not fit on the line of its expression is on the
   next line, in the column of the expression, which reads as the next part
   of an enclosing construct rather than as a part of the expression
   ([item_attributes] in ocst_pretty.ml, for [with_attributes]: [item +|
   values]). *)
let attribute =
  function_with_a_long_name argument_number_one
  [@some_attribute function_with_a_long_name argument_number_one argument_two]

(* 7. Similar constructs have the same formatting: a parenthesized
   constraint.

   [:] is in the column of the bracket for [(x : t)] ([constrained_paren])
   but in the column of the name for [~(x : t)] ([glued_paren] and
   [binding_head]). *)
let paren_constraint =
  ( some_value_with_a_long_name
  : some_long_type_name_that_is_long -> another_type
  )

let labelled_paren =
  fun
    ~(some_long_label_name
      : some_long_type_name_that_is_long -> another_long_type_name
    )
  -> body

(* 8. Other: a comment between arguments changes their packing.

   The arguments fit on one line after the function, with the comment, but
   the first one is alone on its line. *)
let comment_between_arguments =
  function_with_a_long_name
    argument_number_one
    argument_number_two (* trailing comment *) argument_three
