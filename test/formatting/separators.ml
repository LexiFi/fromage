(* A separator after a multi-line element is alone on its line, in the
   column of the element: [;] in sequences, lists, arrays and records, [,] in
   tuples, in expressions and in patterns. The exception is at the end of
   this file. *)

let sequence =
  f
    aaaaaaaaaaaa bbbbbbbbbbbb cccccccccccc dddddddddddd eeeeeeeeeeee
    ffffffffffff gggggggggggg hhhhhhhhhhhh iiiiiiiiiiii
  ;
  g x;
  h y

let tuple =
  ( f
      aaaaaaaaaaaa bbbbbbbbbbbb cccccccccccc dddddddddddd eeeeeeeeeeee
      ffffffffffff gggggggggggg hhhhhhhhhhhh iiiiiiiiiiii
    ,
    g x, h y
  )

let constructor =
  Some
    ( f
        aaaaaaaaaaaa bbbbbbbbbbbb cccccccccccc dddddddddddd eeeeeeeeeeee
        ffffffffffff gggggggggggg hhhhhhhhhhhh iiiiiiiiiiii
      ,
      g x
    )

let list =
  [ f
      aaaaaaaaaaaa bbbbbbbbbbbb cccccccccccc dddddddddddd eeeeeeeeeeee
      ffffffffffff gggggggggggg hhhhhhhhhhhh iiiiiiiiiiii
  ;
    g x; h y;
  ]

let array =
  [|
    f
      aaaaaaaaaaaa bbbbbbbbbbbb cccccccccccc dddddddddddd eeeeeeeeeeee
      ffffffffffff gggggggggggg hhhhhhhhhhhh iiiiiiiiiiii
  ;
    g x
  |]

let record =
  { r with
    first =
      f
        aaaaaaaaaaaa bbbbbbbbbbbb cccccccccccc dddddddddddd eeeeeeeeeeee
        ffffffffffff gggggggggggg hhhhhhhhhhhh iiiiiiiiiiii
  ;
    second = g x; third = g x;
  }

let patterns =
  function
  | ( Aaaaaaaaaaaa
        ( bbbbbbbbbbbb, cccccccccccc, dddddddddddd, eeeeeeeeeeee,
          ffffffffffff, gggggggggggg, hhhhhhhhhhhh, iiiiiiiiiiii
        )
      ,
      x
    )
  -> 1
  | ~first:
      ( Aaaaaaaaaaaa
          ( bbbbbbbbbbbb, cccccccccccc, dddddddddddd,
            eeeeeeeeeeee, ffffffffffff, gggggggggggg, hhhhhhhhhhhh
          )
      )
    ,
    ~second, ..
  -> 2
  | [ Aaaaaaaaaaaa
        ( bbbbbbbbbbbb, cccccccccccc, dddddddddddd, eeeeeeeeeeee,
          ffffffffffff, gggggggggggg, hhhhhhhhhhhh, iiiiiiiiiiii
        )
    ;
      x
    ]
  -> 3
  | [|
      Aaaaaaaaaaaa
        ( bbbbbbbbbbbb, cccccccccccc, dddddddddddd, eeeeeeeeeeee,
          ffffffffffff, gggggggggggg, hhhhhhhhhhhh, iiiiiiiiiiii
        )
    ;
      x
    |]
  -> 4
  | { first =
        Aaaaaaaaaaaa
          ( bbbbbbbbbbbb, cccccccccccc, dddddddddddd, eeeeeeeeeeee,
            ffffffffffff, gggggggggggg, hhhhhhhhhhhh, iiiiiiiiiiii
          )
    ;
      _
    }
  -> 5
  | { first =
        Aaaaaaaaaaaa
          ( bbbbbbbbbbbb, cccccccccccc, dddddddddddd, eeeeeeeeeeee,
            ffffffffffff, gggggggggggg, hhhhhhhhhhhh, iiiiiiiiiiii
          )
    ;
      _
    }
  ->
      { first =
          Aaaaaaaaaaaa
            ( bbbbbbbbbbbb, cccccccccccc, dddddddddddd, eeeeeeeeeeee,
              ffffffffffff, gggggggggggg, hhhhhhhhhhhh, iiiiiiiiiiii
            )
      ;
        y = x
      }

(* After [)], [end] or [done] which close a value on several lines, the
   separator follows them on their line. *)
let closed () =
  begin
    function_with_a_long_name argument_number_one argument_number_two;
    function_with_a_long_name argument_number_three argument_number_four
  end;
  ( function_with_a_long_name
      argument_number_one argument_number_two
      argument_number_three argument_number_four
  );
  while condition do
    function_with_a_long_name
      argument_number_one argument_number_two
      argument_number_three argument_number_four
  done;
  for index = 1 to count do
    function_with_a_long_name
      argument_number_one argument_number_two
      argument_number_three argument_number_four
  done;
  last
