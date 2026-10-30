(* The lists in brackets, on several lines: their elements start in the same
   column on every line. A bracket glued after something ([matrix.%{],
   [M.(]) has its content on the next lines. *)

let index_user =
  matrix.%{
    aaaaaaaaaaaaaaaa; bbbbbbbbbbbbbbbbbbbbb; cccccccccccccccccccc; dddddddddd
  }

let index_paren =
  matrix.%(
    aaaaaaaaaaaaaaaa; bbbbbbbbbbbbbbbbbbbbb; cccccccccccccccccccc; dddddddddd
  )

let index_builtin =
  array.(
    aaaaaaaaaaaaaaaa + bbbbbbbbbbbbbbbbbbbbb + cccccccccccccccccccc + dddddddddd
  )

let index_string =
  string.[
    aaaaaaaaaaaaaaaa + bbbbbbbbbbbbbbbbbbbbb + cccccccccccccccccccc + dddddddddd
  ]

let index_qualified =
  matrix.M.%{aaaaaaaaaaaaaaaa; bbbbbbbbbbbbbbbbbbbbb; cccccccccccccccccccc; ddd}

let index_assign =
  matrix.%{aaaaaaaaaaaaaaaa; bbbbbbbbbbbbbbbbbbbbb; cccccccccccccccccccc}
  <- value

let open_paren =
  M.(
    aaaaaaaaaaaaaaaa, bbbbbbbbbbbbbbbbbbbbb, cccccccccccccccccccc, dddddddddddd
  )

let open_record =
  M.{
    aaaaaaaaaaaaaaaa = 1; bbbbbbbbbbbbbbbbbbbbb = 2; cccccccccccccccccccc = 3
  }

let open_list =
  M.[
    aaaaaaaaaaaaaaaa; bbbbbbbbbbbbbbbbbbbbb; cccccccccccccccccccc; dddddddddd
  ]

let open_list =
  M.[
    aaaaaaaaaaaaaaaa; bbbbbbbbbbbbbbbbbbbbb;
    cccccccccccccccccccc; dddddddddd; eeeeeeeeeeeeeeeeeeeeeeeeee
  ]

let list =
  [ aaaaaaaaaaaaaaaa; bbbbbbbbbbbbbbbbbbbbb;
    cccccccccccccccccccc; dddddddddd; eeeeeeeeeeeeeeeeeeeeeeeeee
  ]

let open_array =
  M.[| aaaaaaaaaaaaaaaa; bbbbbbbbbbbbbbbbbbbbb; cccccccccccccccccccc; dddddd |]

let open_array =
  M.[|
    aaaaaaaaaaaaaaaa; bbbbbbbbbbbbbbbbbbbbb;
    cccccccccccccccccccc; dddddd; eeeeeeeeeeeeeeeeee
  |]

let f =
  function
    M.[ aaaaaaaaaaaaaaaa; bbbbbbbbbbbbbbbbbbbbb; cccccccccccccccccccc; dddddd ]
  -> 1

let g =
  function
    M.{ aaaaaaaaaaaaaaaa; bbbbbbbbbbbbbbbbbbbbb; cccccccccccccccccccc; dddddd }
  -> 1

type closed =
  [<
    `Aaaaaaaaaaa | `Bbbbbbbbbbbbbbb
  | `Cccccccccccccc | `Dddddddddddd | `Eeeeeeeeeee
  ]

type opened =
  [>
    `Aaaaaaaaaaa | `Bbbbbbbbbbbbbbb
  | `Cccccccccccccc | `Dddddddddddd | `Eeeeeeeeeee
  ]

type exact =
  [ `Aaaaaaaaaaa | `Bbbbbbbbbbbbbbb
  | `Cccccccccccccc | `Dddddddddddd | `Eeeeeeeeeeeee
  ]

type exact_bar =
  [ | `Aaaaaaaaaaa | `Bbbbbbbbbbbbbbb
    | `Cccccccccccccc | `Dddddddddddd | `Eeeeeeeee
  ]

type obj =
  < aaaaaaaaaaaaaa : int; bbbbbbbbbbbbbbbbbb : int;
    cccccccccccccccc : int; dddddd : int
  >

type obj_open =
  < aaaaaaaaaaaaaa : int; bbbbbbbbbbbbbbbbbb : int; cccccccccccccccc : int; .. >

let override =
  {<
    aaaaaaaaaaaaaa = 1; bbbbbbbbbbbbbbbbbb = 2;
    cccccccccccccccc = 3; ddddddd = 4
  >}

let record_with =
  { base_record_value with aaaaaaaaaaaaaa = 1; bbbbbbbbbbbbbbbbbb = 2; ccccc = 3
  }

module type S =
  sig
    val f :
      (module Aaaaaaaaaaaaaaaa with type t = bbbbbbbbbbbbbb and type u = ccccc)
      -> unit
  end

let package =
  (module Aaaaaaaaaaaaaaaaaaaaa : Bbbbbbbbbbbbbbbbbb with type t = cccccccccccc)

let f
    ~(aaaaaaaaaaaaaaaaaaaa
      : bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb * cccccccccccccccccccccccccccc
    )
  = x

let f =
  function
    effect
      Aaaaaaaaaaaaaaaaaaaaaaaaaaaaaa (bbbbbbbbbbbbbbbbbbbbb, ccccccccccccccc), k
  -> 1

(* Brackets where one element is on several lines, the others short. *)

let list_one =
  [ a;
    function_with_a_long_name
      argument_number_one argument_number_two
      argument_number_three argument_number_four
  ;
    b
  ]

let array_one =
  [|
    a;
    function_with_a_long_name
      argument_number_one argument_number_two
      argument_number_three argument_number_four
  ;
    b
  |]

let record_one =
  { a = 1;
    field =
      function_with_a_long_name
        argument_number_one argument_number_two
        argument_number_three argument_number_four
  ;
    c = 3
  }

let record_with_one =
  { base with
    field =
      function_with_a_long_name
        argument_number_one argument_number_two
        argument_number_three argument_number_four
  ;
    c = 3
  }

let record_with_one =
  { ( function_with_a_long_name
        argument_number_one argument_number_two
        argument_number_three argument_number_four
    )
  with
    field =
      function_with_a_long_name
        argument_number_one argument_number_two
        argument_number_three argument_number_four
  ;
    c = 3
  }

let tuple_one =
  ( a,
    function_with_a_long_name
      argument_number_one argument_number_two
      argument_number_three argument_number_four
    ,
    b
  )

let paren_one =
  ( function_with_a_long_name
      argument_number_one argument_number_two
      argument_number_three argument_number_four
  )

let index_one =
  matrix.%{
    a;
    function_with_a_long_name
      argument_number_one argument_number_two
      argument_number_three argument_number_four
  ;
    b
  }

let open_one =
  M.[
    a;
    function_with_a_long_name
      argument_number_one argument_number_two
      argument_number_three argument_number_four
  ;
    b
  ]

let override_one =
  object
    method m =
      {<
        a = 1;
        field =
          function_with_a_long_name
            argument_number_one argument_number_two
            argument_number_three argument_number_four
      ;
        c = 3
      >}
  end

let f =
  function
    [ a;
      { field_aaaaaaaaaaaaa; field_bbbbbbbbbbbbbbbbbbb;
        field_ccccccccccccccccc; field_dddddddddddd
      }
    ;
      b
    ]
  -> 1

type record_one =
  { a : int;
    field :
      aaaaaaaaaaaaaaaaaaaa -> bbbbbbbbbbbbbbbbbbbbbbbb
      -> cccccccccccccccccccc -> dddddddddddddddddddd
      -> e
  ;
    c : int;
  }

type obj_one =
  < a : int;
    meth :
      aaaaaaaaaaaaaaaaaaaa -> bbbbbbbbbbbbbbbbbbbbbbbb
      -> cccccccccccccccccccc -> dddddddddddddddddddd
      -> e
  ;
    c : int
  >

type variant_one =
  [ `A
  | `Long of
      aaaaaaaaaaaaaaaaaaaa * bbbbbbbbbbbbbbbbbbbbbbbb
      * cccccccccccccccccccc * dddddddddddddddddddd * e
  | `C
  ]

type variant_bar_one =
  [ | `A
    | `Long of
        aaaaaaaaaaaaaaaaaaaa * bbbbbbbbbbbbbbbbbbbbbbbb
        * cccccccccccccccccccc * dddddddddddddddddddd * e
    | `C
  ]

type closed_one =
  [<
    `A
  | `Long of
      aaaaaaaaaaaaaaaaaaaa * bbbbbbbbbbbbbbbbbbbbbbbb
      * cccccccccccccccccccc * dddddddddddddddddddd * e
  | `C
  ]

type args_one =
  ( a,
    aaaaaaaaaaaaaaaaaaaa -> bbbbbbbbbbbbbbbbbbbbbbbb
    -> cccccccccccccccccccc -> dddddddddddddddddddd
    -> e
  ,
    c
  ) t

class class_args_one =
  [ a,
    aaaaaaaaaaaaaaaaaaaa -> bbbbbbbbbbbbbbbbbbbbbbbb
    -> cccccccccccccccccccc -> dddddddddddddddddddd
    -> e
  ,
    c
  ] d

(* Labelled parameters and arguments with a constraint: the name glued to
   [~(] or [?(], the constraint on the next lines, [)] in the column of [~]. *)

let f
    ?(aaaaaaaaaaaaaaaaaaaa
      : bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb
        * cccccccccccccccccccccccccccc * dddddddddddddddddddd
      = default_value
    )
    ()
  = x

let () =
  g
    ~(aaaaaaaaaaaaaaaaaaaa
      : bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb :> cccccccccccccccccccccccccccc * dddd
    )

let f
    ( a,
      ~(aaaaaaaaaaaaaaaaaaaa
        : bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb
          * cccccccccccccccccccccccccccc * dddddddddddddddddddd
      )
    )
  = x

(* [begin] with a keyword after it ([begin fun], [begin if]) is on one line
   with the keyword, but not with another [begin], which would be hidden. *)
let begin_sequence () =
  begin
    begin match value with
    | Aaaaaaaaaaaaaaaaaaaa -> function_with_a_long_name argument_number_one
    | Bbbbbbbbbbbbbbbbbbbb -> function_with_a_long_name argument_number_two
    end;
    value
  end
