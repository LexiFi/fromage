(* The attributes after an item ([attached_attributes]): on the line of the
   item when both are on one line, else on the next lines, indented by 2,
   packed. An attribute is never glued to the last line of an item on several
   lines. *)

(* A long attribute after a constructor: on its own lines, its [\]] in the
   column of its [\[@]. The line of the string is too long, which no layout
   can shorten. *)
type t2a_constructor =
    A of int
      [@deprecated
        "this constructor is deprecated, use another one instead, see the documentation"
      ]
  | B

(* A short attribute after a record field whose type is on several lines. *)
type t2a_field =
  { aaaaaaaaaa :
      aaaaaaaaaaaaaaaaaaaa * bbbbbbbbbbbbbbbbbbbbbbbb
      * cccccccccccccccccccc * dddddddddddddddddddd * e
      [@deprecated
        "this constructor is deprecated, use another one instead, see the documentation"
      ]
  ;
    b : int
  }

(* The same after an object field. *)
type t2a_object =
  < meth :
      aaaaaaaaaaaaaaaaaaaa * bbbbbbbbbbbbbbbbbbbbbbbb
      * cccccccccccccccccccc * dddddddddddddddddddd * e
      [@deprecated
        "this constructor is deprecated, use another one instead, see the documentation"
      ]
  ;
    b : int
  >

(* The same after a polymorphic variant tag, whose types are indented after
   it, the first tag as the others after their [| ]. *)
type t2a_tag =
  [ `Aaaaaaaaaa of
      aaaaaaaaaaaaaaaaaaaa * bbbbbbbbbbbbbbbbbbbbbbbb
      * cccccccccccccccccccc * dddddddddddddddddddd * e
      [@deprecated
        "this constructor is deprecated, use another one instead, see the documentation"
      ]
  | `B
  ]

(* The same after an extension constructor. *)
type t2a_extension +=
    Aaaaaaaaaaa of
      aaaaaaaaaaaaaaaaaaaa * bbbbbbbbbbbbbbbbbbbbbbbb
      * cccccccccccccccccccc * dddddddddddddddddddd * e
      [@deprecated
        "this constructor is deprecated, use another one instead, see the documentation"
      ]

(* Several short attributes after a constructor: below it, packed. *)
type t2a_attributes =
    Aaaaaaaaaaaaaaaaaaaa of aaaaaaaaaaaaaaaaaaaa * bbbbbbbbbbbbbbbbbbbb
      [@ocaml.warning "-32"] [@ocaml.deprecated "old"] [@inline]
      [@unboxed] [@immediate] [@local] [@tail_mod_cons] [@b]
  | B

(* Attributes on one line with their item. *)
type t_short = A of int [@a] | B [@b] [@c]

(* An attribute after an expression on several lines: it is attached to the
   whole expression, so it is in its column. *)
let x_expression =
  ( function_with_a_long_name
      argument_number_one argument_number_two
      argument_number_three argument_number_four
    [@inline]
  )

(* Attributes after a type. *)
type t_type = (int [@a]) * (string [@b] [@c])

(* Attributes after items: [\[@@...\]]. *)
let x1 = 1 [@@inline]
let x1_long =
  function_with_a_long_name
    argument_number_one argument_number_two
    argument_number_three argument_number_four
[@@ocaml.warning "-32-37"]

let x1_many = 1
[@@inline] [@@ocaml.warning "-32"]
[@@deprecated "use something_with_a_long_name instead"]

type t1 = A | B [@@deriving show, eq, ord]
external f1 : int -> int = "f1" [@@noalloc]

(* Floating attributes. *)
[@@@ocaml.warning "-32"]
[@@@ocaml.text
  "a floating attribute with a long payload which does not fit on one line at all"
]
