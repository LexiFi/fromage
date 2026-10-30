(* The tokens after a part which may be on several lines: [:], [:>], [=],
   [when], [as]. The rule: a token after a part on one line follows it on
   its line, and a token after a part on several lines starts the next line,
   so that the end of a long part is easy to find, and the token is easy to
   see at the start of its line. Its column is that of the construct it
   belongs to. After a token of one character ([:], [=], [?]), a part on
   several lines is on the line of the token, its next lines indented by 2,
   as in [( ]; after a longer token ([:>]), it is on the next lines,
   indented by 2. *)

(* {1 Tokens after an expression or a pattern} *)

(* [private] qualifies the whole definition: when the definition does not
   fit on the line of [private], [private] is alone on its line, and the
   definition starts the next one, in the same column, so that [private] is
   seen at once and the definition keeps its own layout. *)
type t2b_private =
  private
  aaaaaaaaaaaaaaaaaaaa -> bbbbbbbbbbbbbbbbbbbbbbbb
  -> cccccccccccccccccccc -> dddddddddddddddddddd
  -> e

type t2b_private_record =
  private
  { aaaaaaaaaaaaaaaaaaaa : int; bbbbbbbbbbbbbbbbbbbbbbbbbbbbbb : string }

let f2c =
  function
  | Constructor
      ( aaaaaaaaaaaaaaaa, bbbbbbbbbbbbbbbbbbbbbbbbb,
        ccccccccccccccccccccccc, dddddddddddddddddd, e
      )
    when
      function_with_a_long_name
        argument_number_one argument_number_two
        argument_number_three argument_number_four
  -> 1
  | Constructor
      ( aaaaaaaaaaaaaaaa, bbbbbbbbbbbbbbbbbbbbbbbbb,
        ccccccccccccccccccccccc, dddddddddddddddddd, e
      )
  -> 4
  | _ -> 2

(* Assignments are chains of operators, whose value flows into the left
   side: a value on several lines is indented by 2, after the operator
   alone on its line after a left side on several lines, or after the
   operator at the end of the line of a left side on one line. *)
let () =
  ( function_with_a_long_name
      argument_number_one argument_number_two
      argument_number_three argument_number_four
  ).contents
  :=
    function_with_a_long_name
      argument_number_one argument_number_two
      argument_number_three argument_number_four

let () =
  ( function_with_a_long_name
      argument_number_one argument_number_two
      argument_number_three argument_number_four
  ).field
  <-
    function_with_a_long_name
      argument_number_one argument_number_two
      argument_number_three argument_number_four

let () =
  ( function_with_a_long_name
      argument_number_one argument_number_two
      argument_number_three argument_number_four
  ).(index)
  <-
    function_with_a_long_name
      argument_number_one argument_number_two
      argument_number_three argument_number_four

let o2d4 =
  object
    val mutable field = 0
    method m =
      field <-
        function_with_a_long_name
          argument_number_one argument_number_two
          argument_number_three argument_number_four
  end

(* The parentheses belong to the constraint: [:] and [:>] start their
   lines in their column, as the separators of a bracket. *)
let x2e =
  ( function_with_a_long_name
      argument_number_one argument_number_two
      argument_number_three argument_number_four
  : aaaaaaaaaaaaaaaaaaaa
  :> bbbbbbbbbbbbbbbbbbbbbbbb
  )

let x2e2 =
  ( function_with_a_long_name
      argument_number_one argument_number_two
      argument_number_three argument_number_four
  :>
    aaaaaaaaaaaaaaaaaaaa -> bbbbbbbbbbbbbbbbbbbbbbbb
    -> cccccccccccccccccccc -> dddddddddddddddddddd
    -> e
  )

module M2e3 =
  ( val
      function_with_a_long_name
        argument_number_one argument_number_two
        argument_number_three argument_number_four
  : Ssssssssssssssssssssssss with type t = int
  :> Tttttttttttttttttttttttttt with type t = int
  )

class c2f = object inherit short as self_with_a_long_name end

class c2f =
  object
    inherit
      function_with_a_long_name
        argument_number_one argument_number_two
        argument_number_three argument_number_four
      as self_with_a_long_name
  end

let x2g =
  [%ext
    ? Constructor
        ( aaaaaaaaaaaaaaaa, bbbbbbbbbbbbbbbbbbbbbbbbb,
          ccccccccccccccccccccccc, dddddddddddddddddd, e
        )
    when
      function_with_a_long_name
        argument_number_one argument_number_two
        argument_number_three argument_number_four
  ]

(* The first item of a structure payload starts the next line, as the
   items of [struct]. *)
[%%ext
  let aaaaaaaaaaaaaaaaaaaa =
    function_with_a_long_name
      argument_number_one argument_number_two
      argument_number_three argument_number_four
]

(* A block on several lines starts the next line, as the body of a
   binding. *)
include
  struct
    let aaaaaaaaaaaaaaaa = 1
    let bbbbbbbbbbbbbbbbbbb = 2
    let ccccccccc = 3
  end

open
  struct
    let aaaaaaaaaaaaaaaa = 1
    let bbbbbbbbbbbbbbbbbbb = 2
    let ccccccccc = 3
  end

(* Inside the parentheses of a functor parameter, a type on several lines
   is in the column of the name. *)
module F2h3
    ( Xxxxx :
      sig
        val aaaaaaaaaaaaaaaa : int
        val bbbbbbbbbbbbbbbbbbb : int
        val ccccccccc : int
      end
    )
  = struct end

(* {1 Types after a name} *)

let f3a
    ( Constructor
        ( aaaaaaaaaaaaaaaa, bbbbbbbbbbbbbbbbbbbbbbbbb,
          ccccccccccccccccccccccc, dddddddddddddddddd, e
        )
    : aaaaaaaaaaaaaaaaaaaa -> bbbbbbbbbbbbbbbbbbbbbbbb
      -> cccccccccccccccccccc -> dddddddddddddddddddd
      -> e
    )
  = 1

(* The field of a record is the start of its construct: [:] and [=] start
   their lines in its column. *)
let f3a2
    { aaaaaaaaaaaaaaaaaaaa
      : aaaaaaaaaaaaaaaaaaaa -> bbbbbbbbbbbbbbbbbbbbbbbb
        -> cccccccccccccccccccc -> dddddddddddddddddddd
        -> e
    }
  = 1

let x3a3 =
  { aaaaaaaaaaaaaaaaaaaa
    : aaaaaaaaaaaaaaaaaaaa -> bbbbbbbbbbbbbbbbbbbbbbbb
      -> cccccccccccccccccccc -> dddddddddddddddddddd
      -> e
    = x
  }

(* After a name, [:] ends the line: the head is a fixed number of tokens,
   so the type would not gain any room on the next line. *)
class virtual c3b =
  object
    val virtual aaaaaaaaaaaaaaaaaaaa :
      aaaaaaaaaaaaaaaaaaaa -> bbbbbbbbbbbbbbbbbbbbbbbb
      -> cccccccccccccccccccc -> dddddddddddddddddddd
      -> e
    method virtual bbbbbbbbbbbbbbbbbbbb :
      aaaaaaaaaaaaaaaaaaaa -> bbbbbbbbbbbbbbbbbbbbbbbb
      -> cccccccccccccccccccc -> dddddddddddddddddddd
      -> e
  end

class c3c =
  ( function_with_a_long_name
      argument_number_one argument_number_two
      argument_number_three argument_number_four
  : aaaaaaaaaaaaaaaaaaaa -> bbbbbbbbbbbbbbbbbbbbbbbb
    -> cccccccccccccccccccc -> dddddddddddddddddddd -> e
    -> object end
  )

module M3c2 =
  ( struct
      let aaaaaaaaaaaaaaaa = 1
      let bbbbbbbbbbbbbbbbbbb = 2
      let ccccccccc = 3
    end
  : sig
      val aaaaaaaaaaaaaaaa : int
      val bbbbbbbbbbbbbbbbbbb : int
      val ccccccccc : int
    end
  )

let x3c3 =
  ( function_with_a_long_name
      argument_number_one argument_number_two
      argument_number_three argument_number_four
  : aaaaaaaaaaaaaaaaaaaa -> bbbbbbbbbbbbbbbbbbbbbbbb
    -> cccccccccccccccccccc -> dddddddddddddddddddd
    -> e
  )
