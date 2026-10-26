(* The lists which are not packed yet, with their current layout at the width
   of 80, for a decision on each one. Packed lists put as many elements per
   line as fit, an element on several lines on lines of its own: the
   arguments of applications, the parameters of functions, tuples, chains of
   operators, or-patterns and attributes. "On one line, or one per line"
   means all the elements on one line, or each on its own line, never a mix;
   the elements may then be on one line after a bracket broken on several
   lines (cases 11 and 17). The layout errors of this file, listed in
   pretty_test.expected, are noted with their cases. *)

(* 1. The parameters of a type: packed, on the lines after [type]; then the
   definition after [=] on the next line when it fits on it, else [=] alone
   on its line and the definition on the next lines, as in let bindings (more
   variations in type_heads.ml). *)
(* Decision: this one should be packed + the other changes I made *)
type
  ( 'aaaaaaaaaaaaa, 'bbbbbbbbbbbbbbbb,
    'ccccccccccccccc, 'ddddddddddddddd, 'eeeeeeeeeee
  ) t1
  = int

type
  ( 'aaaaaaaaaaaaa, 'bbbbbbbbbbbbbbbb,
    'ccccccccccccccc, 'ddddddddddddddd, 'eeeeeeeeeee
  ) t1
  =
  ( aaaaaaaaaaaaaaaaaa, bbbbbbbbbbbbbbbbbbbb,
    cccccccccccccccccc, dddddddddddddddd
  ) result_type

(* 2. The arguments of a type constructor: on one line, or one per line. *)
(* Decision: this one should be packed *)
type t2 =
  ( aaaaaaaaaaaaaaaaaa, bbbbbbbbbbbbbbbbbbbb,
    cccccccccccccccccc, dddddddddddddddd
  ) result_type

(* 3. Arrow types: on one line, or one domain per line. *)
(* Decision: this one should be packed *)
type t3 =
  aaaaaaaaaaaaaaaa -> bbbbbbbbbbbbbbbbbbbb
  -> cccccccccccccccccc -> dddddddddddddddd
  -> e

(* 4. Tuple types: on one line, or one element per line. *)
(* Decision: this one should be packed *)
type t4 =
  aaaaaaaaaaaaaaaa * bbbbbbbbbbbbbbbbbbbb
  * cccccccccccccccccc * dddddddddddddddd * e

(* 5. The arguments of a constructor: on one line, or one per line. *)
(* Decision: this one should be packed *)
type t5 =
    Constructor of
      aaaaaaaaaaaaaa * bbbbbbbbbbbbbbbbbb
      * cccccccccccccccc * ddddddddddddddd * eeeeeeeee

(* 6. The constructors of a variant type: on one line, or one per line. *)
(* Decision: this one should be packed *)
type t6 =
    Aaaaaaaaaaa | Bbbbbbbbbbbbb | Cccccccccccc
  | Dddddddddddd | Eeeeeeeeeeeee | Fffffffffff

(* 7. The fields of a record type: on one line, or one per line. *)
(* Decision: this one should be packed *)
type t7 =
  { aaaaaaaaaa : int; bbbbbbbbbbb : int;
    cccccccccc : int; ddddddddddd : int; e : int
  }

(* 8. The tags of a polymorphic variant type: on one line, or one per line. *)
(* Decision: this one should be packed *)
type t8 =
  [ `Aaaaaaaaaa | `Bbbbbbbbbbbb | `Cccccccccccc | `Dddddddddddd | `Eeeeeeeeeeeee
  ]

(* 9. The present tags after [>], in a polymorphic variant type: separated
   by spaces, never broken. *)
(* Decision: this one should be packed. Generally a list of stuff thats never broken is clearly a bug and will result in overflowing lines. How come did fuzzing not find such cases ? *)
type t9 =
  [<
      `Aaaaaaaaaa | `Bbbbbbbbbbbb | `Cccccccccccc
    >
      `Aaaaaaaaaa `Bbbbbbbbbbbb `Cccccccccccc
      `Dddddddddddddddddd `Eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
  ]

(* 10. The types of an argument of a polymorphic variant tag, after [of &]:
   separated by spaces, never broken. *)
(* Decision: this one should be packed *)
type t10 =
  [<
      `Aaaaaaaaaa of &
        aaaaaaaaaaaaaaaa & bbbbbbbbbbbbbbbbbbbbb & ccccccccccccccccccccc
        & ccccccccccccccccccccc & ccccccccccccccccccccc & ccccccccccccccccccccc
  ]

(* 11. The fields of an object type: on one line, or one per line; here on
   one line, after [<] and before [>] alone on its line. *)
(* Decision: this one should be packed *)
type t11 =
  < aaaaaaaaaaa : int; bbbbbbbbbbbbb : int; ccccccccccc : int; ddddddddddd : int
  >

(* 12. The type variables of a polymorphic type: separated by spaces, never
   broken, even when the line is then longer than the width: an error. *)
(* Decision: this one should be packed. Also, the dot is the terminator of the whole block, if the block is multiline, the dot should be on its own line. *)
let f12
  : 'aaaaaaaaaaaa 'bbbbbbbbbbbbbbb
    'cccccccccccccccc 'ddddddddddddddd 'eeeeeeeeee
    . 'a
  = x

let f12_5
  : 'aaaaaaaaaaaa 'bbbbbbbbbbbbbbb 'cccccccccccccccc
    'ddddddddddddddd 'eeeeeeeeee 'ffffffffffff
    . 'a
  = x

(* 13. The names of [(type a b ...)]: separated by spaces, never broken, even
   when the line is then longer than the width: an error. *)
(* Decision: this one should be packed *)
let f13
    ( type
        aaaaaaaaaaaa bbbbbbbbbbbbbbb
        cccccccccccccccc ddddddddddddddd eeeeeeeeeee
    )
  = x

(* 14. The constraints of a type declaration: on one line, or on the next
   lines, separated by spaces, where each constraint may break after its
   [=]. *)
(* Thats like super ugly and contrary to the principle of the formatting. *)
(* Decision: always break before [constraint] *)
type 'a t14 = 'a
  constraint 'a = aaaaaaaaaaaaaaaaaa
  constraint 'a = bbbbbbbbbbbbbbbbb
  constraint 'a = c

(* 15. The names of the primitives of an external: on one line, or all of them
   on the next lines, separated by spaces. *)
(* Decision: this one should be packed *)
external f15 : int -> int =
  "aaaaaaaaaaaaaaaaaaaa" "bbbbbbbbbbbbbbbbbbbbbbb" "cccccccccccccccccccc"

(* 16. The parameters of a GADT constructor: on one line, or one part per
   line. *)
(* Decision: this one should be packed *)
type _ t16 =
    Constructor :
      aaaaaaaaaaaaaaaa * bbbbbbbbbbbbbbbbbbbbbb * cccccccccccccc * ddddddddddd
  -> dddddd t16
  | Constructor2 :
      aaaaaaaaaaaaaaaa * bbbbbbbbbbbbbbbbbbbbbb * cccccccccccccc * ddddddddddd
  -> dddddd t16

type _ t16_long =
  | Constructor :
      aaaaaaaaaaaaaaaa * bbbbbbbbbbbbbbbbbbbbbb
      * cccccccccccccc * ddddddddddd * eeeeeeeeeeeeeee
  -> dddddd t16_long
  | Constructor2 : aaaaaaaaaaaaaaaa * bbbbbbbbbbbbbbbbbbbbbb ->
      ( cccccccccccccc, ddddddddddd, eeeeeeeeeeeeeeeeeeeeeeeeee, fffffffffffff
      ) t16_long

(* 17. List literals and list patterns: on one line, or one element per line;
   here on one line, after [[] and before []] alone on its line. *)
(* Decision: this one should be packed *)
let l17 =
  [ aaaaaaaaaaaa; bbbbbbbbbbbbbbb; cccccccccccccccc;
    ddddddddddddddd; eeeeeeeeeeeee
  ]

let f17 =
  function
    [ aaaaaaaaaaaa; bbbbbbbbbbbbbbb; cccccccccccccccc; ddddddddddddddd; eeeee ]
  -> 1

(* 18. Array literals and array patterns: on one line, or one element per
   line, after [[|] on its line. *)
(* Decision: this one should be packed *)
let a18 =
  [|
    aaaaaaaaaaaa; bbbbbbbbbbbbbbb; cccccccccccccccc; ddddddddddddddd; eeeeeeeeee
  |]

(* 19. Record literals and record patterns: on one line, or one field per
   line. *)
(* Decision: this one should be packed *)
let r19 =
  { aaaaaaaaaaaa = 1; bbbbbbbbbbbbbbb = 2;
    cccccccccccccccc = 3; ddddddddddddddd = 4
  }

let r19 =
  { aaaaaaaaaaaa = 1; bbbbbbbbbbbbbbb = 2; cccccccccccccccc = 3;
    ddddddddddddddd = 4; eeeeeeeeeeeeeee = 5;
  }

let f19 =
  function
    { aaaaaaaaaaaa; bbbbbbbbbbbbbbb; cccccccccccccccc; ddddddddddddddd; _ } -> 1

(* 20. The instance variables of [{< ... >}]: on one line, or one per line,
   after [{<] on its line. *)
(* Decision: this one should be packed *)
let o20 =
  object
    val x = 1
    method m = {< aaaaaaaaaaaa = 1; bbbbbbbbbbbbbbb = 2; cccccccc = 3 >}
  end

(* 21. The indices of a user-defined index operator: packed. The bracket
   opened after [matrix.%] counts as opened where [matrix] starts, as for
   the local opens [M.(]. *)
(* Decision: this one should be packed *)
let i21 =
  matrix.%{
    aaaaaaaaaaaaaaaa; bbbbbbbbbbbbbbbbbbbbb; cccccccccccccccccccc; dddddddddd
  }

let i21_long =
  matrix.%{
    aaaaaaaaaaaaaaaa; bbbbbbbbbbbbbbbbbbbbb;
    cccccccccccccccccccc; dddddddddd; eeeeeeeeeeeeeeeeee
  }

(* 22. Cons patterns: on one line, or one element per line (cons expressions
   are packed, as chains of operators); here on one line. *)
(* big problem here, it should not be possible to not have a break after = if the expr is multiline. Check that globally. *)
(* Decision: this one should be packed *)
(* [::] is a separator here, it is only allowed to end a line if whats before is single line, and if whats after is multine, it should have a break right after it. Test this.*)
let f22 =
  function
    aaaaaaaaaaaa :: bbbbbbbbbbbbbbb
    :: cccccccccccccccc :: ddddddddddddd :: eeeeeeeee
  -> 1

(* 23. The parameters of a functor: separated by spaces, never broken. *)
(* Decision: this one should be packed *)
module F23 =
  functor (Aaaaaaaaaaaa : S) (Bbbbbbbbbbbbbbbbbb : S) (Ccccccccccccccc : S) ->
    struct end

(* 24. The parameters of a module binding: each one after the previous ones,
   or on the next line, indented by 4. *)
(* Super mega ugly formatting. Should obviously be packed but also should feel a lot more like [let = ...].
   New test files to show all lot of variations around this so I can modify and show you wahts expected.
    *)
module F24
    (Aaaaaaaaaaaa : S) (Bbbbbbbbbbbbbbbbbb : S) (Ccccccccccccccc : S) (D : S)
  = struct end

module F24_long
    (Aaaaaaaaaaaa : S) (Bbbbbbbbbbbbbbbbbb : S)
    (Ccccccccccccccc : S) (Dddddddddddddddd : S)
  = struct end

(* 25. The arguments of a functor application: each one after the previous
   ones, or on the next line, indented by 2. *)
(* Decision: this one should be packed, and should be treated more globably like a regular function application *)
module M25 =
  F (Aaaaaaaaaaaaaaaa) (Bbbbbbbbbbbbbbbbbbbbb) (Cccccccccccccccccccc) (Ddddddd)

(* 26. The arguments of a class application: packed, after a break after
   [=] (except before [object]), more indented than [d]. *)
(* Decision: this one should be packed.
More globably, this kind of formatting is very bad. Here [d] is the parent and
should be treated like it: every arg passed to it should be more indented.
Because you dont want to have an increase in indentation of 12, this means a break after the equal.

I thought this reasoning was clear and should have been respected throughout the codebase, how to make sure its the
case ?
*)
class c26 =
  d
    aaaaaaaaaaaaaaaaaaaa bbbbbbbbbbbbbbbbbbbbbbbbb
    cccccccccccccccccccccc ddddddd

(* 27. The type parameters of a class: as those of a type (case 1), and its
   value parameters as those of a function (more variations in classes.ml
   and class_specs.mli). *)
(* Decision: this one should be packed. Also super ugly, please tst more in a separate file that I will review. *)
class
  ['aaaaaaaaaaaaaa, 'bbbbbbbbbbbbbbbbbbb, 'cccccccccccccccc, 'ddddddddddd] c27
  = object end

class
  [ 'aaaaaaaaaaaaaa, 'bbbbbbbbbbbbbbbbbbb,
    'cccccccccccccccc, 'ddddddddddd, 'eeeeeeeee
  ] c27_long
  = object end

(* 28. The constraints of [with]: on one line, or one per line. *)
(* Decision: this one should be packed *)
module type S28 =
  S with type aaaaaaaa = int and type bbbbbbbbbbbbbb = int and type c = int

module type S28_long =
  S
    with type aaaaaaaa = int and type bbbbbbbbbbbbbb = int
    and type cccccccccc = int and type d = int

(* 29. Always one per line, even when short: the bindings of [and], the
   cases of [match] and [function], sequences, and the items of structures
   and signatures. *)

(* Why is this grouped together ? Expand and I will tell you. Lazy. *)
let x29 = 1
and y29 = 2

let f29 x =
  match x with
    A -> 1
  | B -> 2
