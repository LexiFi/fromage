(* The constructs which share their layouts (pretty/syntax_layouts.mli), one
   case per construct, with subcases where each of their parts is long: on
   several lines, even on lines of its own. *)

(* 1. [let x = e] (binding). *)

(* 1a. Short. *)
let x1 = 1

(* 1b. Long body. *)
let x1_body =
  function_with_a_long_name
    argument_number_one argument_number_two
    argument_number_three argument_number_four

(* 1c. Long body, which is a block. *)
let x1_block =
  object
    method aaaaaaaaaaaa = 1
    method bbbbbbbbbbbbbbb = 2
    method ccccccccc = 3
  end

(* 2. [let x : t = e] (constrained binding). *)

(* 2a. Short. *)
let x2 : int = 1

(* 2b. Long type. *)
let x2_type
  : aaaaaaaaaaaaaaaaaaaa -> bbbbbbbbbbbbbbbbbbbbbbbb
    -> cccccccccccccccccccc -> dddddddddddddddddddd
    -> e
  = f

(* 2c. Long body. *)
let x2_body : int =
  function_with_a_long_name
    argument_number_one argument_number_two
    argument_number_three argument_number_four

(* 2d. Long type and long body. *)
let x2_both
  : aaaaaaaaaaaaaaaaaaaa -> bbbbbbbbbbbbbbbbbbbbbbbb
    -> cccccccccccccccccccc -> dddddddddddddddddddd
    -> e
  =
  function_with_a_long_name
    argument_number_one argument_number_two
    argument_number_three argument_number_four

(* 2e. Coercions, short. *)
let x2_coercion :> int = 1
let x2_both_coercion : int :> int = 1

(* 2f. Coercions, long. *)
let x2_long_coercion
  : aaaaaaaaaaaaaaaaaaaa -> bbbbbbbbbbbbbbbbbbbbbbbb
  :> aaaaaaaaaaaaaaaaaaaa -> bbbbbbbbbbbbbbbbbbbbbbbb
  = f

(* 2g. Coercions, with long types. *)
let x2_longer_coercion
  : aaaaaaaaaaaaaaaaaaaa -> bbbbbbbbbbbbbbbbbbbbbbbb
    -> cccccccccccccccccccc -> dddddddddddddddddddd
    -> e
  :>
    aaaaaaaaaaaaaaaaaaaa -> bbbbbbbbbbbbbbbbbbbbbbbb
    -> cccccccccccccccccccc -> dddddddddddddddddddd
    -> e
  = f

(* 3. [let f : 'a. t = e] (polymorphic binding). *)

(* 3a. Short. *)
let f3 : 'a. 'a -> 'a = fun x -> x

(* 3b. Long variables. *)
let f3_variables
  : 'aaaaaaaaaaaa 'bbbbbbbbbbbbbbb 'cccccccccccccccc
    'ddddddddddddddd 'eeeeeeeeee 'ffffffffffffffffffff
    . 'a
  = x

(* 3c. Long type. *)
let f3_type
  : 'a.
      aaaaaaaaaaaaaaaaaaaa -> bbbbbbbbbbbbbbbbbbbbbbbb
      -> cccccccccccccccccccc -> dddddddddddddddddddd
      -> e
  = f

(* 3d. Long body. *)
let f3_body : 'a. 'a =
  function_with_a_long_name
    argument_number_one argument_number_two
    argument_number_three argument_number_four

(* 4. [let (p : t) = e] and [let p = e] (pattern bindings). *)

(* 4a. Short. *)
let (x4 : int) = 1
let (x4', y4) : int * int = 1, 2

(* 4b. Long pattern. *)
let
  aaaaaaaaaaaaaaaa, bbbbbbbbbbbbbbbbbbbbbbbbb,
  ccccccccccccccccccccccc, dddddddddddddddddd, eeeeeeeeeeeee
  = value

(* 4c. Long type. *)
let (x4_type, y4_type)
  : aaaaaaaaaaaaaaaaaaaa * bbbbbbbbbbbbbbbbbbbbbbbb
    * cccccccccccccccccccc * dddddddddddddddddddd * e
  = value

(* 4d. Long body. *)
let x4_body, y4_body =
  function_with_a_long_name
    argument_number_one argument_number_two
    argument_number_three argument_number_four

(* 5. [let f : type a. t = e] (newtype binding). *)

(* 5a. Short. *)
let f5 : type a. a -> a = fun x -> x

(* 5b. Long type. *)
let f5_type
  : type a.
      aaaaaaaaaaaaaaaaaaaa -> bbbbbbbbbbbbbbbbbbbbbbbb
      -> cccccccccccccccccccc -> dddddddddddddddddddd
      -> e
  = f

(* 5c. Long body. *)
let f5_body : type a. a =
  function_with_a_long_name
    argument_number_one argument_number_two
    argument_number_three argument_number_four

(* 6. [let f x y : t = e] (function binding). *)

(* 6a. Short. *)
let f6 x y = x + y

(* 6b. Long parameters. *)
let f6_parameters
    aaaaaaaaaaaaaaaa bbbbbbbbbbbbbbbbbbbbbbbbb
    ccccccccccccccccccccccc dddddddddddddddddd eeeeeeeeeeeee
  = x

(* 6c. Long constraint. *)
let f6_constraint x
  : aaaaaaaaaaaaaaaaaaaa * bbbbbbbbbbbbbbbbbbbbbbbb
    * cccccccccccccccccccc * dddddddddddddddddddd * e
  = x

(* 6d. Long body. *)
let f6_body x y =
  function_with_a_long_name
    argument_number_one argument_number_two
    argument_number_three argument_number_four

(* 6e. Long parameters and long body. *)
let f6_both
    aaaaaaaaaaaaaaaa bbbbbbbbbbbbbbbbbbbbbbbbb
    ccccccccccccccccccccccc dddddddddddddddddd eeeeeeeeeeeee
  =
    function_with_a_long_name
      argument_number_one argument_number_two
      argument_number_three argument_number_four

(* 6f. A parameter on several lines. *)
let f6_parameter
    ( { aaaaaaaaaaaaaaaa; bbbbbbbbbbbbbbbbbbbbbbbbb;
        ccccccccccccccccccccccc; dddddddddd
      }
      as r
    )
  = r

(* 7. [let* x = e] (binding operators). *)

(* 7a. Short. *)
let f7 () =
  let* x = y in
  let+ z : int = w
  and+ v = u
  in
  x

let f7 () =
  let* x = y in
  let+
    ( { aaaaaaaaaaaaaaaa; bbbbbbbbbbbbbbbbbbbbbbbbb;
        ccccccccccccccccccccccc; dddddddddd
      }
      as r
    )
    : int
    = w
  and+ v = u
  in
  x

(* 7b. Long body. *)
let f7_body () =
  let* x =
    function_with_a_long_name
      argument_number_one argument_number_two
      argument_number_three argument_number_four
  in
  x

(* 7c. Long constraint. *)
let f7_constraint () =
  let* x
    : aaaaaaaaaaaaaaaaaaaa * bbbbbbbbbbbbbbbbbbbbbbbb
      * cccccccccccccccccccc * dddddddddddddddddddd * e
    = y
  in
  x

(* 8. Record fields [{ x = e }] and record patterns [{ x = p }]. *)

(* 8a. Short. *)
let r8 = { x = 1; y : int = 2 }
let f8 { x = a; y : int = b } = a + b

(* 8b. Long value. *)
let r8_value =
  { field =
      function_with_a_long_name
        argument_number_one argument_number_two
        argument_number_three argument_number_four
  }

(* 8c. Long pattern. *)
let f8_pattern
    { field =
        aaaaaaaaaaaaaaaa, bbbbbbbbbbbbbbbbbbbbbbbbb,
        ccccccccccccccccccccccc, dddddddddddddddddd, eeeeeeeeeeeee
    }
  = 1

(* 9. Default values [?(x = e)] and [?x:(p = e)]. *)

(* 9a. Short. *)
let f9 ?(x = 1) ?y:(z = 2) () = x + z

(* 9b. Long default value. *)
let f9_default
    ?(x =
      function_with_a_long_name
        argument_number_one argument_number_two
        argument_number_three argument_number_four
    )
    ()
  = 1

(* 9c. Long pattern. *)
let f9_pattern
    ?a:(
      ( aaaaaaaaaaaaaaaa, bbbbbbbbbbbbbbbbbbbbbbbbb,
        ccccccccccccccccccccccc, dddddddddddddddddd, eeeeeeeeeeeee
      )
        = x
    )
    ()
  = 1

(* 9d. [?x:(p : t = e)], long type. *)
let f9_label_type
    ?aaaaaaaa:(
      pattern
        : bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb
          * cccccccccccccccccccccccccccc * dddddddddddddddddddd
        = default
    )
    ()
  = 1

(* 9e. [?x:(p = e)], long default value. *)
let f9_label_default
    ?aaaaaaaa:(
      pattern =
        function_with_a_long_name
          argument_number_one argument_number_two
          argument_number_three argument_number_four
    )
    ()
  = 1

(* 9f. [?x:(p : t = e)], long pattern, type and default value. *)
let f9_label_all
    ?aaaaaaaa:(
      ( aaaaaaaaaaaaaaaa, bbbbbbbbbbbbbbbbbbbbbbbbb,
        ccccccccccccccccccccccc, dddddddddddddddddd
      )
        : bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb
          * cccccccccccccccccccccccccccc * dddddddddddddddddddd
        =
        function_with_a_long_name
          argument_number_one argument_number_two
          argument_number_three argument_number_four
    )
    ()
  = 1

(* 10. [for i = a to b]. *)

(* 10a. Short. *)
let () = for i = 1 to 10 do print_int i done

(* 10b. Long bounds. *)
let () =
  for index =
    function_with_a_long_name
      argument_number_one argument_number_two
      argument_number_three argument_number_four
    to
      function_with_a_long_name
        argument_number_one argument_number_two
        argument_number_three argument_number_four
  do
    ()
  done

(* 10c. Long stop. *)
let () =
  for index = 0
    to
      function_with_a_long_name
        argument_number_one argument_number_two
        argument_number_three argument_number_four
  do
    ()
  done

(* 10d. Long start, short stop. *)
let () =
  for index =
    function_with_a_long_name
      argument_number_one argument_number_two
      argument_number_three argument_number_four
    downto 0
  do
    ()
  done

(* 10e. [for index =] which cannot be on one line, after a comment on its own
   line: the range on the lines after [for]. *)
let () =
  for
    (* The index. *)
    index
    = function_with_a_long_name
        argument_number_one argument_number_two argument_number_three
    to 10
  do
    ()
  done

(* 11. The fields of objects: [val], [method] and [constraint]. *)

(* 11a. Short. *)
let o11 =
  object
    val x = 1
    val y : int = 2
    method m = x
    method n z = z
    method p : 'a. 'a -> 'a = fun x -> x
    constraint 'a = int
  end

(* 11b. Long bodies. *)
let o11_bodies =
  object
    val value =
      function_with_a_long_name
        argument_number_one argument_number_two
        argument_number_three argument_number_four
    val typed : int =
      function_with_a_long_name
        argument_number_one argument_number_two
        argument_number_three argument_number_four
    method meth =
      function_with_a_long_name
        argument_number_one argument_number_two
        argument_number_three argument_number_four
    method with_parameters
        aaaaaaaaaaaaaaaa bbbbbbbbbbbbbbbbbbbbbbbbb
        ccccccccccccccccccccccc dddddddddddddddddd eeeeeeeeeeeee
      = 1
    method poly : 'a. 'a -> 'a =
      function_with_a_long_name
        argument_number_one argument_number_two
        argument_number_three argument_number_four
  end

(* 11c. Long types. *)
let o11_types =
  object
    val typed
      : aaaaaaaaaaaaaaaaaaaa -> bbbbbbbbbbbbbbbbbbbbbbbb
        -> cccccccccccccccccccc -> dddddddddddddddddddd
        -> e
      = f
    method poly
      : 'a.
          aaaaaaaaaaaaaaaaaaaa -> bbbbbbbbbbbbbbbbbbbbbbbb
          -> cccccccccccccccccccc -> dddddddddddddddddddd
          -> e
      = f
    constraint 'a =
      aaaaaaaaaaaaaaaaaaaa -> bbbbbbbbbbbbbbbbbbbbbbbb
      -> cccccccccccccccccccc -> dddddddddddddddddddd
      -> e
  end

(* 12. Classes: [class c x = e], [class c : t = e], and their type
   parameters. *)

(* 12a. Short. *)
class c12 = object end
class c12_parameters x y = object end
class c12_constraint : object end = object end

(* 12b. Long type parameters. *)
class
  [ 'aaaaaaaaaaaaaa, 'bbbbbbbbbbbbbbbbbbb, 'cccccccccccccccc,
    'ddddddddddd, 'eeeeeeeee, 'ffffffffffffffff
  ] c12_type_parameters
  = object end

(* 12c. Long parameters. *)
class c12_long_parameters
    aaaaaaaaaaaaaaaa bbbbbbbbbbbbbbbbbbbbbbbbb
    ccccccccccccccccccccccc dddddddddddddddddd eeeeeeeeeeeee
  = object end

(* 12d. Long body. *)
class c12_body =
  function_with_a_long_name
    argument_number_one argument_number_two
    argument_number_three argument_number_four

(* 12e. Long body, which is a block. *)
class c12_block x =
  object
    method aaaaaaaaaaaa = 1
    method bbbbbbbbbbbbbbb = 2
    method ccccccccc = 3
  end

(* 12f. Long constraint. *)
class c12_long_constraint
  : aaaaaaaaaaaaaaaaaaaa -> bbbbbbbbbbbbbbbbbbbbbbbb
    -> cccccccccccccccccccc -> dddddddddddddddddddd -> e
    -> object end
  = f

(* 12g. Long parameters and long body, which is a block. *)
class c12_both
    aaaaaaaaaaaaaaaa bbbbbbbbbbbbbbbbbbbbbbbbb
    ccccccccccccccccccccccc dddddddddddddddddd eeeeeeeeeeeee
  =
    object
      method aaaaaaaaaaaa = 1
      method bbbbbbbbbbbbbbb = 2
      method ccccccccc = 3
    end

(* 12h. [class type c = object ... end]. *)
class type ct12 =
  object
    method aaaaaaaaaaaa : int
    method bbbbbbbbbbbbbbb : int
    method ccccccccccccccc : int
  end

(* 13. Type declarations: [type t = u], [type t = u = A | B], [type t =
   private u], [constraint t = u]. *)

(* 13a. Short. *)
type t13 = int
type t13_equation = t13 = A | B
type t13_private = private int
type 'a t13_constraint = 'a
  constraint 'a = int

(* 13b. Long head. *)
type
  ( 'aaaaaaaaaaaaaa, 'bbbbbbbbbbbbbbbbbbb, 'cccccccccccccccc,
    'ddddddddddd, 'eeeeeeeee, 'ffffffffffffffff
  ) t13_head
  = int

(* 13c. Long definition. *)
type t13_definition =
  aaaaaaaaaaaaaaaaaaaa -> bbbbbbbbbbbbbbbbbbbbbbbb
  -> cccccccccccccccccccc -> dddddddddddddddddddd
  -> e

(* 13d. Long head and long definition. *)
type
  ( 'aaaaaaaaaaaaaa, 'bbbbbbbbbbbbbbbbbbb, 'cccccccccccccccc,
    'ddddddddddd, 'eeeeeeeee, 'ffffffffffffffff
  ) t13_both
  =
  aaaaaaaaaaaaaaaaaaaa -> bbbbbbbbbbbbbbbbbbbbbbbb
  -> cccccccccccccccccccc -> dddddddddddddddddddd
  -> e

(* 13e. Long equation. *)
type t13_long_equation
  = ( aaaaaaaaaaaaaaaaaaaaaa, bbbbbbbbbbbbbbbbbbbbbbbbbb,
      cccccccccccccccccccccccccc, ddddddddddd
    ) t
  = A | B

(* 13e1.5. Very long equation. *)
type t13_long_equation
  = ( aaaaaaaaaaaaaaaaaaaaaa,
      ( aaaaaaaaaaaaaaaaaaaaaa, bbbbbbbbbbbbbbbbbbbbbbbbbb,
        cccccccccccccccccccccccccc, ddddddddddd
      ) t
    ,
      ( aaaaaaaaaaaaaaaaaaaaaa, bbbbbbbbbbbbbbbbbbbbbbbbbb,
        cccccccccccccccccccccccccc,
        ( aaaaaaaaaaaaaaaaaaaaaa, bbbbbbbbbbbbbbbbbbbbbbbbbb,
          cccccccccccccccccccccccccc, ddddddddddd
        ) MyModule.Submodule.tttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttt
      ) t
    ,
      ddddddddddd
    ) t
  = A | B

(* 13e2. Long equation, then a record. *)
type t13_equation_record
  = ( aaaaaaaaaaaaaaaaaaaaaa, bbbbbbbbbbbbbbbbbbbbbbbbbb,
      cccccccccccccccccccccccccc, ddddddddddd
    ) t
  = { x : int; y : int }

(* 13e3. Long equation, then [private] and constructors. *)
type t13_equation_private
  = ( aaaaaaaaaaaaaaaaaaaaaa, bbbbbbbbbbbbbbbbbbbbbbbbbb,
      cccccccccccccccccccccccccc, ddddddddddd
    ) t
  = private A | B

(* 13e4. Long equation, then [..]. *)
type t13_equation_open
  = ( aaaaaaaaaaaaaaaaaaaaaa, bbbbbbbbbbbbbbbbbbbbbbbbbb,
      cccccccccccccccccccccccccc, ddddddddddd
    ) t
  = ..

(* 13e5. Long equation, then a long record. *)
type t13_equation_long_record
  = ( aaaaaaaaaaaaaaaaaaaaaa, bbbbbbbbbbbbbbbbbbbbbbbbbb,
      cccccccccccccccccccccccccc, ddddddddddd
    ) t
  =
  { aaaaaaaaaaaaaaaaaaaa : int; bbbbbbbbbbbbbbbbbbbbbbbbb : string;
    cccccccccccccccccccc : float
  }

(* 13f. Long private definition. *)
type t13_long_private =
  private
  aaaaaaaaaaaaaaaaaaaa -> bbbbbbbbbbbbbbbbbbbbbbbb
  -> cccccccccccccccccccc -> dddddddddddddddddddd
  -> e

(* 13g. Long constraint. *)
type 'a t13_long_constraint = 'a
  constraint 'a =
    aaaaaaaaaaaaaaaaaaaa -> bbbbbbbbbbbbbbbbbbbbbbbb
    -> cccccccccccccccccccc -> dddddddddddddddddddd
    -> e

(* 14. The constraints of [with]. *)

(* 14a. Short. *)
module type S14 = S with type t = int and module M = N and module type T = U

(* 14b. Long type. *)
module type S14_type =
  S
    with type t =
      aaaaaaaaaaaaaaaaaaaa -> bbbbbbbbbbbbbbbbbbbbbbbb
      -> cccccccccccccccccccc -> dddddddddddddddddddd
      -> e

(* 14c. Long module type. *)
module type S14_module_type =
  S
    with module type T =
      sig
        val aaaaaaaaaaaaaaaa : int
        val bbbbbbbbbbbbbbbbbbb : int
        val c : int
      end

(* 14d. Substitutions. *)
module type S14_substitutions =
  S
    with type t :=
      aaaaaaaaaaaaaaaaaaaa -> bbbbbbbbbbbbbbbbbbbbbbbb
      -> cccccccccccccccccccc -> dddddddddddddddddddd
      -> e
    and module M := Bbbbbbbbbbbbb

(* 15. [external f : t = "primitive"]. *)

(* 15a. Short. *)
external f15 : int -> int = "f15"

(* 15b. Long type. *)
external f15_type :
  aaaaaaaaaaaaaaaaaaaa -> bbbbbbbbbbbbbbbbbbbbbbbb
  -> cccccccccccccccccccc -> dddddddddddddddddddd
  -> e
  = "f"

(* 15c. Long primitives. *)
external f15_primitives : int -> int =
  "aaaaaaaaaaaaaaaaaaaa" "bbbbbbbbbbbbbbbbbbbbbbb"
  "cccccccccccccccccccccc" "dddddddddddddddddddd"

(* 16. [exception E = F] (rebinding). *)

(* 16a. Short. *)
exception E16 = F16

(* 16b. A path on a line of its own (a path is never on several lines). *)
exception E16_long_path =
  Aaaaaaaaaaaaaaaaaaaaaaaaaa.Bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb.Ccccccccccc

(* 17. Modules: [module M = e], [module M : S = e], [module F (X : S) = e]
   and [module type S = t]. *)

(* 17a. Short. *)
module M17 = struct end
module M17_constraint : S = struct end
module F17 (X : S) = struct end
module type S17 = sig end

(* 17b. Long parameters. *)
module F17_parameters
    (Aaaaaaaaaaaa : S) (Bbbbbbbbbbbbbbbbbb : S)
    (Ccccccccccccccc : S) (Dddddddddddddddd : S) (Eeeeeeeeeeeee : S)
  = struct end

(* 17c. Long body, which is a block. *)
module M17_block =
  struct
    let aaaaaaaaaaaaaaaa = 1
    let bbbbbbbbbbbbbbbbbbb = 2
    let cccccccccccc = 3
  end

(* 17d. Long body, which is not a block. *)
module M17_application =
  Ffffffffffffffffff
    (Aaaaaaaaaaaaaaaaaaaa) (Bbbbbbbbbbbbbbbbbbbbbb)
    (Cccccccccccccccccc) (Ddddddddddddddd)

(* 17e. Long parameters and long body. *)
module F17_both
    (Aaaaaaaaaaaa : S) (Bbbbbbbbbbbbbbbbbb : S)
    (Ccccccccccccccc : S) (Dddddddddddddddd : S) (Eeeeeeeeeeeee : S)
  =
  Ffffffffffffffffff
    (Aaaaaaaaaaaaaaaaaaaa) (Bbbbbbbbbbbbbbbbbbbbbb)
    (Cccccccccccccccccc) (Ddddddddddddddd)

(* 17f. Long parameters and long body, which is a block. *)
module F17_block
    (Aaaaaaaaaaaa : S) (Bbbbbbbbbbbbbbbbbb : S)
    (Ccccccccccccccc : S) (Dddddddddddddddd : S) (Eeeeeeeeeeeee : S)
  =
  struct
    let aaaaaaaaaaaaaaaa = 1
    let bbbbbbbbbbbbbbbbbbb = 2
    let cccccccccccc = 3
  end

(* 17g. Long constraint. *)
module M17_long_constraint
  : Ssssssssssssssssssss
      with type aaaaaaaaaaaaa = int and type bbbbbbbbbbbbbbb = int
  = struct end

(* 17h. Long module type. *)
module type S17_long =
  sig
    val aaaaaaaaaaaaaaaa : int
    val bbbbbbbbbbbbbbbbbbb : int
    val c : int
  end

(* 18. The cases of [match], [function] and [try]. *)

(* 18a. Short. *)
let f18 =
  function
    A -> 1
  | B -> 2

(* 18b. Long pattern. *)
let f18_pattern =
  function
    Constructor
      ( aaaaaaaaaaaaaaaa, bbbbbbbbbbbbbbbbbbbbbbbbb,
        ccccccccccccccccccccccc, dddddddddddddddddd, eeeeeeeeeeeee
      )
  -> 1
  | _ -> 2

(* 18c. Long guard. *)
let f18_guard =
  function
    x when
      function_with_a_long_name
        argument_number_one argument_number_two
        argument_number_three argument_number_four
  -> 1
  | _ -> 2

(* 18d. Long body. *)
let f18_body =
  function
    A ->
      function_with_a_long_name
        argument_number_one argument_number_two
        argument_number_three argument_number_four
  | B -> 2

(* 18e. Long pattern and long body. *)
let f18_both x =
  match x with
    Constructor
      ( aaaaaaaaaaaaaaaa, bbbbbbbbbbbbbbbbbbbbbbbbb,
        ccccccccccccccccccccccc, dddddddddddddddddd, eeeeeeeeeeeee
      )
  ->
      function_with_a_long_name
        argument_number_one argument_number_two
        argument_number_three argument_number_four
  | _ -> 2

(* 18f. [try], long body. *)
let f18_try () =
  try
    function_with_a_long_name
      argument_number_one argument_number_two
      argument_number_three argument_number_four
  with
    Not_found ->
      function_with_a_long_name
        argument_number_one argument_number_two
        argument_number_three argument_number_four

(* 19. The constructors of GADTs, after a bar and in exceptions. *)

(* 19a. Short. *)
type _ t19 = A : int -> int t19 | B : string -> string t19
exception E19 : int -> exn

(* 19b. Long arguments. *)
type _ t19_arguments =
    Constructor :
      aaaaaaaaaaaaaaaaaaaa * bbbbbbbbbbbbbbbbbbbbbbbb
      * cccccccccccccccccccc * dddddddddddddddddddd * e
  -> int t19_arguments

exception E19_arguments :
  aaaaaaaaaaaaaaaaaaaa * bbbbbbbbbbbbbbbbbbbbbbbb
  * cccccccccccccccccccc * dddddddddddddddddddd * e
  -> exn

(* 19c. Long result. *)
type _ t19_result =
    Constructor : int ->
      ( 'aaaaaaaaaaaaaa, 'bbbbbbbbbbbbbbbbbbb, 'cccccccccccccccc,
        'ddddddddddd, 'eeeeeeeee, 'ffffffffffffffff
      ) t19_result

(* 19d. Long arguments and long result. *)
type _ t19_both =
    Constructor :
      aaaaaaaaaaaaaaaaaaaa * bbbbbbbbbbbbbbbbbbbbbbbb
      * cccccccccccccccccccc * dddddddddddddddddddd * e
  ->
      ( 'aaaaaaaaaaaaaa, 'bbbbbbbbbbbbbbbbbbb, 'cccccccccccccccc,
        'ddddddddddd, 'eeeeeeeee, 'ffffffffffffffff
      ) t19_both

(* 20. [fun x y -> e]. *)

(* 20a. Short. *)
let f20 = fun x y -> x + y

(* 20b. Long parameters. *)
let f20_parameters =
  fun
    aaaaaaaaaaaaaaaa bbbbbbbbbbbbbbbbbbbbbbbbb
    ccccccccccccccccccccccc dddddddddddddddddd eeeeeeeeeeeee
  -> 1

(* Even longer *)
let f20_parameters =
  fun
    aaaaaaaaaaaaaaaa bbbbbbbbbbbbbbbbbbbbbbbbb ccccccccccccccccccccccc
    dddddddddddddddddd eeeeeeeeeeeee ffffffffff ggggggggggggg
    hhhhhhhhhhhh iiiiiiiii jjjjjjjjjjj kkkkkkkkkkk llllllllll mmmmmmmmm
  -> 1

(* Can be grouped with empty lines *)
let f20_parameters =
  fun
    aaaaaaaaaaaaaaaa bbbbbbbbbbbbbbbbbbbbbbbbb
    ccccccccccccccccccccccc dddddddddddddddddd

    eeeeeeeeeeeee ffffffffff ggggggggggggg

    hhhhhhhhhhhh iiiiiiiii jjjjjjjjjjj kkkkkkkkkkk llllllllll mmmmmmmmm
  -> 1

(* 20c. Long constraint. *)
let f20_constraint =
  fun
    x
    : ( aaaaaaaaaaaaaaaaaaaa * bbbbbbbbbbbbbbbbbbbbbbbb
        * cccccccccccccccccccc * dddddddddddddddddddd * e
      )
  -> x

(* 20d. Long body. *)
let f20_body =
  fun x y ->
    function_with_a_long_name
      argument_number_one argument_number_two
      argument_number_three argument_number_four

(* 20e. Long parameters and long body. *)
let f20_both =
  fun
    aaaaaaaaaaaaaaaa bbbbbbbbbbbbbbbbbbbbbbbbb
    ccccccccccccccccccccccc dddddddddddddddddd eeeeeeeeeeeee
  ->
    function_with_a_long_name
      argument_number_one argument_number_two
      argument_number_three argument_number_four

(* 20f. As an argument, long body. *)
let () =
  List.iter
    ( fun x ->
        function_with_a_long_name
          argument_number_one argument_number_two
          argument_number_three argument_number_four
    )
    list

(* 21. [functor (X : S) -> M]. *)

(* 21a. Short. *)
module F21 = functor (X : S) -> struct end

(* 21b. Long parameters. *)
module F21_parameters =
  functor
    (Aaaaaaaaaaaa : S) (Bbbbbbbbbbbbbbbbbb : S)
    (Ccccccccccccccc : S) (Dddddddddddddddd : S) (Eeeeeeeeeeeee : S)
  -> struct end

(* 21c. Long body, which is a block. *)
module F21_block =
  functor (X : S) ->
    struct
      let aaaaaaaaaaaaaaaa = 1
      let bbbbbbbbbbbbbbbbbbb = 2
      let cccccccccccc = 3
    end

(* 21d. Long body, which is not a block. *)
module F21_application =
  functor (X : S) ->
    Ffffffffffffffffff
      (Aaaaaaaaaaaaaaaaaaaa) (Bbbbbbbbbbbbbbbbbbbbbb)
      (Cccccccccccccccccc) (Ddddddddddddddd)

(* 21e. Long parameters and long body. *)
module F21_both =
  functor
    (Aaaaaaaaaaaa : S) (Bbbbbbbbbbbbbbbbbb : S)
    (Ccccccccccccccc : S) (Dddddddddddddddd : S) (Eeeeeeeeeeeee : S)
  ->
    Ffffffffffffffffff
      (Aaaaaaaaaaaaaaaaaaaa) (Bbbbbbbbbbbbbbbbbbbbbb)
      (Cccccccccccccccccc) (Ddddddddddddddd)

(* 22. Functor types: [functor (X : S) -> T] and [(X : S) -> T]. *)

(* 22a. Short. *)
module type S22 = functor (X : S) -> sig end
module type S22_short = (X : S) -> S

(* 22b. Long parameters. *)
module type S22_parameters =
  functor
    (Aaaaaaaaaaaa : S) (Bbbbbbbbbbbbbbbbbb : S)
    (Ccccccccccccccc : S) (Dddddddddddddddd : S) (Eeeeeeeeeeeee : S)
  -> sig end

(* 22c. Long body. *)
module type S22_body =
  functor (X : S) ->
    sig
      val aaaaaaaaaaaaaaaa : int
      val bbbbbbbbbbbbbbbbbbb : int
      val c : int
    end

(* 22d. [S -> T], long operands. *)
module type S22_arrow =
  ( Ssssssssssssssssssssssss
      with type aaaaaaaaaaaaaaaa = int and type bbbbbbbbbbbbbbbbbbbbbb = int
      and type bbbbbbbbbbbbbbbbbbbbbb = int
      and type bbbbbbbbbbbbbbbbbbbbbb = int
  )
  ->
  ( Tttttttttttttttttttttt
      with type aaaaaaaaaaaaaaaa = int and type bbbbbbbbbbbbbbbbbbbbbb = int
  )

(* 23. [fun x -> e] in classes. *)

(* 23a. Short. *)
class c23 = fun x -> object end

(* 23b. Long parameters. *)
class c23_parameters =
  fun
    aaaaaaaaaaaaaaaa bbbbbbbbbbbbbbbbbbbbbbbbb
    ccccccccccccccccccccccc dddddddddddddddddd eeeeeeeeeeeee
  -> object end

(* 23c. Long body, which is a block. *)
class c23_block =
  fun x ->
    object
      method aaaaaaaaaaaa = 1
      method bbbbbbbbbbbbbbb = 2
      method ccccccccc = 3
    end

(* 23d. Long body, which is not a block. *)
class c23_application =
  fun x ->
    function_with_a_long_name
      argument_number_one argument_number_two
      argument_number_three argument_number_four

(* 24. Chains of operators in expressions, and operators as names. *)

(* 24a. Short. *)
let x24 = a + b * c = d && e
let equal24 = ( = )

(* 24b. Long chain. *)
let x24_chain =
  aaaaaaaaaaaaaaaa + bbbbbbbbbbbbbbbbbbbbbbbb + cccccccccccccccccc
  + ddddddddddddd + eeeeeeeeeeeeeeeeeeeeee + f

(* 24c. Long operand. *)
let x24_operand =
  aaaaaaaaaaaaaaaa
  +
  function_with_a_long_name
    argument_number_one argument_number_two
    argument_number_three argument_number_four
  + c

(* 24d. Right associative. *)
let x24_right =
  aaaaaaaaaaaaaaaa @ bbbbbbbbbbbbbbbbbbbbbbbb @ cccccccccccccccccc
  @ ddddddddddddddddd @ eeeeeeeeeeeeeeeeeeeeee @ f

(* 24e. Comparison with [=], long operands. *)
let x24_equal =
  function_with_a_long_name
    argument_number_one argument_number_two
    argument_number_three argument_number_four
  =
  function_with_a_long_name
    argument_number_one argument_number_two
    argument_number_three argument_number_four

(* 25. Lists built with [::]. *)

(* 25a. Short. *)
let l25 = a :: b :: c

(* 25b. Long. *)
let l25_long =
  aaaaaaaaaaaaaaaa :: bbbbbbbbbbbbbbbbbbbbbbbb :: cccccccccccccccccc
  :: ddddddddddddddd :: eeeeeeeeeeeeeeeeeeeeeeee :: f

(* 25c. Long element. *)
let l25_element =
  aaaaaaaaaaaaaaaa
  ::
  function_with_a_long_name
    argument_number_one argument_number_two
    argument_number_three argument_number_four
  :: c

(* 26. Arrow types. *)

(* 26a. Short. *)
type t26 = a -> b -> c

(* 26b. Long. *)
type t26_long =
  aaaaaaaaaaaaaaaaaaaa -> bbbbbbbbbbbbbbbbbbbbbbbb
  -> cccccccccccccccccccc -> dddddddddddddddddddd
  -> e

(* 26c. Long domain. *)
type t26_domain =
  ( 'aaaaaaaaaaaaaa, 'bbbbbbbbbbbbbbbbbbb, 'cccccccccccccccc,
    'ddddddddddd, 'eeeeeeeee, 'ffffffffffffffff
  ) t
  -> e

(* 26d. Labelled. *)
type t26_labelled =
  label_one:aaaaaaaaaaaaaaaa -> ?label_two:bbbbbbbbbbbbbbbbbbbbbbbb
  -> cccccccccccccc -> ddddddddddddddddddd
  -> e

(* 26e. Functor parameters. *)
type t26_functor =
  ( module Mmmmmmmmmmmmmmmmmmm :
      Ssssssssssssssssssss with type t = int and type u = int
  )
  -> Mmmmmmmmmmmmmmmmmmm.t
  -> unit

type t26_functor =
  ( module Mmmmmmmmmmmmmmmmmmm :
      Ssssssssssssssssssss
        with type t = int and type u = int
        and type u = int and type u = int and type u = int
  )
  -> Mmmmmmmmmmmmmmmmmmm.t
  -> unit

(* 27. Arrow class types. *)

(* 27a. Short. *)
class c27 : int -> object end = fun _ -> object end

(* 27b. Long. *)
class c27_long
  : aaaaaaaaaaaaaaaaaaaa -> bbbbbbbbbbbbbbbbbbbbbbbb
    -> cccccccccccccccccccc -> dddddddddddddddddddd -> e
    -> object end
  = f

(* 27c. Long, with a block. *)
class c27_block
  : aaaaaaaaaaaaaaaaaaaa -> bbbbbbbbbbbbbbbbbbbbbbbb
    -> cccccccccccccccccccc -> dddddddddddddddddddd -> e
    ->
    object
      method aaaaaaaaaaaaaaaa : int
      method bbbbbbbbbbbbbbbbbbb : int
      method c : int
    end
  = f
