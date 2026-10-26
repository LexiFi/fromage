(* Type definitions and type expressions which are not in the other tests:
   GADTs, [nonrec], extensions, class types, object types. *)

(* 1. GADTs. *)
type _ t1 =
    Int : int -> int t1 | Pair : 'a t1 * 'b t1 -> ('a * 'b) t1
  | Function : ('aaaaaaaaaaaa -> 'bbbbbbbbbbbbbbbbbbb) t1 * 'aaaaaaaaaaaa t1 ->
      'bbbbbbbbbbbbbbbbbbb t1

type _ t1_long =
    Constructor_with_a_long_name :
      aaaaaaaaaaaaaaaaaaaa * bbbbbbbbbbbbbbbbbbbbbbbb * cccccccccccccccccccc
  -> dddddddddddddddddddd t1_long

type _ t1_record =
    Record :
      { field_number_one : int; field_number_two : string;
        field_number_three : bool
      }
  -> int t1_record

(* 2. [nonrec]. *)
type nonrec t2 = t2 list
type nonrec t2_long =
  aaaaaaaaaaaaaaaaaaaa -> bbbbbbbbbbbbbbbbbbbbbbbb -> cccccccccccccccccccc
  -> t2_long

(* 3. Extensions of types. *)
type t3 += A3 | B3 of int
type t3 +=
    Constructor_number_one of aaaaaaaaaaaaaaaaaaaa
  | Constructor_number_two of bbbbbbbbbbbbbbbbbbbbbbbb
  | Constructor_number_three

type 'a Module_with_a_long_name.t3 += private Constructor of 'a

(* 4. Class types in type expressions. *)
type t4 = #c4 list
type 'a t4_long =
  < m : int; n : 'a -> 'a; .. > -> #class_type_with_a_long_name
  -> ('a, 'b) #other_class_type
  -> unit

(* 5. Object types. *)
type t5 =
  < method_number_one : int;
    method_number_two : aaaaaaaaaaaaaaaaaaaa -> bbbbbbbbbbbbbbbbbbbbbbbb;
    method_number_three : string
  >

type t5_method =
  < method_with_a_long_name :
      aaaaaaaaaaaaaaaaaaaa -> bbbbbbbbbbbbbbbbbbbbbbbb -> cccccccccccccccccccc
      -> dddddddddddddddddddd
  >

(* 6. Polymorphic and labelled arguments. *)
type t6 =
  aaaaaaaaaaaa:int -> ?bbbbbbbbbbbbbbbbbbb:string -> cccccccccccc:('a. 'a -> 'a)
  -> unit

type t6_poly =
  { field :
      'aaaaaaaaaaaa 'bbbbbbbbbbbbbbbbbbb.
        'aaaaaaaaaaaa -> 'bbbbbbbbbbbbbbbbbbb -> cccccccccccccccccccc
  }

(* 7. First-class module types. *)
type t7 =
  ( module
      S
        with type aaaaaaaaaaaaaaaa = int
        and type bbbbbbbbbbbbbbbbbbbbbb = string and type c = bool
  )

(* 8. Constraints. *)
type 'a t8 = 'a list
  constraint 'a = < m : int; .. >

type ('a, 'b) t8_long = ('a * 'b) list
  constraint 'a = aaaaaaaaaaaaaaaaaaaa -> bbbbbbbbbbbbbbbbbbbbbbbb
  constraint 'b = cccccccccccccccccccc
