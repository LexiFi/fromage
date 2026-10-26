open! M
val unary : int list
val tuple : int * string
val binary : (int, string) result
type t = int list
type color =
    Red | Blue of int | Green of string * int
  | Record of { a : int; b : int option list }

type point = { x : int; mutable y : int }
exception E of int * string

(** {1 Docstrings} *)

(** A floating docstring. *)

(** Documentation of [documented]. *)
val documented : int
(** After [documented]. *)

val between : int (** After [between]. *) -> int

type fields =
  { first : int; (** First field. *)
    second : string;
    (** Second field. *)
    third : bool (** Third field. *)
  }

type constructors =
  | A (** Constructor A. *)
  | B of int
  (** Constructor B. *)

(**/**)

val hidden : unit

(**/**)

(** {1 Values} *)

val ( >>= ) : 'a option -> ('a -> 'b option) -> 'b option
val ( .%() ) : 'a array -> int -> 'a
val ( .%{;..}<- ) : 'a array -> int array -> 'a -> unit
val ( mod ) : int -> int -> int
val labelled : x:int -> ?y:string -> ?z :float -> unit -> unit
val poly : 'a 'b. ('a -> 'b) -> 'a list -> 'b list
val poly_argument : ('a. 'a -> 'a) -> unit
external primitive : int -> int = "caml_primitive" "caml_primitive_byte"
[@@noalloc]

val labeled_tuple : x:int * y:string -> int * z:float
val alias : ([> `A ] as 'a) -> 'a
val first_class : (module S with type t = int and type u = string) -> unit
val dependent : (module M : S) -> M.t
val local_open : M.(t) -> M.[ `A ] -> unit
val objects : < m : int; n : 'a. 'a -> 'a; .. > -> < > -> #c -> int #d -> unit
val variants :
  [ `A | `B of int & string | `C of & int ]
  -> [< `A | `B > `A ] -> [ | `A ] -> [> ]
  -> unit

val with_attributes : (int [@attribute]) -> (int [@a] [@b]) -> unit

(** {1 Types} *)

type +'a covariant
type -'a contravariant
type !'a injective
and +!'a both
and _ anonymous

type ('a, 'b) pair = 'a * 'b
  constraint 'a = int

type nonrec t = t
type abstract [@@deriving] [@@other]
type private_alias = private int
type private_variant = private A | B
type private_record = private { f : int }
type synonym = M.t = A | B of int
type open_type = ..
type empty = |
type 'a gadt =
  | Int : int gadt | Pair : 'a gadt * 'b gadt -> ('a * 'b) gadt
  | Exists : 'a. 'a gadt -> unit gadt | Record : { value : 'a } -> 'a gadt

type special = [] | (::) of int * special | () | true | false
type poly_field = { poly : 'a. 'a -> 'a; mutable counter : int [@attribute] }
type open_type += Added of int | Rebound [@@attr]
type 'a M.t += private Other
type t := int
type ext = [%extension]
type 'a functor_path = 'a F(X).t

(** {1 Exceptions} *)

exception Empty
exception With_record of { code : int }
exception Gadt : int -> exn [@@attr]

(** {1 Modules} *)

module Sub : sig val x : int end
module Alias = Sub
module Functor (X : S) (_ : T) () : S with type t = X.t
module rec Rec1 : S
and Rec2 : T

module Substituted := Sub
module type S = sig type t end
module type Abstract
module type Functor_type = functor (X : S) -> S
module type Arrow = S -> T
module type With = S with type t := int and module M = N and module type T = U
module type Of = module type of struct end
module type Of_path = module type of Sub [@attribute]
module type Sub_type := S
include S with type t = int
open M.N

(** {1 Classes} *)

class c
  : object
      inherit d
      val mutable virtual v : int
      method private virtual m : 'a. 'a -> 'a
      constraint 'a = int
    end

class virtual ['a] d
  : x:int -> ?y:int -> 'a list -> object ('self) method get : 'a end
and e : object end

class type ct = object end
class type ['a] ct2 = ['a] c

(** {1 Attributes and extensions} *)

[@@@warning]
[%%extension]
[%%extension : val x : int]
val%ext extended : int
type%ext extended_type = int
val with_type_payload : int [@@attribute : int -> int]
val with_signature_payload : int [@@attribute : val x : int]
