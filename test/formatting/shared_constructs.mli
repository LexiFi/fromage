(* The constructs which share their layouts (pretty/syntax_layouts.mli) and
   are only in signatures, one case per construct, with subcases where each
   of their parts is long (on several lines). *)

(* 1. [class c : t] (class descriptions). *)

(* 1a. Short. *)
class c1 : object end

(* 1b. Long type parameters. *)
class
  [ 'aaaaaaaaaaaaaa, 'bbbbbbbbbbbbbbbbbbb,
    'cccccccccccccccc, 'ddddddddddd, 'eeeeeeeee
  ] c1_type_parameters
  : object end

(* 1c. Long class type. *)
class c1_type
  : aaaaaaaaaaaaaaaa -> bbbbbbbbbbbbbbbbbbbbbbbb -> cccccccccccccccccc
    -> object end

(* 1d. Long head and long class type. *)
class
  [ 'aaaaaaaaaaaaaa, 'bbbbbbbbbbbbbbbbbbb,
    'cccccccccccccccc, 'ddddddddddd, 'eeeeeeeee
  ] c1_both
  : aaaaaaaaaaaaaaaa -> bbbbbbbbbbbbbbbbbbbbbbbb -> cccccccccccccccccc
    -> object end

(* 2. [module M = N] (module aliases). *)

(* 2a. Short. *)
module M2 = N2

(* 2b. A path on a line of its own (a path is never on several lines). *)
module M2_long =
  Aaaaaaaaaaaaaaaaaaaaaaaaaa.Bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb.Cccccccccccc

(* 3. [module type S = t] in signatures. *)

(* 3a. Short. *)
module type S3 = sig end

(* 3b. Long. *)
module type S3_long =
  sig
    val aaaaaaaaaaaaaaaa : int
    val bbbbbbbbbbbbbbbbbbb : int
    val c : int
  end

(* 4. [type t := u] (substitutions). *)

(* 4a. Short. *)
type t4 := int

(* 4b. Long. *)
type t4_long :=
  aaaaaaaaaaaaaaaaaaaa -> bbbbbbbbbbbbbbbbbbbbbbbb -> cccccccccccccccccccc
  -> ddd

(* 5. [external f : t = "primitive"] in signatures. *)

(* 5a. Short. *)
external f5 : int -> int = "f5"

(* 5b. Long type and long primitives. *)
external f5_long :
  aaaaaaaaaaaaaaaaaaaa -> bbbbbbbbbbbbbbbbbbbbbbbb -> cccccccccccccccccccc -> d
  =
  "aaaaaaaaaaaaaaaaaaaa" "bbbbbbbbbbbbbbbbbbbbbbb"
  "cccccccccccccccccccccc" "dddddddd"
