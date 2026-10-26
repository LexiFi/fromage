(* The types after a name in signatures (see separators_after_long_parts.ml).
   After a name, [:] ends the line: the head is a fixed number of tokens, so
   the type would not gain any room on the next line. *)

val aaaaaaaaaaaaaaaaaaaa :
  aaaaaaaaaaaaaaaaaaaa -> bbbbbbbbbbbbbbbbbbbbbbbb
  -> cccccccccccccccccccc -> dddddddddddddddddddd
  -> e

external bbbbbbbbbbbbbbbbbbbb :
  aaaaaaaaaaaaaaaaaaaa -> bbbbbbbbbbbbbbbbbbbbbbbb
  -> cccccccccccccccccccc -> dddddddddddddddddddd
  -> e
  = "primitive"

module Mmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmm :
  Ssssssssssssssssssssssss
    with type aaaaaaaaaaaaaaaa = int and type bbbbbbbbbbbbbbbbbbbbbb = int

module Mmmmmmmmmmmmmmmmmmmm2 :
  sig
    val aaaaaaaaaaaaaaaa : int
    val bbbbbbbbbbbbbbbbbbb : int
    val ccccccccc : int
  end

(* With parameters, the head of a functor is not a fixed number of tokens:
   it is laid out as the head of a binding, and [:] starts its line. *)
module F3d5
    ( X :
      sig
        val aaaaaaaaaaaaaaaa : int
        val bbbbbbbbbbbbbbbbbbb : int
        val ccccccccc : int
      end
    )
  : S
