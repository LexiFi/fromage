(* The blocks, [struct], [sig] and [object]: on one line only with one item
   at most; with two items or more, each item on its line. *)

(* 1. One item: on one line when it fits. *)
module M1 = struct let x = 1 end
module type S1 = sig val x : int end
class c1 = object method m = 1 end

(* 2. Two items: never on one line. *)
module M2 =
  struct
    let x = 1
    let y = 2
  end

module type S2 =
  sig
    val x : int
    val y : int
  end

class c2 =
  object
    method m = 1
    method n = 2
  end
