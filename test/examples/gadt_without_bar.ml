(* GADT constructors without a bar before them: [->] cannot start a line in
   the column of the bar, as in a case of [match]. *)

exception E (* a *) : int -> exn

exception Aaaaaaaaaaaaaaaaaaaaa :
  aaaaaaaaaaaaaaaaaaaaaaa * bbbbbbbbbbbbbbbbbbbbbbbbbbbbbb * ccccccccccccccccc
  -> exn

type t +=
    Bbbbbbbbbbbbbbbbbbbbbbb :
      aaaaaaaaaaaaaaaaaaaaaaa * bbbbbbbbbbbbbbbbbbbbbbbbbbbbbb * ccccccccccccc
  -> t

type t += Cccccccc : (* b *) int -> t | Dddddddd : (* c *) int -> t

let f =
  function
    exception Aaaaaaaaaaaaaaaaaaaaa (x, y, z) -> x
  | _ -> 0
