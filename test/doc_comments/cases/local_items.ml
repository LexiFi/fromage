let x =
  let (** Before M. *) module M = struct end (** After M. *) in
  let type t = A (** A. *) in
  let [@@@a] (** Marked by the attribute. *) in
  ()
