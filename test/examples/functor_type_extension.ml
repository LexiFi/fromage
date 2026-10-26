type t = (module%a.b [@x [ 1; [ 2 ] ]] M : S) -> M.t
type u = (module%a [@x] [@y [||]] M : S) -> M.t
(* Every kind of bracket in the attributes. *)
type v =
  ( module%a
      [@x : [< `A ] * [> `B ]] [@y let x = 1 [@@z]]
      [@w [@@@z]] [@u [%e]] [@s [%%f]] M : S
  )
  -> M.t

(* Every kind of bracket in the attributes. *)
type v = (module%a [@w [@@@z]] [@u [%e]] [@s [%%f]] M : S) -> M.t
