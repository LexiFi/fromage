let () = ()
;;
(** Before x. *)
let x = 1
(** After x. *)
and y = 2
(** After y. *)
;;
(** Before the expression. *)
let x = 1
(** After x, in the expression. *)
and y = 2
(** After y, in the expression. *)
in
x + y
;;
let%ext x = 1 (** x. *) and y = 2 in x
;;
let module M = N (** M. *) in M.x
