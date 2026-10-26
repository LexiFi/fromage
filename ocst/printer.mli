(** Losslessly replay tokens stored in a CST. *)

val implementation : Buffer.t -> Cst.implementation -> unit
val interface : Buffer.t -> Cst.interface -> unit
val string_of_implementation : Cst.implementation -> string
val string_of_interface : Cst.interface -> string
val implementation_tokens : Cst.implementation -> Token.any_token list
(** The tokens of an implementation, in source order (ending with [EOF]). *)

val interface_tokens : Cst.interface -> Token.any_token list
