(** Conversion from OCST to the compiler parsetree.  Locations are derived
    while replaying CST tokens and are not retained by the CST. *)

val implementation :
  filename:string -> Cst.implementation -> Parsetree.structure

val interface : filename:string -> Cst.interface -> Parsetree.signature
