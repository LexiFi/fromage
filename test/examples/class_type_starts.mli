(* The starts of class types: a class path, class arguments, an extension,
   or the type of an argument before '->' (which may start the same way). *)
class c1 : c
class c2 : M.c [@attr]
class c3 : [int] c
class c4 : [int, string] M.c
class c5 : [ int | `A ] -> c
class c6 : [ `A ] -> c
class c7 : [ | `A ] list * int -> c
class c8 : [%ext]
class c9 : [%ext] -> c
class c10 : int list -> c
class c11 : #c -> object end
class c12 : (int, string) result -> c
class c13 : 'a -> c
class c14
  : let open M in
    c

class c15 : [ int | string ] list -> c
