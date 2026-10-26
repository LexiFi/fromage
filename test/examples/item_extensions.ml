(* Extensions after the keyword of items made of several [and] parts: the
   extension comes before [rec] in modules and after [type] in class types. *)

module%ext rec K = A

module%ext rec K = A

module%ext rec M : S = M [@@foo]
and[@attr] N : S = N [@@bar]
and O : S = O

module%ext [@attr] rec M : S = M

module%ext [@a] [@b] rec M : S = M
and N : S = N

module (* a *) %ext (* b *) rec (* c *) M (* d *) : S = M

module%ext.id rec M : S = M

module%ext M = N

module%ext [@attr] M = N [@@foo]

module%ext F (X : S) = X

module rec M : S = M
and N : S = N

module [@attr] rec M : S = M

class%ext c = object end

class%ext c = object end
and[@attr] d = object end [@@foo]

class%ext [@attr] virtual ['a] c = object end

class type%ext ct = object end

class type%ext ct = object end
and[@attr] dt = object end [@@foo]

class type%ext [@attr] ['a] ct = object end

class type (* a *) %ext (* b *) ct = object end

let x =
  let module%ext M = N in
  M.x
