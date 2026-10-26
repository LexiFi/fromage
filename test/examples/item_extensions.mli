(* Extensions after the keyword of items made of several [and] parts, in
   signatures. *)

module%ext rec M : S

module%ext rec M : S [@@foo]
and[@attr] N : S [@@bar]
and O : S

module%ext [@attr] rec M : S
and N : S

module (* a *) %ext (* b *) rec (* c *) M : S

module%ext M : S

module%ext M = N

module%ext M := N

module%ext F (X : S) : S

module rec M : S
and N : S

module type%ext S = T

class%ext c : object end

class%ext c : object end
and[@attr] d : object end [@@foo]

class type%ext ct = object end

class type%ext ct = object end
and[@attr] dt = object end

class type (* a *) %ext (* b *) ct = object end
