(* A first element [~(x : t)] of a tuple is located up to the end of the
   second element, attributes included ([$endpos] in the grammar). Found by
   the fuzzer, with [--exhaust]. *)

let _ = ~(x :> t), y [@attr]
let _ = ~(x : t), y [@attr]
let _ = ~(x : t :> u), y [@attr] [@attr]
let _ = ~(x :> t), y [@attr], z
let _ = ~(x :> t), y + z [@attr]
let _ = (~(x :> t), y [@attr])
