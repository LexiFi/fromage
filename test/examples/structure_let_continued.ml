(* A [let ... in] expression at the start of a structure, or after [;;], is
   the first operand of the operators after it: its body ends with [;]. *)
  let {%%ext|payload|} in
  a; b;
land c

;;
  let x = 1 in
  a;
land b

;;
let x = 1
and y = 2
in
a;
,
b
