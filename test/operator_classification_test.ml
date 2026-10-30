(* The kinds of infix operators ([Ocst_pretty.Operator_classification]): the
   operators of the standard library, the usual ones of other libraries, and
   operators made up to exercise each rule. *)

let standard_library =
  [ "+"; "-"; "*"; "/"; "**"; "+."; "-."; "*."; "/."; "mod"; "land"; "lor";
    "lxor"; "lsl"; "lsr"; "asr"; "="; "<>"; "=="; "!="; "<"; ">"; "<=";
    ">="; "&&"; "||"; "&"; "or"; "^"; "@"; "::"; "|>"; "@@"; ":="; "<-"
  ]

let other_libraries =
  [ ">>="; ">>|"; ">|="; ">>"; "<<"; "<|"; "=<<"; "<$>"; "<*>"; "<+>";
    ">=>"; "<=<"; "<."; ">=."; "=."; "^^"; "++"; "+-"; "+|"; "+/"; "%"
  ]

let made_up =
  [ "|>>"; "%>"; "=>"; "<||"; "@@@"; "+="; "^="; "==="; "!=="; "&&&"; "***" ]

let () =
  List.iter
    ( fun text ->
        let kind =
          match Ocst_pretty.Operator_classification.classify text with
          | Symmetric -> "symmetric"
          | Left_into_right -> "left into right"
          | Right_into_left -> "right into left"
        in
        Printf.printf "%-5s %s\n" text kind
    )
    (standard_library @ other_libraries @ made_up)
