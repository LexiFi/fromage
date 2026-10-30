(* The kinds of infix operators: see operator_classification.mli. *)

type t = Symmetric | Left_into_right | Right_into_left

let comparisons = [ "="; "<>"; "=="; "!="; "<"; ">"; "<="; ">=" ]

let table =
    List.map
      (fun text -> text, Symmetric)
      ( comparisons
      @
        [ "+"; "-"; "*"; "/"; "**"; "+."; "-."; "*."; "/.";
          "mod"; "land"; "lor"; "lxor"; "lsl"; "lsr";
          "asr"; "&&"; "||"; "&"; "or"; "^"; "@"; "::"
        ]
      )
  @
    List.map
      (fun text -> text, Left_into_right) [ "|>"; ">>="; ">>|"; ">|="; ">>" ]
  @
    List.map
      (fun text -> text, Right_into_left)
      [ "@@"; ":="; "<-"; "<<"; "<|"; "=<<" ]

(** A comparison, with [.] after it: [<.], [>=.]. *)
let is_comparison text =
  let rec core length =
    if length > 0 && text.[length - 1] = '.' then core (length - 1)
    else String.sub text 0 length
  in
  List.mem (core (String.length text)) comparisons

let classify text =
  match List.assoc_opt text table with
  | Some kind -> kind
  | None ->
      let first = text.[0]
      and last = text.[String.length text - 1]
      in
      let right = String.contains text '>'
      and left = String.contains text '<'
      in
      if is_comparison text || (left && right) then Symmetric
      else if right then Left_into_right
      else if left then Right_into_left
      else if String.starts_with ~prefix:"@@" text then Right_into_left
      else if last = '=' && first <> '=' && first <> '!' then Right_into_left
      else Symmetric
