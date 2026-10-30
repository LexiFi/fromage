(** The kinds of infix operators, which have different layouts on several
    lines (see [Syntax_layouts.operator_chain]): whether their operands have
    the same role, or one of them flows into the other. *)

type t =
  | Symmetric
  (** The operands have the same role: [a + b], [a = b], [a && b], [a ^ b],
      [x :: l]. *)
  | Left_into_right
  (** The left operand flows into the right one: [x |> f], [m >>= f]. *)
  | Right_into_left
  (** The right operand flows into the left one: [f @@ x], [r := v],
      [r.x <- v]. *)

val classify : string -> t
(** The kind of an operator, from its text: the operators of the standard
    library and the usual ones by a table, the others by the direction in
    which their characters point:
    - both [<] and [>], as [<$>] or [<*>]: [Symmetric];
    - [>] only, as [|>>], [%>] or [>=>]: [Left_into_right];
    - [<] only, as [<|], [<<] or [=<<]: [Right_into_left];
    - [@@...], as [@@]: [Right_into_left];
    - an assignment, ending with [=] but not an equality, as [+=]:
      [Right_into_left];
    - otherwise, as [++], [+.] or [^^]: [Symmetric].
    The comparisons are [Symmetric], also with a [.] after them, as [<.] or
    [>=.]. *)
