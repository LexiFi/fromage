(** The differences of two texts, line by line, from their longest common
    subsequence of lines, as in a unified diff. *)

val diff : ?context:int -> string -> string -> string
(** [diff before after]: the lines of [after] which differ from those of
    [before], each line prefixed by [-] (removed), [+] (added) or a space
    (kept), with [context] lines (3 by default) around the changes, and
    [...] where lines are left out; empty when the texts are equal. *)
