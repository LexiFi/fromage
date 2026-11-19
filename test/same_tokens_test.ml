(* The messages of [Ocst_pretty.Same_tokens], which checks that formatting
   only changes the whitespace of a source, with the compiler's lexer. *)

let show name source formatted =
  Printf.printf "== %s\n" name;
  match Ocst_pretty.Same_tokens.check ~filename:"test.ml" source formatted with
  | Ok () -> print_endline "same tokens"
  | Error (difference : Ocst_pretty.Same_tokens.difference) ->
      Printf.printf
        "summary: %s\ndetails:\n%s\n" difference.summary difference.details

let () =
  show
    "other line breaks and indentation"
    "let x = f a b (* c *)\n(** d *)\nlet y = 2\n"
    "let x =\n  f a b (* c *)\n(** d *)\nlet y = 2\n"
  ;
  show
    "multi-line strings keep their text"
    "let x = \"a\\\n   b\"\n" "let x =\n  \"a\\\n   b\"\n"
  ;
  show
    "other spaces in a comment" "let x = 1 (* a  b *)\n" "let x = 1 (* a b *)\n"
  ;
  show
    "a removed comment" "let x = 1 (* a *)\nlet y = 2\n"
    "let x = 1\nlet y = 2\n"
  ;
  show
    "other spaces in a doc comment"
    "(** a  b *)\nlet x = 1\n" "(** a b *)\nlet x = 1\n"
  ;
  (* The lexer has no negative literals: [-1] is [-] and [1]. *)
  show "a negative number" "let x = f - 1\n" "let x = f -1\n";
  show "merged tokens" "let x = a - -1\n" "let x = a --1\n";
  show "an operator which opens a comment" "let x = ( * )\n" "let x = (*)\n";
  show
    "a blank line between items"
    "let x = 1\nlet y = 2\n" "let x = 1\n\nlet y = 2\n"
  ;
  (* Without blank lines, the doc comment documents [y] and [x]; after a
     blank line, only [x]. *)
  show
    "a doc comment detached from the previous item"
    "val y : int\n(** Doc. *)\nval x : int\n"
    "val y : int\n\n(** Doc. *)\nval x : int\n"
  ;
  show
    "a doc comment detached from the next item"
    "(** Doc. *)\nval x : int\n" "(** Doc. *)\n\nval x : int\n"
  ;
  show "other code" "let x = 1\n" "let x = 2\n";
  show "a hash-bang" "#!/bin/ocaml\nlet x = 1\n" "#!/bin/ocaml\nlet x =\n  1\n";
  show
    "a line directive moved" "let x = 1\n# 3 \"a.ml\"\nlet y = 2\n"
    "let x = 1 # 3 \"a.ml\"\nlet y = 2\n"
  ;
  show "a source which does not lex" "let x = \"a\n" "let x =\n  \"a\n"
