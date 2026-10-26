(* The messages of [Ocst.Same_ast], which checks that formatting keeps the
   AST of a source (except for the locations). *)

let show name check source formatted =
  Printf.printf "== %s\n" name;
  match check ~filename:"test.ml" source formatted with
  | Ok () -> print_endline "same AST"
  | Error (difference : Ocst.Same_ast.difference) ->
      Printf.printf
        "summary: %s\ndetails:\n%s\n" difference.summary difference.details

let () =
  show
    "other line breaks" Ocst.Same_ast.implementation
    "let x = f a b\nlet y = 2\n" "let x =\n  f a b\n\nlet y = 2\n"
  ;
  (* Without blank lines, the doc comment documents [y] and [x]; after a
     blank line, only [x]. *)
  show
    "a doc comment detached by a blank line" Ocst.Same_ast.interface
    "val y : int\n(** Doc. *)\nval x : int\n"
    "val y : int\n\n(** Doc. *)\nval x : int\n"
  ;
  show "other code" Ocst.Same_ast.implementation "let x = 1\n" "let x = 2\n";
  show
    "a formatted source which does not parse"
    Ocst.Same_ast.implementation "let x = 1\n" "let x =\n"
