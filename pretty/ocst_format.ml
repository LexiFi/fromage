(* Formats files with [Ocst_pretty]: [ocst_format.exe FILE...] prints them
   formatted, [ocst_format.exe -i FILE...] formats them in place, and
   [--width N] sets the width of the lines (80 by default). It fails,
   without changing a file, when formatting would change the AST of the file
   (except for the locations): its message says which item changes, and,
   with [--debug], shows the item and the formatted source. *)

(** The formatted source, which must have the same AST as [source] for the
    compiler's parser, except for the locations ([Ocst.Same_ast]). *)
let format ~debug ~width filename source =
  let tokens = Ocst.lex source in
  let formatted, same_ast =
    if Filename.check_suffix filename ".mli" then
      ( Ocst_pretty.interface
          ~width (Ocst.Parser.interface_tokens ~filename tokens)
        ,
        Ocst.Same_ast.interface
      )
    else
      ( Ocst_pretty.implementation
          ~width (Ocst.Parser.implementation_tokens ~filename tokens)
        ,
        Ocst.Same_ast.implementation
      )
  in
  match same_ast ~filename source formatted with
  | Ok () -> formatted
  | Error (difference : Ocst.Same_ast.difference) ->
      Printf.eprintf
        "%s: formatting would change the AST, the file is left as it is: %s\n"
        filename difference.summary
      ;
      if debug then prerr_endline difference.details
      else prerr_endline "(--debug shows the item and the formatted source)"
      ;
      exit 2

let () =
  let in_place = ref false
  and width = ref Ocst_pretty.default_width
  and debug = ref false
  and files = ref []
  in
  let options =
    [ "-i", Arg.Set in_place, " Format the files in place";
      "--width", Arg.Set_int width,
      Printf.sprintf
        "N The width of the lines (default %d)" Ocst_pretty.default_width
    ;
      "-w", Arg.Set_int width, "N Same as --width";
      "--debug", Arg.Set debug, " Show the details of the errors";
    ]
  in
  Arg.parse
    (Arg.align options) (fun file -> files := file :: !files)
    "ocst_format.exe [-i] [--width N] [--debug] FILE..."
  ;
  List.iter
    ( fun filename ->
        let source = In_channel.with_open_bin filename In_channel.input_all in
        match format ~debug:!debug ~width:!width filename source with
        | formatted ->
            if not !in_place then print_string formatted
            else if formatted <> source then
              Out_channel.with_open_bin
                filename (fun channel -> output_string channel formatted)
        | exception Ocst.Syntax_error.Error error ->
            prerr_endline (Ocst.Syntax_error.to_string error); exit 1
    )
    (List.rev !files)
