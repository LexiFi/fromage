(* The doc comments of the files of cases: for each file, the AST of OCST
   with its doc attributes, the warnings about doc comments, and whether the
   compiler agrees. *)

open Roundtrip

let read_file filename = In_channel.with_open_bin filename In_channel.input_all

(* A text, indented, without its last newline. *)
let indented prefix text =
  let lines = String.split_on_char '\n' text in
  let lines =
    match List.rev lines with
      "" :: lines -> List.rev lines
    | _ -> lines
  in
  String.concat "\n" (List.map (fun line -> prefix ^ line) lines)

(* The AST of OCST, printed as source, and its warnings. *)
let ocst ~filename source =
  let tokens = Ocst.lex source in
  if Filename.check_suffix filename ".mli" then
    let cst = Ocst.Parser.interface_tokens ~filename tokens in
    let ast, warnings =
      docstring_warnings (fun () -> Ocst.To_ast.interface ~filename cst)
    in
    Format.asprintf "%a" Pprintast.signature ast, warnings
  else
    let cst = Ocst.Parser.implementation_tokens ~filename tokens in
    let ast, warnings =
      docstring_warnings (fun () -> Ocst.To_ast.implementation ~filename cst)
    in
    Format.asprintf "%a" Pprintast.structure ast, warnings

let () =
  let directory =
    match Sys.argv with
    | [| _; directory |] -> directory
    | _ -> failwith "usage: doc_comments.exe CASES"
  in
  let files = List.sort compare (Array.to_list (Sys.readdir directory)) in
  List.iter
    ( fun name ->
        let source = read_file (Filename.concat directory name) in
        let filename = "cases/" ^ name in
        Printf.printf "== %s\n%s\n" name (indented "  | " source);
        begin match ocst ~filename source with
        | ast, warnings ->
            Printf.printf "AST:\n%s\n" (indented "  " ast);
            List.iter
              ( fun warning ->
                  Printf.printf "Warning: %s\n" (describe_warning warning)
              )
              warnings
        | exception Ocst.Syntax_error.Error error ->
            Printf.printf
              "Syntax error (every case must be valid):\n%s\n"
              (indented "  " (Ocst.Syntax_error.to_string error))
        | exception error ->
            Printf.printf "OCST fails: %s\n" (Printexc.to_string error)
        end;
        let check =
          if Filename.check_suffix name ".mli" then check_interface
          else check_implementation
        in
        begin match
          failure ~name (fun () -> ignore (check filename source))
        with
        | None -> print_endline "The compiler agrees."
        | Some { message; _ } ->
            Printf.printf "The compiler disagrees: %s\n" message
        end;
        print_newline ()
    )
    files
