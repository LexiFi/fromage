(* The syntax errors of OCST on the files of test/errors/cases, next to
   the errors of the compiler, and the errors of the parser which no file
   raises. *)

open Roundtrip

let read_file filename = In_channel.with_open_bin filename In_channel.input_all

(* Messages may quote invalid UTF-8 from the sources. *)
let printable text =
  if String.is_valid_utf_8 text then text else String.escaped text

(* A source, indented, without its last newline. *)
let indented source =
  let lines = String.split_on_char '\n' source in
  let lines =
    match List.rev lines with
      "" :: lines -> List.rev lines
    | _ -> lines
  in
  String.concat "\n" (List.map (fun line -> "  | " ^ line) lines)

(* The compiler's error, without the source excerpt. *)
let compiler_error ~filename source =
  let lexbuf = Lexing.from_string source in
  Location.init lexbuf filename;
  let parse =
    if Filename.check_suffix filename ".mli" then
      fun () -> ignore (Parse.interface lexbuf)
    else fun () -> ignore (Parse.implementation lexbuf)
  in
  match parse () with
  | () -> "no error: the compiler must reject the file too."
  | exception error ->
      match Location.error_of_exn error with
      | Some (`Ok { main = { loc; txt }; _ }) ->
          Ocst.Syntax_error.location_text loc ^ "\nError: "
          ^ Format.asprintf "%a" Format_doc.Doc.format txt
      | Some `Already_displayed | None -> Printexc.to_string error

(* The error of OCST and its identifier. *)
let ocst_error ~filename source =
  let tokens = Ocst.lex source in
  let parse =
    if Filename.check_suffix filename ".mli" then
      fun () -> ignore (Ocst.Parser.interface_tokens ~filename tokens)
    else fun () -> ignore (Ocst.Parser.implementation_tokens ~filename tokens)
  in
  match parse () with
  | () -> None, "no error: OCST must reject the file."
  | exception Ocst.Syntax_error.Error error ->
      Some error.id, Ocst.Syntax_error.to_string error
  | exception error -> None, "unexpected exception: " ^ Printexc.to_string error

(* The identifiers of the errors of the parser, [syntax_error "id" ...], in
   the order of the source, with repetitions. *)
let declared_errors parser =
  let source = read_file parser in
  let regexp = Str.regexp {|syntax_error "\([a-z0-9-]+\)"|} in
  let rec loop start ids =
    match Str.search_forward regexp source start with
    | position -> loop (position + 1) (Str.matched_group 1 source :: ids)
    | exception Not_found -> List.rev ids
  in
  List.filter (( <> ) "internal") (loop 0 [])

let () =
  Warnings.parse_options false "-a" |> ignore;
  let root = Filename.concat (git_root ()) "ocst" in
  let directory = Filename.concat root "test/errors/cases" in
  let files = List.sort compare (Array.to_list (Sys.readdir directory)) in
  let raised =
    List.filter_map
      ( fun name ->
          let source = read_file (Filename.concat directory name) in
          let filename = "cases/" ^ name in
          let id, ocst = ocst_error ~filename source in
          Printf.printf
            "== %s\n%s\nOCST [%s]:\n%s\nCompiler:\n%s\n\n" name
            (indented (printable source)) (Option.value id ~default:"none")
            (printable ocst) (printable (compiler_error ~filename source))
          ;
          id
      )
      files
  in
  if List.mem "internal" raised then
    print_endline "Some files raise internal errors."
  ;
  let declared = declared_errors (Filename.concat root "parser.ml") in
  let rec duplicates =
    function
    | id :: (id' :: _ as rest) when id = id' ->
        id :: duplicates (List.filter (( <> ) id) rest)
    | _ :: rest -> duplicates rest
    | [] -> []
  in
  begin match duplicates (List.sort compare declared) with
  | [] -> ()
  | ids ->
      Printf.printf
        "Errors declared more than once in parser.ml (%d):\n%s\n"
        (List.length ids) (String.concat "\n" ids)
  end;
  match
    List.filter
      (fun id -> not (List.mem id raised)) (List.sort_uniq compare declared)
  with
  | [] -> print_endline "Every error has a test."
  | untested ->
      Printf.printf
        "Errors without a test in test/errors/cases (%d):\n%s\n"
        (List.length untested) (String.concat "\n" untested)
