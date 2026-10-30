(* Checks the layout which the formatter [Ocst_pretty] gives to the files
   given on the command line: the files are formatted (as [make fmt] does), so
   that the tests of formatting/ show the expected layouts, except an input
   [a.ml] with an [a.ml.expected], whose layout is part of the test (as blank
   lines and doc comments), and which [a.ml.expected] shows formatted;
   the formatted source follows the layout rules ([Layout_rules]), with lines
   of at most the width, for the default width and for 120; and formatting it
   again gives the same source. The round-trip tests check that
   the formatter keeps the tokens and the comments. *)

let format ?width ~filename source =
  let tokens = Ocst.lex source in
  if Filename.check_suffix filename ".mli" then
    Ocst_pretty.interface ?width (Ocst.Parser.interface_tokens ~filename tokens)
  else
    Ocst_pretty.implementation
      ?width (Ocst.Parser.implementation_tokens ~filename tokens)

(** The first line which differs between two texts, with both versions. *)
let first_difference expected actual =
  let rec loop line expected actual =
    match expected, actual with
    | e :: expected, a :: actual when e = a -> loop (line + 1) expected actual
    | e :: _, a :: _ -> Some (line, e, a)
    | e :: _, [] -> Some (line, e, "<end>")
    | [], a :: _ -> Some (line, "<end>", a)
    | [], [] -> None
  in
  loop 1 (String.split_on_char '\n' expected) (String.split_on_char '\n' actual)

(** The problems of the source formatted for [width]: its errors of layout
    ([Layout_rules]), and the changes when formatting it again. *)
let layout_problems ~name ~filename ~width formatted =
  let name =
    if width = Ocst_pretty.default_width then name
    else Printf.sprintf "%s (width %d)" name width
  in
  let errors =
    List.map
      (fun (line, message) -> Printf.sprintf "%s:%d: %s" name line message)
      (Layout_rules.errors ~filename formatted)
  in
  match first_difference formatted (format ~width ~filename formatted) with
  | None -> errors
  | Some (line, first, second) ->
        errors
      @
        [ Printf.sprintf
            "%s:%d: formatting again changes %S into %S" name line first second
        ]

(** The problems of a file: it must be formatted (by [make fmt], for the
    default width), and its layouts for the default width and for 120 must
    follow the rules. A file [a.ml] with an [a.ml.expected] is an input left
    as it is, which [a.ml.expected] shows formatted. *)
let check filename =
  let source = In_channel.with_open_bin filename In_channel.input_all in
  let name = Filename.basename filename in
  let formatted = format ~filename source in
  let expected_file = filename ^ ".expected" in
  let name, expected =
    if Sys.file_exists expected_file then
      ( Filename.basename expected_file,
        In_channel.with_open_bin expected_file In_channel.input_all
      )
    else name, source
  in
  let not_formatted =
    match first_difference expected formatted with
    | None -> []
    | Some (line, expected, formatted) ->
        [ Printf.sprintf
            "%s:%d: not formatted: %S instead of %S (run make fmt)"
            name line expected formatted
        ]
  in
    not_formatted
  @ layout_problems ~name ~filename ~width:Ocst_pretty.default_width formatted
  @
    layout_problems
      ~name ~filename ~width:120 (format ~width:120 ~filename source)

let main files =
  (* As in ocst_format.ml: less work for the major GC when formatting. *)
  Gc.set { (Gc.get ()) with space_overhead = 200 };
  let problems = List.concat_map check files in
  Printf.printf "Formatted %d files.\n" (List.length files);
  List.iter print_endline problems

let () =
  let open Cmdliner in
  let files = Arg.(value & pos_all file [] & info [] ~docv:"FILE") in
  exit
    ( Cmd.eval
        ( Cmd.make
            ( Cmd.info
                "pretty_test" ~doc:"Check the layout of the formatted files."
            )
            Term.(const main $ files)
        )
    )
