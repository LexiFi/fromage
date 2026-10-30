(* The round-trips on the OCaml files of the repository: the lexer
   round-trip ([Lexer_check]), and the parser round-trip of [Roundtrip], with
   the formatter when [Pretty_check] is linked. *)

open Roundtrip

let source_files root =
  let command =
    Printf.sprintf
      "git -C %s ls-files -co --exclude-standard -- '*.ml' '*.mli'"
      (Filename.quote root)
  in
  let channel = Unix.open_process_in command in
  let rec read files =
    match input_line channel with
    | path -> read (path :: files)
    | exception End_of_file -> List.rev files
  in
  let files = read [] in
  match Unix.close_process_in channel with
  | Unix.WEXITED 0 ->
      List.filter Sys.file_exists (List.map (Filename.concat root) files)
  | _ -> failwith "could not list OCaml source files"

let read_file filename =
  let channel = open_in_bin filename in
  Fun.protect
    ~finally:(fun () -> close_in channel)
    (fun () -> really_input_string channel (in_channel_length channel))

(* The examples of OCST are valid: a rejected example would test nothing. *)
let is_example filename =
  let marker = "test/examples/" in
  let rec find index =
      index + String.length marker <= String.length filename
    &&
      ( String.sub filename index (String.length marker) = marker
        || find (index + 1)
      )
  in
  find 0

let check_file filename =
  let source = read_file filename in
  Lexer_check.check filename source;
  let accepted =
    if Filename.check_suffix filename ".mli" then
      check_interface filename source
    else check_implementation filename source
  in
  if is_example filename && not accepted then
    failwith (filename ^ ": the example is rejected by both parsers")

(* Without arguments, checks the OCaml files of the repository; with
   arguments, checks the given files, for debugging. *)
let main files =
  Warnings.parse_options false "-a" |> ignore;
  let files =
    match files with
    | [] -> source_files (git_root ())
    | files -> files
  in
  Printf.printf "Detected %d files\n" (List.length files);
  let failures =
    List.filter_map
      (fun filename -> failure ~name:filename (fun () -> check_file filename))
      files
  in
  report ~title:"OCST codebase round-trip" failures

let () =
  let open Cmdliner in
  let files = Arg.(value & pos_all file [] & info [] ~docv:"FILE") in
  exit
    ( Cmd.eval
        ( Cmd.make
            ( Cmd.info
                "codebase_roundtrip"
                ~doc:
                  "Check the round-trips of the OCaml files of the \
                   repository, or of the files given."
            )
            Term.(const main $ files)
        )
    )
