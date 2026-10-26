(* Checks the layout of the sources of OCST, given on the command line (see
   [Layout_rules]): [layout.exe FILE...]. *)

let check path =
  let errors =
    Layout_rules.errors
      ~filename:path (In_channel.with_open_bin path In_channel.input_all)
  in
  List.iter
    (fun (line, message) -> Printf.printf "%s:%d: %s\n" path line message)
    errors
  ;
  errors = []

let () =
  let files = ref [] in
  Arg.parse [] (fun file -> files := file :: !files) "layout.exe FILE...";
  if not (List.for_all Fun.id (List.map check (List.rev !files))) then exit 1
