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

let main files = if not (List.for_all Fun.id (List.map check files)) then exit 1

let () =
  let open Cmdliner in
  let files = Arg.(value & pos_all file [] & info [] ~docv:"FILE") in
  exit
    ( Cmd.eval
        ( Cmd.make
            (Cmd.info "layout" ~doc:"Check the layout of the sources of OCST.")
            Term.(const main $ files)
        )
    )
