(* Formats files with [Ocst_pretty]: [ocst_format.exe FILE...] prints them
   formatted, [ocst_format.exe -i FILE...] formats them in place, and
   [--width N] sets the width of the lines (80 by default). It fails,
   without changing a file, when formatting would change the AST of the file
   (except for the locations): its message says which item changes, and,
   with [--debug], shows the item and the formatted source. *)

let main in_place width debug files =
  (* Formatting keeps the memo tables of the layouts alive until the end of
     the file, and the major GC marks them again and again: it works less
     with this space overhead (10% faster, 20% more memory). *)
  Gc.set { (Gc.get ()) with space_overhead = 200 };
  List.iter
    ( fun filename ->
        let source = In_channel.with_open_bin filename In_channel.input_all in
        let interface = Filename.check_suffix filename ".mli" in
        match Ocst_pretty.format ~width ~interface ~filename source with
        | Ok formatted ->
            if not in_place then print_string formatted
            else if formatted <> source then
              Out_channel.with_open_bin
                filename (fun channel -> output_string channel formatted)
        | Error (Syntax_error error) ->
            prerr_endline (Ocst.Syntax_error.to_string error); exit 1
        | Error (Changes_ast difference) ->
            Printf.eprintf
              "%s: formatting would change the AST, the file is left as it \
               is: %s\n"
              filename difference.summary
            ;
            if debug then prerr_endline difference.details
            else
              prerr_endline "(--debug shows the item and the formatted source)"
            ;
            exit 2
    )
    files

let () =
  let open Cmdliner in
  let in_place =
    Arg.(value & flag & info [ "i" ] ~doc:"Format the files in place.")
  and width =
    Arg.(
      value & opt int Ocst_pretty.default_width
      & info [ "w"; "width" ] ~docv:"N" ~doc:"The width of the lines."
    )
  and debug =
    Arg.(
      value
    & flag
    &
      info
        [ "debug" ]
        ~doc:
          "When formatting would change the AST of a file, show the item \
             and the formatted source."
    )
  and files = Arg.(non_empty & pos_all file [] & info [] ~docv:"FILE")
  in
  let doc = "Format OCaml files."
  and exits =
      Cmd.Exit.info 1 ~doc:"on a syntax error."
    ::
      Cmd.Exit.info
        2 ~doc:"when formatting would change the AST of a file (left as it is)."
    :: Cmd.Exit.defaults
  in
  exit
    ( Cmd.eval
        ( Cmd.make
            (Cmd.info "fromage" ~doc ~exits)
            Term.(const main $ in_place $ width $ debug $ files)
        )
    )
