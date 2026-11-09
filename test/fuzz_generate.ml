(* The dune rules of the fuzzing, and the sentences which they check (see
   [Fuzz_configurations]):
   fuzz_generate.exe rules (test | coverage)
   fuzz_generate.exe sentences --grammar GRAMMAR (--configuration I | --deep NAME)

   This program does not depend on OCST: a change of OCST does not generate
   the sentences again. *)

open Fuzz_configurations

let rules =
  let open Cmdliner in
  let directory =
    Arg.(
      required
      & pos 0 (some (enum [ "test", "test"; "coverage", "coverage" ])) None
      & info [] ~docv:"DIRECTORY"
    )
  in
  Cmd.v
    ( Cmd.info
        "rules" ~doc:"Print the rules of DIRECTORY/fuzz.inc (test or coverage)."
    )
    Term.(
      const (fun directory -> print_string (dune_rules directory)) $ directory
    )

let sentences =
  let open Cmdliner in
  let menhir =
    Arg.(
      value & opt string "menhir"
      & info [ "menhir" ] ~docv:"PROGRAM" ~doc:"Menhir, for the grammar."
    )
  and fuzzer =
    Arg.(
      value & opt string "ocamlgrammarfuzzer"
      & info [ "fuzzer" ] ~docv:"PROGRAM" ~doc:"ocamlgrammarfuzzer."
    )
  and source =
    Arg.(
      required
    & opt (some file) None
    &
      info
        [ "grammar" ] ~docv:"GRAMMAR"
        ~doc:"The grammar of the compiler (parser.mly)."
    )
  and index =
    Arg.(
      value
    & opt (some int) None
    &
      info
        [ "configuration" ] ~docv:"I"
        ~doc:"The Ith configuration (from 0) of the tests."
    )
  and deep =
    Arg.(
      value
    &
      opt
        ( some
            ( enum
                (List.map (fun { name; _ } -> name, name) deep_configurations)
            )
        )
        None
    &
      info
        [ "deep" ] ~docv:"NAME"
        ~doc:"The configuration of the deep fuzzing NAME."
    )
  in
  let main menhir fuzzer source index deep =
    match index, deep with
    | None, None | Some _, Some _ ->
        Error "one of --configuration and --deep is required"
    | _ ->
        let { entrypoint; arguments; _ } =
          List.hd
            (selected ~quick:false ~entrypoints:[] ~extra:None ~index ~deep [])
        in
        let cmly = grammar ~menhir source in
        print_string
          ( fuzzer_output
              ~fuzzer ~cmly ([ "--entrypoint"; entrypoint ] @ arguments)
          )
        ;
        Ok ()
  in
  Cmd.v
    ( Cmd.info
        "sentences"
        ~doc:
          "Print the sentences of a configuration, as the fuzzer prints \
           them, for $(b,driver.exe --sentences)."
    )
    Term.(term_result' (const main $ menhir $ fuzzer $ source $ index $ deep))

let () =
  exit
    ( Cmdliner.Cmd.eval
        ( Cmdliner.Cmd.group
            ( Cmdliner.Cmd.info
                "fuzz_generate"
                ~doc:"Generate the dune rules and the sentences of the fuzzing."
            )
            [ rules; sentences ]
        )
    )
