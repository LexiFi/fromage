(* An executable named ocamlformat, which formats with fromage
   ([Ocst_pretty]), with the command line of ocamlformat, as far as dune fmt
   and the editors use it: [ocamlformat --impl FILE] prints FILE formatted.
   The configuration is read from the [.ocamlformat] files, of which only
   [margin] and [disable] are honored (see config.mli). *)

(* Where the formatted sources go. *)
type mode = Stdout | Output of string | Inplace | Check

let read input =
  if input = "-" then In_channel.input_all stdin
  else In_channel.with_open_bin input In_channel.input_all

let write file text =
  Out_channel.with_open_bin file (fun channel -> output_string channel text)

(* Formats [input] ([-] for the standard input), named [filename] in the
   errors, and for its configuration and its kind (unless [interface]). Its
   error, if any. *)
let format
    ~mode ~interface ~margin ~root
    ~enable_outside_detected_project ~filename input
  =
    let interface =
      match interface with
      | Some interface -> interface
      | None when Filename.check_suffix filename ".mli" -> true
      | None when Filename.check_suffix filename ".ml" -> false
      | None ->
          failwith
            ( Printf.sprintf
                "%s: unknown extension, use --impl or --intf" filename
            )
    in
    let source = read input in
    let formatted =
      match Config.find ?root ~enable_outside_detected_project filename with
      | Leave -> Ok source
      | Format { margin = configured } ->
          let width =
            match margin, configured with
            | Some margin, _ | None, Some margin -> margin
            | None, None -> Ocst_pretty.default_width
          in
          match Ocst_pretty.format ~width ~interface ~filename source with
          | Ok formatted -> Ok formatted
          | Error (Syntax_error error) ->
              Error (Ocst.Syntax_error.to_string error)
          | Error (Changes_ast difference) ->
              Error
                ( Printf.sprintf
                    "%s: formatting would change the AST, the file is left \
                     as it is: %s"
                    filename difference.summary
                )
    in
    match formatted, mode with
    | Error message, _ -> Some message
    | Ok formatted, Stdout -> print_string formatted; None
    | Ok formatted, Output file -> write file formatted; None
    | Ok formatted, Inplace ->
        if formatted <> source then write input formatted; None
    | Ok formatted, Check ->
        if formatted = source then None else Some (filename ^ ": not formatted")

let main
    impl intf name inplace output check margin
    root enable_outside_detected_project inputs
  =
    (* As in ocst_format.ml: formatting keeps the memo tables of the layouts
       alive until the end of the file, which the major GC marks again and
       again. *)
    Gc.set { (Gc.get ()) with space_overhead = 200 };
    let interface =
      match impl, intf with
      | true, false -> Ok (Some false)
      | false, true -> Ok (Some true)
      | false, false -> Ok None
      | true, true -> Error "--impl and --intf are exclusive"
    and mode =
      match inplace, output, check, inputs with
      | _, _, _, [] -> Error "Must specify at least one input file"
      | true, None, false, inputs when List.mem "-" inputs ->
          Error "--inplace cannot format the standard input"
      | true, None, false, _ -> Ok Inplace
      | false, None, true, _ -> Ok Check
      | false, output, false, [ _ ] ->
          Ok (Option.fold ~none:Stdout ~some:(fun file -> Output file) output)
      | false, _, false, _ ->
          Error
            "Must specify exactly one input file without --inplace or --check"
      | _ -> Error "--inplace, --output and --check are exclusive"
    in
    match interface, mode, name, inputs with
    | Error message, _, _, _ | _, Error message, _, _ -> Error message
    | _, _, Some _, _ :: _ :: _ -> Error "--name needs a single input file"
    | Ok interface, Ok mode, name, inputs ->
        let errors =
          List.filter_map
            ( fun input ->
                let filename = Option.value name ~default:input in
                match
                  format
                    ~mode ~interface ~margin ~root
                    ~enable_outside_detected_project ~filename input
                with
                | error -> error
                | exception (Failure message | Sys_error message) ->
                    Some message
            )
            inputs
        in
        List.iter prerr_endline errors; if errors <> [] then exit 1; Ok ()

let () =
  let open Cmdliner in
  let flag names doc = Arg.(value & flag & info names ~doc) in
  let impl = flag [ "impl" ] "Parse the inputs as implementations."
  and intf = flag [ "intf" ] "Parse the inputs as interfaces."
  and name =
    Arg.(
      value
    & opt (some string) None
    &
      info
        [ "name" ] ~docv:"NAME"
        ~doc:
          "The name of the input, in the errors, and to find its \
           configuration and its kind: by default, the input file."
    )
  and inplace = flag [ "i"; "inplace" ] "Format the files in place."
  and output =
    Arg.(
      value
    & opt (some string) None
    &
      info
        [ "o"; "output" ] ~docv:"FILE"
        ~doc:"Write the formatted source to FILE."
    )
  and check =
    flag
      [ "check" ]
      "Check that the files are formatted, without changing them: fail when \
       one is not."
  and margin =
    Arg.(
      value
    & opt (some int) None
    &
      info
        [ "m"; "margin" ] ~docv:"N"
        ~doc:
          "The width of the lines, instead of the margin of the \
           configuration (80 by default)."
    )
  and root =
    Arg.(
      value
    & opt (some dir) None
    &
      info
        [ "root" ] ~docv:"DIR"
        ~doc:
          "The root of the project, instead of the nearest directory with a \
           .git, a .hg or a dune-project."
    )
  and enable_outside_detected_project =
    flag
      [ "enable-outside-detected-project" ]
      "Read the configuration files above the root of the project too."
  and inputs =
    Arg.(
      value
    & pos_all string []
    &
      info
        [] ~docv:"FILE"
        ~doc:"The files to format, $(b,-) for the standard input."
    )
  in
  let doc =
    "Format OCaml files with fromage, with the command line of ocamlformat."
  in
  exit
    ( Cmd.eval_result
        ( Cmd.make
            (Cmd.info "ocamlformat" ~version:"%%VERSION%%" ~doc)
            Term.(
              const main $ impl $ intf $ name $ inplace $ output $ check
              $ margin $ root $ enable_outside_detected_project $ inputs
            )
        )
    )
