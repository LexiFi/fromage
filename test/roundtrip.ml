(* The backtraces of unexpected exceptions are in the reports. *)
let () = Printexc.record_backtrace true

(** The root of the repository: the source tree of dune when a dune rule
    runs the tests (in its sandbox, git cannot find the repository), else
    the root of the Git repository. *)
let git_root () =
  match Sys.getenv_opt "DUNE_SOURCEROOT" with
  | Some root -> root
  | None ->
      let channel = Unix.open_process_in "git rev-parse --show-toplevel" in
      match input_line channel, Unix.close_process_in channel with
      | root, Unix.WEXITED 0 -> root
      | _ -> failwith "could not determine the Git root"

(* The warnings about doc comments (warning 50) emitted by [parse], with its
   result. *)
let docstring_warnings parse =
  let warnings = ref [] in
  let reporter = !Location.warning_reporter
  and state = Warnings.backup ()
  in
  ignore (Warnings.parse_options false "+50");
  ( Location.warning_reporter
    :=
    ( fun loc warning ->
        begin match warning with
        | Warnings.Unexpected_docstring _ ->
            warnings := (loc, warning) :: !warnings
        | _ -> ()
        end;
        None
    )
  );
  Fun.protect
    ~finally:
      (fun () -> Location.warning_reporter := reporter; Warnings.restore state)
    ( fun () ->
        let value = parse () in
        value, List.rev !warnings
    )

let describe_warning ((loc : Location.t), warning) =
  Printf.sprintf
    "line %d, characters %d-%d: %s" loc.loc_start.pos_lnum
    (loc.loc_start.pos_cnum - loc.loc_start.pos_bol)
    (loc.loc_end.pos_cnum - loc.loc_start.pos_bol)
    begin match warning with
    | Warnings.Unexpected_docstring true -> "unattached doc comment"
    | _ -> "ambiguous doc comment"
    end

(* The first difference between the warnings of OCST and the compiler. *)
let check_warnings ~name ocst compiler =
  if ocst <> compiler then
    let rec first ocst compiler =
      match ocst, compiler with
      | warning :: ocst, warning' :: compiler when warning = warning' ->
          first ocst compiler
      | warning :: _, _ ->
          "OCST warns: " ^ describe_warning warning
          ^
          ( match compiler with
              warning :: _ -> ", the compiler: " ^ describe_warning warning
            | [] -> ""
          )
      | [], warning :: _ -> "the compiler warns: " ^ describe_warning warning
      | [], [] -> assert false
    in
    failwith
      ( Printf.sprintf
          "%s: doc comment warnings differ: %s" name (first ocst compiler)
      )

let compiler_implementation filename source =
  let lexbuf = Lexing.from_string source in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  Parse.implementation lexbuf

let compiler_interface filename source =
  let lexbuf = Lexing.from_string source in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  Parse.interface lexbuf

let structure_dump structure =
  Format.asprintf "%a" Printast.implementation structure

let signature_dump signature = Format.asprintf "%a" Printast.interface signature

(* The first line where two AST dumps differ, to keep failure reports short. *)
let first_difference ocst compiler =
  let ocst = String.split_on_char '\n' ocst in
  let compiler = String.split_on_char '\n' compiler in
  let rec loop line ocst compiler =
    match ocst, compiler with
    | o :: ocst, c :: compiler when o = c -> loop (line + 1) ocst compiler
    | o :: _, c :: _ ->
        Printf.sprintf
          "line %d: OCST %s | compiler %s" line (String.trim o) (String.trim c)
    | o :: _, [] ->
        Printf.sprintf "line %d: OCST %s | compiler <end>" line (String.trim o)
    | [], c :: _ ->
        Printf.sprintf "line %d: OCST <end> | compiler %s" line (String.trim c)
    | [], [] -> "identical dumps (difference not shown by Printast)"
  in
  loop 1 ocst compiler

(* {1 Structural differences}

   The first difference between two ASTs, including what Printast does not
   show (ghost flags, location stacks...): the path of block fields to it, the
   two values, and the innermost locations around it. *)

let is_position (value : Obj.t) =
  Obj.is_block value && Obj.tag value = 0
  && Obj.size value = 4 && Obj.is_block (Obj.field value 0)
  && Obj.tag (Obj.field value 0) = Obj.string_tag
  && Obj.is_int (Obj.field value 1) && Obj.is_int (Obj.field value 2)
  && Obj.is_int (Obj.field value 3)

let is_location (value : Obj.t) =
  Obj.is_block value && Obj.tag value = 0
  && Obj.size value = 3 && is_position (Obj.field value 0)
  && is_position (Obj.field value 1) && Obj.is_int (Obj.field value 2)

let location_text (value : Obj.t) =
  let loc : Location.t = Obj.obj value in
  Format.asprintf
    "%a%s" Location.print_loc loc (if loc.loc_ghost then " (ghost)" else "")

let value_text (value : Obj.t) =
  if Obj.is_int value then string_of_int (Obj.obj value)
  else if is_location value then location_text value
  else if Obj.tag value = Obj.string_tag then
    Printf.sprintf "%S" (Obj.obj value)
  else if Obj.tag value = Obj.double_tag then string_of_float (Obj.obj value)
  else Printf.sprintf "<block tag %d, size %d>" (Obj.tag value) (Obj.size value)

let rec difference path locations (ocst : Obj.t) (compiler : Obj.t) =
  let locations =
    if is_location ocst && is_location compiler then Some (ocst, compiler)
    else locations
  in
  let found () = Some (List.rev path, locations, ocst, compiler) in
  if Obj.is_int ocst || Obj.is_int compiler then
    if ocst == compiler then None else found ()
  else if
    Obj.tag ocst <> Obj.tag compiler || Obj.size ocst <> Obj.size compiler
  then found ()
  else if Obj.tag ocst = Obj.string_tag then
    if (Obj.obj ocst : string) = Obj.obj compiler then None else found ()
  else if Obj.tag ocst = Obj.double_tag then
    if (Obj.obj ocst : float) = Obj.obj compiler then None else found ()
  else if Obj.tag ocst >= Obj.no_scan_tag then None
  else
    let rec fields index =
      if index = Obj.size ocst then None
      else
        match
          difference
            (index :: path) locations (Obj.field ocst index)
            (Obj.field compiler index)
        with
        | None -> fields (index + 1)
        | found -> found
    in
    fields 0

(* The first top-level item that differs, printed by [print], and the first
   difference in it. *)
let item_details print ocst compiler =
  let rec loop index ocst compiler =
    match ocst, compiler with
    | o :: ocst, c :: compiler when Stdlib.(o = c) ->
        loop (index + 1) ocst compiler
    | o :: _, c :: _ ->
        let buffer = Buffer.create 256 in
        Printf.bprintf
          buffer "  item %d:\n%s\n" index
          ( String.concat
              "\n"
              ( List.map
                  (fun line -> "    " ^ line)
                  (String.split_on_char '\n' (Format.asprintf "%a" print c))
              )
          )
        ;
        begin match difference [] None (Obj.repr o) (Obj.repr c) with
        | None -> ()
        | Some (path, locations, o, c) ->
            Printf.bprintf
              buffer "  at field path %s: OCST %s | compiler %s\n"
              (String.concat "." (List.map string_of_int path))
              (value_text o) (value_text c)
            ;
            Option.iter
              ( fun (o, c) ->
                  Printf.bprintf
                    buffer "  in location: OCST %s | compiler %s\n"
                    (location_text o) (location_text c)
              )
              locations
        end;
        Buffer.contents buffer
    | _ :: _, [] ->
        Printf.sprintf "  OCST has %d more items\n" (List.length ocst)
    | [], _ :: _ ->
        Printf.sprintf "  compiler has %d more items\n" (List.length compiler)
    | [], [] -> ""
  in
  loop 0 ocst compiler

(* A failure: a one-line message, and details shown for the first failure
   only. *)
exception Mismatch of string * string Lazy.t

(* {1 Formatting}

   A formatter prints a CST with other line breaks and indentation: the
   formatted source has the same tokens and comments as the original one. *)

type formatter =
  { format_implementation : Ocst.Cst.implementation -> string;
    format_interface : Ocst.Cst.interface -> string;
  }

let formatter = ref None

(* The comments and tokens of a source, without its whitespace. A comment
   which starts its line ([own_line]) stays at the start of a line when
   formatted. *)
type significant =
  | Trivia of { text : string; own_line : bool; blank : bool }
  | Token of { kind : Ocst.Token.any_kind; own_line : bool; blank : bool }
  (** [blank]: after a blank line, which formatting keeps (except at the
      start and at the end of the source). Formatting may add blank lines,
      after the items on several lines of structures and signatures. *)

(** The tokens and comments of a source, with the line of each one. *)
let significant source =
  let tokens = Ocst.lex source in
  let spans = Ocst.Token_position.spans ~filename:"" tokens in
  (* Is the next comment or token at the start of its line? *)
  let line_start = ref true
  (* The newlines since the previous comment or token. *)
  and newlines = ref 0
  and first = ref true
  in
  (* Is the next comment or token after a blank line? *)
  let blank () =
    let blank = !newlines >= 2 && not !first in
    newlines := 0; first := false; blank
  in
  List.concat
    ( List.mapi
        ( fun index (token : Ocst.Token.any_token) ->
            let line = spans.(index).start.pos_lnum in
            let trivia =
              List.filter_map
                ( function
                  | Ocst.Token.Whitespace text ->
                      String.iter (fun c -> if c = '\n' then incr newlines) text
                      ;
                      if String.contains text '\n' then line_start := true;
                      None
                  | trivia ->
                      let own_line = !line_start in
                      line_start := false;
                      Some
                        ( Trivia
                            { text = Ocst.Token.trivia_text trivia;
                              own_line; blank = blank ()
                            }
                          ,
                          line
                        )
                )
                token.leading
            in
            let own_line =
              match token.kind with
              | `DOCSTRING _ -> !line_start
              | _ -> false
            in
            line_start := false;
            let blank =
              match token.kind with
              | `EOF -> newlines := 0; false
              | _ -> blank ()
            in
            trivia @ [ Token { kind = token.kind; own_line; blank }, line ]
        )
        tokens
    )

(* [result] keeps [original]: the same comment or token, at the start of a
   line if [original] is, after a blank line if [original] is. *)
let keeps original result =
  match original, result with
  | Trivia { text; own_line; blank }, Trivia result ->
      text = result.text && ((not own_line) || result.own_line)
      && ((not blank) || result.blank)
  | Token { kind; own_line; blank }, Token result ->
      kind = result.kind && ((not own_line) || result.own_line)
      && ((not blank) || result.blank)
  | _ -> false

let describe_significant =
  let on_its_line own_line = if own_line then " at the start of a line" else ""
  in
  let after_blank blank = if blank then " after a blank line" else "" in
  function
  | Trivia { text; own_line; blank } ->
      Printf.sprintf
        "comment %S%s%s" text (on_its_line own_line) (after_blank blank)
  | Token { kind; own_line; blank } ->
      Printf.sprintf
        "token %S%s%s" (Ocst.Token.text (Ocst.Token.make kind))
        (on_its_line own_line) (after_blank blank)

(** The lines of [text] around [line]. *)
let lines_around text line =
  String.split_on_char '\n' text
  |> List.mapi (fun index text -> index + 1, text)
  |> List.filter (fun (index, _) -> abs (index - line) <= 5)
  |> List.map (fun (index, text) -> Printf.sprintf "%5d %s" index text)
  |> String.concat "\n"

(* The formatter keeps the comments and tokens of [source] ([keeps]), and its
   AST for the compiler's parser, except for the locations ([same_ast]): the
   items documented by the doc comments too. *)
let check_format ~name ~same_ast source format =
  match format () with
  | exception error ->
      failwith
        ( Printf.sprintf
            "%s: the formatter failed: %s" name (Printexc.to_string error)
        )
  | formatted ->
      let rec loop original result =
        match original, result with
        | [], [] -> ()
        | (o, _) :: original, (r, _) :: result when keeps o r ->
            loop original result
        | _ ->
            let describe =
              function
              | (value, line) :: _ ->
                  Printf.sprintf
                    "%s (line %d)" (describe_significant value) line
              | [] -> "the end"
            in
            let line =
              match result with
                (_, line) :: _ -> line
              | [] -> 0
            in
            raise
              ( Mismatch
                  ( Printf.sprintf
                      "%s: the formatter changed %s into %s"
                      name (describe original) (describe result)
                    ,
                    lazy ("Formatted source:\n" ^ lines_around formatted line)
                  )
              )
      in
      loop (significant source) (significant formatted);
      match same_ast source formatted with
      | Ok () -> ()
      | Error (difference : Ocst.Same_ast.difference) ->
          raise
            ( Mismatch
                ( Printf.sprintf
                    "%s: the formatter changed the AST: %s"
                    name difference.summary
                  ,
                  lazy difference.details
                )
            )

(* The compiler's message for an error, on one line. *)
let error_message error =
  match error with
  | Ocst.Syntax_error.Error error ->
      String.concat
        " " (String.split_on_char '\n' (Ocst.Syntax_error.to_string error))
  | _ ->
      match Location.error_of_exn error with
      | Some (`Ok report) ->
          let text = Format.asprintf "%a" Location.print_report report in
          String.concat
            " " (List.filter (( <> ) "") (String.split_on_char '\n' text))
      | Some `Already_displayed | None -> Printexc.to_string error

(* The location and the message of the compiler's error on a source, if any. *)
let compiler_error ~interface filename source =
  match
    if interface then ignore (compiler_interface filename source)
    else ignore (compiler_implementation filename source)
  with
  | () -> None
  | exception error ->
      match Location.error_of_exn error with
      | Some (`Ok { main = { loc; txt }; _ }) ->
          Some (loc, Format.asprintf "%a" Format_doc.Doc.format txt)
      | Some `Already_displayed | None ->
          Some (Location.none, Printexc.to_string error)

let attempt parse =
  try Ok (parse ()) with error -> Error (error_message error)

(* OCST's parser fails with a syntax error, never with an internal error or
   another exception, even on sources that the compiler rejects. *)
let attempt_ocst ~name parse =
  match parse () with
  | value -> Ok value
  | exception (Ocst.Syntax_error.Error { id = "internal"; _ } as error) ->
      let backtrace = Printexc.get_backtrace () in
      raise
        ( Mismatch
            ( Printf.sprintf
                "%s: OCST internal error: %s" name (error_message error)
              ,
              lazy ("OCST backtrace:\n" ^ backtrace)
            )
        )
  | exception (Ocst.Syntax_error.Error _ as error) ->
      Error (error_message error)
  | exception error ->
      let backtrace = Printexc.get_backtrace () in
      raise
        ( Mismatch
            ( Printf.sprintf
                "%s: OCST raised an exception: %s"
                name (Printexc.to_string error)
              ,
              lazy ("OCST backtrace:\n" ^ backtrace)
            )
        )

let check_implementation ?(name = "") filename source =
  let name = if name = "" then filename else name in
  let cst =
    attempt_ocst
      ~name
      (fun () -> Ocst.Parser.implementation_tokens ~filename (Ocst.lex source))
  in
  let compiler =
    attempt
      ( fun () ->
          docstring_warnings (fun () -> compiler_implementation filename source)
      )
  in
  match cst, compiler with
  | Error _, Error _ -> false
  | Error error, Ok _ ->
      failwith (Printf.sprintf "%s: OCST parser failed: %s" name error)
  | Ok _, Error error ->
      failwith (Printf.sprintf "%s: compiler parser failed: %s" name error)
  | Ok cst, Ok (compiler_ast, compiler_warnings) ->
      Option.iter
        ( fun formatter ->
            check_format
              ~name ~same_ast:(Ocst.Same_ast.implementation ~filename)
              source (fun () -> formatter.format_implementation cst)
        )
        !formatter
      ;
      let printed = Ocst.Printer.string_of_implementation cst in
      if printed <> source then failwith (name ^ ": CST printer changed source")
      ;
      let ocst_ast, ocst_warnings =
        docstring_warnings (fun () -> Ocst.To_ast.implementation ~filename cst)
      in
      check_warnings ~name ocst_warnings compiler_warnings;
      if not Stdlib.(ocst_ast = compiler_ast) then
        raise
          ( Mismatch
              ( Printf.sprintf
                  "%s: AST mismatch at %s" name
                  ( first_difference
                      (structure_dump ocst_ast) (structure_dump compiler_ast)
                  )
                ,
                lazy
                  (item_details Pprintast.structure_item ocst_ast compiler_ast)
              )
          )
      ;
      true

let check_interface ?(name = "") filename source =
  let name = if name = "" then filename else name in
  let cst =
    attempt_ocst
      ~name (fun () -> Ocst.Parser.interface_tokens ~filename (Ocst.lex source))
  in
  let compiler =
    attempt
      ( fun () ->
          docstring_warnings (fun () -> compiler_interface filename source)
      )
  in
  match cst, compiler with
  | Error _, Error _ -> false
  | Error error, Ok _ ->
      failwith (Printf.sprintf "%s: OCST parser failed: %s" name error)
  | Ok _, Error error ->
      failwith (Printf.sprintf "%s: compiler parser failed: %s" name error)
  | Ok cst, Ok (compiler_ast, compiler_warnings) ->
      Option.iter
        ( fun formatter ->
            check_format
              ~name ~same_ast:(Ocst.Same_ast.interface ~filename)
              source (fun () -> formatter.format_interface cst)
        )
        !formatter
      ;
      let printed = Ocst.Printer.string_of_interface cst in
      if printed <> source then failwith (name ^ ": CST printer changed source")
      ;
      let ocst_ast, ocst_warnings =
        docstring_warnings (fun () -> Ocst.To_ast.interface ~filename cst)
      in
      check_warnings ~name ocst_warnings compiler_warnings;
      if not Stdlib.(ocst_ast = compiler_ast) then
        raise
          ( Mismatch
              ( Printf.sprintf
                  "%s: AST mismatch at %s" name
                  ( first_difference
                      (signature_dump ocst_ast) (signature_dump compiler_ast)
                  )
                ,
                lazy
                  (item_details Pprintast.signature_item ocst_ast compiler_ast)
              )
          )
      ;
      true

(* {1 Running checks} *)

type failure = { message : string; details : string Lazy.t option }

(* Runs [check], whose failures are reported for [name]. *)
let failure ~name check =
  try check (); None with
  | Mismatch (message, details) -> Some { message; details = Some details }
  | Failure message -> Some { message; details = None }
  | error ->
      let backtrace = Printexc.get_backtrace () in
      Some
        { message = name ^ ": " ^ Printexc.to_string error;
          details = Some (lazy ("Backtrace:\n" ^ backtrace));
        }

let report ~title failures =
  match failures with
  | [] -> Printf.printf "%s passed.\n" title
  | { message; details } :: others ->
      Printf.printf "%s failures:\n" title;
      print_endline message;
      Option.iter (fun details -> print_string (Lazy.force details)) details;
      List.iter (fun { message; _ } -> print_endline message) others
