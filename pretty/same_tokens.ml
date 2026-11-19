(* Compares the input of the compiler's parser for two sources: see
   same_tokens.mli. *)

type kind = Token | Comment | Doc_comment | Hash_bang | Lexing_failed

(* A token or a comment: its text in the source, and its line. [blank]: after
   a blank line, for the attachment of the doc comments, only for the doc
   comments and the tokens which follow doc comments, as [Lexer.token]
   computes it. *)
type lexeme = { kind : kind; text : string; blank : bool; line : int }

(* The state of [Lexer.token] between two tokens: the newlines since the last
   token or doc comment (a comment cancels a newline, not a blank line), and
   whether there are doc comments since the last token. *)
type gap = { newlines : [ `None | `Newline | `Blank ]; doc_comments : bool }

let token_gap = { newlines = `None; doc_comments = false }

let text source (start : Lexing.position) (stop : Lexing.position) =
  String.sub source start.pos_cnum (stop.pos_cnum - start.pos_cnum)

(** The tokens and comments of [source] for the compiler's lexer, until its
    end or until the lexer fails, without the whitespace (nor the line
    directives, which only change the positions). *)
let lexemes ~filename source =
  Lexer.init ();
  let lexbuf = Lexing.from_string source in
  Location.init lexbuf filename;
  Lexer.skip_hash_bang lexbuf;
  let hash_bang =
    if lexbuf.lex_curr_pos = 0 then []
    else
      [ { kind = Hash_bang; text = Lexing.lexeme lexbuf; blank = false; line = 1
        }
      ]
  in
  let lexeme kind ~blank (start : Lexing.position) stop =
    { kind; text = text source start stop; blank; line = start.pos_lnum }
  in
  let rec loop gap lexemes =
    match Lexer.token_with_comments lexbuf with
    | Parser.EOL ->
        let newlines =
          match gap.newlines with
            `None -> `Newline
          | `Newline | `Blank -> `Blank
        in
        loop { gap with newlines } lexemes
    | Parser.COMMENT (_, loc) ->
        let newlines =
          match gap.newlines with
            `None | `Newline -> `None
          | `Blank -> `Blank
        in
        loop
          { gap with newlines }
          (lexeme Comment ~blank:false loc.loc_start loc.loc_end :: lexemes)
    | Parser.DOCSTRING doc ->
        let loc = Docstrings.docstring_loc doc in
        loop
          { newlines = `None; doc_comments = true }
          ( lexeme
              Doc_comment ~blank:(gap.newlines = `Blank)
              loc.loc_start loc.loc_end
          :: lexemes
          )
    | token ->
        ( let lexemes =
              lexeme
                Token ~blank:(gap.doc_comments && gap.newlines = `Blank)
                lexbuf.lex_start_p lexbuf.lex_curr_p
            :: lexemes
          in
          match token with
          | Parser.EOF -> List.rev lexemes
          | _ -> loop token_gap lexemes
        )
    | exception Lexer.Error (_, loc) ->
        List.rev
          ( { kind = Lexing_failed; text = "";
              blank = false; line = loc.loc_start.pos_lnum
            }
          :: lexemes
          )
  in
  let warnings = !Lexer.print_warnings in
  Lexer.print_warnings := false;
  Fun.protect
    ~finally:(fun () -> Lexer.print_warnings := warnings)
    (fun () -> hash_bang @ loop token_gap [])

let describe =
  function
  | [] -> "the end"
  | { kind; text; blank; line } :: _ ->
      let what =
        match kind with
        | Token when text = "" -> "the end"
        | Token -> Printf.sprintf "token %S" text
        | Comment -> Printf.sprintf "comment %S" text
        | Doc_comment -> Printf.sprintf "doc comment %S" text
        | Hash_bang -> Printf.sprintf "%S" text
        | Lexing_failed -> "a lexing error"
      in
      Printf.sprintf
        "%s%s (line %d)" what (if blank then " after a blank line" else "") line

(** The lines of [text] around [line]. *)
let lines_around text line =
    String.split_on_char '\n' text
  |> List.mapi (fun index text -> index + 1, text)
  |> List.filter (fun (index, _) -> abs (index - line) <= 5)
  |> List.map (fun (index, text) -> Printf.sprintf "%5d %s" index text)
  |> String.concat "\n"

type difference = { summary : string; details : string }

let check ~filename source formatted =
  let same original result =
      original.kind = result.kind && String.equal original.text result.text
    && original.blank = result.blank
  in
  let rec compare original result =
    match original, result with
    | [], [] -> Ok ()
    | o :: original, r :: result when same o r -> compare original result
    | _ ->
        let line =
          match result with
          | { line; _ } :: _ -> line
          | [] -> List.length (String.split_on_char '\n' formatted)
        in
        Error
          { summary =
              Printf.sprintf
                "%s is formatted as %s" (describe original) (describe result)
          ;
            details = "Formatted source:\n" ^ lines_around formatted line;
          }
  in
  compare (lexemes ~filename source) (lexemes ~filename formatted)
