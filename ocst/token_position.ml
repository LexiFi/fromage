type span = { start : Lexing.position; stop : Lexing.position }

let initial ~filename =
  { Lexing.pos_fname = filename; pos_lnum = 1; pos_bol = 0; pos_cnum = 0 }

let advance position text =
  (* The line and its start after the newlines from [offset]. *)
  let rec newlines line bol offset =
    match String.index_from_opt text offset '\n' with
    | Some newline ->
        newlines
          (line + 1) (position.Lexing.pos_cnum + newline + 1) (newline + 1)
    | None -> line, bol
  in
  let line, bol = newlines position.Lexing.pos_lnum position.Lexing.pos_bol 0 in
  { position with
    Lexing.pos_cnum = position.Lexing.pos_cnum + String.length text;
    pos_lnum = line; pos_bol = bol
  }

(* A line directive [# num "name" ...] sets the file name and the line number of
   the next line, as [Lexer.update_loc] does. *)
let line_directive position text =
  let position = advance position text in
  let length = String.length text in
  let rec skip_blanks index =
    if index < length && (text.[index] = ' ' || text.[index] = '\t') then
      skip_blanks (index + 1)
    else index
  in
  let rec digits index =
    if index < length && text.[index] >= '0' && text.[index] <= '9' then
      digits (index + 1)
    else index
  in
  let number_start = skip_blanks 1 in
  let number_stop = digits number_start in
  let name_start = skip_blanks number_stop + 1 in
  let name_stop = String.index_from text name_start '"' in
  let line =
    int_of_string (String.sub text number_start (number_stop - number_start))
  in
  { position with
    Lexing.pos_fname = String.sub text name_start (name_stop - name_start);
    pos_lnum = line - 1; pos_bol = position.Lexing.pos_cnum
  }

let skip_trivia position =
  function
  | Token.Whitespace text | Token.Comment text | Token.Warning (_, text) ->
      advance position text
  | Token.Line_directive text -> line_directive position text

let skip_leading position (token : _ Token.token) =
  List.fold_left skip_trivia position token.leading

let spans ~filename tokens =
  let position = ref (initial ~filename) in
  Array.of_list
    ( List.map
        ( fun token ->
            let start = skip_leading !position token in
            let stop = advance start (Token.text token) in
            position := stop; { start; stop }
        )
        tokens
    )
