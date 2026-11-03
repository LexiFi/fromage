(** The layout rules of the sources of OCST (see ocst/HACKING.md). The
    length of the lines is the formatter's, which pretty_expressive keeps
    within the width when a layout can.
    - a bracket ([(], [\[], [{], [\[|]...), a [begin] closed by [end], a [let]
      closed by [in] or a [fun] closed by [->] is closed on its line, or in its
      column (or to the left of it, for a bracket glued after a path or a
      label, and for [then begin] and [else begin], in the column where their
      line starts) by a token which starts its line;
    - a bracket closed on another line with content on its line has that
      content 2 columns after it, so that the next lines of the content can
      be indented after its first token, or 4 for the first operand of a
      chain of operators whose operators are 2 columns after it;
    - the indentation of a line of code is even, and increases by at most 4
      from a line of code to the next, where the content of the line starts:
      after the items of two columns at its start ([| ], [( ], [\[ ]), as if
      each was on its own line;
    - in a bracket, a line after a separator ([;] or [,]) starts in the
      column of the first element of the bracket: the elements of a list
      start in the same column on all its lines;
    - there are never two empty lines in a row (outside strings and
      comments). *)

let max_indentation_step = 4

type opening =
  | Bracket (** A checked bracket. *) | Begin | Let | Fun
  | Block (** [struct], [sig], [object] and [do]: not checked. *)

let opening : Ocst.Token.any_kind -> opening option =
  function
  | `LPAREN | `LBRACE | `LBRACKET | `LBRACKETBAR | `LBRACKETLESS
    | `LBRACKETGREATER | `LBRACELESS | `LBRACKETAT | `LBRACKETATAT
    | `LBRACKETATATAT | `LBRACKETPERCENT | `LBRACKETPERCENTPERCENT
  -> Some Bracket
  | `LET | `LETOP _ -> Some Let
  | `FUN -> Some Fun
  | `BEGIN -> Some Begin
  | `STRUCT | `SIG | `OBJECT | `DO -> Some Block
  | _ -> None

let is_infix_operator =
  function
  | `INFIXOP0 _ | `INFIXOP1 _ | `INFIXOP2 _ | `INFIXOP3 _ | `INFIXOP4 _
    | `HASHOP _ | `PLUS | `PLUSDOT | `PLUSEQ | `MINUS | `MINUSDOT
    | `STAR | `PERCENT | `EQUAL | `LESS | `GREATER | `AMPERSAND
    | `AMPERAMPER | `OR | `BARBAR | `COLONCOLON | `COLONEQUAL | `LESSMINUS
  -> true
  | _ -> false

let is_closing_bracket =
  function
  | `RPAREN | `RBRACE | `RBRACKET | `BARRBRACKET
    | `GREATERRBRACE | `GREATERRBRACKET
  -> true
  | _ -> false

(** The errors of a source: their lines and messages, in order. *)
let errors ~filename source =
  let tokens = Ocst.lex source in
  let spans = Ocst.Token_position.spans ~filename tokens in
  let errors = ref [] in
  let error line message = errors := (line, message) :: !errors in
  let lines = Array.of_list (String.split_on_char '\n' source) in
  let position (position : Lexing.position) =
    position.pos_lnum, position.pos_cnum - position.pos_bol
  in
  (* The column of the first token of each line, if it starts the line. *)
  let first_tokens = Array.make (Array.length lines + 1) None in
  (* The column where the content of each line starts: after the items of two
     columns at its start ([| ], [( ], [\[ ]), which are collapsed lines. In
     [| ( f], [f] would be two lines below, indented by 4. *)
  let contents = Array.make (Array.length lines + 1) None in
  (* Whether each line starts with an infix operator. *)
  let operator_lines = Array.make (Array.length lines + 1) false in
  let collapsing = ref false in
  let stack = ref [] in
  (* A bracket closed on another line than its own: closed in its column, and
     by a token which starts its line. *)
  let close what (line, column) (line', column') =
    if line <> line' then begin if column <> column' then
        error
          line
          ( Printf.sprintf
              "%s at %d:%d closed at %d:%d" what line column line' column'
          )
      ;
      if
          line' < Array.length first_tokens
        && first_tokens.(line') <> Some column'
      then
        error
          line'
          ( Printf.sprintf
              "%s at %d:%d closed at %d:%d, which does not start its line"
              what line column line' column'
          )
    end
  in
  let previous_token = ref None
  and module_path = ref None
  (* The position of the first token in each bracket, by the position of the
     bracket on the stack. *)
  and first_elements = Hashtbl.create 16
  (* The position of the first token after each bracket, by the position of
     the bracket on the stack, and the kind of the bracket. *)
  and after_brackets = Hashtbl.create 16
  (* The brackets in the value of a field ([{ x = a, b; ...}]), from [=] to
     the next [;]: their commas are those of the value. *)
  and in_values = Hashtbl.create 16
  (* The brackets of attributes and extensions, whose identifier is glued to
     the bracket ([\[@@attr]): not checked by [content_after_bracket]. *)
  and attribute_brackets = Hashtbl.create 16
  in
  (* The first operand of a chain of operators, which starts the content of
     a bracket, 4 columns after it: the next line which starts to its left
     starts with an operator, 2 columns after the bracket, as in [(   a],
     then [  && b]. *)
  let chain_operand (line, column) content_column =
    let rec next line' =
      if line' >= Array.length first_tokens then false
      else
        match first_tokens.(line') with
        | Some column' when column' < content_column ->
            column' = column + 2 && operator_lines.(line')
        | _ -> next (line' + 1)
    in
    content_column = column + 4 && next (line + 1)
  in
  (* A bracket closed on another line, with content on its line: the content
     starts 2 columns after the bracket (or the path or label it is glued
     to), so that its next lines can be indented after its first token by 2
     or 4, as in [( f], or 4 for the first operand of a chain
     ([chain_operand]); else the content starts the next line, as after
     [\[|]. *)
  let content_after_bracket ((line, column) as bracket) (line', _) =
    match Hashtbl.find_opt after_brackets bracket with
    | _ when Hashtbl.mem attribute_brackets bracket -> ()
    | Some (content_line, content_column)
      when
          line' <> line && content_line = line && content_column <> column + 2
        && not (chain_operand bracket content_column)
    ->
        error
          line
          ( Printf.sprintf
              "content at %d:%d after the bracket at %d:%d, which is not \
               closed on its line: it should start the next line"
              content_line content_column line column
          )
    | _ -> ()
  in
  List.iteri
    ( fun index (token : Ocst.Token.any_token) ->
        let start = position spans.(index).start in
        let line, column = start in
        let previous = !previous_token in
        previous_token := Some (token.kind, start);
        (* The start of the path [M.N.] or [x.%] which ends before the
           token. *)
        begin match token.kind, previous with
        | (`UIDENT _ | `LIDENT _), Some (`DOT, _) -> ()
        | (`UIDENT _ | `LIDENT _), _ -> module_path := Some start
        | (`DOT | `DOTOP _), Some ((`UIDENT _ | `LIDENT _), _) -> ()
        | _, Some ((`DOT | `DOTOP _), _) -> ()
        | _ -> module_path := None
        end;
        let starts_line =
          line < Array.length first_tokens && first_tokens.(line) = None
        in
        begin match !stack, previous with
        | (Bracket, _) :: _, _ when is_closing_bracket token.kind -> ()
        | (Bracket, bracket) :: _, _
          when not (Hashtbl.mem first_elements bracket)
        ->
            Hashtbl.add first_elements bracket start;
            Hashtbl.replace after_brackets bracket start
        | (Bracket, bracket) :: _, Some (`COMMA, _)
          when Hashtbl.mem in_values bracket
        -> ()
        | (Bracket, bracket) :: _, Some ((`SEMI | `COMMA), _) when starts_line
        ->
            let first_line, first_column = Hashtbl.find first_elements bracket
            in
            (* The element of a chain operand starts where its operators
               start. *)
            if
                first_line < line
              && first_column <> column
              &&
                not
                  ( first_column = column + 2
                    && chain_operand bracket first_column
                  )
            then
              error
                line
                ( Printf.sprintf
                    "element at column %d, the first one of its bracket at %d:%d"
                    column first_line first_column
                )
        | _ -> ()
        end;
        if starts_line then begin
          first_tokens.(line) <- Some column;
          operator_lines.(line) <- is_infix_operator token.kind;
          contents.(line) <- Some column;
          collapsing := true
        end
        else if !collapsing then begin match previous, contents.(line) with
        | Some (previous_kind, (previous_line, previous_column)), Some content
          when
              previous_line = line
            && previous_column = content
            && column = content + 2
            &&
                String.length (Ocst.Token.text (Ocst.Token.make previous_kind))
              <= 2
        -> contents.(line) <- Some column
        | _ -> collapsing := false
        end
        ;
        begin match opening token.kind, token.kind with
        | Some opening, _ ->
            (* A keyword after [begin] is aligned as after [( ]: [begin fun]
               is closed by [->] two columns after where [begin] is opened. *)
            let start =
              match previous, !module_path with
              | Some (`BEGIN, (begin_line, begin_column)), _
                when begin_line = line
              ->
                  let begin_column =
                    match !stack with
                    | (Begin, (_, opened)) :: _ -> opened
                    | _ -> begin_column
                  in
                  line, begin_column + 2
              (* [then begin] and [else begin] are opened where their line
                 starts: [end] is in the column of [if] or [else]. *)
              | Some ((`THEN | `ELSE), (keyword_line, _)), _
                when token.kind = `BEGIN && keyword_line = line
              -> line, Option.value contents.(line) ~default:column
              (* The bracket of a local open [M.(e)] or [M.{ x }], or of an
                 index [x.%{ i }], is opened where the path starts. *)
              | Some ((`DOT | `DOTOP _), (dot_line, _)),
                Some (path_line, path_column)
                when dot_line = line && path_line = line
              -> line, path_column
              (* The bracket of a labelled parameter [~(x : t)] or [?x:(x = 1)]
                 is opened where its label starts. *)
              | Some
                  ( (`TILDE | `QUESTION | `LABEL _ | `OPTLABEL _),
                    (label_line, label_column)
                  )
                ,
                _
                when label_line = line
              -> line, label_column
              | _ -> start
            in
            begin match token.kind with
            | `LBRACKETAT | `LBRACKETATAT | `LBRACKETATATAT
              | `LBRACKETPERCENT | `LBRACKETPERCENTPERCENT
            -> Hashtbl.replace attribute_brackets start ()
            | _ -> ()
            end;
            stack := (opening, start) :: !stack
        | None, `MINUSGREATER ->
            begin match !stack with
            | (Fun, position) :: rest ->
                close "'fun'" position start; stack := rest
            | _ -> ()
            end
        | None, `IN ->
            let rec pop =
              function
              | (Fun, _) :: rest -> pop rest
              | (Let, position) :: rest -> close "'let'" position start; rest
              | stack -> stack
            in
            stack := pop !stack
        | None, kind when is_closing_bracket kind || kind = `END || kind = `DONE
        ->
            let rec pop =
              function
              | ((Fun | Let), _) :: rest -> pop rest
              | (Bracket, position) :: rest ->
                  if is_closing_bracket kind then begin
                    close "bracket" position start;
                    content_after_bracket position start
                  end
                  ;
                  rest
              | (Begin, position) :: rest ->
                  if kind = `END then close "'begin'" position start; rest
              | (Block, _) :: rest -> rest
              | [] -> []
            in
            stack := pop !stack
        | None, _ -> ()
        end;
        (* After [fun ... ->], [let ... in], [then] or [else], the separators
           of the bracket are those of a body, not of a list. *)
        match token.kind, !stack with
        | (`MINUSGREATER | `IN | `THEN | `ELSE), (Bracket, bracket) :: _ ->
            Hashtbl.replace first_elements bracket (max_int, 0)
        | `EQUAL, (Bracket, bracket) :: _ ->
            Hashtbl.replace in_values bracket ()
        | `SEMI, (Bracket, bracket) :: _ -> Hashtbl.remove in_values bracket
        | _ -> ()
    )
    tokens
  ;
  let previous = ref None in
  Array.iteri
    ( fun index text ->
        let line = index + 1 in
        let rec blanks index =
          if index < String.length text && text.[index] = ' ' then
            blanks (index + 1)
          else index
        in
        let indentation =
          if String.trim text = "" then None else Some (blanks 0)
        in
        match indentation, first_tokens.(line) with
        | Some indentation, Some column when column = indentation ->
            if indentation mod 2 = 1 then
              error line (Printf.sprintf "odd indentation %d" indentation)
            ;
            begin match !previous with
            | Some previous when indentation > previous + max_indentation_step
            ->
                error
                  line
                  ( Printf.sprintf
                      "indentation increases by %d" (indentation - previous)
                  )
            | _ -> ()
            end;
            previous := Some (Option.value contents.(line) ~default:indentation)
        | _ -> ()
    )
    lines
  ;
  (* Two empty lines in a row: three newlines in the whitespace before a
     comment or a token. *)
  List.iteri
    ( fun index (token : Ocst.Token.any_token) ->
        List.iter
          ( function
            | Ocst.Token.Whitespace text ->
                let newlines =
                  String.fold_left
                    (fun count c -> if c = '\n' then count + 1 else count)
                    0 text
                in
                if newlines >= 3 then
                  error spans.(index).start.pos_lnum "two empty lines in a row"
            | _ -> ()
          )
          token.leading
    )
    tokens
  ;
  List.sort compare !errors
