{
(* A lossless lexer producing the same tokens as the OCaml lexer
   (ocaml/parsing/lexer.mll), keeping blanks, comments and line directives
   as trivia. The rules and their order mirror the OCaml lexer. *)

open Token

type item =
  | Trivia_item of trivia
  | Token_item of any_token

let make_any kind : any_token = make kind

exception Lexing_error of lexing_error


let keyword name : any_kind option =
  match keyword_of_string name with
  | Some keyword -> Some (keyword :> any_kind)
  | None ->
      match name with
      | "lor" | "lxor" | "mod" | "land" ->
          Some (`INFIXOP3 (Infixop3.of_string_unsafe name))
      | "lsl" | "lsr" | "asr" ->
          Some (`INFIXOP4 (Infixop4.of_string_unsafe name))
      | _ -> None

let char_for_backslash = function
  | 'n' -> '\010'
  | 'r' -> '\013'
  | 'b' -> '\008'
  | 't' -> '\009'
  | c -> c

let digit_value c =
  match c with
  | 'a' .. 'f' -> 10 + Char.code c - Char.code 'a'
  | 'A' .. 'F' -> 10 + Char.code c - Char.code 'A'
  | '0' .. '9' -> Char.code c - Char.code '0'
  | _ -> assert false

let num_value text ~base ~first ~last =
  let c = ref 0 in
  for i = first to last do
    c := (base * !c) + digit_value text.[i]
  done;
  !c

let char_for_decimal_code ~in_comment text i =
  let c = num_value text ~base:10 ~first:i ~last:(i + 2) in
  if c > 255 then
    if in_comment then 'x' else raise (Lexing_error `Illegal_escape)
  else Char.chr c

let char_for_octal_code ~in_comment text i =
  let c = num_value text ~base:8 ~first:i ~last:(i + 2) in
  if c > 255 then
    if in_comment then 'x' else raise (Lexing_error `Illegal_escape)
  else Char.chr c

let char_for_hexadecimal_code text i =
  Char.chr (num_value text ~base:16 ~first:i ~last:(i + 1))

let uchar_for_uchar_escape text =
  let first = 3 and last = String.length text - 2 in
  if last - first + 1 > 6 then raise (Lexing_error `Illegal_escape)
  else
    let cp = num_value text ~base:16 ~first ~last in
    if Uchar.is_valid cp then Uchar.unsafe_of_int cp else raise (Lexing_error `Illegal_escape)

(* #12502: newlines [\r*\n] are normalized by dropping one carriage return. *)
let add_normalized_newline buffer newline =
  let length = String.length newline in
  if length = 1 then Buffer.add_char buffer '\n'
  else Buffer.add_substring buffer newline 1 (length - 1)

let normalize_name raw =
  match Misc.Utf8_lexeme.normalize raw with
  | Ok name -> name
  | Error _ -> raise (Lexing_error `Invalid_encoding)

let ident_for_extended raw =
  let name = normalize_name raw in
  match Misc.Utf8_lexeme.validate_identifier name with
  | Misc.Utf8_lexeme.Valid -> name
  | _ -> raise (Lexing_error `Invalid_identifier_character)

let validate_delim raw =
  let name = normalize_name raw in
  if Misc.Utf8_lexeme.is_lowercase name then name
  else raise (Lexing_error `Non_lowercase_delimiter)

let validate_ext raw =
  let name = normalize_name raw in
  match Misc.Utf8_lexeme.validate_identifier ~with_dot:true name with
  | Misc.Utf8_lexeme.Valid -> name
  | _ -> raise (Lexing_error `Invalid_identifier_character)

let lax_delim raw =
  match Misc.Utf8_lexeme.normalize raw with
  | Error _ -> None
  | Ok name -> if Misc.Utf8_lexeme.is_lowercase name then Some name else None

let is_keyword name = keyword name <> None

let check_label_name ?(raw_escape = false) name =
  if Misc.Utf8_lexeme.is_capitalized name then raise (Lexing_error `Capitalized_label);
  if (not raw_escape) && is_keyword name then raise (Lexing_error `Keyword_label)

let punctuation_kind : string -> any_kind = function
  | "&" -> `AMPERSAND | "&&" -> `AMPERAMPER | "`" -> `BACKQUOTE | "'" -> `QUOTE
  | "(" -> `LPAREN | ")" -> `RPAREN | "*" -> `STAR | "," -> `COMMA
  | "->" -> `MINUSGREATER | "." -> `DOT | ".." -> `DOTDOT | ":" -> `COLON
  | "::" -> `COLONCOLON | ":=" -> `COLONEQUAL | ":>" -> `COLONGREATER
  | ";" -> `SEMI | ";;" -> `SEMISEMI | "<" -> `LESS | "<-" -> `LESSMINUS
  | "=" -> `EQUAL | "[" -> `LBRACKET | "[|" -> `LBRACKETBAR
  | "[<" -> `LBRACKETLESS | "[>" -> `LBRACKETGREATER | "]" -> `RBRACKET
  | "{" -> `LBRACE | "{<" -> `LBRACELESS | "|" -> `BAR | "||" -> `BARBAR
  | "|]" -> `BARRBRACKET | ">" -> `GREATER | ">]" -> `GREATERRBRACKET
  | "}" -> `RBRACE | ">}" -> `GREATERRBRACE | "[@" -> `LBRACKETAT
  | "[@@" -> `LBRACKETATAT | "[@@@" -> `LBRACKETATATAT
  | "[%" -> `LBRACKETPERCENT | "[%%" -> `LBRACKETPERCENTPERCENT
  | "!" -> `BANG | "+" -> `PLUS | "+." -> `PLUSDOT | "+=" -> `PLUSEQ
  | "-" -> `MINUS | "-." -> `MINUSDOT
  | text ->
      (* The lexer only calls it on the symbols above. *)
      (invalid_arg ("Cst_lexer.punctuation_kind: " ^ text) [@coverage off])

(* The state of a string being lexed: its source text and its value. *)
type string_state = { raw : Buffer.t; value : Buffer.t; in_comment : bool }

let new_string_state ~in_comment =
  { raw = Buffer.create 64; value = Buffer.create 64; in_comment }

let error_token ~reason ~text ~start =
  make_any (`ERROR { reason; text; offset_start = start; offset_stop = start + String.length text })

let at_beginning_of_line lexbuf =
  let start = Lexing.lexeme_start lexbuf in
  start = 0 || Bytes.get lexbuf.Lexing.lex_buffer (start - 1) = '\n'
}

let newline = ('\013'* '\010')
let blank = [' ' '\009' '\012']
let lowercase = ['a'-'z' '_']
let uppercase = ['A'-'Z']
let identstart = lowercase | uppercase
let identchar = ['A'-'Z' 'a'-'z' '_' '\'' '0'-'9']
let utf8 = ['\192'-'\255'] ['\128'-'\191']*
let identstart_ext = identstart | utf8
let identchar_ext = identchar | utf8
let delim_ext = (lowercase | uppercase | utf8)*

let symbolchar =
  ['!' '$' '%' '&' '*' '+' '-' '.' '/' ':' '<' '=' '>' '?' '@' '^' '|' '~']
let dotsymbolchar =
  ['!' '$' '%' '&' '*' '+' '-' '/' ':' '=' '>' '?' '@' '^' '|']
let symbolchar_or_hash =
  symbolchar | '#'
let kwdopchar =
  ['$' '&' '*' '+' '-' '/' '<' '=' '>' '@' '^' '|']

let ident_ext = identstart_ext  identchar_ext*
let extattrident = ident_ext ('.' ident_ext)*

let decimal_literal =
  ['0'-'9'] ['0'-'9' '_']*
let hex_digit =
  ['0'-'9' 'A'-'F' 'a'-'f']
let hex_literal =
  '0' ['x' 'X'] ['0'-'9' 'A'-'F' 'a'-'f']['0'-'9' 'A'-'F' 'a'-'f' '_']*
let oct_literal =
  '0' ['o' 'O'] ['0'-'7'] ['0'-'7' '_']*
let bin_literal =
  '0' ['b' 'B'] ['0'-'1'] ['0'-'1' '_']*
let int_literal =
  decimal_literal | hex_literal | oct_literal | bin_literal
let float_literal =
  ['0'-'9'] ['0'-'9' '_']*
  ('.' ['0'-'9' '_']* )?
  (['e' 'E'] ['+' '-']? ['0'-'9'] ['0'-'9' '_']* )?
let hex_float_literal =
  '0' ['x' 'X']
  ['0'-'9' 'A'-'F' 'a'-'f'] ['0'-'9' 'A'-'F' 'a'-'f' '_']*
  ('.' ['0'-'9' 'A'-'F' 'a'-'f' '_']* )?
  (['p' 'P'] ['+' '-']? ['0'-'9'] ['0'-'9' '_']* )?
let literal_modifier = ['G'-'Z' 'g'-'z']
let raw_ident_escape = "\\#"

rule token = parse
  | (blank | newline)+ as text
      { Trivia_item (Whitespace text) }
  | "_"
      { Token_item (make_any `UNDERSCORE) }
  | "~"
      { Token_item (make_any `TILDE) }
  | ".~"
      { raise (Lexing_error `Reserved_sequence) }
  | "~" (identstart identchar * as name) ':'
      { check_label_name name;
        Token_item (make_any (`LABEL (Lident.of_string_unsafe name)))
      }
  | "~" ((raw_ident_escape? as escape) (ident_ext as raw_name) as source) ':'
      { let name = ident_for_extended raw_name in
        check_label_name ~raw_escape:(escape <> "") name;
        Token_item (make_any (`LABEL (Lident.of_string_unsafe source)))
      }
  | "?"
      { Token_item (make_any `QUESTION) }
  | "?" (lowercase identchar * as name) ':'
      { check_label_name name;
        Token_item (make_any (`OPTLABEL (Lident.of_string_unsafe name)))
      }
  | "?" ((raw_ident_escape? as escape) (ident_ext as raw_name) as source) ':'
      { let name = ident_for_extended raw_name in
        check_label_name ~raw_escape:(escape <> "") name;
        Token_item (make_any (`OPTLABEL (Lident.of_string_unsafe source)))
      }
  | lowercase identchar * as name
      { let kind =
          match keyword name with
          | Some kind -> kind
          | None -> `LIDENT (Lident.of_string_unsafe name)
        in
        Token_item (make_any kind)
      }
  | uppercase identchar * as name
      { Token_item (make_any (`UIDENT (Uident.of_string_unsafe name))) }
  | ((raw_ident_escape? as escape) (ident_ext as raw_name)) as source
      { let name = ident_for_extended raw_name in
        if Misc.Utf8_lexeme.is_capitalized name then
          begin
            if escape = "" then Token_item (make_any (`UIDENT (Uident.of_string_unsafe source)))
            else raise (Lexing_error `Capitalized_raw_identifier)
          end
        else Token_item (make_any (`LIDENT (Lident.of_string_unsafe source)))
      }
  | int_literal as lit
      { let digits = Integer_digits.of_string_unsafe lit in
        Token_item (make_any (`INT { digits; suffix = None }))
      }
  | (int_literal as lit) (literal_modifier as modif)
      { let digits = Integer_digits.of_string_unsafe lit in
        Token_item (make_any (`INT { digits; suffix = Some modif }))
      }
  | float_literal | hex_float_literal as lit
      { let digits = Float_digits.of_string_unsafe lit in
        Token_item (make_any (`FLOAT { digits; suffix = None }))
      }
  | (float_literal | hex_float_literal as lit) (literal_modifier as modif)
      { let digits = Float_digits.of_string_unsafe lit in
        Token_item (make_any (`FLOAT { digits; suffix = Some modif }))
      }
  | (float_literal | hex_float_literal | int_literal) identchar+
      { raise (Lexing_error `Invalid_literal) }
  | "\""
      { let state = new_string_state ~in_comment:false in
        string state lexbuf;
        let contents = Buffer.contents state.raw in
        let value = Buffer.contents state.value in
        Token_item (make_any (`STRING { value; delimiter = None; contents }))
      }
  | "{" (delim_ext as raw_name) '|'
      { let delim = validate_delim raw_name in
        let state = new_string_state ~in_comment:false in
        ignore (quoted_string delim state lexbuf);
        let value = Buffer.contents state.value in
        let delimiter = Some (Quoted_string_delimiter.of_string_unsafe raw_name) in
        let contents = Buffer.contents state.raw in
        Token_item (make_any (`STRING { value; delimiter; contents }))
      }
  | "{%" (extattrident as raw_id) "|"
      { ignore (validate_ext raw_id);
        let state = new_string_state ~in_comment:false in
        ignore (quoted_string "" state lexbuf);
        Token_item
          ( make_any
              ( `QUOTED_STRING_EXPR
                  { id = raw_id; blanks = ""; delimiter = "";
                    contents = Buffer.contents state.raw;
                    value = Buffer.contents state.value
                  }
              )
          )
      }
  | "{%" (extattrident as raw_id) (blank+ as blanks)
    (delim_ext as raw_delim) "|"
      { ignore (validate_ext raw_id);
        let delim = validate_delim raw_delim in
        let state = new_string_state ~in_comment:false in
        ignore (quoted_string delim state lexbuf);
        Token_item
          ( make_any
              ( `QUOTED_STRING_EXPR
                  { id = raw_id; blanks; delimiter = raw_delim;
                    contents = Buffer.contents state.raw;
                    value = Buffer.contents state.value
                  }
              )
          )
      }
  | "{%%" (extattrident as raw_id) "|"
      { ignore (validate_ext raw_id);
        let state = new_string_state ~in_comment:false in
        ignore (quoted_string "" state lexbuf);
        Token_item
          ( make_any
              ( `QUOTED_STRING_ITEM
                  { id = raw_id; blanks = ""; delimiter = "";
                    contents = Buffer.contents state.raw;
                    value = Buffer.contents state.value
                  }
              )
          )
      }
  | "{%%" (extattrident as raw_id) (blank+ as blanks)
    (delim_ext as raw_delim) "|"
      { ignore (validate_ext raw_id);
        let delim = validate_delim raw_delim in
        let state = new_string_state ~in_comment:false in
        ignore (quoted_string delim state lexbuf);
        Token_item
          ( make_any
              ( `QUOTED_STRING_ITEM
                  { id = raw_id; blanks; delimiter = raw_delim;
                    contents = Buffer.contents state.raw;
                    value = Buffer.contents state.value
                  }
              )
          )
      }
  | "\'" (newline as contents) "\'"
      { Token_item (make_any (`CHAR { value = '\n'; contents })) }
  | "\'" ([^ '\\' '\'' '\010' '\013'] as c) "\'"
      { let contents = String.make 1 c in
        Token_item (make_any (`CHAR { value = c; contents }))
      }
  | "\'" ("\\" (['\\' '\'' '\"' 'n' 't' 'b' 'r' ' '] as c) as contents) "\'"
      { let value = char_for_backslash c in
        Token_item (make_any (`CHAR { value; contents }))
      }
  | "\'" ("\\" ['0'-'9'] ['0'-'9'] ['0'-'9'] as contents) "\'"
      { let value = char_for_decimal_code ~in_comment:false contents 1 in
        Token_item (make_any (`CHAR { value; contents }))
      }
  | "\'" ("\\" 'o' ['0'-'7'] ['0'-'7'] ['0'-'7'] as contents) "\'"
      { let value = char_for_octal_code ~in_comment:false contents 2 in
        Token_item (make_any (`CHAR { value; contents }))
      }
  | "\'" ("\\" 'x' ['0'-'9' 'a'-'f' 'A'-'F']
    ['0'-'9' 'a'-'f' 'A'-'F'] as contents) "\'"
      { let value = char_for_hexadecimal_code contents 2 in
        Token_item (make_any (`CHAR { value; contents }))
      }
  | "\'" "\\" [^ '#']
      { raise (Lexing_error `Illegal_escape) }
  | "\'\'"
      { raise (Lexing_error `Empty_character_literal) }
  | "(*"
      { let buffer = Buffer.create 64 in
        Buffer.add_string buffer (Lexing.lexeme lexbuf);
        comment buffer 1 lexbuf;
        Trivia_item (Comment (Buffer.contents buffer))
      }
  | "(**"
      { let buffer = Buffer.create 64 in
        Buffer.add_string buffer (Lexing.lexeme lexbuf);
        comment buffer 1 lexbuf;
        Token_item (make_any (`DOCSTRING (Buffer.contents buffer)))
      }
  | "(**" ('*'+)
      { let buffer = Buffer.create 64 in
        Buffer.add_string buffer (Lexing.lexeme lexbuf);
        comment buffer 1 lexbuf;
        Trivia_item (Comment (Buffer.contents buffer))
      }
  | "(*)"
      { let buffer = Buffer.create 64 in
        Buffer.add_string buffer (Lexing.lexeme lexbuf);
        comment buffer 1 lexbuf;
        Trivia_item (Comment (Buffer.contents buffer))
      }
  | "(*" ('*'*) "*)" as text
      { if text = "(**)" then Token_item (make_any (`DOCSTRING text))
        else Trivia_item (Comment text)
      }
  | "*)"
      { lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_curr_pos - 1;
        Token_item (make_any `STAR)
      }
  | "#"
      { if not (at_beginning_of_line lexbuf) then Token_item (make_any `HASH)
        else
          let start = lexbuf.Lexing.lex_curr_pos in
          match directive lexbuf with
          | text -> Trivia_item (Line_directive ("#" ^ text))
          | exception Failure _ ->
              lexbuf.Lexing.lex_curr_pos <- start;
              Token_item (make_any `HASH)
      }
  | "&" | "&&" | "`" | "\'" | "(" | ")" | "*" | "," | "->" | "." | ".."
  | ":" | "::" | ":=" | ":>" | ";" | ";;" | "<" | "<-" | "=" | "[" | "[|"
  | "[<" | "[>" | "]" | "{" | "{<" | "|" | "||" | "|]" | ">" | ">]" | "}"
  | ">}" | "[@" | "[@@" | "[@@@" | "[%" | "[%%" | "!" | "+" | "+." | "+="
  | "-" | "-." as text
      { Token_item (make_any (punctuation_kind text)) }
  | "." (dotsymbolchar symbolchar* as op)
      { Token_item (make_any (`DOTOP (Dotop.of_string_unsafe op))) }
  | "!="
      { Token_item (make_any (`INFIXOP0 (Infixop0.of_string_unsafe "!="))) }
  | "!" symbolchar_or_hash + as op
      { Token_item (make_any (`PREFIXOP (Prefixop.of_string_unsafe op))) }
  | ['~' '?'] symbolchar_or_hash + as op
      { Token_item (make_any (`PREFIXOP (Prefixop.of_string_unsafe op))) }
  | ['=' '<' '>' '|' '&' '$'] symbolchar * as op
      { Token_item (make_any (`INFIXOP0 (Infixop0.of_string_unsafe op))) }
  | ['@' '^'] symbolchar * as op
      { Token_item (make_any (`INFIXOP1 (Infixop1.of_string_unsafe op))) }
  | ['+' '-'] symbolchar * as op
      { Token_item (make_any (`INFIXOP2 (Infixop2.of_string_unsafe op))) }
  | "**" symbolchar * as op
      { Token_item (make_any (`INFIXOP4 (Infixop4.of_string_unsafe op))) }
  | '%'
      { Token_item (make_any `PERCENT) }
  | ['*' '/' '%'] symbolchar * as op
      { Token_item (make_any (`INFIXOP3 (Infixop3.of_string_unsafe op))) }
  | '#' symbolchar_or_hash + as op
      { Token_item (make_any (`HASHOP (Hashop.of_string_unsafe op))) }
  | "let" kwdopchar dotsymbolchar * as op
      { Token_item (make_any (`LETOP (Letop.of_string_unsafe op))) }
  | "and" kwdopchar dotsymbolchar * as op
      { Token_item (make_any (`ANDOP (Andop.of_string_unsafe op))) }
  | eof
      { raise End_of_file }
  | _
      { raise (Lexing_error `Illegal_character) }

and directive = parse
  | ([' ' '\t']* (['0'-'9']+ as num) [' ' '\t']*
        ("\"" ([^ '\010' '\013' '\"' ] *) "\""))
        [^ '\010' '\013'] * as text
      { match int_of_string num with
        | exception _ -> raise (Lexing_error `Invalid_directive)
        | _ -> text
      }

and comment buffer depth = parse
  | "(*"
      { Buffer.add_string buffer (Lexing.lexeme lexbuf);
        comment buffer (depth + 1) lexbuf
      }
  | "*)"
      { Buffer.add_string buffer (Lexing.lexeme lexbuf);
        if depth > 1 then comment buffer (depth - 1) lexbuf
      }
  | "\""
      { Buffer.add_char buffer '"';
        let state = new_string_state ~in_comment:true in
        (try string state lexbuf with Lexing_error `Unterminated_string -> raise (Lexing_error `Unterminated_comment));
        Buffer.add_buffer buffer state.raw;
        Buffer.add_char buffer '"';
        comment buffer depth lexbuf
      }
  | "{" ('%' '%'? extattrident blank*)? (delim_ext as raw_delim) "|"
      { Buffer.add_string buffer (Lexing.lexeme lexbuf);
        match lax_delim raw_delim with
        | None -> comment buffer depth lexbuf
        | Some delim ->
            let state = new_string_state ~in_comment:true in
            let closing =
              try quoted_string delim state lexbuf
              with Lexing_error `Unterminated_quoted_string ->
                raise (Lexing_error `Unterminated_comment)
            in
            Buffer.add_buffer buffer state.raw;
            Buffer.add_string buffer ("|" ^ closing ^ "}");
            comment buffer depth lexbuf
      }
  | "\'\'"
      { Buffer.add_string buffer (Lexing.lexeme lexbuf);
        comment buffer depth lexbuf
      }
  | "\'" newline "\'"
      { Buffer.add_string buffer (Lexing.lexeme lexbuf);
        comment buffer depth lexbuf
      }
  | "\'" [^ '\\' '\'' '\010' '\013' ] "\'"
      { Buffer.add_string buffer (Lexing.lexeme lexbuf);
        comment buffer depth lexbuf
      }
  | "\'\\" ['\\' '\"' '\'' 'n' 't' 'b' 'r' ' '] "\'"
      { Buffer.add_string buffer (Lexing.lexeme lexbuf);
        comment buffer depth lexbuf
      }
  | "\'\\" ['0'-'9'] ['0'-'9'] ['0'-'9'] "\'"
      { Buffer.add_string buffer (Lexing.lexeme lexbuf);
        comment buffer depth lexbuf
      }
  | "\'\\" 'o' ['0'-'3'] ['0'-'7'] ['0'-'7'] "\'"
      { Buffer.add_string buffer (Lexing.lexeme lexbuf);
        comment buffer depth lexbuf
      }
  | "\'\\" 'x' ['0'-'9' 'a'-'f' 'A'-'F'] ['0'-'9' 'a'-'f' 'A'-'F'] "\'"
      { Buffer.add_string buffer (Lexing.lexeme lexbuf);
        comment buffer depth lexbuf
      }
  | eof
      { raise (Lexing_error `Unterminated_comment) }
  | newline
      { Buffer.add_string buffer (Lexing.lexeme lexbuf);
        comment buffer depth lexbuf
      }
  | ident_ext
      { Buffer.add_string buffer (Lexing.lexeme lexbuf);
        comment buffer depth lexbuf
      }
  | _
      { Buffer.add_string buffer (Lexing.lexeme lexbuf);
        comment buffer depth lexbuf
      }

and string state = parse
  | '\"'
      { () }
  | '\\' newline ([' ' '\t'] * as space)
      { Buffer.add_string state.raw (Lexing.lexeme lexbuf);
        ignore space;
        string state lexbuf
      }
  | '\\' (['\\' '\'' '\"' 'n' 't' 'b' 'r' ' '] as c)
      { Buffer.add_string state.raw (Lexing.lexeme lexbuf);
        Buffer.add_char state.value (char_for_backslash c);
        string state lexbuf
      }
  | '\\' ['0'-'9'] ['0'-'9'] ['0'-'9']
      { let text = Lexing.lexeme lexbuf in
        Buffer.add_string state.raw text;
        Buffer.add_char state.value (char_for_decimal_code ~in_comment:state.in_comment text 1);
        string state lexbuf
      }
  | '\\' 'o' ['0'-'7'] ['0'-'7'] ['0'-'7']
      { let text = Lexing.lexeme lexbuf in
        Buffer.add_string state.raw text;
        Buffer.add_char state.value (char_for_octal_code ~in_comment:state.in_comment text 2);
        string state lexbuf
      }
  | '\\' 'x' ['0'-'9' 'a'-'f' 'A'-'F'] ['0'-'9' 'a'-'f' 'A'-'F']
      { let text = Lexing.lexeme lexbuf in
        Buffer.add_string state.raw text;
        Buffer.add_char state.value (char_for_hexadecimal_code text 2);
        string state lexbuf
      }
  | '\\' 'u' '{' hex_digit+ '}'
      { let text = Lexing.lexeme lexbuf in
        Buffer.add_string state.raw text;
        Buffer.add_utf_8_uchar state.value (uchar_for_uchar_escape text);
        string state lexbuf
      }
  | '\\' _
      { let text = Lexing.lexeme lexbuf in
        Buffer.add_string state.raw text;
        Buffer.add_string state.value text;
        string state lexbuf
      }
  | newline as nl
      { Buffer.add_string state.raw nl;
        add_normalized_newline state.value nl;
        string state lexbuf
      }
  | eof
      { raise (Lexing_error `Unterminated_string) }
  | (_ as c)
      { Buffer.add_char state.raw c;
        Buffer.add_char state.value c;
        string state lexbuf
      }

and quoted_string delim state = parse
  | newline as nl
      { Buffer.add_string state.raw nl;
        add_normalized_newline state.value nl;
        quoted_string delim state lexbuf
      }
  | eof
      { raise (Lexing_error `Unterminated_quoted_string) }
  | "|" (ident_ext? as raw_edelim) "}"
      { let edelim = normalize_name raw_edelim in
        if delim = edelim then raw_edelim
        else
          begin
            let text = Lexing.lexeme lexbuf in
            Buffer.add_string state.raw text;
            Buffer.add_string state.value text;
            quoted_string delim state lexbuf
          end
      }
  | (_ as c)
      { Buffer.add_char state.raw c;
        Buffer.add_char state.value c;
        quoted_string delim state lexbuf
      }

{

let add_trivia trivia (token : any_token) =
  { token with leading = List.rev_append trivia token.leading }

let tokenize source =
  let lexbuf = Lexing.from_string source in
  let rec loop trivia tokens =
    let start = lexbuf.Lexing.lex_curr_pos in
    match token lexbuf with
    | Trivia_item item -> loop (item :: trivia) tokens
    | Token_item item -> loop [] (add_trivia trivia item :: tokens)
    | exception End_of_file ->
        List.rev (add_trivia trivia (make_any `EOF) :: tokens)
    | exception Lexing_error reason ->
        (* An unterminated construct extends to the end of the input; any other
           error covers the text matched so far, or at least one character. *)
        let stop =
          match reason with
          | `Unterminated_comment | `Unterminated_string | `Unterminated_quoted_string ->
              String.length source
          | _ -> max lexbuf.Lexing.lex_curr_pos (start + 1)
        in
        let stop = min stop (String.length source) in
        lexbuf.Lexing.lex_curr_pos <- stop;
        let text = String.sub source start (stop - start) in
        let error = error_token ~reason ~text ~start in
        loop [] (add_trivia trivia error :: tokens)
  in
  loop [] []
}
