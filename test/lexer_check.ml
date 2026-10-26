(**************************************************************************)
(*                                                                        *)
(*  Copyright (C) 2000-2026 LexiFi SAS. All rights reserved.             *)
(*                                                                        *)
(*  No part of this document may be reproduced or transmitted in any     *)
(*  form or for any purpose without the express permission of LexiFi SAS.*)
(*                                                                        *)
(**************************************************************************)

let check_roundtrip filename input =
  let output = Ocst.source_of_tokens (Ocst.lex input) in
  if input <> output then begin let rec first_difference offset =
      if offset = String.length input || offset = String.length output then
        offset
      else if input.[offset] = output.[offset] then
        first_difference (offset + 1)
      else offset
    in
    failwith
      ( Printf.sprintf
          "%s: round trip differs at byte %d" filename (first_difference 0)
      )
  end

(* Conversion of OCST tokens to the tokens of the OCaml parser. *)

module Compiler_lexer = Lexer
module Compiler_parser = Parser
open Ocst

let normalize name =
  match Misc.Utf8_lexeme.normalize name with Ok name | Error name -> name

let location start stop =
  { Location.loc_start = start; loc_end = stop; loc_ghost = false }

let parser_token (span : Token_position.span) (token : Token.any_token)
  : Compiler_parser.token
  =
    let open Compiler_parser in
    match token.kind with
    | `EOF -> EOF
    | `AND -> AND
    | `AS -> AS
    | `ASSERT -> ASSERT
    | `BEGIN -> BEGIN
    | `CLASS -> CLASS
    | `CONSTRAINT -> CONSTRAINT
    | `DO -> DO
    | `DONE -> DONE
    | `DOWNTO -> DOWNTO
    | `EFFECT -> EFFECT
    | `ELSE -> ELSE
    | `END -> END
    | `EXCEPTION -> EXCEPTION
    | `EXTERNAL -> EXTERNAL
    | `FALSE -> FALSE
    | `FOR -> FOR
    | `FUN -> FUN
    | `FUNCTION -> FUNCTION
    | `FUNCTOR -> FUNCTOR
    | `IF -> IF
    | `IN -> IN
    | `INCLUDE -> INCLUDE
    | `INHERIT -> INHERIT
    | `INITIALIZER -> INITIALIZER
    | `LAZY -> LAZY
    | `LET -> LET
    | `MATCH -> MATCH
    | `METHOD -> METHOD
    | `MODULE -> MODULE
    | `MUTABLE -> MUTABLE
    | `NEW -> NEW
    | `NONREC -> NONREC
    | `OBJECT -> OBJECT
    | `OF -> OF
    | `OPEN -> OPEN
    | `OR -> OR
    | `PRIVATE -> PRIVATE
    | `REC -> REC
    | `SIG -> SIG
    | `STRUCT -> STRUCT
    | `THEN -> THEN
    | `TO -> TO
    | `TRUE -> TRUE
    | `TRY -> TRY
    | `TYPE -> TYPE
    | `VAL -> VAL
    | `VIRTUAL -> VIRTUAL
    | `WHEN -> WHEN
    | `WHILE -> WHILE
    | `WITH -> WITH
    | `UNDERSCORE -> UNDERSCORE
    | `QUOTE -> QUOTE
    | `BACKQUOTE -> BACKQUOTE
    | `LIDENT name -> LIDENT (Token.Lident.to_string name)
    | `UIDENT name -> UIDENT (Token.Uident.to_string name)
    | `LABEL name -> LABEL (Token.Lident.to_string name)
    | `OPTLABEL name -> OPTLABEL (Token.Lident.to_string name)
    | `INT (literal : Token.integer_literal) ->
        INT (Token.Integer_digits.to_string literal.digits, literal.suffix)
    | `FLOAT (literal : Token.float_literal) ->
        FLOAT (Token.Float_digits.to_string literal.digits, literal.suffix)
    | `STRING (literal : Token.string_literal) ->
        let opening, delimiter =
          match literal.delimiter with
          | None -> "\"", None
          | Some delimiter ->
              let delimiter = Token.Quoted_string_delimiter.to_string delimiter
              in
              "{" ^ delimiter ^ "|", Some (normalize delimiter)
        in
        let start = Token_position.advance span.start opening in
        let stop = Token_position.advance start literal.contents in
        STRING (literal.value, location start stop, delimiter)
    | `CHAR (literal : Token.char_literal) -> CHAR literal.value
    | `QUOTED_STRING_EXPR q | `QUOTED_STRING_ITEM q ->
        let prefix =
          match token.kind with
            `QUOTED_STRING_ITEM _ -> "{%%"
          | _ -> "{%"
        in
        let id = normalize q.id in
        let shift = String.length prefix in
        let id_start =
          { span.start with pos_cnum = span.start.pos_cnum + shift }
        in
        let id_stop =
          { id_start with pos_cnum = id_start.pos_cnum + String.length id }
        in
        let start =
          Token_position.advance
            span.start (prefix ^ q.id ^ q.blanks ^ q.delimiter ^ "|")
        in
        let stop = Token_position.advance start q.contents in
        let payload =
          ( id, location id_start id_stop, q.value,
            location start stop, Some (normalize q.delimiter)
          )
        in
        begin match token.kind with
        | `QUOTED_STRING_ITEM _ -> QUOTED_STRING_ITEM payload
        | _ -> QUOTED_STRING_EXPR payload
        end
    | `LPAREN -> LPAREN
    | `RPAREN -> RPAREN
    | `LBRACE -> LBRACE
    | `RBRACE -> RBRACE
    | `LBRACKET -> LBRACKET
    | `RBRACKET -> RBRACKET
    | `LBRACKETBAR -> LBRACKETBAR
    | `BARRBRACKET -> BARRBRACKET
    | `LBRACKETLESS -> LBRACKETLESS
    | `LBRACKETGREATER -> LBRACKETGREATER
    | `LBRACELESS -> LBRACELESS
    | `GREATERRBRACE -> GREATERRBRACE
    | `GREATERRBRACKET -> GREATERRBRACKET
    | `LBRACKETAT -> LBRACKETAT
    | `LBRACKETATAT -> LBRACKETATAT
    | `LBRACKETATATAT -> LBRACKETATATAT
    | `LBRACKETPERCENT -> LBRACKETPERCENT
    | `LBRACKETPERCENTPERCENT -> LBRACKETPERCENTPERCENT
    | `COMMA -> COMMA
    | `SEMI -> SEMI
    | `SEMISEMI -> SEMISEMI
    | `BAR -> BAR
    | `BARBAR -> BARBAR
    | `COLON -> COLON
    | `COLONCOLON -> COLONCOLON
    | `EQUAL -> EQUAL
    | `MINUSGREATER -> MINUSGREATER
    | `DOT -> DOT
    | `DOTDOT -> DOTDOT
    | `LESSMINUS -> LESSMINUS
    | `COLONEQUAL -> COLONEQUAL
    | `COLONGREATER -> COLONGREATER
    | `LESS -> LESS
    | `GREATER -> GREATER
    | `AMPERSAND -> AMPERSAND
    | `AMPERAMPER -> AMPERAMPER
    | `PLUS -> PLUS
    | `PLUSDOT -> PLUSDOT
    | `PLUSEQ -> PLUSEQ
    | `MINUS -> MINUS
    | `MINUSDOT -> MINUSDOT
    | `PERCENT -> PERCENT
    | `HASH -> HASH
    | `BANG -> BANG
    | `TILDE -> TILDE
    | `QUESTION -> QUESTION
    | `STAR -> STAR
    | `PREFIXOP op -> PREFIXOP (Token.Prefixop.to_string op)
    | `HASHOP op -> HASHOP (Token.Hashop.to_string op)
    | `INFIXOP0 op -> INFIXOP0 (Token.Infixop0.to_string op)
    | `INFIXOP1 op -> INFIXOP1 (Token.Infixop1.to_string op)
    | `INFIXOP2 op -> INFIXOP2 (Token.Infixop2.to_string op)
    | `INFIXOP3 op -> INFIXOP3 (Token.Infixop3.to_string op)
    | `INFIXOP4 op -> INFIXOP4 (Token.Infixop4.to_string op)
    | `DOTOP op -> DOTOP (Token.Dotop.to_string op)
    | `LETOP op -> LETOP (Token.Letop.to_string op)
    | `ANDOP op -> ANDOP (Token.Andop.to_string op)
    | `DOCSTRING text ->
        DOCSTRING
          ( Docstrings.docstring
              (Token.docstring_body text) (location span.start span.stop)
          )
    | `ERROR _ -> assert false

type lexed =
  | Lexed of Compiler_parser.token * Lexing.position * Lexing.position
  | Lexing_failed

(* The tokens of the OCaml lexer, with its doc comments, without its comments
   and newlines. *)
let compiler_tokens filename source =
  Compiler_lexer.init ();
  let lexbuf = Lexing.from_string source in
  Location.init lexbuf filename;
  let rec loop tokens =
    match Compiler_lexer.token_with_comments lexbuf with
    | Compiler_parser.EOF as token ->
        List.rev
          (Lexed (token, lexbuf.lex_start_p, lexbuf.lex_curr_p) :: tokens)
    | Compiler_parser.COMMENT _ | Compiler_parser.EOL -> loop tokens
    | Compiler_parser.DOCSTRING doc as token ->
        let loc = Docstrings.docstring_loc doc in
        loop (Lexed (token, loc.loc_start, loc.loc_end) :: tokens)
    | token ->
        loop (Lexed (token, lexbuf.lex_start_p, lexbuf.lex_curr_p) :: tokens)
    | exception Compiler_lexer.Error _ -> List.rev (Lexing_failed :: tokens)
  in
  loop []

let describe_position (position : Lexing.position) =
  Printf.sprintf
    "%s:%d:%d" position.pos_fname position.pos_lnum
    (position.pos_cnum - position.pos_bol)

let check_tokens filename source =
  let tokens = Ocst.lex source in
  let spans = Token_position.spans ~filename tokens in
  let rec compare index ocst compiler =
    match ocst, compiler with
    | { Token.kind = `ERROR _; _ } :: _, Lexing_failed :: _ -> ()
    | { Token.kind = `ERROR error; _ } :: _, _ ->
        failwith
          ( Printf.sprintf
              "%s: OCST lexing error on %S at %s, the compiler lexer succeeds"
              filename error.Token.text (describe_position spans.(index).start)
          )
    | _, Lexing_failed :: _ ->
        failwith
          ( Printf.sprintf
              "%s: the compiler lexer fails at token %d, OCST does not"
              filename index
          )
    | token :: ocst, Lexed (expected, start, stop) :: compiler ->
        let span = spans.(index) in
        let actual = parser_token span token in
        if actual <> expected || span.start <> start || span.stop <> stop then
          failwith
            ( Printf.sprintf
                "%s: token %S at %s differs from the compiler token at %s"
                filename (Token.text token)
                (describe_position span.start) (describe_position start)
            )
        ;
        compare (index + 1) ocst compiler
    | [], [] -> ()
    | _ -> failwith (filename ^ ": token counts differ")
  in
  compare 0 tokens (compiler_tokens filename source)

let check filename source =
  Compiler_lexer.print_warnings := false;
  check_roundtrip filename source;
  check_tokens filename source
