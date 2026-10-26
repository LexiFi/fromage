type warning = Unattached_docstring

type trivia =
  | Whitespace of string | Comment of string
  | Line_directive of string | Warning of warning * string

type lexing_error =
  [ `Illegal_escape | `Invalid_encoding | `Invalid_identifier_character
  | `Non_lowercase_delimiter | `Capitalized_label | `Keyword_label
  | `Reserved_sequence | `Capitalized_raw_identifier | `Invalid_literal
  | `Empty_character_literal | `Illegal_character | `Invalid_directive
  | `Unterminated_comment | `Unterminated_string | `Unterminated_quoted_string
  ]

type error =
  { reason : lexing_error; text : string; offset_start : int; offset_stop : int
  }

let all predicate text =
  let rec loop index =
    index = String.length text || predicate text.[index] && loop (index + 1)
  in
  loop 0

let is_ident_tail =
  function
  | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' | '\'' -> true
  | _ -> false

type keyword =
  [ `AND | `AS | `ASSERT | `BEGIN | `CLASS | `CONSTRAINT | `DO | `DONE | `DOWNTO
  | `EFFECT | `ELSE | `END | `EXCEPTION | `EXTERNAL | `FALSE | `FOR | `FUN
  | `FUNCTION | `FUNCTOR | `IF | `IN | `INCLUDE | `INHERIT | `INITIALIZER
  | `LAZY | `LET | `MATCH | `METHOD | `MODULE | `MUTABLE | `NEW | `NONREC
  | `OBJECT | `OF | `OPEN | `OR | `PRIVATE | `REC | `SIG | `STRUCT | `THEN
  | `TO | `TRUE | `TRY | `TYPE | `VAL | `VIRTUAL | `WHEN | `WHILE | `WITH
  ]

let keyword_of_string : string -> [> keyword ] option =
  function
  | "and" -> Some `AND
  | "as" -> Some `AS
  | "assert" -> Some `ASSERT
  | "begin" -> Some `BEGIN
  | "class" -> Some `CLASS
  | "constraint" -> Some `CONSTRAINT
  | "do" -> Some `DO
  | "done" -> Some `DONE
  | "downto" -> Some `DOWNTO
  | "effect" -> Some `EFFECT
  | "else" -> Some `ELSE
  | "end" -> Some `END
  | "exception" -> Some `EXCEPTION
  | "external" -> Some `EXTERNAL
  | "false" -> Some `FALSE
  | "for" -> Some `FOR
  | "fun" -> Some `FUN
  | "function" -> Some `FUNCTION
  | "functor" -> Some `FUNCTOR
  | "if" -> Some `IF
  | "in" -> Some `IN
  | "include" -> Some `INCLUDE
  | "inherit" -> Some `INHERIT
  | "initializer" -> Some `INITIALIZER
  | "lazy" -> Some `LAZY
  | "let" -> Some `LET
  | "match" -> Some `MATCH
  | "method" -> Some `METHOD
  | "module" -> Some `MODULE
  | "mutable" -> Some `MUTABLE
  | "new" -> Some `NEW
  | "nonrec" -> Some `NONREC
  | "object" -> Some `OBJECT
  | "of" -> Some `OF
  | "open" -> Some `OPEN
  | "or" -> Some `OR
  | "private" -> Some `PRIVATE
  | "rec" -> Some `REC
  | "sig" -> Some `SIG
  | "struct" -> Some `STRUCT
  | "then" -> Some `THEN
  | "to" -> Some `TO
  | "true" -> Some `TRUE
  | "try" -> Some `TRY
  | "type" -> Some `TYPE
  | "val" -> Some `VAL
  | "virtual" -> Some `VIRTUAL
  | "when" -> Some `WHEN
  | "while" -> Some `WHILE
  | "with" -> Some `WITH
  | _ -> None

let is_word_operator =
  function
  | "land" | "lor" | "lsl" | "lsr" | "lxor" | "mod" | "asr" -> true
  | _ -> false

(* The name of an identifier: raw identifiers lose their [\#] prefix and UTF-8
   identifiers are normalized, as in the OCaml lexer. *)
let identifier_name source =
  let raw =
    if String.length source >= 2 && source.[0] = '\\' && source.[1] = '#' then
      String.sub source 2 (String.length source - 2)
    else source
  in
  match Misc.Utf8_lexeme.normalize raw with Ok name | Error name -> name

let is_ascii text = all (fun c -> Char.code c < 128) text

module type SafeString =
  sig
    type t
    val of_string : string -> t option
    val of_string_exn : string -> t
    val of_string_unsafe : string -> t
    val to_string : t -> string
  end

module Safe_string
    ( Spec :
      sig
        val name : string
        val valid : string -> bool
      end
    )
  =
  struct
    type t = string
    let of_string text = if Spec.valid text then Some text else None
    let of_string_exn text =
      match of_string text with
        Some value -> value
      | None -> invalid_arg Spec.name

    let of_string_unsafe text = text
    let to_string value = value
  end

module Lident =
  struct
    type t = string
    let of_string text =
      let valid_plain name =
        String.length name > 0
        &&
        ( match name.[0] with
            'a' .. 'z' | '_' -> true
          | _ -> false
        )
        && all is_ident_tail name
      in
      let valid_ext name =
        (not (is_ascii name))
        &&
        begin match Misc.Utf8_lexeme.normalize name with
        | Ok name ->
            Misc.Utf8_lexeme.is_valid_identifier name
            && not (Misc.Utf8_lexeme.is_capitalized name)
        | Error _ -> false
        end
      in
      if String.length text > 2 && text.[0] = '\\' && text.[1] = '#' then
        let name = String.sub text 2 (String.length text - 2) in
        if valid_plain name || valid_ext name then Some text else None
      else if
        text <> "_" && valid_plain text
        && keyword_of_string text = None && not (is_word_operator text)
      then Some text
      else if valid_ext text then Some text else None

    let of_string_exn text =
      match of_string text with
        Some value -> value
      | None -> invalid_arg "Lident"

    let of_string_unsafe text = text
    let to_string = identifier_name
    let source value = value
  end

module Uident =
  struct
    type t = string
    let of_string text =
      if
        String.length text > 0
        &&
        ( match text.[0] with
            'A' .. 'Z' -> true
          | _ -> false
        )
        && all is_ident_tail text
      then Some text
      else if not (is_ascii text) then
        match Misc.Utf8_lexeme.normalize text with
        | Ok name when
            Misc.Utf8_lexeme.is_valid_identifier name
            && Misc.Utf8_lexeme.is_capitalized name
        -> Some text
        | _ -> None
      else None

    let of_string_exn text =
      match of_string text with
        Some value -> value
      | None -> invalid_arg "Uident"

    let of_string_unsafe text = text
    let to_string = identifier_name
    let source value = value
  end

module Quoted_string_delimiter =
  Safe_string
    ( struct
        let name = "Quoted_string_delimiter"
        let valid =
          all
            ( function
                'a' .. 'z' | '_' -> true
              | _ -> false
            )
      end
    )

module Numeric_text =
  struct
    let of_string text =
      if
        String.length text > 0
        &&
        ( match text.[0] with
            '0' .. '9' -> true
          | _ -> false
        )
        &&
        all
          ( function
            | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' | '.' | '+' | '-' ->
                true
            | _ -> false
          )
          text
      then Some text
      else None
  end

let is_hex text = String.length text > 1 && (text.[1] = 'x' || text.[1] = 'X')

module Integer_digits =
  Safe_string
    ( struct
        let name = "Integer_digits"
        let valid text =
          Numeric_text.of_string text <> None && not (String.contains text '.')
          && not (String.contains text '+') && not (String.contains text '-')
          &&
          ( is_hex text
            || not (String.contains text 'e' || String.contains text 'E')
          )
          &&
          not
            ( is_hex text
              && (String.contains text 'p' || String.contains text 'P')
            )
      end
    )

module Float_digits =
  Safe_string
    ( struct
        let name = "Float_digits"
        let valid text =
          Numeric_text.of_string text <> None
          && Integer_digits.of_string text = None
      end
    )

module Symbol =
  Safe_string
    ( struct
        let name = "Symbol"
        let valid text = String.length text = 1 || is_word_operator text
      end
    )

(* The operator characters of the OCaml lexer. *)
let is_symbolchar =
  function
  | '!' | '$' | '%' | '&' | '*' | '+' | '-' | '.' | '/'
    | ':' | '<' | '=' | '>' | '?' | '@' | '^' | '|' | '~'
  -> true
  | _ -> false

let is_dotsymbolchar c = c <> '.' && c <> '<' && c <> '~' && is_symbolchar c
let is_symbolchar_or_hash c = c = '#' || is_symbolchar c

let is_kwdopchar =
  function
  | '$' | '&' | '*' | '+' | '-' | '/' | '<' | '=' | '>' | '@' | '^' | '|' ->
      true
  | _ -> false

(* [text] starts with [prefix], followed by a character satisfying [first],
   then by characters satisfying [rest]. *)
let operator_text ~prefix ~first ~rest text =
  let start = String.length prefix in
  String.starts_with ~prefix text
  && String.length text > start && first text.[start]
  && all rest (String.sub text (start + 1) (String.length text - start - 1))

(* Each [valid] accepts the texts which the lexer reads as one token of that
   kind: they match its rule, and no earlier rule (such as a punctuation
   symbol) matches the same text. *)

module Prefixop =
  Safe_string
    ( struct
        let name = "Prefixop"
        let valid text =
          text <> "!="
          &&
          operator_text
            ~prefix:""
            ~first:
              ( function
                  '!' | '~' | '?' -> true
                | _ -> false
              )
            ~rest:is_symbolchar_or_hash text
          && String.length text > 1
      end
    )

module Hashop =
  Safe_string
    ( struct
        let name = "Hashop"
        let valid =
          operator_text
            ~prefix:"#" ~first:is_symbolchar_or_hash ~rest:is_symbolchar_or_hash
      end
    )

module Infixop0 =
  Safe_string
    ( struct
        let name = "Infixop0"
        let valid text =
          text = "!="
          ||
          ( operator_text
              ~prefix:""
              ~first:
                ( function
                    '=' | '<' | '>' | '|' | '&' | '$' -> true
                  | _ -> false
                )
              ~rest:is_symbolchar text
            && not (List.mem text [ "&"; "&&"; "<"; "<-"; "="; ">"; "|"; "||" ])
          )
      end
    )

module Infixop1 =
  Safe_string
    ( struct
        let name = "Infixop1"
        let valid =
          operator_text
            ~prefix:""
            ~first:
              ( function
                  '@' | '^' -> true
                | _ -> false
              )
            ~rest:is_symbolchar
      end
    )

module Infixop2 =
  Safe_string
    ( struct
        let name = "Infixop2"
        let valid text =
          operator_text
            ~prefix:""
            ~first:
              ( function
                  '+' | '-' -> true
                | _ -> false
              )
            ~rest:is_symbolchar text
          && not (List.mem text [ "+"; "+."; "+="; "-"; "-."; "->" ])
      end
    )

module Infixop3 =
  Safe_string
    ( struct
        let name = "Infixop3"
        let valid text =
          List.mem text [ "land"; "lor"; "lxor"; "mod" ]
          ||
          ( operator_text
              ~prefix:""
              ~first:
                ( function
                    '*' | '/' | '%' -> true
                  | _ -> false
                )
              ~rest:is_symbolchar text
            && not (List.mem text [ "*"; "%" ])
            && not (String.starts_with ~prefix:"**" text)
          )
      end
    )

module Infixop4 =
  Safe_string
    ( struct
        let name = "Infixop4"
        let valid text =
          List.mem text [ "lsl"; "lsr"; "asr" ]
          ||
          operator_text ~prefix:"*" ~first:(( = ) '*') ~rest:is_symbolchar text
      end
    )

module Dotop =
  Safe_string
    ( struct
        let name = "Dotop"
        let valid =
          operator_text ~prefix:"" ~first:is_dotsymbolchar ~rest:is_symbolchar
      end
    )

module Letop =
  Safe_string
    ( struct
        let name = "Letop"
        let valid =
          operator_text ~prefix:"let" ~first:is_kwdopchar ~rest:is_dotsymbolchar
      end
    )

module Andop =
  Safe_string
    ( struct
        let name = "Andop"
        let valid =
          operator_text ~prefix:"and" ~first:is_kwdopchar ~rest:is_dotsymbolchar
      end
    )

type integer_literal = { digits : Integer_digits.t; suffix : char option }
type float_literal = { digits : Float_digits.t; suffix : char option }

type string_literal =
  { value : string; delimiter : Quoted_string_delimiter.t option;
    contents : string;
  }

type char_literal = { value : char; contents : string }

type quoted_extension =
  { id : string; blanks : string; delimiter : string;
    contents : string; value : string;
  }

type any_kind =
  [ `EOF | `AND | `AS | `ASSERT | `BEGIN | `CLASS | `CONSTRAINT | `DO
  | `DONE | `DOWNTO | `EFFECT | `ELSE | `END | `EXCEPTION | `EXTERNAL
  | `FALSE | `FOR | `FUN | `FUNCTION | `FUNCTOR | `IF | `IN | `INCLUDE
  | `INHERIT | `INITIALIZER | `LAZY | `LET | `MATCH | `METHOD
  | `MODULE | `MUTABLE | `NEW | `NONREC | `OBJECT | `OF | `OPEN | `OR
  | `PRIVATE | `REC | `SIG | `STRUCT | `THEN | `TO | `TRUE | `TRY
  | `TYPE | `VAL | `VIRTUAL | `WHEN | `WHILE | `WITH | `UNDERSCORE
  | `QUOTE | `BACKQUOTE | `LIDENT of Lident.t | `UIDENT of Uident.t
  | `LABEL of Lident.t | `OPTLABEL of Lident.t | `INT of integer_literal
  | `FLOAT of float_literal | `STRING of string_literal
  | `CHAR of char_literal | `QUOTED_STRING_EXPR of quoted_extension
  | `QUOTED_STRING_ITEM of quoted_extension | `LPAREN | `RPAREN | `LBRACE
  | `RBRACE | `LBRACKET | `RBRACKET | `LBRACKETBAR | `BARRBRACKET
  | `LBRACKETLESS | `LBRACKETGREATER | `LBRACELESS | `GREATERRBRACE
  | `GREATERRBRACKET | `LBRACKETAT | `LBRACKETATAT | `LBRACKETATATAT
  | `LBRACKETPERCENT | `LBRACKETPERCENTPERCENT | `COMMA | `SEMI | `SEMISEMI
  | `BAR | `BARBAR | `COLON | `COLONCOLON | `EQUAL | `MINUSGREATER | `DOT
  | `DOTDOT | `LESSMINUS | `COLONEQUAL | `COLONGREATER | `LESS | `GREATER
  | `AMPERSAND | `AMPERAMPER | `PLUS | `PLUSDOT | `PLUSEQ | `MINUS
  | `MINUSDOT | `PERCENT | `HASH | `BANG | `TILDE | `QUESTION | `STAR
  | `PREFIXOP of Prefixop.t | `HASHOP of Hashop.t | `INFIXOP0 of Infixop0.t
  | `INFIXOP1 of Infixop1.t | `INFIXOP2 of Infixop2.t | `INFIXOP3 of Infixop3.t
  | `INFIXOP4 of Infixop4.t | `DOTOP of Dotop.t | `LETOP of Letop.t
  | `ANDOP of Andop.t | `DOCSTRING of string | `ERROR of error
  ]

type +'kind token = { leading : trivia list; kind : 'kind }
type any_token = any_kind token

let make kind = { leading = []; kind }

let suffix_text =
  function
    None -> ""
  | Some suffix -> String.make 1 suffix

let trivia_text =
  function
  | Whitespace text | Comment text | Line_directive text | Warning (_, text) ->
      text

let kind_text : any_kind -> string =
  function
  | `EOF -> ""
  | `AND -> "and"
  | `AS -> "as"
  | `ASSERT -> "assert"
  | `BEGIN -> "begin"
  | `CLASS -> "class"
  | `CONSTRAINT -> "constraint"
  | `DO -> "do"
  | `DONE -> "done"
  | `DOWNTO -> "downto"
  | `EFFECT -> "effect"
  | `ELSE -> "else"
  | `END -> "end"
  | `EXCEPTION -> "exception"
  | `EXTERNAL -> "external"
  | `FALSE -> "false"
  | `FOR -> "for"
  | `FUN -> "fun"
  | `FUNCTION -> "function"
  | `FUNCTOR -> "functor"
  | `IF -> "if"
  | `IN -> "in"
  | `INCLUDE -> "include"
  | `INHERIT -> "inherit"
  | `INITIALIZER -> "initializer"
  | `LAZY -> "lazy"
  | `LET -> "let"
  | `MATCH -> "match"
  | `METHOD -> "method"
  | `MODULE -> "module"
  | `MUTABLE -> "mutable"
  | `NEW -> "new"
  | `NONREC -> "nonrec"
  | `OBJECT -> "object"
  | `OF -> "of"
  | `OPEN -> "open"
  | `OR -> "or"
  | `PRIVATE -> "private"
  | `REC -> "rec"
  | `SIG -> "sig"
  | `STRUCT -> "struct"
  | `THEN -> "then"
  | `TO -> "to"
  | `TRUE -> "true"
  | `TRY -> "try"
  | `TYPE -> "type"
  | `VAL -> "val"
  | `VIRTUAL -> "virtual"
  | `WHEN -> "when"
  | `WHILE -> "while"
  | `WITH -> "with"
  | `UNDERSCORE -> "_"
  | `QUOTE -> "'"
  | `BACKQUOTE -> "`"
  | `LIDENT name -> name
  | `UIDENT name -> name
  | `LABEL name -> "~" ^ name ^ ":"
  | `OPTLABEL name -> "?" ^ name ^ ":"
  | `INT (literal : integer_literal) ->
      literal.digits ^ suffix_text literal.suffix
  | `FLOAT (literal : float_literal) ->
      literal.digits ^ suffix_text literal.suffix
  | `STRING ({ delimiter = None; contents; _ } : string_literal) ->
      "\"" ^ contents ^ "\""
  | `STRING ({ delimiter = Some delimiter; contents; _ } : string_literal) ->
      "{" ^ delimiter ^ "|" ^ contents ^ "|" ^ delimiter ^ "}"
  | `CHAR (literal : char_literal) -> "'" ^ literal.contents ^ "'"
  | `QUOTED_STRING_EXPR (q : quoted_extension) ->
      "{%" ^ q.id ^ q.blanks ^ q.delimiter ^ "|"
      ^ q.contents ^ "|" ^ q.delimiter ^ "}"
  | `QUOTED_STRING_ITEM (q : quoted_extension) ->
      "{%%" ^ q.id ^ q.blanks ^ q.delimiter ^ "|"
      ^ q.contents ^ "|" ^ q.delimiter ^ "}"
  | `LPAREN -> "("
  | `RPAREN -> ")"
  | `LBRACE -> "{"
  | `RBRACE -> "}"
  | `LBRACKET -> "["
  | `RBRACKET -> "]"
  | `LBRACKETBAR -> "[|"
  | `BARRBRACKET -> "|]"
  | `LBRACKETLESS -> "[<"
  | `LBRACKETGREATER -> "[>"
  | `LBRACELESS -> "{<"
  | `GREATERRBRACE -> ">}"
  | `GREATERRBRACKET -> ">]"
  | `LBRACKETAT -> "[@"
  | `LBRACKETATAT -> "[@@"
  | `LBRACKETATATAT -> "[@@@"
  | `LBRACKETPERCENT -> "[%"
  | `LBRACKETPERCENTPERCENT -> "[%%"
  | `COMMA -> ","
  | `SEMI -> ";"
  | `SEMISEMI -> ";;"
  | `BAR -> "|"
  | `BARBAR -> "||"
  | `COLON -> ":"
  | `COLONCOLON -> "::"
  | `EQUAL -> "="
  | `MINUSGREATER -> "->"
  | `DOT -> "."
  | `DOTDOT -> ".."
  | `LESSMINUS -> "<-"
  | `COLONEQUAL -> ":="
  | `COLONGREATER -> ":>"
  | `LESS -> "<"
  | `GREATER -> ">"
  | `AMPERSAND -> "&"
  | `AMPERAMPER -> "&&"
  | `PLUS -> "+"
  | `PLUSDOT -> "+."
  | `PLUSEQ -> "+="
  | `MINUS -> "-"
  | `MINUSDOT -> "-."
  | `PERCENT -> "%"
  | `HASH -> "#"
  | `BANG -> "!"
  | `TILDE -> "~"
  | `QUESTION -> "?"
  | `STAR -> "*"
  | `PREFIXOP op | `HASHOP op | `INFIXOP0 op | `INFIXOP1 op
    | `INFIXOP2 op | `INFIXOP3 op | `INFIXOP4 op | `LETOP op | `ANDOP op
  -> op
  | `DOTOP op -> "." ^ op
  | `DOCSTRING text -> text
  | `ERROR error -> error.text

let text token = kind_text (token.kind :> any_kind)

let description token =
  match (token.kind :> any_kind) with
  | `EOF -> "end of input"
  | `LIDENT name | `UIDENT name -> Printf.sprintf "identifier '%s'" name
  | `LABEL _ | `OPTLABEL _ -> Printf.sprintf "label '%s'" (text token)
  | `INT _ -> "integer literal"
  | `FLOAT _ -> "float literal"
  | `STRING _ -> "string literal"
  | `CHAR _ -> "character literal"
  | `QUOTED_STRING_EXPR _ | `QUOTED_STRING_ITEM _ -> "quoted extension"
  | `PREFIXOP _ | `HASHOP _ | `INFIXOP0 _ | `INFIXOP1 _ | `INFIXOP2 _
    | `INFIXOP3 _ | `INFIXOP4 _ | `LETOP _ | `ANDOP _ | `DOTOP _
  -> Printf.sprintf "operator '%s'" (text token)
  | `DOCSTRING _ -> "doc comment"
  | `ERROR _ -> "lexical error"
  | _ -> Printf.sprintf "'%s'" (text token)

(* Newlines [\r*\n] lose one carriage return in docstrings. *)
let normalize_newlines text =
  let buffer = Buffer.create (String.length text) in
  String.iteri
    ( fun index character ->
        if
          not
            ( character = '\r' && index + 1 < String.length text
              && text.[index + 1] = '\n'
            )
        then Buffer.add_char buffer character
    )
    text
  ;
  Buffer.contents buffer

let docstring_body text =
  if text = "(**)" then ""
  else normalize_newlines (String.sub text 3 (String.length text - 5))
