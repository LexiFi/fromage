(** Lossless OCaml tokens, matching the tokens of the OCaml lexer. *)

(** Why a token was turned into trivia. *)
type warning =
  | Unattached_docstring
  (** A doc comment which documents nothing (warning 50 of the compiler,
      "unattached documentation comment"). *)

type trivia =
  | Whitespace of string
  (** Blanks and newlines (a newline is [\r*\n]). *)
  | Comment of string
  (** A comment, including its delimiters. Doc comments are tokens instead
      ([DOCSTRING]). *)
  | Line_directive of string
  (** A line directive such as [# 12 "file.ml"], from the [#] to the end of
      the line (the newline itself is not included). *)
  | Warning of warning * string
  (** The text of a token which the parser turned into trivia, such as a doc
      comment which documents nothing. *)

type keyword =
  [ `AND | `AS | `ASSERT | `BEGIN | `CLASS | `CONSTRAINT | `DO | `DONE | `DOWNTO
  | `EFFECT | `ELSE | `END | `EXCEPTION | `EXTERNAL | `FALSE | `FOR | `FUN
  | `FUNCTION | `FUNCTOR | `IF | `IN | `INCLUDE | `INHERIT | `INITIALIZER
  | `LAZY | `LET | `MATCH | `METHOD | `MODULE | `MUTABLE | `NEW | `NONREC
  | `OBJECT | `OF | `OPEN | `OR | `PRIVATE | `REC | `SIG | `STRUCT | `THEN
  | `TO | `TRUE | `TRY | `TYPE | `VAL | `VIRTUAL | `WHEN | `WHILE | `WITH
  ]

val keyword_of_string : string -> [> keyword ] option
(** The keyword spelled by a string, if any. Word operators such as [mod] are
    not keywords. *)

type lexing_error =
  [ `Illegal_escape | `Invalid_encoding | `Invalid_identifier_character
  | `Non_lowercase_delimiter | `Capitalized_label | `Keyword_label
  | `Reserved_sequence | `Capitalized_raw_identifier | `Invalid_literal
  | `Empty_character_literal | `Illegal_character | `Invalid_directive
  | `Unterminated_comment | `Unterminated_string | `Unterminated_quoted_string
  ]
(** The lexical errors. A string which is unterminated in a comment is an
    unterminated comment. *)

type error =
  { reason : lexing_error; text : string; offset_start : int; offset_stop : int
  }

(** A string checked to be a valid token payload. *)
module type SafeString =
  sig
    type t

    val of_string : string -> t option
    (** [None] if the string is not valid. *)

    val of_string_exn : string -> t
    (** Raises [Invalid_argument] if the string is not valid. *)

    val of_string_unsafe : string -> t
    (** Does not check the string, which must be valid. *)

    val to_string : t -> string
  end

module Lident :
  sig
    include SafeString

    val source : t -> string
    (** The text of the identifier in the source. *)
  end
(** A lowercase identifier (a non-keyword identifier beginning with a
    lowercase letter or an underscore), as written in the source: it may be a
    raw identifier ([\#name]) or contain UTF-8 characters. Its [to_string] is
    the name of the identifier, as used in the AST. *)

module Uident :
  sig
    include SafeString

    val source : t -> string
  end
(** An identifier beginning with an uppercase letter. *)

module Quoted_string_delimiter : SafeString
(** Zero or more lowercase letters. *)

module Integer_digits : SafeString
(** A lexer-recognized integer spelling: starts with a digit, without . or e. *)

module Float_digits : SafeString
(** A lexer-recognized float spelling: starts with a digit and has . or e. *)

module Symbol : SafeString
(** One punctuation character or a word operator such as [land] or [mod]. *)

(* The texts of the operator tokens: each operator token has its own type,
   whose [of_string] accepts exactly the texts the lexer reads as one token of
   that kind. *)

module Prefixop : SafeString
(** [!] followed by operator characters (but not [!=]), or [~] or [?]
    followed by operator characters: [!!], [~-], [?+]. *)

module Hashop : SafeString
(** [#] followed by operator characters: [##], [#=]. *)

module Infixop0 : SafeString
(** [=], [<], [>], [|], [&] or [$] followed by operator characters, but not a
    punctuation symbol such as [=] or [<-]; or [!=]. *)

module Infixop1 : SafeString
(** [@] or [^] followed by operator characters: [@@], [^]. *)

module Infixop2 : SafeString
(** [+] or [-] followed by operator characters, but not a punctuation symbol
    such as [+] or [->]: [+!], [--]. *)

module Infixop3 : SafeString
(** [*], [/] or [%] followed by operator characters, but not [*], [%] or an
    [Infixop4] ([**...]); or [land], [lor], [lxor] or [mod]. *)

module Infixop4 : SafeString
(** [**] followed by operator characters; or [lsl], [lsr] or [asr]. *)

module Dotop : SafeString
(** The operator of an indexing operator, without its leading dot: [%] in [.%()]. *)

module Letop : SafeString
(** [let] followed by a binding operator: [let*], [let+]. *)

module Andop : SafeString
(** [and] followed by a binding operator: [and*], [and+]. *)

type integer_literal = { digits : Integer_digits.t; suffix : char option }
type float_literal = { digits : Float_digits.t; suffix : char option }

type string_literal =
  { value : string; (** The decoded value of the string. *)
    delimiter : Quoted_string_delimiter.t option;
    contents : string; (** The source text between the delimiters. *)
  }

type char_literal = { value : char; contents : string }

type quoted_extension =
  { id : string; (** The extension identifier, as written in the source. *)
    blanks : string; (** The blanks between the identifier and the delimiter. *)
    delimiter : string;
    contents : string; (** The source text between the delimiters. *)
    value : string; (** The value of the string (with normalized newlines). *)
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
  | `QUOTED_STRING_ITEM of quoted_extension | `LPAREN | `RPAREN
  | `LBRACE | `RBRACE | `LBRACKET | `RBRACKET | `LBRACKETBAR
  | `BARRBRACKET | `LBRACKETLESS | `LBRACKETGREATER | `LBRACELESS
  | `GREATERRBRACE | `GREATERRBRACKET | `LBRACKETAT | `LBRACKETATAT
  | `LBRACKETATATAT | `LBRACKETPERCENT | `LBRACKETPERCENTPERCENT
  | `COMMA | `SEMI | `SEMISEMI | `BAR | `BARBAR | `COLON | `COLONCOLON
  | `EQUAL | `MINUSGREATER | `DOT | `DOTDOT | `LESSMINUS | `COLONEQUAL
  | `COLONGREATER | `LESS | `GREATER | `AMPERSAND | `AMPERAMPER
  | `PLUS | `PLUSDOT | `PLUSEQ | `MINUS | `MINUSDOT | `PERCENT | `HASH
  | `BANG | `TILDE | `QUESTION | `STAR | `PREFIXOP of Prefixop.t
  | `HASHOP of Hashop.t | `INFIXOP0 of Infixop0.t | `INFIXOP1 of Infixop1.t
  | `INFIXOP2 of Infixop2.t | `INFIXOP3 of Infixop3.t | `INFIXOP4 of Infixop4.t
  | `DOTOP of Dotop.t (** The operator, without its leading dot. *)
  | `LETOP of Letop.t | `ANDOP of Andop.t
  | `DOCSTRING of string
    (** A doc comment [(** ... *)] or [(**)], with its delimiters. *)
  | `ERROR of error
  ]

type +'kind token = { leading : trivia list; kind : 'kind }
type any_token = any_kind token

val make : 'kind -> 'kind token
val text : [< any_kind ] token -> string
(** The source text of a token, without its leading trivia. *)

val docstring_body : string -> string
(** The contents of a doc comment, as in the compiler's [Docstrings]: the text
    between [(**] and [*)], with [\r\n] read as [\n]. *)

val description : [< any_kind ] token -> string
(** The token for an error message: its text in quotes for a keyword or a
    symbol (['in'], [')']), or what it is for a token with a payload
    ([identifier 'x'], [string literal]), and [end of input] for [`EOF]. *)

val trivia_text : trivia -> string
