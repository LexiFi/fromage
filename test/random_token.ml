open Ocst

let token expected kind =
  let token = Token.make kind in
  if Token.text token <> expected then
    failwith
      ( Printf.sprintf
          "Random_token: %S is printed %S" expected (Token.text token)
      )
  ;
  token

let keywords =
  List.map
    (fun (text, kind) -> token text kind)
    [ "and", `AND; "as", `AS; "assert", `ASSERT; "begin", `BEGIN;
      "class", `CLASS; "constraint", `CONSTRAINT; "do", `DO;
      "done", `DONE; "downto", `DOWNTO; "effect", `EFFECT; "else", `ELSE;
      "end", `END; "exception", `EXCEPTION; "external", `EXTERNAL;
      "false", `FALSE; "for", `FOR; "fun", `FUN; "function", `FUNCTION;
      "functor", `FUNCTOR; "if", `IF; "in", `IN; "include", `INCLUDE;
      "inherit", `INHERIT; "initializer", `INITIALIZER; "lazy", `LAZY;
      "let", `LET; "match", `MATCH; "method", `METHOD; "module", `MODULE;
      "mutable", `MUTABLE; "new", `NEW; "nonrec", `NONREC; "object", `OBJECT;
      "of", `OF; "open", `OPEN; "or", `OR; "private", `PRIVATE;
      "rec", `REC; "sig", `SIG; "struct", `STRUCT; "then", `THEN;
      "to", `TO; "true", `TRUE; "try", `TRY; "type", `TYPE; "val", `VAL;
      "virtual", `VIRTUAL; "when", `WHEN; "while", `WHILE; "with", `WITH
    ]

let symbols =
    List.map
      (fun (text, kind) -> token text kind)
      [ "_", `UNDERSCORE; "'", `QUOTE; "`", `BACKQUOTE; "(", `LPAREN;
        ")", `RPAREN; "{", `LBRACE; "}", `RBRACE; "[", `LBRACKET;
        "]", `RBRACKET; "[|", `LBRACKETBAR; "|]", `BARRBRACKET;
        "[<", `LBRACKETLESS; "[>", `LBRACKETGREATER; "{<", `LBRACELESS;
        ">}", `GREATERRBRACE; ">]", `GREATERRBRACKET; "[@", `LBRACKETAT;
        "[@@", `LBRACKETATAT; "[@@@", `LBRACKETATATAT; "[%", `LBRACKETPERCENT;
        "[%%", `LBRACKETPERCENTPERCENT; ",", `COMMA; ";", `SEMI;
        ";;", `SEMISEMI; "|", `BAR; "||", `BARBAR; ":", `COLON;
        "::", `COLONCOLON; "=", `EQUAL; "->", `MINUSGREATER;
        ".", `DOT; "..", `DOTDOT; "<-", `LESSMINUS; ":=", `COLONEQUAL;
        ":>", `COLONGREATER; "<", `LESS; ">", `GREATER; "&", `AMPERSAND;
        "&&", `AMPERAMPER; "+", `PLUS; "+.", `PLUSDOT; "+=", `PLUSEQ;
        "-", `MINUS; "-.", `MINUSDOT; "%", `PERCENT; "#", `HASH;
        "!", `BANG; "~", `TILDE; "?", `QUESTION; "*", `STAR
      ]
  @
    [ token "!!" (`PREFIXOP (Token.Prefixop.of_string_exn "!!"));
      token "##" (`HASHOP (Token.Hashop.of_string_exn "##"));
      token "<=" (`INFIXOP0 (Token.Infixop0.of_string_exn "<="));
      token "^" (`INFIXOP1 (Token.Infixop1.of_string_exn "^"));
      token "+!" (`INFIXOP2 (Token.Infixop2.of_string_exn "+!"));
      token "mod" (`INFIXOP3 (Token.Infixop3.of_string_exn "mod"));
      token "**" (`INFIXOP4 (Token.Infixop4.of_string_exn "**"));
      token "let*" (`LETOP (Token.Letop.of_string_exn "let*"));
      token "and*" (`ANDOP (Token.Andop.of_string_exn "and*"));
      token ".+" (`DOTOP (Token.Dotop.of_string_exn "+"))
    ]

let literals =
  let lident name = Token.Lident.of_string_exn name in
  let integer digits suffix =
    `INT
      ( { digits = Token.Integer_digits.of_string_exn digits; suffix }
      : Token.integer_literal
      )
  in
  let float digits suffix =
    `FLOAT
      ( { digits = Token.Float_digits.of_string_exn digits; suffix }
      : Token.float_literal
      )
  in
  let quoted id contents : Token.quoted_extension =
    { id; blanks = ""; delimiter = ""; contents; value = contents }
  in
  List.map
    (fun (text, kind) -> token text kind)
    [ "lower_name", `LIDENT (lident "lower_name");
      "Upper_name", `UIDENT (Token.Uident.of_string_exn "Upper_name");
      "~label:", `LABEL (lident "label");
      "?label:", `OPTLABEL (lident "label"); "42", integer "42" None;
      "42L", integer "42" (Some 'L'); "1.5", float "1.5" None;
      "\"hello\\n\"",
      `STRING
        { Token.value = "hello\n"; delimiter = None; contents = "hello\\n" }
    ;
      "{tag|hello|tag}",
      `STRING
        { Token.value = "hello";
          delimiter = Some (Token.Quoted_string_delimiter.of_string_exn "tag");
          contents = "hello";
        }
    ;
      "'x'", `CHAR { Token.value = 'x'; contents = "x" };
      "{%ext|s|}", `QUOTED_STRING_EXPR (quoted "ext" "s");
      "{%%ext|s|}", `QUOTED_STRING_ITEM (quoted "ext" "s");
      "(** doc *)", `DOCSTRING "(** doc *)"; "(**)", `DOCSTRING "(**)"
    ]

let all = Array.of_list (keywords @ symbols @ literals)

let random state = all.(Random.State.int state (Array.length all))
