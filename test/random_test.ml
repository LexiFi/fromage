open Ocst

let token = Random_token.token
let eof = token "" `EOF

let expect_valid name value =
  if Option.is_none value then failwith (name ^ " should be valid")

let expect_invalid name value =
  if Option.is_some value then failwith (name ^ " should be invalid")

let checked_string_types () =
  expect_valid "lident" (Token.Lident.of_string "value_name'");
  expect_invalid "lident keyword" (Token.Lident.of_string "let");
  expect_invalid "lident uppercase" (Token.Lident.of_string "Value");
  expect_invalid "lident underscore" (Token.Lident.of_string "_");
  expect_valid "uident" (Token.Uident.of_string "Value_name'");
  expect_invalid "uident lowercase" (Token.Uident.of_string "value");
  expect_valid
    "quoted delimiter" (Token.Quoted_string_delimiter.of_string "tag")
  ;
  expect_invalid
    "quoted delimiter uppercase" (Token.Quoted_string_delimiter.of_string "Tag")
  ;
  expect_valid "symbol" (Token.Symbol.of_string "+");
  expect_invalid "symbol text" (Token.Symbol.of_string "++");
  expect_valid "integer digits" (Token.Integer_digits.of_string "42");
  expect_invalid "integer float" (Token.Integer_digits.of_string "1.5");
  expect_valid "float digits" (Token.Float_digits.of_string "1.5");
  expect_invalid "float integer" (Token.Float_digits.of_string "42")

(* Every rule of the checked string types. *)
let checked_string_rules () =
  let valid name check text = expect_valid (name ^ " " ^ text) (check text) in
  let invalid name check text = expect_invalid (name ^ " " ^ text) (check text)
  in
  let raises name f =
    match f () with
      _ -> failwith (name ^ " should raise")
    | exception Invalid_argument _ -> ()
  in
  List.iter
    (valid "lident" Token.Lident.of_string)
    [ "x1'"; "_x"; "\\#let"; "\\#x"; "\\#\xc3\xa9"; "\xc3\xa9" ]
  ;
  List.iter
    (invalid "lident" Token.Lident.of_string)
    [ "x-"; "mod"; "\\#\xc3\x89"; "\\#1x"; "\xc3\x89"; "\xff"; "1x" ]
  ;
  raises "lident" (fun () -> Token.Lident.of_string_exn "Let");
  if Token.Lident.source (Token.Lident.of_string_exn "\\#let") <> "\\#let" then
    failwith "lident source"
  ;
  (* The name of invalid UTF-8 is its normalization, which does not fail. *)
  ignore (Token.Lident.to_string (Token.Lident.of_string_unsafe "\xff"));
  List.iter (valid "uident" Token.Uident.of_string) [ "X1'"; "\xc3\x89" ];
  List.iter
    (invalid "uident" Token.Uident.of_string) [ "X-"; "\xc3\xa9"; "\xff" ]
  ;
  raises "uident" (fun () -> Token.Uident.of_string_exn "x");
  if Token.Uident.source (Token.Uident.of_string_exn "X") <> "X" then
    failwith "uident source"
  ;
  valid "delimiter" Token.Quoted_string_delimiter.of_string "a_b";
  raises "delimiter" (fun () -> Token.Quoted_string_delimiter.of_string_exn "A")
  ;
  List.iter
    (valid "integer" Token.Integer_digits.of_string) [ "0xE"; "0X1F"; "1_000" ]
  ;
  List.iter
    (invalid "integer" Token.Integer_digits.of_string)
    [ "0x1p"; "0x1P"; "1e5"; "1E5"; "1.0"; "1+"; "1-"; "Ab"; "1#" ]
  ;
  raises "integer" (fun () -> Token.Integer_digits.of_string_exn "1.0");
  List.iter
    (valid "float" Token.Float_digits.of_string) [ "1.5e+3"; "1e-3"; "0x1p3" ]
  ;
  raises "float" (fun () -> Token.Float_digits.of_string_exn "12");
  List.iter
    (valid "symbol" Token.Symbol.of_string)
    [ "+"; "land"; "lor"; "lsl"; "lsr"; "lxor"; "mod"; "asr" ]
  ;
  raises "symbol" (fun () -> Token.Symbol.of_string_exn "++");
  ignore (Token.Symbol.of_string_exn "+");
  if Token.Symbol.to_string (Token.Symbol.of_string_unsafe "+") <> "+" then
    failwith "symbol"
  ;
  (* The tokens which no error message names: errors at the end of the input
     are worded differently, doc comments are skipped and lexical errors are
     reported by the lexer. *)
  List.iter
    ( fun (kind, expected) ->
        if Token.description (Token.make kind) <> expected then
          failwith ("description " ^ expected)
    )
    [ `EOF, "end of input"; `DOCSTRING "(** d *)", "doc comment";
      `ERROR
        { Token.reason = `Illegal_character;
          text = "t"; offset_start = 0; offset_stop = 1
        }
      ,
      "lexical error"
    ]
  ;
  (* A newline [\r*\n] loses one carriage return; other carriage returns stay. *)
  if Token.docstring_body "(**a\r\r\nb\rc\r*)" <> "a\r\nb\rc\r" then
    failwith "docstring_body"

(* Each operator type accepts exactly the texts which the lexer reads as one
   token of its kind: every text of up to three operator characters (or [a]),
   and the operator words. *)
let operator_types () =
  let chars = "!#$%&*+-./:<=>?@^|~a" in
  let extend texts =
    List.concat_map
      ( fun text ->
          List.init
            (String.length chars) (fun i -> text ^ String.make 1 chars.[i])
      )
      texts
  in
  let texts1 = extend [ "" ] in
  let texts2 = extend texts1 in
  let texts =
    texts1 @ texts2 @ extend texts2 @ List.map (( ^ ) "let") texts2
    @ List.map (( ^ ) "and") texts2
    @ [ ""; "land"; "lor"; "lxor"; "mod"; "lsl"; "lsr"; "asr"; "let"; "and" ]
  in
  let lexed text =
    match lex text with
    | [ (token : Token.any_token); { kind = `EOF; _ } ] -> Some token.kind
    | _ -> None
  in
  let expect name valid lexed text =
    if valid <> lexed then
      failwith
        ( Printf.sprintf
            "%s %S: of_string %b but lexed %b" name text valid lexed
        )
  in
  List.iter
    ( fun text ->
        let kind = lexed text in
        let is f =
          match kind with
            Some kind -> f kind
          | None -> false
        in
        expect
          "Prefixop" (Token.Prefixop.of_string text <> None)
          ( is
              ( function
                  `PREFIXOP op -> Token.Prefixop.to_string op = text
                | _ -> false
              )
          )
          text
        ;
        expect
          "Hashop" (Token.Hashop.of_string text <> None)
          ( is
              ( function
                  `HASHOP op -> Token.Hashop.to_string op = text
                | _ -> false
              )
          )
          text
        ;
        expect
          "Infixop0" (Token.Infixop0.of_string text <> None)
          ( is
              ( function
                  `INFIXOP0 op -> Token.Infixop0.to_string op = text
                | _ -> false
              )
          )
          text
        ;
        expect
          "Infixop1" (Token.Infixop1.of_string text <> None)
          ( is
              ( function
                  `INFIXOP1 op -> Token.Infixop1.to_string op = text
                | _ -> false
              )
          )
          text
        ;
        expect
          "Infixop2" (Token.Infixop2.of_string text <> None)
          ( is
              ( function
                  `INFIXOP2 op -> Token.Infixop2.to_string op = text
                | _ -> false
              )
          )
          text
        ;
        expect
          "Infixop3" (Token.Infixop3.of_string text <> None)
          ( is
              ( function
                  `INFIXOP3 op -> Token.Infixop3.to_string op = text
                | _ -> false
              )
          )
          text
        ;
        expect
          "Infixop4" (Token.Infixop4.of_string text <> None)
          ( is
              ( function
                  `INFIXOP4 op -> Token.Infixop4.to_string op = text
                | _ -> false
              )
          )
          text
        ;
        expect
          "Letop" (Token.Letop.of_string text <> None)
          ( is
              ( function
                  `LETOP op -> Token.Letop.to_string op = text
                | _ -> false
              )
          )
          text
        ;
        expect
          "Andop" (Token.Andop.of_string text <> None)
          ( is
              ( function
                  `ANDOP op -> Token.Andop.to_string op = text
                | _ -> false
              )
          )
          text
        ;
        expect
          "Dotop" (Token.Dotop.of_string text <> None)
          begin match lexed ("." ^ text) with
          | Some (`DOTOP op) -> Token.Dotop.to_string op = text
          | _ -> false
          end
          text
    )
    texts
  ;
  match Token.Infixop0.of_string_exn "->" with
  | _ -> failwith "Infixop0 \"->\" should raise"
  | exception Invalid_argument _ -> ()

let check name stream =
  let expected = Token_stream.tokens stream @ [ eof ] in
  let actual = lex (Token_stream.source stream) in
  if expected <> actual then
    failwith (Printf.sprintf "%s: token stream does not round-trip" name)

(* Inserting groups of tokens, and the tokens which cannot be inserted. *)
let token_stream_groups () =
  let ident name = token name (`LIDENT (Token.Lident.of_string_exn name)) in
  let quote = token "'" `QUOTE in
  (* A group which the lexer emits together: ['] and a name. *)
  let stream = Token_stream.insert_group Token_stream.empty [ quote; ident "a" ]
  in
  let stream = Token_stream.insert_group stream [] in
  let stream =
    Token_stream.insert_group_create_trivia
      stream
      [ ident "b"; ident "c" |> fun t -> { t with leading = [ Whitespace " " ] }
      ]
  in
  let stream = Token_stream.insert_group_create_trivia stream [] in
  (* A group which needs no trivia. *)
  let parens =
    Token_stream.insert_group_create_trivia stream [ token "(" `LPAREN ]
  in
  check
    "a group without trivia" (Token_stream.insert parens (token ")" `RPAREN))
  ;
  check "token groups" stream;
  begin match Token_stream.insert_group stream [ ident "d" ] with
  | _ -> failwith "a group without trivia after an identifier"
  | exception Token_stream.Missing_trivia _ -> ()
  end;
  (* An identifier which lexes as a keyword cannot be inserted. *)
  let keyword = token "let" (`LIDENT (Token.Lident.of_string_unsafe "let")) in
  begin match Token_stream.insert Token_stream.empty keyword with
  | _ -> failwith "a keyword inserted as an identifier"
  | exception Token_stream.Invalid_token _ -> ()
  end;
  begin match Token_stream.insert_create_trivia Token_stream.empty keyword with
  | _ -> failwith "a keyword inserted as an identifier"
  | exception Token_stream.Invalid_token _ -> ()
  end;
  begin match Token_stream.insert_group Token_stream.empty [ keyword ] with
  | _ -> failwith "a keyword inserted as an identifier"
  | exception Token_stream.Invalid_token _ -> ()
  end;
  (* [EOF] cannot be inserted: the source has fewer tokens than the stream. *)
  begin match Token_stream.insert stream (Token.make `EOF) with
  | _ -> failwith "EOF inserted"
  | exception Token_stream.Missing_trivia _ -> ()
  end;
  (* Two tokens lexed as one. *)
  let minus = token "-" `MINUS
  and greater = token ">" `GREATER
  in
  match
    Token_stream.insert (Token_stream.insert Token_stream.empty minus) greater
  with
  | _ -> failwith "[-] and [>] inserted without trivia"
  | exception Token_stream.Missing_trivia _ -> ()

let expect_missing_trivia () =
  let stream = Token_stream.insert Token_stream.empty (token "let" `LET) in
  match
    Token_stream.insert
      stream (token "x" (`LIDENT (Token.Lident.of_string_exn "x")))
  with
  | _ -> failwith "LET followed by LIDENT must require trivia"
  | exception Token_stream.Missing_trivia _ -> ()

let explicit_regressions () =
  expect_missing_trivia ();
  let stream =
    Token_stream.empty
    |>
    fun stream ->
      Token_stream.insert_create_trivia stream (token "let" `LET)
      |>
      fun stream ->
        Token_stream.insert_create_trivia
          stream (token "x" (`LIDENT (Token.Lident.of_string_exn "x")))
  in
  check "keyword-identifier boundary" stream;
  let stream =
    Token_stream.empty
    |>
    fun stream ->
      Token_stream.insert_create_trivia
        stream
        ( token
            "1.5"
            ( `FLOAT
                { Token.digits = Token.Float_digits.of_string_exn "1.5";
                  suffix = None
                }
            )
        )
  in
  check "float literal" stream;
  let stream =
    Token_stream.insert_create_trivia Token_stream.empty (token "'" `QUOTE)
  in
  check "standalone quote token" stream

let unterminated_errors =
  [ "(*", `Unterminated_comment; "\"", `Unterminated_string;
    "{tag|", `Unterminated_quoted_string
  ]

let insert_error stream text reason =
  let placeholder =
    token
      text
      (`ERROR { Token.reason; text = text; offset_start = 0; offset_stop = 0 })
  in
  let inserted = Token_stream.insert_create_trivia stream placeholder in
  match List.rev (Token_stream.tokens inserted) with
  | _ :: _ ->
      let offset_stop = String.length (Token_stream.source inserted) in
      let offset_start = offset_stop - String.length text in
      let error =
        token
          text (`ERROR { Token.reason; text = text; offset_start; offset_stop })
      in
      Token_stream.insert_create_trivia stream error
  | [] -> assert false

let all_token_kinds () =
  let stream =
    List.fold_left
      Token_stream.insert_create_trivia
      Token_stream.empty (Array.to_list Random_token.all)
  in
  check "all non-error token kinds" stream;
  let variable =
    Token_stream.empty
    |>
    fun stream ->
      Token_stream.insert_create_trivia stream (token "'" `QUOTE)
      |>
      fun stream ->
        Token_stream.insert_create_trivia
          stream (token "a" (`LIDENT (Token.Lident.of_string_exn "a")))
  in
  check "type variable tokens" variable;
  List.iter
    ( fun (text, reason) ->
        check
          ("unterminated " ^ text) (insert_error Token_stream.empty text reason)
    )
    unterminated_errors

let () =
  checked_string_types ();
  checked_string_rules ();
  operator_types ();
  token_stream_groups ();
  explicit_regressions ();
  all_token_kinds ();
  let state = Random.State.make [| 0x0c57; 0x1eaf; 0x2026 |] in
  for sequence = 1 to 1_000 do
    let stream = ref Token_stream.empty in
    for _ = 1 to 1 + Random.State.int state 30 do
      stream
      := Token_stream.insert_create_trivia !stream (Random_token.random state)
    done;
    if Random.State.bool state then begin let text, reason =
        List.nth
          unterminated_errors
          (Random.State.int state (List.length unterminated_errors))
      in
      stream := insert_error !stream text reason
    end
    ;
    check (Printf.sprintf "random sequence %d" sequence) !stream
  done;
  print_endline "OCST lexer token-stream round-trip passed."
