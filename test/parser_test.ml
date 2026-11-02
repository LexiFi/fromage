open Ocst

let parse source = Parser.implementation_tokens (lex source)

let expect_let source =
  match parse source with
  | [ Cst.Item (_, Str_value _, _) ], _ -> ()
  | _ -> failwith "expected one structure let binding"

let rejected parse = try ignore (parse ()); false with _ -> true

let expect_rejected name compiler ocst =
  if not (rejected compiler) then
    failwith (name ^ ": compiler accepted invalid input")
  ;
  if not (rejected ocst) then
    failwith (name ^ ": OCST parser accepted invalid input")

let () =
  expect_let "let answer = 42";
  expect_let "let message = {tag|hello|tag}";
  expect_let "let value = (1, 2)";
  expect_let "let rec l = ~lbl: 5, ~lbl2: 10 :: l";
  ignore (parse "type t = F(M).t");
  expect_rejected
    "missing let pattern"
    (fun () -> Parse.implementation (Lexing.from_string "let = 1"))
    (fun () -> Parser.implementation_tokens (lex "let = 1"))
  ;
  expect_rejected
    "missing value type"
    (fun () -> Parse.interface (Lexing.from_string "val x :"))
    (fun () -> Parser.interface_tokens (lex "val x :"))
  ;
  expect_rejected
    "unclosed module signature"
    ( fun () ->
        Parse.interface (Lexing.from_string "module M : sig val x : int")
    )
    (fun () -> Parser.interface_tokens (lex "module M : sig val x : int"))
  ;
  let interface_source = "val answer : int\n" in
  let interface = Parser.interface_tokens (lex interface_source) in
  if Printer.string_of_interface interface <> interface_source then
    failwith "CST interface printer changed source"
  ;
  let compiler_lexbuf = Lexing.from_string interface_source in
  compiler_lexbuf.lex_curr_p <-
    { compiler_lexbuf.lex_curr_p with pos_fname = "parser_test.mli" }
  ;
  let compiler_interface = Parse.interface compiler_lexbuf in
  if
    not
      Stdlib.(
        To_ast.interface ~filename:"parser_test.mli" interface
        = compiler_interface
      )
  then failwith "CST interface AST differs from compiler AST"
  ;
  let source = "let answer = 42" in
  let cst = Parser.implementation_tokens (lex source) in
  if Printer.string_of_implementation cst <> source then
    failwith "CST printer changed source"

(* The entry points on token streams, and the printers on buffers. *)
let () =
  let stream tokens =
    List.fold_left Token_stream.insert_create_trivia Token_stream.empty tokens
  in
  let without_eof source =
    List.filter
      (fun (token : Token.any_token) -> token.kind <> `EOF) (lex source)
  in
  let implementation = Parser.implementation (stream (without_eof "let x = 1"))
  in
  let interface = Parser.interface (stream (without_eof "val x : int")) in
  let buffer = Buffer.create 16 in
  Printer.implementation buffer implementation;
  Buffer.add_char buffer '\n';
  Printer.interface buffer interface;
  if Buffer.contents buffer <> "let x = 1\nval x : int" then
    failwith "the CST of a token stream changed the source"
  ;
  (* The tokens end with [EOF]. *)
  match Parser.implementation_tokens (without_eof "let x = 1") with
  | _ -> failwith "OCST accepted tokens without EOF"
  | exception Invalid_argument _ -> ()

(* With [-unsafe], indexing uses [unsafe_get] and [unsafe_set], as in the
   compiler. *)
let () =
  let source = "let _ = a.(0) <- a.(1)\nlet _ = s.[0]\n" in
  Clflags.unsafe := true;
  let compiler = Parse.implementation (Lexing.from_string source) in
  let ocst =
    To_ast.implementation
      ~filename:"" (Parser.implementation_tokens (lex source))
  in
  Clflags.unsafe := false;
  if not Stdlib.(ocst = compiler) then
    failwith "the AST with -unsafe differs from the compiler's"

(* The tokens of a CST, whose unattached doc comments are warnings in the
   trivia, give the same CST. *)
let () =
  let source = "let x = (** unattached *) 1\n" in
  let cst = Parser.implementation_tokens (lex source) in
  let again = Parser.implementation_tokens (Printer.implementation_tokens cst)
  in
  if Printer.string_of_implementation again <> source then
    failwith "the tokens of a CST give another source"
