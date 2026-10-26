module Parser = Parser
module Printer = Printer
module To_ast = To_ast
module Token = Token
module Cst = Cst
module Cst_lexer = Cst_lexer
module Token_stream = Token_stream
module Token_position = Token_position
module Syntax_error = Syntax_error
module Same_ast = Same_ast

let lex = Cst_lexer.tokenize

let source_of_tokens tokens =
  String.concat
    ""
    ( List.map
        ( fun token ->
            String.concat "" (List.map Token.trivia_text token.Token.leading)
            ^ Token.text token
        )
        tokens
    )
