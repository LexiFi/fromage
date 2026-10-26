(* A recursive-descent parser producing the CST. It follows the productions of
   ocaml/parsing/parser.mly; the precedences of the Menhir grammar are
   reproduced by precedence climbing for binary operators. *)

open Cst

(* An error explains what is wrong and how to fix it: see {!E}. *)
type error = { id : string; message : string }

type state =
  { tokens : Token.any_token array;
    (** The tokens, without the doc comments. *)
    eof : Token.any_token;
    docs : Doc_comments.t;
    (** The doc comments, between the tokens. *)
    raw : Token.any_token list;
    (** All the tokens, with the doc comments and [EOF], for locations. *)
    filename : string; mutable index : int;
    mutable spans : Token_position.span array option;
    (** The locations of the tokens, computed for the first error. *)
    mutable stopped : (int * error) option;
    (** A construct which ended before the token at this index, since it cannot
        be followed by it, and the error to report if the parser stops there. *)
  }

(* {1 Errors}

   An error names the illegal token and says what was expected there,
   without referring to the grammar. An error used once is written where it
   is raised, by a call to [syntax_error]; the module [E] holds the
   others. Its identifier is unique: test/errors/errors.ml checks that every
   error of this file is raised by a file of test/errors/cases, and is
   declared once. *)

let syntax_error id message = { id; message }

(* The location of the token at [index], or of the end of the input. *)
let location state index =
  let spans =
    match state.spans with
    | Some spans -> spans
    | None ->
        (* Without the doc comments. *)
        let spans =
          List.combine
            state.raw
            ( Array.to_list
                (Token_position.spans ~filename:state.filename state.raw)
            )
          |>
          List.filter_map
            ( fun ((token : Token.any_token), span) ->
                match token.kind with
                  `DOCSTRING _ -> None
                | _ -> Some span
            )
        in
        let spans = Array.of_list spans in
        state.spans <- Some spans; spans
  in
  let { Token_position.start; stop } =
    spans.(min index (Array.length spans - 1))
  in
  Location.{ loc_start = start; loc_end = stop; loc_ghost = false }

(* The errors are exceptions, which the parser raises with [raise] (which
   bisect_ppx knows not to return, unlike a function raising them). The message
   starts with the illegal token: "Illegal ')', '..' expected.", "Unexpected
   end of input, ...", also for the internal errors: "Illegal ')', OCST
   internal error: ...". *)
let failure_at ?(notes = []) state index { id; message } =
  let notes = List.map (fun (index, note) -> location state index, note) notes
  in
  let message =
    if index >= Array.length state.tokens then
      "Unexpected end of input, " ^ message ^ "."
    else
      "Illegal " ^ Token.description state.tokens.(index) ^ ", " ^ message ^ "."
  in
  Syntax_error.Error { id; location = location state index; message; notes }

(* An error at the next token: the reason why a construct stopped before it,
   if any, is more precise. *)
let failure ?notes state error =
  match state.stopped with
  | Some (index, reason) when index = state.index ->
      failure_at state index reason
  | _ -> failure_at ?notes state state.index error

(* A construct ended before the next token, for [reason]. *)
let stop state reason = state.stopped <- Some (state.index, reason)

(* An error which the parser itself should prevent: no test can reach it,
   hence [@coverage off], as for the other code which no source can reach
   (such as the [_ -> None] of the selectors of [take state internal]). *)
let internal_failure state what =
  failure
    state
    ( syntax_error
        "internal" ("OCST internal error: " ^ what ^ ". Please report it")
    )
[@@coverage off]

let internal =
  syntax_error
    "internal" "OCST internal error: unexpected token. Please report it"

(* The errors raised in several places, and the errors computed from an
   argument, by syntactic category. A message tells what was expected where
   the parser stopped, after the illegal token: "Illegal ')', an array index
   expects an expression." *)
module E =
  struct
    let simple_argument =
      "an argument, such as an identifier, a literal or an expression in parentheses"

    (* Identifiers and attributes. *)
    let unclosed_index_operator_bracket =
      syntax_error "unclosed-index-operator-bracket" "'(', '[' or '{' expected"

    let type_parameter =
      syntax_error
        "type-parameter"
        "a type parameter expects a type variable or '_', as in ''a' or '+'a'"

    (* Core types. *)
    let optional_argument_type =
      syntax_error
        "optional-argument-type"
        "an optional label in a type expects the type of the argument"

    let optional_argument_arrow =
      syntax_error "optional-argument-arrow" "'->' expected"

    let arrow_result =
      syntax_error "arrow-result" "'->' in a type expects the result type"

    let poly_type_body =
      syntax_error "poly-type-body" "the type variables and '.' expect a type"

    let parenthesized_type =
      syntax_error "parenthesized-type" "'(' in a type expects a type"

    let unclosed_parenthesized_type =
      syntax_error "unclosed-parenthesized-type" "')' expected"

    (* Expressions. *)
    let record_field_name =
      syntax_error
        "record-field-name"
        "a record expects a lowercase identifier or a path, as in 'x' or 'M.x'"

    (* Class types. *)
    let class_arrow = syntax_error "class-arrow" "'->' expected"

    let unclosed_class_type_arguments =
      syntax_error "unclosed-class-type-arguments" "',' or ']' expected"

    let class_type_name =
      syntax_error
        "class-type-name"
        "'#' expects a lowercase identifier or a path, as in '#c' or '#M.c'"

    let type_after_module_path =
      syntax_error
        "type-after-module-path"
        "'.' and a lowercase identifier expected, as in 'M.t': a module is not a type"

    let method_type =
      syntax_error "method-type" "the name of a method and ':' expect a type"

    (* Module types. *)
    let functor_type_arrow_module =
      syntax_error "functor-type-arrow-module" "'->' expected"

    let functor_result_type =
      syntax_error
        "functor-result-type" "'->' expects the module type of the result"

    let module_type_of = syntax_error "module-type-of" "'type of' expected"
    let with_type_definition =
      syntax_error
        "with-type-definition"
        "'=' or ':=' in a 'with type' constraint expects a type"

    let constraint_left =
      syntax_error "constraint-left" "'constraint' expects a type"

    let constraint_equal = syntax_error "constraint-equal" "'=' expected"
    let constraint_right =
      syntax_error "constraint-right" "'=' in a type constraint expects a type"

    (* Signatures. *)
    let signature_item = syntax_error "signature-item" "a signature expects a declaration, such as 'val x : int', 'type t' or 'module M : S'"
    let module_name =
      syntax_error
        "module-name" "'module' expects a capitalized identifier or '_'"

    let recursive_module_name =
      syntax_error
        "recursive-module-name"
        "'module rec' or 'and' expects a capitalized identifier or '_'"

    let module_declaration_type =
      syntax_error
        "module-declaration-type"
        "':' in a module declaration expects a module type"

    let module_declaration_functor_argument =
      syntax_error
        "module-declaration-functor-argument"
        "'(' expects a functor argument, as in '(X : S)' or '()'"

    let open_path =
      syntax_error
        "open-path"
        "'open' expects a capitalized identifier or a path, as in 'M' or 'M.N'"

    (* Type declarations. *)
    let unclosed_record_type =
      syntax_error "unclosed-record-type" "';' or '}' expected"

    let constructor_name =
      syntax_error
        "constructor-name" "a variant type expects a capitalized identifier"

    let constructor_tuple_element =
      syntax_error
        "constructor-tuple-element"
        "'*' in the arguments of a constructor expects a type"

    let gadt_arrow = syntax_error "gadt-arrow" "'->' expected"
    let gadt_result =
      syntax_error
        "gadt-result"
        "'->' in the type of a constructor expects the result type"

    (* Classes. *)
    let instance_variable_name =
      syntax_error
        "instance-variable-name" "'val' expects a lowercase identifier"

    let instance_variable_type =
      syntax_error
        "instance-variable-type" "':' after an instance variable expects a type"

    let method_name =
      syntax_error "method-name" "'method' expects a lowercase identifier"

    let class_fun_parameter =
      syntax_error
        "class-fun-parameter" "'fun' in a class expression expects a parameter"

    let class_let_extension =
      syntax_error
        "class-let-extension"
        "a 'let' in a class expression cannot have an extension"

    let class_let_body =
      syntax_error
        "class-let-body" "'in' in a class expression expects a class expression"

    let unclosed_object = syntax_error "unclosed-object" "this object expects 'val', 'method', 'inherit', 'initializer', 'constraint' or 'end'"
    let unclosed_self = syntax_error "unclosed-self" "')' expected"
    let method_equal = syntax_error "method-equal" "'=' expected"
    let method_body =
      syntax_error "method-body" "'=' in a method expects an expression"

    let class_definition =
      syntax_error
        "class-definition"
        "'=' in a class definition expects a class expression"

    (* Module expressions and structures. *)
    let unpack_type =
      syntax_error
        "unpack-type" "':' or ':>' in '(val e : S)' expects a module type"

    let unclosed_parenthesized_module_expr =
      syntax_error "unclosed-parenthesized-module-expr" "')' expected"

    let structure_item =
      syntax_error
        "structure-item"
        "a structure expects a definition, such as 'let x = 1', 'type t = int' or 'module M = N', \
       or an expression"

    let module_definition =
      syntax_error
        "module-definition" "'=' in a module definition expects a module"

    (* Let bindings. *)
    let let_type = syntax_error "let-type" "':' in a binding expects a type"
    let let_typed_equal = syntax_error "let-typed-equal" "'=' expected"
    let let_value =
      syntax_error "let-value" "'=' in a binding expects an expression"

    let let_function_equal = syntax_error "let-function-equal" "'=' expected"
    let function_body =
      syntax_error
        "function-body" "'=' in a function definition expects an expression"

    let let_pattern_equal = syntax_error "let-pattern-equal" "'=' expected"
    let letop_value =
      syntax_error "letop-value" "'=' in a 'let*' binding expects an expression"

    let type_constraint_type =
      syntax_error "type-constraint-type" "':' or ':>' expects a type"

    (* Function parameters and cases. *)
    let optional_parameter_name =
      syntax_error
        "optional-parameter-name" "'?' expects a lowercase identifier"

    let unclosed_optional_parameter =
      syntax_error "unclosed-optional-parameter" "')' expected"

    let labelled_parameter_name =
      syntax_error
        "labelled-parameter-name"
        "'~' in a parameter expects a lowercase identifier"

    let parenthesized_pattern =
      syntax_error "parenthesized-pattern" "'(' in a pattern expects a pattern"

    let pattern_constraint_type =
      syntax_error "pattern-constraint-type" "':' in a pattern expects a type"

    let unclosed_parenthesized_pattern =
      syntax_error "unclosed-parenthesized-pattern" "')' expected"

    let unclosed_newtypes = syntax_error "unclosed-newtypes" "')' expected"
    let punned_label_name =
      syntax_error "punned-label-name" "'~(' expects a lowercase identifier"

    let unclosed_punned_label =
      syntax_error "unclosed-punned-label" "')' expected"

    (* Expressions. *)
    let cons_rhs = syntax_error "cons-rhs" "'::' expects an expression"
    let labelled_expr_alone =
      syntax_error
        "labelled-expr-alone"
        "',' expected, a labelled expression '~x:e' is an element of a tuple"

    let label_value =
      syntax_error
        "label-value"
        "a label expects an argument, such as an identifier, a literal or an expression in \
       parentheses"

    let tilde_label_name =
      syntax_error "tilde-label-name" "'~' expects a lowercase identifier"

    let operator_rhs operator =
      syntax_error
        "operator-rhs"
        (Printf.sprintf "'%s' expects its right operand" operator)

    let sign_operand sign =
      syntax_error
        "sign-operand"
        (Printf.sprintf "the sign '%s' expects an expression" sign)

    let assignment_value =
      syntax_error "assignment-value" "'<-' expects an expression"

    let let_body = syntax_error "let-body" "'in' expects an expression"
    let hashop_rhs operator =
      syntax_error
        "hashop-rhs" (Printf.sprintf "'%s' expects %s" operator simple_argument)

    let prefix_operand operator =
      syntax_error
        "prefix-operand"
        (Printf.sprintf "'%s' expects %s" operator simple_argument)

    let field_after_module =
      syntax_error
        "field-after-module"
        "'.' and a lowercase identifier expected, as in 'r.M.x'"

    let unclosed_index closing =
      syntax_error "unclosed-index" (Printf.sprintf "'%s' expected" closing)

    let index_expr = syntax_error "index-expr" "an index expects an expression"
    let pack_type =
      syntax_error
        "pack-type" "':' in a first-class module expects a module type"

    let unclosed_pack = syntax_error "unclosed-pack" "')' expected"
    let unclosed_parenthesized_expr =
      syntax_error "unclosed-parenthesized-expr" "')' expected"

    (* Lexical errors, with the text of the token: their messages are complete. *)
    let lexer (reason : Token.lexing_error) text =
      let text =
        if String.length text > 20 then String.sub text 0 20 ^ "..." else text
      in
      match reason with
      | `Illegal_escape ->
          syntax_error
            "illegal-escape"
            ( Printf.sprintf
                "Illegal escape sequence in '%s': the escape sequences are '\\\\', '\\\"', '\\'', \
               '\\n', '\\t', '\\b', '\\r', '\\ ', '\\ddd', '\\xhh', '\\ohhh' and '\\u{h...}'."
                text
            )
      | `Invalid_encoding ->
          syntax_error
            "invalid-encoding" "Illegal text, the source expects valid UTF-8."
      | `Invalid_identifier_character ->
          syntax_error "invalid-identifier-character" (Printf.sprintf "Illegal identifier '%s', an identifier expects letters, digits, '_' and quotes." text)
      | `Non_lowercase_delimiter ->
          syntax_error
            "quoted-string-delimiter"
            ( Printf.sprintf
                "Illegal delimiter in '%s', the delimiter of a quoted string expects lowercase \
               letters and '_'."
                text
            )
      | `Capitalized_label ->
          syntax_error
            "capitalized-label"
            ( Printf.sprintf
                "Illegal label '%s', a label expects a lowercase identifier."
                text
            )
      | `Keyword_label ->
          syntax_error
            "keyword-label"
            ( Printf.sprintf
                "Illegal label '%s', a keyword cannot be a label." text
            )
      | `Reserved_sequence ->
          syntax_error
            "reserved-sequence"
            "Illegal '.~', this sequence is reserved for MetaOCaml: a space is expected between \
           '.' and '~'."
      | `Capitalized_raw_identifier ->
          syntax_error
            "capitalized-raw-identifier"
            ( Printf.sprintf
                "Illegal raw identifier '%s', a raw identifier expects a lowercase identifier."
                text
            )
      | `Invalid_literal ->
          syntax_error
            "invalid-literal"
            ( Printf.sprintf
                "Illegal number '%s', a number expects digits and '_', possibly with a suffix \
               'l', 'L' or 'n'."
                text
            )
      | `Empty_character_literal ->
          syntax_error
            "empty-character-literal"
            "Illegal character literal '''', a character literal expects one character."
      | `Illegal_character ->
          syntax_error
            "illegal-character" (Printf.sprintf "Illegal character '%s'." text)
      | `Invalid_directive ->
          syntax_error
            "invalid-directive"
            ( Printf.sprintf
                "Illegal line directive '%s', a line directive expects a line number and a file \
               name."
                text
            )
      | `Unterminated_comment ->
          syntax_error
            "unterminated-comment"
            "Unterminated comment, '(*' expects '*)' (a '\"' in a comment starts a string, \
           which expects '\"' too)."
      | `Unterminated_string ->
          syntax_error
            "unterminated-string" "Unterminated string, '\"' expects '\"'."
      | `Unterminated_quoted_string ->
          syntax_error
            "unterminated-quoted-string"
            ( Printf.sprintf
                "Unterminated quoted string, '%s' expects its closing delimiter followed by '}'." (String.sub text 0 (String.index text '|' + 1))
            )

    let annotation_newtype_names =
      syntax_error
        "annotation-newtype-names"
        "'type' in a type annotation expects lowercase identifiers"
  end

(* {1 Tokens} *)

let peek state offset : Token.any_kind =
  let index = state.index + offset in
  if index < Array.length state.tokens then state.tokens.(index).kind else `EOF

let next state = peek state 0

let is state (kind : [< Token.any_kind ]) =
  next state = (kind :> Token.any_kind)

(* The next token, of [kind], the kind which the caller has just matched on
   [next state] and bound ([| `LIDENT _ as kind -> shift state kind]): it is
   not checked again. *)
let shift state (kind : 'kind) : 'kind tok =
  let leading =
    Doc_comments.remaining state.docs state.index
    @ state.tokens.(state.index).leading
  in
  state.index <- state.index + 1; { Token.leading; kind }

(* The next token if [select] accepts its kind, with a more precise kind. *)
let take_opt state (select : Token.any_kind -> 'kind option) : 'kind tok option
  =
    match select (next state) with
    | Some kind when state.index < Array.length state.tokens ->
        Some (shift state kind)
    | _ -> None

(* As [take_opt], or fails with [error]. *)
let take state error select =
  match take_opt state select with
  | Some token -> token
  | None -> raise (failure state error)

let select_kind (kind : [< Token.any_kind ]) next =
  if next = (kind :> Token.any_kind) then Some kind else None

(* The next token, of a kind already checked. *)
let token state (kind : ([< Token.any_kind ] as 'kind)) : 'kind tok =
  match select_kind kind (next state) with
  | Some _ -> take state internal (select_kind kind)
  | None ->
      ( raise
          (internal_failure state ("expected " ^ Token.text (Token.make kind)))
        [@coverage off]
      )

(* A token taken without settling the doc comments before it, until the
   construct it starts or separates knows how it claims them: [settle] then
   takes the doc comments which nothing claims as warnings, as [take] does,
   as the semantic actions of parser.mly run when a rule is reduced. *)
type 'kind pending = { pending_index : int; pending_kind : 'kind }

(* The next token, of a kind already checked, pending. *)
let take_pending state (kind : ([< Token.any_kind ] as 'kind)) : 'kind pending
  =
    match select_kind kind (next state) with
    | Some _ ->
        let index = state.index in
        state.index <- index + 1; { pending_index = index; pending_kind = kind }
    | None ->
        ( raise
            ( internal_failure
                state ("expected " ^ Token.text (Token.make kind))
            )
          [@coverage off]
        )

(* A pending token before its doc comments are settled, without them. *)
let unsettled state { pending_index; pending_kind } : _ tok =
  { Token.leading = state.tokens.(pending_index).leading; kind = pending_kind }

let settle state { pending_index; pending_kind } : _ tok =
  { Token.leading =
      Doc_comments.remaining state.docs pending_index
      @ state.tokens.(pending_index).leading
  ;
    kind = pending_kind
  }

(* The next token, which must be of [kind]. *)
let expect state (kind : ([< Token.any_kind ] as 'kind)) error : 'kind tok =
  take state error (select_kind kind)

let optional state kind = take_opt state (select_kind kind)

(* The token closing a group opened by the token at [opening], or [error],
   which also points at the opening token. *)
let closing state ~opening kind error =
  if is state kind then token state kind
  else
    raise
      ( failure
          state error
          ~notes:
            [ opening,
              Printf.sprintf
                "This '%s' is not closed." (Token.text state.tokens.(opening))
            ]
      )

(* [left content right], with fixed opening and closing tokens. The caller
   checks the opening token. *)
let delimited state left right ~unclosed content : _ delimited =
  let opening = state.index in
  let left = token state left in
  let content = content state in
  { left; content; right = closing state ~opening right unclosed }

let paren state ~unclosed content =
  delimited state `LPAREN `RPAREN ~unclosed content

(* A token of an already checked kind, followed by a value. *)
let preceded state kind value =
  let token = token state kind in
  token, value state

(* An optional token followed by a value. *)
let preceded_option state kind value =
  if is state kind then Some (preceded state kind value) else None

let mk node = { node; attributes = [] }

(* Attributes after a node do not change it: [Typ.attr], [Exp.attr]... *)
let add_attributes (value : 'a with_attributes) attributes =
  { value with attributes = value.attributes @ attributes }

let select_lident =
  function
    `LIDENT n -> Some (`LIDENT n)
  | _ -> None

let lident state error : lident = take state error select_lident

(* The next token if it is a lowercase identifier. *)
let optional_lident state : lident option = take_opt state select_lident

let uident state error : uident =
  take
    state error
    ( function
        `UIDENT n -> Some (`UIDENT n)
      | _ -> None
    )

let ident state error : ident_token =
  take
    state error
    ( function
        `LIDENT n -> Some (`LIDENT n)
      | `UIDENT n -> Some (`UIDENT n)
      | _ -> None
    )

let select_string =
  function
    `STRING s -> Some (`STRING s)
  | _ -> None

let string_literal state error : string_token = take state error select_string

let is_lident =
  function
    `LIDENT _ -> true
  | _ -> false

let is_uident =
  function
    `UIDENT _ -> true
  | _ -> false

let is_ident =
  function
    `LIDENT _ | `UIDENT _ -> true
  | _ -> false

(* {1 Lists} *)

(* A nonempty list of elements separated by [separator]: the next element
   starts when [continue] accepts the token after the separator. *)
let non_empty_separated
    ?(continue = fun _ -> true) state separator element first
  =
    let rec rest values =
      if is state separator && continue (peek state 1) then
        let separator = token state separator in
        let value = element state in
        rest ((separator, value) :: values)
      else List.rev values
    in
    { first; rest = rest [] }

let separated state separator element =
  Many (non_empty_separated state separator element (element state))

(* A nonempty list, separated or terminated with [;]: [stop] tells whether the
   list ends after a [;]. *)
let rec separated_or_terminated state element ~stop : _ semis =
  separated_or_terminated_from state element ~stop (element state)

(* As [separated_or_terminated], after the first element. *)
and separated_or_terminated_from state element ~stop first : _ semis =
  let rec rest values =
    if is state `SEMI then
      let separator = token state `SEMI in
      if stop (next state) then List.rev values, Some separator
      else
        let value = element state in
        rest ((separator, value) :: values)
    else List.rev values, None
  in
  let rest, trailing = rest [] in
  { first; rest }, trailing

(* A nonempty list of elements separated or terminated by [;], before a
   closing token. *)
let semi_list element closing state =
  separated_or_terminated state element ~stop:(fun kind -> kind = closing)

let semi_list_or_empty element closing state =
  if next state = closing then None else Some (semi_list element closing state)

(* The elements parsed by [element] while [predicate state] holds. *)
let rec repeat_while state predicate element =
  if predicate state then
    let value = element state in
    value :: repeat_while state predicate element
  else []

let rec repeat state predicate element =
  if predicate (next state) then
    let value = element state in
    value :: repeat state predicate element
  else []

(* {1 Doc comments}

   The parser claims the doc comments as the semantic actions of parser.mly,
   at the boundaries before and after items ({!Doc_comments}), before taking
   the next token, which takes the doc comments that nothing claims as
   warnings. *)

(* How an item claims the doc comments around it. *)
type claim =
  | Documented (** [symbol_docs]: the item takes the doc comments around it. *)
  | Marked
  (** [mark_symbol_docs]: the item takes no doc comment, but makes the doc
      comments around it ambiguous, as a floating attribute. *)
  | Undocumented (** [;;] *)

(* The doc comment after an item ending at the next token. *)
let claim_after state =
  function
  | Documented -> Doc_comments.post_docs state.docs state.index
  | Marked -> Doc_comments.mark_post state.docs state.index; None
  | Undocumented -> None

(* The doc comment before an item starting at the token at [index]. *)
let claim_before_at state index =
  function
  | Documented -> Doc_comments.pre_docs state.docs index
  | Marked -> Doc_comments.mark_pre state.docs index; None
  | Undocumented -> None

(* The doc comment before an item starting at the next token. *)
let claim_before state claim = claim_before_at state state.index claim

(* How [and_list] treats the doc comments before each [and]. *)
type and_docs =
  | Claim_docs (** claims them, in a group of items *)
  | Ignore_docs (** leaves them as warnings, in an expression *)
  | Delay_docs of int list ref
  (** leaves them, and the [and]s pending (from the last one), until the
      group is known to be items or an expression ([settle_let_bindings]) *)

(* How an element of [items] claims the doc comments before it: before it is
   parsed ([Claim_first]), or once it is parsed far enough to know it, its
   first token being pending until then ([Claim_after]): the parser of the
   element then calls the function it receives with its claim, before
   settling its first token. *)
type 'a element_parser =
  | Claim_first of claim * (state -> 'a)
  | Claim_after of ((claim -> unit) -> state -> 'a)

(* [symbol_info]: the doc comment of a constructor or a field ending at the
   next token. *)
let info state =
  Option.map
    (Doc_comments.place state.docs) (Doc_comments.info state.docs state.index)

(* A list of items and their doc comments, as [extra_text] and [text_str] in
   parser.mly. [element state] says how the item at the next token claims
   doc comments, and parses it, or returns [None] at the end of the list.
   The doc comments at the start of a list are only taken when the list has
   items for the compiler: for a list with [;;], [starts] tells whether an
   item starts with a token. *)
let items
  : 'a.
      state -> ?starts:(Token.any_kind -> bool)
      -> (state -> 'a element_parser option)
      -> 'a element list
  =
  fun state ?starts element ->
    let docs = state.docs in
    (* [extra_text]: does the list have items, or floating doc comments? *)
    let nonempty =
      match starts with
      | None -> element state <> None
      | Some starts ->
          let rec loop index =
            if
              index < Array.length state.tokens
              && state.tokens.(index).kind = `SEMISEMI
            then Doc_comments.has_text docs index || loop (index + 1)
            else
              starts
                ( if index < Array.length state.tokens then
                    state.tokens.(index).kind
                  else `EOF
                )
          in
          loop state.index
    in
    let finish previous post elements =
      match previous with
      | Some (_, pre, item) -> Item (pre, item, post) :: elements
      | None -> elements
    in
    let floating values elements =
      List.rev_append (List.map (fun value -> Floating value) values) elements
    in
    let rec loop ~first previous elements =
      let k = state.index in
      let post =
        match previous with
          Some (claim, _, _) -> claim_after state claim
        | None -> None
      in
      match element state with
      | None ->
          let texts =
            if nonempty then Doc_comments.post_extra_text docs k
            else
              let post = Doc_comments.post_text docs k in
              post @ Doc_comments.post_extra_text docs k
          in
          let post, values, _ =
            Doc_comments.resolve docs k ~post ~texts ~pre:None
          in
          List.rev (floating values (finish previous post elements))
      | Some element ->
          (* The doc comments before the item, once its claim is known. *)
          let start claim =
            let extra =
              if first && nonempty then Doc_comments.pre_extra_text docs k
              else []
            in
            let texts = extra @ Doc_comments.text docs k in
            let pre = claim_before_at state k claim in
            Doc_comments.resolve docs k ~post ~texts ~pre
          in
          let claim, (post, values, pre), item =
            match element with
            | Claim_first (claim, parse) ->
                let started = start claim in
                claim, started, parse state
            | Claim_after parse ->
                let started = ref None in
                let item =
                  parse
                    (fun claim -> started := Some (claim, start claim)) state
                in
                begin match !started with
                | Some (claim, started) -> claim, started, item
                | None ->
                    ( raise (internal_failure state "an item without its claim")
                      [@coverage off]
                    )
                end
          in
          let elements = floating values (finish previous post elements) in
          loop ~first:false (Some (claim, pre, item)) elements
    in
    loop ~first:true None []

(* An item alone, such as [let module M = N in e], with its doc comments. *)
let documented state claim parse =
  let pre = claim_before state claim in
  let _, _, pre =
    Doc_comments.resolve state.docs state.index ~post:None ~texts:[] ~pre
  in
  let item = parse state in
  let post = claim_after state claim in
  let post, _, _ =
    Doc_comments.resolve state.docs state.index ~post ~texts:[] ~pre:None
  in
  (pre, item, post)

(* {1 Identifiers} *)

let attr_id_kind =
  function
  | `LIDENT _ | `UIDENT _ -> true
  | kind -> Token.keyword_of_string (Token.text (Token.make kind)) = Some kind

let attr_id_token state error : attr_id_token =
  take
    state error
    begin function
    | `LIDENT n -> Some (`LIDENT n)
    | `UIDENT n -> Some (`UIDENT n)
    | kind ->
        match Token.keyword_of_string (Token.text (Token.make kind)) with
        | Some keyword when (keyword :> Token.any_kind) = kind -> Some keyword
        | _ -> None
    end

let attr_id state error =
  non_empty_separated
    state `DOT (fun state -> attr_id_token state internal)
    (attr_id_token state error) ~continue:attr_id_kind

let is_operator_kind =
  function
  | `PREFIXOP _ | `LETOP _ | `ANDOP _ | `HASHOP _ | `BANG | `INFIXOP0 _
    | `INFIXOP1 _ | `INFIXOP2 _ | `INFIXOP3 _ | `INFIXOP4 _ | `PLUS | `PLUSDOT
    | `PLUSEQ | `MINUS | `MINUSDOT | `STAR | `PERCENT | `EQUAL | `LESS
    | `GREATER | `OR | `BARBAR | `AMPERSAND | `AMPERAMPER | `COLONEQUAL
  -> true
  | _ -> false

let operator_token state : operator_token =
  take
    state internal
    begin function
    | `PREFIXOP op -> Some (`PREFIXOP op)
    | `LETOP op -> Some (`LETOP op)
    | `ANDOP op -> Some (`ANDOP op)
    | `HASHOP op -> Some (`HASHOP op)
    | `BANG -> Some `BANG
    | `INFIXOP0 op -> Some (`INFIXOP0 op)
    | `INFIXOP1 op -> Some (`INFIXOP1 op)
    | `INFIXOP2 op -> Some (`INFIXOP2 op)
    | `INFIXOP3 op -> Some (`INFIXOP3 op)
    | `INFIXOP4 op -> Some (`INFIXOP4 op)
    | `PLUS -> Some `PLUS
    | `PLUSDOT -> Some `PLUSDOT
    | `PLUSEQ -> Some `PLUSEQ
    | `MINUS -> Some `MINUS
    | `MINUSDOT -> Some `MINUSDOT
    | `STAR -> Some `STAR
    | `PERCENT -> Some `PERCENT
    | `EQUAL -> Some `EQUAL
    | `LESS -> Some `LESS
    | `GREATER -> Some `GREATER
    | `OR -> Some `OR
    | `BARBAR -> Some `BARBAR
    | `AMPERSAND -> Some `AMPERSAND
    | `AMPERAMPER -> Some `AMPERAMPER
    | `COLONEQUAL -> Some `COLONEQUAL
    | _ -> (None [@coverage off])
    end

(* Does [( operator )] start at [offset]? *)
let operator_at state offset =
  peek state offset = `LPAREN
  &&
  ( is_operator_kind (peek state (offset + 1))
    && peek state (offset + 2) = `RPAREN
    ||
    begin match peek state (offset + 1), peek state (offset + 2) with
    | `DOTOP _, (`LPAREN | `LBRACKET | `LBRACE) -> true
    | _ -> false
    end
  )

(* Parentheses, brackets or braces around a content, which is parsed knowing
   the closing token. The caller checks the opening token. [unclosed] is the
   error for a missing closing token, given its text. *)
let brackets state ~unclosed (content : Token.any_kind -> 'a) : 'a brackets =
  match next state with
  | `LPAREN ->
      Parens
        ( delimited
            state `LPAREN `RPAREN ~unclosed:(unclosed ")")
            (fun _ -> content `RPAREN)
        )
  | `LBRACKET ->
      Brackets
        ( delimited
            state `LBRACKET `RBRACKET ~unclosed:(unclosed "]")
            (fun _ -> content `RBRACKET)
        )
  | `LBRACE ->
      Braces
        ( delimited
            state `LBRACE `RBRACE ~unclosed:(unclosed "}")
            (fun _ -> content `RBRACE)
        )
  | _ -> (raise (internal_failure state "expected a bracket") [@coverage off])

(* The operator of [( op )], checked by [operator_at]. *)
let operator state =
  match next state with
  | `DOTOP _ as kind ->
      let dotop = shift state kind in
      let brackets =
        brackets
          state ~unclosed:E.unclosed_index
          ( fun _ ->
              if is state `SEMI then
                let semi = token state `SEMI in
                Some
                  ( semi,
                    expect
                      state `DOTDOT
                      (syntax_error "index-operator-dotdot" "'..' expected")
                  )
              else None
          )
      in
      Index_operator (dotop, brackets, optional state `LESSMINUS)
  | _ -> Operator (operator_token state)

(* A value name, or [error]. *)
let val_ident state error =
  match optional_lident state with
  | Some name -> Val_lident name
  | None when operator_at state 0 ->
      Val_operator
        ( paren
            state
            ~unclosed:(syntax_error "unclosed-index-operator" "')' expected")
            operator
        )
  | None -> raise (failure state error)

let is_val_ident_at state offset =
  is_lident (peek state offset) || operator_at state offset

(* The offset after a value name starting at [offset]. *)
let after_val_ident state offset =
  if is_lident (peek state offset) then offset + 1
  else
    match peek state (offset + 1) with
    | `DOTOP _ ->
        let offset = offset + 3 in
        let offset = if peek state offset = `SEMI then offset + 2 else offset in
        let offset = offset + 1 in
        let offset =
          if peek state offset = `LESSMINUS then offset + 1 else offset
        in
        offset + 1
    | _ -> offset + 3

(* A module path [A.B(C).D]: functor applications are only allowed in
   extended module paths. *)
(* A module path, starting with an uppercase name or [error]. *)
let module_path state ~apply error =
  let rec start error = loop (Path_ident (uident state error))
  and loop path =
    match next state, peek state 1 with
    | `DOT, `UIDENT _ ->
        let dot = token state `DOT in
        let name = uident state internal in
        loop (Path_dot (path, dot, name))
    | `LPAREN, `UIDENT _ when apply ->
        loop
          ( Path_apply
              ( path,
                paren
                  state
                  ~unclosed:
                    (syntax_error "unclosed-functor-application" "')' expected")
                  (fun _ -> start internal)
              )
          )
    | _ -> path
  in
  start error

(* Splits [A.B.C] into the qualifier [A.B] and the last name [C]. *)
let split_path state =
  function
  | Path_ident name -> None, name
  | Path_dot (path, dot, name) -> Some (path, dot), name
  | Path_apply _ ->
      raise
        ( failure
            state
            ( syntax_error
                "functor-application-module-type"
                "a functor application is not a module type: a module type of its result is expected, \
                 as in 'F(X).S'"
            )
        )

(* A name qualified by a module path, as in [mk_longident(prefix, final)]:
   [error] when the name is missing, before or after the path. *)
let qualified state ~apply final error =
  if is_uident (next state) then begin
    let path = module_path state ~apply internal in
    let dot = expect state `DOT error in
    (Some (path, dot), final state error)
  end
  else (None, final state error)

let type_longident state error = qualified state ~apply:true lident error
let label_longident state error = qualified state ~apply:false lident error
let class_longident state error = qualified state ~apply:false lident error

(* [mty_longident]: an extended module path, with a final identifier, or
   [error]. *)
let mty_longident state error =
  if is_uident (next state) then begin
    let path = module_path state ~apply:true internal in
    if is state `DOT && is_lident (peek state 1) then
      let dot = token state `DOT in
      (Some (path, dot), (lident state internal :> ident_token))
    else
      let qualifier, name = split_path state path in
      (qualifier, (name :> ident_token))
  end
  else (None, (lident state error :> ident_token))

(* A constructor name, or [error]. The parentheses and brackets of [(::)],
   [()] and [[]] are checked by the callers ([is_constr_ident_at]). *)
let constr_ident state error =
  match next state with
  | `UIDENT _ as kind -> Constr_uident (shift state kind)
  | `LPAREN when peek state 1 = `COLONCOLON ->
      Constr_cons
        ( paren
            state
            ~unclosed:(syntax_error "unclosed-cons-constructor" "')' expected")
            (fun state -> token state `COLONCOLON)
        )
  | `LPAREN ->
      Constr_unit
        ( paren
            state
            ~unclosed:(syntax_error "unclosed-unit-constructor" "')' expected")
            ignore
        )
  | `LBRACKET ->
      Constr_nil
        ( delimited
            state `LBRACKET `RBRACKET
            ~unclosed:(syntax_error "unclosed-nil-constructor" "']' expected")
            ignore
        )
  | `FALSE as kind -> Constr_false (shift state kind)
  | `TRUE as kind -> Constr_true (shift state kind)
  | _ -> raise (failure state error)

let is_constr_ident_at state offset =
  match peek state offset, peek state (offset + 1) with
  | `UIDENT _, _ | `FALSE, _ | `TRUE, _
    | `LBRACKET, `RBRACKET | `LPAREN, `RPAREN
  -> true
  | `LPAREN, `COLONCOLON -> peek state (offset + 2) = `RPAREN
  | _ -> false

let name_tag state =
  let backquote = token state `BACKQUOTE in
  ( backquote,
    ident
      state
      (syntax_error "variant-tag-name" "'`' expects an identifier, as in '`A'")
  )

let type_variable state =
  let quote = token state `QUOTE in
  ( quote,
    ident
      state
      ( syntax_error
          "type-variable-name" "a quote expects an identifier, as in ''a'"
      )
  )

(* Does a list of type variables ['a 'b] followed by a dot start at
   [offset]? *)
let typevars_then_dot state offset =
  let rec loop offset count =
    match peek state offset with
    | `QUOTE when is_ident (peek state (offset + 1)) ->
        loop (offset + 2) (count + 1)
    | `DOT -> count > 0
    | _ -> false
  in
  loop offset 0

let rec type_variables state =
  if is state `QUOTE then
    let variable = type_variable state in
    variable :: type_variables state
  else []

(* The values given by [element] until it gives none. *)
let rec repeat_some state element =
  match element state with
  | Some value -> value :: repeat_some state element
  | None -> []

let lidents state = repeat_some state optional_lident

(* A module name or [_], or [error]. *)
let module_name state error =
  if is state `UNDERSCORE then Module_anonymous (token state `UNDERSCORE)
  else Module_name (uident state error)

(* A constant, possibly signed, or [error]. *)
let signed_constant state error =
  match next state, peek state 1 with
  | ((`MINUS | `PLUS) as sign), ((`INT _ | `FLOAT _) as constant) ->
      let sign = shift state sign in
      let constant = shift state constant in
      Signed (sign, constant)
  | _ ->
      Unsigned
        ( take
            state error
            begin function
            | `INT v -> Some (`INT v)
            | `FLOAT v -> Some (`FLOAT v)
            | `STRING v -> Some (`STRING v)
            | `CHAR v -> Some (`CHAR v)
            | _ -> None
            end
        )

let is_signed_constant_at state offset =
  match peek state offset, peek state (offset + 1) with
  | (`INT _ | `FLOAT _ | `STRING _ | `CHAR _), _ -> true
  | (`MINUS | `PLUS), (`INT _ | `FLOAT _) -> true
  | _ -> false

(* Skips a balanced group of square brackets starting at [offset]. *)
let skip_brackets state offset =
  let rec loop offset depth =
    match peek state offset with
    | `EOF -> offset
    | `LBRACKET | `LBRACKETBAR | `LBRACKETLESS | `LBRACKETGREATER
      | `LBRACKETAT | `LBRACKETATAT | `LBRACKETATATAT
      | `LBRACKETPERCENT | `LBRACKETPERCENTPERCENT
    -> loop (offset + 1) (depth + 1)
    | `RBRACKET | `BARRBRACKET ->
        if depth = 1 then offset + 1 else loop (offset + 1) (depth - 1)
    | _ -> loop (offset + 1) depth
  in
  loop offset 0

(* Skips [%ext [@attr] ...] starting at [offset]. *)
let skip_ext_attributes state offset =
  let offset =
    if peek state offset = `PERCENT then
      let rec id offset =
        if attr_id_kind (peek state offset) then
          if peek state (offset + 1) = `DOT then id (offset + 2) else offset + 1
        else offset
      in
      id (offset + 1)
    else offset
  in
  let rec attributes offset =
    if peek state offset = `LBRACKETAT then
      attributes (skip_brackets state offset)
    else offset
  in
  attributes offset

let starts_signature_item =
  function
  | `VAL | `EXTERNAL | `TYPE | `EXCEPTION | `MODULE
    | `OPEN | `INCLUDE | `CLASS | `LBRACKETATATAT
    | `LBRACKETPERCENTPERCENT | `QUOTED_STRING_ITEM _ | `SEMISEMI
  -> true
  | _ -> false

let starts_structure_item =
  function
  | `LET | `VAL | `EXTERNAL | `TYPE | `EXCEPTION | `MODULE | `OPEN | `INCLUDE
    | `CLASS | `LBRACKETATATAT | `LBRACKETPERCENTPERCENT | `QUOTED_STRING_ITEM _
  -> true
  | _ -> false

let starts_simple_expr =
  function
  | `LIDENT _ | `UIDENT _ | `INT _ | `FLOAT _ | `STRING _ | `CHAR _
    | `TRUE | `FALSE | `LPAREN | `LBRACKET | `LBRACKETBAR
    | `LBRACE | `LBRACELESS | `BEGIN | `NEW | `OBJECT | `BACKQUOTE
    | `PREFIXOP _ | `BANG | `LBRACKETPERCENT | `QUOTED_STRING_EXPR _
  -> true
  | _ -> false

let starts_argument =
  function
  | `LABEL _ | `OPTLABEL _ | `TILDE | `QUESTION -> true
  | kind -> starts_simple_expr kind

let starts_expr =
  function
  | `LET | `LETOP _ | `MATCH | `TRY | `IF | `FUN
    | `FUNCTION | `WHILE | `FOR | `ASSERT | `LAZY | `MINUS
    | `MINUSDOT | `PLUS | `PLUSDOT | `LABEL _ | `TILDE
  -> true
  | kind -> starts_simple_expr kind

let starts_simple_pattern =
  function
  | `LIDENT _ | `UIDENT _ | `UNDERSCORE | `INT _ | `FLOAT _ | `STRING _
    | `CHAR _ | `TRUE | `FALSE | `LPAREN | `LBRACKET | `LBRACKETBAR | `LBRACE
    | `BACKQUOTE | `HASH | `LBRACKETPERCENT | `QUOTED_STRING_EXPR _
  -> true
  | _ -> false

let starts_type_parameter =
  function
  | `QUOTE | `UNDERSCORE | `PLUS | `MINUS | `BANG | `INFIXOP2 _ | `PREFIXOP _ ->
      true
  | _ -> false

(* The variance and injectivity of a type parameter, as in [type_variance]
   of parser.mly: [+], [-] or [!], [+] or [-] with [!] before or after it, or
   a single operator such as [+!] or [!-]. *)
let variance state : variance_token list =
  let invalid () = raise (failure state (syntax_error "type-variance" "a variance expects '+', '-', '+-' or '-+', possibly with '!' before or after it")) in
  match next state with
  | `INFIXOP2 op as kind ->
      if
        List.mem
          (Token.Infixop2.to_string op) [ "+!"; "-!"; "+-"; "-+"; "+-!"; "-+!" ]
      then [ shift state kind ]
      else invalid ()
  | `PREFIXOP op as kind ->
      if
        List.mem (Token.Prefixop.to_string op) [ "!+"; "!-"; "!+-"; "!-+" ]
      then [ shift state kind ]
      else invalid ()
  | (`PLUS | `MINUS) as kind ->
      let first = shift state kind in
      begin match next state with
      | `BANG as kind -> [ first; shift state kind ]
      | _ -> [ first ]
      end
  | `BANG as kind ->
      let first = shift state kind in
      begin match next state with
      | (`PLUS | `MINUS) as kind -> [ first; shift state kind ]
      | _ -> [ first ]
      end
  | _ -> []

(* A type parameter, or [error]. *)
let type_parameter error state =
  let variance = variance state in
  let variable =
    match next state with
    | `UNDERSCORE -> `Any (token state `UNDERSCORE)
    | `QUOTE -> `Var (type_variable state)
    | _ -> raise (failure state error)
  in
  (variance, variable)

let type_parameters state =
  if is state `LPAREN then
    Many_type_parameters
      ( paren
          state
          ~unclosed:
            (syntax_error "unclosed-type-parameters" "',' or ')' expected")
          ( fun state ->
              separated state `COMMA (type_parameter E.type_parameter)
          )
      )
  else if starts_type_parameter (next state) then
    One_type_parameter (type_parameter E.type_parameter state)
  else No_type_parameters

let class_parameters state : class_parameters =
  if is state `LBRACKET then
    Some
      ( delimited
          state `LBRACKET `RBRACKET
          ~unclosed:
            ( syntax_error
                "unclosed-class-type-parameters" "',' or ']' expected"
            )
          ( fun state ->
              separated
                state `COMMA
                ( type_parameter
                    ( syntax_error
                        "class-type-parameter"
                        "a class parameter expects a type variable or '_'"
                    )
                )
          )
      )
  else None

(* Flags such as [mutable virtual], in any order, each at most once. *)
let flags state (select : Token.any_kind -> 'kind option) =
  let rec check offset seen =
    match select (peek state offset) with
    | Some kind when List.mem kind seen ->
        raise
          ( failure_at
              state (state.index + offset)
              (syntax_error "repeated-flag" "this flag is already given")
          )
    | Some kind -> check (offset + 1) (kind :: seen)
    | None -> ()
  in
  check 0 []; repeat_some state (fun state -> take_opt state select)

(* {1 The grammar} *)

let rec attribute_with state opening =
  delimited
    state opening `RBRACKET
    ~unclosed:(syntax_error "unclosed-attribute" "']' expected")
    attribute_contents

and attribute_contents state =
  let id =
    attr_id
      state
      ( syntax_error
          "attribute-name"
          "an attribute or an extension expects an identifier, as in '[@inline]'"
      )
  in
  (id, payload state)

and attributes state =
  if is state `LBRACKETAT then
    let attribute = attribute_with state `LBRACKETAT in
    attribute :: attributes state
  else []

and post_item_attributes state =
  if is state `LBRACKETATAT then
    let attribute = attribute_with state `LBRACKETATAT in
    attribute :: post_item_attributes state
  else []

and floating_attribute state = attribute_with state `LBRACKETATATAT

and ext_attributes state =
  let ext =
    preceded_option
      state `PERCENT
      ( fun state ->
          attr_id
            state
            ( syntax_error
                "keyword-extension-name"
                "'%' after a keyword expects an identifier, as in 'let%ext'"
            )
      )
  in
  (ext, attributes state)

(* [first and [@attr] x and ...]. The items claim the doc comments before
   [and] ([docs]), except the bindings of [let] expressions. *)
and and_list : 'a. ?docs:and_docs -> state -> (state -> 'a) -> 'a -> 'a and_list
  =
  fun ?(docs = Claim_docs) state element first ->
    let rest =
      repeat
        state (fun kind -> kind = `AND)
        ( fun state ->
            let docstring, and_ =
              match docs with
              | Claim_docs ->
                  let docstring = and_docstring state state.index in
                  docstring, token state `AND
              | Ignore_docs -> (None, [], None), token state `AND
              | Delay_docs indexes ->
                  let and_ = take_pending state `AND in
                  indexes := and_.pending_index :: !indexes;
                  (None, [], None), unsettled state and_
            in
            let attributes = attributes state in
            ~docstring, and_, attributes, element state
        )
    in
    (first, rest)

(* The doc comments before the [and] at [index], in a group of items. *)
and and_docstring state index =
  let post = Doc_comments.post_docs state.docs index in
  let texts = Doc_comments.text state.docs index in
  let pre = Doc_comments.pre_docs state.docs index in
  Doc_comments.resolve state.docs index ~post ~texts ~pre

(* [| x | y], with an optional leading bar. *)
and bar_list : 'a. state -> (state -> 'a) -> 'a bar_list =
  fun state element ->
    let bar = optional state `BAR in
    (bar, non_empty_separated state `BAR element (element state))

and payload state =
  match next state with
  | `COLON ->
      let colon = token state `COLON in
      if is state `RBRACKET || starts_signature_item (next state) then
        Payload_signature (colon, signature state)
      else
        Payload_type
          ( colon,
            core_type
              state
              ( syntax_error
                  "payload-type"
                  "':' in an attribute or an extension expects a type or a signature"
              )
          )
  | `QUESTION ->
      let question = token state `QUESTION in
      let pattern =
        pattern
          state
          ( syntax_error
              "payload-pattern"
              "'?' in an attribute or an extension expects a pattern"
          )
      in
      let guard =
        preceded_option
          state `WHEN
          ( fun state ->
              seq_expr
                state
                (syntax_error "payload-guard" "'when' expects a condition")
          )
      in
      Payload_pattern (question, pattern, guard)
  | _ -> Payload_structure (structure state)

and extension state =
  match next state with
  | `QUOTED_STRING_EXPR _ as kind -> Quoted_extension (shift state kind)
  | _ ->
      Extension
        ( delimited
            state `LBRACKETPERCENT `RBRACKET
            ~unclosed:(syntax_error "unclosed-extension" "']' expected")
            attribute_contents
        )

and item_extension state =
  match next state with
  | `QUOTED_STRING_ITEM _ as kind -> Quoted_extension (shift state kind)
  | _ ->
      Extension
        ( delimited
            state `LBRACKETPERCENTPERCENT `RBRACKET
            ~unclosed:(syntax_error "unclosed-item-extension" "']' expected")
            attribute_contents
        )

(* {2 Core types}

   The functions parsing a syntactic category take the error to report when
   nothing of this category starts at the next token. *)

and core_type state error =
  let typ = alias_type state error in
  if is state `LBRACKETAT then
    let attributes = attributes state in
    add_attributes typ attributes
  else typ

and core_type_no_attr state error = alias_type state error

and alias_type state error =
  let rec loop typ =
    if is state `AS then
      let as_ = token state `AS in
      if not (is state `QUOTE) then
        raise
          ( failure
              state
              ( syntax_error
                  "alias-type-variable" "'as' in a type expects a type variable"
              )
          )
      ;
      let variable = type_variable state in
      loop (mk (Typ_alias (typ, as_, variable)))
    else typ
  in
  loop (function_type state error)

(* [(module M : S) -> t]. *)
and functor_type_at state offset =
  peek state offset = `LPAREN && peek state (offset + 1) = `MODULE
  &&
  begin let offset = skip_ext_attributes state (offset + 2) in
    is_uident (peek state offset) && peek state (offset + 1) = `COLON
  end

and function_type state error =
  match next state with
  | `OPTLABEL _ as kind ->
      let label = Optional (shift state kind) in
      arrow_rest
        state label (param_type state E.optional_argument_type)
        E.optional_argument_arrow
  | `QUESTION when is_lident (peek state 1) && peek state 2 = `COLON ->
      let question = token state `QUESTION in
      let name = lident state internal in
      let colon = token state `COLON in
      arrow_rest
        state (Optional_split (question, name, colon))
        (param_type state E.optional_argument_type) E.optional_argument_arrow
  | `LIDENT _ when peek state 1 = `COLON ->
      let name = lident state internal in
      let colon = token state `COLON in
      let label = Labelled (name, colon) in
      if functor_type_at state 0 then functor_type state label
      else begin let domain =
          param_type
            state
            ( syntax_error
                "labelled-argument-type"
                "a label in a type expects the type of the argument"
            )
        in
        if is state `MINUSGREATER then arrow_rest state label domain internal
        else
          match domain with
          | { node = Typ_tuple ({ first = (None, first); _ } as elements);
              attributes = []
            }
          ->
              mk
                ( Typ_tuple
                    { elements with first = (Some (name, colon), first) }
                )
          | _ ->
              raise
                ( failure
                    state
                    (syntax_error "labelled-type-arrow" "'->' or '*' expected")
                )
      end
  | _ when functor_type_at state 0 -> functor_type state Nolabel
  | _ ->
      let domain = param_type state error in
      if is state `MINUSGREATER then arrow_rest state Nolabel domain internal
      else begin
        begin match domain with
        | { node = Typ_paren { content = { node = Typ_poly _; _ }; _ };
            attributes = []
          }
        ->
            raise
              ( failure
                  state
                  ( syntax_error
                      "poly-type-arrow"
                      "a polymorphic type in parentheses is the type of an argument, and expects '->'"
                  )
              )
        | _ -> ()
        end;
        domain
      end

and arrow_rest state label domain error =
  (* [extra_rhs]: the doc comment after the domain. *)
  let doc = info state in
  let arrow = expect state `MINUSGREATER error in
  let codomain = function_type state E.arrow_result in
  mk (Typ_arrow (label, (domain, doc), arrow, codomain))

and functor_type state label =
  let parameter =
    paren
      state
      ~unclosed:(syntax_error "unclosed-functor-type-argument" "')' expected")
      ( fun state ->
          let module_ = token state `MODULE in
          let ext_attributes = ext_attributes state in
          let name = uident state internal in
          let colon = token state `COLON in
          ( module_, ext_attributes, name, colon,
            package_module_type
              state
              ( syntax_error
                  "functor-type-package"
                  "':' in a first-class module argument expects a module type"
              )
          )
      )
  in
  let arrow =
    expect
      state `MINUSGREATER (syntax_error "functor-type-arrow" "'->' expected")
  in
  let codomain = function_type state E.arrow_result in
  mk (Typ_functor (label, parameter, arrow, codomain))

and param_type state error =
  if is state `LPAREN && typevars_then_dot state 1 then
    mk
      ( Typ_paren
          ( paren
              state ~unclosed:(syntax_error "unclosed-poly-type" "')' expected")
              poly_type
          )
      )
  else tuple_type state error

(* ['a 'b. t], checked by [typevars_then_dot]. *)
and poly_type state =
  let variables = type_variables state in
  let dot = token state `DOT in
  mk (Typ_poly (variables, dot, core_type state E.poly_type_body))

(* A type parsed by [body], possibly polymorphic. *)
and possibly_poly state error body =
  if typevars_then_dot state 0 then
    let variables = type_variables state in
    let dot = token state `DOT in
    mk (Typ_poly (variables, dot, body state E.poly_type_body))
  else body state error

and tuple_type state error = tuple_type_from state (atomic_type state error)

(* A tuple type, after its first element. *)
and tuple_type_from state first =
  if is state `STAR then
    let element state =
      let label =
        if is_lident (next state) && peek state 1 = `COLON then
          let name = lident state internal in
          Some (name, token state `COLON)
        else None
      in
      label,
      atomic_type
        state
        (syntax_error "tuple-type-element" "'*' in a type expects another type")
    in
    mk (Typ_tuple (non_empty_separated state `STAR element (None, first)))
  else first

and atomic_type state error =
  let base =
    match next state with
    | `LPAREN when peek state 1 = `MODULE -> package_type state
    | `LPAREN ->
        let opening = state.index in
        let left = token state `LPAREN in
        let first = core_type state E.parenthesized_type in
        if is state `COMMA then begin let content =
            Many
              ( non_empty_separated
                  state `COMMA
                  ( fun state ->
                      core_type
                        state
                        ( syntax_error
                            "type-argument"
                            "',' in type arguments expects another type"
                        )
                  )
                  first
              )
          in
          let right =
            closing
              state ~opening `RPAREN
              (syntax_error "unclosed-type-arguments" "',' or ')' expected")
          in
          type_application
            state (Type_arguments { left; content; right })
            ( syntax_error
                "type-arguments-constructor"
                "type arguments in parentheses expect a lowercase identifier or a path, as in \
                   '(int, string) result'"
            )
        end
        else
          let right =
            closing state ~opening `RPAREN E.unclosed_parenthesized_type
          in
          mk (Typ_paren { left; content = first; right })
    | `LESS -> object_type state
    | (`LBRACKET | `LBRACKETGREATER | `LBRACKETLESS) as kind ->
        variant_type state kind
    | `LBRACKETPERCENT | `QUOTED_STRING_EXPR _ ->
        mk (Typ_extension (extension state))
    | `QUOTE -> mk (Typ_var (type_variable state))
    | `UNDERSCORE -> mk (Typ_any (token state `UNDERSCORE))
    | `HASH ->
        let hash = token state `HASH in
        mk
          ( Typ_class
              (No_type_arguments, hash, type_longident state E.class_type_name)
          )
    | `LIDENT _ as kind ->
        mk (Typ_constr (No_type_arguments, (None, shift state kind)))
    | `UIDENT _ ->
        let path = module_path state ~apply:true internal in
        let dot = expect state `DOT E.type_after_module_path in
        begin match next state with
        | `LIDENT _ as kind ->
            mk
              ( Typ_constr
                  (No_type_arguments, (Some (path, dot), shift state kind))
              )
        | `LPAREN when peek state 1 = `MODULE ->
            mk (Typ_open (path, dot, package_type state))
        | `LPAREN ->
            let typ =
              mk
                ( Typ_paren
                    ( paren
                        state ~unclosed:E.unclosed_parenthesized_type
                        (fun state -> core_type state E.parenthesized_type)
                    )
                )
            in
            mk (Typ_open (path, dot, typ))
        | (`LBRACKET | `LBRACKETGREATER | `LBRACKETLESS) as kind ->
            mk (Typ_open (path, dot, variant_type state kind))
        | _ -> raise (failure state E.type_after_module_path)
        end
    | _ -> raise (failure state error)
  in
  type_postfix state base

and type_application state arguments error =
  match next state with
  | `HASH ->
      let hash = token state `HASH in
      type_postfix
        state
        ( mk
            ( Typ_class
                (arguments, hash, type_longident state E.class_type_name)
            )
        )
  | `LIDENT _ | `UIDENT _ ->
      type_postfix
        state
        ( mk
            ( Typ_constr
                ( arguments,
                  type_longident
                    state
                    ( syntax_error
                        "type-constructor-name"
                        "'.' and a lowercase identifier expected, as in 'M.t'"
                    )
                )
            )
        )
  | _ -> raise (failure state error)

and type_postfix state typ =
  match next state with
  | `LIDENT _ | `UIDENT _ | `HASH ->
      type_application state (Type_argument typ) internal
  | _ -> typ

and package_type state =
  mk
    ( Typ_package
        ( paren
            state
            ~unclosed:(syntax_error "unclosed-package-type" "')' expected")
            ( fun state ->
                let module_ = token state `MODULE in
                let ext_attributes = ext_attributes state in
                ( module_, ext_attributes,
                  package_module_type
                    state
                    ( syntax_error
                        "package-type"
                        "'module' in a type expects a module type"
                    )
                )
            )
        )
    )

and object_type state =
  let opening = state.index in
  let less = token state `LESS in
  let rec fields values =
    match next state with
    | `GREATER -> List.rev values, None
    | `DOTDOT ->
        let dotdot = token state `DOTDOT in
        List.rev values, Some dotdot
    | `LIDENT _ when peek state 1 = `COLON ->
        let name = lident state internal in
        let colon = token state `COLON in
        let typ = possibly_poly state E.method_type core_type_no_attr in
        let attributes = attributes state in
        let doc, semi = field_end state in
        let field = Object_tag (name, colon, typ, attributes, doc, semi) in
        if semi = None then List.rev (field :: values), None
        else fields (field :: values)
    | _ ->
        let typ =
          atomic_type
            state
            ( syntax_error
                "object-type-field"
                "an object type expects a method 'm : t', an inherited type or '..'"
            )
        in
        let semi = optional state `SEMI in
        let field = Object_inherit (typ, semi) in
        if semi = None then List.rev (field :: values), None
        else fields (field :: values)
  in
  let fields, dotdot = fields [] in
  let greater =
    closing
      state ~opening `GREATER
      (syntax_error "unclosed-object-type" "';' or '>' expected")
  in
  mk (Typ_object { left = less; content = (fields, dotdot); right = greater })

and attributes_after_semi state = attributes state

(* A polymorphic variant type, whose opening bracket, of [kind], the caller
   has matched. *)
and variant_type state kind =
  let opening_index = state.index in
  let opening = shift state kind in
  let leading_bar = optional state `BAR in
  let fields =
    if is state `RBRACKET then begin
      (* Only [[>]] is empty, without a bar. *)
      if opening.kind <> `LBRACKETGREATER || leading_bar <> None then
        raise
          ( failure
              state
              ( syntax_error
                  "variant-type-empty"
                  "a polymorphic variant type expects a tag (only '[> ]' can be empty)"
              )
          )
      ;
      Zero
    end
    else separated state `BAR row_field
  in
  variant_type_rest state ~opening_index opening leading_bar fields

(* A polymorphic variant type, after its fields. *)
and variant_type_rest state ~opening_index opening leading_bar fields =
  begin match opening.kind, leading_bar, fields with
  | `LBRACKET, None, Many { first = Row_inherit _; rest = [] } ->
      raise (failure state (syntax_error "variant-type-single-inherit" "a polymorphic variant type made of a single included type expects a '|' before it"))
  | _ -> ()
  end;
  let tags =
    if is state `GREATER then begin if opening.kind <> `LBRACKETLESS then
        raise
          ( failure
              state
              ( syntax_error
                  "variant-type-tags"
                  "only a type '[< ...]' can list its present tags after '>'"
              )
          )
      ;
      let greater = token state `GREATER in
      let tags = repeat state (fun kind -> kind = `BACKQUOTE) name_tag in
      if tags = [] then
        raise
          ( failure
              state
              ( syntax_error
                  "variant-type-tags-empty"
                  "'>' in a polymorphic variant type expects tags"
              )
          )
      ;
      Some (greater, tags)
    end
    else None
  in
  let closing =
    closing
      state ~opening:opening_index `RBRACKET
      (syntax_error "unclosed-variant-type" "'|' or ']' expected")
  in
  mk
    ( Typ_variant
        { left = opening; content = (leading_bar, fields, tags); right = closing
        }
    )

and row_field state =
  if is state `BACKQUOTE then
    let tag = name_tag state in
    let of_ =
      if is state `OF then
        let of_ = token state `OF in
        let ampersand = optional state `AMPERSAND in
        let argument state =
          core_type_no_attr
            state
            ( syntax_error
                "variant-tag-argument"
                "'of' or '&' in a variant tag expects a type"
            )
        in
        Some (of_, ampersand, separated state `AMPERSAND argument)
      else None
    in
    let attributes = attributes state in
    Row_tag (tag, of_, attributes, info state)
  else
    Row_inherit
      ( core_type
          state
          ( syntax_error
              "row-field"
              "a polymorphic variant type expects a tag, as in '`A', or a type"
          )
      )

(* {2 Module types} *)

(* A functor argument, checked by [functor_arg_at]. *)
and functor_arg state =
  let opening = state.index in
  let left = token state `LPAREN in
  if is state `RPAREN then
    Functor_unit { left; content = (); right = token state `RPAREN }
  else
    let name = module_name state internal in
    let colon = token state `COLON in
    let typ =
      module_type
        state
        ( syntax_error
            "functor-argument-type"
            "':' in a functor argument expects a module type"
        )
    in
    let right =
      closing
        state ~opening `RPAREN
        (syntax_error "unclosed-functor-argument" "')' expected")
    in
    Functor_named { left; content = (name, colon, typ); right }

and functor_arg_at state offset =
  peek state offset = `LPAREN
  &&
  begin match peek state (offset + 1), peek state (offset + 2) with
  | `RPAREN, _ -> true
  | (`UIDENT _ | `UNDERSCORE), `COLON -> true
  | _ -> false
  end

and functor_args state =
  if functor_arg_at state 0 then
    let argument = functor_arg state in
    argument :: functor_args state
  else []

and module_type state error =
  module_type_gen state ~arrow:true ~with_:true error

(* A module type which may not be followed by [->] (unless [arrow]) or [with]
   (unless [with_]) at its top level, as on the right of
   [with module type X = T]. *)
and module_type_gen state ~arrow ~with_ error =
  let base =
    match next state with
    | `SIG ->
        mk
          ( Mty_signature
              ( delimited
                  state `SIG `END
                  ~unclosed:(syntax_error "unclosed-signature" "this signature expects a declaration, such as 'val x : int' or 'type t', or 'end'")
                  ( fun state ->
                      let attributes = attributes state in
                      attributes, signature state
                  )
              )
          )
    | `FUNCTOR ->
        let functor_ = token state `FUNCTOR in
        let attributes = attributes state in
        let args = functor_args state in
        if args = [] then
          raise
            ( failure
                state
                ( syntax_error
                    "functor-arguments"
                    "'functor' expects arguments, as in 'functor (X : S) -> T'"
                )
            )
        ;
        let arrow = expect state `MINUSGREATER E.functor_type_arrow_module in
        let body = module_type state E.functor_result_type in
        mk (Mty_functor (Some (functor_, attributes), args, arrow, body))
    | `LPAREN when functor_arg_at state 0 ->
        let args = functor_args state in
        let arrow = expect state `MINUSGREATER E.functor_type_arrow_module in
        let body = module_type state E.functor_result_type in
        mk (Mty_functor (None, args, arrow, body))
    | `LPAREN ->
        mk
          ( Mty_paren
              ( paren
                  state
                  ~unclosed:
                    ( syntax_error
                        "unclosed-parenthesized-module-type" "')' expected"
                    )
                  ( fun state ->
                      module_type
                        state
                        ( syntax_error
                            "parenthesized-module-type"
                            "'(' in a module type expects a module type"
                        )
                  )
              )
          )
    | `MODULE ->
        let module_ = token state `MODULE in
        let type_ = expect state `TYPE E.module_type_of in
        let of_ = expect state `OF E.module_type_of in
        let attributes = attributes state in
        let expr =
          module_expr
            state
            ( syntax_error
                "module-type-of-module" "'module type of' expects a module"
            )
        in
        mk (Mty_typeof (module_, type_, of_, attributes, expr))
    | `LBRACKETPERCENT | `QUOTED_STRING_EXPR _ ->
        mk (Mty_extension (extension state))
    | `LIDENT _ | `UIDENT _ -> mk (Mty_ident (mty_longident state internal))
    | _ -> raise (failure state error)
  in
  module_type_postfix state ~arrow ~with_ base

(* [package_type]: a module type checked by [package_type_of_module_type], a
   path with type constraints [S with type t = u]. *)
and package_module_type state error =
  let start = state.index in
  let typ = module_type state error in
  let valid_constraint =
    function
    | With_type (_, No_type_parameters, _, _, None, _, []) -> true
    | _ -> false
  in
  (* Parentheses are not in the Parsetree. *)
  let rec unparenthesized (typ : module_type) =
    match typ.node with
      Mty_paren { content; _ } -> unparenthesized content
    | _ -> typ
  in
  let is_path (typ : module_type) =
    match (unparenthesized typ).node with
      Mty_ident _ -> true
    | _ -> false
  in
  match (unparenthesized typ).node with
  | Mty_ident _ -> typ
  | Mty_with (base, _, Many { first; rest }) when is_path base ->
      if List.for_all valid_constraint (first :: List.map snd rest) then typ
      else
        raise
          ( failure_at
              state start
              ( syntax_error
                  "package-type-constraint"
                  "a first-class module type only accepts 'with type' constraints without type \
                   parameters, 'private' or 'constraint'"
              )
          )
  | _ ->
      raise
        ( failure_at
            state start
            ( syntax_error
                "package-type-path"
                "a first-class module type expects an identifier or a path, as in 'S' or 'M.S', possibly \
                 with 'with type' constraints"
            )
        )

and module_type_postfix state ~arrow ~with_ typ =
  match next state with
  | `WITH when with_ ->
      let with_token = token state `WITH in
      let constraints = separated state `AND with_constraint in
      module_type_postfix
        state ~arrow ~with_ (mk (Mty_with (typ, with_token, constraints)))
  | `MINUSGREATER when arrow ->
      let arrow = token state `MINUSGREATER in
      mk (Mty_arrow (typ, arrow, module_type state E.functor_result_type))
  | `LBRACKETAT ->
      let attribute = attribute_with state `LBRACKETAT in
      module_type_postfix state ~arrow ~with_ (add_attributes typ [ attribute ])
  | _ -> typ

and with_constraint state =
  match next state, peek state 1 with
  | `TYPE, _ ->
      let type_ = token state `TYPE in
      let parameters = type_parameters state in
      let path =
        label_longident
          state
          ( syntax_error
              "with-type-name"
              "'with type' expects a lowercase identifier or a path, as in 't' or 'M.t'"
          )
      in
      if is state `COLONEQUAL then
        let colonequal = token state `COLONEQUAL in
        let typ = core_type_no_attr state E.with_type_definition in
        With_typesubst (type_, parameters, path, colonequal, typ)
      else
        let equal =
          expect
            state `EQUAL (syntax_error "with-type-equal" "'=' or ':=' expected")
        in
        let private_ = optional state `PRIVATE in
        let typ = core_type_no_attr state E.with_type_definition in
        let constraints = type_constraints state in
        With_type (type_, parameters, path, equal, private_, typ, constraints)
  | `MODULE, `TYPE ->
      let module_ = token state `MODULE in
      let type_ = token state `TYPE in
      let path =
        mty_longident
          state
          ( syntax_error
              "with-module-type-name"
              "'with module type' expects an identifier or a path, as in 'S' or 'M.S'"
          )
      in
      let equal =
        equal_or_colonequal
          state (syntax_error "with-module-type-equal" "'=' or ':=' expected")
      in
      (* The precedence of [=] is higher than the one of [->] and [with], the
         one of [:=] is between them. *)
      let typ =
        module_type_gen
          state ~arrow:(equal.Token.kind = `COLONEQUAL) ~with_:false
          ( syntax_error
              "with-module-type-definition"
              "'=' or ':=' in a 'with module type' constraint expects a module type"
          )
      in
      With_modtype (module_, type_, path, equal, typ)
  | `MODULE, _ ->
      let module_ = token state `MODULE in
      let path =
        module_path
          state ~apply:false
          ( syntax_error
              "with-module-name"
              "'with module' expects a capitalized identifier or a path, as in 'M' or 'M.N'"
          )
      in
      let equal =
        equal_or_colonequal
          state (syntax_error "with-module-equal" "'=' or ':=' expected")
      in
      let target =
        module_path
          state ~apply:true
          ( syntax_error
              "with-module-path"
              "'=' or ':=' in a 'with module' constraint expects a capitalized identifier or a path, \
               as in 'M' or 'M.N'"
          )
      in
      With_module (module_, path, equal, target)
  | _ ->
      raise
        ( failure
            state
            ( syntax_error
                "with-constraint"
                "'with' or 'and' expects a constraint, such as 'type t = int' or 'module M = N'"
            )
        )

and equal_or_colonequal state error =
  take
    state error
    ( function
        `EQUAL -> Some `EQUAL
      | `COLONEQUAL -> Some `COLONEQUAL
      | _ -> None
    )

and type_constraints state =
  repeat state (fun kind -> kind = `CONSTRAINT) type_constraint_clause

and type_constraint_clause state : type_constraint_clause =
  let constraint_ = token state `CONSTRAINT in
  let left = core_type state E.constraint_left in
  let equal = expect state `EQUAL E.constraint_equal in
  let right = core_type state E.constraint_right in
  (constraint_, left, equal, right)

(* {2 Signatures} *)

and signature state =
  items
    state ~starts:starts_signature_item
    ( fun state ->
        if starts_signature_item (next state) then
          let claim =
            match next state with
            | `LBRACKETATATAT -> Marked
            | `SEMISEMI -> Undocumented
            | _ -> Documented
          in
          Some (Claim_first (claim, signature_item))
        else None
    )

and signature_item state =
  match next state with
  | (`VAL | `EXTERNAL) as kind -> Sig_value (value_description state kind)
  | `TYPE ->
      begin match type_item state ~rebind:false ~substitution:true with
      | `Typext value -> Sig_typext value
      | `Declarations value ->
          if is_substitution value then Sig_typesubst value else Sig_type value
      end
  | `EXCEPTION -> Sig_exception (exception_declaration state ~rebind:false)
  | `MODULE ->
      begin match peek state 1 with
      | `TYPE ->
          let value : module_type_declaration = module_type_declaration state in
          begin match value with
          | _, _, _, _, Some ({ Token.kind = `COLONEQUAL; _ }, _), _ ->
              Sig_modtypesubst value
          | _ -> Sig_modtype value
          end
      | _ ->
          let module_ = token state `MODULE in
          let ext_attributes = ext_attributes state in
          if is state `REC then begin let rec_ = token state `REC in
            let declaration state = module_declaration state ~recursive:true in
            let declarations = and_list state declaration (declaration state) in
            Sig_recmodule (module_, ext_attributes, rec_, declarations)
          end
          else if is_uident (next state) && peek state 1 = `COLONEQUAL then
            let name = uident state internal in
            let colonequal = token state `COLONEQUAL in
            let path =
              module_path
                state ~apply:true
                ( syntax_error
                    "module-substitution-path"
                    "':=' in a module substitution expects a capitalized identifier or a path, as in \
                     'M' or 'M.N'"
                )
            in
            let post_attributes = post_item_attributes state in
            Sig_modsubst
              (module_, ext_attributes, name, colonequal, path, post_attributes)
          else
            Sig_module
              ( module_, ext_attributes,
                module_declaration state ~recursive:false
              )
      end
  | `OPEN ->
      let open_ = token state `OPEN in
      let bang = optional state `BANG in
      let ext_attributes = ext_attributes state in
      let expr = module_path state ~apply:true E.open_path in
      let post_attributes = post_item_attributes state in
      Sig_open (open_, bang, ext_attributes, expr, post_attributes)
  | `INCLUDE ->
      let include_ = token state `INCLUDE in
      let ext_attributes = ext_attributes state in
      let expr =
        module_type
          state
          (syntax_error "include-module-type" "'include' expects a module type")
      in
      let post_attributes = post_item_attributes state in
      Sig_include (include_, ext_attributes, expr, post_attributes)
  | `CLASS ->
      if peek state 1 = `TYPE then
        let class_, type_, ext_attributes, declarations =
          class_type_declarations state
        in
        Sig_class_type (class_, type_, ext_attributes, declarations)
      else
        let class_, ext_attributes, descriptions =
          class_infos
            state
            ( fun state ->
                let colon =
                  expect
                    state `COLON
                    (syntax_error "class-description-colon" "':' expected")
                in
                colon,
                class_type
                  state
                  ( syntax_error
                      "class-description-type"
                      "':' in a class description expects a class type"
                  )
            )
        in
        Sig_class (class_, ext_attributes, descriptions)
  | `LBRACKETATATAT -> Sig_attribute (floating_attribute state)
  | `LBRACKETPERCENTPERCENT | `QUOTED_STRING_ITEM _ ->
      let extension = item_extension state in
      Sig_extension (extension, post_item_attributes state)
  | _ (* [;;], checked by the caller *) -> Sig_semisemi (token state `SEMISEMI)

(* Whether type declarations are substitutions [type t := u]. The groups are
   checked by [type_item]. *)
and is_substitution ((_, _, _, ((_, _, kind, _, _), _)) : type_declarations) =
  match kind with
  | Kind_defined ({ Token.kind = `COLONEQUAL; _ }, _) -> true
  | _ -> false

(* [val x : t] or [external x : t = "p"], whose keyword, of [kind], the
   caller has matched. *)
and value_description state kind =
  let keyword = shift state kind in
  let ext_attributes = ext_attributes state in
  let name =
    val_ident
      state
      ( syntax_error
          "value-name"
          "'val' or 'external' expects a lowercase identifier or an operator in parentheses, as in \
           'x' or '( + )'"
      )
  in
  let colon = expect state `COLON (syntax_error "value-colon" "':' expected") in
  let typ =
    possibly_poly
      state
      (syntax_error "value-type" "':' in a value declaration expects a type")
      core_type
  in
  let primitives =
    match keyword.kind with
    | `EXTERNAL ->
        let equal =
          expect state `EQUAL (syntax_error "external-equal" "'=' expected")
        in
        let primitives =
          repeat_some state (fun state -> take_opt state select_string)
        in
        if primitives = [] then
          raise
            ( failure
                state
                ( syntax_error
                    "external-primitive"
                    "'=' in an external declaration expects a string literal"
                )
            )
        ;
        Some (equal, primitives)
    | `VAL -> None
  in
  let post_attributes = post_item_attributes state in
  (keyword, ext_attributes, name, colon, typ, primitives, post_attributes)

and module_declaration state ~recursive : module_declaration =
  let name =
    module_name
      state (if recursive then E.recursive_module_name else E.module_name)
  in
  let body =
    if recursive then
      let colon =
        expect
          state `COLON (syntax_error "recursive-module-colon" "':' expected")
      in
      Mdecl_type (colon, module_type state E.module_declaration_type)
    else module_declaration_body state ~alias:true
  in
  (name, body, post_item_attributes state)

(* After the name of a module or its arguments: [alias] for a module which is
   not a functor. *)
and module_declaration_body state ~alias =
  match next state with
  | `COLON ->
      let colon = token state `COLON in
      Mdecl_type (colon, module_type state E.module_declaration_type)
  | `EQUAL when alias ->
      let equal = token state `EQUAL in
      Mdecl_alias
        ( equal,
          module_path
            state ~apply:false (syntax_error "module-alias-path" "'=' in a module alias expects a capitalized identifier or a path, as in 'M' or 'M.N'")
        )
  | `LPAREN ->
      if not (functor_arg_at state 0) then
        raise (failure state E.module_declaration_functor_argument)
      ;
      let argument = functor_arg state in
      Mdecl_functor (argument, module_declaration_body state ~alias:false)
  | _ ->
      raise
        ( failure
            state
            ( if alias then
                (syntax_error "module-declaration-body" "':' or '=' expected")
              else (syntax_error "functor-declaration-body" "':' expected")
            )
        )

(* [module type S = T], or [module type S := T] in signatures
   ([substitution]). *)
and module_type_declaration ?(substitution = true) state
  : module_type_declaration
  =
    let module_ = token state `MODULE in
    let type_ = token state `TYPE in
    let ext_attributes = ext_attributes state in
    let name =
      ident
        state
        (syntax_error "module-type-name" "'module type' expects an identifier")
    in
    if is state `COLONEQUAL && not substitution then
      raise
        ( failure
            state
            ( syntax_error
                "module-type-substitution-in-structure"
                "module type substitutions 'module type S := ...' are only allowed in signatures: '=' is \
               expected"
            )
        )
    ;
    let body =
      match next state with
      | (`EQUAL | `COLONEQUAL) as kind ->
          let equal = shift state kind in
          Some
            ( equal,
              module_type
                state
                ( syntax_error
                    "module-type-definition"
                    "'=' or ':=' in a module type declaration expects a module type"
                )
            )
      | _ -> None
    in
    (module_, type_, ext_attributes, name, body, post_item_attributes state)

(* Type declarations or a type extension. Substitutions [type t := u] are
   allowed in signatures ([substitution]): all the declarations of a group
   are substitutions ([type_subst_declarations]), without [nonrec]
   ([no_nonrec_flag]), or none are. *)
and type_item state ~rebind ~substitution =
  let type_ = token state `TYPE in
  let ext_attributes = ext_attributes state in
  let nonrec_index = state.index in
  let nonrec_ = optional state `NONREC in
  let parameters = type_parameters state in
  if
    is_uident (next state)
    &&
    not
      ( match peek state 1 with
          `DOT | `LPAREN -> true
        | _ -> false
      )
  then
    raise
      ( failure
          state
          ( syntax_error
              "capitalized-type-name"
              "the name of a type expects a lowercase identifier"
          )
      )
  ;
  let is_extension =
    is_uident (next state) || (is_lident (next state) && peek state 1 = `PLUSEQ)
  in
  if is_extension then begin if nonrec_ <> None then
      raise
        ( failure_at
            state nonrec_index
            ( syntax_error
                "type-extension-nonrec"
                "a type extension 'type t += ...' cannot be 'nonrec'"
            )
        )
    ;
    let path =
      type_longident
        state
        ( syntax_error
            "type-extension-name"
            "a type extension expects a lowercase identifier or a path, as in 't' or 'M.t'"
        )
    in
    let pluseq =
      expect
        state `PLUSEQ (syntax_error "type-extension-pluseq" "'+=' expected")
    in
    let private_ = optional state `PRIVATE in
    let constructors = extension_constructors state ~rebind in
    let post_attributes = post_item_attributes state in
    `Typext
      ( type_, ext_attributes, parameters, path,
        pluseq, private_, constructors, post_attributes
      )
  end
  else begin
    (* The index of the [:=] of the first declaration, if any. *)
    let group = ref None in
    let declaration state parameters =
      let name =
        lident
          state
          ( syntax_error
              "type-name" "'type' or 'and' expects a lowercase identifier"
          )
      in
      let kind_index = state.index in
      let is_substitution = is state `COLONEQUAL in
      begin match !group with
      | None -> group := Some is_substitution
      | Some true when not is_substitution && not (is state `EQUAL) ->
          raise
            ( failure_at
                state (kind_index - 1)
                ( syntax_error
                    "type-substitution-abstract"
                    "a type of a group of substitutions expects ':=' and a type"
                )
            )
      | Some group ->
          if group <> is_substitution then
            raise
              ( failure_at
                  state kind_index
                  ( syntax_error
                      "type-substitution-mixed"
                      "the types of a group are either all substitutions with ':=', or all definitions with \
                         '='"
                  )
              )
      end;
      if is_substitution && not substitution then
        raise
          ( failure_at
              state kind_index
              (syntax_error "type-substitution-in-structure" "type substitutions 'type t := ...' are only allowed in signatures: '=' is expected")
          )
      ;
      if is_substitution && nonrec_ <> None then
        raise
          ( failure_at
              state nonrec_index
              ( syntax_error
                  "type-substitution-nonrec"
                  "a type substitution 'type t := ...' cannot be 'nonrec'"
              )
          )
      ;
      type_declaration state parameters name
    in
    let declarations =
      and_list
        state (fun state -> declaration state (type_parameters state))
        (declaration state parameters)
    in
    `Declarations (type_, ext_attributes, nonrec_, declarations)
  end

and type_declaration state parameters name =
  let kind =
    match next state with
    | (`EQUAL | `COLONEQUAL) as kind ->
        let equal = shift state kind in
        Kind_defined (equal, type_definition state)
    | _ -> Kind_abstract
  in
  let constraints = type_constraints state in
  (parameters, name, kind, constraints, post_item_attributes state)

(* Does a constructor declaration (or an empty variant) start here? *)
and starts_constructors state =
  match next state, peek state 1 with
  | `BAR, _ -> true
  | `UIDENT _, (`DOT | `LPAREN) -> false
  | `UIDENT _, _ | (`TRUE | `FALSE), _ -> true
  | `LBRACKET, `RBRACKET | `LPAREN, `RPAREN -> true
  | `LPAREN, `COLONCOLON -> true
  | _ -> false

and type_definition state =
  let kind_after synonym =
    let private_ = optional state `PRIVATE in
    match next state with
    | `DOTDOT -> Def_open (synonym, private_, token state `DOTDOT)
    | `LBRACE ->
        Def_record
          ( synonym, private_,
            delimited
              state `LBRACE `RBRACE
              ~unclosed:E.unclosed_record_type label_declarations
          )
    | _ when starts_constructors state ->
        Def_variant (synonym, private_, constructor_declarations state)
    | _ ->
        if synonym <> None then
          raise
            ( failure
                state
                ( syntax_error
                    "type-kind-after-manifest"
                    "a re-exported type 'type t = M.t = ...' expects constructors, a record or '..'"
                )
            )
        ;
        Def_manifest
          ( private_,
            core_type
              state
              ( syntax_error
                  "private-type"
                  "'private' expects a type, constructors or a record"
              )
          )
  in
  match next state with
  | `EXTERNAL ->
      let external_ = token state `EXTERNAL in
      Def_external
        ( external_,
          string_literal
            state
            ( syntax_error
                "external-type"
                "'external' in a type definition expects a string literal"
            )
        )
  | `PRIVATE | `DOTDOT | `LBRACE -> kind_after None
  | _ when starts_constructors state -> kind_after None
  | _ ->
      let typ =
        core_type
          state
          ( syntax_error
              "type-definition"
              "'=' in a type definition expects a type, constructors or a record"
          )
      in
      if is state `EQUAL then
        let equal = token state `EQUAL in
        kind_after (Some (typ, equal))
      else Def_manifest (None, typ)

and constructor_declarations state =
  if is state `BAR && not (is_constr_ident_at state 1) then
    Constructors_empty (token state `BAR)
  else
    Constructors
      ( bar_list
          state
          ( fun state ->
              let constructor = constructor_declaration state in
              constructor, info state
          )
      )

and constructor_declaration state : constructor_declaration =
  let name = constr_ident state E.constructor_name in
  let arguments = constructor_signature state in
  (name, arguments, attributes state)

and constructor_signature state =
  match next state with
  | `OF ->
      let of_ = token state `OF in
      Args_of (of_, constructor_arguments state)
  | `COLON ->
      let colon = token state `COLON in
      let variables =
        if typevars_then_dot state 0 then
          let variables = type_variables state in
          Some (variables, token state `DOT)
        else None
      in
      if is state `LBRACE then begin let arguments = constructor_arguments state
        in
        let arrow = expect state `MINUSGREATER E.gadt_arrow in
        Args_gadt
          ( colon, variables, Some (arguments, arrow),
            atomic_type state E.gadt_result
          )
      end
      else begin let first =
          atomic_type
            state
            ( syntax_error
                "gadt-type" "':' after a constructor expects its type"
            )
        in
        if is state `STAR || is state `MINUSGREATER then begin
          let element state = atomic_type state E.constructor_tuple_element in
          let arguments =
            Constructor_tuple
              (Many (non_empty_separated state `STAR element first))
          in
          let arrow = expect state `MINUSGREATER E.gadt_arrow in
          Args_gadt
            ( colon, variables, Some (arguments, arrow),
              atomic_type state E.gadt_result
            )
        end
        else Args_gadt (colon, variables, None, first)
      end
  | _ -> Args_none

and constructor_arguments state =
  if is state `LBRACE then
    Constructor_record
      ( delimited
          state `LBRACE `RBRACE
          ~unclosed:E.unclosed_record_type label_declarations
      )
  else
    let first =
      atomic_type
        state
        ( syntax_error
            "constructor-argument-type"
            "'of' expects the types of the arguments, or a record"
        )
    in
    let element state = atomic_type state E.constructor_tuple_element in
    Constructor_tuple (Many (non_empty_separated state `STAR element first))

(* The end of a record or object field: its doc comment, before the [;] or
   after it, and the [;] with its attributes ([rhs_info] then [symbol_info]
   in the grammar). *)
and field_end state =
  let doc = info state in
  if is state `SEMI then
    let semi = token state `SEMI in
    let attributes = attributes_after_semi state in
    doc, Some (semi, attributes, if doc = None then info state else None)
  else doc, None

and label_declarations state =
  let mutable_ = optional state `MUTABLE in
  let name =
    lident
      state
      (syntax_error "field-name" "a record type expects a lowercase identifier")
  in
  let colon = expect state `COLON (syntax_error "field-colon" "':' expected") in
  let typ =
    possibly_poly
      state
      ( syntax_error
          "field-type" "':' in a record type expects the type of the field"
      )
      core_type_no_attr
  in
  let attributes = attributes state in
  let doc, semi = field_end state in
  let label = (mutable_, name, colon, typ, attributes, doc, semi) in
  if semi <> None && not (is state `RBRACE) then
    label :: label_declarations state
  else [ label ]

and extension_constructors state ~rebind =
  bar_list
    state
    ( fun state ->
        let constructor = extension_constructor state ~rebind E.constructor_name
        in
        constructor, info state
    )

and extension_constructor state ~rebind error =
  let name = constr_ident state error in
  if rebind && is state `EQUAL then
    let equal = token state `EQUAL in
    let path =
      constr_longident state (syntax_error "extension-constructor-rebind" "'=' after a constructor expects a capitalized identifier or a path, as in 'C' or 'M.C'")
    in
    Ext_rebind (name, equal, path, attributes state)
  else
    let arguments = constructor_signature state in
    Ext_decl (name, arguments, attributes state)

and exception_declaration state ~rebind =
  let exception_ = token state `EXCEPTION in
  let ext_attributes = ext_attributes state in
  let constructor =
    extension_constructor
      state ~rebind
      ( syntax_error
          "exception-name" "'exception' expects a capitalized identifier"
      )
  in
  (exception_, ext_attributes, constructor, post_item_attributes state)

(* A constructor name, possibly qualified, or [error]. *)
and constr_longident state error =
  if is_uident (next state) then begin
    let path = module_path state ~apply:false internal in
    if
      is state `DOT && peek state 1 = `LPAREN && peek state 2 = `COLONCOLON
    then
      let dot = token state `DOT in
      (Some (path, dot), constr_ident state internal)
    else
      let qualifier, name = split_path state path in
      (qualifier, Constr_uident name)
  end
  else (None, constr_ident state error)

(* {2 Classes} *)

(* [class c = ... and d = ...], after [class] and its extension. *)
and class_info_list
  : 'body. state -> (state -> 'body) -> 'body class_info and_list
  =
  fun state body ->
    let info state = class_info state body in
    and_list state info (info state)

and class_infos
  : 'body.
      state -> (state -> 'body)
      -> [ `CLASS ] tok * ext_attributes * 'body class_info and_list
  =
  fun state body ->
    let class_ = token state `CLASS in
    let ext_attributes = ext_attributes state in
    class_, ext_attributes, class_info_list state body

and class_info : 'body. state -> (state -> 'body) -> 'body class_info =
  fun state body ->
    let virtual_ = optional state `VIRTUAL in
    let parameters = class_parameters state in
    let name =
      lident
        state
        ( syntax_error
            "class-name" "'class' or 'and' expects a lowercase identifier"
        )
    in
    let body = body state in
    (virtual_, parameters, name, body, post_item_attributes state)

and class_type_declarations state =
  let class_ = token state `CLASS in
  let type_ = token state `TYPE in
  let ext_attributes = ext_attributes state in
  let declarations =
    class_info_list
      state
      ( fun state ->
          let equal =
            expect state `EQUAL (syntax_error "class-type-equal" "'=' expected")
          in
          equal,
          class_signature
            state
            ( syntax_error
                "class-type-definition"
                "'=' in a class type declaration expects a class type"
            )
      )
  in
  class_, type_, ext_attributes, declarations

and arrow_label_start state =
  match next state with
  | `OPTLABEL _ -> true
  | `QUESTION -> is_lident (peek state 1) && peek state 2 = `COLON
  | `LIDENT _ -> peek state 1 = `COLON
  | _ -> false

(* A label, checked by [arrow_label_start]. *)
and arrow_label state =
  match next state with
  | `OPTLABEL _ as kind -> Optional (shift state kind)
  | `QUESTION ->
      let question = token state `QUESTION in
      let name = lident state internal in
      Optional_split (question, name, token state `COLON)
  | _ (* [x:] *) ->
      let name = lident state internal in
      Labelled (name, token state `COLON)

and class_type state error =
  let arrow label domain =
    let arrow = expect state `MINUSGREATER E.class_arrow in
    let codomain =
      class_type
        state
        ( syntax_error
            "class-arrow-result" "'->' in a class type expects a class type"
        )
    in
    Cty_arrow (label, domain, arrow, codomain)
  in
  if arrow_label_start state then
    let label = arrow_label state in
    arrow
      label
      ( tuple_type
          state
          ( syntax_error
              "class-arrow-domain"
              "a label in a class type expects the type of the argument"
          )
      )
  else
    match next state with
    | `OBJECT | `LET -> Cty_signature (class_signature state error)
    | `LBRACKET when
        not (List.mem (peek state 1) [ `BACKQUOTE; `BAR; `RBRACKET ])
    ->
        (* [[t, u] c], or a polymorphic variant type [[t | `A]] as the
           domain of an arrow: the types after [[] tell them apart. *)
        let opening = state.index in
        let left = token state `LBRACKET in
        let first = class_argument state in
        if is state `BAR then
          let fields =
            Many (non_empty_separated state `BAR row_field (Row_inherit first))
          in
          let variant =
            variant_type_rest
              state ~opening_index:opening (left :> _ tok) None fields
          in
          arrow Nolabel (tuple_type_from state (type_postfix state variant))
        else
          let content =
            Many (non_empty_separated state `COMMA class_argument first)
          in
          let right =
            closing state ~opening `RBRACKET E.unclosed_class_type_arguments
          in
          Cty_signature
            (class_signature_rest state (Some { left; content; right }) error)
    | _ ->
        let domain = tuple_type state error in
        if is state `MINUSGREATER then arrow Nolabel domain
        else
          (* Not a domain: a class path or an extension. *)
          let base =
            match domain with
            | { node = Typ_constr (No_type_arguments, path); attributes = [] }
            -> mk (Csig_constr (None, path))
            | { node = Typ_extension extension; attributes = [] } ->
                mk (Csig_extension extension)
            | _ -> raise (failure state E.class_arrow)
          in
          Cty_signature (class_signature_attributes state base)

and class_signature state error =
  let base =
    match next state with
    | `OBJECT ->
        let opening = state.index in
        let object_ = token state `OBJECT in
        let attributes = attributes state in
        let self =
          if is state `LPAREN then
            Some
              ( paren
                  state
                  ~unclosed:
                    (syntax_error "unclosed-class-self-type" "')' expected")
                  ( fun state ->
                      core_type
                        state
                        ( syntax_error
                            "class-self-type"
                            "'(' after 'object' expects the type of self"
                        )
                  )
              )
          else None
        in
        let fields =
          items
            state
            ( fun state ->
                match next state with
                | `LBRACKETATATAT ->
                    Some (Claim_first (Marked, class_sig_field))
                | `INHERIT | `VAL | `METHOD | `CONSTRAINT
                  | `LBRACKETPERCENTPERCENT | `QUOTED_STRING_ITEM _
                -> Some (Claim_first (Documented, class_sig_field))
                | _ -> None
            )
        in
        let end_ =
          closing
            state ~opening `END
            ( syntax_error
                "unclosed-class-signature"
                "this object type expects 'val', 'method', 'inherit', 'constraint' or 'end'"
            )
        in
        mk
          ( Csig_object
              { left = object_;
                content = (attributes, self, fields); right = end_
              }
          )
    | `LET ->
        let let_ = token state `LET in
        let open_ =
          expect
            state `OPEN (syntax_error "class-type-let-open" "'open' expected")
        in
        let bang = optional state `BANG in
        let attributes = attributes state in
        let path = module_path state ~apply:false E.open_path in
        let in_ =
          expect
            state `IN (syntax_error "class-type-let-open-in" "'in' expected")
        in
        let body =
          class_signature
            state
            ( syntax_error
                "class-type-let-open-body"
                "'in' in a class type expects a class type"
            )
        in
        mk (Csig_open (let_, open_, bang, attributes, path, in_, body))
    | `LBRACKETPERCENT | `QUOTED_STRING_EXPR _ ->
        mk (Csig_extension (extension state))
    | _ -> class_signature_constr state (class_arguments state) error
  in
  class_signature_attributes state base

(* A class signature after its arguments: [[t] c]. *)
and class_signature_rest state arguments error =
  class_signature_attributes
    state (class_signature_constr state arguments error)

and class_signature_constr state arguments error =
  let error =
    if arguments = None then error
    else
      ( syntax_error
          "class-type-arguments-name"
          "class type arguments in brackets expect a lowercase identifier or a path, as in \
           '[int] c'"
      )
  in
  mk (Csig_constr (arguments, type_longident state error))

(* The attributes after a class signature. *)
and class_signature_attributes state base =
  if is state `LBRACKETAT then
    let attributes = attributes state in
    add_attributes base attributes
  else base

and class_arguments state : class_arguments =
  if is state `LBRACKET then
    Some
      ( delimited
          state `LBRACKET `RBRACKET ~unclosed:E.unclosed_class_type_arguments
          (fun state -> separated state `COMMA class_argument)
      )
  else None

and class_argument state =
  core_type
    state
    ( syntax_error
        "class-type-argument" "'[' or ',' in class arguments expects a type"
    )

and class_sig_field state =
  match next state with
  | `INHERIT ->
      let inherit_ = token state `INHERIT in
      let attributes = attributes state in
      let typ =
        class_signature
          state
          ( syntax_error
              "inherit-class-type"
              "'inherit' in an object type expects a class type"
          )
      in
      let post_attributes = post_item_attributes state in
      Ctf_inherit (inherit_, attributes, typ, ~post_attributes)
  | `VAL ->
      let val_ = token state `VAL in
      let attributes = attributes state in
      let flags =
        flags
          state
          ( function
              `MUTABLE -> Some `MUTABLE
            | `VIRTUAL -> Some `VIRTUAL
            | _ -> None
          )
      in
      let name = lident state E.instance_variable_name in
      let colon =
        expect
          state `COLON (syntax_error "instance-variable-colon" "':' expected")
      in
      let typ = core_type state E.instance_variable_type in
      let post_attributes = post_item_attributes state in
      Ctf_val (val_, attributes, flags, name, colon, typ, ~post_attributes)
  | `METHOD ->
      let method_ = token state `METHOD in
      let attributes = attributes state in
      let flags =
        flags
          state
          ( function
              `PRIVATE -> Some `PRIVATE
            | `VIRTUAL -> Some `VIRTUAL
            | _ -> None
          )
      in
      let name = lident state E.method_name in
      let colon =
        expect state `COLON (syntax_error "method-colon" "':' expected")
      in
      let typ = possibly_poly state E.method_type core_type in
      let post_attributes = post_item_attributes state in
      Ctf_method
        (method_, attributes, flags, name, colon, typ, ~post_attributes)
  | `CONSTRAINT ->
      let constraint_ = token state `CONSTRAINT in
      let attributes = attributes state in
      let left = core_type state E.constraint_left in
      let equal = expect state `EQUAL E.constraint_equal in
      let right = core_type state E.constraint_right in
      let post_attributes = post_item_attributes state in
      Ctf_constraint
        (constraint_, attributes, left, equal, right, ~post_attributes)
  | `LBRACKETATATAT -> Ctf_attribute (floating_attribute state)
  | _ ->
      let extension = item_extension state in
      Ctf_extension (extension, post_item_attributes state)

and class_expr state error =
  let base =
    match next state with
    | `FUN ->
        let fun_ = token state `FUN in
        let attributes = attributes state in
        if not (param_at state) then raise (failure state E.class_fun_parameter)
        ;
        let params =
          repeat_while
            state param_at
            (fun state -> param_pattern state E.class_fun_parameter)
        in
        let arrow =
          expect
            state `MINUSGREATER (syntax_error "class-fun-arrow" "'->' expected")
        in
        let body =
          class_expr
            state
            ( syntax_error
                "class-fun-body"
                "'->' in a class function expects a class expression"
            )
        in
        mk (Cl_fun (fun_, attributes, params, arrow, body))
    | `LET when peek state 1 = `OPEN ->
        let let_ = token state `LET in
        let open_ = token state `OPEN in
        let bang = optional state `BANG in
        let attributes = attributes state in
        let path = module_path state ~apply:false E.open_path in
        let in_ =
          expect state `IN (syntax_error "class-let-open-in" "'in' expected")
        in
        let body = class_expr state E.class_let_body in
        mk (Cl_open (let_, open_, bang, attributes, path, in_, body))
    | `LET when peek state 1 = `PERCENT ->
        raise (failure_at state (state.index + 1) E.class_let_extension)
    | `LET ->
        (* [let_bindings(no_ext)]: [let%ext] is rejected above. *)
        let bindings = let_bindings state ~docs:Ignore_docs in
        let in_ = expect state `IN (syntax_error "class-let-in" "'in' expected")
        in
        mk (Cl_let (bindings, in_, class_expr state E.class_let_body))
    | `LBRACKETPERCENT | `QUOTED_STRING_EXPR _ ->
        mk (Cl_extension (extension state))
    | _ ->
        let head = class_simple_expr state error in
        if starts_argument (next state) then
          mk (Cl_apply (head, repeat state starts_argument argument))
        else head
  in
  if is state `LBRACKETAT then
    let attributes = attributes state in
    add_attributes base attributes
  else base

and class_simple_expr state error =
  match next state with
  | `LPAREN ->
      let opening = state.index in
      let left = token state `LPAREN in
      let expr =
        class_expr
          state
          ( syntax_error
              "parenthesized-class-expr"
              "'(' in a class expression expects a class expression"
          )
      in
      if is state `COLON then
        let colon = token state `COLON in
        let typ =
          class_type
            state
            ( syntax_error
                "class-constraint-type"
                "':' in a class expression expects a class type"
            )
        in
        let right =
          closing
            state ~opening `RPAREN
            (syntax_error "unclosed-class-constraint" "')' expected")
        in
        mk (Cl_constraint { left; content = (expr, colon, typ); right })
      else
        let right =
          closing
            state ~opening `RPAREN
            (syntax_error "unclosed-parenthesized-class-expr" "')' expected")
        in
        mk (Cl_paren { left; content = expr; right })
  | `OBJECT ->
      mk
        ( Cl_structure
            ( delimited
                state `OBJECT `END ~unclosed:E.unclosed_object
                ( fun state ->
                    let attributes = attributes state in
                    attributes, class_structure state
                )
            )
        )
  | _ ->
      let arguments = class_arguments state in
      let error =
        if arguments = None then error
        else
          ( syntax_error
              "class-arguments-name"
              "class type arguments in brackets expect a lowercase identifier or a path, as in '[int] \
               c'"
          )
      in
      mk (Cl_constr (arguments, class_longident state error))

and class_structure state =
  let self =
    if is state `LPAREN then
      let opening = state.index in
      let left = token state `LPAREN in
      let pattern =
        pattern
          state
          ( syntax_error
              "self-pattern" "'(' after 'object' expects a pattern for self"
          )
      in
      if is state `COLON then
        let colon = token state `COLON in
        let typ =
          core_type
            state
            ( syntax_error
                "self-type" "':' in the pattern of self expects a type"
            )
        in
        let right = closing state ~opening `RPAREN E.unclosed_self in
        Some (Self_constraint { left; content = (pattern, colon, typ); right })
      else
        let right = closing state ~opening `RPAREN E.unclosed_self in
        Some (Self_pattern { left; content = pattern; right })
    else None
  in
  let fields =
    items
      state
      ( fun state ->
          match next state with
          | `LBRACKETATATAT -> Some (Claim_first (Marked, class_field))
          | `INHERIT | `VAL | `METHOD | `CONSTRAINT | `INITIALIZER
            | `LBRACKETPERCENTPERCENT | `QUOTED_STRING_ITEM _
          -> Some (Claim_first (Documented, class_field))
          | _ -> None
      )
  in
  (self, fields)

(* [method!] and [val!] override a definition: they cannot be virtual. *)
and no_virtual_override state bang =
  let rec check offset =
    match peek state offset with
    | `VIRTUAL ->
        raise
          ( failure_at
              state (state.index + offset)
              ( syntax_error
                  "virtual-override"
                  "a definition overridden with '!' cannot be virtual"
              )
          )
    | `MUTABLE | `PRIVATE -> check (offset + 1)
    | _ -> ()
  in
  if bang <> None then check 0

and class_field state =
  match next state with
  | `INHERIT ->
      let inherit_ = token state `INHERIT in
      let bang = optional state `BANG in
      let attributes = attributes state in
      let expr =
        class_expr
          state
          ( syntax_error
              "inherit-class-expr" "'inherit' expects a class expression"
          )
      in
      let alias =
        preceded_option
          state `AS
          ( fun state ->
              lident
                state
                ( syntax_error
                    "inherit-alias"
                    "'as' after 'inherit' expects a lowercase identifier"
                )
          )
      in
      let post_attributes = post_item_attributes state in
      Cf_inherit (inherit_, bang, attributes, expr, alias, ~post_attributes)
  | `VAL ->
      let val_ = token state `VAL in
      let bang = optional state `BANG in
      let attributes = attributes state in
      no_virtual_override state bang;
      let flags =
        flags
          state
          ( function
              `MUTABLE -> Some `MUTABLE
            | `VIRTUAL -> Some `VIRTUAL
            | _ -> None
          )
      in
      let name = lident state E.instance_variable_name in
      let body =
        if List.exists (fun flag -> flag.Token.kind = `VIRTUAL) flags then
          let colon =
            expect
              state `COLON
              ( syntax_error
                  "virtual-instance-variable-colon"
                  "':' expected, a virtual instance variable has no value"
              )
          in
          Value_virtual (colon, core_type state E.instance_variable_type)
        else
          let constraint_ = optional_type_constraint state in
          let equal =
            expect
              state `EQUAL
              (syntax_error "instance-variable-equal" "'=' expected")
          in
          Value_concrete
            ( constraint_, equal,
              seq_expr
                state
                ( syntax_error
                    "instance-variable-value"
                    "'=' after an instance variable expects an expression"
                )
            )
      in
      let post_attributes = post_item_attributes state in
      Cf_val (val_, bang, attributes, flags, name, body, ~post_attributes)
  | `METHOD ->
      let method_ = token state `METHOD in
      let bang = optional state `BANG in
      let attributes = attributes state in
      no_virtual_override state bang;
      let flags =
        flags
          state
          ( function
              `PRIVATE -> Some `PRIVATE
            | `VIRTUAL -> Some `VIRTUAL
            | _ -> None
          )
      in
      let name = lident state E.method_name in
      let virtual_ = List.exists (fun flag -> flag.Token.kind = `VIRTUAL) flags
      in
      let body =
        match next state with
        | `COLON when virtual_ ->
            let colon = token state `COLON in
            Method_virtual (colon, possibly_poly state E.method_type core_type)
        | _ when virtual_ ->
            raise
              ( failure
                  state
                  ( syntax_error
                      "virtual-method-colon"
                      "':' expected, a virtual method has no body"
                  )
              )
        | `COLON when peek state 1 = `TYPE ->
            let colon = token state `COLON in
            let type_ = token state `TYPE in
            let names = lidents state in
            if names = [] then raise (failure state E.annotation_newtype_names);
            let dot =
              expect
                state `DOT (syntax_error "method-newtype-dot" "'.' expected")
            in
            let typ = core_type state E.method_type in
            let equal = expect state `EQUAL E.method_equal in
            Method_newtypes
              ( colon, (type_, names), dot, typ,
                equal, seq_expr state E.method_body
              )
        | `COLON ->
            let colon = token state `COLON in
            let typ = possibly_poly state E.method_type core_type in
            let equal = expect state `EQUAL E.method_equal in
            Method_poly (colon, typ, equal, seq_expr state E.method_body)
        | `EQUAL ->
            (* [EQUAL seq_expr]: a [function] here is an expression. *)
            let equal = token state `EQUAL in
            Method_strict ([], None, equal, seq_expr state E.method_body)
        | `COLONGREATER ->
            raise
              ( failure
                  state
                  ( syntax_error
                      "method-coercion"
                      "a method without parameters cannot have a coercion ':>'"
                  )
              )
        | _ ->
            let params = function_params state in
            let constraint_ = optional_type_constraint state in
            let equal =
              expect
                state `EQUAL
                (syntax_error "method-parameters-equal" "'=' expected")
            in
            Method_strict
              (params, constraint_, equal, fun_body state E.method_body)
      in
      let post_attributes = post_item_attributes state in
      Cf_method (method_, bang, attributes, flags, name, body, ~post_attributes)
  | `CONSTRAINT ->
      let constraint_ = token state `CONSTRAINT in
      let attributes = attributes state in
      let left = core_type state E.constraint_left in
      let equal = expect state `EQUAL E.constraint_equal in
      let right = core_type state E.constraint_right in
      let post_attributes = post_item_attributes state in
      Cf_constraint
        (constraint_, attributes, left, equal, right, ~post_attributes)
  | `INITIALIZER ->
      let initializer_ = token state `INITIALIZER in
      let attributes = attributes state in
      let expr =
        seq_expr
          state
          ( syntax_error
              "initializer-body" "'initializer' expects an expression"
          )
      in
      let post_attributes = post_item_attributes state in
      Cf_initializer (initializer_, attributes, expr, ~post_attributes)
  | `LBRACKETATATAT -> Cf_attribute (floating_attribute state)
  | _ ->
      let extension = item_extension state in
      Cf_extension (extension, post_item_attributes state)

and class_binding state =
  match next state with
  | `EQUAL ->
      let equal = token state `EQUAL in
      Class_equal (equal, class_expr state E.class_definition)
  | `COLON ->
      let colon = token state `COLON in
      let typ =
        class_type
          state
          ( syntax_error
              "class-binding-type"
              "':' in a class definition expects a class type"
          )
      in
      let equal =
        expect state `EQUAL (syntax_error "class-binding-equal" "'=' expected")
      in
      Class_constraint (colon, typ, equal, class_expr state E.class_definition)
  | _ ->
      let param =
        param_pattern
          state
          (syntax_error "class-parameter" "a parameter, ':' or '=' expected")
      in
      Class_param (param, class_binding state)

(* {2 Module expressions} *)

and module_expr state error =
  let base =
    match next state with
    | `STRUCT ->
        mk
          ( Mod_structure
              ( delimited
                  state `STRUCT `END
                  ~unclosed:
                    ( syntax_error
                        "unclosed-structure"
                        "this structure expects a definition, such as 'let x = 1' or 'type t = int', or \
                         'end'"
                    )
                  ( fun state ->
                      let attributes = attributes state in
                      attributes, structure state
                  )
              )
          )
    | `FUNCTOR ->
        let functor_ = token state `FUNCTOR in
        let attributes = attributes state in
        let args = functor_args state in
        if args = [] then
          raise
            ( failure
                state
                ( syntax_error
                    "functor-expr-arguments"
                    "'functor' expects arguments, as in 'functor (X : S) -> M'"
                )
            )
        ;
        let arrow =
          expect
            state `MINUSGREATER
            (syntax_error "functor-expr-arrow" "'->' expected")
        in
        let body =
          module_expr
            state
            (syntax_error "functor-body" "'->' in a functor expects a module")
        in
        mk (Mod_functor (functor_, attributes, args, arrow, body))
    | `LPAREN -> paren_module_expr state
    | `LBRACKETPERCENT | `QUOTED_STRING_EXPR _ ->
        mk (Mod_extension (extension state))
    | _ -> mk (Mod_ident (module_path state ~apply:false error))
  in
  module_expr_postfix state base

and module_expr_postfix state expr =
  match next state with
  | `LPAREN when peek state 1 = `RPAREN ->
      module_expr_postfix
        state
        (mk (Mod_apply_unit (expr, paren state ~unclosed:internal ignore)))
  | `LPAREN ->
      let argument = paren_module_expr state in
      module_expr_postfix state (mk (Mod_apply (expr, argument)))
  | `LBRACKETAT ->
      let attribute = attribute_with state `LBRACKETAT in
      module_expr_postfix state (add_attributes expr [ attribute ])
  | _ -> expr

and paren_module_expr state =
  let opening = state.index in
  let left = token state `LPAREN in
  if is state `VAL then begin let val_ = token state `VAL in
    let attributes = attributes state in
    let expr =
      expr
        state
        (syntax_error "unpack-expr" "'val' in a module expects an expression")
    in
    let package =
      match next state with
      | `COLON ->
          let colon = token state `COLON in
          let typ = package_module_type state E.unpack_type in
          if is state `COLONGREATER then
            let coerce = token state `COLONGREATER in
            Package_coerce
              ( Some (colon, typ), coerce,
                package_module_type state E.unpack_type
              )
          else Package_constraint (colon, typ)
      | `COLONGREATER ->
          let coerce = token state `COLONGREATER in
          Package_coerce (None, coerce, package_module_type state E.unpack_type)
      | _ -> No_package
    in
    let rparen =
      closing
        state ~opening `RPAREN (syntax_error "unclosed-unpack" "')' expected")
    in
    mk
      ( Mod_unpack
          { left; content = (val_, attributes, expr, package); right = rparen }
      )
  end
  else begin let expr =
      module_expr
        state
        ( syntax_error
            "parenthesized-module-expr" "'(' in a module expects a module"
        )
    in
    if is state `COLON then
      let colon = token state `COLON in
      let typ =
        module_type
          state
          ( syntax_error
              "module-constraint-type" "':' in a module expects a module type"
          )
      in
      let right =
        closing state ~opening `RPAREN E.unclosed_parenthesized_module_expr
      in
      mk (Mod_constraint { left; content = (expr, colon, typ); right })
    else
      let right =
        closing state ~opening `RPAREN E.unclosed_parenthesized_module_expr
      in
      mk (Mod_paren { left; content = expr; right })
  end

(* {2 Structures} *)

and structure state =
  (* An expression may start the structure, or follow [;;]. *)
  let expression = ref true in
  let item claim parse =
    Some
      ( Claim_first
          ( claim,
            fun state ->
              let item = parse state in
              expression := false; item
          )
      )
  in
  let structure_expression state =
    let expr = seq_expr state E.structure_item in
    Str_eval (expr, post_item_attributes state)
  in
  (* After [;;], as [items] calls it. *)
  let starts kind = starts_structure_item kind || starts_expr kind in
  items
    state ~starts
    ( fun state ->
        match next state with
        | `SEMISEMI ->
            Some
              ( Claim_first
                  ( Undocumented,
                    fun state ->
                      expression := true; Str_semisemi (token state `SEMISEMI)
                  )
              )
        | `LET when !expression ->
            Some
              ( Claim_after
                  ( fun start state ->
                      let item = let_item_or_expression start state in
                      expression := false; item
                  )
              )
        | `LBRACKETATATAT -> item Marked structure_item
        | kind when starts_structure_item kind -> item Documented structure_item
        | kind when !expression && starts_expr kind ->
            item Marked structure_expression
        | _ -> None
    )

(* Where an expression may start a structure item: [let] starts bindings,
   which are an item ([let x = e]), or an expression if [in] follows them
   ([let x = e in e']), which do not claim the same doc comments. The [let]
   and the [and]s stay pending until [in] tells which one it is: the doc
   comments are then claimed with [start], for the item, and with
   [settle_let_bindings], for the bindings. *)
and let_item_or_expression start state =
  let let_ = take_pending state `LET in
  let ext_attributes = ext_attributes state in
  if let_item_at state then begin
    start Marked;
    let let_ = settle state let_ in
    let expr =
      seq_expr_from state (let_item_expression state let_ ext_attributes)
    in
    Str_eval (expr, post_item_attributes state)
  end
  else
    let ands = ref [] in
    let bindings =
      let_bindings_rest
        state ~docs:(Delay_docs ands) (unsettled state let_) ext_attributes
    in
    let items = not (is state `IN) in
    start (if items then Documented else Marked);
    let bindings =
      settle_let_bindings state ~items let_ (List.rev !ands) bindings
    in
    if items then Str_value bindings
    else
      let in_ = token state `IN in
      let expr =
        seq_expr_from
          state (mk (Exp_let (bindings, in_, seq_expr state E.let_body)))
      in
      Str_eval (expr, post_item_attributes state)

(* Bindings parsed with [Delay_docs], whose [let] and [and]s at [ands] are
   pending: their doc comments claimed for [items], or left as warnings for
   an expression, in the order of the tokens, and their tokens settled. *)
and settle_let_bindings
    state ~items let_ ands (_, ext_attributes, rec_, (first, rest))
  =
    let let_ = settle state let_ in
    let rest =
      List.map2
        ( fun index (~docstring:_, _, attributes, binding) ->
            let docstring =
              if items then and_docstring state index else (None, [], None)
            in
            let and_ =
              settle state { pending_index = index; pending_kind = `AND }
            in
            ~docstring, and_, attributes, binding
        )
        ands rest
    in
    (let_, ext_attributes, rec_, (first, rest))

and structure_item state =
  match next state with
  | `LET -> Str_value (let_bindings state ~docs:Claim_docs)
  | (`VAL | `EXTERNAL) as kind -> Str_primitive (value_description state kind)
  | `TYPE ->
      begin match type_item state ~rebind:true ~substitution:false with
      | `Typext value -> Str_typext value
      | `Declarations value -> Str_type value
      end
  | `EXCEPTION -> Str_exception (exception_declaration state ~rebind:true)
  | `MODULE ->
      if peek state 1 = `TYPE then begin
        let declaration = module_type_declaration ~substitution:false state in
        Str_modtype declaration
      end
      else begin let module_ = token state `MODULE in
        let ext_attributes = ext_attributes state in
        match optional state `REC with
        | None ->
            Str_module
              (module_, ext_attributes, module_binding state E.module_name)
        | Some rec_ ->
            let binding state = module_binding state E.recursive_module_name in
            let bindings = and_list state binding (binding state) in
            Str_recmodule (module_, ext_attributes, rec_, bindings)
      end
  | `OPEN ->
      let open_ = token state `OPEN in
      let bang = optional state `BANG in
      let ext_attributes = ext_attributes state in
      let expr =
        module_expr state (syntax_error "open-module" "'open' expects a module")
      in
      let post_attributes = post_item_attributes state in
      Str_open (open_, bang, ext_attributes, expr, post_attributes)
  | `INCLUDE ->
      let include_ = token state `INCLUDE in
      let ext_attributes = ext_attributes state in
      let expr =
        module_expr
          state (syntax_error "include-module" "'include' expects a module")
      in
      let post_attributes = post_item_attributes state in
      Str_include (include_, ext_attributes, expr, post_attributes)
  | `CLASS ->
      if peek state 1 = `TYPE then
        let class_, type_, ext_attributes, declarations =
          class_type_declarations state
        in
        Str_class_type (class_, type_, ext_attributes, declarations)
      else
        let class_, ext_attributes, bindings = class_infos state class_binding
        in
        Str_class (class_, ext_attributes, bindings)
  | `LBRACKETATATAT -> Str_attribute (floating_attribute state)
  | _ (* [[%%ext]], checked by the caller *) ->
      let extension = item_extension state in
      Str_extension (extension, post_item_attributes state)

and module_binding state name_error : module_binding =
  let name = module_name state name_error in
  let body = module_binding_body state ~functor_:false in
  (name, body, post_item_attributes state)

and module_binding_body state ~functor_ =
  match next state with
  | `EQUAL ->
      let equal = token state `EQUAL in
      Mbind_equal (equal, module_expr state E.module_definition)
  | `COLON ->
      let colon = token state `COLON in
      let typ =
        module_type
          state
          ( syntax_error
              "module-binding-type"
              "':' in a module definition expects a module type"
          )
      in
      let equal =
        expect state `EQUAL (syntax_error "module-binding-equal" "'=' expected")
      in
      Mbind_constraint
        (colon, typ, equal, module_expr state E.module_definition)
  | `LPAREN ->
      if not (functor_arg_at state 0) then
        raise (failure state E.module_declaration_functor_argument)
      ;
      let argument = functor_arg state in
      Mbind_functor (argument, module_binding_body state ~functor_:true)
  | _ ->
      raise
        ( failure
            state
            ( if functor_ then
                (syntax_error "functor-binding-body" "'=' or ':' expected")
              else (syntax_error "module-binding-body" "'=' or ':' expected")
            )
        )

(* {2 Let bindings} *)

(* [let x = e and y = e], whose bindings claim doc comments in structures
   ([docs]). *)
and let_bindings state ~docs =
  let let_ = token state `LET in
  let ext_attributes = ext_attributes state in
  let_bindings_rest state ~docs let_ ext_attributes

and let_bindings_rest state ~docs let_ (ext_attributes : ext_attributes) =
  let rec_ = optional state `REC in
  (* Punned bindings [let x] are only allowed with an extension. *)
  let pun = fst ext_attributes <> None in
  let bindings =
    and_list ~docs state (let_binding ~pun) (let_binding state ~pun)
  in
  (let_, ext_attributes, rec_, bindings)

and let_binding state ~pun : let_binding =
  let body = let_binding_body state ~pun in
  (body, post_item_attributes state)

(* A value name followed by something else than a pattern continuation. *)
and let_ident_at state =
  is_val_ident_at state 0
  &&
  begin match peek state (after_val_ident state 0) with
  | `COMMA | `COLONCOLON | `AS | `BAR | `LBRACKETAT -> false
  | _ -> true
  end

and let_binding_body state ~pun =
  if let_ident_at state then begin let name = val_ident state internal in
    match next state with
    | `COLON when peek state 1 = `TYPE ->
        let colon = token state `COLON in
        let type_ = token state `TYPE in
        let names = lidents state in
        if names = [] then raise (failure state E.annotation_newtype_names);
        let dot =
          expect state `DOT (syntax_error "let-newtype-dot" "'.' expected")
        in
        let typ = core_type state E.let_type in
        let equal = expect state `EQUAL E.let_typed_equal in
        Lb_newtypes
          ( name,
            (colon, (type_, names), dot, typ, equal, seq_expr state E.let_value)
          )
    | `COLON when typevars_then_dot state 1 ->
        let colon = token state `COLON in
        let variables = type_variables state in
        let dot = token state `DOT in
        let typ = core_type state E.let_type in
        let equal = expect state `EQUAL E.let_typed_equal in
        Lb_poly
          (name, colon, variables, dot, typ, equal, seq_expr state E.let_value)
    | `EQUAL as kind ->
        let equal = shift state kind in
        Lb_function (name, ([], None, equal, seq_expr state E.let_value))
    | _ ->
        begin match optional_type_constraint state with
        | Some constraint_ ->
            let equal = expect state `EQUAL E.let_typed_equal in
            Lb_constrained
              (name, constraint_, equal, seq_expr state E.let_value)
        | None when param_at state ->
            let params = function_params state in
            let constraint_ = optional_type_constraint state in
            let equal = expect state `EQUAL E.let_function_equal in
            Lb_function
              ( name,
                (params, constraint_, equal, fun_body state E.function_body)
              )
        | None ->
            if pun then Lb_pun name
            else
              raise
                ( failure
                    state
                    ( syntax_error
                        "let-binding-equal" "'=' or a parameter expected"
                    )
                )
        end
  end
  else begin
    (* [simple_pattern_not_ident COLON core_type EQUAL seq_expr]: only a simple
         pattern has a type constraint. *)
    let pattern =
      pattern_no_exn
        state
        ( syntax_error
            "let-pattern" "'let' or 'and' expects an identifier or a pattern"
        )
    in
    if is state `COLON then begin if not (is_simple_pattern pattern) then
        raise
          ( failure
              state
              ( syntax_error
                  "let-constraint-pattern"
                  "a binding only accepts a type after a name or a pattern in parentheses"
              )
          )
      ;
      let colon = token state `COLON in
      let typ = core_type state E.let_type in
      let equal = expect state `EQUAL E.let_typed_equal in
      Lb_pattern_constraint
        (pattern, colon, typ, equal, seq_expr state E.let_value)
    end
    else
      let equal = expect state `EQUAL E.let_pattern_equal in
      Lb_pattern (pattern, equal, seq_expr state E.let_value)
  end

and letop_binding state =
  if let_ident_at state then begin let name = val_ident state internal in
    match next state with
    | `EQUAL ->
        let equal = token state `EQUAL in
        Lop_function (name, ([], None, equal, seq_expr state E.letop_value))
    | _ when param_at state ->
        let params = function_params state in
        let constraint_ = optional_type_constraint state in
        let equal = expect state `EQUAL E.let_function_equal in
        Lop_function
          (name, (params, constraint_, equal, fun_body state E.function_body))
    | `COLON -> letop_constraint state (mk (Pat_var name))
    | _ -> Lop_pun name
  end
  else begin
    (* [simple_pattern COLON core_type EQUAL seq_expr]: only a simple pattern
         has a type constraint. *)
    let pattern =
      pattern_no_exn
        state
        ( syntax_error
            "letop-pattern"
            "'let*' or 'and*' expects an identifier or a pattern"
        )
    in
    if is state `COLON then begin if not (is_simple_pattern pattern) then
        raise
          ( failure
              state
              ( syntax_error
                  "letop-constraint-pattern"
                  "a 'let*' binding only accepts a type after a name or a pattern in parentheses"
              )
          )
      ;
      letop_constraint state pattern
    end
    else
      let equal = expect state `EQUAL E.let_pattern_equal in
      Lop_pattern (pattern, equal, seq_expr state E.letop_value)
  end

and letop_constraint state pattern =
  if peek state 1 = `TYPE || typevars_then_dot state 1 then
    raise
      ( failure_at
          state (state.index + 1)
          ( syntax_error
              "letop-constraint-poly"
              "a 'let*' binding cannot have a polymorphic type or locally abstract types"
          )
      )
  ;
  let colon = token state `COLON in
  let typ = core_type state E.let_type in
  let equal = expect state `EQUAL E.let_typed_equal in
  Lop_constraint (pattern, colon, typ, equal, seq_expr state E.letop_value)

(* [: t], [:> t] or [: t :> u], or [error]. *)
and type_constraint state error =
  match optional_type_constraint state with
  | Some constraint_ -> constraint_
  | None -> raise (failure state error)

(* [: t], [:> t] or [: t :> u], if the next token starts one. *)
and optional_type_constraint state =
  match next state with
  | `COLONGREATER as kind ->
      let coerce = shift state kind in
      Some (Coerce (None, coerce, core_type state E.type_constraint_type))
  | `COLON as kind ->
      let colon = shift state kind in
      let typ = core_type state E.type_constraint_type in
      begin match optional state `COLONGREATER with
      | Some coerce ->
          Some
            ( Coerce
                ( Some (colon, typ), coerce,
                  core_type state E.type_constraint_type
                )
            )
      | None -> Some (Constraint (colon, typ))
      end
  | _ -> None

(* {2 Function parameters} *)

(* Does a parameter start here? A sign starts a signed constant. *)
and param_at state =
  match next state with
  | `QUESTION | `OPTLABEL _ | `TILDE | `LABEL _ -> true
  | `MINUS | `PLUS -> is_signed_constant_at state 0
  | kind -> starts_simple_pattern kind

and function_params state =
  if param_at state then
    let param = function_param state in
    param :: function_params state
  else []

(* A parameter, checked by [param_at]. *)
and function_param state =
  if is state `LPAREN && peek state 1 = `TYPE then
    Param_newtypes (paren state ~unclosed:E.unclosed_newtypes newtypes)
  else Param_value (param_pattern state internal)

(* An optional default value [= e]. *)
and default_value state =
  preceded_option
    state `EQUAL
    ( fun state ->
        seq_expr
          state
          ( syntax_error
              "parameter-default"
              "'=' in an optional parameter expects an expression"
          )
    )

and optional_type state =
  if is state `COLON then
    let colon = token state `COLON in
    Some
      ( colon,
        possibly_poly
          state
          (syntax_error "parameter-type" "':' in a parameter expects a type")
          core_type
      )
  else None

(* A parameter, or [error]. *)
and param_pattern state error =
  match next state with
  | `QUESTION when peek state 1 = `LPAREN ->
      let question = token state `QUESTION in
      Param_optional_default
        ( question,
          paren
            state ~unclosed:E.unclosed_optional_parameter
            ( fun state ->
                let pattern = label_pattern state E.optional_parameter_name in
                pattern, default_value state
            )
        )
  | `QUESTION ->
      let question = token state `QUESTION in
      Param_optional (question, lident state E.optional_parameter_name)
  | `OPTLABEL _ as kind when peek state 1 = `LPAREN ->
      let optlabel = shift state kind in
      Param_optlabel_default
        ( optlabel,
          paren
            state ~unclosed:E.unclosed_optional_parameter
            ( fun state ->
                let pattern =
                  pattern
                    state
                    ( syntax_error
                        "optional-parameter-pattern"
                        "'(' after an optional label expects a pattern"
                    )
                in
                let typ = optional_type state in
                (pattern, typ), default_value state
            )
        )
  | `OPTLABEL _ as kind ->
      let optlabel = shift state kind in
      let var =
        take
          state
          ( syntax_error
              "optional-label-pattern"
              "an optional label in a parameter expects a lowercase identifier, '_', or a pattern \
               with a default value in parentheses"
          )
          ( function
              `LIDENT name -> Some (`LIDENT name)
            | `UNDERSCORE -> Some `UNDERSCORE
            | _ -> None
          )
      in
      Param_optlabel (optlabel, var)
  | `TILDE when peek state 1 = `LPAREN ->
      let tilde = token state `TILDE in
      Param_labelled_paren
        ( tilde,
          paren
            state
            ~unclosed:
              (syntax_error "unclosed-labelled-parameter" "')' expected")
            (fun state -> label_pattern state E.labelled_parameter_name)
        )
  | `TILDE ->
      let tilde = token state `TILDE in
      Param_labelled (tilde, lident state E.labelled_parameter_name)
  | `LABEL _ as kind ->
      let label = shift state kind in
      begin match
        paren_pattern_or_poly
          state
          ( syntax_error
              "label-parameter-pattern"
              "a label in a parameter expects a pattern, such as an identifier, '_', a literal or a \
               pattern in parentheses"
          )
      with
      | `Poly value -> Param_label_poly (label, value)
      | `Pattern pattern -> Param_label (label, pattern)
      end
  | _ ->
      begin match paren_pattern_or_poly state error with
      | `Poly value -> Param_poly value
      | `Pattern pattern -> Param_pattern pattern
      end

(* A simple pattern, or [(pattern : 'a. t)], or [error]. *)
and paren_pattern_or_poly state error =
  if
    is state `LPAREN && not (operator_at state 0)
    &&
    begin match peek state 1, peek state 2 with
    | (`MODULE | `RPAREN), _ | `COLONCOLON, `RPAREN -> false
    | _ -> true
    end
  then begin let opening = state.index in
    let left = token state `LPAREN in
    let content = pattern state E.parenthesized_pattern in
    if is state `COLON then begin let colon = token state `COLON in
      if typevars_then_dot state 0 then
        let typ = poly_type state in
        let right =
          closing state ~opening `RPAREN E.unclosed_parenthesized_pattern
        in
        `Poly { left; content = (content, colon, typ); right }
      else
        let typ = core_type state E.pattern_constraint_type in
        let right =
          closing state ~opening `RPAREN E.unclosed_parenthesized_pattern
        in
        `Pattern
          (mk (Pat_constraint { left; content = (content, colon, typ); right }))
    end
    else
      let right =
        closing state ~opening `RPAREN E.unclosed_parenthesized_pattern
      in
      `Pattern (mk (Pat_paren { left; content; right }))
  end
  else `Pattern (simple_pattern state error)

(* [(type a b)]. *)
and newtypes state =
  let type_ = token state `TYPE in
  let names = lidents state in
  if names = [] then
    raise
      ( failure
          state
          (syntax_error "newtype-names" "'(type' expects lowercase identifiers")
      )
  ;
  (type_, names)

(* [x] or [x : t], or [error]. *)
and label_pattern state error =
  let name = lident state error in
  (name, optional_type state)

(* [x : t] or [x :> t], in punned labels. *)
and name_constraint state =
  let name = lident state E.punned_label_name in
  name,
  type_constraint state (syntax_error "punned-label-constraint" "':' expected")

(* [do e done], after the condition or the bounds of a loop. *)
and do_done state error =
  if not (is state `DO) then raise (failure state error);
  delimited
    state `DO `DONE
    ~unclosed:
      ( syntax_error
          "unclosed-loop"
          "'done' \
           expected"
      )
    ( fun state ->
        seq_expr state (syntax_error "loop-body" "'do' expects an expression")
    )

(* The body of a function after its parameters: a [function] there gives the
   cases of the function ([fun_body]). *)
and fun_body state error =
  if is state `FUNCTION then
    let function_ = function_ state in
    if closed_function ~comma:true function_ && is state `COMMA then
      let tuple =
        expr_tuple_operators state (unreachable_function_tuple state function_)
      in
      seq_expr_rest state (expr_assignment state tuple)
    else function_
  else seq_expr state error

(* [function | p -> e | ...]. *)
and function_ state =
  let function_ = token state `FUNCTION in
  let ext_attributes = ext_attributes state in
  mk (Exp_function (function_, ext_attributes, match_cases state))

(* Is the expression a [function] whose last case is closed: can it not be
   extended by a following comma ([~comma]), or by an operator or a
   semicolon? A [function] is not a [fun_expr]: when its last case is closed,
   the following token applies to the enclosing expression. *)
and closed_function ~comma (expr : expr) =
  match expr with
  | { node = Exp_function (_, _, (_, { first; rest })); attributes = [] } ->
      let _, _, _, last =
        match List.rev rest with
          (_, case) :: _ -> case
        | [] -> first
      in
      closed_case ~comma last
  | _ -> false

(* The right-hand side of a case is closed when it is unreachable or ends with
   a semicolon. A [function] there takes a following comma (as the first
   element of a tuple), but not an operator or a semicolon. *)
and closed_case ~comma (expr : expr) =
  match expr with
  | { node = Exp_unreachable _ | Exp_semi _; attributes = [] } -> true
  | { node = Exp_sequence (_, _, rest) | Exp_sequence_ext (_, _, _, _, rest);
      attributes = []
    }
  -> closed_case ~comma rest
  | { node = Exp_function _; attributes = [] } ->
      (not comma) && closed_function ~comma expr
  | _ -> false

(* [function p -> ., e]: a [function] ending with an unreachable case is the
   first element of a tuple when a comma follows, wherever it is an [expr]. *)
and unreachable_function_tuple state function_ =
  let first = Tuple_expr (~label:None, function_) in
  mk
    ( Exp_tuple
        (non_empty_separated state `COMMA tuple_element_after_comma first)
    )

(* A [function], which is a tuple if it ends with an unreachable case followed
   by a comma, in an operand of precedence at least [minimum]. *)
and function_expr state ~minimum =
  let function_ = function_ state in
  if closed_function ~comma:true function_ && is state `COMMA then
    fst
      ( tuple_operators
          state minimum (unreachable_function_tuple state function_)
      )
  else function_

and match_cases state = bar_list state match_case

and match_case state : case =
  let lhs =
    pattern state (syntax_error "case-pattern" "a case expects a pattern")
  in
  let guard =
    preceded_option
      state `WHEN
      ( fun state ->
          seq_expr
            state
            (syntax_error "case-guard" "'when' in a case expects a condition")
      )
  in
  let arrow =
    expect state `MINUSGREATER (syntax_error "case-arrow" "'->' expected")
  in
  let rhs =
    (* [pattern MINUSGREATER DOT]: an unreachable case has no guard. *)
    if is state `DOT then
      if guard = None then mk (Exp_unreachable (token state `DOT))
      else
        raise
          ( failure
              state
              ( syntax_error
                  "unreachable-case-guard"
                  "an unreachable case 'p -> .' cannot have a guard"
              )
          )
    else
      seq_expr
        state
        ( syntax_error
            "case-body" "'->' in a case expects an expression, or '.'"
        )
  in
  (lhs, guard, arrow, rhs)

(* {2 Expressions} *)

and seq_expr state error = seq_expr_rest state (expr state error)

(* A sequence whose first expression starts with [operand], an expression
   starting with a keyword ([let ... in e]), as parsed by [expr_keyword]:
   the operators, the tuple and the sequence after it, as [seq_expr] parses
   them after [expr_keyword]. *)
and seq_expr_from state operand =
  let first = Tuple_expr (~label:None, binary_loop state 0 operand) in
  let expr =
    expr_assignment
      state (expr_tuple_operators state (expr_tuple_from state first))
  in
  seq_expr_rest state expr

(* The rest of a sequence after its first expression. A [function] is not a
   [fun_expr], which [fun_expr SEMI seq_expr] requires: when its last case is
   closed, a following [;] belongs to the enclosing expression
   ([let x = e in function p -> . ; e]). *)
and seq_expr_rest state expr =
  if closed_function ~comma:false expr then
    stop
      state
      ( syntax_error
          "closed-function"
          "a 'function' whose last case is '.' or ends with ';' cannot be followed by more code: \
           parentheses around it are expected, as in '(function x -> .)'"
      )
  ;
  if is state `SEMI && not (closed_function ~comma:false expr) then begin
    let semi = token state `SEMI in
    if is state `PERCENT then
      let percent = token state `PERCENT in
      let id =
        attr_id
          state
          ( syntax_error
              "sequence-extension-name"
              "'%' in a sequence expects an identifier, as in '; %ext e'"
          )
      in
      mk
        ( Exp_sequence_ext
            ( expr, semi, percent, id,
              seq_expr
                state
                ( syntax_error
                    "sequence-extension-expr" "'; %ext' expects an expression"
                )
            )
        )
    else if starts_expr (next state) then
      mk (Exp_sequence (expr, semi, seq_expr state internal))
    else mk (Exp_semi (expr, semi))
  end
  else expr

(* A [function] ending with an unreachable case and a comma starts a tuple,
   which, after operators, may be the first element of another tuple:
   [function p -> ., function q -> . :: l, e] is
   [((function p -> ., function q -> .) :: l), e]. *)
and expr state error =
  let lhs =
    if is state `FUNCTION then
      let function_ = function_ state in
      if closed_function ~comma:true function_ && is state `COMMA then
        unreachable_function_tuple state function_
      else function_
    else expr_tuple state error
  in
  expr_assignment state (expr_tuple_operators state lhs)

(* [e := e], of lower precedence than tuples ([::] is a binary operator, see
   [binary_loop]). A closed [function] is not a [fun_expr] and has no such
   operator; after one, as the right operand, the operators apply to the whole
   expression: [a := function p -> . :: b] is [(a := function p -> .) :: b]. *)
and expr_assignment state lhs =
  if closed_function ~comma:false lhs then lhs
  else
    (* After the operators, a comma starts a tuple: [a := function p -> . ** b,
       c] is [((a := function p -> .) ** b), c]. *)
    let after rhs expr =
      if closed_function ~comma:false rhs then begin let start = state.index in
        let expr = binary_loop state 0 expr in
        let expr =
          if state.index > start && is state `COMMA then
            let first = Tuple_expr (~label:None, expr) in
            expr_tuple_operators
              state
              ( mk
                  ( Exp_tuple
                      ( non_empty_separated
                          state `COMMA tuple_element_after_comma first
                      )
                  )
              )
          else expr
        in
        expr_assignment state expr
      end
      else expr
    in
    if is state `COLONEQUAL then
      let operator = operator_token state in
      let rhs =
        expr state (syntax_error "assignment-rhs" "':=' expects an expression")
      in
      after rhs (mk (Exp_infix (lhs, operator, rhs)))
    else lhs

and expr_tuple state error = expr_tuple_from state (tuple_element state error)

(* A tuple, after its first element, or the first element alone. *)
and expr_tuple_from state first =
  if is state `COMMA then
    mk
      ( Exp_tuple
          (non_empty_separated state `COMMA tuple_element_after_comma first)
      )
  else
    match first with
    | Tuple_expr (~label:None, expr) -> expr
    | _ -> raise (failure state E.labelled_expr_alone)

and tuple_element_after_comma state =
  tuple_element
    state
    (syntax_error "tuple-element" "',' expects another element of the tuple")

and tuple_element state error =
  match next state with
  | `LABEL _ as kind ->
      let label = shift state kind in
      Tuple_expr (~label:(Some label), simple_expr state E.label_value)
  | `TILDE when peek state 1 = `LPAREN ->
      let tilde = token state `TILDE in
      Tuple_pun_constraint
        (tilde, paren state ~unclosed:E.unclosed_punned_label name_constraint)
  | `TILDE ->
      let tilde = token state `TILDE in
      Tuple_pun (tilde, lident state E.tilde_label_name)
  | `FUNCTION -> Tuple_expr (~label:None, function_expr state ~minimum:0)
  | _ -> Tuple_expr (~label:None, expr_binary state 0 error)

(* Binary operators, by increasing precedence. Attributes after an expression
   bind between [@...] and [::]. *)
and binary_level =
  function
  | `OR | `BARBAR -> Some (6, `Right)
  | `AMPERSAND | `AMPERAMPER -> Some (8, `Right)
  | `INFIXOP0 _ | `EQUAL | `LESS | `GREATER -> Some (10, `Left)
  | `INFIXOP1 _ -> Some (12, `Right)
  | `LBRACKETAT -> Some (13, `Left)
  | `COLONCOLON -> Some (14, `Right)
  | `INFIXOP2 _ | `PLUS | `PLUSDOT | `MINUS | `MINUSDOT | `PLUSEQ ->
      Some (16, `Left)
  | `PERCENT | `INFIXOP3 _ | `STAR -> Some (18, `Left)
  | `INFIXOP4 _ -> Some (20, `Right)
  | _ -> None

and expr_binary state minimum error =
  binary_loop state minimum (expr_unary state error)

(* The operators of precedence at least [minimum] after [lhs]. *)
and binary_loop state minimum lhs =
  match binary_level (next state) with
  | Some (level, associativity) when level >= minimum ->
      if is state `LBRACKETAT then
        let attribute = attribute_with state `LBRACKETAT in
        binary_loop state minimum (add_attributes lhs [ attribute ])
      else begin let right_minimum =
          if associativity = `Right then level else level + 1
        in
        if is state `COLONCOLON then
          let cons = token state `COLONCOLON in
          let rhs = binary_operand state right_minimum E.cons_rhs in
          binary_loop state minimum (mk (Exp_cons (lhs, cons, rhs)))
        else
          let operator = operator_token state in
          let error = E.operator_rhs (Token.text operator) in
          let rhs = binary_operand state right_minimum error in
          binary_loop state minimum (mk (Exp_infix (lhs, operator, rhs)))
      end
  | _ -> lhs

(* Does a tuple end with an element which is not a [fun_expr]: a labelled
   element ([~x:e], [~x], [~(x : t)]) or a closed [function]? An operator or
   an attribute after it applies to the whole tuple ([a, ~x:e ** b]). *)
and ends_with_closed_element (expr : expr) =
  match expr with
  | { node = Exp_tuple { first; rest }; attributes = [] } ->
      let last = List.fold_left (fun _ (_, element) -> element) first rest in
      begin match last with
      | Tuple_expr (~label:None, expr) -> closed_function ~comma:false expr
      | Tuple_expr (~label:(Some _), _) | Tuple_pun _ | Tuple_pun_constraint _
      -> true
      end
  | _ -> false

(* The operators of precedence at least [minimum] applied to such a tuple,
   and whether there are some. *)
and tuple_operators state minimum tuple =
  if ends_with_closed_element tuple && binary_level (next state) <> None then
    binary_loop state minimum tuple, true
  else tuple, false

(* At the top level of an expression, a comma after such operators starts a
   tuple: [a, function p -> . ** b, c] is [((a, function p -> .) ** b), c]. *)
and expr_tuple_operators state tuple =
  match tuple_operators state 0 tuple with
  | expr, true when is state `COMMA ->
      let first = Tuple_expr (~label:None, expr) in
      expr_tuple_operators
        state
        ( mk
            ( Exp_tuple
                ( non_empty_separated
                    state `COMMA tuple_element_after_comma first
                )
            )
        )
  | expr, _ -> expr

(* The right operand of an operator is an [expr]: a [function], or a tuple
   starting with a labelled element ([a + ~x, y] is [a + (~x, y)]). *)
and binary_operand state minimum error =
  if is state `FUNCTION then function_expr state ~minimum
  else if starts_labelled_tuple state then
    fst (tuple_operators state minimum (labelled_tuple_expr state))
  else expr_binary state minimum error

and expr_unary state error =
  match next state with
  | (`MINUS | `MINUSDOT | `PLUS | `PLUSDOT) as kind ->
      let operator = shift state kind in
      mk
        ( Exp_unary
            ( operator,
              unary_operand state (E.sign_operand (Token.text operator))
            )
        )
  | _ -> expr_application state error

(* The operand of a sign, which may be a tuple starting with a labelled
   element ([- ~x, y]), as the right operand of an operator. *)
and unary_operand state error =
  if starts_labelled_tuple state then labelled_tuple_expr state
  else expr_unary state error

and starts_labelled_tuple state =
  match next state with
    `LABEL _ | `TILDE -> true
  | _ -> false

(* A tuple starting with a labelled element, which is not an expression by
   itself: [~x, y]. *)
and labelled_tuple_expr state =
  let first = tuple_element state internal in
  if not (is state `COMMA) then raise (failure state E.labelled_expr_alone);
  mk
    ( Exp_tuple
        (non_empty_separated state `COMMA tuple_element_after_comma first)
    )

and expr_application state error =
  match next state with
  | `LET | `LETOP _ | `MATCH | `TRY | `IF | `FUN
    | `FUNCTION | `WHILE | `FOR | `ASSERT | `LAZY
  -> expr_keyword state
  | _ ->
      let head = simple_expr state error in
      let applied =
        match head with
        | { node = Exp_construct (constructor, None); attributes = [] } when
            starts_simple_expr (next state)
        -> mk (Exp_construct (constructor, Some (simple_expr state internal)))
        | { node = Exp_variant (tag, None); attributes = [] } when
            starts_simple_expr (next state)
        -> mk (Exp_variant (tag, Some (simple_expr state internal)))
        | _ when starts_argument (next state) ->
            mk (Exp_apply (head, repeat state starts_argument argument))
        | _ -> head
      in
      if is state `LESSMINUS then
        match applied with
        | { node = Exp_field (record, dot, label); attributes = [] } ->
            let arrow = token state `LESSMINUS in
            mk
              ( Exp_setfield
                  (record, dot, label, arrow, expr state E.assignment_value)
              )
        | { node = Exp_index (_, Index_builtin _, Brackets _, _); _ } ->
            raise
              ( failure
                  state
                  ( syntax_error
                      "string-assignment"
                      "strings are immutable and cannot be assigned: use 'Bytes.set'"
                  )
              )
        | { node = Exp_index (array, dot, index, None); attributes = [] } ->
            let arrow = token state `LESSMINUS in
            mk
              ( Exp_index
                  ( array, dot, index,
                    Some (arrow, expr state E.assignment_value)
                  )
              )
        | { node = Exp_ident (None, Val_lident name); attributes = [] } ->
            let arrow = token state `LESSMINUS in
            mk (Exp_setinstvar (name, arrow, expr state E.assignment_value))
        | _ -> applied
      else applied

(* An argument, checked by [starts_argument]. *)
and argument state =
  match next state with
  | `LABEL _ as kind ->
      let label = shift state kind in
      Arg_label (label, simple_expr state E.label_value)
  | `TILDE when peek state 1 = `LPAREN ->
      let tilde = token state `TILDE in
      Arg_pun_constraint
        (tilde, paren state ~unclosed:E.unclosed_punned_label name_constraint)
  | `TILDE ->
      let tilde = token state `TILDE in
      Arg_pun (tilde, lident state E.tilde_label_name)
  | `QUESTION ->
      let question = token state `QUESTION in
      Arg_optional_pun
        ( question,
          lident
            state
            ( syntax_error
                "question-label-name" "'?' expects a lowercase identifier"
            )
        )
  | `OPTLABEL _ as kind ->
      let optlabel = shift state kind in
      Arg_optlabel
        ( optlabel,
          simple_expr
            state
            ( syntax_error
                "optional-label-value"
                "an optional label expects an argument, such as an identifier, a literal or an \
                 expression in parentheses"
            )
        )
  | _ -> Arg_expr (simple_expr state internal)

(* After [let] and its extension and attributes: an item, as in
   [let module M = N in e], rather than bindings. *)
and let_item_at state =
  match next state with
  | `OPEN | `MODULE | `EXCEPTION | `TYPE | `EXTERNAL | `VAL | `CLASS
    | `LBRACKETATATAT | `LBRACKETPERCENTPERCENT | `QUOTED_STRING_ITEM _
  -> true
  | _ -> false

(* [let module M = N in e], after [let] and its extension and attributes,
   checked by [let_item_at]. *)
and let_item_expression state let_ ext_attributes =
  let claim = if is state `LBRACKETATATAT then Marked else Documented in
  let item = documented state claim structure_item in
  let in_ = expect state `IN (syntax_error "let-item-in" "'in' expected") in
  let body = seq_expr state E.let_body in
  mk (Exp_struct_item (let_, ext_attributes, item, in_, body))

(* The expressions starting with a keyword, checked by the caller. *)
and expr_keyword state =
  match next state with
  | `LET ->
      let let_ = token state `LET in
      let ext_attributes = ext_attributes state in
      if let_item_at state then let_item_expression state let_ ext_attributes
      else begin let bindings =
          let_bindings_rest state ~docs:Ignore_docs let_ ext_attributes
        in
        let in_ = expect state `IN (syntax_error "let-in" "'in' expected") in
        mk (Exp_let (bindings, in_, seq_expr state E.let_body))
      end
  | `LETOP _ as kind ->
      let let_op = shift state kind in
      let first = letop_binding state in
      let rec rest values =
        match next state with
        | `ANDOP _ as kind ->
            let and_op = shift state kind in
            rest ((and_op, letop_binding state) :: values)
        | _ -> List.rev values
      in
      let bindings = { first; rest = rest [] } in
      let in_ = expect state `IN (syntax_error "letop-in" "'in' expected") in
      mk (Exp_letop (let_op, bindings, in_, seq_expr state E.let_body))
  | `MATCH ->
      let match_ = token state `MATCH in
      let ext_attributes = ext_attributes state in
      let scrutinee =
        seq_expr
          state (syntax_error "match-scrutinee" "'match' expects an expression")
      in
      let with_ =
        expect state `WITH (syntax_error "match-with" "'with' expected")
      in
      mk
        ( Exp_match
            (match_, ext_attributes, scrutinee, with_, match_cases state)
        )
  | `TRY ->
      let try_ = token state `TRY in
      let ext_attributes = ext_attributes state in
      let body =
        seq_expr state (syntax_error "try-body" "'try' expects an expression")
      in
      let with_ = expect state `WITH (syntax_error "try-with" "'with' expected")
      in
      mk (Exp_try (try_, ext_attributes, body, with_, match_cases state))
  | `IF ->
      let if_ = token state `IF in
      let ext_attributes = ext_attributes state in
      let condition =
        seq_expr state (syntax_error "if-condition" "'if' expects a condition")
      in
      let then_ = expect state `THEN (syntax_error "if-then" "'then' expected")
      in
      let if_true =
        expr state (syntax_error "then-branch" "'then' expects an expression")
      in
      let if_false =
        preceded_option
          state `ELSE
          ( fun state ->
              expr
                state
                (syntax_error "else-branch" "'else' expects an expression")
          )
      in
      mk (Exp_if (if_, ext_attributes, condition, then_, if_true, if_false))
  | `FUN ->
      let fun_ = token state `FUN in
      let ext_attributes = ext_attributes state in
      let params = function_params state in
      if params = [] then
        raise
          ( failure
              state (syntax_error "fun-parameter" "'fun' expects a parameter")
          )
      ;
      let constraint_ =
        preceded_option
          state `COLON
          ( fun state ->
              atomic_type
                state
                ( syntax_error
                    "fun-return-type"
                    "':' after the parameters of 'fun' expects a type"
                )
          )
      in
      let arrow =
        expect state `MINUSGREATER (syntax_error "fun-arrow" "'->' expected")
      in
      mk
        ( Exp_fun
            ( fun_, ext_attributes, params, constraint_, arrow,
              fun_body
                state
                ( syntax_error
                    "fun-body" "'->' in a function expects an expression"
                )
            )
        )
  | `FUNCTION ->
      (* The operand of a sign ([%prec prec_unary_minus]), which does not
         extend over a comma or a bar: a single case. *)
      let function_ = token state `FUNCTION in
      let ext_attributes = ext_attributes state in
      let bar = optional state `BAR in
      let case = match_case state in
      if is state `BAR then
        stop
          state
          ( syntax_error
              "sign-function-case"
              "a 'function' after a sign has a single case: parentheses around it are expected, as in \
               '- (function A -> 1 | B -> 2)'"
          )
      ;
      mk
        ( Exp_function
            (function_, ext_attributes, (bar, { first = case; rest = [] }))
        )
  | `WHILE ->
      let while_ = token state `WHILE in
      let ext_attributes = ext_attributes state in
      let condition =
        seq_expr
          state (syntax_error "while-condition" "'while' expects a condition")
      in
      mk
        ( Exp_while
            ( while_, ext_attributes, condition,
              do_done state (syntax_error "while-do" "'do' expected")
            )
        )
  | `FOR ->
      let for_ = token state `FOR in
      let ext_attributes = ext_attributes state in
      let pattern =
        pattern
          state
          ( syntax_error
              "for-index" "'for' expects a lowercase identifier or '_'"
          )
      in
      let equal = expect state `EQUAL (syntax_error "for-equal" "'=' expected")
      in
      let start =
        seq_expr
          state
          (syntax_error "for-start" "'=' in a 'for' loop expects an expression")
      in
      let direction =
        take
          state (syntax_error "for-direction" "'to' or 'downto' expected")
          ( function
              `TO -> Some `TO
            | `DOWNTO -> Some `DOWNTO
            | _ -> None
          )
      in
      let stop =
        seq_expr
          state
          (syntax_error "for-stop" "'to' or 'downto' expects an expression")
      in
      mk
        ( Exp_for
            ( for_, ext_attributes, pattern, equal, start, direction,
              stop, do_done state (syntax_error "for-do" "'do' expected")
            )
        )
  | `ASSERT ->
      let assert_ = token state `ASSERT in
      let ext_attributes = ext_attributes state in
      mk
        ( Exp_assert
            ( assert_, ext_attributes,
              simple_expr
                state
                ( syntax_error
                    "assert-expr"
                    "'assert' expects an argument, such as an identifier, a literal or an expression in \
                     parentheses"
                )
            )
        )
  | `LAZY ->
      let lazy_ = token state `LAZY in
      let ext_attributes = ext_attributes state in
      mk
        ( Exp_lazy
            ( lazy_, ext_attributes,
              simple_expr
                state
                ( syntax_error
                    "lazy-expr"
                    "'lazy' expects an argument, such as an identifier, a literal or an expression in \
                     parentheses"
                )
            )
        )
  | _ -> (raise (internal_failure state "expected a keyword") [@coverage off])

(* Simple expressions: an atom followed by field accesses, indexing, method
   calls and [##] operators. The operand of a prefix operator is an atom:
   [!a.b] is [(!a).b]. *)
and simple_expr state error =
  let rec loop expr =
    match next state, peek state 1 with
    | `HASH, `LIDENT _ ->
        let hash = token state `HASH in
        loop
          ( dot_postfix
              state (mk (Exp_send (expr, hash, lident state internal)))
          )
    | `HASHOP _, _ ->
        let operator = operator_token state in
        let rhs =
          dot_postfix state (atom state (E.hashop_rhs (Token.text operator)))
        in
        loop (mk (Exp_infix (expr, operator, rhs)))
    | _ -> expr
  in
  loop (dot_postfix state (atom state error))

and atom state error =
  match next state with
  | (`INT _ | `FLOAT _ | `STRING _ | `CHAR _) as kind ->
      mk (Exp_constant (shift state kind))
  | `LIDENT _ as kind -> mk (Exp_ident (None, Val_lident (shift state kind)))
  | `LPAREN when operator_at state 0 ->
      mk (Exp_ident (None, val_ident state internal))
  | `UIDENT _ -> qualified_expr state
  | `TRUE as kind ->
      mk (Exp_construct ((None, Constr_true (shift state kind)), None))
  | `FALSE as kind ->
      mk (Exp_construct ((None, Constr_false (shift state kind)), None))
  | `LPAREN -> paren_expr state
  | `LBRACKET -> list_expr state
  | `LBRACKETBAR -> array_expr state
  | `LBRACE -> record_expr state
  | `LBRACELESS -> override_expr state
  | `BEGIN ->
      mk
        ( Exp_begin
            ( delimited
                state `BEGIN `END
                ~unclosed:(syntax_error "unclosed-begin" "'end' expected")
                ( fun state ->
                    let ext_attributes = ext_attributes state in
                    ext_attributes,
                    if is state `END then None
                    else
                      Some
                        ( seq_expr
                            state
                            ( syntax_error
                                "begin-body"
                                "'begin' expects an expression or 'end'"
                            )
                        )
                )
            )
        )
  | `NEW ->
      let new_ = token state `NEW in
      let ext_attributes = ext_attributes state in
      mk
        ( Exp_new
            ( new_, ext_attributes,
              class_longident
                state
                ( syntax_error
                    "new-class"
                    "'new' expects a lowercase identifier or a path, as in 'c' or 'M.c'"
                )
            )
        )
  | `OBJECT ->
      mk
        ( Exp_object
            ( delimited
                state `OBJECT `END ~unclosed:E.unclosed_object
                ( fun state ->
                    let ext_attributes = ext_attributes state in
                    ext_attributes, class_structure state
                )
            )
        )
  | `BACKQUOTE -> mk (Exp_variant (name_tag state, None))
  | (`PREFIXOP _ | `BANG) as kind ->
      let operator = shift state kind in
      mk
        ( Exp_prefix
            (operator, atom state (E.prefix_operand (Token.text operator)))
        )
  | `LBRACKETPERCENT | `QUOTED_STRING_EXPR _ ->
      mk (Exp_extension (extension state))
  | _ -> raise (failure state error)

(* Field accesses and indexing. *)
and dot_postfix state expr =
  match next state, peek state 1 with
  | `DOT, (`LPAREN | `LBRACKET | `LBRACE) ->
      let dot = token state `DOT in
      dot_postfix state (index_expr state expr (Index_builtin dot))
  | `DOT, `LIDENT _ ->
      let dot = token state `DOT in
      let label = (None, lident state internal) in
      dot_postfix state (mk (Exp_field (expr, dot, label)))
  | `DOT, `UIDENT _ ->
      let dot = token state `DOT in
      let path = module_path state ~apply:false internal in
      begin match next state with
      | `DOTOP _ as kind ->
          let dotop = shift state kind in
          dot_postfix
            state (index_expr state expr (Index_user (Some (dot, path), dotop)))
      | _ ->
          let dot' = expect state `DOT E.field_after_module in
          let label = (Some (path, dot'), lident state E.field_after_module) in
          dot_postfix state (mk (Exp_field (expr, dot, label)))
      end
  | `DOT, _ ->
      raise
        ( failure_at
            state (state.index + 1)
            ( syntax_error
                "after-dot" "an identifier, '(', '[' or '{' expected after '.'"
            )
        )
  | (`DOTOP _ as kind), _ ->
      let dotop = shift state kind in
      if
        not
          ( match next state with
              `LPAREN | `LBRACKET | `LBRACE -> true
            | _ -> false
          )
      then raise (failure state E.unclosed_index_operator_bracket)
      ;
      dot_postfix state (index_expr state expr (Index_user (None, dotop)))
  | _ -> expr

(* An index after [.] or an indexing operator, checked by the caller. *)
and index_expr state array dot =
  if
    not
      ( match next state with
          `LPAREN | `LBRACKET | `LBRACE -> true
        | _ -> false
      )
  then raise (failure state E.unclosed_index_operator_bracket)
  ;
  let index =
    brackets
      state ~unclosed:E.unclosed_index
      ( fun closing ->
          match dot with
          | Index_builtin _ ->
              { first = seq_expr state E.index_expr; rest = [] }, None
          | Index_user _ ->
              separated_or_terminated
                state (fun state -> expr state E.index_expr)
                ~stop:(fun kind -> kind = closing)
      )
  in
  mk (Exp_index (array, dot, index, None))

(* An expression starting with an uppercase identifier: a constructor, a
   qualified value or a local open. *)
and qualified_expr state =
  let path = module_path state ~apply:false internal in
  match next state, peek state 1 with
  | `DOT, `LIDENT _ ->
      let dot = token state `DOT in
      mk (Exp_ident (Some (path, dot), val_ident state internal))
  | `DOT, `LPAREN when operator_at state 1 ->
      let dot = token state `DOT in
      mk (Exp_ident (Some (path, dot), val_ident state internal))
  | `DOT, `LPAREN when peek state 2 = `COLONCOLON && peek state 3 = `RPAREN ->
      let dot = token state `DOT in
      mk (Exp_construct ((Some (path, dot), constr_ident state internal), None))
  | `DOT, (`LPAREN | `LBRACE | `LBRACELESS | `LBRACKETBAR | `LBRACKET) ->
      let dot = token state `DOT in
      let expr =
        match next state with
        | `LPAREN when peek state 1 = `RPAREN ->
            mk (Exp_construct ((None, constr_ident state internal), None))
        | `LPAREN when peek state 1 = `MODULE ->
            paren_expr ~local_open:true state
        | `LPAREN ->
            mk
              ( Exp_paren
                  ( paren
                      state
                      ~unclosed:
                        (syntax_error "unclosed-local-open" "')' expected")
                      ( fun state ->
                          seq_expr
                            state
                            ( syntax_error
                                "local-open-expr" "'M.(' expects an expression"
                            )
                      )
                  )
              )
        | `LBRACE -> record_expr state
        | `LBRACELESS ->
            if peek state 1 = `GREATERRBRACE then
              raise
                ( failure_at
                    state (state.index + 1)
                    ( syntax_error
                        "local-open-empty-override"
                        "a copy of an object after a module path expects a lowercase identifier"
                    )
                )
            ;
            override_expr state
        | `LBRACKETBAR -> array_expr state
        | _ -> list_expr state
      in
      mk (Exp_local_open (path, dot, expr))
  | `DOT, _ ->
      raise
        ( failure_at
            state (state.index + 1)
            ( syntax_error
                "after-module-dot"
                "an identifier, '(', '[', '[|', '{' or '{<' expected after 'M.'"
            )
        )
  | _ ->
      let qualifier, name = split_path state path in
      mk (Exp_construct ((qualifier, Constr_uident name), None))

(* A parenthesized expression. After [M.] ([local_open]), a first-class
   module needs its module type. *)
and paren_expr ?(local_open = false) state =
  match peek state 1, peek state 2 with
  | `RPAREN, _ | `COLONCOLON, `RPAREN ->
      mk (Exp_construct ((None, constr_ident state internal), None))
  | `MODULE, _ ->
      mk
        ( Exp_pack
            ( paren
                state ~unclosed:E.unclosed_pack
                ( fun state ->
                    let module_ = token state `MODULE in
                    let ext_attributes = ext_attributes state in
                    let expr =
                      module_expr
                        state
                        ( syntax_error
                            "pack-module" "'(module' expects a module"
                        )
                    in
                    if local_open && not (is state `COLON) then
                      raise
                        ( failure
                            state
                            ( syntax_error
                                "local-open-pack-type"
                                "':' expected, a first-class module after 'M.' needs its module type, as in \
                                 'M.(module E : S)'"
                            )
                        )
                    ;
                    let package =
                      preceded_option
                        state `COLON
                        (fun state -> package_module_type state E.pack_type)
                    in
                    (module_, ext_attributes, expr, package)
                )
            )
        )
  | _ ->
      let opening = state.index in
      let left = token state `LPAREN in
      let content =
        seq_expr
          state (syntax_error "parenthesized-expr" "'(' expects an expression")
      in
      let constraint_ = optional_type_constraint state in
      let right = closing state ~opening `RPAREN E.unclosed_parenthesized_expr
      in
      begin match constraint_ with
      | Some constraint_ ->
          mk (Exp_constraint { left; content = (content, constraint_); right })
      | None -> mk (Exp_paren { left; content; right })
      end

and list_expr state =
  if peek state 1 = `RBRACKET then
    mk (Exp_construct ((None, constr_ident state internal), None))
  else
    mk
      ( Exp_list
          ( delimited
              state `LBRACKET `RBRACKET
              ~unclosed:(syntax_error "unclosed-list" "';' or ']' expected")
              ( semi_list
                  ( fun state ->
                      expr
                        state
                        ( syntax_error
                            "list-element" "a list expects an expression"
                        )
                  )
                  `RBRACKET
              )
          )
      )

and array_expr state =
  mk
    ( Exp_array
        ( delimited
            state `LBRACKETBAR `BARRBRACKET
            ~unclosed:(syntax_error "unclosed-array" "';' or '|]' expected")
            ( semi_list_or_empty
                ( fun state ->
                    expr
                      state
                      ( syntax_error
                          "array-element" "an array expects an expression"
                      )
                )
                `BARRBRACKET
            )
        )
    )

and record_expr state =
  mk
    ( Exp_record
        ( delimited
            state `LBRACE `RBRACE
            ~unclosed:(syntax_error "unclosed-record" "';' or '}' expected")
            record_content
        )
    )

(* [e with x = v; ...] or [x = v; ...]: an expression, which is the base if
   [with] follows it, else the label of the first field. *)
and record_content state =
  let fields first =
    separated_or_terminated_from
      state record_field ~stop:(fun kind -> kind = `RBRACE) first
  in
  match next state with
  | `LIDENT _ | `UIDENT _ ->
      let value = simple_expr state E.record_field_name in
      if is state `WITH then
        let with_ = token state `WITH in
        Some (value, with_), semi_list record_field `RBRACE state
      else begin match value with
      | { node = Exp_ident (qualifier, Val_lident name); attributes = [] } ->
          None, fields (record_field_after_label state (qualifier, name))
      | _ -> raise (failure state E.record_field_name)
      end
  | _ ->
      let value = simple_expr state E.record_field_name in
      let with_ =
        expect
          state `WITH
          ( syntax_error
              "record-base-with"
              "'with' expected after the base of a record, as in '{ r with x = v }'"
          )
      in
      Some (value, with_), semi_list record_field `RBRACE state

and record_field state =
  record_field_after_label state (label_longident state E.record_field_name)

and record_field_after_label state label =
  let constraint_ = optional_type_constraint state in
  ( label, constraint_,
    preceded_option
      state `EQUAL
      ( fun state ->
          expr
            state
            ( syntax_error
                "record-field-value" "'=' in a record expects an expression"
            )
      )
  )

and override_expr state =
  let field state =
    let name =
      lident
        state
        ( syntax_error
            "override-field-name" "'{<' expects a lowercase identifier"
        )
    in
    ( name,
      preceded_option
        state `EQUAL
        ( fun state ->
            expr
              state
              ( syntax_error
                  "override-field-value"
                  "'=' in '{< ... >}' expects an expression"
              )
        )
    )
  in
  mk
    ( Exp_override
        ( delimited
            state `LBRACELESS `GREATERRBRACE
            ~unclosed:(syntax_error "unclosed-override" "';' or '>}' expected")
            (semi_list_or_empty field `GREATERRBRACE)
        )
    )

(* {2 Patterns} *)

and pattern state error = pattern_level state ~exn:true 0 error

and pattern_no_exn state error = pattern_level state ~exn:false 0 error

(* Pattern operators by increasing precedence: [as] (2), [|] (4), [,] (6),
   attributes (7), [::] (8). *)
and pattern_level state ~exn minimum error =
  let lhs =
    match next state with
    | `LABEL _ | `TILDE -> labelled_tuple_pattern state ~exn
    | _ -> pattern_gen state ~exn error
  in
  pattern_loop state ~exn minimum lhs

(* A tuple starting with a labelled element, which is not a pattern by
   itself: wherever a [pattern] is expected, even after an operator of higher
   precedence ([p :: ~x, y]), the tuple extends over the following commas. *)
and labelled_tuple_pattern state ~exn =
  let first = tuple_pattern_element state ~exn internal in
  if not (is state `COMMA) then
    raise
      ( failure
          state
          ( syntax_error
              "labelled-pattern-alone"
              "',' expected, a labelled pattern '~x:p' is an element of a tuple"
          )
      )
  ;
  tuple_pattern state ~exn first

(* The argument of [exception], a constructor or a variant: a [pattern] which
   ends before [as], [|], [::] and [,], unless it is a labelled tuple. *)
and pattern_argument state error =
  match next state with
  | `LABEL _ | `TILDE -> labelled_tuple_pattern state ~exn:true
  | _ -> pattern_gen state ~exn:true error

and pattern_loop state ~exn minimum lhs =
  match next state with
  | `AS when minimum <= 2 ->
      let as_ = token state `AS in
      let name =
        val_ident
          state
          ( syntax_error
              "alias-name" "'as' in a pattern expects a lowercase identifier"
          )
      in
      pattern_loop state ~exn minimum (mk (Pat_alias (lhs, as_, name)))
  | `BAR when minimum <= 4 ->
      let bar = token state `BAR in
      let rhs =
        pattern_level
          state ~exn:true 5
          ( syntax_error
              "or-pattern-rhs" "'|' in a pattern expects another pattern"
          )
      in
      pattern_loop state ~exn minimum (mk (Pat_or (lhs, bar, rhs)))
  | `COMMA when minimum <= 6 ->
      pattern_loop
        state ~exn minimum (tuple_pattern state ~exn (Tuple_pattern lhs))
  | `LBRACKETAT when minimum <= 7 ->
      let attribute = attribute_with state `LBRACKETAT in
      pattern_loop state ~exn minimum (add_attributes lhs [ attribute ])
  | `COLONCOLON when minimum <= 8 ->
      let cons = token state `COLONCOLON in
      let rhs =
        pattern_level
          state ~exn:true 8
          ( syntax_error
              "cons-pattern-rhs" "'::' in a pattern expects a pattern"
          )
      in
      pattern_loop state ~exn minimum (mk (Pat_cons (lhs, cons, rhs)))
  | _ -> lhs

and tuple_pattern state ~exn first =
  let elements =
    non_empty_separated
      state `COMMA
      ( fun state ->
          tuple_pattern_element
            state ~exn
            ( syntax_error
                "tuple-pattern-element"
                "',' in a pattern expects another element of the tuple"
            )
      )
      first ~continue:(fun kind -> kind <> `DOTDOT)
  in
  let open_ =
    if is state `COMMA && peek state 1 = `DOTDOT then
      let comma = token state `COMMA in
      Some (comma, token state `DOTDOT)
    else None
  in
  mk (Pat_tuple (elements, open_))

and tuple_pattern_element state ~exn error =
  match next state with
  | `LABEL _ as kind ->
      let label = shift state kind in
      Tuple_pattern_label
        ( label,
          simple_pattern
            state
            ( syntax_error
                "label-pattern-value"
                "a label in a pattern expects a pattern, such as an identifier, '_', a literal or a \
                 pattern in parentheses"
            )
        )
  | `TILDE when peek state 1 = `LPAREN ->
      let tilde = token state `TILDE in
      Tuple_pattern_pun_constraint
        ( tilde,
          paren
            state ~unclosed:E.unclosed_punned_label
            ( fun state ->
                let name = lident state E.punned_label_name in
                let colon =
                  expect
                    state `COLON
                    (syntax_error "punned-pattern-colon" "':' expected")
                in
                name, colon, core_type state E.pattern_constraint_type
            )
        )
  | `TILDE ->
      let tilde = token state `TILDE in
      Tuple_pattern_pun (tilde, lident state E.tilde_label_name)
  | _ -> Tuple_pattern (pattern_level state ~exn 7 error)

(* Can a constructor argument start here? A sign must be followed by a
   number. *)
and constructor_argument_follows state =
  match next state with
  | `LAZY | `EXCEPTION | `EFFECT | `LABEL _ | `TILDE -> true
  | `MINUS | `PLUS ->
      ( match peek state 1 with
          `INT _ | `FLOAT _ -> true
        | _ -> false
      )
  | kind -> starts_simple_pattern kind

and pattern_gen state ~exn error =
  match next state with
  | `EXCEPTION when exn ->
      let exception_ = token state `EXCEPTION in
      let ext_attributes = ext_attributes state in
      mk
        ( Pat_exception
            ( exception_, ext_attributes,
              pattern_argument
                state
                ( syntax_error
                    "exception-pattern"
                    "'exception' in a pattern expects a pattern"
                )
            )
        )
  | `EFFECT when not exn ->
      raise
        ( failure
            state
            ( syntax_error
                "effect-pattern-position"
                "an effect pattern is only allowed in a case, or in parentheses, as in \
                 'let (effect e, k) = ...'"
            )
        )
  | `EFFECT ->
      let effect_ = token state `EFFECT in
      let first =
        pattern_gen
          state ~exn:false
          (syntax_error "effect-pattern" "'effect' expects a pattern")
      in
      let comma =
        expect state `COMMA (syntax_error "effect-comma" "',' expected")
      in
      mk
        ( Pat_effect
            ( effect_, first, comma,
              simple_pattern
                state
                ( syntax_error
                    "effect-continuation"
                    "',' after an effect expects a pattern"
                )
            )
        )
  | `LAZY ->
      let lazy_ = token state `LAZY in
      let ext_attributes = ext_attributes state in
      mk
        ( Pat_lazy
            ( lazy_, ext_attributes,
              simple_pattern
                state
                ( syntax_error
                    "lazy-pattern"
                    "'lazy' in a pattern expects a pattern, such as an identifier, '_', a literal or a \
                     pattern in parentheses"
                )
            )
        )
  | `BACKQUOTE ->
      let tag = name_tag state in
      if constructor_argument_follows state then
        mk (Pat_variant (tag, Some (pattern_argument state internal)))
      else mk (Pat_variant (tag, None))
  | _ ->
      let pattern = simple_pattern state error in
      match pattern with
      | { node = Pat_construct (constructor, None); attributes = [] } ->
          if is state `LPAREN && peek state 1 = `TYPE then begin
            let newtypes = paren state ~unclosed:E.unclosed_newtypes newtypes in
            mk
              ( Pat_construct_newtypes
                  ( constructor, newtypes,
                    simple_pattern
                      state
                      ( syntax_error
                          "constructor-newtypes-pattern"
                          "the locally abstract types of a constructor pattern expect a pattern"
                      )
                  )
              )
          end
          else if constructor_argument_follows state then
            mk
              ( Pat_construct
                  (constructor, Some (pattern_argument state internal))
              )
          else pattern
      | _ -> pattern

(* Is the pattern one which [simple_pattern] parses: an atom, without
   attributes, which may have a type in a binding ([let (p : t) = e]). A
   name, also an atom, is a binding of its own ([let_ident_at]). *)
and is_simple_pattern (pattern : pattern) =
  match pattern with
  | { attributes = _ :: _; _ } -> false
  | { node =
        ( Pat_any _ | Pat_interval _ | Pat_constant _ | Pat_open _
        | Pat_construct (_, None) | Pat_variant (_, None)
        | Pat_type _ | Pat_unpack _ | Pat_constraint _ | Pat_paren _
        | Pat_list _ | Pat_array _ | Pat_record _ | Pat_extension _
        )
    ;
      _
    }
  -> true
  | _ -> false

and simple_pattern state error =
  match next state with
  | `LIDENT _ as kind -> mk (Pat_var (Val_lident (shift state kind)))
  | `LPAREN when operator_at state 0 -> mk (Pat_var (val_ident state internal))
  | `UNDERSCORE -> mk (Pat_any (token state `UNDERSCORE))
  | _ when is_signed_constant_at state 0 ->
      let first = signed_constant state internal in
      if is state `DOTDOT then
        let dotdot = token state `DOTDOT in
        mk
          ( Pat_interval
              ( first, dotdot,
                signed_constant
                  state
                  ( syntax_error
                      "interval-bound"
                      "'..' in an interval pattern expects a literal, as in ''a'..'z''"
                  )
              )
          )
      else mk (Pat_constant first)
  | `UIDENT _ ->
      let path = module_path state ~apply:false internal in
      begin match next state, peek state 1 with
      | `DOT, (`LBRACE | `LBRACKETBAR | `LBRACKET) ->
          let dot = token state `DOT in
          mk (Pat_open (path, dot, simple_pattern state internal))
      | `DOT, `LPAREN when peek state 2 = `COLONCOLON && peek state 3 = `RPAREN
      ->
          let dot = token state `DOT in
          mk
            ( Pat_construct
                ((Some (path, dot), constr_ident state internal), None)
            )
      | `DOT, `LPAREN when peek state 2 = `RPAREN ->
          let dot = token state `DOT in
          let unit = (None, constr_ident state internal) in
          mk (Pat_open (path, dot, mk (Pat_construct (unit, None))))
      | `DOT, `LPAREN ->
          let dot = token state `DOT in
          let inner =
            paren
              state
              ~unclosed:
                (syntax_error "unclosed-local-open-pattern" "')' expected")
              ( fun state ->
                  pattern
                    state
                    ( syntax_error
                        "local-open-pattern"
                        "'M.(' in a pattern expects a pattern"
                    )
              )
          in
          mk (Pat_open (path, dot, mk (Pat_paren inner)))
      | `DOT, _ ->
          raise
            ( failure_at
                state (state.index + 1)
                ( syntax_error
                    "after-module-dot-pattern"
                    "a capitalized identifier, '(', '[', '[|' or '{' expected after 'M.'"
                )
            )
      | _ ->
          let qualifier, name = split_path state path in
          mk (Pat_construct ((qualifier, Constr_uident name), None))
      end
  | `TRUE as kind ->
      mk (Pat_construct ((None, Constr_true (shift state kind)), None))
  | `FALSE as kind ->
      mk (Pat_construct ((None, Constr_false (shift state kind)), None))
  | `BACKQUOTE -> mk (Pat_variant (name_tag state, None))
  | `HASH ->
      let hash = token state `HASH in
      mk
        ( Pat_type
            ( hash,
              type_longident state (syntax_error "pattern-type-name" "'#' in a pattern expects a lowercase identifier or a path, as in '#t' or '#M.t'")
            )
        )
  | `LPAREN ->
      begin match peek state 1, peek state 2 with
      | `RPAREN, _ | `COLONCOLON, `RPAREN ->
          mk (Pat_construct ((None, constr_ident state internal), None))
      | `MODULE, _ ->
          mk
            ( Pat_unpack
                ( paren
                    state ~unclosed:E.unclosed_pack
                    ( fun state ->
                        let module_ = token state `MODULE in
                        let ext_attributes = ext_attributes state in
                        let name =
                          module_name
                            state
                            ( syntax_error
                                "unpack-pattern-name"
                                "'(module' in a pattern expects a capitalized identifier or '_'"
                            )
                        in
                        let package =
                          preceded_option
                            state `COLON
                            (fun state -> package_module_type state E.pack_type)
                        in
                        (module_, ext_attributes, name, package)
                    )
                )
            )
      | _ ->
          let opening = state.index in
          let left = token state `LPAREN in
          let content = pattern state E.parenthesized_pattern in
          if is state `COLON then
            let colon = token state `COLON in
            let typ = core_type state E.pattern_constraint_type in
            let right =
              closing state ~opening `RPAREN E.unclosed_parenthesized_pattern
            in
            mk (Pat_constraint { left; content = (content, colon, typ); right })
          else
            let right =
              closing state ~opening `RPAREN E.unclosed_parenthesized_pattern
            in
            mk (Pat_paren { left; content; right })
      end
  | `LBRACKET when peek state 1 = `RBRACKET ->
      mk (Pat_construct ((None, constr_ident state internal), None))
  | `LBRACKET ->
      mk
        ( Pat_list
            ( delimited
                state `LBRACKET `RBRACKET
                ~unclosed:
                  (syntax_error "unclosed-list-pattern" "';' or ']' expected")
                ( semi_list
                    ( fun state ->
                        pattern
                          state
                          ( syntax_error
                              "list-pattern-element"
                              "a list pattern expects a pattern"
                          )
                    )
                    `RBRACKET
                )
            )
        )
  | `LBRACKETBAR ->
      mk
        ( Pat_array
            ( delimited
                state `LBRACKETBAR `BARRBRACKET
                ~unclosed:
                  (syntax_error "unclosed-array-pattern" "';' or '|]' expected")
                ( semi_list_or_empty
                    ( fun state ->
                        pattern
                          state
                          ( syntax_error
                              "array-pattern-element"
                              "an array pattern expects a pattern"
                          )
                    )
                    `BARRBRACKET
                )
            )
        )
  | `LBRACE -> record_pattern state
  | `LBRACKETPERCENT | `QUOTED_STRING_EXPR _ ->
      mk (Pat_extension (extension state))
  | _ -> raise (failure state error)

and record_pattern state =
  let field state =
    let label =
      label_longident
        state
        ( syntax_error
            "record-pattern-field"
            "a record pattern expects a lowercase identifier, a path or '_'"
        )
    in
    let typ =
      preceded_option
        state `COLON (fun state -> core_type state E.pattern_constraint_type)
    in
    ( label, typ,
      preceded_option
        state `EQUAL
        ( fun state ->
            pattern
              state
              ( syntax_error
                  "record-pattern-value"
                  "'=' in a record pattern expects a pattern"
              )
        )
    )
  in
  mk
    ( Pat_record
        ( delimited
            state `LBRACE `RBRACE
            ~unclosed:
              (syntax_error "unclosed-record-pattern" "';' or '}' expected")
            ( fun state ->
                let fields =
                  separated_or_terminated
                    state field
                    ~stop:(fun kind -> kind = `RBRACE || kind = `UNDERSCORE)
                in
                (* [_] closes the fields after a [;]. *)
                let wildcard =
                  if is state `UNDERSCORE && snd fields <> None then
                    let underscore = token state `UNDERSCORE in
                    Some (underscore, optional state `SEMI)
                  else None
                in
                (fields, wildcard)
            )
        )
    )

(* {1 Entry points} *)

let split_eof tokens =
  match List.rev tokens with
  | { Token.kind = `EOF; leading } :: tokens ->
      (List.rev tokens, ({ Token.leading; kind = `EOF } : [ `EOF ] tok))
  | _ -> invalid_arg "Parser: the token stream does not end with EOF"

let make_state ~filename raw =
  let tokens, docs = Doc_comments.split raw in
  let tokens, eof = split_eof tokens in
  { tokens = Array.of_list tokens; eof = (eof :> Token.any_token);
    docs; raw; filename; index = 0; spans = None; stopped = None
  }
  ,
  eof

(* [EOF], with the doc comments before it which document nothing. *)
let eof_token state (eof : [ `EOF ] tok) : [ `EOF ] tok =
  { eof with
    leading = Doc_comments.remaining state.docs state.index @ eof.leading
  }

(* The first lexical error. An unterminated comment or string is located at
   its opening delimiter, not up to the end of the input. *)
let check_errors state =
  Array.iteri
    ( fun index (token : Token.any_token) ->
        match token.kind with
        | `ERROR error ->
            let { id; message } = E.lexer error.Token.reason error.text in
            let location = location state index in
            let opening =
              match error.reason with
              | `Unterminated_comment -> Some 2
              | `Unterminated_string -> Some 1
              | `Unterminated_quoted_string ->
                  String.index_opt error.text '|' |> Option.map succ
              | _ -> None
            in
            let location =
              match opening with
              | Some length ->
                  let start = location.loc_start in
                  { location with
                    loc_end = { start with pos_cnum = start.pos_cnum + length }
                  }
              | None -> location
            in
            raise (Syntax_error.Error { id; location; message; notes = [] })
        | _ -> ()
    )
    state.tokens

let implementation_tokens ?(filename = "") tokens : Cst.implementation =
  let state, eof = make_state ~filename tokens in
  check_errors state;
  let items = structure state in
  if state.index <> Array.length state.tokens then
    raise (failure state E.structure_item)
  ;
  (items, eof_token state eof)

let interface_tokens ?(filename = "") tokens : Cst.interface =
  let state, eof = make_state ~filename tokens in
  check_errors state;
  let items = signature state in
  if state.index <> Array.length state.tokens then
    raise (failure state E.signature_item)
  ;
  (items, eof_token state eof)

(* A token stream has no [EOF]. *)
let stream_tokens stream = Token_stream.tokens stream @ [ Token.make `EOF ]

let implementation ?filename stream =
  implementation_tokens ?filename (stream_tokens stream)

let interface ?filename stream =
  interface_tokens ?filename (stream_tokens stream)
