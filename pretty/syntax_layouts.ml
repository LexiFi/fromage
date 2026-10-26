(* The layouts of the constructs shared by several parts of the grammar: see
   syntax_layouts.mli. *)

open Ocst

type safe_kind =
  [ `EOF | `AND | `ASSERT | `BEGIN | `CLASS | `CONSTRAINT | `DO | `DONE
  | `DOWNTO | `EFFECT | `ELSE | `END | `EXCEPTION | `EXTERNAL | `FALSE
  | `FOR | `FUN | `FUNCTION | `FUNCTOR | `IF | `IN | `INCLUDE | `INHERIT
  | `INITIALIZER | `LAZY | `LET | `MATCH | `METHOD | `MODULE | `MUTABLE | `NEW
  | `NONREC | `OBJECT | `OF | `OPEN | `OR | `PRIVATE | `REC | `SIG | `STRUCT
  | `THEN | `TO | `TRUE | `TRY | `TYPE | `VAL | `VIRTUAL | `WHEN | `WHILE
  | `WITH | `UNDERSCORE | `QUOTE | `BACKQUOTE | `LIDENT of Token.Lident.t
  | `UIDENT of Token.Uident.t | `LABEL of Token.Lident.t
  | `OPTLABEL of Token.Lident.t | `INT of Token.integer_literal
  | `FLOAT of Token.float_literal | `STRING of Token.string_literal
  | `CHAR of Token.char_literal | `QUOTED_STRING_EXPR of Token.quoted_extension
  | `QUOTED_STRING_ITEM of Token.quoted_extension | `LPAREN | `RPAREN
  | `LBRACE | `RBRACE | `LBRACKET | `RBRACKET | `LBRACKETBAR
  | `BARRBRACKET | `LBRACKETLESS | `LBRACKETGREATER | `LBRACELESS
  | `GREATERRBRACE | `GREATERRBRACKET | `LBRACKETAT | `LBRACKETATAT
  | `LBRACKETATATAT | `LBRACKETPERCENT | `LBRACKETPERCENTPERCENT | `COMMA
  | `SEMI | `SEMISEMI | `BAR | `BARBAR | `COLON | `COLONCOLON | `DOT
  | `DOTDOT | `COLONEQUAL | `LESS | `GREATER | `AMPERSAND | `AMPERAMPER
  | `PLUS | `PLUSDOT | `PLUSEQ | `MINUS | `MINUSDOT | `PERCENT | `HASH
  | `BANG | `TILDE | `QUESTION | `STAR | `PREFIXOP of Token.Prefixop.t
  | `HASHOP of Token.Hashop.t | `INFIXOP0 of Token.Infixop0.t
  | `INFIXOP1 of Token.Infixop1.t | `INFIXOP2 of Token.Infixop2.t
  | `INFIXOP3 of Token.Infixop3.t | `INFIXOP4 of Token.Infixop4.t
  | `DOTOP of Token.Dotop.t | `LETOP of Token.Letop.t
  | `ANDOP of Token.Andop.t | `DOCSTRING of string | `ERROR of Token.error
  ]

type operator_kind =
  [ `PREFIXOP of Token.Prefixop.t | `LETOP of Token.Letop.t
  | `ANDOP of Token.Andop.t | `HASHOP of Token.Hashop.t | `BANG
  | `INFIXOP0 of Token.Infixop0.t | `INFIXOP1 of Token.Infixop1.t
  | `INFIXOP2 of Token.Infixop2.t | `INFIXOP3 of Token.Infixop3.t
  | `INFIXOP4 of Token.Infixop4.t | `PLUS | `PLUSDOT | `PLUSEQ | `MINUS
  | `MINUSDOT | `STAR | `PERCENT | `EQUAL | `LESS | `GREATER | `OR | `BARBAR
  | `AMPERSAND | `AMPERAMPER | `COLONEQUAL | `LESSMINUS | `COLONCOLON
  ]

module Make (D : Doc.S) =
  struct
    open D

    let tok (value : [< safe_kind ] Token.token) = D.tok value
    let paren = D.paren
    let separated_after = D.separated_after
    let separated_before = D.separated_before

    (* {1 Operators} *)

    let operator_name operator = D.tok operator

    let operator_before operator operand = after_line (D.tok operator) operand
    let arrow_type first rest =
      let after_arrow (~arrow, ~typ) = after_line (D.tok arrow) typ in
      match List.rev rest with
      | result :: domains ->
          let domains = first :: List.rev_map after_arrow domains
          and result = after_arrow result
          in
          flat (words domains +- result)
          <|> (pack_list_isolating_multiline ~indent:0 domains +| result)
      | [] -> first

    let operator_chain values = pack_list_isolating_multiline ~indent:0 values

    (* {1 Type constraints} *)

    (** A type after a token of one character, [: t] or [= t]: on the line
        of the token, its next lines indented by 2. *)
    let after_short_token (token, typ) = D.tok token +- nest 2 typ

    (** A type after [:>]: on the line of [:>] when it is on one line, else
        on the next lines, indented by 2. *)
    let after_coercion (coerce, typ) = after (D.tok coerce) typ

    let constraint_parts ?constraint_ ?coercion () =
      List.filter_map
        Fun.id
        [ Option.map after_short_token constraint_;
          Option.map after_coercion coercion
        ]

    let constrained ?constraint_ ?coercion value =
      let parts = constraint_parts ?constraint_ ?coercion () in
      if parts = [] then value
      else flat (value +- words parts) <|> (value +| vertical parts)

    let constrained_paren ?constraint_ ?coercion ~left value ~right =
      let left = D.tok left
      and right = D.tok right
      in
      let parts = constraint_parts ?constraint_ ?coercion () in
      if parts = [] then bracket left value right
      else
        (* The parts in the column of the brackets, which belong to the
           constraint. *)
        flat (left ++ value +- words parts ++ right)
        <|> (left +- nest 2 value +| vertical parts +| right)

    (* {1 Let-like bindings} *)

    let separator_on_its_own_line ~parts_indent ~indent head separator body =
      below ~indent (below ~indent:parts_indent head separator) body

    let binding_head
        ?keyword ?(params = []) ?(parts_indent = 2)
        ?constraint_ ?coercion ?equation head
      =
        (* [head params]: on one line, or the parameters packed on the next
           lines, indented by 4 from the line where [head] starts. *)
        let with_params head =
          if params = [] then head else pack_after_head ~indent:4 head params
        in
        (* [keyword head]: on one line, or [head] on the lines after
           [keyword], indented by 2. *)
        let head =
          match keyword with
          | None -> with_params head
          | Some keyword ->
              with_params (flat (keyword +- head))
              <|> losing_ties (keyword +/ with_params head)
        in
        (* [: t], [:> u] and [= v]: all on one line, or each starting its
           line. *)
        let parts =
          List.filter_map
            Fun.id
            [ Option.map after_short_token constraint_;
              Option.map after_coercion coercion;
              Option.map after_short_token equation
            ]
        in
        let parts = flat (words parts) <|> vertical parts in
        (* [head parts]: on one line, or the parts starting the next lines,
           indented by 2. *)
        if is_empty parts then head
        else flat (head +- parts) <|> below ~indent:parts_indent head parts

    let binding
        ?(indent = 2) ?keyword ?words:(words = empty) ?params
        ?(parts_indent = 2) ?constraint_ ?coercion ?equation ~head equal ~body
      =
        let head =
          binding_head
            ?keyword ?params ~parts_indent ?constraint_ ?coercion ?equation head
        in
        let separator = D.tok equal in
        let body =
          if is_empty words then body
          else
            (* [words] before [body] on one line, or alone on its line, in
               the column of [body]. *)
            flat (words +- body) <|> (words +| body)
        in
        (* [inline]: not after comments on lines of their own before [=]. *)
        after (inline (flat head) separator) body
        <|>
        losing_ties (below ~indent:parts_indent head (flat (separator +- body)))
        <|>
        losing_ties
          (separator_on_its_own_line ~parts_indent ~indent head separator body)

    (* {1 For loops} *)

    let for_range ~index equal ~start ~direction ~stop =
      let equal = D.tok equal in
      let stop = after direction stop in
      flat (index +- equal +- start +- stop)
      <|> (flat (index +- equal +- start) +| stop)
      <|> (index +| (equal +- nest 2 start) +| stop)

    (* {1 Aliases} *)

    let alias value as_ name =
      let alias = D.tok as_ +- name in
      (* After a value on several lines, [as] starts the next line. *)
      flat (value +- alias) <|> losing_ties (value +| alias)

    (* {1 Match-like cases} *)

    type case = head:t * arrow:[ `MINUSGREATER ] Token.token option * body:t

    (** A case after its bar, at a level indented by 2 from it ([bar_list]):
      [->] after a head on several lines is 2 columns before it. *)
    let case_after_bar ((~head, ~arrow, ~body) : case) =
      match arrow with
      | None -> head
      | Some arrow ->
          let arrow = D.tok arrow in
          let with_body head = inline head (flat body) <|> (head +/ body) in
          let arrow_line =
            inline arrow (flat body) <|> below ~indent:4 arrow body
          in
          with_body (flat (head +- arrow))
          <|> as_last_resort (below ~indent:(-2) head arrow_line)

    let guarded head guard =
      match guard with
      | None -> head
      | Some (when_, guard) ->
          let guard = after (D.tok when_) guard in
          (* After a head on several lines, [when] starts the next line. *)
          (flat head +- guard) <|> losing_ties (head +| guard)

    let cases ?doc print values =
      bar_list ?doc (fun value -> case_after_bar (print value)) values

    let case_without_bar ((~head, ~arrow, ~body) : case) =
      match arrow with
      | None -> head
      | Some arrow ->
          let arrow = D.tok arrow in
          (* After a head on several lines, [->] starts the next line. *)
          after (flat head +- arrow) body
          <|> losing_ties (head +/ after arrow body)

    (* {1 Fun-like functions} *)

    let fun_like ~head ~params arrow ~body =
      let arrow = D.tok arrow in
      let head =
        flat (head +- words params +- arrow)
        (* Only for parameters on several lines. *)
        <|> losing_ties (pack_after_head ~indent:2 head params +| arrow)
      in
      after head body
  end
