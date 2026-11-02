(** The layouts of the constructs which several parts of the grammar share,
    between [Doc] and [Ocst_pretty]: the let-like bindings ([x = e]), the
    match-like cases ([p -> e]), the fun-like functions ([fun x -> e]), the
    chains of operators (with the assignments, [r := v], [r.x <- v]), the type
    constraints ([(e : t :> u)]) and the aliases ([p as x]). Their tokens,
    [=], [->], [<-], [:>] and [as], can only be printed by these layouts: the
    [tok] of this module, which shadows the one of [Doc] in [Ocst_pretty],
    refuses them, so that each construct which prints them shares its layout
    with the others of its kind. *)

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
(** The tokens which [tok] prints: all of them but [=], [->], [<-], [:>]
    and [as]. *)

type operator_kind =
  [ `PREFIXOP of Token.Prefixop.t | `LETOP of Token.Letop.t
  | `ANDOP of Token.Andop.t | `HASHOP of Token.Hashop.t | `BANG
  | `INFIXOP0 of Token.Infixop0.t | `INFIXOP1 of Token.Infixop1.t
  | `INFIXOP2 of Token.Infixop2.t | `INFIXOP3 of Token.Infixop3.t
  | `INFIXOP4 of Token.Infixop4.t | `PLUS | `PLUSDOT | `PLUSEQ | `MINUS
  | `MINUSDOT | `STAR | `PERCENT | `EQUAL | `LESS | `GREATER | `OR | `BARBAR
  | `AMPERSAND | `AMPERAMPER | `COLONEQUAL | `LESSMINUS | `COLONCOLON
  ]
(** The operators of the chains of [operator_before]: those of
    [Cst.operator_token] and [::]. The arrows of types are [arrow_type]'s. *)

module Make (D : Doc.S)
  : sig
      open D

      (** {1 Tokens} *)

      val tok : [< safe_kind ] Token.token -> t
      (** [Doc.tok], for the tokens which no shared construct prints. *)

      val paren :
        ?spaces:bool -> ?inline:bool -> ('a -> t)
        -> ([< safe_kind ], 'a, [< safe_kind ]) Cst.delimited
        -> t
      (** [Doc.paren], with brackets which are not [=] or [->]. *)

      val separated_after :
        ?in_bracket:bool -> ('a -> t)
        -> ('a, [< safe_kind ]) Cst.non_empty_separated_list
        -> t list
      (** [Doc.separated_after], with separators which are not [=] or [->]. *)

      val separated_before :
        ('a -> t) -> ('a, [< safe_kind ]) Cst.non_empty_separated_list
        -> t list
      (** [Doc.separated_before], with separators which are not [=] or [->]. *)

      (** {1 Operators}

      In the examples, a part on several lines is shown on two of its lines:
      the first one ends with [...], and the next one is in the column of the
      first one. The other lines of a part keep its own layout (an application
      indents its arguments by 2), which these layouts do not change: they
      place the first line of each part, and the separators between them. *)

      val operator_name : Cst.operator_token -> t
      (** An operator as the name of a value: [=] in [( = )]. *)

      val operator_chain :
        ?in_bracket:bool -> Operator_classification.t -> t
        -> ([< operator_kind ] Token.token * t) list
        -> t
      (** [operator_chain kind first rest]: a chain of binary operators of
      [kind] and of the same precedence, nested in one another, as
      [a + b - c] ([(a + b) - c]) or [a :: b :: l] ([a :: (b :: l)]): their
      operands in the order of the source, [first], then each one after the
      operator before it ([+ b], [- c]). With all the operands on one line,
      packed at the same indentation:
      {[
        a + b + c
        + d + e
      ]}
      Else each operator starts its line, alone before an operand on several
      lines, and the operands are indented from the operators by 2, for a
      [Symmetric] operator:
      {[
            a ...
            a
          +
            b ...
            b
          + c
      ]}
      only the first one for [Left_into_right]:
      {[
            a ...
            a
          |>
          b ...
          b
          |> c
      ]}
      and all of them but the first one for [Right_into_left]:
      {[
          a
          :=
            b ...
            b
      ]}
      where, after an operand on one line, the operator can also end its
      line, each next operand indented by 2 from the one before it, which is
      chosen over the other layouts of the same number of lines:
      {[
          a :=
            b ...
            b
      ]}
      {[
          a @@
            b @@
              c ...
              c
      ]}
      [in_bracket]: the chain starts where the content of a bracket starts,
      as in [( a + b )] or [\[ a + b; c \]], where the first operand cannot be
      indented: the operators are in the column of the bracket instead.
      {[
        ( a ...
          a
        +
          b ...
          b
        )
      ]} *)

      val arrow_type :
        t -> (arrow:[ `MINUSGREATER ] Token.token * typ:t) list -> t
      (** [arrow_type first rest]: an arrow type ([a -> b -> c]), of core types,
      class types or module types: its first type, then each arrow and the
      type after it, the last one being the result. On one line, or the
      domains packed, each after its arrow, and the result on a line of its
      own:
      {[
        a -> b -> c
        -> d -> e
        -> result
      ]}
      A domain or a result on several lines has a line break before it and
      after it, its arrow alone on its line:
      {[
        a
        -> b
        ->
        ( domain ...
          domain
        )
        -> c
        ->
        result ...
        result
      ]} *)

      (** {1 Type constraints} *)

      val constrained :
        ?constraint_:([ `COLON ] Token.token * t)
        -> ?coercion:([ `COLONGREATER ] Token.token * t) -> t
        -> t
      (** [constrained ~constraint_ ~coercion value]: the fields of records
      without a value, [{ x : t }], [{ x :> t }]. All on one line, or
      each part starting a line, in the column of [value]. A type after [:],
      which is one character, is on its line, its next lines indented by 2; a
      type after [:>] is on its line only when it is on one line, else on
      the next lines, indented by 2:
      {[
        value : type :> type
      ]}
      {[
        value ...
        value
        : type ...
          type
        :>
          type ...
          type
      ]} *)

      val constrained_paren :
        ?constraint_:([ `COLON ] Token.token * t)
        -> ?coercion:([ `COLONGREATER ] Token.token * t)
        -> left:[< safe_kind ] Token.token -> t
        -> right:[< safe_kind ] Token.token
        -> t
      (** [constrained_paren ~constraint_ ~coercion ~left value ~right]: a
      constraint whose parentheses belong to it, [(e : t :> u)], [(p : t)],
      [(c : ct)], [(M : S)], [(val e : S :> T)], [(module M : S)]. On one
      line, or [value] after [left], and each part starting a line, in the
      column of the brackets, its type as in [constrained]:
      {[
        (value : type :> type)
      ]}
      {[
        ( value ...
          value
        : type ...
          type
        :>
          type ...
          type
        )
      ]} *)

      (** {1 Let-like bindings} *)

      val binding_head :
        ?keyword:t -> ?params:t list -> ?parts_indent:int
        -> ?constraint_:([ `COLON ] Token.token * t)
        -> ?coercion:([ `COLONGREATER ] Token.token * t)
        -> ?equation:([< `EQUAL | `COLONEQUAL ] Token.token * t) -> t
        -> t
      (** The head of [binding], alone: an abstract type ([type t]), or a class
      description ([class c : t], where [t] is the constraint). *)

      val binding :
        ?indent:int -> ?keyword:t -> ?words:t -> ?params:t list
        -> ?parts_indent:int -> ?constraint_:([ `COLON ] Token.token * t)
        -> ?coercion:([ `COLONGREATER ] Token.token * t)
        -> ?equation:([< `EQUAL | `COLONEQUAL ] Token.token * t)
        -> head:t -> [< `EQUAL | `COLONEQUAL ] Token.token -> body:t
        -> t
      (** [binding ~keyword ~head ~params ~constraint_ ~coercion ~equation equal
      ~words ~body]: the let-like constructs, [x = e], [let x : t = e],
      [type t = u = private A | B], [M := N], [{ x = e }], [f x y : t = e],
      [class c x = e], [module F (X : S) = M].

      [keyword] ([let], [and], [type], [class], [module]) and [head] are on
      one line, or [head] is on the lines after [keyword], indented by 2:
      {[
        let
          ( pattern ...
            pattern
          )
          : type
          = body
      ]}
      [head] and [params] are on one line, or the parameters are packed on
      the lines after [head], indented by 4 from the line where [head]
      starts, a parameter on several lines alone on its lines: by 6 from
      [keyword] after a [head] on the lines after it.
      {[
        class
          [ 'a ...
            'b
          ] c
              params params
          = body
      ]}
      They are followed by [constraint_] ([: t]), [coercion] ([:> u]) and
      [equation] ([= v] in [type t = v = A | B]), each its token and a type
      (the token printed here): all on one line,
      or each starting the next line, indented by 2, its type after its token
      as in [constrained] (a type on several lines after [:>] is on the next
      lines):
      {[
        head
            params params
            ( param ...
              param
            )
            params
          : type ...
            type
          :>
            type ...
            type
          = body
      ]}
      {[
        type t
          = ( type ...
              type
            ) u
          = A | B
      ]}
      This head is followed by [=], [words] ([private]) and [body]. A [head]
      on one line is followed by [=], and by [words] and [body] on the same
      line or on the next lines, indented by 2:
      {[
        head = words body
      ]}
      {[
        head =
          body ...
          body
      ]}
      [words] are on the line of [body], or alone on their line, with [body]
      in their column:
      {[
        head =
          words
          body ...
          body
      ]}
      A [head] on several lines is never followed by [=]: [=] starts the
      next line, indented by 2, followed by [body] when it fits on the line,
      else alone on its line, with [body] on the next lines, indented by
      [indent] (2 by default, 4 for functions, methods and classes):
      {[
        head ...
        head
          = body
      ]}
      {[
        head ...
        head
          =
          body ...
          body
      ]}
      A block ([struct ... end]) is as any [body]: on the line of [=] only
      when it is on one line.

      [parts_indent] (2 by default) is the indentation of the parts and of
      [=] when they start a line: 0 for the fields of records, inside their
      braces:
      {[
        { field
          : type ...
            type
          = body
        }
      ]} *)

      (** {1 For loops} *)

      val for_head :
        keyword:t -> index:t -> [ `EQUAL ] Token.token -> start:t
        -> direction:t -> stop:t -> [ `DO ] Token.token
        -> t
      (** [for i = a to b do], after [keyword] ([for], with its extension and
          attributes):
      {[
        for index = start to stop do
      ]}
      Else [for index =] on its line, [start] after it or below it, and
      [to stop] on the next line:
      {[
        for index = start
          to
            stop ...
            stop
        do
      ]}
      {[
        for index =
          start ...
          start
          to stop
        do
      ]}
      Only when [for index =] cannot be on one line (after comments on lines
      of their own), the range on the lines after [for], each part starting
      its line when [start] is on several lines:
      {[
        for
          index
          = start ...
            start
          to stop
        do
      ]} *)

      (** {1 Aliases} *)

      val alias : t -> [ `AS ] Token.token -> t -> t
      (** [alias value as_ name]: [p as x], [t as 'a], [inherit c as x]:
      {[
        value as name
      ]}
      After a [value] on several lines, [as] starts the next line, in the
      column of [value]:
      {[
        value ...
        value
        as name
      ]} *)

      (** {1 Match-like cases} *)

      type case = head:t * arrow:[ `MINUSGREATER ] Token.token option * body:t
      (** A case [head -> body], or [head] alone when [arrow] is [None] ([body]
      is then [empty]): a case of [match], or a constructor of a variant
      type, which has [->] in a GADT. *)

      val guarded : t -> ([ `WHEN ] Token.token * t) option -> t
      (** [guarded head guard]: a pattern and its guard, [p when e], as the head
      of a case:
      {[
        head when guard
      ]}
      {[
        head when
          guard ...
          guard
      ]}
      After a [head] on several lines, [when] starts the next line, in the
      column of the head, the guard after it or on the next lines, indented by
      2:
      {[
        head ...
        head
        when guard
      ]} *)

      val cases :
        ?doc:('a -> Cst.docstring option) -> ('a -> case) -> 'a Cst.bar_list
        -> t list
      (** The cases of [match], [function] and [try], or the constructors of a
      variant type, each after its bar ([Doc.bar_list]):
      {[
        | head -> body
      ]}
      {[
        | head ->
            body ...
            body
      ]}
      A [head] on several lines is ended by [->], in the column of its bar,
      followed by [body], or by [body] on the next lines, indented by 4:
      {[
        | head ...
          head
        -> body
      ]}
      {[
        | head ...
          head
        ->
            body ...
            body
      ]} *)

      val case_without_bar : case -> t
      (** A case without a bar, as in an exception [E : a -> exn]:
      {[
        head -> body
      ]}
      {[
        head ->
          body ...
          body
      ]}
      A [head] on several lines is ended by [->] on the next line, indented
      by 2:
      {[
        head ...
        head
          -> body
      ]} *)

      (** {1 Fun-like functions} *)

      val fun_like :
        head:t -> params:t list -> [ `MINUSGREATER ] Token.token -> body:t -> t
      (** [fun x y -> e], [functor (X : S) -> M], [fun x -> object ... end]:
      {[
        head params -> body
      ]}
      {[
        head params ->
          body ...
          body
      ]}
      Parameters which do not fit on the line of [head] are packed on the next
      lines, indented by 2 (a parameter on several lines alone on its lines),
      and [->] starts the next line, in the column of [head]:
      {[
        head
          params params
          ( param ...
            param
          )
        -> body
      ]}
      {[
        head
          params params
        ->
          body ...
          body
      ]} *)
    end
