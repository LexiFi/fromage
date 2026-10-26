(** Parser-independent, lossless OCaml concrete syntax tree.
    Nodes hold syntax tokens, never locations.

    The tree follows the productions of the OCaml grammar
    (ocaml/parsing/parser.mly): each node keeps all its tokens, in source
    order, with the most specific token type. *)

type 'kind tok = 'kind Token.token

(** [x] *)
type lident = [ `LIDENT of Token.Lident.t ] tok

(** [M] *)
type uident = [ `UIDENT of Token.Uident.t ] tok

(** [x] or [M] *)
type ident_token = [ `LIDENT of Token.Lident.t | `UIDENT of Token.Uident.t ] tok

(** ["s"] or [{|s|}]*)
type string_token = [ `STRING of Token.string_literal ] tok

type ('left, 'a, 'right) delimited =
  { left : 'left tok; content : 'a; right : 'right tok; }

type ('a, 'separator) non_empty_separated_list =
  { first : 'a; rest : ('separator tok * 'a) list }

type ('a, 'separator) separated_list =
  | Zero (** [] *)
  | Many of ('a, 'separator) non_empty_separated_list (** [x, y, ...] *)

(** [x; y; ...] or [x; y; ...;] *)
type 'a semis = ('a, [ `SEMI ]) non_empty_separated_list * [ `SEMI ] tok option

(** [x, y, ...] *)
type 'a commas = ('a, [ `COMMA ]) separated_list

(** [x | y | ...] *)
type 'a bars = ('a, [ `BAR ]) separated_list

(** [x * y * ...] *)
type 'a stars = ('a, [ `STAR ]) separated_list

(** [(x)] *)
type 'a paren = ([ `LPAREN ], 'a, [ `RPAREN ]) delimited

(** [x | y | ...] or [ | x | y | ...] *)
type 'a bar_list = [ `BAR ] tok option * ('a, [ `BAR ]) non_empty_separated_list

(** [()...)], [{...}] or [[...]]. Used for custom index operators. *)
type 'a brackets =
  | Parens of 'a paren (** [(x)] *)
  | Brackets of ([ `LBRACKET ], 'a, [ `RBRACKET ]) delimited (** [[x]] *)
  | Braces of ([ `LBRACE ], 'a, [ `RBRACE ]) delimited (** [{x}] *)

(** {1 Doc comments}

    A doc comment is in the CST when it documents something: an item, a
    constructor or a field, or as floating text between items. Otherwise, it
    is a warning in the trivia of the next token ({!Token.Warning}), as for
    the compiler (warning 50). *)

(** [(** doc *)] *)
type docstring = [ `DOCSTRING of string ] tok

(** A doc comment between items, which documents none of them alone. *)
type floating =
  | Text of docstring
  (** [(** text *)], separated from the items around it by blank lines, or at
      the start or the end of a list of items *)
  | Ambiguous of docstring
  (** [x (** doc *) y]: after an item and before another one, without blank
      lines; it documents both, and the compiler warns about it *)

(** [(** doc *) x (** doc *)], or [x] *)
type 'a documented = docstring option * 'a * docstring option

type 'a element =
  | Item of 'a documented (** [(** doc *) x (** doc *)] *)
  | Floating of floating (** [(** text *)] *)

(** The doc comments before [and], in a group of items: the doc comment of
    the previous item, floating doc comments, and the doc comment of the next
    item, as in [x (** doc *) (** text *) (** doc *) and y]. *)
type and_docs = docstring option * floating list * docstring option

(** {1 Identifiers} *)

type module_path =
  | Path_ident of uident (** [M] *)
  | Path_dot of module_path * [ `DOT ] tok * uident (** [M.N] *)
  | Path_apply of module_path * module_path paren (** [F(X)] *)

(** [x] or [M.x] *)
type 'name longident = (module_path * [ `DOT ] tok) option * 'name

(** [+], [!], [>>=] *)
type operator_token =
  [ `PREFIXOP of Token.Prefixop.t | `LETOP of Token.Letop.t
  | `ANDOP of Token.Andop.t | `HASHOP of Token.Hashop.t | `BANG
  | `INFIXOP0 of Token.Infixop0.t | `INFIXOP1 of Token.Infixop1.t
  | `INFIXOP2 of Token.Infixop2.t | `INFIXOP3 of Token.Infixop3.t
  | `INFIXOP4 of Token.Infixop4.t | `PLUS | `PLUSDOT | `PLUSEQ
  | `MINUS | `MINUSDOT | `STAR | `PERCENT | `EQUAL | `LESS | `GREATER
  | `OR | `BARBAR | `AMPERSAND | `AMPERAMPER | `COLONEQUAL
  ] tok

type operator =
  | Operator of operator_token (** [( + )] *)
  | Index_operator of
      [ `DOTOP of Token.Dotop.t ] tok
      * ([ `SEMI ] tok * [ `DOTDOT ] tok) option brackets
      * [ `LESSMINUS ] tok option (** [( .%() )] or [( .%(;..)<- )] *)

type val_ident =
  | Val_lident of lident (** [x] *)
  | Val_operator of operator paren (** [( + )] *)

type constr_ident =
  | Constr_uident of uident (** [C] *)
  | Constr_cons of [ `COLONCOLON ] tok paren (** [(::)] *)
  | Constr_nil of ([ `LBRACKET ], unit, [ `RBRACKET ]) delimited (** [[]] *)
  | Constr_unit of unit paren (** [()] *)
  | Constr_false of [ `FALSE ] tok (** [false] *)
  | Constr_true of [ `TRUE ] tok (** [true] *)

(** [a] or [M] or a keyword, in [[@a.M.open]] *)
type attr_id_token =
  [ `LIDENT of Token.Lident.t | `UIDENT of Token.Uident.t | Token.keyword ] tok

(** [a.b. ...] *)
type attr_id = (attr_id_token, [ `DOT ]) non_empty_separated_list

(** ['a] *)
type type_variable = [ `QUOTE ] tok * ident_token

(** [`A] *)
type name_tag = [ `BACKQUOTE ] tok * ident_token

(** [1], [1.], ["s"] or ['c'] *)
type constant =
  [ `INT of Token.integer_literal | `FLOAT of Token.float_literal
  | `STRING of Token.string_literal | `CHAR of Token.char_literal
  ] tok

(** [1] or [-1] *)
type signed_constant =
  | Signed of
      [ `MINUS | `PLUS ] tok
      * [ `INT of Token.integer_literal | `FLOAT of Token.float_literal ] tok
  | Unsigned of constant

type rec_flag = [ `REC ] tok option (** [rec] *)

(** [to] or [downto] *)
type direction = [ `TO | `DOWNTO ] tok

type arrow_label =
  | Nolabel (** [t -> u] *)
  | Labelled of (lident * [ `COLON ] tok) (** [x:t -> u] *)
  | Optional of [ `OPTLABEL of Token.Lident.t ] tok (** [?x -> u] *)
  | Optional_split of [ `QUESTION ] tok * lident * [ `COLON ] tok (** [?x:t -> u] *)

(** [+], [-], [!], [+!] or [-!] *)
type variance_token =
  [ `PLUS | `MINUS | `BANG | `INFIXOP2 of Token.Infixop2.t
  | `PREFIXOP of Token.Prefixop.t
  ] tok

(** [+'a] or [_] *)
type type_parameter =
  variance_token list * [ `Var of type_variable | `Any of [ `UNDERSCORE ] tok ]

type type_parameters =
  | No_type_parameters (** [t] *)
  | One_type_parameter of type_parameter (** ['a t] *)
  | Many_type_parameters of type_parameter commas paren (** [('a, 'b, ...) t] *)

type module_name =
  | Module_name of uident (** [M] *)
  | Module_anonymous of [ `UNDERSCORE ] tok (** [_] *)

(** {1 The main syntactic categories} *)

(** [node []] *)
type 'a with_attributes = { node : 'a; attributes : attribute list }

(** [[@attr]], [[@@attr]] or [[@@@attr]] *)
and attribute =
  ( [ `LBRACKETAT | `LBRACKETATAT | `LBRACKETATATAT ],
    attribute_contents, [ `RBRACKET ]
  ) delimited

(** The identifier and the payload of an attribute or an extension. *)
and attribute_contents = attr_id * payload
and extension =
  | Extension of
      ( [ `LBRACKETPERCENT | `LBRACKETPERCENTPERCENT ],
        attribute_contents, [ `RBRACKET ]
      ) delimited (** [[%ext]] *)
  | Quoted_extension of
      [ `QUOTED_STRING_EXPR of Token.quoted_extension
      | `QUOTED_STRING_ITEM of Token.quoted_extension
      ] tok (** [{%ext|...|}] *)
and payload =
  | Payload_structure of structure (** [[%ext let x = e]] *)
  | Payload_signature of [ `COLON ] tok * signature (** [[%ext : sig end]] *)
  | Payload_type of [ `COLON ] tok * core_type (** [[%ext : t]] *)
  | Payload_pattern of
      [ `QUESTION ] tok * pattern * ([ `WHEN ] tok * expr) option
  (** [[%ext? p]] or [[%ext? p when e]] *)

(** nothing or [%ext [@a] [@b] ...], after a keyword *)
and ext_attributes = ([ `PERCENT ] tok * attr_id) option * attribute list

(** [x and [@attr] y and ...]: the items after the first one have attributes after
    [and] ([and_let_binding], [and_type_declaration]...), and the doc comments
    before [and]. The doc comments before the first item and after the last
    one belong to the enclosing item. *)
and 'a and_list =
  'a * (docstring:and_docs * [ `AND ] tok * attribute list * 'a) list

(** [label domain -> codomain] *)
and ('domain, 'codomain) arrow =
  arrow_label * 'domain * [ `MINUSGREATER ] tok * 'codomain

(** {2 Core types} *)

and core_type = core_type_node with_attributes
and core_type_node =
  | Typ_any of [ `UNDERSCORE ] tok (** [_] *)
  | Typ_var of type_variable (** ['a] *)
  | Typ_arrow of (core_type * docstring option, core_type) arrow
  (** [t -> u] or [t (** doc *) -> u] *)
  | Typ_functor of (functor_type_parameter paren, core_type) arrow
  (** [(module X : S) -> t] *)
  | Typ_tuple of
      ( (lident * [ `COLON ] tok) option * core_type, [ `STAR ]
      ) non_empty_separated_list
  (** [t * u * ...] or [x:t * y:u * ...] *)
  | Typ_constr of type_arguments * lident longident (** [t list] *)
  | Typ_class of type_arguments * [ `HASH ] tok * lident longident (** [#c] *)
  | Typ_object of
      ( [ `LESS ], object_field list * [ `DOTDOT ] tok option, [ `GREATER ]
      ) delimited
  (** [< m : t; n : u; ... >] or [< m : t; n : u; ...; .. >] *)
  | Typ_alias of core_type * [ `AS ] tok * type_variable (** [t as 'a] *)
  | Typ_variant of
      ( [ `LBRACKET | `LBRACKETGREATER | `LBRACKETLESS ],
        variant_fields, [ `RBRACKET ]
      ) delimited
  (** [[ `A | `B | ... ]] *)
  | Typ_poly of type_variable list * [ `DOT ] tok * core_type (** ['a 'b ... . t] *)
  | Typ_package of package_type paren (** [(module S)] *)
  | Typ_open of module_path * [ `DOT ] tok * core_type (** [M.t] *)
  | Typ_extension of extension (** [[%ext]] *)
  | Typ_paren of core_type paren (** [(t)] *)

(** [`A | `B | ...] or [| `A | `B | ... > `A `B ...] *)
and variant_fields =
  [ `BAR ] tok option * row_field bars
  * ([ `GREATER ] tok * name_tag list) option

(** [module S] *)
and package_type = [ `MODULE ] tok * ext_attributes * module_type

(** [module M : S], in [(module M : S) -> t] *)
and functor_type_parameter =
  [ `MODULE ] tok * ext_attributes * uident * [ `COLON ] tok * module_type
and type_arguments =
  | No_type_arguments (** [t] *) | Type_argument of core_type (** [t c] *)
  | Type_arguments of core_type commas paren (** [(t, u, ...) c] *)
and row_field =
  | Row_tag of
      name_tag
      * ( [ `OF ] tok * [ `AMPERSAND ] tok option
          * (core_type, [ `AMPERSAND ]) separated_list
        ) option
      * attribute list * docstring option
  (** [`A] or [`A of & t & u & ... (** doc *)] *)
  | Row_inherit of core_type (** [t] *)
and object_field =
  | Object_tag of
      lident * [ `COLON ] tok * core_type * attribute list * docstring option
      * ([ `SEMI ] tok * attribute list * docstring option) option
  (** [m : t] or [m : t (** doc *); (** doc *)] (one doc comment at most) *)
  | Object_inherit of core_type * [ `SEMI ] tok option (** [t] or [t;] *)

(** {2 Patterns} *)

and pattern = pattern_node with_attributes
and pattern_node =
  | Pat_any of [ `UNDERSCORE ] tok (** [_] *) | Pat_var of val_ident (** [x] *)
  | Pat_alias of pattern * [ `AS ] tok * val_ident (** [p as x] *)
  | Pat_constant of signed_constant (** [1] *)
  | Pat_interval of signed_constant * [ `DOTDOT ] tok * signed_constant (** ['a' .. 'z'] *)
  | Pat_tuple of
      (tuple_pattern_element, [ `COMMA ]) non_empty_separated_list
      * ([ `COMMA ] tok * [ `DOTDOT ] tok) option (** [p, q, ...] or [p, q, ..., ..] *)
  | Pat_construct of constr_ident longident * pattern option (** [C] or [C p] *)
  | Pat_construct_newtypes of
      constr_ident longident * newtypes paren * pattern (** [C (type a b ...) p] *)
  | Pat_variant of name_tag * pattern option (** [`A] or [`A p] *)
  | Pat_record of ([ `LBRACE ], record_pattern, [ `RBRACE ]) delimited (** [{ x = p; y = q; ... }] *)
  | Pat_array of
      ([ `LBRACKETBAR ], pattern semis option, [ `BARRBRACKET ]) delimited (** [[||]] or [[| p; q; ... |]] *)
  | Pat_list of ([ `LBRACKET ], pattern semis, [ `RBRACKET ]) delimited (** [[p; q; ...]] *)
  | Pat_or of pattern * [ `BAR ] tok * pattern (** [p | q] *)
  | Pat_cons of pattern * [ `COLONCOLON ] tok * pattern (** [p :: q] *)
  | Pat_constraint of constrained_pattern (** [(p : t)] *)
  | Pat_type of [ `HASH ] tok * lident longident (** [#t] *)
  | Pat_lazy of [ `LAZY ] tok * ext_attributes * pattern (** [lazy p] *)
  | Pat_unpack of module_name packed_module paren (** [(module M : S)] *)
  | Pat_exception of [ `EXCEPTION ] tok * ext_attributes * pattern (** [exception p] *)
  | Pat_effect of [ `EFFECT ] tok * pattern * [ `COMMA ] tok * pattern (** [effect p, k] *)
  | Pat_extension of extension (** [[%ext]] *)
  | Pat_open of module_path * [ `DOT ] tok * pattern (** [M.p] *)
  | Pat_paren of pattern paren (** [(p)] *)

(** [(p : t)] *)
and constrained_pattern = (pattern * [ `COLON ] tok * core_type) paren
and tuple_pattern_element =
  | Tuple_pattern of pattern (** [p] *)
  | Tuple_pattern_label of [ `LABEL of Token.Lident.t ] tok * pattern (** [~x:p] *)
  | Tuple_pattern_pun of [ `TILDE ] tok * lident (** [~x] *)
  | Tuple_pattern_pun_constraint of
      [ `TILDE ] tok * (lident * [ `COLON ] tok * core_type) paren (** [~(x : t)] *)

(** [x = p; y = q; ...] or [x = p; y = q; ...; _;] *)
and record_pattern =
  record_pattern_field semis
  * ([ `UNDERSCORE ] tok * [ `SEMI ] tok option) option

(** [module M] or [module M : S], in a pattern or an expression *)
and 'a packed_module =
  [ `MODULE ] tok * ext_attributes * 'a * ([ `COLON ] tok * module_type) option

(** [type a b ...] *)
and newtypes = [ `TYPE ] tok * lident list

(** [x] or [x : t = p] *)
and record_pattern_field =
  lident longident * ([ `COLON ] tok * core_type) option
  * ([ `EQUAL ] tok * pattern) option

(** {2 Expressions} *)

and expr = expr_node with_attributes
and expr_node =
  | Exp_ident of val_ident longident (** [x] *)
  | Exp_constant of constant (** [1] *)
  | Exp_construct of constr_ident longident * expr option (** [C] or [C e] *)
  | Exp_variant of name_tag * expr option (** [`A] or [`A e] *)
  | Exp_paren of expr paren (** [(e)] *)
  | Exp_constraint of (expr * type_constraint) paren (** [(e : t)] *)
  | Exp_begin of
      ([ `BEGIN ], ext_attributes * expr option, [ `END ]) delimited (** [begin end] or [begin e end] *)
  | Exp_apply of expr * argument list (** [f x y ...] *)
  | Exp_infix of expr * operator_token * expr (** [x + y] *)
  | Exp_prefix of [ `PREFIXOP of Token.Prefixop.t | `BANG ] tok * expr (** [!x] *)
  | Exp_unary of [ `MINUS | `MINUSDOT | `PLUS | `PLUSDOT ] tok * expr (** [-x] *)
  | Exp_tuple of (tuple_element, [ `COMMA ]) non_empty_separated_list (** [x, y, ...] *)
  | Exp_cons of expr * [ `COLONCOLON ] tok * expr (** [x :: xs] *)
  | Exp_field of expr * [ `DOT ] tok * lident longident (** [e.x] *)
  | Exp_setfield of
      expr * [ `DOT ] tok * lident longident * [ `LESSMINUS ] tok * expr (** [e.x <- v] *)
  | Exp_index of
      expr * index_dot * expr semis brackets
      * ([ `LESSMINUS ] tok * expr) option (** [a.(i)] or [a.(i) <- v] *)
  | Exp_setinstvar of lident * [ `LESSMINUS ] tok * expr (** [x <- e] *)
  | Exp_send of expr * [ `HASH ] tok * lident (** [e#m] *)
  | Exp_let of let_bindings * [ `IN ] tok * expr (** [let x = e in e] *)
  | Exp_letop of
      [ `LETOP of Token.Letop.t ] tok
      * (letop_binding, [ `ANDOP of Token.Andop.t ]) non_empty_separated_list
      * [ `IN ] tok * expr (** [let* x = e and* y = e and* ... in e] *)
  | Exp_struct_item of
      [ `LET ] tok * ext_attributes
      * structure_item documented * [ `IN ] tok * expr (** [let module M = X in e] *)
  | Exp_fun of
      [ `FUN ] tok * ext_attributes * function_param list
      * ([ `COLON ] tok * core_type) option * [ `MINUSGREATER ] tok * expr
  (** [fun x y ... -> e] or [fun x y ... : t -> e]. When the body is an
      [Exp_function] without attributes, its cases are those of the function,
      as in [fun x -> function | p -> e | ...], which takes two arguments. *)
  | Exp_function of [ `FUNCTION ] tok * ext_attributes * case bar_list
  (** [function | p -> e | q -> e | ...] *)
  | Exp_match of
      [ `MATCH ] tok * ext_attributes * expr * [ `WITH ] tok * case bar_list
  (** [match e with | p -> e | q -> e | ...] *)
  | Exp_try of
      [ `TRY ] tok * ext_attributes * expr * [ `WITH ] tok * case bar_list
  (** [try e with | p -> e | q -> e | ...] *)
  | Exp_if of
      [ `IF ] tok * ext_attributes * expr * [ `THEN ] tok
      * expr * ([ `ELSE ] tok * expr) option (** [if e then e] or [if e then e else e] *)
  | Exp_while of [ `WHILE ] tok * ext_attributes * expr * do_done (** [while e do e done] *)
  | Exp_for of
      [ `FOR ] tok * ext_attributes * pattern * [ `EQUAL ] tok
      * expr * [ `TO | `DOWNTO ] tok * expr * do_done (** [for x = e to e do e done] *)
  | Exp_assert of [ `ASSERT ] tok * ext_attributes * expr (** [assert e] *)
  | Exp_lazy of [ `LAZY ] tok * ext_attributes * expr (** [lazy e] *)
  | Exp_sequence of expr * [ `SEMI ] tok * expr (** [e; e'] *)
  | Exp_sequence_ext of
      expr * [ `SEMI ] tok * [ `PERCENT ] tok * attr_id * expr (** [e; %ext e] *)
  | Exp_semi of (expr * [ `SEMI ] tok) (** [e;] *)
  | Exp_new of [ `NEW ] tok * ext_attributes * lident longident (** [new c] *)
  | Exp_pack of module_expr packed_module paren (** [(module M : S)] *)
  | Exp_object of
      ([ `OBJECT ], ext_attributes * class_structure, [ `END ]) delimited
  (** [object val x = e val y = e ... end] or [object (p) val x = e val y = e ... end] *)
  | Exp_override of
      ( [ `LBRACELESS ], object_expr_field semis option, [ `GREATERRBRACE ]
      ) delimited
  (** [{< >}] or [{< x = e; y = e; ... >}] *)
  | Exp_local_open of module_path * [ `DOT ] tok * expr (** [M.(e)] *)
  | Exp_record of ([ `LBRACE ], record_expr, [ `RBRACE ]) delimited (** [{ x = e; y = e; ... }] *)
  | Exp_array of
      ([ `LBRACKETBAR ], expr semis option, [ `BARRBRACKET ]) delimited (** [[||]] or [[| x; y; ... |]] *)
  | Exp_list of ([ `LBRACKET ], expr semis, [ `RBRACKET ]) delimited (** [[x; y; ...]] *)
  | Exp_extension of extension (** [[%ext]] *)
  | Exp_unreachable of [ `DOT ] tok (** [.] *)

(** [do e done] *)
and do_done = ([ `DO ], expr, [ `DONE ]) delimited

(** [x = e; y = e; ...] or [e with x = e; y = e; ...] *)
and record_expr = (expr * [ `WITH ] tok) option * record_field semis
and index_dot =
  | Index_builtin of [ `DOT ] tok (** [.] *)
  | Index_user of
      ([ `DOT ] tok * module_path) option
      * [ `DOTOP of Token.Dotop.t ] tok (** [.%] or [.M.%] *)
and tuple_element =
  | Tuple_expr of (label:([ `LABEL of Token.Lident.t ] tok) option * expr) (** [e] or [~x:e] *)
  | Tuple_pun of [ `TILDE ] tok * lident (** [~x] *)
  | Tuple_pun_constraint of [ `TILDE ] tok * (lident * type_constraint) paren
  (** [~(x : t)] or [~(x : t :> u)] *)
and argument =
  | Arg_expr of expr (** [e] *)
  | Arg_label of [ `LABEL of Token.Lident.t ] tok * expr (** [~x:e] *)
  | Arg_pun of [ `TILDE ] tok * lident (** [~x] *)
  | Arg_pun_constraint of
      [ `TILDE ] tok * (lident * type_constraint) paren (** [~(x : t)] *)
  | Arg_optional_pun of [ `QUESTION ] tok * lident (** [?x] *)
  | Arg_optlabel of [ `OPTLABEL of Token.Lident.t ] tok * expr (** [?x:e] *)
and type_constraint =
  | Constraint of ([ `COLON ] tok * core_type) (** [: t] *)
  | Coerce of
      ([ `COLON ] tok * core_type) option
      * [ `COLONGREATER ] tok * core_type (** [:> u] or [: t :> u] *)

(** [x] or [x : t = e] *)
and record_field =
  lident longident * type_constraint option * ([ `EQUAL ] tok * expr) option

(** [x] or [x = e] *)
and object_expr_field = lident * ([ `EQUAL ] tok * expr) option

(** [let rec x = e and y = e and ...] *)
and let_bindings =
  [ `LET ] tok * ext_attributes * rec_flag * let_binding and_list

(** [x = e [@@attr]] *)
and let_binding = let_binding_body * attribute list

(** [x y ... = e] or [x y ... : t = e]. With parameters, as for [Exp_fun], a
    body which is an [Exp_function] without attributes gives the cases of the
    function. *)
and function_binding =
  function_param list * type_constraint option * [ `EQUAL ] tok * expr

(** [: type a b ... . t = e] *)
and newtype_binding =
  [ `COLON ] tok * newtypes * [ `DOT ] tok * core_type * [ `EQUAL ] tok * expr
and let_binding_body =
  | Lb_function of val_ident * function_binding (** [let f x y ... = e] *)
  | Lb_constrained of val_ident * type_constraint * [ `EQUAL ] tok * expr (** [let x : t = e] *)
  | Lb_poly of
      val_ident * [ `COLON ] tok * type_variable list
      * [ `DOT ] tok * core_type * [ `EQUAL ] tok * expr (** [let x : 'a 'b ... . t = e] *)
  | Lb_newtypes of val_ident * newtype_binding (** [let f : type a b ... . t = e] *)
  | Lb_pattern of pattern * [ `EQUAL ] tok * expr (** [let p = e] *)
  | Lb_pattern_constraint of
      pattern * [ `COLON ] tok * core_type * [ `EQUAL ] tok * expr (** [let p : t = e] *)
  | Lb_pun of val_ident (** [let x] *)
and letop_binding =
  | Lop_function of val_ident * function_binding (** [let* x = e] *)
  | Lop_pun of val_ident (** [let* x] *)
  | Lop_constraint of
      pattern * [ `COLON ] tok * core_type * [ `EQUAL ] tok * expr (** [let* (p : t) = e] *)
  | Lop_pattern of pattern * [ `EQUAL ] tok * expr (** [let* p = e] *)
and function_param =
  | Param_newtypes of newtypes paren (** [(type a b ...)] *)
  | Param_value of param_pattern (** [p] *)
and param_pattern =
  | Param_optional_default of
      [ `QUESTION ] tok * (label_pattern * default) paren (** [?(x)] or [?(x : t = e)] *)
  | Param_optional of [ `QUESTION ] tok * lident (** [?x] *)
  | Param_optlabel_default of
      [ `OPTLABEL of Token.Lident.t ] tok * (let_pattern * default) paren (** [?x:(p)] or [?x:(p : t = e)] *)
  | Param_optlabel of
      [ `OPTLABEL of Token.Lident.t ] tok
      * [ `LIDENT of Token.Lident.t | `UNDERSCORE ] tok
  (** [?x:y] or [?x:_] *)
  | Param_labelled_paren of [ `TILDE ] tok * label_pattern paren (** [~(x)] or [~(x : t)] *)
  | Param_labelled of [ `TILDE ] tok * lident (** [~x] *)
  | Param_label of [ `LABEL of Token.Lident.t ] tok * pattern (** [~x:p] *)
  | Param_label_poly of
      [ `LABEL of Token.Lident.t ] tok * constrained_pattern (** [~x:(p : t)] *)
  | Param_pattern of pattern (** [p] *)
  | Param_poly of constrained_pattern (** [(p : 'a. t)] *)

(** [x] or [x : t] *)
and label_pattern = lident * ([ `COLON ] tok * core_type) option

(** [p] or [p : t] *)
and let_pattern = pattern * ([ `COLON ] tok * core_type) option

(** nothing or [= e], in [?(x = e)] *)
and default = ([ `EQUAL ] tok * expr) option

(** [p -> e] or [p when e -> e] *)
and case =
  pattern * ([ `WHEN ] tok * expr) option * [ `MINUSGREATER ] tok * expr

(** {2 Classes} *)

(** nothing or [['a, 'b, ...]], in [class ['a, 'b, ...] c = e] *)
and class_parameters =
  ([ `LBRACKET ], type_parameter commas, [ `RBRACKET ]) delimited option

(** nothing or [[t, u, ...]], in [[t, u, ...] c] *)
and class_arguments =
  ([ `LBRACKET ], core_type commas, [ `RBRACKET ]) delimited option

(** [let open M in body] or [let open! M in body] *)
and 'body let_open =
  [ `LET ] tok * [ `OPEN ] tok * [ `BANG ] tok option
  * attribute list * module_path * [ `IN ] tok * 'body

and class_expr = class_expr_node with_attributes
and class_expr_node =
  | Cl_constr of class_arguments * lident longident (** [c] *)
  | Cl_structure of
      ([ `OBJECT ], attribute list * class_structure, [ `END ]) delimited
  (** [object val x = e val y = e ... end] or [object (p) val x = e val y = e ... end] *)
  | Cl_fun of
      [ `FUN ] tok * attribute list * param_pattern list
      * [ `MINUSGREATER ] tok * class_expr (** [fun x y ... -> c] *)
  | Cl_apply of class_expr * argument list (** [c x y ...] *)
  | Cl_let of let_bindings * [ `IN ] tok * class_expr (** [let x = e in c] *)
  | Cl_open of class_expr let_open (** [let open M in c] *)
  | Cl_constraint of (class_expr * [ `COLON ] tok * class_type) paren (** [(c : ct)] *)
  | Cl_extension of extension (** [[%ext]] *)
  | Cl_paren of class_expr paren (** [(c)] *)
and class_structure = class_self option * class_field element list
and class_self =
  | Self_pattern of pattern paren (** [(p)] *)
  | Self_constraint of constrained_pattern (** [(p : t)] *)
and class_field =
  | Cf_inherit of
      ( [ `INHERIT ] tok * [ `BANG ] tok option * attribute list * class_expr
        * ([ `AS ] tok * lident) option * post_attributes:attribute list
      ) (** [inherit c] or [inherit! c as x] *)
  | Cf_val of
      ( [ `VAL ] tok * [ `BANG ] tok option * attribute list
        * [ `MUTABLE | `VIRTUAL ] tok list * lident
        * class_value_body * post_attributes:attribute list
      ) (** [val x = e] or [val! x = e] *)
  | Cf_method of
      ( [ `METHOD ] tok * [ `BANG ] tok option * attribute list
        * [ `PRIVATE | `VIRTUAL ] tok list * lident
        * method_body * post_attributes:attribute list
      ) (** [method m = e] or [method! m = e] *)
  | Cf_constraint of
      ( [ `CONSTRAINT ] tok * attribute list * core_type
        * [ `EQUAL ] tok * core_type * post_attributes:attribute list
      ) (** [constraint t = u] *)
  | Cf_initializer of
      ( [ `INITIALIZER ] tok * attribute list
        * expr * post_attributes:attribute list
      ) (** [initializer e] *)
  | Cf_extension of extension * attribute list (** [[%%ext]] *)
  | Cf_attribute of attribute (** [[@@@attr]] *)
and class_value_body =
  | Value_virtual of ([ `COLON ] tok * core_type) (** [: t] *)
  | Value_concrete of type_constraint option * [ `EQUAL ] tok * expr (** [= e] or [: t = e] *)
and method_body =
  | Method_virtual of ([ `COLON ] tok * core_type) (** [: t] *)
  | Method_strict of function_binding (** [x = e] *)
  | Method_poly of [ `COLON ] tok * core_type * [ `EQUAL ] tok * expr (** [: t = e] *)
  | Method_newtypes of newtype_binding (** [: type a. t = e] *)
and class_type =
  | Cty_signature of class_signature (** [object end] *)
  | Cty_arrow of (core_type, class_type) arrow (** [t -> ct] *)
and class_signature = class_signature_node with_attributes
and class_signature_node =
  | Csig_constr of class_arguments * lident longident (** [c] *)
  | Csig_extension of extension (** [[%ext]] *)
  | Csig_object of ([ `OBJECT ], class_signature_body, [ `END ]) delimited (** [object val x : t val y : u ... end] *)
  | Csig_open of class_signature let_open (** [let open M in ct] *)

(** [val x : t val y : u ...] or [(t) val x : t val y : u ...] *)
and class_signature_body =
  attribute list * core_type paren option * class_sig_field element list
and class_sig_field =
  | Ctf_inherit of
      ( [ `INHERIT ] tok * attribute list
        * class_signature * post_attributes:attribute list
      ) (** [inherit ct] *)
  | Ctf_val of
      ( [ `VAL ] tok * attribute list * [ `MUTABLE | `VIRTUAL ] tok list
        * lident * [ `COLON ] tok * core_type * post_attributes:attribute list
      ) (** [val x : t] *)
  | Ctf_method of
      ( [ `METHOD ] tok * attribute list * [ `PRIVATE | `VIRTUAL ] tok list
        * lident * [ `COLON ] tok * core_type * post_attributes:attribute list
      ) (** [method m : t] *)
  | Ctf_constraint of
      ( [ `CONSTRAINT ] tok * attribute list * core_type
        * [ `EQUAL ] tok * core_type * post_attributes:attribute list
      ) (** [constraint t = u] *)
  | Ctf_extension of extension * attribute list (** [[%%ext]] *)
  | Ctf_attribute of attribute (** [[@@@attr]] *)

(** [c body] or [virtual ['a] c body], after [class] or [and] *)
and 'body class_info =
  [ `VIRTUAL ] tok option * class_parameters * lident * 'body * attribute list

(** [: ct], in [class c : ct] *)
and class_description_body = [ `COLON ] tok * class_type

(** [= ct], in [class type c = ct] *)
and class_type_declaration_body = [ `EQUAL ] tok * class_signature
and class_binding =
  | Class_equal of ([ `EQUAL ] tok * class_expr) (** [= c] *)
  | Class_constraint of
      [ `COLON ] tok * class_type * [ `EQUAL ] tok * class_expr (** [: ct = c] *)
  | Class_param of param_pattern * class_binding (** [p = c] *)

(** {2 Modules} *)

and functor_arg =
  | Functor_unit of unit paren (** [()] *)
  | Functor_named of (module_name * [ `COLON ] tok * module_type) paren (** [(X : S)] *)

and module_expr = module_expr_node with_attributes
and module_expr_node =
  | Mod_ident of module_path (** [M] *)
  | Mod_structure of
      ([ `STRUCT ], attribute list * structure, [ `END ]) delimited
  (** [struct let x = e let y = e ... end] *)
  | Mod_functor of
      [ `FUNCTOR ] tok * attribute list * functor_arg list
      * [ `MINUSGREATER ] tok * module_expr
  (** [functor (X : S) (Y : T) ... -> M] *)
  | Mod_apply of module_expr * module_expr (** [F(X)] *)
  | Mod_apply_unit of module_expr * unit paren (** [F()] *)
  | Mod_constraint of (module_expr * [ `COLON ] tok * module_type) paren (** [(M : S)] *)
  | Mod_paren of module_expr paren (** [(M)] *)
  | Mod_unpack of val_unpack paren (** [(val e : S)] *)
  | Mod_extension of extension (** [[%ext]] *)

(** [val e : S] *)
and val_unpack = [ `VAL ] tok * attribute list * expr * package_annotation
and package_annotation =
  | No_package (** [] *)
  | Package_constraint of ([ `COLON ] tok * module_type) (** [: S] *)
  | Package_coerce of
      ([ `COLON ] tok * module_type) option
      * [ `COLONGREATER ] tok * module_type (** [:> T] or [: S :> T] *)

and module_type = module_type_node with_attributes
and module_type_node =
  | Mty_ident of ident_token longident (** [S] *)
  | Mty_signature of
      ([ `SIG ], attribute list * signature, [ `END ]) delimited (** [sig val x : t val y : u ... end] *)
  | Mty_functor of
      ([ `FUNCTOR ] tok * attribute list) option * functor_arg list
      * [ `MINUSGREATER ] tok * module_type (** [(X : S) (Y : T) ... -> U] or [functor (X : S) (Y : T) ... -> U] *)
  | Mty_arrow of module_type * [ `MINUSGREATER ] tok * module_type (** [S -> T] *)
  | Mty_with of
      module_type * [ `WITH ] tok * (with_constraint, [ `AND ]) separated_list
  (** [S with type t = u and type v = w and ...] *)
  | Mty_typeof of
      [ `MODULE ] tok * [ `TYPE ] tok * [ `OF ] tok
      * attribute list * module_expr (** [module type of M] *)
  | Mty_extension of extension (** [[%ext]] *)
  | Mty_paren of module_type paren (** [(S)] *)
and with_constraint =
  | With_type of
      [ `TYPE ] tok * type_parameters * lident longident * [ `EQUAL ] tok
      * [ `PRIVATE ] tok option * core_type * type_constraint_clause list (** [type t = u] or [type t = private u] *)
  | With_typesubst of
      [ `TYPE ] tok * type_parameters * lident longident
      * [ `COLONEQUAL ] tok * core_type (** [type t := u] *)
  | With_module of
      [ `MODULE ] tok * module_path
      * [ `EQUAL | `COLONEQUAL ] tok * module_path (** [module M = N] *)
  | With_modtype of
      [ `MODULE ] tok * [ `TYPE ] tok * ident_token longident
      * [ `EQUAL | `COLONEQUAL ] tok * module_type (** [module type S = T] *)

(** {2 Signatures and structures} *)

and signature = signature_item element list
and signature_item =
  | Sig_value of value_description (** [val x : t] *)
  | Sig_type of type_declarations (** [type t] *)
  | Sig_typesubst of type_declarations (** [type t := u] *)
  | Sig_typext of type_extension (** [type t += C] *)
  | Sig_exception of exception_declaration (** [exception C] *)
  | Sig_module of [ `MODULE ] tok * ext_attributes * module_declaration (** [module M : S] *)
  | Sig_modsubst of module_substitution (** [module M := N] *)
  | Sig_recmodule of
      [ `MODULE ] tok * ext_attributes
      * [ `REC ] tok * module_declaration and_list
  (** [module rec M : S and N : T and ...] *)
  | Sig_modtype of module_type_declaration (** [module type S = T] *)
  | Sig_modtypesubst of module_type_declaration (** [module type S := T] *)
  | Sig_open of module_path open_infos (** [open M] *)
  | Sig_include of module_type include_infos (** [include S] *)
  | Sig_class of
      [ `CLASS ] tok * ext_attributes
      * class_description_body class_info and_list
  (** [class c : ct and d : dt and ...] *)
  | Sig_class_type of
      [ `CLASS ] tok * [ `TYPE ] tok * ext_attributes
      * class_type_declaration_body class_info and_list (** [class type c = object end and d = object end and ...] *)
  | Sig_attribute of attribute (** [[@@@attr]] *)
  | Sig_extension of extension * attribute list (** [[%%ext]] *)
  | Sig_semisemi of [ `SEMISEMI ] tok (** [;;] *)

and structure = structure_item element list
and structure_item =
  | Str_eval of expr * attribute list (** [e] *)
  | Str_value of let_bindings (** [let x = e] *)
  | Str_primitive of value_description (** [external x : t = "x"] *)
  | Str_type of type_declarations (** [type t] *)
  | Str_typext of type_extension (** [type t += C] *)
  | Str_exception of exception_declaration (** [exception C] *)
  | Str_module of [ `MODULE ] tok * ext_attributes * module_binding (** [module M = N] *)
  | Str_recmodule of
      [ `MODULE ] tok * ext_attributes * [ `REC ] tok * module_binding and_list
  (** [module rec M = N and P = Q and ...] *)
  | Str_modtype of module_type_declaration (** [module type S = T] *)
  | Str_open of module_expr open_infos (** [open M] *)
  | Str_class of
      [ `CLASS ] tok * ext_attributes * class_binding class_info and_list
  (** [class c = object end and d = object end and ...] *)
  | Str_class_type of
      [ `CLASS ] tok * [ `TYPE ] tok * ext_attributes
      * class_type_declaration_body class_info and_list (** [class type c = object end and d = object end and ...] *)
  | Str_include of module_expr include_infos (** [include M] *)
  | Str_attribute of attribute (** [[@@@attr]] *)
  | Str_extension of extension * attribute list (** [[%%ext]] *)
  | Str_semisemi of [ `SEMISEMI ] tok (** [;;] *)

(** [val x : t] or [external x : t = "x" "y" ...] *)
and value_description =
  [ `VAL | `EXTERNAL ] tok * ext_attributes * val_ident * [ `COLON ] tok
  * core_type * ([ `EQUAL ] tok * string_token list) option * attribute list

(** [type t and u and ...] or [type nonrec t and u and ...] *)
and type_declarations =
  [ `TYPE ] tok * ext_attributes
  * [ `NONREC ] tok option * type_declaration and_list

(** ['a t = u constraint 'a = v constraint 'b = w ...] *)
and type_declaration =
  type_parameters * lident * type_kind
  * type_constraint_clause list * attribute list

(** [constraint t = u] *)
and type_constraint_clause =
  [ `CONSTRAINT ] tok * core_type * [ `EQUAL ] tok * core_type
and type_kind =
  | Kind_abstract (** [type t] *)
  | Kind_defined of [ `EQUAL | `COLONEQUAL ] tok * type_definition (** [type t = u] *)
and type_definition =
  | Def_manifest of [ `PRIVATE ] tok option * core_type (** [= t] or [= private t] *)
  | Def_variant of
      type_synonym * [ `PRIVATE ] tok option * constructor_declarations (** [= C] or [= t = private C] *)
  | Def_open of type_synonym * [ `PRIVATE ] tok option * [ `DOTDOT ] tok (** [= ..] or [= t = private ..] *)
  | Def_record of
      type_synonym * [ `PRIVATE ] tok option
      * ([ `LBRACE ], label_declaration list, [ `RBRACE ]) delimited
  (** [= { x : t; y : u; ... }] or [= t = private { x : t; y : u; ... }] *)
  | Def_external of [ `EXTERNAL ] tok * string_token (** [= external "x"] *)

(** nothing or [t =], in [type u = t = C] *)
and type_synonym = (core_type * [ `EQUAL ] tok) option
and constructor_declarations =
  | Constructors_empty of [ `BAR ] tok (** [|] *)
  | Constructors of (constructor_declaration * docstring option) bar_list
  (** [C | D | ...] or [C (** doc *) | D (** doc *) | ...] *)

(** [C of t [@attr]] *)
and constructor_declaration =
  constr_ident * constructor_signature * attribute list
and constructor_signature =
  | Args_none (** [C] *)
  | Args_of of [ `OF ] tok * constructor_arguments (** [C of t] *)
  | Args_gadt of
      [ `COLON ] tok * (type_variable list * [ `DOT ] tok) option
      * (constructor_arguments * [ `MINUSGREATER ] tok) option * core_type (** [C : v] or [C : 'a 'b ... . t * u * ... -> v] *)
and constructor_arguments =
  | Constructor_tuple of core_type stars (** [t * u * ...] *)
  | Constructor_record of
      ([ `LBRACE ], label_declaration list, [ `RBRACE ]) delimited (** [{ x : t; y : u; ... }] *)

(** [x : t] or [mutable x : t [@attr] (** doc *); [@attr] (** doc *)] (one doc
    comment at most) *)
and label_declaration =
  [ `MUTABLE ] tok option * lident * [ `COLON ] tok
  * core_type * attribute list * docstring option
  * ([ `SEMI ] tok * attribute list * docstring option) option

(** [type t += C | D | ...] or [type t += private C | D | ...] *)
and type_extension =
  [ `TYPE ] tok * ext_attributes * type_parameters
  * lident longident * [ `PLUSEQ ] tok * [ `PRIVATE ] tok option
  * (extension_constructor * docstring option) bar_list * attribute list
and extension_constructor =
  | Ext_decl of constructor_declaration (** [C of t] *)
  | Ext_rebind of
      constr_ident * [ `EQUAL ] tok
      * constr_ident longident * attribute list (** [C = M.C] *)

(** [exception C] *)
and exception_declaration =
  [ `EXCEPTION ] tok * ext_attributes * extension_constructor * attribute list

(** [M : S], after [module] or [and] *)
and module_declaration = module_name * module_declaration_body * attribute list
and module_declaration_body =
  | Mdecl_type of ([ `COLON ] tok * module_type) (** [: S] *)
  | Mdecl_functor of functor_arg * module_declaration_body (** [(X : S) : T] *)
  | Mdecl_alias of ([ `EQUAL ] tok * module_path) (** [= N] *)

(** [module M := N] *)
and module_substitution =
  [ `MODULE ] tok * ext_attributes * uident
  * [ `COLONEQUAL ] tok * module_path * attribute list

(** [module type S] or [module type S = T] *)
and module_type_declaration =
  [ `MODULE ] tok * [ `TYPE ] tok * ext_attributes * ident_token
  * ([ `EQUAL | `COLONEQUAL ] tok * module_type) option * attribute list

(** [M = N], after [module] or [and] *)
and module_binding = module_name * module_binding_body * attribute list
and module_binding_body =
  | Mbind_equal of ([ `EQUAL ] tok * module_expr) (** [= M] *)
  | Mbind_constraint of
      [ `COLON ] tok * module_type * [ `EQUAL ] tok * module_expr (** [: S = M] *)
  | Mbind_functor of functor_arg * module_binding_body (** [(X : S) = M] *)

(** [open M] or [open! M] *)
and 'a open_infos =
  [ `OPEN ] tok * [ `BANG ] tok option * ext_attributes * 'a * attribute list

(** [include M] *)
and 'a include_infos = [ `INCLUDE ] tok * ext_attributes * 'a * attribute list

(** [let x = e let y = e ...], up to the end of the file *)
type implementation = structure * [ `EOF ] tok

(** [val x : t val y : u ...], up to the end of the file *)
type interface = signature * [ `EOF ] tok
