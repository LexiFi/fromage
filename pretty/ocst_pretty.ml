(* A pretty printer for the CST: it keeps the tokens and the comments of the
   source, and chooses the line breaks and the indentation, following the
   layout rules of ocst/HACKING.md. The layouts are documents of
   pretty_expressive, which chooses the best one for a width, 80 by default. *)

open Ocst
open Cst

(** The layouts of the CST for a width. *)
module Make (Config : Doc.CONFIG) =
  struct
    module D = Doc.Make (Config)
    module L = Syntax_layouts.Make (D)
    open D
    open L

    (* {1 Identifiers} *)

    let floating = function Text value | Ambiguous value -> tok value

    let documented print ((pre, value, post) : _ documented) =
      let value = print value in
      let value =
        match pre with
          None -> value
        | Some doc -> tok doc +| value
      in
      with_info value post

    (** The elements of a block, one per line; as [D.items] for the [items]
        of a structure or a signature. *)
    let elements ?(items = false) print values =
      let values =
        List.map
          ( function
            | Item value -> documented print value, false
            | Floating (Ambiguous _ as value) -> floating value, true
            | Floating (Text _ as value) -> floating value, false
          )
          values
      in
      if items then D.items values else vertical (List.map fst values)

    let rec module_path =
      function
      | Path_ident name -> tok name
      | Path_dot (path, dot, name) -> module_path path ++ tok dot ++ tok name
      | Path_apply (path, { left; content; right }) ->
          module_path path ++ tok left ++ module_path content ++ tok right

    let longident print (qualifier, name) =
        opt (fun (path, dot) -> module_path path ++ tok dot) qualifier
      ++ print name

    let operator =
      function
      | Operator value -> operator_name value
      | Index_operator (dotop, brackets, assign) ->
          let inside = opt (fun (semi, dotdot) -> tok semi ++ tok dotdot) in
          let brackets =
            match brackets with
            | Parens { left; content; right } ->
                tok left ++ inside content ++ tok right
            | Brackets { left; content; right } ->
                tok left ++ inside content ++ tok right
            | Braces { left; content; right } ->
                tok left ++ inside content ++ tok right
          in
          (* [<-] as a part of a name, as [operator_name]. *)
          tok dotop ++ brackets ++ opt D.tok assign

    let val_ident =
      function
      | Val_lident name -> tok name
      | Val_operator { left; content; right } ->
          tok left +- operator content +- tok right

    let constr_ident =
      function
      | Constr_uident name -> tok name
      | Constr_cons { left; content; right } ->
          tok left ++ tok content ++ tok right
      | Constr_nil { left; right; _ } -> tok left ++ tok right
      | Constr_unit { left; right; _ } -> tok left ++ tok right
      | Constr_false value -> tok value
      | Constr_true value -> tok value

    let glued_list values = List.fold_left ( ++ ) empty values

    (** [a.b]: its parts can be keywords, as [as] in [\[@as\]], printed as
        names. *)
    let attr_id { first; rest } =
      glued_list
        ( D.tok first
          :: List.concat_map (fun (dot, id) -> [ tok dot; D.tok id ]) rest
        )

    let type_variable ((quote, name) : type_variable) = tok quote ++ tok name

    (** ['a 'b ... .]: the type variables packed, and the dot after them, or
      alone on its line after them on several lines. *)
    let type_variables_dot variables dot =
      separator_after
        ( pack_list_isolating_multiline
            ~indent:0 (List.map type_variable variables)
        )
        (tok dot)

    let name_tag ((backquote, name) : name_tag) = tok backquote ++ tok name

    let signed_constant =
      function
      | Signed (sign, constant) -> tok sign ++ tok constant
      | Unsigned constant -> tok constant

    let arrow_label =
      function
      | Nolabel -> empty
      | Labelled (name, colon) -> tok name ++ tok colon
      | Optional value -> tok value
      (* [?x:] would be one token. *)
      | Optional_split (question, name, colon) ->
          tok question ++ tok name +- tok colon

    let type_parameter (variance, variable) =
        glued_list (List.map tok variance)
      ++
        match variable with
          `Var variable -> type_variable variable
        | `Any underscore -> tok underscore

    let type_parameters =
      function
      | No_type_parameters -> empty
      | One_type_parameter value -> type_parameter value
      | Many_type_parameters value ->
          paren
            ( fun values ->
                pack_list_isolating_multiline
                  ~indent:0 (commas type_parameter values)
            )
            value

    let module_name =
      function
      | Module_name name -> tok name
      | Module_anonymous underscore -> tok underscore

    (* {1 Precedence of infix operators} *)

    type associativity = Left | Right

    let operator_level (operator : operator_token) =
      match operator.kind with
      | `HASHOP _ -> 8, Left
      | `INFIXOP4 _ -> 7, Right
      | `INFIXOP3 _ | `STAR | `PERCENT -> 6, Left
      | `INFIXOP2 _ | `PLUS | `PLUSDOT | `MINUS | `MINUSDOT -> 5, Left
      | `INFIXOP1 _ -> 4, Right
      | `INFIXOP0 _ | `EQUAL | `LESS | `GREATER | `PLUSEQ -> 3, Left
      | `AMPERSAND | `AMPERAMPER -> 2, Right
      | `OR | `BARBAR -> 1, Right
      | `COLONEQUAL -> 0, Right
      | `PREFIXOP _ | `LETOP _ | `ANDOP _ | `BANG -> 9, Left

    let operator_kind (operator : [< Token.any_kind ] Token.token) =
      Operator_classification.classify
        (Token.text (operator :> Token.any_token))

    (* {1 Layouts shared by patterns and expressions} *)

    (** [\[x; y; ...\]], with the elements printed by [print]; see [bracket] for
      [inline]. *)
    let list_literal ?inline print value =
      paren
        ~spaces:true ?inline
        ( fun values ->
            pack_list_isolating_multiline ~indent:0 (semis print values)
        )
        value

    (** [\[| x; y; ... |\]] or [\[||\]], with the elements printed by [print]. *)
    let array_literal print { left; content; right } =
      bracket
        ~spaces:true ~inline:false (tok left)
        ( opt
            ( fun values ->
                pack_list_isolating_multiline ~indent:0 (semis print values)
            )
            content
        )
        (tok right)

    (** [{ x = v; y = w; ... }], with the fields printed by [field]: in a pattern
      with [suffix] [_] after them, and in an expression with [base] ([e]
      and [with]) before them; see [bracket] for [inline]. *)
    let record_literal
        ?base ?(suffix = empty) ?inline field { left; content = fields; right }
      =
        let fields =
          pack_list_isolating_multiline
            ~indent:0 (semis field fields @ [ suffix ])
        in
        let left = tok left
        and right = tok right
        in
        match base with
        | None -> bracket ~spaces:true ?inline left fields right
        | Some (base, with_) ->
            let with_ = tok with_ in
            (* [{ base with] on the line of the bracket; after a base on
               several lines, [with] alone in the column of the bracket, and
               the fields indented by 2. *)
            let content =
                flat (base +- with_ +- fields)
              <|> (flat (base +- with_) +| fields)
            in
            begin match inline with
            | Some false ->
                bracket
                  ~spaces:true ~inline:false left
                  (content <|> losing_ties (base +| with_ +| fields)) right
            | _ ->
                  bracket ~spaces:true left content right
                <|>
                  losing_ties
                    ( below ~indent:2 (left +- nest 2 base +| with_) fields
                      +| right
                    )
            end

    (* {1 The grammar} *)

    let rec attribute { left; content = id, payload; right } =
      extension_like (tok left) id payload (tok right)

    and extension_like left id payload right =
      let head = left ++ attr_id id in
      match payload with
      | Payload_structure [] -> head ++ right
      | _ ->
          let payload = payload_doc payload in
          flat (head +- payload ++ right) <|> (head +/ payload +| right)

    and payload_doc =
      function
      | Payload_structure items -> structure items
      | Payload_signature (colon, items) -> after (tok colon) (signature items)
      | Payload_type (colon, typ) -> after (tok colon) (core_type typ)
      | Payload_pattern (question, value, guard) ->
          (* [?] is one character: the pattern on its line, its next lines
             indented by 2 from [?]. *)
          guarded (tok question +- nest 2 (pattern value)) (guard_expr guard)

    (** [p when e]. *)
    and guarded_pattern value guard = guarded (pattern value) (guard_expr guard)

    and guard_expr guard =
      Option.map (fun (when_, guard) -> when_, expr guard) guard

    (** The attributes after a node, as many per line as fit, a multi-line
      attribute on lines of its own. *)
    and attributes values =
      pack_list_isolating_multiline ~indent:0 (List.map attribute values)

    and with_attributes : 'a. ('a -> t) -> 'a with_attributes -> t =
      fun print { node; attributes = values } ->
        (* The attributes of the whole node, in its column. *)
        item_attributes (print node) values

    (** The attributes after an item, [\[@@...\]], or after a node: on the
        line of the item when both are on one line, else on the next lines. *)
    and item_attributes item values =
      let values = attributes values in
      if is_empty values then item
      else flat item +- flat values <|> (item +| values)

    (** [item [@attr] ...]: the attributes after [item] when both are on one
        line, else on the next lines, indented by 2, packed. *)
    and attached_attributes item values =
      let values = attributes values in
      if is_empty values then item
      else flat item +- flat values <|> (item +/ values)

    and extension =
      function
      | Extension { left; content = id, payload; right } ->
          extension_like (tok left) id payload (tok right)
      | Quoted_extension value -> tok value

    and ext_attributes (ext, values) =
      after
        (opt (fun (percent, id) -> tok percent ++ attr_id id) ext)
        (attributes values)

    (** [keyword%ext [@attr]]. *)
    and keyword
      : 'kind.
          ([< Syntax_layouts.safe_kind ] as 'kind) Token.token
          -> ext_attributes
          -> t
      =
      fun keyword (ext, values) ->
        let keyword =
            tok keyword
          ++ opt (fun (percent, id) -> tok percent ++ attr_id id) ext
        in
        after keyword (attributes values)

    (** [first and second and ...], one per line, with the doc comments before
      [and]: each of the others printed by [print] after its prefix, [and]
      and its attributes. *)
    and and_list : 'a. t -> (t -> 'a -> t) -> 'a and_list -> t =
      fun first print (_, rest) ->
        List.fold_left
          ( fun
              doc
              ( ~docstring:((post, values, pre) : and_docs),
                and_, attributes_, value
              )
            ->
              let doc =
                vertical
                  ( (with_info doc post :: List.map floating values)
                    @ [ opt tok pre ]
                  )
              in
              doc +| print (tok and_ ++ attributes attributes_) value
          )
          first rest

    (* {2 Core types} *)

    and core_type value = with_attributes core_type_node value

    (** The first type of an arrow type, and the arrows and types after it:
        [a], then [-> b] and [-> c] in [a -> b -> c]. *)
    and arrow_parts (typ : core_type) =
      let first, rest =
        match typ with
        | { node = Typ_arrow (label, (domain, doc), arrow, codomain);
            attributes = []
          }
        ->
            ( with_info (arrow_label label ++ core_type domain) doc,
              Some (arrow, codomain)
            )
        | { node = Typ_functor (label, parameter, arrow, codomain);
            attributes = []
          }
        ->
            ( arrow_label label ++ paren functor_type_parameter parameter,
              Some (arrow, codomain)
            )
        | _ -> core_type typ, None
      in
      match rest with
      | None -> first, []
      | Some (arrow, codomain) ->
          let next, rest = arrow_parts codomain in
          first, (~arrow, ~typ:next) :: rest

    and functor_type_parameter
        ((module_, ext, name, colon, typ) : functor_type_parameter)
      = after (keyword module_ ext +- tok name +- tok colon) (module_type typ)

    and core_type_node node =
      match node with
      | Typ_any value -> tok value
      | Typ_var value -> type_variable value
      | Typ_arrow _ | Typ_functor _ ->
          let first, rest = arrow_parts { node; attributes = [] } in
          arrow_type first rest
      | Typ_tuple elements ->
          let element (label, typ) =
              opt (fun (name, colon) -> tok name ++ tok colon) label
            ++ core_type typ
          in
          pack_list_isolating_multiline
            ~indent:0 (separated_before element elements)
      | Typ_constr (arguments, path) ->
          type_arguments arguments +- longident tok path
      | Typ_class (arguments, hash, path) ->
          type_arguments arguments +- (tok hash ++ longident tok path)
      | Typ_object { left; content = fields, dotdot; right } ->
          bracket
            ~spaces:true (tok left)
            ( pack_list_isolating_multiline
                ~indent:0 (List.map object_field fields @ [ opt tok dotdot ])
            )
            (tok right)
      | Typ_alias (typ, as_, variable) ->
          alias (core_type typ) as_ (type_variable variable)
      | Typ_variant
          { left = left_token; content = leading_bar, fields, tags; right }
      ->
          (* After [\[ ], the fields on the line of the bracket: the first
             one in the column of the others after their bars, or each after
             its bar after [\[ |], as after [( ]; after [\[<] or [\[>], on the
             next lines, as the constructors of a variant type. *)
          let after_bracket = left_token.kind = `LBRACKET in
          let fields =
            match fields with
            | Zero -> [ opt tok leading_bar ]
            | Many fields when after_bracket && leading_bar = None ->
                (* The first field after [\[ ], indented as the others after
                   their [| ]. *)
                begin match separated_before row_field fields with
                | first :: rest -> nest 2 first :: rest
                | [] -> []
                end
            | Many fields -> bar_list row_field (leading_bar, fields)
          in
          let tags =
            opt
              ( fun (greater, tags) ->
                  pack_after_head
                    ~indent:2 (tok greater) (List.map name_tag tags)
              )
              tags
          in
          let content =
            pack_list_isolating_multiline ~indent:0 (fields @ [ tags ])
          and left = tok left_token
          and right = tok right
          in
          begin match leading_bar with
          | Some _ when after_bracket -> bracket ~spaces:true left content right
          | _ ->
                flat (left +- content +- right)
              <|>
                if after_bracket then left +- content +| right
                else left +| content +| right
          end
      | Typ_poly (variables, dot, typ) ->
          after (type_variables_dot variables dot) (core_type typ)
      | Typ_package value ->
          paren
            ( fun ((module_, ext, typ) : package_type) ->
                after (keyword module_ ext) (module_type typ)
            )
            value
      | Typ_open (path, dot, typ) ->
          module_path path ++ tok dot ++ core_type typ
      | Typ_extension value -> extension value
      | Typ_paren value -> paren core_type value

    and type_arguments =
      function
      | No_type_arguments -> empty
      | Type_argument typ -> core_type typ
      | Type_arguments value ->
          paren
            ( fun values ->
                pack_list_isolating_multiline
                  ~indent:0 (commas core_type values)
            )
            value

    and row_field =
      function
      | Row_tag (tag, of_, values, doc) ->
          (* As a constructor: [`A of] on its line, then the types. *)
          let value =
            match of_ with
            | None -> name_tag tag
            | Some (of_, ampersand, types) ->
                let types =
                  match types with
                    Zero -> []
                  | Many types -> separated_before core_type types
                in
                after
                  (name_tag tag +- tok of_ +- opt tok ampersand)
                  (pack_list_isolating_multiline ~indent:0 types)
          in
          with_info (attached_attributes value values) doc
      | Row_inherit typ -> core_type typ

    and field_end value doc semi =
      let value = with_info value doc in
      match semi with
      | None -> value
      | Some (semi, values, doc) ->
          (* As in [semis]: [;] alone on its line after a value on several
             lines. *)
          with_info
            ( attached_attributes
                (separator_after ~in_bracket:true value (tok semi)) values
            )
            doc

    and object_field =
      function
      | Object_tag (name, colon, typ, values, doc, semi) ->
          field_end
            ( attached_attributes
                (after (tok name +- tok colon) (core_type typ)) values
            )
            doc semi
      | Object_inherit (typ, semi) -> core_type typ ++ opt tok semi

    (* {2 Patterns} *)

    and pattern value = with_attributes (pattern_node ~in_bracket:false) value

    (** A pattern which starts where the content of a bracket starts (see
      [bracket_expr]). *)
    and bracket_pattern (value : pattern) =
      match value with
      | { node; attributes = [] } -> pattern_node ~in_bracket:true node
      | value -> pattern value

    and or_patterns (value : pattern) =
      match value with
      | { node = Pat_or (left, bar, right); attributes = [] } ->
          or_patterns left @ [ tok bar +- pattern right ]
      | _ -> [ pattern value ]

    (** The alternatives of an or-pattern, as many per line as fit, an
      alternative on several lines on lines of its own: the lines after the
      first one are [shift] columns to the right of it. *)
    and or_pattern_lines ~shift (value : pattern) =
      pack_list_isolating_multiline ~indent:shift (or_patterns value)

    (** A pattern after a bar or a parenthesis, aligned with
      the other alternatives of an or-pattern. *)
    and shifted_pattern (value : pattern) =
      match value with
      | { node = Pat_or _; attributes = [] } ->
          or_pattern_lines ~shift:(-2) value
      | _ -> bracket_pattern value

    (** As [cons_operands], for a pattern. *)
    and cons_patterns left cons right =
      let rec operands rest cons (value : pattern) =
        match value with
        | { node = Pat_cons (left, next, right); attributes = [] } ->
            operands ((cons, pattern left) :: rest) next right
        | _ -> List.rev ((cons, pattern value) :: rest)
      in
      pattern left, operands [] cons right

    and pattern_node ~in_bracket node =
      match node with
      | Pat_any value -> tok value
      | Pat_var name -> val_ident name
      | Pat_alias (value, as_, name) ->
          alias (pattern value) as_ (val_ident name)
      | Pat_constant value -> signed_constant value
      | Pat_interval (low, dotdot, high) ->
          words [ signed_constant low; tok dotdot; signed_constant high ]
      | Pat_tuple (elements, open_) ->
          let elements =
            separated_after (tuple_pattern_element ~in_bracket) elements
          in
          let elements =
            match open_ with
            | None -> elements
            | Some (comma, dotdot) ->
                begin match List.rev elements with
                | last :: others ->
                    List.rev
                      (tok dotdot :: separator_after last (tok comma) :: others)
                | [] -> []
                end
          in
          pack_list_isolating_multiline ~indent:0 elements
      | Pat_construct (constructor, argument) ->
          after (longident constr_ident constructor) (opt pattern argument)
      | Pat_construct_newtypes (constructor, types, argument) ->
          after
            (longident constr_ident constructor +- paren newtypes types)
            (pattern argument)
      | Pat_variant (tag, argument) ->
          after (name_tag tag) (opt pattern argument)
      | Pat_record value -> record_pattern value
      | Pat_array value -> array_literal bracket_pattern value
      | Pat_list value -> list_literal bracket_pattern value
      | Pat_or _ -> or_pattern_lines ~shift:0 { node; attributes = [] }
      | Pat_cons (left, cons, right) ->
          let first, rest = cons_patterns left cons right in
          operator_chain ~in_bracket (operator_kind cons) first rest
      | Pat_constraint value -> poly_pattern value
      | Pat_type (hash, path) -> tok hash ++ longident tok path
      | Pat_lazy (lazy_, ext, value) ->
          after (keyword lazy_ ext) (pattern value)
      | Pat_unpack value -> packed_module module_name value
      | Pat_exception (exception_, ext, value) ->
          after (keyword exception_ ext) (pattern value)
      | Pat_effect (effect_, value, comma, continuation) ->
          after
            (tok effect_)
            ( pack_list_isolating_multiline
                ~indent:0 [ pattern value ++ tok comma; pattern continuation ]
            )
      | Pat_extension value -> extension value
      | Pat_open (path, dot, value) ->
          module_path path ++ tok dot ++ glued_pattern value
      | Pat_paren value -> paren shifted_pattern value

    and record_pattern ?inline { left; content = fields, wildcard; right } =
      let suffix =
        opt (fun (underscore, semi) -> tok underscore ++ opt tok semi) wildcard
      in
      record_literal
        ~suffix ?inline record_pattern_field { left; content = fields; right }

    (** A pattern glued after [M.]: its bracket does not start its line, so its
      content is on the next lines, as after [\[|]. *)
    and glued_pattern (value : pattern) =
      match value with
      | { node = Pat_paren value; attributes = [] } ->
          paren ~inline:false shifted_pattern value
      | { node = Pat_list value; attributes = [] } ->
          list_literal ~inline:false bracket_pattern value
      | { node = Pat_record value; attributes = [] } ->
          record_pattern ~inline:false value
      | value -> pattern value

    (** An element of a tuple pattern, in a bracket when [in_bracket]. *)
    and tuple_pattern_element ~in_bracket =
      function
      | Tuple_pattern value ->
          if in_bracket then bracket_pattern value else pattern value
      | Tuple_pattern_label (label, value) ->
          labelled (tok label) (pattern value)
      | Tuple_pattern_pun (tilde, name) -> tok tilde ++ tok name
      | Tuple_pattern_pun_constraint (tilde, value) ->
          glued_paren
            (tok tilde)
            ( fun (name, colon, typ) ->
                binding_head ~constraint_:(colon_type (colon, typ)) (tok name)
            )
            value

    and record_pattern_field ((label, typ, value) : record_pattern_field) =
      let head = longident tok label in
      match value with
      | None -> constrained ?constraint_:(Option.map colon_type typ) head
      | Some (equal, value) ->
          binding
            ~head ~parts_indent:0 ?constraint_:(Option.map colon_type typ)
            equal ~body:(pattern value)

    (** [: t] for [binding]. *)
    and colon_type (colon, typ) = colon, core_type typ

    and poly_pattern { left; content = value, colon, typ; right } =
      constrained_paren
        ~constraint_:(colon, core_type typ) ~left (pattern value) ~right

    (** [(module M : S)]. *)
    and packed_module : 'a. ('a -> t) -> 'a packed_module paren -> t =
      fun print { left; content = module_, ext, value, package; right } ->
        constrained_paren
          ?constraint_:
            (Option.map (fun (colon, typ) -> colon, module_type typ) package)
          ~left (keyword module_ ext +- print value) ~right

    and newtypes ((type_, names) : newtypes) =
      pack_after_head ~indent:2 (tok type_) (List.map tok names)

    (* {2 Expressions} *)

    and expr value = with_attributes (expr_node ~in_bracket:false) value

    (** An expression which starts where the content of a bracket starts, as
      in [( e )] or [\[ e; ... \]] (see [operator_chain]). *)
    and bracket_expr (value : expr) =
      match value with
      | { node; attributes = [] } -> expr_node ~in_bracket:true node
      | value -> expr value

    (** The operands of the chain of [left operator right], with the operators
      of the same level, associativity and kind as [operator]: the first
      operand, then each operator and the operand after it. *)
    and infix_operands left (operator : operator_token) right =
      let level = operator_level operator
      and kind = operator_kind operator
      in
      let same (other : operator_token) =
        operator_level other = level && operator_kind other = kind
      in
      match snd level with
      | Left ->
          (* [((a + b) + c) + d]: the operands from the last one. *)
          let rec operands rest (value : expr) =
            match value with
            | { node = Exp_infix (left, operator, right); attributes = [] } when
                same operator
            -> operands ((operator, expr right) :: rest) left
            | _ -> expr value, rest
          in
          operands [ operator, expr right ] left
      | Right ->
          (* [a + (b + (c + d))]. *)
          let rec operands rest operator (value : expr) =
            match value with
            | { node = Exp_infix (left, next, right); attributes = [] } when
                same next
            -> operands ((operator, expr left) :: rest) next right
            | _ -> List.rev ((operator, expr value) :: rest)
          in
          expr left, operands [] operator right

    (** [e#m#n]: [e], then each call, [#m] and [#n]. *)
    and method_calls (value : expr) =
      match value with
      | { node = Exp_send (value, hash, name); attributes = [] } ->
          method_calls value @ [ tok hash ++ tok name ]
      | _ -> [ expr value ]

    (** As [infix_operands], for [left :: right]. *)
    and cons_operands left cons right =
      let rec operands rest cons (value : expr) =
        match value with
        | { node = Exp_cons (left, next, right); attributes = [] } ->
            operands ((cons, expr left) :: rest) next right
        | _ -> List.rev ((cons, expr value) :: rest)
      in
      expr left, operands [] cons right

    and sequence (value : expr) =
      match value with
      | { node = Exp_sequence (left, semi, right); attributes = [] } ->
          separator_after (expr left) (tok semi) :: sequence right
      | { node = Exp_sequence_ext (left, semi, percent, id, right);
          attributes = []
        }
      ->
          begin match sequence right with
          | first :: rest ->
                separator_after (expr left) (tok semi)
              :: (tok percent ++ attr_id id +- first) :: rest
          | [] -> []
          end
      | _ -> [ expr value ]

    and application (function_ : expr) arguments =
      let function_ = expr function_
      and arguments = List.map argument arguments
      in
      pack_after_head ~indent:2 function_ arguments

    and if_then_else (if_, ext, condition, then_, if_true, if_false) =
      (* A branch after [then] or [else]: [begin] on their line, and [end]
         in the column of [if]. *)
      let branch head (value : expr) =
        match value with
        | { node = Exp_begin _; attributes = [] } -> head +- expr value
        | _ -> after head (expr value)
      in
      (* [if c then a], and the parts after it: each [else if c then a] of
         the chain, then [else b]. *)
      let rec chain (if_, ext, condition, then_, if_true, if_false) =
        let head = keyword if_ ext
        and condition = expr condition
        and then_ = tok then_
        in
        (* [if c then] on one line whenever it fits, even when the branch
           then takes more lines: [c] alone on its lines only as a last
           resort. *)
        let head =
            flat (head +- condition +- then_)
          <|> as_last_resort (head +/ condition +| then_)
        in
        let then_part = branch head if_true in
        match if_false with
        | None -> then_part, []
        | Some
            ( else_,
              { node = Exp_if (if_, ext, condition, then_, if_true, if_false);
                attributes = []
              }
            )
        ->
            let first, rest =
              chain (if_, ext, condition, then_, if_true, if_false)
            in
            then_part, (tok else_ +- first) :: rest
        | Some (else_, if_false) -> then_part, [ branch (tok else_) if_false ]
      in
      (* The whole chain on one line, or each part on its line: not the first
         parts on lines of their own and the next ones on one line. *)
      match chain (if_, ext, condition, then_, if_true, if_false) with
      | then_part, [] -> then_part
      | then_part, else_parts -> lines (then_part :: else_parts)

    and match_cases values = vertical (cases case values)

    (** [p when e -> e]. *)
    and case ((lhs, guard, arrow, rhs) : Cst.case) : L.case =
      ~head:(guarded_pattern lhs guard), ~arrow:(Some arrow), ~body:(expr rhs)

    (** [match e with] and its cases: one case may be on the same line. *)
    and with_cases head ((_, { rest; _ }) as values : Cst.case bar_list) =
      with_cases_doc head (match_cases values) ~single:(rest = [])

    and with_cases_doc head cases ~single =
      if single then flat (head +- cases) <|> (head +| cases) else head +| cases

    (** [if e then], [match e with]...: on one line, or [e] on the next lines,
      indented, and [last] on the line after them. *)
    and keyword_head keyword value last =
      flat (keyword +- value +- last) <|> (keyword +/ value +| last)

    (** [let ... in body]: [in] on the line of the bindings, or below [let]. *)
    and let_in bindings in_ body =
      let in_ = tok in_ in
      ((flat bindings +- in_) <|> (bindings +| in_)) +| body

    and expr_node ~in_bracket node =
      match node with
      | Exp_ident path -> longident val_ident path
      | Exp_constant value -> tok value
      | Exp_construct (constructor, argument) ->
          after (longident constr_ident constructor) (opt expr argument)
      | Exp_variant (tag, argument) -> after (name_tag tag) (opt expr argument)
      | Exp_paren value -> paren bracket_expr value
      | Exp_constraint { left; content = value, constraint_; right } ->
          let constraint_, coercion = type_constraint_parts constraint_ in
          constrained_paren
            ?constraint_ ?coercion ~left (bracket_expr value) ~right
      | Exp_begin { left; content = ext, body; right } ->
          let left = keyword left ext
          and right = tok right
          in
          closed @@
            begin match body with
            | None -> left +- right
            | Some
                ( { node = Exp_match _ | Exp_try _ | Exp_function _;
                    attributes = []
                  }
                  as body
                )
            ->
                let body = expr body in
                (* After comments on lines of their own, the body on the
                   next lines, indented. *)
                  flat (left +- body +- right)
                <|> (inline left body +| right) <|> (left +/ body +| right)
            | Some body ->
                let body = expr body in
                (* [begin fun], [begin if]...: the keyword on the line of [begin],
                and the rest as after [( ]. *)
                let block = bracket ~spaces:true ~inline:false left body right
                in
                (* Not before another [begin]: [begin begin] would hide the
                  second one. *)
                let begins = first_kind body = Some `BEGIN in
                (* [let] always starts the line after [begin], even when all
                   fits on one line: [begin let x = e in], then the body
                   indented by 2, would hide the body after [let]. *)
                if first_kind body = Some `LET then left +/ body +| right
                else if
                    starts_with_keyword body && not begins
                  && not (starts_with_comments_of_previous_line body)
                then
                    flat (left +- body +- right)
                  <|> (inline left (nest 2 body) +| right) <|> block
                else block
            end
      | Exp_apply (function_, arguments) -> application function_ arguments
      | Exp_infix (left, operator, right) ->
          let first, rest = infix_operands left operator right in
          operator_chain ~in_bracket (operator_kind operator) first rest
      | Exp_prefix (operator, value) -> tok operator ++ expr value
      | Exp_unary (operator, value) -> tok operator ++ expr value
      | Exp_tuple elements ->
          (* In a bracket, the elements are where its content starts. *)
          pack_list_isolating_multiline
            ~indent:0 (separated_after (tuple_element ~in_bracket) elements)
      | Exp_cons (left, cons, right) ->
          let first, rest = cons_operands left cons right in
          operator_chain ~in_bracket (operator_kind cons) first rest
      | Exp_field (record, dot, label) ->
          expr record ++ tok dot ++ longident tok label
      | Exp_setfield (record, dot, label, arrow, value) ->
          operator_chain
            ~in_bracket (operator_kind arrow)
            (expr record ++ tok dot ++ longident tok label)
            [ arrow, expr value ]
      | Exp_index (array, dot, index, assign) ->
          let dot =
            match dot with
            | Index_builtin dot -> tok dot
            | Index_user (qualifier, dotop) ->
                  opt (fun (dot, path) -> tok dot ++ module_path path) qualifier
                ++ tok dotop
          in
          let index =
            match index with
            | Parens value ->
                paren
                  ~inline:false
                  ( fun values ->
                      pack_list_isolating_multiline
                        ~indent:0 (semis bracket_expr values)
                  )
                  value
            | Brackets value ->
                paren
                  ~inline:false
                  ( fun values ->
                      pack_list_isolating_multiline
                        ~indent:0 (semis bracket_expr values)
                  )
                  value
            | Braces value ->
                paren
                  ~inline:false
                  ( fun values ->
                      pack_list_isolating_multiline
                        ~indent:0 (semis bracket_expr values)
                  )
                  value
          in
          let value = expr array ++ dot ++ index in
          begin match assign with
          | None -> value
          | Some (arrow, assigned) ->
              operator_chain
                ~in_bracket (operator_kind arrow) value [ arrow, expr assigned ]
          end
      | Exp_setinstvar (name, arrow, value) ->
          operator_chain
            ~in_bracket (operator_kind arrow) (tok name) [ arrow, expr value ]
      | Exp_send _ ->
          pack_list_isolating_multiline
            ~glued:true ~indent:2 (method_calls { node; attributes = [] })
      | Exp_let (bindings, in_, body) ->
          let_in (let_bindings bindings) in_ (expr body)
      | Exp_letop (let_op, { first; rest }, in_, body) ->
          let bindings =
            vertical
              ( letop_binding (tok let_op) first
              ::
                List.map
                  (fun (and_op, binding) -> letop_binding (tok and_op) binding)
                  rest
              )
          in
          let_in bindings in_ (expr body)
      | Exp_struct_item (let_, ext, item, in_, body) ->
          let_in
            (keyword let_ ext +- documented structure_item item) in_ (expr body)
      | Exp_fun (fun_, ext, params, constraint_, arrow, body) ->
          let params =
              List.map function_param params
            @
              [ opt
                  (fun (colon, typ) -> tok colon +- nest 2 (core_type typ))
                  constraint_
              ]
          in
          fun_like ~head:(keyword fun_ ext) ~params arrow ~body:(expr body)
      | Exp_function (function_, ext, values) ->
          with_cases (keyword function_ ext) values
      | Exp_match (match_, ext, scrutinee, with_, values) ->
          let head = keyword match_ ext in
          let head = keyword_head head (expr scrutinee) (tok with_) in
          with_cases head values
      | Exp_try (try_, ext, body, with_, values) ->
          let head = keyword try_ ext in
          let head = keyword_head head (expr body) (tok with_) in
          with_cases head values
      | Exp_if (if_', ext, condition, then_, if_true, if_false) ->
          if_then_else (if_', ext, condition, then_, if_true, if_false)
      | Exp_while
          ( while_, ext, condition,
            { left = do_; content = body; right = done_ }
          )
      ->
          let head = keyword while_ ext in
          let head = keyword_head head (expr condition) (tok do_) in
          let body = expr body
          and done_ = tok done_
          in
          closed (flat (head +- body +- done_) <|> (head +/ body +| done_))
      | Exp_for
          ( for_, ext, index, equal, start, direction, stop,
            { left = do_; content = body; right = done_ }
          )
      ->
          let head =
            for_head
              ~keyword:(keyword for_ ext)
              ~index:(pattern index) equal ~start:(expr start)
              ~direction:(tok direction) ~stop:(expr stop) do_
          in
          let body = expr body
          and done_ = tok done_
          in
          closed (flat (head +- body +- done_) <|> (head +/ body +| done_))
      | Exp_assert (assert_, ext, value) ->
          after (keyword assert_ ext) (expr value)
      | Exp_lazy (lazy_, ext, value) -> after (keyword lazy_ ext) (expr value)
      | Exp_sequence _ | Exp_sequence_ext _ ->
          lines (sequence { node; attributes = [] })
      | Exp_semi (value, semi) -> separator_after (expr value) (tok semi)
      | Exp_new (new_, ext, path) -> keyword new_ ext +- longident tok path
      | Exp_pack value -> packed_module module_expr value
      | Exp_object { left; content = ext, body; right } ->
          class_structure (keyword left ext) body (tok right)
      | Exp_override { left; content; right } ->
          let content =
            opt
              ( fun values ->
                  pack_list_isolating_multiline
                    ~indent:0 (semis object_expr_field values)
              )
              content
          in
          bracket ~spaces:true ~inline:false (tok left) content (tok right)
      | Exp_local_open (path, dot, value) ->
          module_path path ++ tok dot ++ glued_expr value
      | Exp_record value -> record_expr value
      | Exp_array value -> array_literal bracket_expr value
      | Exp_list value -> list_literal bracket_expr value
      | Exp_extension value -> extension value
      | Exp_unreachable dot -> tok dot

    and record_expr ?inline { left; content = base, fields; right } =
      record_literal
        ?base:(Option.map (fun (base, with_) -> bracket_expr base, with_) base)
        ?inline record_field { left; content = fields; right }

    (** An expression glued after [M.]: its bracket does not start its line, so
      its content is on the next lines, as after [\[|]. *)
    and glued_expr (value : expr) =
      match value with
      | { node = Exp_paren value; attributes = [] } ->
          paren ~inline:false bracket_expr value
      | { node = Exp_list value; attributes = [] } ->
          list_literal ~inline:false bracket_expr value
      | { node = Exp_record value; attributes = [] } ->
          record_expr ~inline:false value
      | value -> expr value

    (** An element of a tuple, in a bracket when [in_bracket] (see
      [bracket_expr]). *)
    and tuple_element ~in_bracket =
      function
      | Tuple_expr (~label, value) ->
          ( match label with
              None -> if in_bracket then bracket_expr value else expr value
            | Some label -> labelled (tok label) (expr value)
          )
      | Tuple_pun (tilde, name) -> tok tilde ++ tok name
      | Tuple_pun_constraint (tilde, value) ->
          glued_paren (tok tilde) name_constraint value

    and argument =
      function
      | Arg_expr value -> expr value
      | Arg_label (label, value) -> labelled (tok label) (expr value)
      | Arg_pun (tilde, name) -> tok tilde ++ tok name
      | Arg_pun_constraint (tilde, value) ->
          glued_paren (tok tilde) name_constraint value
      | Arg_optional_pun (question, name) -> tok question ++ tok name
      | Arg_optlabel (label, value) -> labelled (tok label) (expr value)

    (** [x : t] in [~(x : t)]. *)
    and name_constraint (name, constraint_) =
      let constraint_, coercion = type_constraint_parts constraint_ in
      binding_head ?constraint_ ?coercion (tok name)

    (** [~(x : t)], [?(x : t = e)]: on one line, or the content glued to the
        bracket (2 columns after [~]), its next lines indented by 2, and [)]
        alone in the column of [~]. *)
    and glued_paren
      : 'a. t -> ('a -> t) -> ([ `LPAREN ], 'a, [ `RPAREN ]) Cst.delimited -> t
      =
      fun prefix print { left; content; right } ->
        let left = prefix ++ tok left
        and content = print content
        and right = tok right
        in
        flat (left ++ content ++ right) <|> (left ++ content +| right)

    (** The [: t] and [:> u] of a constraint, for [binding]. *)
    and type_constraint_parts =
      function
      | Constraint (colon, typ) -> Some (colon, core_type typ), None
      | Coerce (ground, coerce, typ) ->
          Option.map colon_type ground, Some (coerce, core_type typ)

    (** As [type_constraint_parts], for an optional constraint. *)
    and optional_constraint_parts =
      function
      | Some constraint_ -> type_constraint_parts constraint_
      | None -> None, None

    and record_field ((label, constraint_, value) : record_field) =
      let head = longident tok label in
      let constraint_, coercion = optional_constraint_parts constraint_ in
      match value with
      | None -> constrained ?constraint_ ?coercion head
      | Some (equal, value) ->
          binding
            ~head ~parts_indent:0 ?constraint_
            ?coercion equal ~body:(expr value)

    and object_expr_field ((name, value) : object_expr_field) =
      match value with
      | None -> tok name
      | Some (equal, value) -> binding ~head:(tok name) equal ~body:(expr value)

    and let_bindings ((let_, ext, rec_, bindings) : let_bindings) =
      let first, _ = bindings in
      and_list
        (let_binding (keyword let_ ext +- opt tok rec_) first)
        let_binding bindings

    (** A binding after [prefix] ([let], [and]). *)
    and let_binding prefix ((body, post_attributes) : let_binding) =
      item_attributes (let_binding_body prefix body) post_attributes

    (** [name params : t = body]: the head on one line, followed by the body,
      or the parameters one per line, indented by 4, then the type of the
      result and [=], each on its line, indented by 2, and the body below [=]. *)
    and function_binding
        ?keyword name ((params, constraint_, equal, body) : function_binding)
      =
        let constraint_, coercion = optional_constraint_parts constraint_ in
        binding
          ~indent:4 ?keyword ~head:name ~params:(List.map function_param params)
          ?constraint_ ?coercion equal ~body:(expr body)

    and newtype_binding
        ?keyword name ((colon, types, dot, typ, equal, value) : newtype_binding)
      =
        binding
          ?keyword ~head:name
          ~constraint_:
            (colon, after (newtypes types ++ tok dot) (core_type typ))
          equal ~body:(expr value)

    and let_binding_body keyword =
      function
      | Lb_pattern (value, equal, body) ->
          binding ~keyword ~head:(pattern value) equal ~body:(expr body)
      | Lb_pattern_constraint (value, colon, typ, equal, body) ->
          binding
            ~keyword ~head:(pattern value)
            ~constraint_:(colon_type (colon, typ)) equal ~body:(expr body)
      | Lb_function (name, value) ->
          function_binding ~keyword (val_ident name) value
      | Lb_constrained (name, constraint_, equal, value) ->
          let constraint_, coercion = type_constraint_parts constraint_ in
          binding
            ~keyword ~head:(val_ident name) ?constraint_
            ?coercion equal ~body:(expr value)
      | Lb_poly (name, colon, variables, dot, typ, equal, value) ->
          binding
            ~keyword ~head:(val_ident name)
            ~constraint_:
              (colon, after (type_variables_dot variables dot) (core_type typ))
            equal ~body:(expr value)
      | Lb_newtypes (name, value) ->
          newtype_binding ~keyword (val_ident name) value
      | Lb_pun name -> keyword +- val_ident name

    (** A binding after [keyword] ([let*], [and*]). *)
    and letop_binding keyword =
      function
      | Lop_function (name, value) ->
          function_binding ~keyword (val_ident name) value
      | Lop_pun name -> keyword +- val_ident name
      | Lop_constraint (value, colon, typ, equal, body) ->
          binding
            ~keyword ~head:(pattern value)
            ~constraint_:(colon_type (colon, typ)) equal ~body:(expr body)
      | Lop_pattern (value, equal, body) ->
          binding ~keyword ~head:(pattern value) equal ~body:(expr body)

    and function_param =
      function
      | Param_newtypes value -> paren newtypes value
      | Param_value value -> param_pattern value

    and label_pattern ((name, typ) : label_pattern) =
      binding_head ?constraint_:(Option.map colon_type typ) (tok name)
    and param_pattern =
      function
      | Param_optional_default (question, value) ->
          glued_paren
            (tok question)
            ( fun (((name, typ) : label_pattern), default) ->
                let constraint_ = Option.map colon_type typ in
                match default with
                | Some (equal, value) ->
                    binding
                      ~head:(tok name) ?constraint_ equal ~body:(expr value)
                | None -> binding_head ?constraint_ (tok name)
            )
            value
      | Param_optional (question, name) -> tok question ++ tok name
      | Param_optlabel_default (optlabel, value) ->
            tok optlabel
          ++
            paren
              ~inline:false
              ( fun (((value, typ) : let_pattern), default) ->
                  let constraint_ = Option.map colon_type typ in
                  match default with
                  | Some (equal, default) ->
                      binding
                        ~head:(pattern value) ?constraint_
                        equal ~body:(expr default)
                  | None -> binding_head ?constraint_ (pattern value)
              )
              value
      | Param_optlabel (optlabel, value) -> tok optlabel ++ tok value
      | Param_labelled_paren (tilde, value) ->
          glued_paren (tok tilde) label_pattern value
      | Param_labelled (tilde, name) -> tok tilde ++ tok name
      | Param_label (label, value) -> labelled (tok label) (pattern value)
      | Param_label_poly (label, value) ->
          labelled (tok label) (poly_pattern value)
      | Param_pattern value -> pattern value
      | Param_poly value -> poly_pattern value

    (* {2 Classes} *)

    and class_arguments (arguments : class_arguments) =
      opt
        ( fun { left; content; right } ->
            bracket
              (tok left)
              ( pack_list_isolating_multiline
                  ~indent:0 (commas core_type content)
              )
              (tok right)
        )
        arguments

    and class_parameters (parameters : class_parameters) =
      opt
        ( fun { left; content; right } ->
            bracket
              (tok left)
              ( pack_list_isolating_multiline
                  ~indent:0 (commas type_parameter content)
              )
              (tok right)
        )
        parameters

    and class_expr value = with_attributes class_expr_node value

    and class_expr_node =
      function
      | Cl_constr (arguments, path) ->
          class_arguments arguments +- longident tok path
      | Cl_structure { left; content = values, body; right } ->
          class_structure (tok left ++ attributes values) body (tok right)
      | Cl_fun (fun_, values, params, arrow, body) ->
          fun_like
            ~head:(tok fun_ ++ attributes values)
            ~params:(List.map param_pattern params)
            arrow ~body:(class_expr body)
      | Cl_apply (value, arguments) ->
          pack_after_head
            ~indent:2 (class_expr value) (List.map argument arguments)
      | Cl_let (bindings, in_, body) ->
          let_in (let_bindings bindings) in_ (class_expr body)
      | Cl_open value -> let_open class_expr value
      | Cl_constraint { left; content = value, colon, typ; right } ->
          constrained_paren
            ~constraint_:(colon, class_type typ) ~left (class_expr value) ~right
      | Cl_extension value -> extension value
      | Cl_paren value -> paren class_expr value

    and let_open : 'a. ('a -> t) -> 'a let_open -> t =
      fun print (let_, open_, bang, values, path, in_, body) ->
        let head =
            glued_list [ tok let_ +- tok open_; opt tok bang ]
          ++ attributes values +- module_path path
        in
        let_in head in_ (print body)

    (** [object (self) fields end]. *)
    and class_structure left ((self, fields) : class_structure) right =
      let self =
        opt
          ( function
              Self_pattern value -> paren pattern value
            | Self_constraint value -> poly_pattern value
          )
          self
      in
      block
        ~items:(List.length fields) (left +- self)
        (elements class_field fields) right

    (** [left content right], for [struct], [sig] and [object], with [items]
        items in [content]: on one line only for one item at most. *)
    and block ~items left content right =
      if is_empty content then left +- right
      else
        let lines = left +/ content +| right in
        if items >= 2 then lines else flat (left +- content +- right) <|> lines

    and class_field =
      function
      | Cf_inherit (inherit_, bang, values, value, alias, ~post_attributes) ->
          let value =
            match alias with
            | None -> class_expr value
            | Some (as_, name) -> L.alias (class_expr value) as_ (tok name)
          in
          item_attributes
            (after (tok inherit_ ++ opt tok bang ++ attributes values) value)
            post_attributes
      | Cf_val (val_, bang, values, flags, name, body, ~post_attributes) ->
          let head =
            words
              (   (tok val_ ++ opt tok bang ++ attributes values)
                :: List.map tok flags
              @ [ tok name ]
              )
          in
          let body =
            match body with
            | Value_virtual (colon, typ) ->
                after (head +- tok colon) (core_type typ)
            | Value_concrete (constraint_, equal, value) ->
                let constraint_, coercion =
                  optional_constraint_parts constraint_
                in
                binding ~head ?constraint_ ?coercion equal ~body:(expr value)
          in
          item_attributes body post_attributes
      | Cf_method (method_, bang, values, flags, name, body, ~post_attributes)
      ->
          let head =
            words
              (   (tok method_ ++ opt tok bang ++ attributes values)
                :: List.map tok flags
              @ [ tok name ]
              )
          in
          let body =
            match body with
            | Method_virtual (colon, typ) ->
                after (head +- tok colon) (core_type typ)
            | Method_strict binding -> function_binding head binding
            | Method_poly (colon, typ, equal, value) ->
                binding
                  ~head ~constraint_:(colon_type (colon, typ))
                  equal ~body:(expr value)
            | Method_newtypes binding -> newtype_binding head binding
          in
          item_attributes body post_attributes
      | Cf_constraint
          (constraint_, values, left, equal, right, ~post_attributes)
      ->
          item_attributes
            ( binding
                ~head:(tok constraint_ ++ attributes values +- core_type left)
                equal ~body:(core_type right)
            )
            post_attributes
      | Cf_initializer (initializer_, values, value, ~post_attributes) ->
          item_attributes
            (after (tok initializer_ ++ attributes values) (expr value))
            post_attributes
      | Cf_extension (value, post_attributes) ->
          item_attributes (extension value) post_attributes
      | Cf_attribute value -> attribute value

    (** A class type, its arrows packed as those of a type. *)
    and class_type value =
      match value with
      | Cty_signature value -> class_signature value
      | Cty_arrow _ ->
          let first, rest = class_type_parts value in
          arrow_type first rest

    (** As [arrow_parts]. *)
    and class_type_parts value =
      match value with
      | Cty_arrow (label, domain, arrow, codomain) ->
          let next, rest = class_type_parts codomain in
          arrow_label label ++ core_type domain, (~arrow, ~typ:next) :: rest
      | value -> class_type value, []

    and class_signature value = with_attributes class_signature_node value

    and class_signature_node =
      function
      | Csig_constr (arguments, path) ->
          class_arguments arguments +- longident tok path
      | Csig_extension value -> extension value
      | Csig_object { left; content = values, self, fields; right } ->
          let left = tok left ++ attributes values +- opt (paren core_type) self
          in
          block
            ~items:(List.length fields) left
            (elements class_sig_field fields) (tok right)
      | Csig_open value -> let_open class_signature value

    and class_sig_field =
      function
      | Ctf_inherit (inherit_, values, typ, ~post_attributes) ->
          item_attributes
            (after (tok inherit_ ++ attributes values) (class_signature typ))
            post_attributes
      | Ctf_val (val_, values, flags, name, colon, typ, ~post_attributes) ->
          let head =
            words
              ( (tok val_ ++ attributes values) :: List.map tok flags
                @ [ tok name; tok colon ]
              )
          in
          item_attributes (after head (core_type typ)) post_attributes
      | Ctf_method (method_, values, flags, name, colon, typ, ~post_attributes)
      ->
          let head =
            words
              ( (tok method_ ++ attributes values) :: List.map tok flags
                @ [ tok name; tok colon ]
              )
          in
          item_attributes (after head (core_type typ)) post_attributes
      | Ctf_constraint
          (constraint_, values, left, equal, right, ~post_attributes)
      ->
          item_attributes
            ( binding
                ~head:(tok constraint_ ++ attributes values +- core_type left)
                equal ~body:(core_type right)
            )
            post_attributes
      | Ctf_extension (value, post_attributes) ->
          item_attributes (extension value) post_attributes
      | Ctf_attribute value -> attribute value

    (** [class [ 'a ] c = ...] after [prefix] ([class], with [virtual]): the head on one line,
      or, when its parameters are on several lines, [prefix] then the
      parameters and the name on the next lines, indented, as in
      [type_declaration]. *)
    and class_info : 'body. ('body -> t -> t -> t) -> t -> 'body class_info -> t
      =
      fun body prefix (virtual_, parameters, name, value, post_attributes) ->
        let prefix = prefix +- opt tok virtual_
        and head = class_parameters parameters +- tok name
        in
        item_attributes (body value prefix head) post_attributes

    (** [prefix head separator body] ([class c = e]), as a type declaration: on
      one line, laid out by [rhs] from its first line ([class c =]); or, when
      [head] is on several lines, after [prefix] on the next lines, indented,
      with [separator] and [body] on the line after it if they fit on one
      line, else [separator] alone on its line, as in a let binding. *)
    and class_description_body
        ((colon, typ) : class_description_body) keyword head
      = binding_head ~keyword ~constraint_:(colon, class_type typ) head

    and class_type_declaration_body
        ((equal, typ) : class_type_declaration_body) keyword head
      = binding ~keyword ~head equal ~body:(class_signature typ)

    (** [class c x y : t = e] after [keyword]: the parameters packed as those
        of a function. *)
    and class_binding value keyword head =
      let rec split params =
        function
        | Class_param (param, value) ->
            split (param_pattern param :: params) value
        | Class_equal (equal, value) -> List.rev params, None, equal, value
        | Class_constraint (colon, typ, equal, value) ->
            (List.rev params, Some (colon, class_type typ), equal, value)
      in
      let params, constraint_, equal, value = split [] value in
      binding
        ~indent:4 ~keyword ~head ~params
        ?constraint_ equal ~body:(class_expr value)

    (* {2 Modules} *)

    and functor_arg =
      function
      | Functor_unit { left; right; _ } -> tok left ++ tok right
      | Functor_named value ->
          let argument (name, colon, typ) =
            (* Inside the parentheses, the type in the column of the name. *)
            after_line (module_name name +- tok colon) (module_type typ)
          in
          paren argument value

    and module_expr value = with_attributes module_expr_node value

    and module_expr_node =
      function
      | Mod_ident path -> module_path path
      | Mod_structure { left; content = values, items; right } ->
          block
            ~items:(List.length items) (tok left ++ attributes values)
            (structure items) (tok right)
      | Mod_functor (functor_, values, args, arrow, body) ->
          fun_like
            ~head:(tok functor_ ++ attributes values)
            ~params:(List.map functor_arg args) arrow ~body:(module_expr body)
      | Mod_apply (value, argument) ->
          (* [F (X) (Y)]: the functor and its arguments, as in an application. *)
          let rec functor_and_arguments (value : module_expr) =
            match value with
            | { node = Mod_apply (value, argument); attributes = [] } ->
                let functor_, arguments = functor_and_arguments value in
                functor_, arguments @ [ module_expr argument ]
            | _ -> module_expr value, []
          in
          let functor_, arguments = functor_and_arguments value in
          pack_after_head
            ~indent:2 functor_ (arguments @ [ module_expr argument ])
      | Mod_apply_unit (value, { left; right; _ }) ->
          module_expr value ++ (tok left ++ tok right)
      | Mod_constraint { left; content = value, colon, typ; right } ->
          constrained_paren
            ~constraint_:(colon, module_type typ)
            ~left (module_expr value) ~right
      | Mod_paren value -> paren module_expr value
      | Mod_unpack { left; content = val_, values, value, package; right } ->
          let constraint_, coercion =
            match package with
            | No_package -> None, None
            | Package_constraint (colon, typ) ->
                Some (colon, module_type typ), None
            | Package_coerce (ground, coerce, typ) ->
                ( Option.map (fun (colon, typ) -> colon, module_type typ) ground
                  ,
                  Some (coerce, module_type typ)
                )
          in
          constrained_paren
            ?constraint_ ?coercion ~left
            (after (tok val_ ++ attributes values) (expr value)) ~right
      | Mod_extension value -> extension value

    and module_type value = with_attributes module_type_node value

    (** As [arrow_parts]. *)
    and module_type_parts (value : module_type) =
      match value with
      | { node = Mty_arrow (left, arrow, right); attributes = [] } ->
          let next, rest = module_type_parts right in
          module_type left, (~arrow, ~typ:next) :: rest
      | value -> module_type value, []

    and module_type_node =
      function
      | Mty_ident path -> longident tok path
      | Mty_signature { left; content = values, items; right } ->
          block
            ~items:(List.length items) (tok left ++ attributes values)
            (signature items) (tok right)
      | Mty_functor (functor_, args, arrow, body) ->
          let head =
            opt
              (fun (functor_, values) -> tok functor_ ++ attributes values)
              functor_
          in
          fun_like
            ~head ~params:(List.map functor_arg args)
            arrow ~body:(module_type body)
      | Mty_arrow (left, arrow, right) ->
          let next, rest = module_type_parts right in
          arrow_type (module_type left) ((~arrow, ~typ:next) :: rest)
      | Mty_with (typ, with_, constraints) ->
          let constraints =
            match constraints with
            | Zero -> []
            | Many constraints -> separated_before with_constraint constraints
          in
          let constraints =
            match constraints with
            | first :: rest -> (tok with_ +- first) :: rest
            | [] -> [ tok with_ ]
          in
          (* As an application: the constraints after the type, packed. *)
          pack_after_head ~indent:2 (module_type typ) constraints
      | Mty_typeof (module_, type_, of_, values, value) ->
          after
            (words [ tok module_; tok type_; tok of_ ] ++ attributes values)
            (module_expr value)
      | Mty_extension value -> extension value
      | Mty_paren value -> paren module_type value

    and with_constraint =
      function
      | With_type (type_, parameters, path, equal, private_, typ, constraints)
      ->
          let head =
            words [ tok type_; type_parameters parameters; longident tok path ]
          in
          constraint_clauses
            (binding ~head equal ~body:(opt tok private_ +- core_type typ))
            constraints
      | With_typesubst (type_, parameters, path, colonequal, typ) ->
          after
            ( words
                [ tok type_; type_parameters parameters;
                  longident tok path; tok colonequal
                ]
            )
            (core_type typ)
      | With_module (module_, path, equal, target) ->
          binding
            ~head:(tok module_ +- module_path path)
            equal ~body:(module_path target)
      | With_modtype (module_, type_, path, equal, typ) ->
          binding
            ~head:(words [ tok module_; tok type_; longident tok path ])
            equal ~body:(module_type typ)

    (** [value constraint t = u constraint ...]: each [constraint] on its line,
      indented. *)
    and constraint_clauses value constraints =
      List.fold_left
        (fun value clause -> value +/ type_constraint_clause clause)
        value constraints

    and type_constraint_clause
        ((constraint_, left, equal, right) : type_constraint_clause)
      =
        binding
          ~head:(tok constraint_ +- core_type left)
          equal ~body:(core_type right)

    (* {2 Signatures and structures} *)

    and signature items = elements ~items:true signature_item items

    and signature_item =
      function
      | Sig_value value -> value_description value
      | Sig_type value | Sig_typesubst value -> type_declarations value
      | Sig_typext value -> type_extension value
      | Sig_exception value -> exception_declaration value
      | Sig_module (module_, ext, declaration) ->
          module_declaration (keyword module_ ext) declaration
      | Sig_modsubst (module_, ext, name, colonequal, path, post_attributes) ->
          item_attributes
            ( after
                (keyword module_ ext +- tok name +- tok colonequal)
                (module_path path)
            )
            post_attributes
      | Sig_recmodule (module_, ext, rec_, declarations) ->
          let first, _ = declarations in
          and_list
            (module_declaration (keyword module_ ext +- tok rec_) first)
            module_declaration declarations
      | Sig_modtype value | Sig_modtypesubst value ->
          module_type_declaration value
      | Sig_open value -> open_infos module_path value
      | Sig_include value -> include_infos module_type value
      | Sig_class (class_, ext, values) ->
          let first, _ = values in
          and_list
            (class_info class_description_body (keyword class_ ext) first)
            (class_info class_description_body) values
      | Sig_class_type (class_, type_, ext, values) ->
          let first, _ = values in
          and_list
            ( class_info
                class_type_declaration_body
                (tok class_ +- keyword type_ ext) first
            )
            (class_info class_type_declaration_body) values
      | Sig_attribute value -> attribute value
      | Sig_extension (value, post_attributes) ->
          item_attributes (extension value) post_attributes
      | Sig_semisemi value -> tok value

    and structure items = elements ~items:true structure_item items

    and structure_item =
      function
      | Str_eval (value, post_attributes) ->
          item_attributes (expr value) post_attributes
      | Str_value bindings -> let_bindings bindings
      | Str_primitive value -> value_description value
      | Str_type value -> type_declarations value
      | Str_typext value -> type_extension value
      | Str_exception value -> exception_declaration value
      | Str_module (module_, ext, binding) ->
          module_binding (keyword module_ ext) binding
      | Str_recmodule (module_, ext, rec_, bindings) ->
          let first, _ = bindings in
          and_list
            (module_binding (keyword module_ ext +- tok rec_) first)
            module_binding bindings
      | Str_modtype value -> module_type_declaration value
      | Str_open value -> open_infos module_expr value
      | Str_class (class_, ext, values) ->
          let first, _ = values in
          and_list
            (class_info class_binding (keyword class_ ext) first)
            (class_info class_binding) values
      | Str_class_type (class_, type_, ext, values) ->
          let first, _ = values in
          and_list
            ( class_info
                class_type_declaration_body
                (tok class_ +- keyword type_ ext) first
            )
            (class_info class_type_declaration_body) values
      | Str_include value -> include_infos module_expr value
      | Str_attribute value -> attribute value
      | Str_extension (value, post_attributes) ->
          item_attributes (extension value) post_attributes
      | Str_semisemi value -> tok value

    and value_description
        ( (keyword_, ext, name, colon, typ, primitives, post_attributes)
        : value_description
        )
      =
        let keyword = keyword keyword_ ext
        and name = val_ident name
        in
        let value =
          match primitives with
          | None -> after (keyword +- name +- tok colon) (core_type typ)
          | Some (equal, names) ->
              (* As [let x : t = e]: [:] starts its line when the type is on
                 several lines, so that the type is not confused with the
                 primitives. A [val], without [=], keeps [:] after the
                 name. *)
              binding
                ~keyword ~head:name ~constraint_:(colon, core_type typ)
                ~body:
                  (pack_list_isolating_multiline ~indent:0 (List.map tok names))
                equal
        in
        item_attributes value post_attributes

    and type_declarations
        ((type_, ext, nonrec_, declarations) : type_declarations)
      =
        let first, _ = declarations in
        and_list
          (type_declaration (keyword type_ ext +- opt tok nonrec_) first)
          type_declaration declarations

    (** [type 'a t = u constraint ... [@@attr]]: the head on one line, or, when
      its parameters are on several lines, [type] then the parameters and the
      name on the next lines, indented, and [=] and the definition on the
      line after them when they fit on it, else [=] alone on its line and the
      definition below it, as in a let binding. *)
    and type_declaration
        prefix
        ( (parameters, name, kind, constraints, post_attributes)
        : type_declaration
        )
      =
        let parameters = type_parameters parameters
        and name = tok name
        in
        let head = parameters +- name in
        let value =
          match kind with
          | Kind_abstract -> binding_head ~keyword:prefix head
          | Kind_defined (equal, type_) ->
              let synonym, words, body = type_definition type_ in
              begin match synonym with
              | None -> binding ~keyword:prefix ~head equal ~words ~body
              | Some (typ, body_equal) ->
                  (* [type t = u = A]: the first [=] with [u], the
                     second one before the body. *)
                  binding
                    ~keyword:prefix ~head ~equation:(equal, typ)
                    body_equal ~words ~body
              end
        in
        let value = constraint_clauses value constraints in
        item_attributes value post_attributes

    (** The equation [u] of [type t = u = A | B] and the [=] after it. *)
    and type_synonym synonym =
      Option.map (fun (typ, equal) -> core_type typ, equal) synonym

    (** A type definition: its equation ([u =]), the words after its last
        [=] ([private]), and its body. *)
    and type_definition =
      function
      | Def_manifest (private_, typ) -> None, opt tok private_, core_type typ
      | Def_variant (synonym, private_, constructors) ->
          let equation = type_synonym synonym
          and words = opt tok private_
          in
          begin match constructors with
          | Constructors_empty bar -> equation, words, tok bar
          | Constructors values ->
              ( equation, words,
                pack_list_isolating_multiline
                  ~indent:0
                  ( cases
                      ~doc:snd
                      (fun (value, _) -> constructor_declaration value) values
                  )
              )
          end
      | Def_open (synonym, private_, dotdot) ->
          type_synonym synonym, opt tok private_, tok dotdot
      | Def_record (synonym, private_, labels) ->
          type_synonym synonym, opt tok private_, record_declaration labels
      | Def_external (external_, name) -> None, empty, tok external_ +- tok name

    and record_declaration { left; content; right } =
      bracket
        ~spaces:true (tok left)
        ( pack_list_isolating_multiline
            ~indent:0 (List.map label_declaration content)
        )
        (tok right)

    (** A constructor, as a case: [C : a -> t [@attr]]. *)
    and constructor_declaration
        ((name, arguments, values) : constructor_declaration)
      : L.case
      =
        let ~head, ~arrow, ~body =
          constructor_signature (constr_ident name) arguments
        in
        match arrow with
        | None -> ~head:(attached_attributes head values), ~arrow, ~body
        | Some _ -> ~head, ~arrow, ~body:(attached_attributes body values)

    (** The arguments of a constructor, after its [name], as a case: [->]
      ends the arguments of a GADT constructor. *)
    and constructor_signature name : Cst.constructor_signature -> L.case =
      function
      | Args_none -> ~head:name, ~arrow:None, ~body:empty
      | Args_of (of_, arguments) ->
          ( ~head:(after (name +- tok of_) (constructor_arguments arguments)),
            ~arrow:None, ~body:empty
          )
      | Args_gadt (colon, variables, arguments, result) ->
          let variables =
            opt
              (fun (variables, dot) -> type_variables_dot variables dot)
              variables
          in
          let head values =
            after
              (name +- tok colon)
              (pack_list_isolating_multiline ~indent:0 (variables :: values))
          in
          begin match arguments with
          | None -> ~head:(head [ core_type result ]), ~arrow:None, ~body:empty
          | Some (arguments, arrow) ->
              ( ~head:(head [ constructor_arguments arguments ]),
                ~arrow:(Some arrow), ~body:(core_type result)
              )
          end

    and constructor_arguments =
      function
      | Constructor_tuple types ->
          pack_list_isolating_multiline
            ~indent:0
            ( match types with
                Zero -> []
              | Many types -> separated_before core_type types
            )
      | Constructor_record labels -> record_declaration labels

    and label_declaration
        ((mutable_, name, colon, typ, values, doc, semi) : label_declaration)
      =
        field_end
          ( attached_attributes
              ( after
                  (opt tok mutable_ +- tok name +- tok colon) (core_type typ)
              )
              values
          )
          doc semi

    and type_extension
        ( ( type_, ext, parameters, path, pluseq,
            private_, constructors, post_attributes
          )
        : type_extension
        )
      =
        let head =
          words
            [ keyword type_ ext; type_parameters parameters;
              longident tok path; tok pluseq; opt tok private_
            ]
        in
        let values =
          cases
            ~doc:snd (fun (value, _) -> extension_constructor value)
            constructors
        in
        item_attributes
          ( flat (head +- words values)
            <|> (head +/ pack_list_isolating_multiline ~indent:0 values)
          )
          post_attributes

    and extension_constructor : extension_constructor -> L.case =
      function
      | Ext_decl value -> constructor_declaration value
      | Ext_rebind (name, equal, path, values) ->
          ( ~head:
              ( attached_attributes
                  ( binding
                      ~head:(constr_ident name) equal
                      ~body:(longident constr_ident path)
                  )
                  values
              )
            ,
            ~arrow:None, ~body:empty
          )

    and exception_declaration
        ( (exception_, ext, constructor, post_attributes)
        : exception_declaration
        )
      =
        item_attributes
          ( keyword exception_ ext
            +- case_without_bar (extension_constructor constructor)
          )
          post_attributes

    and module_declaration
        prefix ((name, body, post_attributes) : module_declaration)
      =
        item_attributes
          (module_declaration_body (prefix +- module_name name) body)
          post_attributes

    (** [module F (X : S) : T], as [binding_head]; without parameters,
        [module M : S] keeps [:] after the name, a fixed number of tokens. *)
    and module_declaration_body head body =
      let rec print params =
        function
        | Mdecl_functor (argument, body) ->
            print (functor_arg argument :: params) body
        | Mdecl_type (colon, typ) when params = [] ->
            after (head +- tok colon) (module_type typ)
        | Mdecl_type (colon, typ) ->
            binding_head
              ~params:(List.rev params)
              ~constraint_:(colon, module_type typ) head
        | Mdecl_alias (equal, path) ->
            binding
              ~head ~params:(List.rev params) equal ~body:(module_path path)
      in
      print [] body

    and module_type_declaration
        ( (module_, type_, ext, name, body, post_attributes)
        : module_type_declaration
        )
      =
        let head = tok module_ +- keyword type_ ext +- tok name in
        let value =
          match body with
          | None -> head
          | Some (equal, typ) -> binding ~head equal ~body:(module_type typ)
        in
        item_attributes value post_attributes

    and module_binding keyword ((name, body, post_attributes) : module_binding)
      =
        item_attributes
          (module_binding_body keyword (module_name name) body) post_attributes

    (** [module F (X : S) : T = body], as a let binding. *)
    and module_binding_body keyword head body =
      let rec split params =
        function
        | Mbind_functor (argument, value) ->
            split (functor_arg argument :: params) value
        | Mbind_equal (equal, value) -> List.rev params, None, equal, value
        | Mbind_constraint (colon, typ, equal, value) ->
            (List.rev params, Some (colon, module_type typ), equal, value)
      in
      let params, constraint_, equal, value = split [] body in
      binding
        ~keyword ~head ~params ?constraint_ equal ~body:(module_expr value)

    and open_infos : 'a. ('a -> t) -> 'a open_infos -> t =
      fun print (open_, bang, ext, value, post_attributes) ->
        let head = tok open_ ++ opt tok bang ++ ext_attributes ext in
        item_attributes (after head (print value)) post_attributes

    and include_infos : 'a. ('a -> t) -> 'a include_infos -> t =
      fun print (include_, ext, value, post_attributes) ->
        item_attributes
          (after (keyword include_ ext) (print value)) post_attributes
  end

module Operator_classification = Operator_classification

let default_width = 80

let implementation
    ?(width = default_width) ((items, eof) as value : implementation)
  =
    let module Layout = Make (struct let page_width = width end) in
    Trivia.set (Printer.implementation_tokens value);
    Fun.protect
      ~finally:Trivia.clear
      (fun () -> Layout.D.to_string (Layout.structure items) eof)

let interface ?(width = default_width) ((items, eof) as value : interface) =
  let module Layout = Make (struct let page_width = width end) in
  Trivia.set (Printer.interface_tokens value);
  Fun.protect
    ~finally:Trivia.clear
    (fun () -> Layout.D.to_string (Layout.signature items) eof)
