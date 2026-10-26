open Cst

(* The printer emits the tokens of a CST, in source order. *)
type sink = Token.any_token -> unit

let token (emit : sink) (value : [< Token.any_kind ] Token.token) =
  emit (value :> Token.any_token)

let option print emit = Option.iter (print emit)
let list print emit = List.iter (print emit)

let non_empty_separated print emit { first; rest } =
  print emit first;
  List.iter
    (fun (separator, value) -> token emit separator; print emit value) rest

let semis print emit ((values, trailing) : _ semis) =
  non_empty_separated print emit values; option token emit trailing

let separated print emit =
  function
  | Zero -> ()
  | Many values -> non_empty_separated print emit values

let bar_list print emit ((bar, values) : _ bar_list) =
  option token emit bar; non_empty_separated print emit values

let delimited print emit { left; content; right } =
  token emit left; print emit content; token emit right

let brackets print emit =
  function
  | Parens value -> delimited print emit value
  | Brackets value -> delimited print emit value
  | Braces value -> delimited print emit value

(* {1 Doc comments} *)

let floating emit = function Text value | Ambiguous value -> token emit value

let documented print emit ((pre, value, post) : _ documented) =
  option token emit pre; print emit value; option token emit post

let elements print emit values =
  List.iter
    ( function
        Item value -> documented print emit value
      | Floating value -> floating emit value
    )
    values

let and_docs emit ((post, values, pre) : and_docs) =
  option token emit post;
  List.iter (floating emit) values;
  option token emit pre

let rec module_path emit =
  function
  | Path_ident name -> token emit name
  | Path_dot (path, dot, name) ->
      module_path emit path; token emit dot; token emit name
  | Path_apply (path, argument) ->
      module_path emit path; delimited module_path emit argument

let longident print emit (qualifier, name) =
  Option.iter
    (fun (path, dot) -> module_path emit path; token emit dot) qualifier
  ;
  print emit name

let operator emit =
  function
  | Operator value -> token emit value
  | Index_operator (dotop, value, assign) ->
      token emit dotop;
      brackets
        ( fun emit ->
            Option.iter
              (fun (semi, dotdot) -> token emit semi; token emit dotdot)
        )
        emit value
      ;
      option token emit assign

let val_ident emit =
  function
  | Val_lident name -> token emit name
  | Val_operator value -> delimited operator emit value

let constr_ident emit =
  function
  | Constr_uident name -> token emit name
  | Constr_cons value -> delimited token emit value
  | Constr_nil value -> delimited (fun _ () -> ()) emit value
  | Constr_unit value -> delimited (fun _ () -> ()) emit value
  | Constr_false value -> token emit value
  | Constr_true value -> token emit value

let attr_id emit (id : attr_id) = non_empty_separated token emit id

let type_variable emit ((quote, name) : type_variable) =
  token emit quote; token emit name

let name_tag emit ((backquote, name) : name_tag) =
  token emit backquote; token emit name

let signed_constant emit =
  function
  | Signed (sign, constant) -> token emit sign; token emit constant
  | Unsigned constant -> token emit constant

let arrow_label emit =
  function
  | Nolabel -> ()
  | Labelled (name, colon) -> token emit name; token emit colon
  | Optional value -> token emit value
  | Optional_split (question, name, colon) ->
      token emit question; token emit name; token emit colon

let type_parameter emit (variance, variable) =
  list token emit variance;
  match variable with
  | `Var variable -> type_variable emit variable
  | `Any underscore -> token emit underscore

let type_parameters emit =
  function
  | No_type_parameters -> ()
  | One_type_parameter value -> type_parameter emit value
  | Many_type_parameters value ->
      delimited (separated type_parameter) emit value

let module_name emit =
  function
  | Module_name name -> token emit name
  | Module_anonymous underscore -> token emit underscore

let rec attribute emit value = delimited attribute_contents emit value

and attribute_contents emit (id, value) = attr_id emit id; payload emit value

and attributes emit values = list attribute emit values

and with_attributes
  : 'a. (sink -> 'a -> unit) -> sink -> 'a with_attributes -> unit
  =
  fun print emit { node; attributes = values } ->
    print emit node; attributes emit values

and extension emit =
  function
  | Extension value -> delimited attribute_contents emit value
  | Quoted_extension value -> token emit value

and payload emit =
  function
  | Payload_structure items -> structure emit items
  | Payload_signature (colon, items) -> token emit colon; signature emit items
  | Payload_type (colon, typ) -> token emit colon; core_type emit typ
  | Payload_pattern (question, value, guard) ->
      token emit question;
      pattern emit value;
      Option.iter
        (fun (when_, guard) -> token emit when_; expr emit guard) guard

and ext_attributes emit (ext, values) =
  Option.iter (fun (percent, id) -> token emit percent; attr_id emit id) ext;
  attributes emit values

and and_list : 'a. (sink -> 'a -> unit) -> sink -> 'a and_list -> unit =
  fun print emit (first, rest) ->
    print emit first;
    List.iter
      ( fun (~docstring, and_, values, value) ->
          and_docs emit docstring;
          token emit and_;
          attributes emit values;
          print emit value
      )
      rest

and arrow
  : 'domain 'codomain.
      (sink -> 'domain -> unit) -> (sink -> 'codomain -> unit)
      -> sink -> ('domain, 'codomain) arrow
      -> unit
  =
  fun domain codomain emit (label, value, arrow, result) ->
    arrow_label emit label;
    domain emit value;
    token emit arrow;
    codomain emit result

and let_open : 'a. (sink -> 'a -> unit) -> sink -> 'a let_open -> unit =
  fun print emit (let_, open_, bang, values, path, in_, body) ->
    token emit let_;
    token emit open_;
    option token emit bang;
    attributes emit values;
    module_path emit path;
    token emit in_;
    print emit body

and packed_module : 'a. (sink -> 'a -> unit) -> sink -> 'a packed_module -> unit
  =
  fun print emit (module_, ext, value, package) ->
    token emit module_;
    ext_attributes emit ext;
    print emit value;
    Option.iter
      (fun (colon, typ) -> token emit colon; module_type emit typ) package

(* {1 Core types} *)

and core_type emit value = with_attributes core_type_node emit value

and core_type_node emit =
  function
  | Typ_any value -> token emit value
  | Typ_var value -> type_variable emit value
  | Typ_arrow value ->
      arrow
        (fun emit (typ, doc) -> core_type emit typ; option token emit doc)
        core_type emit value
  | Typ_functor value ->
      arrow
        ( delimited
            ( fun
                emit
                ((module_, ext, name, colon, package) : functor_type_parameter)
              ->
                token emit module_;
                ext_attributes emit ext;
                token emit name;
                token emit colon;
                module_type emit package
            )
        )
        core_type emit value
  | Typ_tuple elements ->
      non_empty_separated
        ( fun emit (label, typ) ->
            Option.iter
              (fun (name, colon) -> token emit name; token emit colon) label
            ;
            core_type emit typ
        )
        emit elements
  | Typ_constr (arguments, path) ->
      type_arguments emit arguments; longident token emit path
  | Typ_class (arguments, hash, path) ->
      type_arguments emit arguments; token emit hash; longident token emit path
  | Typ_object value ->
      delimited
        ( fun emit (fields, dotdot) ->
            list object_field emit fields; option token emit dotdot
        )
        emit value
  | Typ_alias (typ, as_, variable) ->
      core_type emit typ; token emit as_; type_variable emit variable
  | Typ_variant value ->
      delimited
        ( fun emit (leading_bar, fields, tags) ->
            option token emit leading_bar;
            separated row_field emit fields;
            Option.iter
              ( fun (greater, tags) ->
                  token emit greater; list name_tag emit tags
              )
              tags
        )
        emit value
  | Typ_poly (variables, dot, typ) ->
      list type_variable emit variables; token emit dot; core_type emit typ
  | Typ_package value ->
      delimited
        ( fun emit ((module_, ext, package) : package_type) ->
            token emit module_;
            ext_attributes emit ext;
            module_type emit package
        )
        emit value
  | Typ_open (path, dot, typ) ->
      module_path emit path; token emit dot; core_type emit typ
  | Typ_extension value -> extension emit value
  | Typ_paren value -> delimited core_type emit value

and type_arguments emit =
  function
  | No_type_arguments -> ()
  | Type_argument typ -> core_type emit typ
  | Type_arguments value -> delimited (separated core_type) emit value

and row_field emit =
  function
  | Row_tag (tag, of_, values, doc) ->
      name_tag emit tag;
      Option.iter
        ( fun (of_, ampersand, types) ->
            token emit of_;
            option token emit ampersand;
            separated core_type emit types
        )
        of_
      ;
      attributes emit values;
      option token emit doc
  | Row_inherit typ -> core_type emit typ

and field_end emit doc semi =
  option token emit doc;
  Option.iter
    ( fun (semi, values, doc) ->
        token emit semi; attributes emit values; option token emit doc
    )
    semi

and object_field emit =
  function
  | Object_tag (name, colon, typ, values, doc, semi) ->
      token emit name;
      token emit colon;
      core_type emit typ;
      attributes emit values;
      field_end emit doc semi
  | Object_inherit (typ, semi) -> core_type emit typ; option token emit semi

(* {1 Patterns} *)

and pattern emit value = with_attributes pattern_node emit value

and pattern_node emit =
  function
  | Pat_any value -> token emit value
  | Pat_var name -> val_ident emit name
  | Pat_alias (value, as_, name) ->
      pattern emit value; token emit as_; val_ident emit name
  | Pat_constant value -> signed_constant emit value
  | Pat_interval (low, dotdot, high) ->
      signed_constant emit low; token emit dotdot; signed_constant emit high
  | Pat_tuple (elements, open_) ->
      non_empty_separated tuple_pattern_element emit elements;
      Option.iter
        (fun (comma, dotdot) -> token emit comma; token emit dotdot) open_
  | Pat_construct (constructor, argument) ->
      longident constr_ident emit constructor; option pattern emit argument
  | Pat_construct_newtypes (constructor, types, argument) ->
      longident constr_ident emit constructor;
      delimited newtypes emit types;
      pattern emit argument
  | Pat_variant (tag, argument) ->
      name_tag emit tag; option pattern emit argument
  | Pat_record value ->
      delimited
        ( fun emit ((fields, wildcard) : record_pattern) ->
            semis record_pattern_field emit fields;
            Option.iter
              ( fun (underscore, semi) ->
                  token emit underscore; option token emit semi
              )
              wildcard
        )
        emit value
  | Pat_array value -> delimited (option (semis pattern)) emit value
  | Pat_list value -> delimited (semis pattern) emit value
  | Pat_or (left, bar, right) ->
      pattern emit left; token emit bar; pattern emit right
  | Pat_cons (left, cons, right) ->
      pattern emit left; token emit cons; pattern emit right
  | Pat_constraint value -> poly_pattern emit value
  | Pat_type (hash, path) -> token emit hash; longident token emit path
  | Pat_lazy (lazy_, ext, value) ->
      token emit lazy_; ext_attributes emit ext; pattern emit value
  | Pat_unpack value -> delimited (packed_module module_name) emit value
  | Pat_exception (exception_, ext, value) ->
      token emit exception_; ext_attributes emit ext; pattern emit value
  | Pat_effect (effect_, value, comma, continuation) ->
      token emit effect_;
      pattern emit value;
      token emit comma;
      pattern emit continuation
  | Pat_extension value -> extension emit value
  | Pat_open (path, dot, value) ->
      module_path emit path; token emit dot; pattern emit value
  | Pat_paren value -> delimited pattern emit value

and tuple_pattern_element emit =
  function
  | Tuple_pattern value -> pattern emit value
  | Tuple_pattern_label (label, value) -> token emit label; pattern emit value
  | Tuple_pattern_pun (tilde, name) -> token emit tilde; token emit name
  | Tuple_pattern_pun_constraint (tilde, value) ->
      token emit tilde;
      delimited
        ( fun emit (name, colon, typ) ->
            token emit name; token emit colon; core_type emit typ
        )
        emit value
and record_pattern_field emit ((label, typ, value) : record_pattern_field) =
  longident token emit label;
  Option.iter (fun (colon, typ) -> token emit colon; core_type emit typ) typ;
  Option.iter (fun (equal, value) -> token emit equal; pattern emit value) value

(* [(pattern : type)]. *)
and poly_pattern emit value =
  delimited
    ( fun emit (value, colon, typ) ->
        pattern emit value; token emit colon; core_type emit typ
    )
    emit value

(* {1 Expressions} *)

and expr emit value = with_attributes expr_node emit value

and expr_node emit =
  function
  | Exp_ident path -> longident val_ident emit path
  | Exp_constant value -> token emit value
  | Exp_construct (constructor, argument) ->
      longident constr_ident emit constructor; option expr emit argument
  | Exp_variant (tag, argument) -> name_tag emit tag; option expr emit argument
  | Exp_paren value -> delimited expr emit value
  | Exp_constraint value ->
      delimited
        ( fun emit (value, constraint_) ->
            expr emit value; type_constraint emit constraint_
        )
        emit value
  | Exp_begin value ->
      delimited
        (fun emit (ext, body) -> ext_attributes emit ext; option expr emit body)
        emit value
  | Exp_apply (function_, arguments) ->
      expr emit function_; list argument emit arguments
  | Exp_infix (left, operator, right) ->
      expr emit left; token emit operator; expr emit right
  | Exp_prefix (operator, value) -> token emit operator; expr emit value
  | Exp_unary (operator, value) -> token emit operator; expr emit value
  | Exp_tuple elements -> non_empty_separated tuple_element emit elements
  | Exp_cons (left, cons, right) ->
      expr emit left; token emit cons; expr emit right
  | Exp_field (record, dot, label) ->
      expr emit record; token emit dot; longident token emit label
  | Exp_setfield (record, dot, label, arrow, value) ->
      expr emit record;
      token emit dot;
      longident token emit label;
      token emit arrow;
      expr emit value
  | Exp_index (array, dot, index, assign) ->
      expr emit array;
      begin match dot with
      | Index_builtin dot -> token emit dot
      | Index_user (qualifier, dotop) ->
          Option.iter
            (fun (dot, path) -> token emit dot; module_path emit path) qualifier
          ;
          token emit dotop
      end;
      brackets (semis expr) emit index;
      Option.iter
        (fun (arrow, value) -> token emit arrow; expr emit value) assign
  | Exp_setinstvar (name, arrow, value) ->
      token emit name; token emit arrow; expr emit value
  | Exp_send (value, hash, name) ->
      expr emit value; token emit hash; token emit name
  | Exp_let (bindings, in_, body) ->
      let_bindings emit bindings; token emit in_; expr emit body
  | Exp_letop (let_op, bindings, in_, body) ->
      token emit let_op;
      non_empty_separated letop_binding emit bindings;
      token emit in_;
      expr emit body
  | Exp_struct_item (let_, ext, item, in_, body) ->
      token emit let_;
      ext_attributes emit ext;
      documented structure_item emit item;
      token emit in_;
      expr emit body
  | Exp_fun (fun_, ext, params, constraint_, arrow, body) ->
      token emit fun_;
      ext_attributes emit ext;
      list function_param emit params;
      Option.iter
        (fun (colon, typ) -> token emit colon; core_type emit typ) constraint_
      ;
      token emit arrow;
      expr emit body
  | Exp_function (function_, ext, cases) ->
      token emit function_; ext_attributes emit ext; bar_list case emit cases
  | Exp_match (match_, ext, scrutinee, with_, cases) ->
      token emit match_;
      ext_attributes emit ext;
      expr emit scrutinee;
      token emit with_;
      bar_list case emit cases
  | Exp_try (try_, ext, body, with_, cases) ->
      token emit try_;
      ext_attributes emit ext;
      expr emit body;
      token emit with_;
      bar_list case emit cases
  | Exp_if (if_, ext, condition, then_, if_true, if_false) ->
      token emit if_;
      ext_attributes emit ext;
      expr emit condition;
      token emit then_;
      expr emit if_true;
      Option.iter
        (fun (else_, value) -> token emit else_; expr emit value) if_false
  | Exp_while (while_, ext, condition, body) ->
      token emit while_;
      ext_attributes emit ext;
      expr emit condition;
      delimited expr emit body
  | Exp_for (for_, ext, index, equal, start, direction, stop, body) ->
      token emit for_;
      ext_attributes emit ext;
      pattern emit index;
      token emit equal;
      expr emit start;
      token emit direction;
      expr emit stop;
      delimited expr emit body
  | Exp_assert (assert_, ext, value) ->
      token emit assert_; ext_attributes emit ext; expr emit value
  | Exp_lazy (lazy_, ext, value) ->
      token emit lazy_; ext_attributes emit ext; expr emit value
  | Exp_sequence (left, semi, right) ->
      expr emit left; token emit semi; expr emit right
  | Exp_sequence_ext (left, semi, percent, id, right) ->
      expr emit left;
      token emit semi;
      token emit percent;
      attr_id emit id;
      expr emit right
  | Exp_semi (value, semi) -> expr emit value; token emit semi
  | Exp_new (new_, ext, path) ->
      token emit new_; ext_attributes emit ext; longident token emit path
  | Exp_pack value -> delimited (packed_module module_expr) emit value
  | Exp_object value ->
      delimited
        ( fun emit (ext, body) ->
            ext_attributes emit ext; class_structure emit body
        )
        emit value
  | Exp_override value ->
      delimited (option (semis object_expr_field)) emit value
  | Exp_local_open (path, dot, value) ->
      module_path emit path; token emit dot; expr emit value
  | Exp_record value ->
      delimited
        ( fun emit ((base, fields) : record_expr) ->
            Option.iter
              (fun (base, with_) -> expr emit base; token emit with_) base
            ;
            semis record_field emit fields
        )
        emit value
  | Exp_array value -> delimited (option (semis expr)) emit value
  | Exp_list value -> delimited (semis expr) emit value
  | Exp_extension value -> extension emit value
  | Exp_unreachable dot -> token emit dot

and tuple_element emit =
  function
  | Tuple_expr (~label, value) -> option token emit label; expr emit value
  | Tuple_pun (tilde, name) -> token emit tilde; token emit name
  | Tuple_pun_constraint (tilde, value) ->
      token emit tilde; delimited name_constraint emit value
and argument emit =
  function
  | Arg_expr value -> expr emit value
  | Arg_label (label, value) -> token emit label; expr emit value
  | Arg_pun (tilde, name) -> token emit tilde; token emit name
  | Arg_pun_constraint (tilde, value) ->
      token emit tilde; delimited name_constraint emit value
  | Arg_optional_pun (question, name) -> token emit question; token emit name
  | Arg_optlabel (label, value) -> token emit label; expr emit value

and type_constraint emit =
  function
  | Constraint (colon, typ) -> token emit colon; core_type emit typ
  | Coerce (ground, coerce, typ) ->
      Option.iter
        (fun (colon, typ) -> token emit colon; core_type emit typ) ground
      ;
      token emit coerce;
      core_type emit typ

and record_field emit ((label, constraint_, value) : record_field) =
  longident token emit label;
  option type_constraint emit constraint_;
  Option.iter (fun (equal, value) -> token emit equal; expr emit value) value

and object_expr_field emit ((name, value) : object_expr_field) =
  token emit name;
  Option.iter (fun (equal, value) -> token emit equal; expr emit value) value

and let_bindings emit ((let_, ext, rec_, bindings) : let_bindings) =
  token emit let_;
  ext_attributes emit ext;
  option token emit rec_;
  and_list let_binding emit bindings

and let_binding emit ((body, post_attributes) : let_binding) =
  let_binding_body emit body; attributes emit post_attributes

and function_binding
    emit ((params, constraint_, equal, body) : function_binding)
  =
    list function_param emit params;
    option type_constraint emit constraint_;
    token emit equal;
    expr emit body

and newtype_binding
    emit ((colon, types, dot, typ, equal, value) : newtype_binding)
  =
    token emit colon;
    newtypes emit types;
    token emit dot;
    core_type emit typ;
    token emit equal;
    expr emit value

and let_binding_body emit =
  function
  | Lb_function (name, binding) ->
      val_ident emit name; function_binding emit binding
  | Lb_constrained (name, constraint_, equal, value) ->
      val_ident emit name;
      type_constraint emit constraint_;
      token emit equal;
      expr emit value
  | Lb_poly (name, colon, variables, dot, typ, equal, value) ->
      val_ident emit name;
      token emit colon;
      list type_variable emit variables;
      token emit dot;
      core_type emit typ;
      token emit equal;
      expr emit value
  | Lb_newtypes (name, binding) ->
      val_ident emit name; newtype_binding emit binding
  | Lb_pattern (value, equal, body) ->
      pattern emit value; token emit equal; expr emit body
  | Lb_pattern_constraint (value, colon, typ, equal, body) ->
      pattern emit value;
      token emit colon;
      core_type emit typ;
      token emit equal;
      expr emit body
  | Lb_pun name -> val_ident emit name

and letop_binding emit =
  function
  | Lop_function (name, binding) ->
      val_ident emit name; function_binding emit binding
  | Lop_pun name -> val_ident emit name
  | Lop_constraint (value, colon, typ, equal, body) ->
      pattern emit value;
      token emit colon;
      core_type emit typ;
      token emit equal;
      expr emit body
  | Lop_pattern (value, equal, body) ->
      pattern emit value; token emit equal; expr emit body

and function_param emit =
  function
  | Param_newtypes value -> delimited newtypes emit value
  | Param_value value -> param_pattern emit value

and newtypes emit ((type_, names) : newtypes) =
  token emit type_; list token emit names

and name_constraint emit (name, constraint_) =
  token emit name; type_constraint emit constraint_

and label_pattern emit ((name, typ) : label_pattern) =
  token emit name; optional_type emit typ

and optional_type emit =
  Option.iter (fun (colon, typ) -> token emit colon; core_type emit typ)
and default_value emit =
  Option.iter (fun (equal, value) -> token emit equal; expr emit value)

and param_pattern emit =
  function
  | Param_optional_default (question, value) ->
      token emit question;
      delimited
        ( fun emit (pattern, default) ->
            label_pattern emit pattern; default_value emit default
        )
        emit value
  | Param_optional (question, name) -> token emit question; token emit name
  | Param_optlabel_default (optlabel, value) ->
      token emit optlabel;
      delimited
        ( fun emit (((value, typ) : let_pattern), default) ->
            pattern emit value;
            optional_type emit typ;
            default_value emit default
        )
        emit value
  | Param_optlabel (optlabel, value) -> token emit optlabel; token emit value
  | Param_labelled_paren (tilde, value) ->
      token emit tilde; delimited label_pattern emit value
  | Param_labelled (tilde, name) -> token emit tilde; token emit name
  | Param_label (label, value) -> token emit label; pattern emit value
  | Param_label_poly (label, value) -> token emit label; poly_pattern emit value
  | Param_pattern value -> pattern emit value
  | Param_poly value -> poly_pattern emit value

and case emit ((lhs, guard, arrow, rhs) : case) =
  pattern emit lhs;
  Option.iter (fun (when_, guard) -> token emit when_; expr emit guard) guard;
  token emit arrow;
  expr emit rhs

(* {1 Classes} *)

and class_arguments emit (arguments : class_arguments) =
  option (delimited (separated core_type)) emit arguments

and class_parameters emit (parameters : class_parameters) =
  option (delimited (separated type_parameter)) emit parameters

and class_expr emit value = with_attributes class_expr_node emit value

and class_expr_node emit =
  function
  | Cl_constr (arguments, path) ->
      class_arguments emit arguments; longident token emit path
  | Cl_structure value ->
      delimited
        ( fun emit (values, body) ->
            attributes emit values; class_structure emit body
        )
        emit value
  | Cl_fun (fun_, values, params, arrow, body) ->
      token emit fun_;
      attributes emit values;
      list param_pattern emit params;
      token emit arrow;
      class_expr emit body
  | Cl_apply (value, arguments) ->
      class_expr emit value; list argument emit arguments
  | Cl_let (bindings, in_, body) ->
      let_bindings emit bindings; token emit in_; class_expr emit body
  | Cl_open value -> let_open class_expr emit value
  | Cl_constraint value ->
      delimited
        ( fun emit (value, colon, typ) ->
            class_expr emit value; token emit colon; class_type emit typ
        )
        emit value
  | Cl_extension value -> extension emit value
  | Cl_paren value -> delimited class_expr emit value

and class_structure emit ((self, fields) : class_structure) =
  Option.iter
    begin function
    | Self_pattern value -> delimited pattern emit value
    | Self_constraint value -> poly_pattern emit value
    end
    self
  ;
  elements class_field emit fields

and class_field emit =
  function
  | Cf_inherit (inherit_, bang, values, value, alias, ~post_attributes) ->
      token emit inherit_;
      option token emit bang;
      attributes emit values;
      class_expr emit value;
      Option.iter (fun (as_, name) -> token emit as_; token emit name) alias;
      attributes emit post_attributes
  | Cf_val (val_, bang, values, flags, name, body, ~post_attributes) ->
      token emit val_;
      option token emit bang;
      attributes emit values;
      list token emit flags;
      token emit name;
      begin match body with
      | Value_virtual (colon, typ) -> token emit colon; core_type emit typ
      | Value_concrete (constraint_, equal, value) ->
          option type_constraint emit constraint_;
          token emit equal;
          expr emit value
      end;
      attributes emit post_attributes
  | Cf_method (method_, bang, values, flags, name, body, ~post_attributes) ->
      token emit method_;
      option token emit bang;
      attributes emit values;
      list token emit flags;
      token emit name;
      begin match body with
      | Method_virtual (colon, typ) -> token emit colon; core_type emit typ
      | Method_strict binding -> function_binding emit binding
      | Method_poly (colon, typ, equal, value) ->
          token emit colon;
          core_type emit typ;
          token emit equal;
          expr emit value
      | Method_newtypes binding -> newtype_binding emit binding
      end;
      attributes emit post_attributes
  | Cf_constraint (constraint_, values, left, equal, right, ~post_attributes) ->
      token emit constraint_;
      attributes emit values;
      core_type emit left;
      token emit equal;
      core_type emit right;
      attributes emit post_attributes
  | Cf_initializer (initializer_, values, value, ~post_attributes) ->
      token emit initializer_;
      attributes emit values;
      expr emit value;
      attributes emit post_attributes
  | Cf_extension (value, post_attributes) ->
      extension emit value; attributes emit post_attributes
  | Cf_attribute value -> attribute emit value

and class_type emit =
  function
  | Cty_signature value -> class_signature emit value
  | Cty_arrow value -> arrow core_type class_type emit value

and class_signature emit value = with_attributes class_signature_node emit value

and class_signature_node emit =
  function
  | Csig_constr (arguments, path) ->
      class_arguments emit arguments; longident token emit path
  | Csig_extension value -> extension emit value
  | Csig_object value ->
      delimited
        ( fun emit ((values, self, fields) : class_signature_body) ->
            attributes emit values;
            option (delimited core_type) emit self;
            elements class_sig_field emit fields
        )
        emit value
  | Csig_open value -> let_open class_signature emit value

and class_sig_field emit =
  function
  | Ctf_inherit (inherit_, values, typ, ~post_attributes) ->
      token emit inherit_;
      attributes emit values;
      class_signature emit typ;
      attributes emit post_attributes
  | Ctf_val (val_, values, flags, name, colon, typ, ~post_attributes) ->
      token emit val_;
      attributes emit values;
      list token emit flags;
      token emit name;
      token emit colon;
      core_type emit typ;
      attributes emit post_attributes
  | Ctf_method (method_, values, flags, name, colon, typ, ~post_attributes) ->
      token emit method_;
      attributes emit values;
      list token emit flags;
      token emit name;
      token emit colon;
      core_type emit typ;
      attributes emit post_attributes
  | Ctf_constraint (constraint_, values, left, equal, right, ~post_attributes)
  ->
      token emit constraint_;
      attributes emit values;
      core_type emit left;
      token emit equal;
      core_type emit right;
      attributes emit post_attributes
  | Ctf_extension (value, post_attributes) ->
      extension emit value; attributes emit post_attributes
  | Ctf_attribute value -> attribute emit value

and class_info
  : 'body. (sink -> 'body -> unit) -> sink -> 'body class_info -> unit
  =
  fun body emit (virtual_, parameters, name, value, post_attributes) ->
    option token emit virtual_;
    class_parameters emit parameters;
    token emit name;
    body emit value;
    attributes emit post_attributes

and class_description_body emit ((colon, typ) : class_description_body) =
  token emit colon; class_type emit typ

and class_type_declaration_body
    emit ((equal, typ) : class_type_declaration_body)
  = token emit equal; class_signature emit typ

and class_binding emit =
  function
  | Class_equal (equal, value) -> token emit equal; class_expr emit value
  | Class_constraint (colon, typ, equal, value) ->
      token emit colon;
      class_type emit typ;
      token emit equal;
      class_expr emit value
  | Class_param (param, binding) ->
      param_pattern emit param; class_binding emit binding

(* {1 Modules} *)

and functor_arg emit =
  function
  | Functor_unit value -> delimited (fun _ () -> ()) emit value
  | Functor_named value ->
      delimited
        ( fun emit (name, colon, typ) ->
            module_name emit name; token emit colon; module_type emit typ
        )
        emit value

and module_expr emit value = with_attributes module_expr_node emit value

and module_expr_node emit =
  function
  | Mod_ident path -> module_path emit path
  | Mod_structure value ->
      delimited
        ( fun emit (values, items) ->
            attributes emit values; structure emit items
        )
        emit value
  | Mod_functor (functor_, values, args, arrow, body) ->
      token emit functor_;
      attributes emit values;
      list functor_arg emit args;
      token emit arrow;
      module_expr emit body
  | Mod_apply (value, argument) ->
      module_expr emit value; module_expr emit argument
  | Mod_apply_unit (value, unit) ->
      module_expr emit value; delimited (fun _ () -> ()) emit unit
  | Mod_constraint value ->
      delimited
        ( fun emit (value, colon, typ) ->
            module_expr emit value; token emit colon; module_type emit typ
        )
        emit value
  | Mod_paren value -> delimited module_expr emit value
  | Mod_unpack value ->
      delimited
        ( fun emit ((val_, values, value, package) : val_unpack) ->
            token emit val_;
            attributes emit values;
            expr emit value;
            match package with
            | No_package -> ()
            | Package_constraint (colon, typ) ->
                token emit colon; module_type emit typ
            | Package_coerce (ground, coerce, typ) ->
                Option.iter
                  (fun (colon, typ) -> token emit colon; module_type emit typ)
                  ground
                ;
                token emit coerce;
                module_type emit typ
        )
        emit value
  | Mod_extension value -> extension emit value

and module_type emit value = with_attributes module_type_node emit value

and module_type_node emit =
  function
  | Mty_ident path -> longident token emit path
  | Mty_signature value ->
      delimited
        ( fun emit (values, items) ->
            attributes emit values; signature emit items
        )
        emit value
  | Mty_functor (functor_, args, arrow, body) ->
      Option.iter
        (fun (functor_, values) -> token emit functor_; attributes emit values)
        functor_
      ;
      list functor_arg emit args;
      token emit arrow;
      module_type emit body
  | Mty_arrow (left, arrow, right) ->
      module_type emit left; token emit arrow; module_type emit right
  | Mty_with (typ, with_, constraints) ->
      module_type emit typ;
      token emit with_;
      separated with_constraint emit constraints
  | Mty_typeof (module_, type_, of_, values, value) ->
      token emit module_;
      token emit type_;
      token emit of_;
      attributes emit values;
      module_expr emit value
  | Mty_extension value -> extension emit value
  | Mty_paren value -> delimited module_type emit value

and with_constraint emit =
  function
  | With_type (type_, parameters, path, equal, private_, typ, constraints) ->
      token emit type_;
      type_parameters emit parameters;
      longident token emit path;
      token emit equal;
      option token emit private_;
      core_type emit typ;
      list type_constraint_clause emit constraints
  | With_typesubst (type_, parameters, path, colonequal, typ) ->
      token emit type_;
      type_parameters emit parameters;
      longident token emit path;
      token emit colonequal;
      core_type emit typ
  | With_module (module_, path, equal, target) ->
      token emit module_;
      module_path emit path;
      token emit equal;
      module_path emit target
  | With_modtype (module_, type_, path, equal, typ) ->
      token emit module_;
      token emit type_;
      longident token emit path;
      token emit equal;
      module_type emit typ

and type_constraint_clause
    emit ((constraint_, left, equal, right) : type_constraint_clause)
  =
    token emit constraint_;
    core_type emit left;
    token emit equal;
    core_type emit right

(* {1 Signatures and structures} *)

and signature emit items = elements signature_item emit items

and signature_item emit =
  function
  | Sig_value value -> value_description emit value
  | Sig_type value | Sig_typesubst value -> type_declarations emit value
  | Sig_typext value -> type_extension emit value
  | Sig_exception value -> exception_declaration emit value
  | Sig_module (module_, ext, declaration) ->
      token emit module_;
      ext_attributes emit ext;
      module_declaration emit declaration
  | Sig_modsubst (module_, ext, name, colonequal, path, post_attributes) ->
      token emit module_;
      ext_attributes emit ext;
      token emit name;
      token emit colonequal;
      module_path emit path;
      attributes emit post_attributes
  | Sig_recmodule (module_, ext, rec_, declarations) ->
      token emit module_;
      ext_attributes emit ext;
      token emit rec_;
      and_list module_declaration emit declarations
  | Sig_modtype value | Sig_modtypesubst value ->
      module_type_declaration emit value
  | Sig_open value -> open_infos module_path emit value
  | Sig_include value -> include_infos module_type emit value
  | Sig_class (class_, ext, values) ->
      token emit class_;
      ext_attributes emit ext;
      and_list (class_info class_description_body) emit values
  | Sig_class_type (class_, type_, ext, values) ->
      token emit class_;
      token emit type_;
      ext_attributes emit ext;
      and_list (class_info class_type_declaration_body) emit values
  | Sig_attribute value -> attribute emit value
  | Sig_extension (value, post_attributes) ->
      extension emit value; attributes emit post_attributes
  | Sig_semisemi value -> token emit value

and structure emit items = elements structure_item emit items

and structure_item emit =
  function
  | Str_eval (value, post_attributes) ->
      expr emit value; attributes emit post_attributes
  | Str_value bindings -> let_bindings emit bindings
  | Str_primitive value -> value_description emit value
  | Str_type value -> type_declarations emit value
  | Str_typext value -> type_extension emit value
  | Str_exception value -> exception_declaration emit value
  | Str_module (module_, ext, binding) ->
      token emit module_; ext_attributes emit ext; module_binding emit binding
  | Str_recmodule (module_, ext, rec_, bindings) ->
      token emit module_;
      ext_attributes emit ext;
      token emit rec_;
      and_list module_binding emit bindings
  | Str_modtype value -> module_type_declaration emit value
  | Str_open value -> open_infos module_expr emit value
  | Str_class (class_, ext, values) ->
      token emit class_;
      ext_attributes emit ext;
      and_list (class_info class_binding) emit values
  | Str_class_type (class_, type_, ext, values) ->
      token emit class_;
      token emit type_;
      ext_attributes emit ext;
      and_list (class_info class_type_declaration_body) emit values
  | Str_include value -> include_infos module_expr emit value
  | Str_attribute value -> attribute emit value
  | Str_extension (value, post_attributes) ->
      extension emit value; attributes emit post_attributes
  | Str_semisemi value -> token emit value

and value_description
    emit
    ( (keyword, ext, name, colon, typ, primitives, post_attributes)
    : value_description
    )
  =
    token emit keyword;
    ext_attributes emit ext;
    val_ident emit name;
    token emit colon;
    core_type emit typ;
    Option.iter
      (fun (equal, names) -> token emit equal; list token emit names) primitives
    ;
    attributes emit post_attributes

and type_declarations
    emit ((type_, ext, nonrec_, declarations) : type_declarations)
  =
    token emit type_;
    ext_attributes emit ext;
    option token emit nonrec_;
    and_list type_declaration emit declarations

and type_declaration
    emit
    ((parameters, name, kind, constraints, post_attributes) : type_declaration)
  =
    type_parameters emit parameters;
    token emit name;
    begin match kind with
    | Kind_abstract -> ()
    | Kind_defined (equal, definition) ->
        token emit equal; type_definition emit definition
    end;
    list type_constraint_clause emit constraints;
    attributes emit post_attributes

and type_definition emit =
  function
  | Def_manifest (private_, typ) ->
      option token emit private_; core_type emit typ
  | Def_variant (synonym, private_, constructors) ->
      type_synonym emit synonym;
      option token emit private_;
      begin match constructors with
      | Constructors_empty bar -> token emit bar
      | Constructors values ->
          bar_list
            ( fun emit (value, doc) ->
                constructor_declaration emit value; option token emit doc
            )
            emit values
      end
  | Def_open (synonym, private_, dotdot) ->
      type_synonym emit synonym; option token emit private_; token emit dotdot
  | Def_record (synonym, private_, labels) ->
      type_synonym emit synonym;
      option token emit private_;
      delimited (list label_declaration) emit labels
  | Def_external (external_, name) -> token emit external_; token emit name

and type_synonym emit =
  Option.iter (fun (typ, equal) -> core_type emit typ; token emit equal)

and constructor_declaration
    emit ((name, arguments, values) : constructor_declaration)
  =
    constr_ident emit name;
    constructor_signature emit arguments;
    attributes emit values

and constructor_signature emit =
  function
  | Args_none -> ()
  | Args_of (of_, arguments) ->
      token emit of_; constructor_arguments emit arguments
  | Args_gadt (colon, variables, arguments, result) ->
      token emit colon;
      Option.iter
        ( fun (variables, dot) ->
            list type_variable emit variables; token emit dot
        )
        variables
      ;
      Option.iter
        ( fun (arguments, arrow) ->
            constructor_arguments emit arguments; token emit arrow
        )
        arguments
      ;
      core_type emit result

and constructor_arguments emit =
  function
  | Constructor_tuple types -> separated core_type emit types
  | Constructor_record labels -> delimited (list label_declaration) emit labels

and label_declaration
    emit ((mutable_, name, colon, typ, values, doc, semi) : label_declaration)
  =
    option token emit mutable_;
    token emit name;
    token emit colon;
    core_type emit typ;
    attributes emit values;
    field_end emit doc semi

and type_extension
    emit
    ( ( type_, ext, parameters, path, pluseq,
        private_, constructors, post_attributes
      )
    : type_extension
    )
  =
    token emit type_;
    ext_attributes emit ext;
    type_parameters emit parameters;
    longident token emit path;
    token emit pluseq;
    option token emit private_;
    bar_list
      ( fun emit (value, doc) ->
          extension_constructor emit value; option token emit doc
      )
      emit constructors
    ;
    attributes emit post_attributes

and extension_constructor emit =
  function
  | Ext_decl value -> constructor_declaration emit value
  | Ext_rebind (name, equal, path, values) ->
      constr_ident emit name;
      token emit equal;
      longident constr_ident emit path;
      attributes emit values

and exception_declaration
    emit
    ((exception_, ext, constructor, post_attributes) : exception_declaration)
  =
    token emit exception_;
    ext_attributes emit ext;
    extension_constructor emit constructor;
    attributes emit post_attributes

and module_declaration emit ((name, body, post_attributes) : module_declaration)
  =
    module_name emit name;
    module_declaration_body emit body;
    attributes emit post_attributes

and module_declaration_body emit =
  function
  | Mdecl_type (colon, typ) -> token emit colon; module_type emit typ
  | Mdecl_functor (argument, body) ->
      functor_arg emit argument; module_declaration_body emit body
  | Mdecl_alias (equal, path) -> token emit equal; module_path emit path

and module_type_declaration
    emit
    ( (module_, type_, ext, name, body, post_attributes)
    : module_type_declaration
    )
  =
    token emit module_;
    token emit type_;
    ext_attributes emit ext;
    token emit name;
    Option.iter
      (fun (equal, typ) -> token emit equal; module_type emit typ) body
    ;
    attributes emit post_attributes

and module_binding emit ((name, body, post_attributes) : module_binding) =
  module_name emit name;
  module_binding_body emit body;
  attributes emit post_attributes

and module_binding_body emit =
  function
  | Mbind_equal (equal, value) -> token emit equal; module_expr emit value
  | Mbind_constraint (colon, typ, equal, value) ->
      token emit colon;
      module_type emit typ;
      token emit equal;
      module_expr emit value
  | Mbind_functor (argument, body) ->
      functor_arg emit argument; module_binding_body emit body

and open_infos : 'a. (sink -> 'a -> unit) -> sink -> 'a open_infos -> unit =
  fun print emit (open_, bang, ext, value, post_attributes) ->
    token emit open_;
    option token emit bang;
    ext_attributes emit ext;
    print emit value;
    attributes emit post_attributes

and include_infos : 'a. (sink -> 'a -> unit) -> sink -> 'a include_infos -> unit
  =
  fun print emit (include_, ext, value, post_attributes) ->
    token emit include_;
    ext_attributes emit ext;
    print emit value;
    attributes emit post_attributes

let implementation emit ((items, eof) : implementation) =
  structure emit items; token emit eof

let interface emit ((items, eof) : interface) =
  signature emit items; token emit eof

let tokens print value =
  let tokens = ref [] in
  print (fun token -> tokens := token :: !tokens) value; List.rev !tokens

let implementation_tokens value = tokens implementation value
let interface_tokens value = tokens interface value

let to_buffer print buffer value =
  print
    ( fun token ->
        List.iter
          (fun trivia -> Buffer.add_string buffer (Token.trivia_text trivia))
          token.Token.leading
        ;
        Buffer.add_string buffer (Token.text token)
    )
    value

let string_of_implementation value =
  let buffer = Buffer.create 256 in
  to_buffer implementation buffer value; Buffer.contents buffer

let string_of_interface value =
  let buffer = Buffer.create 256 in
  to_buffer interface buffer value; Buffer.contents buffer

let implementation buffer value = to_buffer implementation buffer value
let interface buffer value = to_buffer interface buffer value
