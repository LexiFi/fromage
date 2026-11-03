open Cst

exception Unsupported of string

type cursor =
  { mutable position : Lexing.position;
    mutable last_end : Lexing.position;
    (** The end of the last consumed token: the position of empty
        productions, as in Menhir. *)
    mutable count : int; (** The number of consumed tokens. *)
    mutable starts : Lexing.position array;
    (** The start of every consumed token, by consumption order. *)
    locations : (string, Token.any_token * Docstrings.docstring) Hashtbl.t;
    (** The doc comments of the CST, with their locations, by text: a doc
        comment is found among those with its text, by physical equality. *)
    mutable docs : Docstrings.docs;
    (** The doc comments of the next item, for its conversion ({!take_docs}). *)
    mutable warnings : (Location.t * bool) list;
    (** The warnings about doc comments: unattached ([true]) or ambiguous. *)
  }

let advance = Token_position.advance

let consume cursor (token : _ Token.token) =
  let start = Token_position.skip_leading cursor.position token in
  cursor.position <- advance start (Token.text token);
  cursor.last_end <- cursor.position;
  if cursor.count = Array.length cursor.starts then
    cursor.starts <-
      Array.append cursor.starts (Array.make (max 64 cursor.count) start)
  ;
  cursor.starts.(cursor.count) <- start;
  cursor.count <- cursor.count + 1;
  Location.{ loc_start = start; loc_end = cursor.position; loc_ghost = false }

(* [with_span cursor f] runs [f] and returns its result with the positions of
   the tokens it consumed ([$sloc] in Menhir): an empty production is located at
   the end of the previous token. *)
let with_span cursor f =
  let count = cursor.count
  and previous_end = cursor.last_end
  in
  let result = f () in
  let start =
    (* An empty production, as in Menhir: none of those which OCST converts
       with [with_span] is empty. *)
    if cursor.count > count then cursor.starts.(count)
    else (previous_end [@coverage off])
  in
  result, (start, cursor.last_end)

let make_loc (loc_start, loc_end) =
  Location.{ loc_start; loc_end; loc_ghost = false }

(* An empty location at the end of the last consumed token, to end spans:
   in the grammar, a production ends with its last token, whatever the location
   of the AST node built from its last symbol. *)
let here cursor = make_loc (cursor.last_end, cursor.last_end)

(* {1 Doc comments}

   The doc comments are in the CST, where they document something. The
   cursor steps over them without including them in the spans, since they
   are not tokens for the grammar. *)

let location start stop =
  Location.{ loc_start = start; loc_end = stop; loc_ghost = false }

(* The doc comments of the tokens of a CST, with their locations, and the
   warnings for the doc comments in the trivia, which document nothing. *)
let scan_docstrings ~filename tokens =
  let position = ref (Token_position.initial ~filename) in
  let locations = Hashtbl.create 64
  and warnings = ref []
  in
  List.iter
    ( fun (token : Token.any_token) ->
        List.iter
          ( fun (trivia : Token.trivia) ->
              let start = !position in
              position := Token_position.skip_trivia start trivia;
              match trivia with
              | Warning (Unattached_docstring, _) ->
                  warnings := (location start !position, true) :: !warnings
              | _ -> ()
          )
          token.leading
        ;
        let start = !position in
        position := advance start (Token.text token);
        match token.kind with
        | `DOCSTRING text ->
            let doc =
              Docstrings.docstring
                (Token.docstring_body text) (location start !position)
            in
            Hashtbl.add locations text (token, doc)
        | _ -> ()
    )
    tokens
  ;
  locations, !warnings

let docstring cursor (token : Cst.docstring) =
  let (`DOCSTRING text) = token.kind in
  snd
    ( List.find
        (fun (other, _) -> other == (token :> Token.any_token))
        (Hashtbl.find_all cursor.locations text)
    )

(* Steps over a doc comment. *)
let skip_doc cursor (token : Cst.docstring) =
  let start = Token_position.skip_leading cursor.position token in
  cursor.position <- advance start (Token.text token)

let skip_docs cursor = Option.iter (skip_doc cursor)

(* An info slot ([symbol_info]). *)
let info cursor doc = skip_docs cursor doc; Option.map (docstring cursor) doc

(* Floating doc comments: the text, and the ambiguous ones, which warn. *)
let skip_floating cursor (value : Cst.floating) =
  match value with
  | Text token -> skip_doc cursor token
  | Ambiguous token ->
      skip_doc cursor token;
      cursor.warnings <-
          (Docstrings.docstring_loc (docstring cursor token), false)
        :: cursor.warnings

let texts cursor values =
  List.filter_map
    ( function
        Cst.Text token -> Some (docstring cursor token)
      | Ambiguous _ -> None
    )
    values

(* The doc comments of the next item, set by the list which contains it. *)
let take_docs cursor =
  let docs = cursor.docs in
  cursor.docs <- Docstrings.empty_docs; docs

(* The doc comment of an item, in its slot, or ambiguous before it (the last
   one) or after it (the first one). *)
let item_docs cursor ~before ~after ((pre, _, post) : _ Cst.documented)
  : Docstrings.docs
  =
    let ambiguous values =
      List.filter_map
        ( function
            Cst.Ambiguous token -> Some token
          | Text _ -> None
        )
        values
    in
    let pick slot values =
      match slot, values with
      | Some token, _ -> Some (docstring cursor token)
      | None, token :: _ -> Some (docstring cursor token)
      | None, [] -> None
    in
    { docs_pre = pick pre (List.rev (ambiguous before));
      docs_post = pick post (ambiguous after)
    }

(* The conversion replays the semantic actions of ocaml/parsing/parser.mly: the
   locations of AST nodes are the spans of the tokens of the corresponding
   productions ([$sloc]), and docstrings are looked up at the same positions. *)

open Parsetree
open Ast_helper

let span (first : Location.t) (last : Location.t) =
  Location.{
    loc_start = first.loc_start; loc_end = last.loc_end; loc_ghost = false
  }

let ghost (loc : Location.t) = { loc with loc_ghost = true }

let mkloc txt loc = Location.mkloc txt loc

(* {1 Combinators over the shared shapes of the CST}

   Every token must be consumed, in source order, for locations to be right:
   these combinators consume the tokens around the converted values. *)

let skip cursor token = ignore (consume cursor token)

let skip_option cursor token = Option.iter (skip cursor) token

(* An optional token standing for a flag, such as [rec] or [mutable]. *)
let flag cursor token = skip_option cursor token; token <> None

let delimited cursor f ({ left; content; right } : (_, _, _) Cst.delimited) =
  skip cursor left;
  let value = f content in
  skip cursor right; value

(* A token followed by a value, such as [: t] or [= e]. *)
let preceded cursor f (token, value) = skip cursor token; f value

(* A value followed by a token, such as [t =] in a type synonym. *)
let followed cursor f (value, token) =
  let value = f value in
  skip cursor token; value

let non_empty_separated
    cursor f ({ first; rest } : _ Cst.non_empty_separated_list)
  =
    let first = f first in
    first :: List.map (preceded cursor f) rest

let semis cursor f ((values, trailing) : _ Cst.semis) =
  let values = non_empty_separated cursor f values in
  skip_option cursor trailing; values

(* A list of [;]-separated elements which may be empty. *)
let semis_option cursor f values =
  Option.fold ~none:[] ~some:(semis cursor f) values

let separated cursor f (list : _ Cst.separated_list) =
  match list with
  | Zero -> []
  | Many values -> non_empty_separated cursor f values

let bar_list cursor f ((bar, values) : _ Cst.bar_list) =
  skip_option cursor bar; non_empty_separated cursor f values

(* The elements of a bar list, each with the bar before it, for elements whose
   location includes their bar. *)
let bar_items ((bar, { first; rest }) : _ Cst.bar_list) =
  (bar, first) :: List.map (fun (bar, value) -> Some bar, value) rest

(* The kind of brackets, and the converted content. *)
let brackets cursor f (value : _ Cst.brackets) =
  match value with
  | Parens value -> `LPAREN, delimited cursor f value
  | Brackets value -> `LBRACKET, delimited cursor f value
  | Braces value -> `LBRACE, delimited cursor f value

(* Runs [f] and returns its result with the location of the tokens it
   consumed ([$sloc]). *)
let located cursor f =
  let value, loc = with_span cursor f in
  value, make_loc loc

(* Runs [f], which returns a function building a node from its location, and
   builds the node at the location of the consumed tokens. *)
let build cursor f =
  let make, loc = located cursor f in
  make loc

(* As [push_loc] in parser.mly; OCST puts no ghost node in parentheses. *)
let push_loc (loc : Location.t) stack =
  if loc.loc_ghost then (stack [@coverage off]) else loc :: stack

let reloc_typ ~loc typ =
  { typ with
    ptyp_loc = loc; ptyp_loc_stack = push_loc typ.ptyp_loc typ.ptyp_loc_stack
  }

let reloc_pat ~loc pat =
  { pat with
    ppat_loc = loc; ppat_loc_stack = push_loc pat.ppat_loc pat.ppat_loc_stack
  }

let reloc_exp ~loc exp =
  { exp with
    pexp_loc = loc; pexp_loc_stack = push_loc exp.pexp_loc exp.pexp_loc_stack
  }

(* {1 Helpers of the grammar} *)

let mkexpvar ~loc name =
  Exp.mk ~loc (Pexp_ident (mkloc (Longident.Lident name) loc))

let mkpatvar ~loc name = Pat.mk ~loc (Ppat_var (mkloc name loc))

let mkstrexp expr attrs =
  { pstr_desc = Pstr_eval (expr, attrs); pstr_loc = expr.pexp_loc }

let mkexp_desc_constraint expr =
  function
  | Pconstraint typ -> Pexp_constraint (expr, typ)
  | Pcoerce (ground, typ) -> Pexp_coerce (expr, ground, typ)

let mkexp_constraint ~loc expr constraint_ =
  Exp.mk ~loc (mkexp_desc_constraint expr constraint_)

let mkexp_attrs ~loc desc (ext, attrs) =
  match ext with
  | None -> Exp.mk ~loc ~attrs desc
  | Some id ->
      Exp.mk
        ~loc
        ( Pexp_extension
            (id, PStr [ mkstrexp (Exp.mk ~loc:(ghost loc) ~attrs desc) [] ])
        )

let mkpat_attrs ~loc desc (ext, attrs) =
  match ext with
  | None -> Pat.mk ~loc ~attrs desc
  | Some id ->
      Pat.mk
        ~loc
        (Ppat_extension (id, PPat (Pat.mk ~loc:(ghost loc) ~attrs desc, None)))

let rec mktailexp (nil_loc : Location.t) =
  function
  | [] ->
      Pexp_construct (mkloc (Longident.Lident "[]") (ghost nil_loc), None),
      nil_loc
  | (head : expression) :: tail ->
      let tail_desc, tail_loc = mktailexp nil_loc tail in
      let loc =
        Location.{
          head.pexp_loc with loc_end = tail_loc.loc_end; loc_ghost = false
        }
      in
      let argument =
        Exp.mk
          ~loc:(ghost loc)
          ( Pexp_tuple
              [ None, head; None, Exp.mk ~loc:(ghost tail_loc) tail_desc ]
          )
      in
      Pexp_construct (mkloc (Longident.Lident "::") (ghost loc), Some argument),
      loc

let rec mktailpat (nil_loc : Location.t) =
  function
  | [] ->
      Ppat_construct (mkloc (Longident.Lident "[]") (ghost nil_loc), None),
      nil_loc
  | (head : pattern) :: tail ->
      let tail_desc, tail_loc = mktailpat nil_loc tail in
      let loc =
        Location.{
          head.ppat_loc with loc_end = tail_loc.loc_end; loc_ghost = false
        }
      in
      let argument =
        Pat.mk
          ~loc:(ghost loc)
          ( Ppat_tuple
              ( [ None, head; None, Pat.mk ~loc:(ghost tail_loc) tail_desc ],
                Closed
              )
          )
      in
      Ppat_construct
        (mkloc (Longident.Lident "::") (ghost loc), Some ([], argument))
      ,
      loc

let neg_string text =
  if String.length text > 0 && text.[0] = '-' then
    String.sub text 1 (String.length text - 1)
  else "-" ^ text

(* [mkuminus] and [mkuplus]: signs are applied to constants. *)
let mkuminus ~sloc ~oploc name (argument : expression) =
  match name, argument.pexp_desc, argument.pexp_attributes with
  | "-", Pexp_constant { pconst_desc = Pconst_integer (n, m); _ }, [] ->
      Pexp_constant (Const.mk ~loc:sloc (Pconst_integer (neg_string n, m)))
  | ("-" | "-."), Pexp_constant { pconst_desc = Pconst_float (f, m); _ }, [] ->
      Pexp_constant (Const.mk ~loc:sloc (Pconst_float (neg_string f, m)))
  | _ -> Pexp_apply (mkexpvar ~loc:oploc ("~" ^ name), [ Nolabel, argument ])

let mkuplus ~sloc ~oploc name (argument : expression) =
  match name, argument.pexp_desc, argument.pexp_attributes with
  | "+", Pexp_constant { pconst_desc = Pconst_integer _ as desc; _ }, []
    | ("+" | "+."),
    Pexp_constant { pconst_desc = Pconst_float _ as desc; _ }, []
  -> Pexp_constant (Const.mk ~loc:sloc desc)
  | _ -> Pexp_apply (mkexpvar ~loc:oploc ("~" ^ name), [ Nolabel, argument ])

(* [mkfunction]: a function whose parameters are all type parameters is a
   sequence of [Pexp_newtype]. *)
let mkfunction params body_constraint body =
  (* The names and locations of the parameters, if they are all types. *)
  let rec newtypes =
    function
    | [] -> Some []
    | { pparam_desc = Pparam_newtype name; pparam_loc } :: rest ->
        Option.map (fun names -> (name, pparam_loc) :: names) (newtypes rest)
    | { pparam_desc = Pparam_val _; _ } :: _ -> None
  in
  let newtypes = newtypes params in
  match body, newtypes with
  | Pfunction_cases _, _ | _, None ->
      Pexp_function (params, body_constraint, body)
  | Pfunction_body body, Some newtypes ->
      let wrapped =
        match body_constraint with
        | None -> body
        | Some constraint_ ->
            Exp.mk
              ~loc:(ghost body.pexp_loc)
              (mkexp_desc_constraint body constraint_)
      in
      let expr =
        List.fold_right
          ( fun (name, (loc : Location.t)) expr ->
              Exp.mk
                ~loc:
                  ( ghost
                      (Location.{ loc with loc_end = body.pexp_loc.loc_end })
                  )
                (Pexp_newtype (name, expr))
          )
          newtypes wrapped
      in
      expr.pexp_desc

(* Indexing operators, as desugared by the grammar. *)
type index_dimension = One | Two | Three | Many

let builtin_index_name ~loc ~assign paren dimension =
  let name = if assign then "set" else "get" in
  let name = if !Clflags.unsafe then "unsafe_" ^ name else name in
  let prefix =
    match paren with
    | `LPAREN -> Longident.Lident "Array"
    | `LBRACKET -> Longident.Lident "String"
    | `LBRACE ->
        let array =
          match dimension with
          | One -> "Array1"
          | Two -> "Array2"
          | Three -> "Array3"
          | Many -> "Genarray"
        in
        Longident.Ldot
          ( Location.mknoloc (Longident.Lident "Bigarray"),
            Location.mknoloc array
          )
  in
  mkloc
    (Longident.Ldot (Location.mknoloc prefix, Location.mknoloc name))
    (ghost loc)

let builtin_index ~loc paren index =
  match paren with
  | `LPAREN | `LBRACKET -> One, [ Asttypes.Nolabel, index ]
  | `LBRACE ->
      let coordinates =
        match index.pexp_desc with
        | Pexp_tuple elements
          when List.for_all (fun (label, _) -> label = None) elements
        -> List.map snd elements
        | _ -> [ index ]
      in
      match coordinates with
      | [ x ] -> One, [ Asttypes.Nolabel, x ]
      | [ x; y ] -> Two, [ Nolabel, x; Nolabel, y ]
      | [ x; y; z ] -> Three, [ Nolabel, x; Nolabel, y; Nolabel, z ]
      | _ -> Many, [ Nolabel, Exp.mk ~loc:(ghost loc) (Pexp_array coordinates) ]

let user_index_name ~loc prefix operator ~assign paren dimension =
  let left, right =
    match paren with
      `LPAREN -> "(", ")"
    | `LBRACKET -> "[", "]"
    | `LBRACE -> "{", "}"
  in
  let name =
    String.concat
      ""
      [ "."; operator; left; (if dimension = One then "" else ";..");
        right; (if assign then "<-" else "")
      ]
  in
  let lid =
    match prefix with
    | None -> Longident.Lident name
    | Some prefix ->
        Longident.Ldot (Location.mknoloc prefix, Location.mknoloc name)
  in
  mkloc lid (ghost loc)

(* The lexer rejects the identifiers which are not valid UTF-8. *)
let normalize name =
  match Misc.Utf8_lexeme.normalize name with
    Ok name -> name
  | Error name -> (name [@coverage off])

(* {1 Identifiers} *)

let lident_name (token : Cst.lident) =
  match token.kind with `LIDENT name -> Token.Lident.to_string name

let uident_name (token : Cst.uident) =
  match token.kind with `UIDENT name -> Token.Uident.to_string name

let ident_name (token : Cst.ident_token) =
  match token.kind with
  | `LIDENT name -> Token.Lident.to_string name
  | `UIDENT name -> Token.Uident.to_string name

let label_name
    ( token
    : [< `LABEL of Token.Lident.t | `OPTLABEL of Token.Lident.t ] Cst.tok
    )
  =
    match token.kind with
      `LABEL name | `OPTLABEL name -> Token.Lident.to_string name

(* A name, located at its token. *)
let name cursor to_string token = mkloc (to_string token) (consume cursor token)
let lident cursor = name cursor lident_name
let uident cursor = name cursor uident_name
let ident cursor = name cursor ident_name

(* A name after a prefix token, located at both: ['a], [`A]. *)
let prefixed_name cursor prefix name =
  let first = consume cursor prefix in
  let name = ident cursor name in
  mkloc name.txt (span first name.loc)

let type_variable cursor ((quote, name) : Cst.type_variable) =
  prefixed_name cursor quote name

let name_tag cursor ((backquote, name) : Cst.name_tag) =
  prefixed_name cursor backquote name

let rec module_path cursor (path : Cst.module_path) : Longident.t Location.loc =
  match path with
  | Path_ident name ->
      Location.map (fun name -> Longident.Lident name) (uident cursor name)
  | Path_dot (prefix, dot, name) ->
      let prefix = module_path cursor prefix in
      let name = preceded cursor (uident cursor) (dot, name) in
      mkloc (Longident.Ldot (prefix, name)) (span prefix.loc name.loc)
  | Path_apply (functor_, argument) ->
      let functor_ = module_path cursor functor_ in
      let argument = delimited cursor (module_path cursor) argument in
      mkloc
        (Longident.Lapply (functor_, argument))
        (span functor_.loc (here cursor))

(* [mk_longident(prefix, final)]. *)
let longident cursor ((qualifier, name) : 'name Cst.longident) final =
  match qualifier with
  | None -> Location.map (fun name -> Longident.Lident name) (final name)
  | Some qualifier ->
      let path = followed cursor (module_path cursor) qualifier in
      let name = final name in
      mkloc (Longident.Ldot (path, name)) (span path.loc name.loc)

let operator_token_text (token : Cst.operator_token) =
  match token.kind with
  | `PREFIXOP op -> Token.Prefixop.to_string op
  | `LETOP op -> Token.Letop.to_string op
  | `ANDOP op -> Token.Andop.to_string op
  | `HASHOP op -> Token.Hashop.to_string op
  | `INFIXOP0 op -> Token.Infixop0.to_string op
  | `INFIXOP1 op -> Token.Infixop1.to_string op
  | `INFIXOP2 op -> Token.Infixop2.to_string op
  | `INFIXOP3 op -> Token.Infixop3.to_string op
  | `INFIXOP4 op -> Token.Infixop4.to_string op
  | _ -> Token.text token

let bracket_texts =
  function
  | `LPAREN -> "(", ")"
  | `LBRACKET -> "[", "]"
  | `LBRACE -> "{", "}"

let operator cursor (operator : Cst.operator) =
  match operator with
  | Operator token -> skip cursor token; operator_token_text token
  | Index_operator (dotop, value, assign) ->
      skip cursor dotop;
      let kind, index =
        brackets
          cursor
          ( Option.map
              (fun (semi, dotdot) -> skip cursor semi; skip cursor dotdot)
          )
          value
      in
      skip_option cursor assign;
      let op = match dotop.kind with `DOTOP op -> Token.Dotop.to_string op in
      let left, right = bracket_texts kind in
        "." ^ op ^ left ^ (if index = None then "" else ";..")
      ^ right ^ (if assign = None then "" else "<-")

let val_ident cursor (value : Cst.val_ident) =
  match value with
  | Val_lident name -> lident cursor name
  | Val_operator value ->
      let text, loc =
        located cursor (fun () -> delimited cursor (operator cursor) value)
      in
      mkloc text loc

let constr_ident cursor (value : Cst.constr_ident) =
  let located_text text f =
    let (), loc = located cursor f in
    mkloc text loc
  in
  match value with
  | Constr_uident name -> uident cursor name
  | Constr_cons value ->
      located_text "::" (fun () -> delimited cursor (skip cursor) value)
  | Constr_nil value ->
      located_text "[]" (fun () -> delimited cursor ignore value)
  | Constr_unit value ->
      located_text "()" (fun () -> delimited cursor ignore value)
  | Constr_false token -> mkloc "false" (consume cursor token)
  | Constr_true token -> mkloc "true" (consume cursor token)

let attr_id_text (token : Cst.attr_id_token) =
  match token.kind with
  | `LIDENT name -> Token.Lident.to_string name
  | `UIDENT name -> Token.Uident.to_string name
  | _ -> Token.text token

let attr_id cursor (id : Cst.attr_id) =
  let texts, loc =
    located
      cursor
      ( fun () ->
          non_empty_separated
            cursor (fun token -> skip cursor token; attr_id_text token) id
      )
  in
  mkloc (String.concat "." texts) loc

let module_name cursor (value : Cst.module_name) =
  match value with
  | Module_name name -> Location.map Option.some (uident cursor name)
  | Module_anonymous underscore -> mkloc None (consume cursor underscore)

let rec_flag cursor : Cst.rec_flag -> Asttypes.rec_flag =
  function
  | None -> Nonrecursive
  | Some token -> skip cursor token; Recursive

let private_flag cursor token : Asttypes.private_flag =
  if flag cursor token then Private else Public

let virtual_flag cursor token : Asttypes.virtual_flag =
  if flag cursor token then Virtual else Concrete

let override_flag cursor token : Asttypes.override_flag =
  if flag cursor token then Override else Fresh

(* A list of flag tokens, such as [mutable virtual]: whether it contains
   [kind]. *)
let flags cursor (tokens : [< Token.any_kind ] Cst.tok list) =
  List.iter (skip cursor) tokens;
  fun kind ->
    List.exists
      (fun (token : _ Cst.tok) -> (token.kind :> Token.any_kind) = kind) tokens

let arrow_label cursor (label : Cst.arrow_label) : Asttypes.arg_label =
  match label with
  | Nolabel -> Nolabel
  | Labelled label -> Labelled (followed cursor (lident cursor) label).txt
  | Optional token -> skip cursor token; Optional (label_name token)
  | Optional_split (question, name, colon) ->
      skip cursor question;
      Optional (followed cursor (lident cursor) (name, colon)).txt

(* {1 Constants} *)

let string_constant (literal : Token.string_literal) (loc : Location.t) =
  let opening =
    match literal.delimiter with
    | None -> "\""
    | Some delimiter ->
        "{" ^ Token.Quoted_string_delimiter.to_string delimiter ^ "|"
  in
  let content_start = advance loc.loc_start opening in
  let content_end = advance content_start literal.contents in
  Pconst_string
    ( literal.value, make_loc (content_start, content_end),
      Option.map
        ( fun delimiter ->
            normalize (Token.Quoted_string_delimiter.to_string delimiter)
        )
        literal.delimiter
    )

let constant cursor (constant : Cst.constant) =
  let loc = consume cursor constant in
  let desc =
    match constant.kind with
    | `INT (literal : Token.integer_literal) ->
        Pconst_integer
          (Token.Integer_digits.to_string literal.digits, literal.suffix)
    | `FLOAT (literal : Token.float_literal) ->
        Pconst_float
          (Token.Float_digits.to_string literal.digits, literal.suffix)
    | `CHAR (literal : Token.char_literal) -> Pconst_char literal.value
    | `STRING literal -> string_constant literal loc
  in
  Const.mk ~loc desc

(* [signed_constant]: the sign is part of the constant. *)
let signed_constant cursor (signed_constant : Cst.signed_constant) =
  match signed_constant with
  | Unsigned value -> constant cursor value
  | Signed (sign, value) ->
      let first = consume cursor sign in
      let value = constant cursor (value :> Cst.constant) in
      let desc =
        match sign.kind, value.pconst_desc with
        | `MINUS, Pconst_integer (n, m) -> Pconst_integer ("-" ^ n, m)
        | `MINUS, Pconst_float (f, m) -> Pconst_float ("-" ^ f, m)
        | _, desc -> desc
      in
      Const.mk ~loc:(span first value.pconst_loc) desc

(* {1 Type parameters} *)

let variance cursor (tokens : Cst.variance_token list) =
  List.iter (skip cursor) tokens;
  let text (token : Cst.variance_token) =
    match token.kind with
    | `INFIXOP2 op -> Token.Infixop2.to_string op
    | `PREFIXOP op -> Token.Prefixop.to_string op
    | _ -> Token.text token
  in
  let open Asttypes in
  match String.concat "" (List.map text tokens) with
  | "" -> NoVariance, NoInjectivity
  | "+" -> Covariant, NoInjectivity
  | "-" -> Contravariant, NoInjectivity
  | "!" -> NoVariance, Injective
  | "+!" | "!+" -> Covariant, Injective
  | "-!" | "!-" -> Contravariant, Injective
  | "+-" | "-+" -> Bivariant, NoInjectivity
  | "+-!" | "-+!" | "!+-" | "!-+" -> Bivariant, Injective
  | text ->
      (* The parser checks the variances. *)
      (raise (Unsupported ("variance " ^ text)) [@coverage off])

let type_parameter cursor ((tokens, variable) : Cst.type_parameter) =
  let variance = variance cursor tokens in
  let typ =
    match variable with
    | `Var variable ->
        let name = type_variable cursor variable in
        Typ.mk ~loc:name.loc (Ptyp_var name.txt)
    | `Any underscore -> Typ.mk ~loc:(consume cursor underscore) Ptyp_any
  in
  typ, variance

let type_parameters cursor (parameters : Cst.type_parameters) =
  match parameters with
  | No_type_parameters -> []
  | One_type_parameter parameter -> [ type_parameter cursor parameter ]
  | Many_type_parameters value ->
      delimited cursor (separated cursor (type_parameter cursor)) value

(* An optional list of parameters between square brackets, for classes. *)
let class_parameters cursor element value =
  match value with
  | None -> []
  | Some value -> delimited cursor (separated cursor element) value

(* {1 Lists of items with their doc comments}

   [text_str], [extra_str] and their equivalents for signatures and classes:
   the floating doc comments are text items, in source order. [item] converts
   an item, or returns [None] for [;;]; the items which [claims] take the
   ambiguous doc comments around them. *)

let elements cursor ~claims text item (values : _ Cst.element list) =
  (* The floating doc comments before each item, and after it. *)
  let rec floating =
    function
    | Cst.Floating value :: rest ->
        let values, rest = floating rest in
        value :: values, rest
    | rest -> [], rest
  in
  let rec loop before =
    function
    | [] -> []
    | Cst.Floating _ :: _ as values ->
        let values, rest = floating values in
        List.iter (skip_floating cursor) values;
        text (texts cursor values) @ loop values rest
    | Cst.Item ((pre, value, post) as documented) :: rest ->
        let after, _ = floating rest in
        cursor.docs <-
          ( if claims value then item_docs cursor ~before ~after documented
            else Docstrings.empty_docs
          )
        ;
        skip_docs cursor pre;
        let item = item value in
        cursor.docs <- Docstrings.empty_docs;
        skip_docs cursor post;
        Option.to_list item @ loop [] rest
  in
  loop [] values

(* The doc comments of the items of a group: those of the group go to its
   first and last items, and those before [and] to the items around them;
   the floating ones are the text of the next item. *)
let group_docs cursor (docs : Docstrings.docs) (_, rest) =
  let rec loop pre =
    function
    | [] -> [ { Docstrings.docs_pre = pre; docs_post = docs.docs_post } ]
    | (~docstring:(post, values, next), _, _, _) :: rest ->
        let previous =
          item_docs cursor ~before:[] ~after:values (None, (), post)
        in
        let next = item_docs cursor ~before:values ~after:[] (next, (), None) in
          { Docstrings.docs_pre = pre; docs_post = previous.docs_post }
        :: loop next.docs_pre rest
  in
  loop docs.docs_pre rest

(* The doc comments before [and], and the text of the next item. *)
let skip_and_docs cursor ((post, values, pre) : Cst.and_docs) =
  skip_docs cursor post;
  List.iter (skip_floating cursor) values;
  skip_docs cursor pre;
  texts cursor values

(* {1 The main syntactic categories} *)

(* The start of an item of a group, up to its name: its extension, its
   attributes and its floating docstrings. *)
type item_head =
  string Location.loc option * attributes * Docstrings.text * Docstrings.docs

let rec attribute cursor (value : Cst.attribute) =
  let (name, payload), loc =
    located
      cursor (fun () -> delimited cursor (attribute_contents cursor) value)
  in
  Attr.mk ~loc name payload

and attribute_contents cursor ((id, value) : Cst.attribute_contents) =
  let id = attr_id cursor id in
  id, payload cursor value

and attributes cursor values = List.map (attribute cursor) values

and payload cursor (value : Cst.payload) =
  match value with
  | Payload_structure items -> PStr (structure cursor items)
  | Payload_signature (colon, items) ->
      PSig (preceded cursor (signature cursor) (colon, items))
  | Payload_type (colon, typ) ->
      PTyp (preceded cursor (core_type cursor) (colon, typ))
  | Payload_pattern (question, value, guard) ->
      let value = preceded cursor (pattern cursor) (question, value) in
      PPat (value, Option.map (preceded cursor (expr cursor)) guard)

and extension cursor (value : Cst.extension) : Parsetree.extension =
  match value with
  | Extension value -> delimited cursor (attribute_contents cursor) value
  | Quoted_extension token ->
      let loc = consume cursor token in
      let prefix, (quoted : Token.quoted_extension) =
        match token.kind with
        | `QUOTED_STRING_EXPR quoted -> "{%", quoted
        | `QUOTED_STRING_ITEM quoted -> "{%%", quoted
      in
      (* The identifier is located by characters, as in the lexer. *)
      let id = normalize quoted.id in
      let id_start =
        { loc.loc_start with
          pos_cnum = loc.loc_start.pos_cnum + String.length prefix
        }
      in
      let id_end =
        { id_start with pos_cnum = id_start.pos_cnum + String.length id }
      in
      let content_start =
        advance
          loc.loc_start
          (prefix ^ quoted.id ^ quoted.blanks ^ quoted.delimiter ^ "|")
      in
      let content_loc =
        make_loc (content_start, advance content_start quoted.contents)
      in
      let constant =
        Const.mk
          ~loc:content_loc
          ( Pconst_string
              (quoted.value, content_loc, Some (normalize quoted.delimiter))
          )
      in
      let expr = Exp.mk ~loc:(ghost loc) (Pexp_constant constant) in
      mkloc id (make_loc (id_start, id_end)), PStr [ mkstrexp expr [] ]

and ext_attributes cursor ((ext, values) : Cst.ext_attributes) =
  let ext = Option.map (preceded cursor (attr_id cursor)) ext in
  ext, attributes cursor values

(* The attributes after a node, which do not change its location. *)
and with_attributes
  : 'a. cursor -> ('a -> attribute -> 'a) -> 'a -> Cst.attribute list -> 'a
  =
  fun cursor add value values ->
    List.fold_left add value (attributes cursor values)

(* {2 Core types} *)

and core_type cursor ({ node; attributes = values } : Cst.core_type) =
  let typ =
    match node with
    | Typ_paren value ->
        (* [LPAREN core_type RPAREN] keeps the location of the inner type. *)
        delimited cursor (core_type cursor) value
    | _ -> build cursor (fun () -> core_type_node cursor node)
  in
  with_attributes cursor Typ.attr typ values

(* The domain of an arrow type: [('a. t)] is relocated to its parentheses. *)
and param_type cursor (typ : Cst.core_type) =
  match typ with
  | { node = Typ_paren ({ content = { node = Typ_poly _; _ }; _ } as value);
      attributes = []
    }
  ->
      let typ, loc =
        located cursor (fun () -> delimited cursor (core_type cursor) value)
      in
      reloc_typ ~loc typ
  | _ -> core_type cursor typ

and type_arguments cursor (arguments : Cst.type_arguments) =
  match arguments with
  | No_type_arguments -> []
  | Type_argument typ -> [ core_type cursor typ ]
  | Type_arguments value ->
      delimited cursor (separated cursor (core_type cursor)) value

(* A labeled tuple element: [x:t]. *)
and tuple_label cursor label =
  Option.map (fun label -> (followed cursor (lident cursor) label).txt) label

and core_type_node cursor (node : Cst.core_type_node) : Location.t -> core_type
  =
    let simple desc loc = Typ.mk ~loc desc in
    match node with
    | Typ_any token -> skip cursor token; simple Ptyp_any
    | Typ_var variable -> simple (Ptyp_var (type_variable cursor variable).txt)
    | Typ_arrow (label, (domain, doc), arrow, codomain) ->
        let label = arrow_label cursor label in
        let domain = param_type cursor domain in
        (* [extra_rhs]: the docstring after the domain. *)
        let info = info cursor doc in
        let domain =
          { domain with
            ptyp_attributes =
              Docstrings.add_info_attrs info domain.ptyp_attributes
          }
        in
        let codomain = preceded cursor (core_type cursor) (arrow, codomain) in
        simple (Ptyp_arrow (label, domain, codomain))
    | Typ_functor (label, parameter, arrow, codomain) ->
        let label = arrow_label cursor label in
        let name, package =
          delimited
            cursor
            ( fun
                ( (module_, ext, name, colon, package)
                : Cst.functor_type_parameter
                )
              ->
                skip cursor module_;
                let _, attrs = ext_attributes cursor ext in
                let name = uident cursor name in
                let package =
                  preceded cursor (package_type cursor) (colon, package)
                in
                name, { package with ppt_attrs = attrs @ package.ppt_attrs }
            )
            parameter
        in
        let codomain = preceded cursor (core_type cursor) (arrow, codomain) in
        simple (Ptyp_functor (label, name, package, codomain))
    | Typ_tuple elements ->
        let element (label, typ) =
          let label = tuple_label cursor label in
          label, core_type cursor typ
        in
        simple (Ptyp_tuple (non_empty_separated cursor element elements))
    | Typ_constr (arguments, path) ->
        let arguments = type_arguments cursor arguments in
        simple (Ptyp_constr (longident cursor path (lident cursor), arguments))
    | Typ_class (arguments, hash, path) ->
        let arguments = type_arguments cursor arguments in
        let path =
          preceded
            cursor (fun path -> longident cursor path (lident cursor))
            (hash, path)
        in
        simple (Ptyp_class (path, arguments))
    | Typ_object value ->
        let fields, closed =
          delimited
            cursor
            ( fun (fields, dotdot) ->
                let fields = List.map (object_field cursor) fields in
                fields, (if flag cursor dotdot then Asttypes.Open else Closed)
            )
            value
        in
        simple (Ptyp_object (fields, closed))
    | Typ_alias (typ, as_, variable) ->
        let typ = core_type cursor typ in
        simple
          ( Ptyp_alias
              (typ, preceded cursor (type_variable cursor) (as_, variable))
          )
    | Typ_variant ({ left = opening; _ } as value) ->
        let fields, tags =
          delimited
            cursor
            ( fun ((leading_bar, fields, tags) : Cst.variant_fields) ->
                skip_option cursor leading_bar;
                let fields = separated cursor (row_field cursor) fields in
                let tag tag = (name_tag cursor tag).txt in
                fields, Option.map (preceded cursor (List.map tag)) tags
            )
            value
        in
        let closed, labels =
          match opening.kind with
          | `LBRACKET -> Asttypes.Closed, None
          | `LBRACKETGREATER -> Asttypes.Open, None
          | `LBRACKETLESS ->
              Asttypes.Closed, Some (Option.value tags ~default:[])
        in
        simple (Ptyp_variant (fields, closed, labels))
    | Typ_poly (variables, dot, typ) ->
        let variables = List.map (type_variable cursor) variables in
        simple
          (Ptyp_poly (variables, preceded cursor (core_type cursor) (dot, typ)))
    | Typ_package value ->
        let ext, package =
          delimited
            cursor
            ( fun ((module_, ext, package) : Cst.package_type) ->
                skip cursor module_;
                let ext = ext_attributes cursor ext in
                ext, package_type cursor package
            )
            value
        in
        (* [mktyp_attrs]. *)
        fun loc ->
          begin match ext with
          | None, attrs -> Typ.mk ~loc ~attrs (Ptyp_package package)
          | Some id, attrs ->
              Typ.mk
                ~loc
                ( Ptyp_extension
                    ( id,
                      PTyp
                        (Typ.mk ~loc:(ghost loc) ~attrs (Ptyp_package package))
                    )
                )
          end
    | Typ_open (path, dot, typ) ->
        let path = followed cursor (module_path cursor) (path, dot) in
        simple (Ptyp_open (path, core_type cursor typ))
    | Typ_extension value -> simple (Ptyp_extension (extension cursor value))
    | Typ_paren _ -> assert false

and row_field cursor (field : Cst.row_field) =
  match field with
  | Row_tag (tag, of_, values, doc) ->
      build
        cursor
        ( fun () ->
            let label = name_tag cursor tag in
            let constant, types =
              match of_ with
              | None -> true, []
              | Some (of_, ampersand, types) ->
                  skip cursor of_;
                  let constant = flag cursor ampersand in
                  constant, separated cursor (core_type cursor) types
            in
            let attrs = attributes cursor values in
            let attrs = Docstrings.add_info_attrs (info cursor doc) attrs in
            fun loc -> Rf.tag ~loc ~attrs label constant types
        )
  | Row_inherit typ ->
      build
        cursor
        ( fun () ->
            let typ = core_type cursor typ in
            fun loc -> Rf.inherit_ ~loc typ
        )

and object_field cursor (field : Cst.object_field) =
  match field with
  | Object_tag (name, colon, typ, values, doc, semi) ->
      build
        cursor
        ( fun () ->
            let label = lident cursor name in
            let typ = preceded cursor (core_type cursor) (colon, typ) in
            let attrs, info =
              field_end cursor (attributes cursor values) doc semi
            in
            fun loc ->
              Of.tag
                ~loc ~attrs:(Docstrings.add_info_attrs info attrs) label typ
        )
  | Object_inherit (typ, semi) ->
      let typ, loc = located cursor (fun () -> core_type cursor typ) in
      skip_option cursor semi; Of.inherit_ ~loc typ

(* The end of a record or object field: the attributes after its [;], and its
   doc comment, which may be before the [;] or after it. *)
and field_end cursor attrs doc semi =
  let before = info cursor doc in
  match semi with
  | None -> attrs, before
  | Some (semi, values, doc) ->
      skip cursor semi;
      let attrs = attrs @ attributes cursor values in
      let after = info cursor doc in
      attrs,
      ( match before with
          Some _ -> before
        | None -> after
      )

(* [package_type_]: a module type restricted to a path with type constraints. *)
and package_type cursor (typ : Cst.module_type) =
  let typ, loc = located cursor (fun () -> module_type cursor typ) in
  let constraint_ =
    function
    | Pwith_type
        ( lid,
          { ptype_params = []; ptype_constraints = [];
            ptype_private = Public; ptype_manifest = Some typ; _
          }
        )
    -> lid, typ
    | _ ->
        (* The parser checks the package types. *)
        (raise (Unsupported "package type constraint") [@coverage off])
  in
  match typ with
  | { pmty_desc = Pmty_ident lid; pmty_attributes; _ } ->
      Typ.package_type ~loc ~attrs:pmty_attributes lid []
  | { pmty_desc = Pmty_with ({ pmty_desc = Pmty_ident lid; _ }, constraints);
      pmty_attributes; _
    }
  ->
      Typ.package_type
        ~loc ~attrs:pmty_attributes lid (List.map constraint_ constraints)
  | _ -> (raise (Unsupported "package type") [@coverage off])

(* {2 Type declarations} *)

and type_constraint_clause
    cursor ((constraint_, left, equal, right) : Cst.type_constraint_clause)
  =
    skip cursor constraint_;
    let (left, right), loc =
      located
        cursor
        ( fun () ->
            let left = core_type cursor left in
            left, preceded cursor (core_type cursor) (equal, right)
        )
    in
    left, right, loc

and label_declaration
    cursor
    ((mutable_, name, colon, typ, values, doc, semi) : Cst.label_declaration)
  =
    build
      cursor
      ( fun () ->
          let mut : Asttypes.mutable_flag =
            if flag cursor mutable_ then Mutable else Immutable
          in
          let name = lident cursor name in
          let typ = preceded cursor (core_type cursor) (colon, typ) in
          let attrs, info = field_end cursor (attributes cursor values) doc semi
          in
          fun loc -> Type.field name typ ~mut ~attrs ~loc ~info
      )

and label_declarations cursor labels =
  delimited cursor (List.map (label_declaration cursor)) labels

and constructor_arguments cursor (arguments : Cst.constructor_arguments) =
  match arguments with
  | Constructor_tuple types ->
      Pcstr_tuple (separated cursor (core_type cursor) types)
  | Constructor_record labels -> Pcstr_record (label_declarations cursor labels)

and constructor_signature cursor (signature : Cst.constructor_signature) =
  match signature with
  | Args_none -> [], Pcstr_tuple [], None
  | Args_of (of_, arguments) ->
      [], preceded cursor (constructor_arguments cursor) (of_, arguments), None
  | Args_gadt (colon, variables, arguments, result) ->
      skip cursor colon;
      let variables =
        match variables with
        | None -> []
        | Some variables ->
            followed cursor (List.map (type_variable cursor)) variables
      in
      let arguments =
        match arguments with
        | None -> Pcstr_tuple []
        | Some arguments ->
            followed cursor (constructor_arguments cursor) arguments
      in
      variables, arguments, Some (core_type cursor result)

(* [generic_constructor_declaration]: the location includes the leading bar. *)
and constructor
    cursor bar ((name, arguments, values) : Cst.constructor_declaration) doc
  =
    let (name, (vars, args, res), attrs), loc =
      located
        cursor
        ( fun () ->
            skip_option cursor bar;
            let name = constr_ident cursor name in
            let signature = constructor_signature cursor arguments in
            name, signature, attributes cursor values
        )
    in
    name, vars, args, res, attrs, loc, info cursor doc

and type_definition cursor (definition : Cst.type_definition) =
  let synonym = Option.map (followed cursor (core_type cursor)) in
  match definition with
  | Def_manifest (private_, typ) ->
      let priv = private_flag cursor private_ in
      Ptype_abstract, priv, Some (core_type cursor typ)
  | Def_variant (manifest, private_, constructors) ->
      let manifest = synonym manifest in
      let priv = private_flag cursor private_ in
      let constructors =
        match constructors with
        | Constructors_empty bar -> skip cursor bar; []
        | Constructors values ->
            List.map
              ( fun (bar, (value, doc)) ->
                  let name, vars, args, res, attrs, loc, info =
                    constructor cursor bar value doc
                  in
                  Type.constructor name ~vars ~args ?res ~attrs ~loc ~info
              )
              (bar_items values)
      in
      Ptype_variant constructors, priv, manifest
  | Def_open (manifest, private_, dotdot) ->
      let manifest = synonym manifest in
      let priv = private_flag cursor private_ in
      skip cursor dotdot; Ptype_open, priv, manifest
  | Def_record (manifest, private_, labels) ->
      let manifest = synonym manifest in
      let priv = private_flag cursor private_ in
      Ptype_record (label_declarations cursor labels), priv, manifest
  | Def_external (external_, name) ->
      Ptype_external (preceded cursor (string_value cursor) (external_, name)),
      Public, None

and string_value cursor (token : Cst.string_token) =
  skip cursor token; match token.kind with `STRING literal -> literal.value

(* [generic_type_declaration] and [generic_and_type_declaration]: [first] is
   the location of the [type] or [and] keyword. *)
and type_declaration
    cursor ~(first : Location.t) ~attrs1 ~text
    ( (parameters, name, kind, constraints, post_attributes)
    : Cst.type_declaration
    )
  =
    let docs = take_docs cursor in
    let params = type_parameters cursor parameters in
    let name = lident cursor name in
    let kind, priv, manifest =
      match kind with
      | Kind_abstract -> Ptype_abstract, Asttypes.Public, None
      | Kind_defined (equal, definition) ->
          preceded cursor (type_definition cursor) (equal, definition)
    in
    let constraints = List.map (type_constraint_clause cursor) constraints in
    let attrs = attrs1 @ attributes cursor post_attributes in
    let loc = span first (here cursor) in
    Type.mk
      name ~params ~constraints ~kind ~priv ?manifest ~attrs ~loc ~docs ~text

and type_declarations
    cursor
    ( (type_, ext, nonrec_, ((first, rest) as declarations))
    : Cst.type_declarations
    )
  =
    let docs = group_docs cursor (take_docs cursor) declarations in
    let first_loc = consume cursor type_ in
    let ext, attrs1 = ext_attributes cursor ext in
    let flag : Asttypes.rec_flag =
      if flag cursor nonrec_ then Nonrecursive else Recursive
    in
    cursor.docs <- List.hd docs;
    let first = type_declaration cursor ~first:first_loc ~attrs1 ~text:[] first
    in
    let rest =
      List.map2
        ( fun (~docstring, and_, values, declaration) docs ->
            let text = skip_and_docs cursor docstring in
            let first = consume cursor and_ in
            let attrs1 = attributes cursor values in
            cursor.docs <- docs;
            type_declaration cursor ~first ~attrs1 ~text declaration
        )
        rest (List.tl docs)
    in
    (flag, ext), first :: rest

and extension_constructor
    cursor bar ((value, doc) : Cst.extension_constructor * _)
  =
    match value with
    | Ext_decl declaration ->
        let name, vars, args, res, attrs, loc, info =
          constructor cursor bar declaration doc
        in
        Te.decl name ~vars ~args ?res ~attrs ~loc ~info
    | Ext_rebind (name, equal, path, values) ->
        build
          cursor
          ( fun () ->
              skip_option cursor bar;
              let name = constr_ident cursor name in
              let path =
                preceded
                  cursor
                  (fun path -> longident cursor path (constr_ident cursor))
                  (equal, path)
              in
              let attrs = attributes cursor values in
              let info = info cursor doc in
              fun loc -> Te.rebind name path ~attrs ~loc ~info
          )

(* An item which may have an extension: the item, the extension and the
   location. [f] returns the extension and a function building the item from
   its location. *)
and item_with_ext
  : 'a.
      cursor -> (unit -> string Location.loc option * (Location.t -> 'a))
      -> 'a * string Location.loc option * Location.t
  =
  fun cursor f ->
    let (ext, make), loc = located cursor f in
    make loc, ext, loc

and type_extension
    cursor
    ( ( type_, ext, parameters, path, pluseq,
        private_, constructors, post_attributes
      )
    : Cst.type_extension
    )
  =
    let docs = take_docs cursor in
    item_with_ext
      cursor
      ( fun () ->
          skip cursor type_;
          let ext, attrs1 = ext_attributes cursor ext in
          let params = type_parameters cursor parameters in
          let path = longident cursor path (lident cursor) in
          skip cursor pluseq;
          let priv = private_flag cursor private_ in
          let constructors =
            List.map
              (fun (bar, value) -> extension_constructor cursor bar value)
              (bar_items constructors)
          in
          let attrs = attrs1 @ attributes cursor post_attributes in
          ext,
          fun loc -> Te.mk path constructors ~params ~priv ~attrs ~docs ~loc
      )

and exception_declaration
    cursor
    ((exception_, ext, value, post_attributes) : Cst.exception_declaration)
  =
    let docs = take_docs cursor in
    let first = consume cursor exception_ in
    let ext, attrs1 = ext_attributes cursor ext in
    match value with
    | Ext_decl (name, arguments, values) ->
        (* The exception constructor does not include the post-item attributes. *)
        let name = constr_ident cursor name in
        let vars, args, res = constructor_signature cursor arguments in
        let attrs2 = attributes cursor values in
        let loc = span first (here cursor) in
        let attrs = attributes cursor post_attributes in
        let item_loc = span first (here cursor) in
        Te.mk_exception
          ~attrs ~loc
          (Te.decl name ~vars ~args ?res ~attrs:(attrs1 @ attrs2) ~loc ~docs)
        ,
        ext, item_loc
    | Ext_rebind (name, equal, path, values) ->
        let name = constr_ident cursor name in
        let path =
          preceded
            cursor (fun path -> longident cursor path (constr_ident cursor))
            (equal, path)
        in
        let attrs2 = attributes cursor values in
        let attrs = attributes cursor post_attributes in
        let loc = span first (here cursor) in
        Te.mk_exception
          ~attrs ~loc (Te.rebind name path ~attrs:(attrs1 @ attrs2) ~loc ~docs)
        ,
        ext, loc

(* [value_description] and [primitive_declaration]. *)
and value_description
    cursor
    ( (keyword, ext, name, colon, typ, primitives, post_attributes)
    : Cst.value_description
    )
  =
    let docs = take_docs cursor in
    item_with_ext
      cursor
      ( fun () ->
          skip cursor keyword;
          let ext, attrs1 = ext_attributes cursor ext in
          let name = val_ident cursor name in
          let typ = preceded cursor (core_type cursor) (colon, typ) in
          let prim =
            match primitives with
            | None -> []
            | Some primitives ->
                preceded cursor (List.map (string_value cursor)) primitives
          in
          let attrs = attrs1 @ attributes cursor post_attributes in
          ext, fun loc -> Val.mk name typ ~prim ~attrs ~loc ~docs
      )

(* {2 Module types} *)

(* A functor argument, with its start position. *)
and functor_arg cursor (argument : Cst.functor_arg) =
  let argument, loc =
    located
      cursor
      ( fun () ->
          match argument with
          | Functor_unit value -> delimited cursor ignore value; Unit
          | Functor_named value ->
              delimited
                cursor
                ( fun (name, colon, typ) ->
                    let name = module_name cursor name in
                    Named
                      (name, preceded cursor (module_type cursor) (colon, typ))
                )
                value
      )
  in
  loc.loc_start, argument

and module_type cursor ({ node; attributes = values } : Cst.module_type) =
  let typ =
    match node with
    | Mty_paren value -> delimited cursor (module_type cursor) value
    | Mty_functor (functor_, args, arrow, body) ->
        let attrs =
          match functor_ with
          | None -> []
          | Some functor_ -> preceded cursor (attributes cursor) functor_
        in
        let args = List.map (functor_arg cursor) args in
        let result = preceded cursor (module_type cursor) (arrow, body) in
        (* [mk_functor_typ]: the functors end with the location of the result,
           which may not include its closing parenthesis. *)
        let typ =
          List.fold_right
            ( fun (start, argument) body ->
                Mty.mk
                  ~loc:(make_loc (start, result.pmty_loc.loc_end))
                  (Pmty_functor (argument, body))
            )
            args result
        in
        { typ with pmty_attributes = attrs @ typ.pmty_attributes }
    | _ -> build cursor (fun () -> module_type_node cursor node)
  in
  with_attributes cursor Mty.attr typ values

and module_type_node cursor (node : Cst.module_type_node)
  : Location.t -> module_type
  =
    let simple desc loc = Mty.mk ~loc desc in
    match node with
    | Mty_ident path ->
        simple (Pmty_ident (longident cursor path (ident cursor)))
    | Mty_signature value ->
        let attrs, items =
          delimited
            cursor
            ( fun (values, items) ->
                let attrs = attributes cursor values in
                attrs, signature cursor items
            )
            value
        in
        fun loc -> Mty.mk ~loc ~attrs (Pmty_signature items)
    | Mty_arrow (left, arrow, right) ->
        let left = module_type cursor left in
        let right = preceded cursor (module_type cursor) (arrow, right) in
        simple (Pmty_functor (Named (Location.mknoloc None, left), right))
    | Mty_with (typ, with_, constraints) ->
        let typ = module_type cursor typ in
        let constraints =
          preceded
            cursor (separated cursor (with_constraint cursor))
            (with_, constraints)
        in
        simple (Pmty_with (typ, constraints))
    | Mty_typeof (module_, type_, of_, values, expr) ->
        skip cursor module_;
        skip cursor type_;
        skip cursor of_;
        let attrs = attributes cursor values in
        let expr = module_expr cursor expr in
        fun loc -> Mty.mk ~loc ~attrs (Pmty_typeof expr)
    | Mty_extension value -> simple (Pmty_extension (extension cursor value))
    | Mty_paren _ | Mty_functor _ -> assert false

and with_constraint cursor (value : Cst.with_constraint) =
  match value with
  | With_type (type_, parameters, path, equal, private_, typ, constraints) ->
      build
        cursor
        ( fun () ->
            skip cursor type_;
            let params = type_parameters cursor parameters in
            let path = longident cursor path (lident cursor) in
            skip cursor equal;
            let priv = private_flag cursor private_ in
            let typ = core_type cursor typ in
            let constraints =
              List.map (type_constraint_clause cursor) constraints
            in
            fun loc ->
              Pwith_type
                ( path,
                  Type.mk
                    (Location.map Longident.last path) ~params
                    ~constraints ~manifest:typ ~priv ~loc
                )
        )
  | With_typesubst (type_, parameters, path, colonequal, typ) ->
      build
        cursor
        ( fun () ->
            skip cursor type_;
            let params = type_parameters cursor parameters in
            let path = longident cursor path (lident cursor) in
            let typ = preceded cursor (core_type cursor) (colonequal, typ) in
            fun loc ->
              Pwith_typesubst
                ( path,
                  Type.mk
                    (Location.map Longident.last path)
                    ~params ~manifest:typ ~loc
                )
        )
  | With_module (module_, path, equal, target) ->
      skip cursor module_;
      let path = module_path cursor path in
      let target = preceded cursor (module_path cursor) (equal, target) in
      begin match equal.kind with
      | `EQUAL -> Pwith_module (path, target)
      | `COLONEQUAL -> Pwith_modsubst (path, target)
      end
  | With_modtype (module_, type_, path, equal, typ) ->
      skip cursor module_;
      skip cursor type_;
      let path = longident cursor path (ident cursor) in
      let typ = preceded cursor (module_type cursor) (equal, typ) in
      begin match equal.kind with
      | `EQUAL -> Pwith_modtype (path, typ)
      | `COLONEQUAL -> Pwith_modtypesubst (path, typ)
      end

(* {2 Module expressions} *)

and module_expr cursor ({ node; attributes = values } : Cst.module_expr) =
  let expr =
    match node with
    | Mod_paren value -> delimited cursor (module_expr cursor) value
    | Mod_functor (functor_, values, args, arrow, body) ->
        let attrs = preceded cursor (attributes cursor) (functor_, values) in
        let args = List.map (functor_arg cursor) args in
        let body = preceded cursor (module_expr cursor) (arrow, body) in
        (* Unlike functor types, functors end with their last token. *)
        let stop = cursor.last_end in
        let expr =
          List.fold_right
            ( fun (start, argument) body ->
                Mod.mk
                  ~loc:(make_loc (start, stop)) (Pmod_functor (argument, body))
            )
            args body
        in
        { expr with pmod_attributes = attrs @ expr.pmod_attributes }
    | _ -> build cursor (fun () -> module_expr_node cursor node)
  in
  with_attributes cursor Mod.attr expr values

and module_expr_node cursor (node : Cst.module_expr_node)
  : Location.t -> module_expr
  =
    let simple desc loc = Mod.mk ~loc desc in
    match node with
    | Mod_ident path -> simple (Pmod_ident (module_path cursor path))
    | Mod_structure value ->
        let attrs, items =
          delimited
            cursor
            ( fun (values, items) ->
                let attrs = attributes cursor values in
                attrs, structure cursor items
            )
            value
        in
        fun loc -> Mod.mk ~loc ~attrs (Pmod_structure items)
    | Mod_apply (expr, argument) ->
        let expr = module_expr cursor expr in
        simple (Pmod_apply (expr, module_expr cursor argument))
    | Mod_apply_unit (expr, unit) ->
        let expr = module_expr cursor expr in
        delimited cursor ignore unit; simple (Pmod_apply_unit expr)
    | Mod_constraint value ->
        delimited
          cursor
          ( fun (expr, colon, typ) ->
              let expr = module_expr cursor expr in
              simple
                ( Pmod_constraint
                    (expr, preceded cursor (module_type cursor) (colon, typ))
                )
          )
          value
    | Mod_unpack value ->
        let attrs, value =
          delimited
            cursor
            ( fun ((val_, values, value, package) : Cst.val_unpack) ->
                let attrs = preceded cursor (attributes cursor) (val_, values)
                in
                attrs, expr_colon_package_type cursor value package
            )
            value
        in
        fun loc -> Mod.mk ~loc ~attrs (Pmod_unpack value)
    | Mod_extension value -> simple (Pmod_extension (extension cursor value))
    | Mod_paren _ | Mod_functor _ -> assert false

(* [expr_colon_package_type]: an expression with an optional package type. *)
and expr_colon_package_type cursor value (package : Cst.package_annotation) =
  let package_typ typ =
    let package, loc = located cursor (fun () -> package_type cursor typ) in
    Typ.mk ~loc (Ptyp_package package)
  in
  let desc, loc =
    located
      cursor
      ( fun () ->
          let value = expr cursor value in
          match package with
          | No_package -> `Expr value
          | Package_constraint constraint_ ->
              `Desc
                ( Pexp_constraint
                    (value, preceded cursor package_typ constraint_)
                )
          | Package_coerce (ground, coerce, typ) ->
              let ground = Option.map (preceded cursor package_typ) ground in
              `Desc
                ( Pexp_coerce
                    (value, ground, preceded cursor package_typ (coerce, typ))
                )
      )
  in
  match desc with
    `Expr value -> value
  | `Desc desc -> Exp.mk ~loc desc

(* {2 Class types} *)

(* [LET OPEN override_flag attributes mod_longident IN]: the attributes and
   the open description, located from [open] to the path. *)
and class_open cursor let_ open_ bang values path in_ =
  skip cursor let_;
  let open_loc = consume cursor open_ in
  let override = override_flag cursor bang in
  let attrs = attributes cursor values in
  let path = module_path cursor path in
  skip cursor in_; attrs, Opn.mk ~override ~loc:(span open_loc path.loc) path

and class_type cursor (typ : Cst.class_type) =
  match typ with
  | Cty_signature signature -> class_signature cursor signature
  | Cty_arrow (label, domain, arrow, codomain) ->
      build
        cursor
        ( fun () ->
            let label = arrow_label cursor label in
            let domain = core_type cursor domain in
            let codomain = preceded cursor (class_type cursor) (arrow, codomain)
            in
            fun loc -> Cty.mk ~loc (Pcty_arrow (label, domain, codomain))
        )

and class_signature cursor ({ node; attributes = values } : Cst.class_signature)
  =
    let typ =
      build
        cursor
        ( fun () ->
            match node with
            | Csig_constr (arguments, path) ->
                let arguments =
                  class_parameters cursor (core_type cursor) arguments
                in
                let path = longident cursor path (lident cursor) in
                fun loc -> Cty.mk ~loc (Pcty_constr (path, arguments))
            | Csig_extension value ->
                let value = extension cursor value in
                fun loc -> Cty.mk ~loc (Pcty_extension value)
            | Csig_object value ->
                let attrs, signature =
                  delimited
                    cursor
                    ( fun ((values, self, fields) : Cst.class_signature_body) ->
                        let attrs = attributes cursor values in
                        (* [class_self_type]: an empty self type is located at the
                         end of the previous token. *)
                        let self =
                          match self with
                          | Some self ->
                              delimited cursor (core_type cursor) self
                          | None -> Typ.mk ~loc:(ghost (here cursor)) Ptyp_any
                        in
                        let field field =
                          Some
                            ( build
                                cursor (fun () -> class_sig_field cursor field)
                            )
                        in
                        let claims =
                          function
                            Cst.Ctf_attribute _ -> false
                          | _ -> true
                        in
                        attrs,
                        Csig.mk
                          self (elements cursor ~claims Ctf.text field fields)
                    )
                    value
                in
                fun loc -> Cty.mk ~loc ~attrs (Pcty_signature signature)
            | Csig_open (let_, open_, bang, values, path, in_, body) ->
                let attrs, description =
                  class_open cursor let_ open_ bang values path in_
                in
                let body = class_signature cursor body in
                fun loc -> Cty.mk ~loc ~attrs (Pcty_open (description, body))
        )
    in
    with_attributes cursor Cty.attr typ values

and class_sig_field cursor (field : Cst.class_sig_field)
  : Location.t -> class_type_field
  =
    let docs = take_docs cursor in
    let with_docs desc attrs loc = Ctf.mk ~loc ~attrs ~docs desc in
    match field with
    | Ctf_inherit (inherit_, values, typ, ~post_attributes) ->
        let attrs1 = preceded cursor (attributes cursor) (inherit_, values) in
        let typ = class_signature cursor typ in
        with_docs
          (Pctf_inherit typ) (attrs1 @ attributes cursor post_attributes)
    | Ctf_val (val_, values, tokens, name, colon, typ, ~post_attributes) ->
        let attrs1 = preceded cursor (attributes cursor) (val_, values) in
        let has = flags cursor tokens in
        let name = lident cursor name in
        let typ = preceded cursor (core_type cursor) (colon, typ) in
        let mut : Asttypes.mutable_flag =
          if has `MUTABLE then Mutable else Immutable
        in
        let virt : Asttypes.virtual_flag =
          if has `VIRTUAL then Virtual else Concrete
        in
        with_docs
          (Pctf_val (name, mut, virt, typ))
          (attrs1 @ attributes cursor post_attributes)
    | Ctf_method (method_, values, tokens, name, colon, typ, ~post_attributes)
    ->
        let attrs1 = preceded cursor (attributes cursor) (method_, values) in
        let has = flags cursor tokens in
        let name = lident cursor name in
        let typ = preceded cursor (core_type cursor) (colon, typ) in
        let priv : Asttypes.private_flag =
          if has `PRIVATE then Private else Public
        in
        let virt : Asttypes.virtual_flag =
          if has `VIRTUAL then Virtual else Concrete
        in
        with_docs
          (Pctf_method (name, priv, virt, typ))
          (attrs1 @ attributes cursor post_attributes)
    | Ctf_constraint (constraint_, values, left, equal, right, ~post_attributes)
    ->
        let attrs1 = preceded cursor (attributes cursor) (constraint_, values)
        in
        let left = core_type cursor left in
        let right = preceded cursor (core_type cursor) (equal, right) in
        with_docs
          (Pctf_constraint (left, right))
          (attrs1 @ attributes cursor post_attributes)
    | Ctf_extension (value, post_attributes) ->
        let value = extension cursor value in
        with_docs (Pctf_extension value) (attributes cursor post_attributes)
    | Ctf_attribute value ->
        let value = attribute cursor value in
        fun loc -> Ctf.mk ~loc (Pctf_attribute value)

(* The head of the first item of a group, after [keyword]: its extension,
   attributes, floating docstrings and docstrings, taken before converting
   the attributes, whose payloads may have items. *)
and keyword_head
  : 'k.
      cursor -> ([< Token.any_kind ] as 'k) Cst.tok
      -> ?type_:[ `TYPE ] Cst.tok -> Cst.ext_attributes
      -> ?rec_:[ `REC ] Cst.tok -> unit
      -> item_head
  =
  fun cursor keyword ?type_ ext ?rec_ () ->
    let docs = take_docs cursor in
    skip cursor keyword;
    skip_option cursor type_;
    let ext, attrs1 = ext_attributes cursor ext in
    skip_option cursor rec_; ext, attrs1, [], docs

(* The head of the other items of a group: the doc comments before [and],
   [and] and its attributes. *)
and and_head cursor and_docs and_ values docs () : item_head =
  let text = skip_and_docs cursor and_docs in
  skip cursor and_;
  let attrs1 = attributes cursor values in
  None, attrs1, text, docs

(* A group of items: the extension of the first one, and the items, which
   [item] converts with their head. *)
and item_group
  : 'a 'b.
      cursor
      ->
      (cursor -> (unit -> item_head) -> 'a -> 'b * string Location.loc option)
      -> (unit -> item_head) -> 'a Cst.and_list
      -> string Location.loc option * 'b list
  =
  fun cursor item head ((first, rest) as group) ->
    let docs = group_docs cursor (take_docs cursor) group in
    cursor.docs <- List.hd docs;
    let first, ext = item cursor head first in
    let rest =
      List.map2
        ( fun (~docstring, and_, values, value) docs ->
            fst (item cursor (and_head cursor docstring and_ values docs) value)
        )
        rest (List.tl docs)
    in
    ext, first :: rest

(* A class description, declaration or class type declaration, with its
   extension. *)
and class_info
  : 'a 'b.
      cursor -> (cursor -> 'a -> 'b) -> (unit -> item_head) -> 'a Cst.class_info
      -> 'b class_infos * string Location.loc option
  =
  fun cursor body head (virtual_, parameters, name, value, post_attributes) ->
    let value, ext, _ =
      item_with_ext
        cursor
        ( fun () ->
            let ext, attrs1, text, docs = head () in
            let virt = virtual_flag cursor virtual_ in
            let params =
              class_parameters cursor (type_parameter cursor) parameters
            in
            let name = lident cursor name in
            let value = body cursor value in
            let attrs = attrs1 @ attributes cursor post_attributes in
            ext,
            fun loc -> Ci.mk name value ~virt ~params ~attrs ~loc ~docs ~text
        )
    in
    value, ext

(* A list of classes: the extension of the first one, and the classes. *)
and class_infos
  : 'a 'b.
      cursor -> (cursor -> 'a -> 'b) -> (unit -> item_head)
      -> 'a Cst.class_info Cst.and_list
      -> string Location.loc option * 'b class_infos list
  =
  fun cursor body head infos ->
    item_group
      cursor (fun cursor head info -> class_info cursor body head info)
      head infos

and class_description_body cursor (value : Cst.class_description_body) =
  preceded cursor (class_type cursor) value

and class_type_declaration_body cursor (value : Cst.class_type_declaration_body)
  = preceded cursor (class_signature cursor) value

(* {2 Patterns} *)

and pattern cursor ({ node; attributes = values } : Cst.pattern) =
  let pattern =
    match node with
    | Pat_paren value ->
        let pattern, loc =
          located cursor (fun () -> delimited cursor (pattern cursor) value)
        in
        reloc_pat ~loc pattern
    | _ -> build cursor (fun () -> pattern_node cursor node)
  in
  with_attributes cursor Pat.attr pattern values

(* A punned label [~x]: the label and the variable. *)
and pun_pattern cursor tilde name =
  skip cursor tilde;
  let name = lident cursor name in
  Some name.txt, mkpatvar ~loc:name.loc name.txt

and tuple_pattern_element cursor (element : Cst.tuple_pattern_element) =
  match element with
  | Tuple_pattern value -> None, pattern cursor value
  | Tuple_pattern_label (label, value) ->
      skip cursor label; Some (label_name label), pattern cursor value
  | Tuple_pattern_pun (tilde, name) -> pun_pattern cursor tilde name
  | Tuple_pattern_pun_constraint (tilde, value) ->
      skip cursor tilde;
      let (name, typ), loc =
        located
          cursor
          ( fun () ->
              delimited
                cursor
                ( fun (name, colon, typ) ->
                    let name = lident cursor name in
                    name, preceded cursor (core_type cursor) (colon, typ)
                )
                value
          )
      in
      Some name.txt,
      Pat.mk ~loc (Ppat_constraint (mkpatvar ~loc:name.loc name.txt, typ))

(* [record_pat_field]: a punned field is a variable. *)
and record_pattern_field cursor ((label, typ, value) : Cst.record_pattern_field)
  =
    let (label, typ, value), loc =
      located
        cursor
        ( fun () ->
            let label = longident cursor label (lident cursor) in
            let typ =
              Option.map
                ( fun typ ->
                    located
                      cursor (fun () -> preceded cursor (core_type cursor) typ)
                )
                typ
            in
            label, typ, Option.map (preceded cursor (pattern cursor)) value
        )
    in
    let constraint_loc, label, value =
      match value, typ with
      | None, _ ->
          loc, { label with loc = ghost label.loc },
          Pat.mk ~loc:label.loc (Ppat_var (Location.map Longident.last label))
      | Some value, Some (_, typ_loc) -> span typ_loc loc, label, value
      | Some value, None -> loc, label, value
    in
    match typ with
    | None -> label, value
    | Some (typ, _) ->
        label, Pat.mk ~loc:constraint_loc (Ppat_constraint (value, typ))

and pattern_node cursor (node : Cst.pattern_node) : Location.t -> pattern =
  let simple desc loc = Pat.mk ~loc desc in
  let constructor constructor =
    longident cursor constructor (constr_ident cursor)
  in
  match node with
  | Pat_any token -> skip cursor token; simple Ppat_any
  | Pat_var name -> simple (Ppat_var (val_ident cursor name))
  | Pat_alias (value, as_, name) ->
      let value = pattern cursor value in
      simple
        (Ppat_alias (value, preceded cursor (val_ident cursor) (as_, name)))
  | Pat_constant value -> simple (Ppat_constant (signed_constant cursor value))
  | Pat_interval (low, dotdot, high) ->
      let low = signed_constant cursor low in
      simple
        ( Ppat_interval
            (low, preceded cursor (signed_constant cursor) (dotdot, high))
        )
  | Pat_tuple (elements, open_) ->
      let elements =
        non_empty_separated cursor (tuple_pattern_element cursor) elements
      in
      let closed : Asttypes.closed_flag =
        match open_ with
        | None -> Closed
        | Some (comma, dotdot) -> skip cursor comma; skip cursor dotdot; Open
      in
      simple (Ppat_tuple (elements, closed))
  | Pat_construct (name, argument) ->
      let name = constructor name in
      let argument =
        Option.map (fun argument -> [], pattern cursor argument) argument
      in
      simple (Ppat_construct (name, argument))
  | Pat_construct_newtypes (name, newtypes, argument) ->
      let name = constructor name in
      let newtypes = delimited cursor (newtype_names cursor) newtypes in
      simple (Ppat_construct (name, Some (newtypes, pattern cursor argument)))
  | Pat_variant (tag, argument) ->
      let tag = name_tag cursor tag in
      simple (Ppat_variant (tag.txt, Option.map (pattern cursor) argument))
  | Pat_record value ->
      let fields, closed =
        delimited
          cursor
          ( fun ((fields, wildcard) : Cst.record_pattern) ->
              let fields = semis cursor (record_pattern_field cursor) fields in
              let closed : Asttypes.closed_flag =
                match wildcard with
                | None -> Closed
                | Some (underscore, semi) ->
                    skip cursor underscore; skip_option cursor semi; Open
              in
              fields, closed
          )
          value
      in
      simple (Ppat_record (fields, closed))
  | Pat_array value ->
      simple
        ( Ppat_array
            (delimited cursor (semis_option cursor (pattern cursor)) value)
        )
  | Pat_list { left; content; right } ->
      skip cursor left;
      let elements = semis cursor (pattern cursor) content in
      let nil_loc = consume cursor right in
      simple (fst (mktailpat nil_loc elements))
  | Pat_or (left, bar, right) ->
      let left = pattern cursor left in
      simple (Ppat_or (left, preceded cursor (pattern cursor) (bar, right)))
  | Pat_cons (left, cons, right) ->
      let left = pattern cursor left in
      let cons_loc = consume cursor cons in
      let right = pattern cursor right in
      fun loc ->
        let argument =
          Pat.mk
            ~loc:(ghost loc) (Ppat_tuple ([ None, left; None, right ], Closed))
        in
        Pat.mk
          ~loc
          ( Ppat_construct
              (mkloc (Longident.Lident "::") cons_loc, Some ([], argument))
          )
  | Pat_constraint value ->
      delimited
        cursor
        ( fun (value, colon, typ) ->
            let value = pattern cursor value in
            simple
              ( Ppat_constraint
                  (value, preceded cursor (core_type cursor) (colon, typ))
              )
        )
        value
  | Pat_type (hash, path) ->
      simple
        ( Ppat_type
            ( preceded
                cursor (fun path -> longident cursor path (lident cursor))
                (hash, path)
            )
        )
  | Pat_lazy (lazy_, ext, value) ->
      let ext = preceded cursor (ext_attributes cursor) (lazy_, ext) in
      let value = pattern cursor value in
      fun loc -> mkpat_attrs ~loc (Ppat_lazy value) ext
  | Pat_unpack value ->
      let ext, name, package =
        delimited
          cursor
          ( fun
              ( (module_, ext, name, package)
              : Cst.module_name Cst.packed_module
              )
            ->
              let ext = preceded cursor (ext_attributes cursor) (module_, ext)
              in
              let name = module_name cursor name in
              ext, name,
              Option.map (preceded cursor (package_type cursor)) package
          )
          value
      in
      fun loc -> mkpat_attrs ~loc (Ppat_unpack (name, package)) ext
  | Pat_exception (exception_, ext, value) ->
      let ext = preceded cursor (ext_attributes cursor) (exception_, ext) in
      let value = pattern cursor value in
      fun loc -> mkpat_attrs ~loc (Ppat_exception value) ext
  | Pat_effect (effect_, value, comma, continuation) ->
      let value = preceded cursor (pattern cursor) (effect_, value) in
      simple
        ( Ppat_effect
            (value, preceded cursor (pattern cursor) (comma, continuation))
        )
  | Pat_extension value -> simple (Ppat_extension (extension cursor value))
  | Pat_open (path, dot, value) ->
      let path = followed cursor (module_path cursor) (path, dot) in
      begin match value with
      | { node = Pat_paren value; attributes = [] } ->
          (* [M.(p)] is not relocated to the parentheses. *)
          simple (Ppat_open (path, delimited cursor (pattern cursor) value))
      | { node =
            Pat_construct ((None, (Constr_unit _ | Constr_nil _)) as name, None)
        ;
          attributes = []
        }
      ->
          (* [M.()] and [M.[]]: the constructor is located at the whole pattern. *)
          let name = constructor name in
          fun loc ->
            Pat.mk
              ~loc (Ppat_open (path, Pat.mk ~loc (Ppat_construct (name, None))))
      | _ -> simple (Ppat_open (path, pattern cursor value))
      end
  | Pat_paren _ -> assert false

and newtype_names cursor ((type_, names) : Cst.newtypes) =
  preceded cursor (List.map (lident cursor)) (type_, names)

(* {2 Expressions} *)

and type_constraint cursor (value : Cst.type_constraint) =
  match value with
  | Constraint constraint_ ->
      Pconstraint (preceded cursor (core_type cursor) constraint_)
  | Coerce (ground, coerce, typ) ->
      let ground = Option.map (preceded cursor (core_type cursor)) ground in
      Pcoerce (ground, preceded cursor (core_type cursor) (coerce, typ))

and expr cursor ({ node; attributes = values } : Cst.expr) =
  let expr =
    match node with
    | Exp_paren value ->
        let expr, loc =
          located cursor (fun () -> delimited cursor (expr cursor) value)
        in
        reloc_exp ~loc expr
    | Exp_semi value -> followed cursor (expr cursor) value
    | _ -> build cursor (fun () -> expr_node cursor node)
  in
  with_attributes cursor Exp.attr expr values

(* A punned label [~x] or [?x]: the label and the variable. *)
and pun_expr
  : 'k.
      cursor -> ([< Token.any_kind ] as 'k) Cst.tok -> Cst.lident
      -> string * expression
  =
  fun cursor prefix name ->
    skip cursor prefix;
    let name = lident cursor name in
    name.txt, mkexpvar ~loc:name.loc name.txt

(* [~(x : t)]: the label and the constrained variable, located from the
   parenthesis. *)
and pun_constraint_expr cursor tilde value =
  skip cursor tilde;
  let (name, constraint_), loc =
    located
      cursor
      ( fun () ->
          delimited
            cursor
            ( fun (name, constraint_) ->
                let name = lident cursor name in
                name, type_constraint cursor constraint_
            )
            value
      )
  in
  name.txt, mkexp_constraint ~loc (mkexpvar ~loc:name.loc name.txt) constraint_

(* A labeled value: [~x:e], [?x:e]. *)
and labelled_expr
  : 'k.
      cursor
      ->
      ( [< `LABEL of Token.Lident.t | `OPTLABEL of Token.Lident.t ] as 'k
      ) Cst.tok
      -> Cst.expr
      -> string * expression
  =
  fun cursor label value ->
    skip cursor label; label_name label, expr cursor value

and tuple_element cursor (element : Cst.tuple_element) =
  let some (label, value) = Some label, value in
  match element with
  | Tuple_expr (~label:None, value) -> None, expr cursor value
  | Tuple_expr (~label:(Some label), value) ->
      some (labelled_expr cursor label value)
  | Tuple_pun (tilde, name) -> some (pun_expr cursor tilde name)
  | Tuple_pun_constraint (tilde, value) ->
      some (pun_constraint_expr cursor tilde value)

and argument cursor (value : Cst.argument) : Asttypes.arg_label * expression =
  let labelled (label, value) = Asttypes.Labelled label, value in
  let optional (label, value) = Asttypes.Optional label, value in
  match value with
  | Arg_expr value -> Nolabel, expr cursor value
  | Arg_label (label, value) -> labelled (labelled_expr cursor label value)
  | Arg_pun (tilde, name) -> labelled (pun_expr cursor tilde name)
  | Arg_pun_constraint (tilde, value) ->
      labelled (pun_constraint_expr cursor tilde value)
  | Arg_optional_pun (question, name) ->
      optional (pun_expr cursor question name)
  | Arg_optlabel (label, value) -> optional (labelled_expr cursor label value)

(* [record_expr_field]: a punned field is a variable. *)
and record_field cursor ((label, constraint_, value) : Cst.record_field) =
  let (label, constraint_, value), loc =
    located
      cursor
      ( fun () ->
          let label = longident cursor label (lident cursor) in
          let constraint_ =
            Option.map
              ( fun constraint_ ->
                  located cursor (fun () -> type_constraint cursor constraint_)
              )
              constraint_
          in
          label, constraint_, Option.map (preceded cursor (expr cursor)) value
      )
  in
  let constraint_loc, label, value =
    match value, constraint_ with
    | None, _ ->
        loc, { label with loc = ghost label.loc },
        Exp.mk
          ~loc:label.loc
          ( Pexp_ident
              ( Location.map
                  (fun id -> Longident.Lident (Longident.last id)) label
              )
          )
    | Some value, Some (_, constraint_loc) ->
        span constraint_loc loc, label, value
    | Some value, None -> loc, label, value
  in
  match constraint_ with
  | None -> label, value
  | Some (constraint_, _) ->
      label, mkexp_constraint ~loc:constraint_loc value constraint_

and case cursor ((lhs, guard, arrow, rhs) : Cst.case) =
  let lhs = pattern cursor lhs in
  let guard = Option.map (preceded cursor (expr cursor)) guard in
  skip cursor arrow;
  let rhs = expr cursor rhs in
  Exp.case lhs ?guard rhs

(* [FUNCTION ext_attributes match_cases]: the extension, the cases and their
   location. *)
and function_cases cursor (function_, ext, cases) =
  let (ext, cases), loc =
    located
      cursor
      ( fun () ->
          let ext = preceded cursor (ext_attributes cursor) (function_, ext) in
          ext, bar_list cursor (case cursor) cases
      )
  in
  ext, cases, loc

(* [function] as an expression. *)
and function_expr ~loc (ext, cases) =
  mkexp_attrs ~loc (mkfunction [] None (Pfunction_cases (cases, loc, []))) ext

(* [fun_body]: a [function] after parameters gives the cases of the function. *)
and function_body cursor (body : Cst.expr) =
  match body with
  | { node = Exp_function (function_, ext, cases); attributes = [] } ->
      let (ext, attrs), cases, loc =
        function_cases cursor (function_, ext, cases)
      in
      begin match ext with
      | None -> Pfunction_cases (cases, loc, attrs)
      | Some _ -> Pfunction_body (function_expr ~loc ((ext, attrs), cases))
      end
  | value -> Pfunction_body (expr cursor value)

(* [label_let_pattern]: a variable with an optional type. *)
and label_pattern cursor ((name, typ) : Cst.label_pattern) =
  let (name, typ), loc =
    located
      cursor
      ( fun () ->
          let name = lident cursor name in
          name, Option.map (preceded cursor (core_type cursor)) typ
      )
  in
  let variable = mkpatvar ~loc:name.loc name.txt in
  let pattern =
    match typ with
    | None -> variable
    | Some typ -> Pat.mk ~loc (Ppat_constraint (variable, typ))
  in
  name.txt, pattern

(* [let_pattern]: a pattern with an optional type. *)
and let_pattern cursor ((value, typ) : Cst.let_pattern) =
  build
    cursor
    ( fun () ->
        let value = pattern cursor value in
        match typ with
        | None -> fun _ -> value
        | Some typ ->
            let typ = preceded cursor (core_type cursor) typ in
            fun loc -> Pat.mk ~loc (Ppat_constraint (value, typ))
    )

and default_value cursor (default : Cst.default) =
  Option.map (preceded cursor (expr cursor)) default

(* [poly_pattern]: [(p : 'a. t)], located without the parentheses. *)
and poly_pattern cursor value =
  delimited
    cursor
    ( fun (value, colon, typ) ->
        build
          cursor
          ( fun () ->
              let value = pattern cursor value in
              let typ = preceded cursor (core_type cursor) (colon, typ) in
              fun loc -> Pat.mk ~loc (Ppat_constraint (value, typ))
          )
    )
    value

(* [simple_param_pattern]. *)
and param_pattern cursor (value : Cst.param_pattern)
  : Asttypes.arg_label * expression option * pattern
  =
    match value with
    | Param_optional_default (question, value) ->
        skip cursor question;
        delimited
          cursor
          ( fun (label, default) ->
              let name, pattern = label_pattern cursor label in
              Asttypes.Optional name, default_value cursor default, pattern
          )
          value
    | Param_optional (question, name) ->
        skip cursor question;
        let name = lident cursor name in
        Asttypes.Optional name.txt, None, mkpatvar ~loc:name.loc name.txt
    | Param_optlabel_default (optlabel, value) ->
        skip cursor optlabel;
        delimited
          cursor
          ( fun (pattern, default) ->
              let pattern = let_pattern cursor pattern in
              Asttypes.Optional (label_name optlabel),
              default_value cursor default, pattern
          )
          value
    | Param_optlabel (optlabel, value) ->
        skip cursor optlabel;
        let loc = consume cursor value in
        let pattern =
          match value.kind with
          | `LIDENT name -> mkpatvar ~loc (Token.Lident.to_string name)
          | `UNDERSCORE -> Pat.mk ~loc Ppat_any
        in
        Asttypes.Optional (label_name optlabel), None, pattern
    | Param_labelled_paren (tilde, value) ->
        skip cursor tilde;
        let name, pattern = delimited cursor (label_pattern cursor) value in
        Asttypes.Labelled name, None, pattern
    | Param_labelled (tilde, name) ->
        skip cursor tilde;
        let name = lident cursor name in
        Asttypes.Labelled name.txt, None, mkpatvar ~loc:name.loc name.txt
    | Param_label (label, value) ->
        skip cursor label;
        Asttypes.Labelled (label_name label), None, pattern cursor value
    | Param_label_poly (label, value) ->
        skip cursor label;
        Asttypes.Labelled (label_name label), None, poly_pattern cursor value
    | Param_pattern value -> Nolabel, None, pattern cursor value
    | Param_poly value -> Nolabel, None, poly_pattern cursor value

(* [fun_params]: [(type a b)] gives one parameter per type, located at the
   parentheses, and ghost if there are several. *)
and function_params cursor params =
  List.concat_map
    ( fun (param : Cst.function_param) ->
        match param with
        | Param_newtypes value ->
            let names, loc =
              located
                cursor (fun () -> delimited cursor (newtype_names cursor) value)
            in
            let loc =
              match names with
                [ _ ] -> loc
              | _ -> ghost loc
            in
            List.map
              ( fun name ->
                  { pparam_loc = loc; pparam_desc = Pparam_newtype name }
              )
              names
        | Param_value value ->
            let (label, default, pattern), loc =
              located cursor (fun () -> param_pattern cursor value)
            in
            [ { pparam_loc = loc;
                pparam_desc = Pparam_val (label, default, pattern)
              }
            ]
    )
    params

(* [strict_binding] after a name: [= e] or [params : t = body]. *)
and strict_binding
    cursor ((params, constraint_, equal, body) : Cst.function_binding)
  =
    match params, constraint_, body with
    | [], None, value -> preceded cursor (expr cursor) (equal, value)
    | _ ->
        build
          cursor
          ( fun () ->
              let params = function_params cursor params in
              let constraint_ = Option.map (type_constraint cursor) constraint_
              in
              let body = preceded cursor (function_body cursor) (equal, body) in
              fun loc ->
                Exp.mk ~loc:(ghost loc) (mkfunction params constraint_ body)
          )

(* [let_binding_body]: the pattern, the expression and the constraint. *)
and let_binding_body cursor (body : Cst.let_binding_body) =
  let name_pattern name =
    let name = val_ident cursor name in
    mkpatvar ~loc:name.loc name.txt
  in
  let constraint_ ?(univars = []) typ =
    Some (Pvc_constraint { locally_abstract_univars = univars; typ })
  in
  match body with
  | Lb_function (name, binding) ->
      let pattern = name_pattern name in
      pattern, strict_binding cursor binding, None
  | Lb_constrained (name, c, equal, value) ->
      let pattern = name_pattern name in
      let c =
        match type_constraint cursor c with
        | Pconstraint typ -> constraint_ typ
        | Pcoerce (ground, coercion) -> Some (Pvc_coercion { ground; coercion })
      in
      pattern, preceded cursor (expr cursor) (equal, value), c
  | Lb_poly (name, colon, variables, dot, typ, equal, value) ->
      let pattern = name_pattern name in
      skip cursor colon;
      let typ =
        build
          cursor
          ( fun () ->
              let variables = List.map (type_variable cursor) variables in
              let typ = preceded cursor (core_type cursor) (dot, typ) in
              fun loc -> Typ.mk ~loc:(ghost loc) (Ptyp_poly (variables, typ))
          )
      in
      pattern, preceded cursor (expr cursor) (equal, value), constraint_ typ
  | Lb_newtypes (name, (colon, newtypes, dot, typ, equal, value)) ->
      let pattern = name_pattern name in
      skip cursor colon;
      let names = newtype_names cursor newtypes in
      let typ = preceded cursor (core_type cursor) (dot, typ) in
      pattern, preceded cursor (expr cursor) (equal, value),
      constraint_ ~univars:names typ
  | Lb_pattern (value, equal, body) ->
      let value = pattern cursor value in
      value, preceded cursor (expr cursor) (equal, body), None
  | Lb_pattern_constraint (value, colon, typ, equal, body) ->
      let value = pattern cursor value in
      let typ = preceded cursor (core_type cursor) (colon, typ) in
      value, preceded cursor (expr cursor) (equal, body), constraint_ typ
  | Lb_pun name ->
      let name = val_ident cursor name in
      mkpatvar ~loc:name.loc name.txt, mkexpvar ~loc:name.loc name.txt, None

(* [let_bindings(ext)]: with [~docs], the bindings get their docstrings, as
   in structures. The first binding starts at [let], the others at [and]. *)
and let_bindings
    cursor ~docs
    ((let_, ext, rec_, ((first, rest) as bindings)) : Cst.let_bindings)
  =
    let group =
      group_docs
        cursor (if docs then take_docs cursor else Docstrings.empty_docs)
        bindings
    in
    let binding ~text docs attrs1 ((body, post_attributes) : Cst.let_binding) =
      let pattern, value, value_constraint = let_binding_body cursor body in
      let attrs = attrs1 @ attributes cursor post_attributes in
      fun (loc : Location.t) ->
        Vb.mk ~loc ~attrs ~docs ~text ?value_constraint pattern value
    in
    let (ext, flag, first), loc =
      located
        cursor
        ( fun () ->
            let ext, attrs1 =
              preceded cursor (ext_attributes cursor) (let_, ext)
            in
            let flag = rec_flag cursor rec_ in
            ext, flag, binding ~text:[] (List.hd group) attrs1 first
        )
    in
    let first = first loc in
    let rest =
      List.map2
        ( fun (~docstring, and_, values, value) docs ->
            let text = skip_and_docs cursor docstring in
            build
              cursor
              ( fun () ->
                  let attrs1 =
                    preceded cursor (attributes cursor) (and_, values)
                  in
                  binding ~text docs attrs1 value
              )
        )
        rest (List.tl group)
    in
    flag, ext, first :: rest

(* [letop_binding_body]. *)
and letop_binding cursor (binding : Cst.letop_binding) =
  match binding with
  | Lop_function (name, binding) ->
      let name = val_ident cursor name in
      mkpatvar ~loc:name.loc name.txt, strict_binding cursor binding
  | Lop_pun name ->
      let name = val_ident cursor name in
      mkpatvar ~loc:name.loc name.txt, mkexpvar ~loc:name.loc name.txt
  | Lop_constraint (value, colon, typ, equal, body) ->
      let pattern =
        build
          cursor
          ( fun () ->
              let value = pattern cursor value in
              let typ = preceded cursor (core_type cursor) (colon, typ) in
              fun loc -> Pat.mk ~loc:(ghost loc) (Ppat_constraint (value, typ))
          )
      in
      pattern, preceded cursor (expr cursor) (equal, body)
  | Lop_pattern (value, equal, body) ->
      let value = pattern cursor value in
      value, preceded cursor (expr cursor) (equal, body)

(* An infix operator applied to two expressions. *)
and infix cursor left (operator : Cst.operator_token) right =
  let left = expr cursor left in
  let operator =
    mkexpvar ~loc:(consume cursor operator) (operator_token_text operator)
  in
  let right = expr cursor right in
  Pexp_apply (operator, [ Nolabel, left; Nolabel, right ])

and expr_node cursor (node : Cst.expr_node) : Location.t -> expression =
  let simple desc loc = Exp.mk ~loc desc in
  let with_ext desc ext loc = mkexp_attrs ~loc desc ext in
  (* A keyword followed by its extension and attributes. *)
  let keyword token ext = preceded cursor (ext_attributes cursor) (token, ext)
  in
  match node with
  | Exp_ident path ->
      simple (Pexp_ident (longident cursor path (val_ident cursor)))
  | Exp_constant value -> simple (Pexp_constant (constant cursor value))
  | Exp_construct (constructor, argument) ->
      let constructor = longident cursor constructor (constr_ident cursor) in
      simple (Pexp_construct (constructor, Option.map (expr cursor) argument))
  | Exp_variant (tag, argument) ->
      let tag = name_tag cursor tag in
      simple (Pexp_variant (tag.txt, Option.map (expr cursor) argument))
  | Exp_constraint value ->
      let value, constraint_ =
        delimited
          cursor
          ( fun (value, constraint_) ->
              let value = expr cursor value in
              value, type_constraint cursor constraint_
          )
          value
      in
      fun loc -> mkexp_constraint ~loc value constraint_
  | Exp_begin value ->
      let (ext, attrs), body =
        delimited
          cursor
          ( fun (ext, body) ->
              let ext = ext_attributes cursor ext in
              ext, Option.map (expr cursor) body
          )
          value
      in
      fun loc ->
        begin match body with
        | Some body ->
            mkexp_attrs ~loc body.pexp_desc (ext, attrs @ body.pexp_attributes)
        | None ->
            mkexp_attrs
              ~loc (Pexp_construct (mkloc (Longident.Lident "()") loc, None))
              (ext, attrs)
        end
  | Exp_apply (function_, arguments) ->
      let function_ = expr cursor function_ in
      simple (Pexp_apply (function_, List.map (argument cursor) arguments))
  | Exp_infix (left, operator, right) ->
      simple (infix cursor left operator right)
  | Exp_prefix (operator, value) ->
      let operator_loc = consume cursor operator in
      let name =
        match operator.kind with
          `PREFIXOP op -> Token.Prefixop.to_string op
        | `BANG -> "!"
      in
      let value = expr cursor value in
      simple (Pexp_apply (mkexpvar ~loc:operator_loc name, [ Nolabel, value ]))
  | Exp_unary (operator, value) ->
      let oploc = consume cursor operator in
      let value = expr cursor value in
      let name = Token.text operator in
      fun sloc ->
        begin match operator.kind with
        | `MINUS | `MINUSDOT ->
            Exp.mk ~loc:sloc (mkuminus ~sloc ~oploc name value)
        | `PLUS | `PLUSDOT -> Exp.mk ~loc:sloc (mkuplus ~sloc ~oploc name value)
        end
  | Exp_tuple
      { first = (Tuple_pun_constraint _ as first);
        rest = (comma, second) :: rest
      }
  ->
      let first = tuple_element cursor first in
      let second = preceded cursor (tuple_element cursor) (comma, second) in
      (* [$endpos] of the base case: the end of the tokens of the second
         element, attributes included. *)
      let second_end = cursor.last_end in
      let rest = List.map (preceded cursor (tuple_element cursor)) rest in
      fun loc ->
        let label, value = first in
        let value_loc =
          { value.pexp_loc with loc_end = second_end; loc_ghost = false }
        in
        Exp.mk
          ~loc
          ( Pexp_tuple
              ((label, { value with pexp_loc = value_loc }) :: second :: rest)
          )
  | Exp_tuple elements ->
      simple
        ( Pexp_tuple
            (non_empty_separated cursor (tuple_element cursor) elements)
        )
  | Exp_cons (left, cons, right) ->
      let left = expr cursor left in
      let cons_loc = consume cursor cons in
      let right = expr cursor right in
      fun loc ->
        let argument =
          Exp.mk ~loc:(ghost loc) (Pexp_tuple [ None, left; None, right ])
        in
        Exp.mk
          ~loc
          ( Pexp_construct
              (mkloc (Longident.Lident "::") cons_loc, Some argument)
          )
  | Exp_field (record, dot, label) ->
      let record = expr cursor record in
      simple
        ( Pexp_field
            ( record,
              preceded
                cursor (fun label -> longident cursor label (lident cursor))
                (dot, label)
            )
        )
  | Exp_setfield (record, dot, label, arrow, value) ->
      let record = expr cursor record in
      let label =
        preceded
          cursor (fun label -> longident cursor label (lident cursor))
          (dot, label)
      in
      simple
        ( Pexp_setfield
            (record, label, preceded cursor (expr cursor) (arrow, value))
        )
  | Exp_index (array, dot, index, assign) ->
      let array = expr cursor array in
      let prefix, operator =
        match dot with
        | Index_builtin dot -> skip cursor dot; None, None
        | Index_user (qualifier, dotop) ->
            let prefix =
              Option.map
                ( fun qualifier ->
                    (preceded cursor (module_path cursor) qualifier).txt
                )
                qualifier
            in
            skip cursor dotop;
            let operator =
              match dotop.kind with `DOTOP op -> Token.Dotop.to_string op
            in
            prefix, Some operator
      in
      let paren, index = brackets cursor (semis cursor (expr cursor)) index in
      let assign = Option.map (preceded cursor (expr cursor)) assign in
      fun loc ->
        let index, name =
          match operator, index with
          | None, [ index ] ->
              let dimension, index = builtin_index ~loc paren index in
              index,
              builtin_index_name ~loc ~assign:(assign <> None) paren dimension
          | None, _ -> assert false
          | Some operator, index ->
              let dimension, index =
                match index with
                | [ index ] -> One, [ Asttypes.Nolabel, index ]
                | index ->
                    Many, [ Asttypes.Nolabel, Exp.mk ~loc (Pexp_array index) ]
              in
              index,
              user_index_name
                ~loc prefix operator ~assign:(assign <> None) paren dimension
        in
        let assign =
          match assign with
            None -> []
          | Some value -> [ Asttypes.Nolabel, value ]
        in
        Exp.mk
          ~loc
          ( Pexp_apply
              ( Exp.mk ~loc:(ghost loc) (Pexp_ident name),
                ((Asttypes.Nolabel, array) :: index) @ assign
              )
          )
  | Exp_setinstvar (name, arrow, value) ->
      let name = lident cursor name in
      simple
        (Pexp_setinstvar (name, preceded cursor (expr cursor) (arrow, value)))
  | Exp_send (value, hash, name) ->
      let value = expr cursor value in
      simple (Pexp_send (value, preceded cursor (lident cursor) (hash, name)))
  | Exp_let (bindings, in_, body) ->
      let flag, ext, bindings = let_bindings cursor ~docs:false bindings in
      let body = preceded cursor (expr cursor) (in_, body) in
      with_ext (Pexp_let (flag, bindings, body)) (ext, [])
  | Exp_letop (let_op, { first = binding; rest = ands }, in_, body) ->
      let op_loc = consume cursor let_op in
      let op = match let_op.kind with `LETOP op -> Token.Letop.to_string op in
      let (pattern, value), binding_loc =
        located cursor (fun () -> letop_binding cursor binding)
      in
      let ands =
        List.map
          ( fun ((and_op : [ `ANDOP of Token.Andop.t ] Cst.tok), binding) ->
              let and_loc = consume cursor and_op in
              let name =
                match and_op.kind with `ANDOP op -> Token.Andop.to_string op
              in
              let pattern, value = letop_binding cursor binding in
              (* The location of an [and*] binding starts with the first
                 binding. *)
              { pbop_op = mkloc name and_loc; pbop_pat = pattern;
                pbop_exp = value; pbop_loc = span binding_loc (here cursor)
              }
          )
          ands
      in
      let body = preceded cursor (expr cursor) (in_, body) in
      fun loc ->
        let let_ =
          { pbop_op = mkloc op op_loc; pbop_pat = pattern;
            pbop_exp = value; pbop_loc = loc
          }
        in
        Exp.mk ~loc (Pexp_letop { let_; ands; body })
  | Exp_struct_item (let_, ext, item, in_, body) ->
      let ext = keyword let_ ext in
      let item = documented_item cursor item in
      let body = preceded cursor (expr cursor) (in_, body) in
      with_ext (Pexp_struct_item (item, body)) ext
  | Exp_fun (fun_, ext, params, constraint_, arrow, body) ->
      let ext = keyword fun_ ext in
      let params = function_params cursor params in
      let constraint_ =
        Option.map
          ( fun constraint_ ->
              Pconstraint (preceded cursor (core_type cursor) constraint_)
          )
          constraint_
      in
      let body = preceded cursor (function_body cursor) (arrow, body) in
      with_ext (mkfunction params constraint_ body) ext
  | Exp_function (function_, ext, cases) ->
      let ext, cases, _ = function_cases cursor (function_, ext, cases) in
      fun loc -> function_expr ~loc (ext, cases)
  | Exp_match (match_, ext, scrutinee, with_, cases) ->
      let ext = keyword match_ ext in
      let scrutinee = expr cursor scrutinee in
      let cases = preceded cursor (bar_list cursor (case cursor)) (with_, cases)
      in
      with_ext (Pexp_match (scrutinee, cases)) ext
  | Exp_try (try_, ext, body, with_, cases) ->
      let ext = keyword try_ ext in
      let body = expr cursor body in
      let cases = preceded cursor (bar_list cursor (case cursor)) (with_, cases)
      in
      with_ext (Pexp_try (body, cases)) ext
  | Exp_if (if_, ext, condition, then_, if_true, if_false) ->
      let ext = keyword if_ ext in
      let condition = expr cursor condition in
      let if_true = preceded cursor (expr cursor) (then_, if_true) in
      let if_false = Option.map (preceded cursor (expr cursor)) if_false in
      with_ext (Pexp_ifthenelse (condition, if_true, if_false)) ext
  | Exp_while (while_, ext, condition, body) ->
      let ext = keyword while_ ext in
      let condition = expr cursor condition in
      let body = delimited cursor (expr cursor) body in
      with_ext (Pexp_while (condition, body)) ext
  | Exp_for (for_, ext, index, equal, start, direction, stop, body) ->
      let ext = keyword for_ ext in
      let index = pattern cursor index in
      let start = preceded cursor (expr cursor) (equal, start) in
      skip cursor direction;
      let direction : Asttypes.direction_flag =
        match direction.kind with
          `TO -> Upto
        | `DOWNTO -> Downto
      in
      let stop = expr cursor stop in
      let body = delimited cursor (expr cursor) body in
      with_ext (Pexp_for (index, start, stop, direction, body)) ext
  | Exp_assert (assert_, ext, value) ->
      let ext = keyword assert_ ext in
      with_ext (Pexp_assert (expr cursor value)) ext
  | Exp_lazy (lazy_, ext, value) ->
      let ext = keyword lazy_ ext in
      with_ext (Pexp_lazy (expr cursor value)) ext
  | Exp_sequence (left, semi, right) ->
      let left = expr cursor left in
      simple (Pexp_sequence (left, preceded cursor (expr cursor) (semi, right)))
  | Exp_sequence_ext (left, semi, percent, id, right) ->
      let left = followed cursor (expr cursor) (left, semi) in
      let id = preceded cursor (attr_id cursor) (percent, id) in
      let right = expr cursor right in
      with_ext (Pexp_sequence (left, right)) (Some id, [])
  | Exp_new (new_, ext, path) ->
      let ext = keyword new_ ext in
      with_ext (Pexp_new (longident cursor path (lident cursor))) ext
  | Exp_pack value ->
      let ext, desc =
        delimited
          cursor
          ( fun
              ( (module_, ext, value, package)
              : Cst.module_expr Cst.packed_module
              )
            ->
              let ext = keyword module_ ext in
              let value = module_expr cursor value in
              ext,
              Pexp_pack
                ( value,
                  Option.map (preceded cursor (package_type cursor)) package
                )
          )
          value
      in
      with_ext desc ext
  | Exp_object value ->
      let ext, body =
        delimited
          cursor
          ( fun (ext, body) ->
              let ext = ext_attributes cursor ext in
              ext, class_structure cursor body
          )
          value
      in
      with_ext (Pexp_object body) ext
  | Exp_override value ->
      let field ((name, value) : Cst.object_expr_field) =
        let name = lident cursor name in
        match value with
        | None ->
            (* A punned field is a variable. *)
            { name with loc = ghost name.loc },
            Exp.mk
              ~loc:name.loc
              ( Pexp_ident
                  (Location.map (fun name -> Longident.Lident name) name)
              )
        | Some value -> name, preceded cursor (expr cursor) value
      in
      simple
        (Pexp_override (delimited cursor (semis_option cursor field) value))
  | Exp_local_open (path, dot, inner) ->
      let path = followed cursor (module_path cursor) (path, dot) in
      let item = Str.open_ (Opn.mk ~loc:path.loc (Mod.ident ~loc:path.loc path))
      in
      begin match inner with
      | { node = Exp_paren value; attributes = [] } ->
          (* [M.(e)] is not relocated to the parentheses. *)
          simple (Pexp_struct_item (item, delimited cursor (expr cursor) value))
      | { node = Exp_override _; attributes = [] } ->
          (* [M.{< ... >}]: the object is located at the whole expression. *)
          let inner = expr cursor inner in
          fun loc ->
            Exp.mk ~loc (Pexp_struct_item (item, { inner with pexp_loc = loc }))
      | _ -> simple (Pexp_struct_item (item, expr cursor inner))
      end
  | Exp_record value ->
      let base, fields =
        delimited
          cursor
          ( fun ((base, fields) : Cst.record_expr) ->
              let base = Option.map (followed cursor (expr cursor)) base in
              base, semis cursor (record_field cursor) fields
          )
          value
      in
      simple (Pexp_record (fields, base))
  | Exp_array value ->
      simple
        ( Pexp_array
            (delimited cursor (semis_option cursor (expr cursor)) value)
        )
  | Exp_list { left; content; right } ->
      skip cursor left;
      let elements = semis cursor (expr cursor) content in
      let nil_loc = consume cursor right in
      simple (fst (mktailexp nil_loc elements))
  | Exp_extension value -> simple (Pexp_extension (extension cursor value))
  | Exp_unreachable dot -> skip cursor dot; simple Pexp_unreachable
  | Exp_paren _ | Exp_semi _ -> assert false

(* {2 Classes} *)

and class_expr cursor ({ node; attributes = values } : Cst.class_expr) =
  let expr =
    match node with
    | Cl_paren value -> delimited cursor (class_expr cursor) value
    | Cl_fun (fun_, values, params, arrow, body) ->
        let attrs = preceded cursor (attributes cursor) (fun_, values) in
        let params =
          List.map
            ( fun param ->
                let param, loc =
                  located cursor (fun () -> param_pattern cursor param)
                in
                param, loc.loc_start
            )
            params
        in
        let body = preceded cursor (class_expr cursor) (arrow, body) in
        (* [class_fun_def]: each parameter starts a function up to the end. *)
        let stop = cursor.last_end in
        let expr =
          List.fold_right
            ( fun ((label, default, pattern), start) body ->
                Cl.mk
                  ~loc:(make_loc (start, stop))
                  (Pcl_fun (label, default, pattern, body))
            )
            params body
        in
        { expr with pcl_attributes = attrs @ expr.pcl_attributes }
    | _ -> build cursor (fun () -> class_expr_node cursor node)
  in
  with_attributes cursor Cl.attr expr values

and class_expr_node cursor (node : Cst.class_expr_node)
  : Location.t -> class_expr
  =
    let simple desc loc = Cl.mk ~loc desc in
    match node with
    | Cl_constr (arguments, path) ->
        let arguments = class_parameters cursor (core_type cursor) arguments in
        simple (Pcl_constr (longident cursor path (lident cursor), arguments))
    | Cl_structure value ->
        let attrs, body =
          delimited
            cursor
            ( fun (values, body) ->
                let attrs = attributes cursor values in
                attrs, class_structure cursor body
            )
            value
        in
        fun loc -> Cl.mk ~loc ~attrs (Pcl_structure body)
    | Cl_apply (expr, arguments) ->
        let expr = class_expr cursor expr in
        simple (Pcl_apply (expr, List.map (argument cursor) arguments))
    | Cl_let (bindings, in_, body) ->
        let flag, _, bindings = let_bindings cursor ~docs:false bindings in
        simple
          ( Pcl_let
              (flag, bindings, preceded cursor (class_expr cursor) (in_, body))
          )
    | Cl_open (let_, open_, bang, values, path, in_, body) ->
        let attrs, description =
          class_open cursor let_ open_ bang values path in_
        in
        let body = class_expr cursor body in
        fun loc -> Cl.mk ~loc ~attrs (Pcl_open (description, body))
    | Cl_constraint value ->
        delimited
          cursor
          ( fun (expr, colon, typ) ->
              let expr = class_expr cursor expr in
              simple
                ( Pcl_constraint
                    (expr, preceded cursor (class_type cursor) (colon, typ))
                )
          )
          value
    | Cl_extension value -> simple (Pcl_extension (extension cursor value))
    | Cl_paren _ | Cl_fun _ -> assert false

and class_structure cursor ((self, fields) : Cst.class_structure) =
  let self =
    match self with
    | None -> Pat.mk ~loc:(ghost (here cursor)) Ppat_any
    | Some (Self_pattern value) ->
        let pattern, loc =
          located cursor (fun () -> delimited cursor (pattern cursor) value)
        in
        reloc_pat ~loc pattern
    | Some (Self_constraint value) ->
        build
          cursor
          ( fun () ->
              delimited
                cursor
                ( fun (value, colon, typ) ->
                    let value = pattern cursor value in
                    let typ = preceded cursor (core_type cursor) (colon, typ) in
                    fun loc -> Pat.mk ~loc (Ppat_constraint (value, typ))
                )
                value
          )
  in
  let field field = Some (build cursor (fun () -> class_field cursor field)) in
  let claims =
    function
      Cst.Cf_attribute _ -> false
    | _ -> true
  in
  Cstr.mk self (elements cursor ~claims Cf.text field fields)

(* The start of a [value] or [method_] production, after its keyword: the
   override flag, the attributes, and a function giving the start of the
   production, which is its first token ([$symbolstartpos]). *)
and field_start cursor bang values =
  let count = cursor.count in
  let override = override_flag cursor bang in
  let attrs = attributes cursor values in
  override, attrs, fun () -> cursor.starts.(count)

and class_field cursor (field : Cst.class_field) : Location.t -> class_field =
  let docs = take_docs cursor in
  let with_docs desc attrs loc = Cf.mk ~loc ~attrs ~docs desc in
  match field with
  | Cf_inherit (inherit_, bang, values, expr, alias, ~post_attributes) ->
      skip cursor inherit_;
      let override = override_flag cursor bang in
      let attrs1 = attributes cursor values in
      let expr = class_expr cursor expr in
      let alias = Option.map (preceded cursor (lident cursor)) alias in
      with_docs
        (Pcf_inherit (override, expr, alias))
        (attrs1 @ attributes cursor post_attributes)
  | Cf_val (val_, bang, values, tokens, name, body, ~post_attributes) ->
      skip cursor val_;
      let override, attrs1, start = field_start cursor bang values in
      let has = flags cursor tokens in
      let mut : Asttypes.mutable_flag =
        if has `MUTABLE then Mutable else Immutable
      in
      let name = lident cursor name in
      let kind =
        match body with
        | Value_virtual typ ->
            Cfk_virtual (preceded cursor (core_type cursor) typ)
        | Value_concrete (None, equal, value) ->
            Cfk_concrete
              (override, preceded cursor (expr cursor) (equal, value))
        | Value_concrete (Some constraint_, equal, value) ->
            let constraint_ = type_constraint cursor constraint_ in
            let value = preceded cursor (expr cursor) (equal, value) in
            let loc = make_loc (start (), cursor.last_end) in
            Cfk_concrete (override, mkexp_constraint ~loc value constraint_)
      in
      with_docs
        (Pcf_val (name, mut, kind)) (attrs1 @ attributes cursor post_attributes)
  | Cf_method (method_, bang, values, tokens, name, body, ~post_attributes) ->
      skip cursor method_;
      let override, attrs1, start = field_start cursor bang values in
      let has = flags cursor tokens in
      let priv : Asttypes.private_flag =
        if has `PRIVATE then Private else Public
      in
      let name = lident cursor name in
      let kind =
        match body with
        | Method_virtual typ ->
            Cfk_virtual (preceded cursor (core_type cursor) typ)
        | Method_strict binding ->
            let value = strict_binding cursor binding in
            let loc = ghost value.pexp_loc in
            Cfk_concrete (override, Exp.mk ~loc (Pexp_poly (value, None)))
        | Method_poly (colon, typ, equal, value) ->
            skip cursor colon;
            let (typ, value), loc =
              located
                cursor
                ( fun () ->
                    let typ = core_type cursor typ in
                    typ, preceded cursor (expr cursor) (equal, value)
                )
            in
            Cfk_concrete
              (override, Exp.mk ~loc:(ghost loc) (Pexp_poly (value, Some typ)))
        | Method_newtypes (colon, (type_, names), dot, typ, equal, value) ->
            skip cursor colon;
            skip cursor type_;
            let (names, typ, value), poly_loc =
              located
                cursor
                ( fun () ->
                    let names = List.map (lident cursor) names in
                    let typ = preceded cursor (core_type cursor) (dot, typ) in
                    names, typ, preceded cursor (expr cursor) (equal, value)
                )
            in
            (* [wrap_type_annotation], at the location of the production. *)
            let loc = make_loc (start (), cursor.last_end) in
            let value = Exp.mk ~loc (Pexp_constraint (value, typ)) in
            let value =
              List.fold_right
                (fun name value -> Exp.mk ~loc (Pexp_newtype (name, value)))
                names value
            in
            let poly =
              Typ.mk
                ~loc:(ghost loc)
                (Ptyp_poly (names, Typ.varify_constructors names typ))
            in
            Cfk_concrete
              ( override,
                Exp.mk ~loc:(ghost poly_loc) (Pexp_poly (value, Some poly))
              )
      in
      with_docs
        (Pcf_method (name, priv, kind))
        (attrs1 @ attributes cursor post_attributes)
  | Cf_constraint (constraint_, values, left, equal, right, ~post_attributes) ->
      let attrs1 = preceded cursor (attributes cursor) (constraint_, values) in
      let left = core_type cursor left in
      let right = preceded cursor (core_type cursor) (equal, right) in
      with_docs
        (Pcf_constraint (left, right))
        (attrs1 @ attributes cursor post_attributes)
  | Cf_initializer (initializer_, values, value, ~post_attributes) ->
      let attrs1 = preceded cursor (attributes cursor) (initializer_, values) in
      let value = expr cursor value in
      with_docs
        (Pcf_initializer value) (attrs1 @ attributes cursor post_attributes)
  | Cf_extension (value, post_attributes) ->
      let value = extension cursor value in
      with_docs (Pcf_extension value) (attributes cursor post_attributes)
  | Cf_attribute value ->
      let value = attribute cursor value in
      fun loc -> Cf.mk ~loc (Pcf_attribute value)

(* [class_fun_binding]. *)
and class_binding cursor (binding : Cst.class_binding) =
  match binding with
  | Class_equal value -> preceded cursor (class_expr cursor) value
  | Class_constraint (colon, typ, equal, expr) ->
      build
        cursor
        ( fun () ->
            let typ = preceded cursor (class_type cursor) (colon, typ) in
            let expr = preceded cursor (class_expr cursor) (equal, expr) in
            fun loc -> Cl.mk ~loc (Pcl_constraint (expr, typ))
        )
  | Class_param (param, binding) ->
      build
        cursor
        ( fun () ->
            let label, default, pattern = param_pattern cursor param in
            let body = class_binding cursor binding in
            fun loc -> Cl.mk ~loc (Pcl_fun (label, default, pattern, body))
        )

(* {2 Module declarations and bindings} *)

(* A module declaration or binding, after its head ([module] or [and]). *)
and module_item
  : 'a 'b.
      cursor -> (unit -> item_head) -> Cst.module_name
      -> (unit -> 'a) -> Cst.attribute list
      ->
      ( string option Location.loc -> 'a -> attrs:attributes
        -> loc:Location.t -> docs:Docstrings.docs -> text:Docstrings.text
        -> 'b
      )
      -> 'b * string Location.loc option
  =
  fun cursor head name body post_attributes make ->
    let value, ext, _ =
      item_with_ext
        cursor
        ( fun () ->
            let ext, attrs1, text, docs = head () in
            let name = module_name cursor name in
            let body = body () in
            let attrs = attrs1 @ attributes cursor post_attributes in
            ext, fun loc -> make name body ~attrs ~loc ~docs ~text
        )
    in
    value, ext

and module_declaration
    cursor head ((name, body, post_attributes) : Cst.module_declaration)
  =
    module_item
      cursor head name
      (fun () -> module_declaration_body cursor body) post_attributes
      ( fun name typ ~attrs ~loc ~docs ~text ->
          Md.mk name typ ~attrs ~loc ~docs ~text
      )

and module_declaration_body cursor (body : Cst.module_declaration_body) =
  match body with
  | Mdecl_type typ -> preceded cursor (module_type cursor) typ
  | Mdecl_functor (argument, body) ->
      build
        cursor
        ( fun () ->
            let _, argument = functor_arg cursor argument in
            let body = module_declaration_body cursor body in
            fun loc -> Mty.mk ~loc (Pmty_functor (argument, body))
        )
  | Mdecl_alias alias ->
      let path = preceded cursor (module_path cursor) alias in
      Mty.alias ~loc:path.loc path

and module_binding
    cursor head ((name, body, post_attributes) : Cst.module_binding)
  =
    module_item
      cursor head name
      (fun () -> module_binding_body cursor body) post_attributes
      ( fun name expr ~attrs ~loc ~docs ~text ->
          Mb.mk name expr ~attrs ~loc ~docs ~text
      )

and module_binding_body cursor (body : Cst.module_binding_body) =
  match body with
  | Mbind_equal value -> preceded cursor (module_expr cursor) value
  | Mbind_constraint (colon, typ, equal, expr) ->
      build
        cursor
        ( fun () ->
            let typ = preceded cursor (module_type cursor) (colon, typ) in
            let expr = preceded cursor (module_expr cursor) (equal, expr) in
            fun loc -> Mod.mk ~loc (Pmod_constraint (expr, typ))
        )
  | Mbind_functor (argument, body) ->
      build
        cursor
        ( fun () ->
            let _, argument = functor_arg cursor argument in
            let body = module_binding_body cursor body in
            fun loc -> Mod.mk ~loc (Pmod_functor (argument, body))
        )

and module_type_declaration
    cursor
    ( (module_, type_, ext, name, body, post_attributes)
    : Cst.module_type_declaration
    )
  =
    let docs = take_docs cursor in
    item_with_ext
      cursor
      ( fun () ->
          skip cursor module_;
          let ext, attrs1 = preceded cursor (ext_attributes cursor) (type_, ext)
          in
          let name = ident cursor name in
          let typ = Option.map (preceded cursor (module_type cursor)) body in
          let attrs = attrs1 @ attributes cursor post_attributes in
          ext, fun loc -> Mtd.mk name ?typ ~attrs ~loc ~docs
      )

and open_infos
  : 'a 'b.
      cursor -> (cursor -> 'a -> 'b) -> 'a Cst.open_infos
      -> 'b open_infos * string Location.loc option * Location.t
  =
  fun cursor expr (open_, bang, ext, value, post_attributes) ->
    let docs = take_docs cursor in
    item_with_ext
      cursor
      ( fun () ->
          skip cursor open_;
          let override = override_flag cursor bang in
          let ext, attrs1 = ext_attributes cursor ext in
          let value = expr cursor value in
          let attrs = attrs1 @ attributes cursor post_attributes in
          ext, fun loc -> Opn.mk value ~override ~attrs ~loc ~docs
      )

and include_infos
  : 'a 'b.
      cursor -> (cursor -> 'a -> 'b) -> 'a Cst.include_infos
      -> 'b include_infos * string Location.loc option * Location.t
  =
  fun cursor expr (include_, ext, value, post_attributes) ->
    let docs = take_docs cursor in
    item_with_ext
      cursor
      ( fun () ->
          let ext, attrs1 =
            preceded cursor (ext_attributes cursor) (include_, ext)
          in
          let value = expr cursor value in
          let attrs = attrs1 @ attributes cursor post_attributes in
          ext, fun loc -> Incl.mk value ~attrs ~loc ~docs
      )

(* {2 Structures and signatures} *)

(* [wrap_mkstr_ext] and [wrap_mksig_ext]: an item with an extension is
   wrapped in an extension item. *)
and wrap_str ~loc desc (ext : string Location.loc option) =
  match ext with
  | None -> Str.mk ~loc desc
  | Some id ->
      Str.mk
        ~loc (Pstr_extension ((id, PStr [ Str.mk ~loc:(ghost loc) desc ]), []))

and wrap_sig ~loc desc (ext : string Location.loc option) =
  match ext with
  | None -> Sig.mk ~loc desc
  | Some id ->
      Sig.mk
        ~loc (Psig_extension ((id, PSig [ Sig.mk ~loc:(ghost loc) desc ]), []))

(* An item extension [[%%id]] with its post-item attributes. *)
and item_extension cursor value post_attributes =
  let docs = take_docs cursor in
  let (value, attrs), loc =
    located
      cursor
      ( fun () ->
          let value = extension cursor value in
          value, attributes cursor post_attributes
      )
  in
  value, Docstrings.add_docs_attrs docs attrs, loc

(* A structure item, or [None] for [;;]. *)
and structure_item cursor (item : Cst.structure_item) =
  (* An item converted by [f], which returns the item, its extension and its
     location. *)
  let wrap f make =
    let value, ext, loc = f () in
    Some (wrap_str ~loc (make value) ext)
  in
  (* An item whose location is the span of [f]. *)
  let spanned f make =
    let (ext, value), loc = located cursor f in
    Some (wrap_str ~loc (make value) ext)
  in
  match item with
  | Str_eval (value, post_attributes) ->
      let value = expr cursor value in
      Some (mkstrexp value (attributes cursor post_attributes))
  | Str_value bindings ->
      let (flag, ext, bindings), loc =
        located cursor (fun () -> let_bindings cursor ~docs:true bindings)
      in
      (* [val_of_let_bindings]: the extension wrapper is ghost. *)
      let item = Str.mk ~loc (Pstr_value (flag, bindings)) in
      Some
        begin match ext with
        | None -> item
        | Some id ->
            Str.mk ~loc:(ghost loc) (Pstr_extension ((id, PStr [ item ]), []))
        end
  | Str_primitive value ->
      wrap
        (fun () -> value_description cursor value)
        (fun value -> Pstr_primitive value)
  | Str_type declarations ->
      spanned
        ( fun () ->
            let (flag, ext), declarations =
              type_declarations cursor declarations
            in
            ext, (flag, declarations)
        )
        (fun (flag, declarations) -> Pstr_type (flag, declarations))
  | Str_typext extension ->
      wrap
        (fun () -> type_extension cursor extension)
        (fun value -> Pstr_typext value)
  | Str_exception declaration ->
      wrap
        (fun () -> exception_declaration cursor declaration)
        (fun value -> Pstr_exception value)
  | Str_module (module_, ext, binding) ->
      spanned
        ( fun () ->
            let binding, ext =
              module_binding cursor (keyword_head cursor module_ ext) binding
            in
            ext, binding
        )
        (fun value -> Pstr_module value)
  | Str_recmodule (module_, ext, rec_, bindings) ->
      spanned
        ( fun () ->
            item_group
              cursor module_binding
              (keyword_head cursor module_ ext ~rec_) bindings
        )
        (fun bindings -> Pstr_recmodule bindings)
  | Str_modtype declaration ->
      wrap
        (fun () -> module_type_declaration cursor declaration)
        (fun value -> Pstr_modtype value)
  | Str_open declaration ->
      wrap
        (fun () -> open_infos cursor module_expr declaration)
        (fun value -> Pstr_open value)
  | Str_class (class_, ext, declarations) ->
      spanned
        ( fun () ->
            class_infos
              cursor class_binding (keyword_head cursor class_ ext) declarations
        )
        (fun declarations -> Pstr_class declarations)
  | Str_class_type (class_, type_, ext, declarations) ->
      spanned
        ( fun () ->
            class_infos
              cursor class_type_declaration_body
              (keyword_head cursor class_ ~type_ ext) declarations
        )
        (fun declarations -> Pstr_class_type declarations)
  | Str_include declaration ->
      wrap
        (fun () -> include_infos cursor module_expr declaration)
        (fun value -> Pstr_include value)
  | Str_attribute value ->
      let value, loc = located cursor (fun () -> attribute cursor value) in
      Some (Str.mk ~loc (Pstr_attribute value))
  | Str_extension (value, post_attributes) ->
      let value, attrs, loc = item_extension cursor value post_attributes in
      Some (Str.mk ~loc (Pstr_extension (value, attrs)))
  | Str_semisemi token -> skip cursor token; None

(* A structure item alone, with its doc comments ([let module M = N in e]). *)
and documented_item
    cursor ((pre, value, post) as item : Cst.structure_item Cst.documented)
  =
    cursor.docs <- item_docs cursor ~before:[] ~after:[] item;
    skip_docs cursor pre;
    let value = Option.get (structure_item cursor value) in
    cursor.docs <- Docstrings.empty_docs; skip_docs cursor post; value

(* A signature item, or [None] for [;;]. *)
and signature_item cursor (item : Cst.signature_item) =
  let wrap f make =
    let value, ext, loc = f () in
    Some (wrap_sig ~loc (make value) ext)
  in
  let spanned f make =
    let (ext, value), loc = located cursor f in
    Some (wrap_sig ~loc (make value) ext)
  in
  match item with
  | Sig_value value ->
      wrap
        (fun () -> value_description cursor value)
        (fun value -> Psig_value value)
  | Sig_type declarations ->
      spanned
        ( fun () ->
            let (flag, ext), declarations =
              type_declarations cursor declarations
            in
            ext, (flag, declarations)
        )
        (fun (flag, declarations) -> Psig_type (flag, declarations))
  | Sig_typesubst declarations ->
      spanned
        ( fun () ->
            let (_, ext), declarations = type_declarations cursor declarations
            in
            ext, declarations
        )
        (fun declarations -> Psig_typesubst declarations)
  | Sig_typext extension ->
      wrap
        (fun () -> type_extension cursor extension)
        (fun value -> Psig_typext value)
  | Sig_exception declaration ->
      wrap
        (fun () -> exception_declaration cursor declaration)
        (fun value -> Psig_exception value)
  | Sig_module (module_, ext, declaration) ->
      spanned
        ( fun () ->
            let declaration, ext =
              module_declaration
                cursor (keyword_head cursor module_ ext) declaration
            in
            ext, declaration
        )
        (fun value -> Psig_module value)
  | Sig_modsubst (module_, ext, name, colonequal, path, post_attributes) ->
      wrap
        ( fun () ->
            let docs = take_docs cursor in
            item_with_ext
              cursor
              ( fun () ->
                  let ext, attrs1 =
                    preceded cursor (ext_attributes cursor) (module_, ext)
                  in
                  let name = uident cursor name in
                  let path =
                    preceded cursor (module_path cursor) (colonequal, path)
                  in
                  let attrs = attrs1 @ attributes cursor post_attributes in
                  ext, fun loc -> Ms.mk name path ~attrs ~loc ~docs
              )
        )
        (fun value -> Psig_modsubst value)
  | Sig_recmodule (module_, ext, rec_, declarations) ->
      spanned
        ( fun () ->
            item_group
              cursor module_declaration
              (keyword_head cursor module_ ext ~rec_) declarations
        )
        (fun declarations -> Psig_recmodule declarations)
  | Sig_modtype declaration ->
      wrap
        (fun () -> module_type_declaration cursor declaration)
        (fun value -> Psig_modtype value)
  | Sig_modtypesubst declaration ->
      wrap
        (fun () -> module_type_declaration cursor declaration)
        (fun value -> Psig_modtypesubst value)
  | Sig_open description ->
      wrap
        (fun () -> open_infos cursor module_path description)
        (fun value -> Psig_open value)
  | Sig_include description ->
      wrap
        (fun () -> include_infos cursor module_type description)
        (fun value -> Psig_include value)
  | Sig_class (class_, ext, descriptions) ->
      spanned
        ( fun () ->
            class_infos
              cursor class_description_body
              (keyword_head cursor class_ ext) descriptions
        )
        (fun descriptions -> Psig_class descriptions)
  | Sig_class_type (class_, type_, ext, declarations) ->
      spanned
        ( fun () ->
            class_infos
              cursor class_type_declaration_body
              (keyword_head cursor class_ ~type_ ext) declarations
        )
        (fun declarations -> Psig_class_type declarations)
  | Sig_attribute value ->
      let value, loc = located cursor (fun () -> attribute cursor value) in
      Some (Sig.mk ~loc (Psig_attribute value))
  | Sig_extension (value, post_attributes) ->
      let value, attrs, loc = item_extension cursor value post_attributes in
      Some (Sig.mk ~loc (Psig_extension (value, attrs)))
  | Sig_semisemi token -> skip cursor token; None

and signature cursor items =
  let claims =
    function
      Cst.Sig_attribute _ | Sig_semisemi _ -> false
    | _ -> true
  in
  elements cursor ~claims Sig.text (signature_item cursor) items

and structure cursor items =
  let claims =
    function
      Cst.Str_eval _ | Str_attribute _ | Str_semisemi _ -> false
    | _ -> true
  in
  elements cursor ~claims Str.text (structure_item cursor) items

let cursor filename tokens =
  let position = Token_position.initial ~filename in
  let locations, warnings = scan_docstrings ~filename tokens in
  { position; last_end = position; count = 0; starts = [||];
    locations; docs = Docstrings.empty_docs; warnings
  }

(* The warnings about doc comments, in source order, as the compiler at the
   end of parsing ([Docstrings.warn_bad_docstrings]). *)
let warn cursor =
    List.sort
      ( fun ((left : Location.t), _) (right, _) ->
          compare left.loc_start.pos_cnum right.loc_start.pos_cnum
      )
      cursor.warnings
  |>
  List.iter
    ( fun (loc, unattached) ->
        Location.prerr_warning loc (Warnings.Unexpected_docstring unattached)
    )

let implementation ~filename (cst : Cst.implementation) =
  let cursor = cursor filename (Printer.implementation_tokens cst) in
  let items = structure cursor (fst cst) in
  warn cursor; items

let interface ~filename (cst : Cst.interface) =
  let cursor = cursor filename (Printer.interface_tokens cst) in
  let items = signature cursor (fst cst) in
  warn cursor; items
