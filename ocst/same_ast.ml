(* Compares the compiler's ASTs of two sources, except for the locations: see
   same_ast.mli. *)

(* {1 Equality modulo locations}

   The ASTs are compared as values: a location is recognized by its shape, so
   that the locations of every part of the AST are ignored, including the
   location stacks, which [Ast_mapper] does not map. *)

(** Is the value a [Lexing.position]: a file name and three integers? *)
let is_position (value : Obj.t) =
  Obj.is_block value && Obj.tag value = 0
  && Obj.size value = 4 && Obj.is_block (Obj.field value 0)
  && Obj.tag (Obj.field value 0) = Obj.string_tag
  && Obj.is_int (Obj.field value 1) && Obj.is_int (Obj.field value 2)
  && Obj.is_int (Obj.field value 3)

(** Is the value a [Location.t]: two positions and a ghost flag? *)
let is_location (value : Obj.t) =
  Obj.is_block value && Obj.tag value = 0
  && Obj.size value = 3 && is_position (Obj.field value 0)
  && is_position (Obj.field value 1) && Obj.is_int (Obj.field value 2)

let rec equal (left : Obj.t) (right : Obj.t) =
  if Obj.is_int left || Obj.is_int right then left == right
  else if is_location left && is_location right then true
  else if Obj.tag left <> Obj.tag right || Obj.size left <> Obj.size right then
    false
  else if Obj.tag left = Obj.string_tag then
    (Obj.obj left : string) = Obj.obj right
  else if Obj.tag left = Obj.double_tag then
    (Obj.obj left : float) = Obj.obj right
  else if Obj.tag left >= Obj.no_scan_tag then left = right
  else
    let rec fields index =
      index = Obj.size left
      ||
      ( equal (Obj.field left index) (Obj.field right index)
        && fields (index + 1)
      )
    in
    fields 0

let equal_modulo_locations left right = equal (Obj.repr left) (Obj.repr right)

(* {1 Parsing} *)

(** [parse source] with the compiler's parser, without its warnings (as
    those about doc comments). *)
let quietly parse ~filename source =
  let state = Warnings.backup () in
  ignore (Warnings.parse_options false "-a");
  Fun.protect
    ~finally:(fun () -> Warnings.restore state)
    ( fun () ->
        let lexbuf = Lexing.from_string source in
        Location.init lexbuf filename;
        match parse lexbuf with
        | ast -> Some ast
        | exception _ -> None
    )

type difference = { summary : string; details : string }

(** The doc comments in an item: the payloads of its [ocaml.doc] and
    [ocaml.text] attributes, in order. *)
let doc_comments iterate item =
  let docs = ref [] in
  let attribute iterator (attribute : Parsetree.attribute) =
    match attribute.attr_name.txt, attribute.attr_payload with
    | ( ("ocaml.doc" | "ocaml.text"),
        PStr
          [ { pstr_desc =
                Pstr_eval
                  ( { pexp_desc =
                        Pexp_constant
                          { pconst_desc = Pconst_string (text, _, _); _ }
                    ;
                      _
                    }
                    ,
                    _
                  )
            ;
              _
            }
          ]
      )
    -> docs := text :: !docs
    | _ -> Ast_iterator.default_iterator.attribute iterator attribute
  in
  iterate { Ast_iterator.default_iterator with attribute } item; List.rev !docs

(** The source around [line], with the numbers of the lines. *)
let lines_around source line =
  String.split_on_char '\n' source
  |> List.mapi (fun index text -> index + 1, text)
  |> List.filter (fun (index, _) -> abs (index - line) <= 5)
  |> List.map (fun (index, text) -> Printf.sprintf "%5d %s" index text)
  |> String.concat "\n"

(** The first item which differs between two lists of items: at [line] (as
    given by [location]), printed by [print], with its doc comments found by
    [iterate]. *)
let compare_items ~print ~location ~iterate ~parse ~filename source formatted_source =
  match quietly parse ~filename source with
  | None -> Ok ()
  | Some original ->
      match quietly parse ~filename formatted_source with
      | None ->
          Error
            { summary = "the compiler's parser rejects the formatted source";
              details = formatted_source
            }
      | Some formatted ->
          let line item = (location item : Location.t).loc_start.pos_lnum in
          let rec loop original formatted =
            match original, formatted with
            | [], [] -> Ok ()
            | o :: original, f :: formatted when equal_modulo_locations o f ->
                loop original formatted
            | o :: _, f :: _ ->
                let quote docs =
                  String.concat ", " (List.map (Printf.sprintf "%S") docs)
                in
                let change =
                  let docs = doc_comments iterate o
                  and docs' = doc_comments iterate f
                  in
                  if docs <> docs' then
                    Printf.sprintf
                      "its doc comments change from [%s] to [%s]"
                      (quote docs) (quote docs')
                  else "its code changes"
                in
                Error
                  { summary =
                      Printf.sprintf
                        "the item at line %d (line %d once formatted): %s"
                        (line o) (line f) change
                  ;
                    details =
                      Format.asprintf
                        "The item:@\n%a@\nonce formatted:@\n%a@\nThe formatted source:@\n%s"
                        print o print f (lines_around formatted_source (line f))
                  }
            | o :: _, [] ->
                Error
                  { summary =
                      Printf.sprintf
                        "the formatted source has no item for line %d" (line o)
                  ;
                    details = formatted_source
                  }
            | [], f :: _ ->
                Error
                  { summary =
                      Printf.sprintf
                        "the formatted source has an extra item at line %d"
                        (line f)
                  ;
                    details = formatted_source
                  }
          in
          loop original formatted

let implementation ~filename source formatted =
  compare_items
    ~print:(fun formatter item -> Pprintast.structure formatter [ item ])
    ~location:(fun (item : Parsetree.structure_item) -> item.pstr_loc)
    ~iterate:(fun iterator item -> iterator.structure_item iterator item)
    ~parse:Parse.implementation ~filename source formatted

let interface ~filename source formatted =
  compare_items
    ~print:(fun formatter item -> Pprintast.signature formatter [ item ])
    ~location:(fun (item : Parsetree.signature_item) -> item.psig_loc)
    ~iterate:(fun iterator item -> iterator.signature_item iterator item)
    ~parse:Parse.interface ~filename source formatted
