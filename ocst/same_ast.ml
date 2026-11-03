(* Compares the compiler's ASTs of two sources, except for the locations: see
   same_ast.mli. *)

(* {1 Equality modulo locations}

   The locations are replaced by [Location.none] with [Ast_mapper], which
   also empties the location stacks (it rebuilds the nodes with
   [Ast_helper]); the ASTs are then compared with [=]. *)

(** The mapper which replaces every location by [Location.none]. *)
let without_locations =
  { Ast_mapper.default_mapper with location = (fun _ _ -> Location.none) }

let structure_item_without_locations item =
  without_locations.structure_item without_locations item

let signature_item_without_locations item =
  without_locations.signature_item without_locations item

(** The dump of an item ([dump]) once its locations are erased ([erase]):
    without them, which are all [Location.none], printed as [none]. *)
let dump_without_locations erase dump item =
  let none = " (_none_[0,0+-1]..[0,0+-1]) ghost" in
  let dump = dump (erase item) in
  let buffer = Buffer.create (String.length dump) in
  let rec copy index =
    if index < String.length dump then
      if
          index + String.length none <= String.length dump
        && String.sub dump index (String.length none) = none
      then copy (index + String.length none)
      else begin Buffer.add_char buffer dump.[index]; copy (index + 1) end
  in
  copy 0; Buffer.contents buffer

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
    given by [location]), shown by the differences of their [dump]s (without
    their locations), with its doc comments found by [iterate]. *)
let compare_items
    ~dump ~location ~iterate ~without_locations
    ~parse ~filename original formatted_source
  =
    match original with
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
              | o :: original, f :: formatted
                when without_locations o = without_locations f
              -> loop original formatted
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
                        Printf.sprintf
                          "The AST of the item, without its locations (- \
                           before, + once formatted):\n%s\nThe formatted \
                           source:\n%s"
                          ( Text_diff.diff
                              (dump_without_locations without_locations dump o)
                              (dump_without_locations without_locations dump f)
                          )
                          (lines_around formatted_source (line f))
                    }
              | o :: _, [] ->
                  Error
                    { summary =
                        Printf.sprintf
                          "the formatted source has no item for line %d"
                          (line o)
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

let compare_implementations ~filename original formatted =
  compare_items
    ~dump:(fun item -> Format.asprintf "%a" Printast.implementation [ item ])
    ~location:(fun (item : Parsetree.structure_item) -> item.pstr_loc)
    ~iterate:(fun iterator item -> iterator.structure_item iterator item)
    ~without_locations:structure_item_without_locations
    ~parse:Parse.implementation ~filename original formatted

let compare_interfaces ~filename original formatted =
  compare_items
    ~dump:(fun item -> Format.asprintf "%a" Printast.interface [ item ])
    ~location:(fun (item : Parsetree.signature_item) -> item.psig_loc)
    ~iterate:(fun iterator item -> iterator.signature_item iterator item)
    ~without_locations:signature_item_without_locations
    ~parse:Parse.interface ~filename original formatted

let implementation ~filename source formatted =
  compare_implementations
    ~filename (quietly Parse.implementation ~filename source) formatted

let interface ~filename source formatted =
  compare_interfaces
    ~filename (quietly Parse.interface ~filename source) formatted

let implementation_of_ast ~filename original formatted =
  compare_implementations ~filename (Some original) formatted

let interface_of_ast ~filename original formatted =
  compare_interfaces ~filename (Some original) formatted
