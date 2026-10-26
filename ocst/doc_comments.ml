type docstring = [ `DOCSTRING of string ] Token.token

(* As in docstrings.ml: a doc comment is attached once it documents
   something, and associated with the items around it without blank lines,
   which is ambiguous for several items. *)
type attached = Unattached | Info | Docs
type associated = Zero | One | Many

type doc =
  { token : docstring; body : string;
    boundary : int; mutable attached : attached;
    mutable associated : associated; mutable placed : bool;
  }

(* The doc comments of a boundary, filed as by [Lexer.token]. *)
type tables =
  { docs : doc array; (** In source order. *) mutable post : doc list;
    mutable pre : doc list; mutable floating : doc list;
    mutable pre_extra : doc list; mutable post_extra : doc list;
  }

type t = { boundaries : tables array }

let empty_tables docs =
  { docs; post = []; pre = []; floating = []; pre_extra = []; post_extra = [] }

(* {1 Filing, as [Lexer.token]} *)

type lines = No_line | New_line | Blank_line

type doc_state =
  | Initial | After of doc list | Before of doc list * doc list * doc list

let attach tables lines docs =
  match docs, lines with
  | Initial, _ -> ()
  | After a, (No_line | New_line) -> tables.post <- List.rev a; tables.pre <- a
  | After a, Blank_line ->
      tables.post <- List.rev a; tables.pre_extra <- List.rev a
  | Before (a, f, b), (No_line | New_line) ->
      tables.post <- List.rev a;
      tables.post_extra <- List.rev_append f (List.rev b);
      tables.floating <- List.rev f;
      tables.pre_extra <- List.rev a;
      tables.pre <- b
  | Before (a, f, b), Blank_line ->
      tables.post <- List.rev a;
      tables.post_extra <- List.rev_append f (List.rev b);
      tables.floating <- List.rev_append f (List.rev b);
      tables.pre_extra <- List.rev a

let skip_trivia lines (trivia : Token.trivia) =
  match trivia with
  | Line_directive _ | Warning _ -> lines
  | Whitespace text ->
      let lines = ref lines in
      String.iter
        ( fun character ->
            if character = '\n' then
              lines
              :=
              match !lines with
                No_line -> New_line
              | New_line | Blank_line -> Blank_line
        )
        text
      ;
      !lines
  | Comment _ ->
      ( match lines with
          No_line | New_line -> No_line
        | Blank_line -> Blank_line
      )

let add_doc docs lines doc =
  if doc.body = "/*" then
    match docs with
    | Initial -> Before ([], [ doc ], [])
    | After a -> Before (a, [ doc ], [])
    | Before (a, f, b) -> Before (a, doc :: b @ f, [])
  else
    match docs, lines with
    | Initial, (No_line | New_line) -> After [ doc ]
    | Initial, Blank_line -> Before ([], [], [ doc ])
    | After a, (No_line | New_line) -> After (doc :: a)
    | After a, Blank_line -> Before (a, [], [ doc ])
    | Before (a, f, b), (No_line | New_line) -> Before (a, f, doc :: b)
    | Before (a, f, b), Blank_line -> Before (a, b @ f, [ doc ])

let split tokens =
  let boundaries = ref []
  and real = ref []
  and count = ref 0
  in
  let rec loop lines docs pending =
    function
    | [] -> ()
    | (token : Token.any_token) :: rest ->
        let lines = List.fold_left skip_trivia lines token.leading in
        match token.kind with
        | `DOCSTRING text ->
            let doc =
              { token = { leading = token.leading; kind = `DOCSTRING text };
                body = Token.docstring_body text; boundary = !count;
                attached = Unattached; associated = Zero; placed = false;
              }
            in
            loop No_line (add_doc docs lines doc) (doc :: pending) rest
        | _ ->
            let tables = empty_tables (Array.of_list (List.rev pending)) in
            attach tables lines docs;
            boundaries := tables :: !boundaries;
            real := token :: !real;
            incr count;
            loop No_line Initial [] rest
  in
  loop No_line Initial [] tokens;
  List.rev !real, { boundaries = Array.of_list (List.rev !boundaries) }

(* {1 Claims, as [Docstrings]} *)

let set
    doc ?(attached = doc.attached)
    ?(associated = doc.associated) ?(placed = doc.placed) ()
  = doc.attached <- attached; doc.associated <- associated; doc.placed <- placed

let tables t k = t.boundaries.(k)

let get_docstring ~info docs =
  match List.find_opt (fun doc -> doc.attached <> Info) docs with
  | None -> None
  | Some doc -> set doc ~attached:(if info then Info else Docs) (); Some doc

let get_docstrings docs =
  List.filter_map
    ( fun doc ->
        (* As [Docstrings.get_docstrings], although the floating and extra
           doc comments of a boundary are never the info of a constructor or
           a field in the grammar of OCaml. *)
        if doc.attached = Info then (None [@coverage off])
        else begin set doc ~attached:Docs (); Some doc end
    )
    docs

let associate docs =
  List.iter
    ( fun doc ->
        set doc ~associated:(if doc.associated = Zero then One else Many) ()
    )
    docs

let pre_docs t k =
  let docs = (tables t k).pre in
  associate docs; get_docstring ~info:false docs

let post_docs t k =
  let docs = (tables t k).post in
  associate docs; get_docstring ~info:false docs

let mark_pre t k = associate (tables t k).pre
let mark_post t k = associate (tables t k).post
let info t k = get_docstring ~info:true (tables t k).post
let text t k = get_docstrings (tables t k).floating
let post_text t k = get_docstrings (tables t k).post
let pre_extra_text t k = get_docstrings (tables t k).pre_extra
let post_extra_text t k = get_docstrings (tables t k).post_extra
let has_text t k =
  List.exists (fun doc -> doc.attached <> Info) (tables t k).floating

(* {1 Placement} *)

let warning doc =
  doc.token.leading
  @ [ Token.Warning (Unattached_docstring, Token.text doc.token) ]

let place t doc =
  let docs = (tables t doc.boundary).docs in
  let rec position index =
    if docs.(index) == doc then index else position (index + 1)
  in
  let position = position 0 in
  (* The checks of the placement, which the parser ensures. *)
  (if doc.placed then invalid_arg "Doc_comments.place: placed twice")
  [@coverage off]
  ;
  for index = position + 1 to Array.length docs - 1 do
    (if docs.(index).placed then invalid_arg "Doc_comments.place: out of order")
    [@coverage off]
  done;
  let leading = ref [] in
  for index = 0 to position - 1 do
    let other = docs.(index) in
    if not other.placed then begin
      set other ~placed:true (); leading := !leading @ warning other
    end
  done;
  set doc ~placed:true ();
  { doc.token with leading = !leading @ doc.token.leading }

let ambiguous doc = doc.attached = Docs && doc.associated = Many

let resolve t k ~post ~texts ~pre =
  let slot =
    function
      Some doc when not (ambiguous doc) && not doc.placed -> Some doc
    | _ -> None
  in
  let post = slot post
  and pre = slot pre
  in
  let is doc =
    function
      Some other -> other == doc
    | None -> false
  in
  let post_token = ref None
  and floating = ref []
  and pre_token = ref None
  in
  Array.iter
    ( fun doc ->
        if doc.placed then ()
        else if is doc post then begin
          ( if !floating <> [] || !pre_token <> None then
              invalid_arg "Doc_comments.resolve"
          )
          [@coverage off]
          ;
          post_token := Some (place t doc)
        end
        else if is doc pre then pre_token := Some (place t doc)
        else if
          ambiguous doc || (doc.attached = Docs && List.memq doc texts)
        then begin
          (if !pre_token <> None then invalid_arg "Doc_comments.resolve")
          [@coverage off]
          ;
          let token = place t doc in
          floating
          :=
          (if ambiguous doc then Cst.Ambiguous token else Cst.Text token)
          :: !floating
        end
    )
    (tables t k).docs
  ;
  !post_token, List.rev !floating, !pre_token

let remaining t k =
  Array.fold_left
    ( fun trivia doc ->
        if doc.placed then trivia
        else begin set doc ~placed:true (); trivia @ warning doc end
    )
    [] (tables t k).docs
