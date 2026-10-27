(* The comments and the blank lines before a token, from its trivia: see
   trivia.mli. *)

open Ocst

type elt =
  | Newline of int | Space of int | Comment of string | Directive of string

type t = elt list

(** The blanks and newlines of [text]: [Space] without newlines, else the
    newlines and the indentation after them; the blanks before a newline
    end their line, and are not kept. *)
let whitespace text =
  let newlines = ref 0
  and spaces = ref 0
  in
  String.iter
    ( function
      | '\n' -> incr newlines; spaces := 0
      | '\r' -> ()
      | _ -> incr spaces
    )
    text
  ;
  (if !newlines > 0 then [ Newline !newlines ] else [])
  @ if !spaces > 0 then [ Space !spaces ] else []

let of_token (token : Token.any_token) =
  List.concat_map
    ( function
      | Token.Whitespace text -> whitespace text
      | Token.Comment text | Token.Warning (_, text) -> [ Comment text ]
      | Token.Line_directive text -> [ Directive text ]
    )
    token.leading

type info =
  { previous_line : string list;
    (** The comments on the line of the previous token, in order: they are
        printed at the end of that line. *)
    before : (int * elt) list;
    (** The comments and the line directives before the token, with the
        number of newlines before each. *)
    newlines : int; (** The number of newlines just before the token. *)
  }

(** The first token of the file being printed, which has no previous line:
    the comments before it are before it. *)
let first = ref None

let set tokens =
  first
  :=
  match tokens with
  | token :: _ -> Some token
  | [] -> None

let clear () = first := None

let info (token : Token.any_token) =
  let is_first =
    match !first with
    | Some first -> first == token
    | None -> false
  in
  let on_previous_line = ref (not is_first)
  and newlines = ref 0
  and previous_line = ref []
  and before = ref []
  in
  List.iter
    ( function
      | Newline count ->
          on_previous_line := false; newlines := !newlines + count
      | Space _ -> ()
      | Comment text when !on_previous_line ->
          previous_line := text :: !previous_line
      | (Comment _ | Directive _) as elt ->
          (* No blank line at the start of the file. *)
          let count = if is_first && !before = [] then 1 else max !newlines 1 in
          on_previous_line := false;
          before := (count, elt) :: !before;
          newlines := 0
    )
    (of_token token)
  ;
  { previous_line = List.rev !previous_line; before = List.rev !before;
    newlines = (if is_first && !before = [] then 0 else !newlines)
  }

(** Two tokens can be printed without a space between them when they are
    lexed as the same two tokens, without comments between them. *)
let glued = Hashtbl.create 256

let can_glue (left : Token.any_token) (right : Token.any_token) =
  let right_info = info right in
  right_info.previous_line = []
  && right_info.before = [] && right_info.newlines < 2
  &&
  let left_text = Token.text left
  and right_text = Token.text right
  in
  left_text = "" || right_text = ""
  ||
  match Hashtbl.find_opt glued (left.kind, right.kind) with
  | Some result -> result
  | None ->
      let last = left_text.[String.length left_text - 1]
      and next = right_text.[0]
      in
      let result =
        (not ((last = '(' && next = '*') || (last = '*' && next = ')')))
        &&
        match Ocst.lex (left_text ^ right_text) with
        | [ first; second; { kind = `EOF; _ } ] ->
            first.leading = [] && second.leading = []
            && first.kind = left.kind && second.kind = right.kind
        | _ -> false
        | exception _ -> false
      in
      (* The cache is bounded: the kinds of the tokens include their texts. *)
      if Hashtbl.length glued > 100_000 then Hashtbl.reset glued;
      Hashtbl.add glued (left.kind, right.kind) result;
      result
