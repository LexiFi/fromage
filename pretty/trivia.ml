(* The comments and the blank lines before the tokens of a file. The
   comments on the line of the previous token are printed after it; the
   others are printed before the token, each on its line, with the blank
   lines before them. *)

open Ocst

module Physical_token =
  struct
    type t = Token.any_token

    let equal = ( == )
    let hash = Hashtbl.hash
  end

module Tokens = Hashtbl.Make (Physical_token)

type comment = Plain of string | Directive of string

type info =
  { mutable trailing : string list;
    (** The comments after the token, on its line, in reverse order. *)
    before : (int * comment) list;
    (** The comments before the token, with the number of newlines before each. *)
    newlines : int; (** The number of newlines just before the token. *)
  }

let count_newlines text =
  String.fold_left (fun count c -> if c = '\n' then count + 1 else count) 0 text

let infos tokens =
  let table = Tokens.create 4096 in
  let previous = ref None in
  List.iteri
    ( fun index (token : Token.any_token) ->
        let hanging = ref (index > 0)
        and newlines = ref 0
        and before = ref []
        in
        List.iter
          ( fun (trivia : Token.trivia) ->
              match trivia, !previous with
              | Token.Whitespace text, _ ->
                  let count = count_newlines text in
                  if
                    count > 0
                  then begin hanging := false; newlines := !newlines + count end
              | (Token.Comment text | Token.Warning (_, text)), Some info when
                  !hanging
              -> info.trailing <- text :: info.trailing
              | _ ->
                  let comment =
                    match trivia with
                    | Token.Line_directive text -> Directive text
                    | _ -> Plain (Token.trivia_text trivia)
                  in
                  (* No blank line at the start of the file. *)
                  let count =
                    if index = 0 && !before = [] then 1 else max !newlines 1
                  in
                  hanging := false;
                  before := (count, comment) :: !before;
                  newlines := 0
          )
          token.leading
        ;
        let newlines = if index = 0 && !before = [] then 0 else !newlines in
        let info = { trailing = []; before = List.rev !before; newlines } in
        Tokens.replace table token info; previous := Some info
    )
    tokens
  ;
  table

let table = ref (Tokens.create 1)
let set tokens = table := infos tokens
let clear () = table := Tokens.create 1
let info (token : Token.any_token) = Tokens.find !table token

(** Two tokens can be printed without a space between them when they are
    lexed as the same two tokens, without comments between them. *)
let glued = Hashtbl.create 256

let can_glue (left : Token.any_token) (right : Token.any_token) =
  let left_info = info left
  and right_info = info right
  in
  left_info.trailing = [] && right_info.before = [] && right_info.newlines < 2
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
