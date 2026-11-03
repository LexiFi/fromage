(* Documents of pretty_expressive, with the tokens of the CST and their
   comments: see doc.mli. *)

open Ocst
open Cst

module type CONFIG = sig val page_width : int end

(* The signature of the documents: see doc.mli. *)
module type S =
  sig
    type t
    (** A document: tokens with their comments, and their layouts. It knows its
      first and last tokens, for [( ++ )], and the comments before its first
      token (its leading comments). *)

    (** {1 Building documents} *)

    val empty : t
    (** Nothing: no token. The concatenations of [empty] and a document add no
      separator. *)

    val is_empty : t -> bool

    val tok : [< Token.any_kind ] Token.token -> t
    (** A token, with the comments around it (see [Trivia]): the comments on
      its line after it, and the comments and blank lines before it. *)

    val opt : ('a -> t) -> 'a option -> t
    (** [print value], or [empty]. *)

    (** {1 Concatenation: fixed separators} *)

    val ( +- ) : t -> t -> t
    (** [left right], with a space between them. *)

    val ( ++ ) : t -> t -> t
    (** [left right], without space between them when the two tokens around
      are lexed as the same two tokens and have no comments between them, as
      in [M.x] or [f(]; with a space otherwise. Never a line break. *)

    val ( +| ) : t -> t -> t
    (** [left], a line break, then [right], at the same level. *)

    val ( +/ ) : t -> t -> t
    (** [left], a line break, then [right], at a level indented by 2. *)

    val below : indent:int -> t -> t -> t
    (** [left], a line break, then [right], at a level indented by [indent],
      which may be negative. *)

    val words : t list -> t
    (** The documents, separated by spaces. *)

    val vertical : t list -> t
    (** The documents, separated by line breaks: one per line, at the same
      level. *)

    val items : (t * bool) list -> t
    (** The items of a structure or a signature, as [vertical], with a blank
      line after an item on several lines (kept when the source has one),
      except next to the items [adjacent] to the items around them ([true]):
      an ambiguous doc comment documents the items before and after it only
      without blank lines between them. *)

    val nest : int -> t -> t
    (** The document, with its level indented by [indent]: the lines after its
      first one are indented by [indent] more. No separator. *)

    val shifted : ?mid_line:bool -> t -> t
    (** The document 2 columns to the right: after two spaces when it starts
      its line, and its next lines indented by 2. In the middle of a line,
      the two spaces are one space, unless [mid_line]: [(   a] after [( ]. *)

    (** {1 Layouts: breakable separators} *)

    val ( <|> ) : t -> t -> t
    (** The layouts of both documents, which have the same tokens. *)

    val as_last_resort : t -> t
    (** The layouts of the document, which are chosen only when the other
      layouts of the choices around it exceed the width of the page or are
      impossible, whatever their numbers of lines: for a layout which is only
      wanted when another one cannot be used, such as [->] at the start of a
      line after a pattern on several lines. *)

    val losing_ties : t -> t
    (** The layouts of the document, which are chosen only when the other
      layouts of the choices around it have more lines, exceed the width of
      the page or are impossible: for a layout which is wanted only when
      another one would take more lines, such as a separator alone on its line
      after a value on several lines. The layouts of the same number of lines
      are otherwise chosen for the most even lengths of their lines. *)

    val flat : t -> t
    (** The layouts of the document without line breaks (except those of its
      leading comments): no layout when its separators include fixed line
      breaks. *)

    val closed : t -> t
    (** The document, as a whole closed by its last token, which starts its
      line when the document is on several lines ([( ... )] from [bracket],
      [begin ... end], [while ... done]): a separator after it follows that
      token ([separator_after]). *)

    val inline : t -> t -> t
    (** [left right], with a space: no layout when [right] has leading
      comments on their own lines or a blank line before it. *)

    val pack_list_isolating_multiline : ?glued:bool -> indent:int -> t list -> t
    (** A list of values, as many per line as fit, the lines after the first
      one at a level indented by [indent]; a value on several lines has a line
      break before it and after it: no value follows it on its last line. Of
      the layouts otherwise equal, the one whose first lines are the longest is
      chosen. The values on a line are separated by spaces, or [glued] to each
      other ([a#m#n]). *)

    val pack_after_head : indent:int -> t -> t list -> t
    (** [head] and its [values] (arguments, parameters...): all on one line,
      or the values packed ([pack_list_isolating_multiline]) on the next lines,
      at a level indented by [indent]:
      {[
        f x y z
      ]}
      {[
        f
          x y
          z
      ]} *)

    val lines : t list -> t
    (** The documents separated by spaces, on one line, or by line breaks at
      the same level: never a mix of both. *)

    val after : t -> t -> t
    (** [left right], with a space and [right] on one line, or with a line break
      and [right] at a level indented by 2, in any layout. *)

    val after_line : t -> t -> t
    (** As [after], with [right] at the same level after the line break. *)

    val hanging : t -> t -> t
    (** [left right], with a space and [right] in any layout, its next lines
      indented from the level of [left]; or with a line break and [right] at a
      level indented by 2. For blocks such as [struct ... end], which are not
      brackets. *)

    val labelled : t -> t -> t
    (** [~label:value]: without space and [value] on one line, or with a line
      break and [value] at a level indented by 2. *)

    val starts_with_comments_of_previous_line : t -> bool
    (** Does the document start with comments written on the line of the
      token before it, which stay at the end of that line? *)

    val starts_with_keyword : t -> bool
    (** Does the document start with a keyword, as [fun] or [if]? *)

    val first_kind : t -> Token.any_kind option
    (** The kind of the first token of the document. *)

    (** {1 Brackets} *)

    val bracket : ?spaces:bool -> ?inline:bool -> t -> t -> t -> t
    (** [bracket left content right], which has two layouts:
      - on one line: [left], [content] and [right] separated by spaces when
        [spaces], as [{ x = 1 }], or without space when possible otherwise, as
        [(x, y)];
      - on several lines: [right] after a line break, in the column of [left],
        which must start its line. When [inline] (the default), the content
        follows [left] after a space, at a level indented by 2, as in [( f x];
        otherwise, it follows a line break, at a level indented by 2, as after
        [\[|] or [{<].

      The content of [( struct], [( sig] and [( object] is not indented: their
      items are indented by 2 after the bracket. *)

    val paren :
      ?spaces:bool -> ?inline:bool -> ('a -> t)
      -> ([< Token.any_kind ], 'a, [< Token.any_kind ]) Cst.delimited
      -> t
    (** [bracket] around delimited content. *)

    (** {1 Lists}

      These functions give the elements of lists, each with its separator; the
      caller joins them, as with [lines] or [vertical]. *)

    val separator_after : ?in_bracket:bool -> t -> t -> t
    (** [separator_after value separator]: [value] on one line and [separator]
      after it, without space when possible, or [value] in any layout and
      [separator] after a line break, at the same level: a separator after a
      multi-line value is alone on its line. [in_bracket]: [value] is an
      element of a bracket, and the separator alone on its line is in the
      column of the bracket, 2 columns before the elements. *)

    val separated_after :
      ?in_bracket:bool -> ('a -> t)
      -> ('a, [< Token.any_kind ]) Cst.non_empty_separated_list
      -> t list
    (** The elements of a separated list, each followed by its separator, as
      with [separator_after]: [x,], or the separator alone on its line after a
      multi-line element. *)

    val separated_before :
      ('a -> t) -> ('a, [< Token.any_kind ]) Cst.non_empty_separated_list
      -> t list
    (** The elements of a separated list, each after the first one preceded by
      its separator and a space, at a level indented by 2 from the
      separator: [* t]. *)

    val semis : ('a -> t) -> 'a Cst.semis -> t list
    (** The elements of a list in a bracket, separated or terminated by [;],
      as with [separated_after ~in_bracket:true]. *)

    val commas : ('a -> t) -> 'a Cst.commas -> t list
    (** The elements of a list in a bracket, separated by [,], as with
      [separated_after ~in_bracket:true]. *)

    val bar_list :
      ?doc:('a -> Cst.docstring option) -> ('a -> t) -> 'a Cst.bar_list
      -> t list
    (** The elements of a list separated by [|], each preceded by its bar and a
      space, at a level indented by 2 from the bar, and followed by its doc
      comment [doc] (see [with_info]). A first element without a bar is
      preceded by two spaces, which indent it as if it had one when it starts
      its line; elsewhere, they are one space. *)

    val with_info : t -> Cst.docstring option -> t
    (** A document followed by its doc comment: after a line break when the doc
      comment was on the next line in the source, after a space otherwise. *)

    (** {1 Rendering} *)

    val to_string : t -> [ `EOF ] Token.token -> string
    (** The text of the document of a file, in its best layout, followed by the
      comments before the end of the file, without spaces at the end of the
      lines. *)
  end

module Make (Config : CONFIG) : S =
  struct
    let page_width = Config.page_width

    (** The costs of the layouts, compared in the order of their fields. *)
    type cost =
      { overflow : int;
        (** The characters after the width, as in the default cost factory
              of pretty_expressive: each one costs more than the previous. *)
        penalty : int; (** The penalties of [as_last_resort]. *)
        height : int; (** The line breaks. *)
        ties : int; (** The penalties of [losing_ties]. *)
        squares : int;
        (** The sum of the squares of the lengths of the lines (from their
              start, with their indentation): for the same characters on the
              same number of lines, the smallest when the lines are of the
              same length. *)
        early_breaks : int;
        (** The line breaks of the packed lists, each counted by the number of
          values after it: of the layouts otherwise equal, the one whose first
          lines are the longest. *)
      }

    let no_cost =
      { overflow = 0; penalty = 0; height = 0;
        ties = 0; squares = 0; early_breaks = 0;
      }

    (** The cost factory of the default one of pretty_expressive, with the
      [squares] of the lines after their number: of the layouts with the same
      number of lines, the one with lines of the most even lengths. *)
    module Cost =
      struct
        type t = cost

        (* Beyond this column, pretty_expressive stops comparing the layouts
           (it "taints" them): 2 × the width, so that a token longer than
           the width does not make it pick the layouts around it at random. *)
        (* Beyond this column, pretty_expressive stops comparing the layouts
           (it "taints" them): 2 × the width, so that a token longer than
           the width does not make it pick the layouts around it at random. *)
        let limit = page_width * 2

        let text position length =
          let stop = position + length in
          let overflow =
            if stop > page_width then
              let start = max page_width position in
              let after = stop - start in
              after * ((2 * (start - page_width)) + after)
            else 0
          in
          { no_cost with
            overflow; squares = (stop * stop) - (position * position);
          }

        let newline indent =
          { no_cost with height = 1; squares = indent * indent }

        let combine left right =
          { overflow = left.overflow + right.overflow;
            penalty = left.penalty + right.penalty;
            height = left.height + right.height; ties = left.ties + right.ties;
            squares = left.squares + right.squares;
            early_breaks = left.early_breaks + right.early_breaks;
          }

        let le left right = compare left right <= 0

        let two_columns_overflow width = { no_cost with penalty = width }

        let two_columns_bias _ = no_cost

        let string_of_cost
            { overflow; penalty; height; ties; squares; early_breaks }
          =
            Printf.sprintf
              "(%d %d %d %d %d %d)" overflow penalty
              height ties squares early_breaks

        let debug_format =
          Pretty_expressive.Printer.make_debug_format page_width
      end

    module P = Pretty_expressive.Printer.Make (Cost)
    (* {1 Documents and their one-line versions}

     The function [flatten] of pretty_expressive keeps all the documents which
     it flattens, forever: the documents here are built with their versions on
     one line instead, as [flatten] would compute them. *)

    (** A document of pretty_expressive and its version on one line. *)
    type versions =
      { doc : P.doc;
        flat : P.doc; (** [P.fail] when the document cannot be on one line. *)
      }

    let both doc = { doc; flat = doc }
    let none = both P.empty
    let ( ^^ ) left right =
      { doc = P.(left.doc ^^ right.doc); flat = P.(left.flat ^^ right.flat) }

    (** A space, which the renderer drops at the end of a line. *)
    let space = both (P.text "\000")

    (** A line break: there is no version on one line. *)
    let line_break = { doc = P.hard_nl; flat = P.fail }

    let nest_versions indent value =
      { value with doc = P.nest indent value.doc }

    (** The lines of texts which are not measured, by number: the lines longer
      than the page, which cannot fit anyway, and the doc comments after
      their items. Such a line is measured as a few characters, by a
      placeholder which [render] replaces: it does not change the layout of
      the code around it. Longer than the computation width of
      pretty_expressive (by default 1.2 times the page width), it would also
      make it stop comparing the layouts. *)
    let long_lines = Hashtbl.create 16

    (** A line of text, without newline. *)
    let line_text ?(measured = true) line =
      if measured && String.length line <= page_width then P.text line
      else
        let number = Hashtbl.length long_lines in
        Hashtbl.replace long_lines number line;
        P.text ("\002" ^ string_of_int number ^ "\002")

    (** A text, which may span several lines, kept as it is. *)
    let verbatim ?measured ?(measured_on_one_line = true) text =
      match String.split_on_char '\n' text with
      | [] -> none
      | [ line ] when measured_on_one_line = Option.value measured ~default:true
      -> both (line_text ?measured line)
      | [ line ] ->
          { doc = line_text ?measured line;
            flat = line_text ~measured:measured_on_one_line line
          }
      | first :: rest ->
          let doc =
            List.fold_left
              ( fun doc line ->
                  P.(doc ^^ reset (hard_nl ^^ line_text ?measured line))
              )
              (line_text ?measured first) rest
          in
          { doc; flat = P.fail }

    (* {1 Comments} *)

    (** A comment or a line directive of [Trivia.info]'s [before], which has
      no blanks nor newlines. *)
    let comment_doc ?measured =
      function
      | Trivia.Comment text -> verbatim ?measured text
      | Directive line -> { doc = P.text ("\001" ^ line); flat = P.fail }
      | Newline _ | Space _ -> none

    (** The comments and blank lines before a token, up to its last comment.
      A comment alone on its line is not measured: whatever its length, it
      does not change the layout of the code around it. The last comment is
      followed by code on its line when [last_followed_on_its_line]. *)
    let comments_before
        ?(last_followed_on_its_line = false) (info : Trivia.info)
      =
        let count = List.length info.before in
        let doc, _ =
          List.fold_left
            ( fun (doc, index) (newlines, comment) ->
                let separator =
                  if index = 0 then if newlines >= 2 then line_break else none
                  else if newlines >= 2 then line_break ^^ line_break
                  else line_break
                in
                let measured = index = count - 1 && last_followed_on_its_line in
                doc ^^ separator ^^ comment_doc ~measured comment, index + 1
            )
            (none, 0) info.before
        in
        doc

    (* {1 Documents} *)

    type t =
      { line_end : versions;
        (** The comments on the line before the document, from the trivia of
            its first token: [join] prints them at the end of the document
            before it, so that they stay on their line. *)
        leading : versions;
        breaks : bool; (** Whether [leading] ends a line: the document then starts a line. *)
        body : versions; first : Token.any_token option;
        last : Token.any_token option;
        closed : bool;
        (** Whether the document is a whole closed by its last token, which
              starts its line when the document is on several lines: [( ... )],
              [begin ... end], [while ... done]. *)
      }

    let empty =
      { line_end = none; leading = none; breaks = false;
        body = none; first = None; last = None; closed = false;
      }

    let is_empty value = Option.is_none value.first

    (** A token, measured unless [~measured:false], and in its version on one
      line unless [~measured_on_one_line:false]. *)
    let token_doc
        ?measured ?measured_on_one_line
        (value : [< Token.any_kind ] Token.token)
      =
        let token = (value :> Token.any_token) in
        let info = Trivia.info token in
        let leading =
          match info.before with
          | [] -> if info.newlines >= 2 then line_break else none
          | _ ->
              let separator =
                if info.newlines >= 2 then line_break ^^ line_break
                else if info.newlines = 1 then line_break
                else space
              in
                comments_before
                  ~last_followed_on_its_line:(info.newlines = 0) info
              ^^ separator
        in
        let line_end =
          List.fold_left
            (fun doc comment -> doc ^^ space ^^ verbatim comment)
            none info.previous_line
        in
        (* A doc comment at the start of its line stays there. *)
        let starts_line =
          match token.kind with
          | `DOCSTRING _ -> info.newlines >= 1
          | _ -> false
        in
        { line_end; leading;
          breaks = info.before <> [] || info.newlines >= 2 || starts_line;
          body = verbatim ?measured ?measured_on_one_line (Token.text token);
          first = Some token; last = Some token; closed = false;
        }

    let tok value = token_doc value

    let opt print =
      function
        None -> empty
      | Some value -> print value

    (** [left right], joined by [separator]. *)
    let join separator left right =
      if is_empty left then right
      else if is_empty right then left
      else
        { line_end = left.line_end;
          leading = left.leading; breaks = left.breaks;
          (* The comments on the line of [left] stay on it, before the
             separator. *)
          body =
            separator
              (left.body ^^ right.line_end) (right.leading ^^ right.body)
        ;
          first = left.first; last = right.last; closed = false;
        }

    let ( +| ) = join (fun left right -> left ^^ line_break ^^ right)

    (* A document after comments on lines of their own ([breaks]) starts its
     line: after a space, the comments would end the line of [left], and be
     printed after it when formatted again. *)
    let ( +- ) left right =
      if right.breaks && not (is_empty left) then left +| right
      else join (fun left right -> left ^^ space ^^ right) left right

    let below ~indent =
      join
        (fun left right -> left ^^ nest_versions indent (line_break ^^ right))

    let ( +/ ) left right = below ~indent:2 left right

    let ( ++ ) left right =
      match left.last, right.first with
      | Some last, Some first
        when Trivia.can_glue last first && not right.breaks
      -> join ( ^^ ) left right
      | _ -> left +- right

    let words values = List.fold_left ( +- ) empty values
    let vertical values = List.fold_left ( +| ) empty values

    (* {1 Layouts} *)

    let ( <|> ) left right =
      { left with
        closed = left.closed && right.closed;
        body =
          { doc = P.(left.body.doc <|> right.body.doc);
            flat = P.(left.body.flat <|> right.body.flat)
          }
      }

    let flat value = { value with body = both value.body.flat }

    let closed value = { value with closed = true }

    let with_cost cost value =
      let cost = P.cost cost in
      { value with
        body = { doc = cost value.body.doc; flat = cost value.body.flat }
      }

    let as_last_resort = with_cost { no_cost with penalty = 1 }
    let losing_ties = with_cost { no_cost with ties = 1 }

    (** A document without layout. *)
    let impossible value = { value with body = both P.fail }

    let nest indent value =
      { value with
        line_end = nest_versions indent value.line_end;
        leading = nest_versions indent value.leading;
        body = nest_versions indent value.body
      }

    let shifted ?(mid_line = false) value =
      let value = nest 2 value in
      let spaces = if mid_line then both (P.text "  ") else space ^^ space in
      { value with body = spaces ^^ value.body }

    let inline left right =
      let value = left +- right in
      if right.breaks && not (is_empty left) then impossible value else value

    let lines values = flat (words values) <|> vertical values

    (** Does the document start after a blank line, before its comments or
      its first token? *)
    let starts_after_blank_line value =
      match value.first with
      | None -> false
      | Some token ->
          let info = Trivia.info token in
          begin match info.before with
          | [] -> info.newlines >= 2
          | (newlines, _) :: _ -> newlines >= 2
          end

    let ( +|| ) =
      join (fun left right -> left ^^ line_break ^^ line_break ^^ right)

    let items values =
        List.fold_right
          ( fun (value, adjacent) (rest, rest_adjacent) ->
              let joined =
                if
                    is_empty rest || starts_after_blank_line rest
                  || adjacent || rest_adjacent
                then value +| rest
                else
                  (* [value] on one line, or a blank line after it. *)
                  (flat value +| rest) <|> (value +|| rest)
              in
              joined, adjacent
          )
          values (empty, false)
      |> fst

    (* The values so far end either with a value on one line ([single]), which
     the next one may follow on its line, or with a value on several lines
     ([multiple]), after which the next one starts a line. *)
    let pack_lines_isolating_multiline ?(glued = false) ~indent first values =
      (* [value] after [previous] on its line: after a space, or [glued]. *)
      let inline previous value =
        if glued then
          let value = previous ++ value in
          if value.breaks then impossible value else value
        else inline previous value
      in
      let count = List.length values in
      let single, multiple, _ =
        List.fold_left
          ( fun (single, multiple, index) value ->
              let any = single <|> multiple
              (* The values after the line break before [value]. *)
              and early =
                with_cost { no_cost with early_breaks = count - index }
              in
              ( inline single (flat value)
                <|> early (below ~indent any (flat value))
                ,
                early (below ~indent any value), index + 1
              )
          )
          (flat first, first, 0) values
      in
      single <|> multiple

    let after left right =
      if is_empty right then left
      else inline left (flat right) <|> (left +/ right)

    let after_line left right =
      if is_empty right then left
      else inline left (flat right) <|> (left +| right)

    let hanging left right = inline left right <|> (left +/ right)

    let pack_list_isolating_multiline ?glued ~indent =
      function
      | first :: rest ->
          pack_lines_isolating_multiline ?glued ~indent first rest
      | [] -> empty

    let pack_after_head ~indent head values =
      match values with
      | [] -> head
      | _ ->
          let values = pack_list_isolating_multiline ~indent:0 values in
          flat (head +- values) <|> below ~indent head values

    let starts_with_comments_of_previous_line value =
      match value.first with
      | Some first -> (Trivia.info first).previous_line <> []
      | None -> false

    let first_kind value =
      Option.map (fun (token : Token.any_token) -> token.kind) value.first

    let starts_with_keyword value =
      match value.first with
        Some { kind = #Token.keyword; _ } -> true
      | _ -> false

    let labelled label value =
      let glued = label ++ flat value in
      (if value.breaks then impossible glued else glued) <|> (label +/ value)

    (* {1 Brackets} *)

    let bracket ?(spaces = false) ?(inline = true) left content right =
      if is_empty content then left ++ right
      else
        let one_line =
          flat
            ( if spaces then left +- content +- right
              else left ++ content ++ right
            )
        in
        let broken =
          if not inline then left +/ content +| right
          else left +- nest 2 content +| right
        in
        closed (one_line <|> losing_ties broken)

    let paren ?spaces ?inline print { left; content; right } =
      bracket ?spaces ?inline (tok left) (print content) (tok right)

    (* {1 Lists} *)

    let separator_after ?(in_bracket = false) value separator =
      let alone =
        if in_bracket then below ~indent:(-2) value separator
        else value +| separator
      in
      match value.last with
      (* After [)], [end] or [done] which close the value, at the start of
         their line, the separator follows on their line. *)
      | Some { kind = `RPAREN | `END | `DONE; _ }
        when
          value.closed && not (starts_with_comments_of_previous_line separator)
      -> value ++ separator
      | _ -> flat value ++ separator <|> losing_ties alone

    let separated_after ?in_bracket print { first; rest } =
      let rec loop previous =
        function
        | [] -> [ previous ]
        | (separator, value) :: rest ->
              separator_after ?in_bracket previous (tok separator)
            :: loop (print value) rest
      in
      loop (print first) rest

    let separated_before print { first; rest } =
        print first
      ::
        List.map
          (fun (separator, value) -> tok separator +- nest 2 (print value)) rest

    let semis print ((values, trailing) : _ semis) =
      match
        List.rev (separated_after ~in_bracket:true print values), trailing
      with
      | last :: others, Some trailing ->
          List.rev
            (separator_after ~in_bracket:true last (tok trailing) :: others)
      | values, _ -> List.rev values

    let commas print =
      function
        Zero -> []
      | Many values -> separated_after ~in_bracket:true print values

    let with_info value doc =
      match doc with
      | None -> value
      | Some doc ->
          let info = Trivia.info (doc :> Token.any_token) in
          if info.before <> [] || info.newlines > 0 then value +| tok doc
          else
            (* A doc comment after its item does not make its line too long.
               It is measured on one line, so that the items around it do not
               seem to fit on one line with it. *)
            value +- token_doc ~measured:false ~measured_on_one_line:true doc

    let bar_list
        ?(doc = fun _ -> None) print ((bar, { first; rest }) : _ bar_list)
      =
        let element bar value =
          with_info (tok bar +- nest 2 (print value)) (doc value)
        in
        let first =
          match bar with
          | Some bar -> element bar first
          | None -> with_info (shifted (print first)) (doc first)
        in
        first :: List.map (fun (bar, value) -> element bar value) rest

    (* {1 Rendering} *)

    (** Prints a document, without the spaces at the end of the lines: the
      indentation of empty lines and the spaces ["\000"] between words. A text
      starting with ["\001"] is a line directive, which starts its line, and a
      text starting with ["\002"] the placeholder of a long line. *)
    let render doc =
      let buffer = Buffer.create 4096 in
      let pending = ref 0
      and indentation = ref false
      and line_start = ref true
      in
      P.pretty_print
        ( fun text ->
            if text = "\n" then begin
              Buffer.add_char buffer '\n';
              pending := 0;
              indentation := true;
              line_start := true
            end
            else if !indentation then begin
              indentation := false; pending := String.length text
            end
            else if text = "" then ()
            else if String.for_all (fun c -> c = '\000') text then
              (* Spaces add up at the start of a line only. *)
              pending :=
                ( if !line_start then !pending + String.length text
                  else max !pending 1
                )
            else if text.[0] = '\001' then begin
              (* A line directive, at the start of a line. *)
              if not !line_start then Buffer.add_char buffer '\n';
              pending := 0;
              line_start := false;
              Buffer.add_string
                buffer (String.sub text 1 (String.length text - 1))
            end
            else begin
              Buffer.add_string buffer (String.make !pending ' ');
              pending := 0;
              line_start := false;
              if text.[0] = '\002' then
                (* The placeholder of a long line: its number between two ['\002']. *)
                let number =
                  int_of_string
                    (String.sub text 1 (String.index_from text 1 '\002' - 1))
                in
                Buffer.add_string buffer (Hashtbl.find long_lines number)
              else Buffer.add_string buffer text
            end
        )
        doc
      ;
      Hashtbl.reset long_lines;
      Buffer.contents buffer

    let to_string value (eof : [ `EOF ] Token.token) =
      let eof = Trivia.info (eof :> Token.any_token) in
      (* The comments on the last line of code, at its end. *)
      let eof_line_end =
        List.fold_left
          (fun doc comment -> doc ^^ space ^^ verbatim comment)
          none eof.previous_line
      in
      let value_doc =
        value.line_end ^^ value.leading ^^ value.body ^^ eof_line_end
      in
      let doc =
        if eof.before = [] then value_doc
        else if is_empty value then comments_before eof
        else value_doc ^^ line_break ^^ comments_before eof
      in
      let text = render doc.doc in
      if text = "" then "" else text ^ "\n"
  end
