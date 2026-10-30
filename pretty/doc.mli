(** Documents of pretty_expressive which print the tokens of the CST, with
    their comments, in layouts which follow the rules of ocst/HACKING.md, for
    the width of the page given to [Make].

    {1 Spaces, line breaks and layouts}

    The combinators join documents with three kinds of separators:
    - a {e space}, printed as one space, except at the end of a line, where
      it is dropped;
    - no space ([( ++ )], when the tokens around allow it);
    - a {e line break}: a newline, followed by the indentation of the
      current level. The level is set by [nest], [( +/ )] and [below],
      relative to the level of the enclosing document, not to the column
      where it starts.

    The separators of the {e concatenation} combinators ([( +- )],
    [( ++ )], [( +| )], [( +/ )], [below], [words], [vertical]) are
    {e fixed}: they are always the same. The {e layout} combinators ([lines],
    [after], [hanging]...) give a choice between several layouts, with
    different separators: their separators are {e breakable}.
    pretty_expressive chooses, for the whole file, the layouts with the
    fewest lines that do not exceed the width of the page.

    Comments add line breaks, whatever the separators: a comment which was on
    its own line in the source stays on its own line, and blank lines are
    kept. Such comments and blank lines before the first token of a document
    are printed before it, whatever its layout: they do not prevent the rest
    of the document from fitting on one line ([flat]), but they prevent it
    from following another document on its line ([inline]). *)

open Ocst

(** The width of the page. *)
module type CONFIG = sig val page_width : int end

(** The documents for a width. Their [tok] prints any token: the printer
    uses the one of [Syntax_layouts] instead, which leaves the tokens of the
    constructs shared by several parts of the grammar to its layouts. *)
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
    (** [left right], with a space between them; or with a line break when
      [right] starts with comments on lines of their own, which would
      otherwise end the line of [left]. *)

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
      multi-line value is alone on its line, except after the [)], [end] or
      [done] which close a [closed] value, and already start the last line of
      the value: [end;], [);], [done;]. [in_bracket]: [value] is an element
      of a bracket, and the separator alone on its line is in the column of
      the bracket, 2 columns before the elements. *)

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

(** The documents for a width: pretty_expressive takes the width in its cost
    factory, a parameter of its functor [Printer.Make]. *)
module Make (_ : CONFIG) : S
