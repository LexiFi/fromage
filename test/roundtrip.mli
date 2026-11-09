(** The checks of OCST against the compiler, which the tests run on all their
    sources (the files of the repository, the generated sentences...): a
    source is lexed and parsed by OCST, printed back, and converted to a
    Parsetree, which must be equal to the compiler's, with the same warnings
    about doc comments (warning 50). *)

val git_root : unit -> string
(** The root of the repository: dune's source tree when a dune rule runs the
    tests, else the root of the Git repository of the current directory. *)

val timed : string -> (unit -> 'a) -> 'a
(** [timed phase f]: [f ()], whose time is added to [phase] (for
    [--timings]). *)

val print_timings : unit -> unit
(** The time spent in each phase, the longest first, on the error output. *)

val docstring_warnings : (unit -> 'a) -> 'a * (Location.t * Warnings.t) list
(** The result of a parser, with the warnings about doc comments (warning 50)
    which it emits, in order. *)

val describe_warning : Location.t * Warnings.t -> string
(** [line 1, characters 8-25: unattached doc comment]. *)

val compiler_error :
  interface:bool -> string -> string -> (Location.t * string) option
(** [compiler_error ~interface filename source]: the location and the message
    of the error of the compiler's parser on [source], if any. *)

type formatter =
  { format_implementation : Ocst.Cst.implementation -> string;
    format_interface : Ocst.Cst.interface -> string;
  }

val formatter : formatter option ref
(** A formatter, which the checks below also check, when it is set: the
    formatted source must have the same tokens and comments as the original
    one. The tests set it by linking the library [pretty_check]. *)

val check : ?name:string -> string -> string -> bool
(** [check ?name filename source]: the checks of OCST on [source], an
    interface if [filename] ends with [.mli], else an implementation. The
    lexer round-trip ([Lexer_check]); then OCST and the compiler accept or
    reject the source alike, OCST failing with a syntax error, never with an
    internal error or another exception; when they accept it, the CST prints
    back to the source, its AST is the compiler's, with the same warnings
    about doc comments, and the formatter, if set, keeps it. [filename] is
    used in locations, [name] (by default [filename]) in error messages.
    Raises an exception when a check fails (see [failure]). Returns whether
    both parsers accept the source. *)

type failure = { message : string; details : string Lazy.t option }

val failure : name:string -> (unit -> unit) -> failure option
(** Runs a check, and returns its failure, if any. Unexpected exceptions are
    reported for [name]. *)

val report : title:string -> failure list -> unit
(** Prints the failures: the details of the first one, and one line for each
    of the others. *)
