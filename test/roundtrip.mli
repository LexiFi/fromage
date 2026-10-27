(** Round-trip checks of OCST against the compiler: a source is lexed and
    parsed by OCST, printed back, and converted to a Parsetree, which must be
    equal to the compiler's, with the same warnings about doc comments
    (warning 50). *)

val git_root : unit -> string
(** The root of the repository: dune's source tree when a dune rule runs the
    tests, else the root of the Git repository of the current directory. *)

val timed : string -> (unit -> 'a) -> 'a
(** [timed phase f]: [f ()], whose time is added to [phase] (for
    [--timings]). *)

val print_timings : unit -> unit
(** The time spent in each phase, the longest first, on the error output. *)

exception Mismatch of string * string Lazy.t
(** An AST mismatch: a one-line message, and details on the first difference. *)

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

val check_implementation : ?name:string -> string -> string -> bool
(** [check_implementation ?name filename source] checks an implementation.
    [filename] is used in locations, [name] (by default [filename]) in error
    messages. Raises [Failure] or [Mismatch] when the check fails. Sources that
    both parsers reject pass, unless OCST fails with an internal error or an
    exception other than a syntax error. Returns whether both parsers accept
    the source. *)

val check_interface : ?name:string -> string -> string -> bool
(** Same as [check_implementation], for an interface. *)

type failure = { message : string; details : string Lazy.t option }

val failure : name:string -> (unit -> unit) -> failure option
(** Runs a check, and returns its failure, if any. Unexpected exceptions are
    reported for [name]. *)

val report : title:string -> failure list -> unit
(** Prints the failures: the details of the first one, and one line for each
    of the others. *)
