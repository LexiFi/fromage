(** The configuration of a file, found as ocamlformat finds it: the
    [.ocamlformat] files of its directory and of the directories above it, up
    to the root of the project (the nearest one with a [.git], a [.hg] or a
    [dune-project], or [root]), the deeper ones overriding the others. Of
    their options, only [margin] and [disable] are read; the others are
    ignored.

    A file is formatted unless it is listed in a [.ocamlformat-ignore] of
    these directories; with [disable = true], only if it is listed in a
    [.ocamlformat-enable]. A listing file has one file per line, relative to
    its directory, where [*] and [?] match any characters but [/]; empty
    lines and lines starting with [#] are ignored.

    Unlike ocamlformat, a file is formatted without a [.ocamlformat] in its
    project. With [enable_outside_detected_project], the configuration files
    above the root of the project are read too. *)

type t =
  | Format of { margin : int option }
  (** The file is formatted, with the [margin] of the configuration. *)
  | Leave (** The file is not formatted. *)

val find : ?root:string -> enable_outside_detected_project:bool -> string -> t
(** [find ?root ~enable_outside_detected_project filename]: the configuration
    of [filename]. Raises [Failure] when the value of [margin] or [disable]
    is invalid. *)
