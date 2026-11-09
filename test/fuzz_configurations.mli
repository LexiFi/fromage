(** The configurations of the fuzzing: the sentences which ocamlgrammarfuzzer
    generates from the grammar of the compiler, with the mutants of each one,
    and the dune rules which generate and check them. It does not depend on
    OCST, so that a change of OCST does not generate the sentences again. *)

val grammar : menhir:string -> string -> string
(** [grammar ~menhir source]: the grammar [source] (parser.mly) compiled by
    Menhir, as a .cmly file, removed at exit. *)

val fuzzer_output : fuzzer:string -> cmly:string -> string list -> string
(** The output of ocamlgrammarfuzzer on the grammar [cmly], with the given
    arguments: its sentences, each followed by \000. *)

val sentences_of_output : string -> string list
(** The sentences of an output of [fuzzer_output]. *)

(** The mutants of each sentence, besides the usual ones (see
    driver.ml). *)
type extra =
  | No_extra | Every_token | Mutants of int
  | No_mutants (** No mutant at all, not even the usual ones. *)
  | Doc_comments of { all : bool }
  (** Only the mutants with doc comments: with every separator ([all]), or
      with random separators. *)

(** A configuration of the fuzzer: its entry point ([implementation] or
    [interface]), its arguments, and the mutants of each sentence. *)
type configuration =
  { entrypoint : string; arguments : string list; extra : extra }

(** A configuration of the deep fuzzing, too long for [runtest-full], checked
    in [parts] rules. *)
type deep = { name : string; parts : int; configuration : configuration }

val deep_configurations : deep list

val selected :
  quick:bool -> entrypoints:string list -> extra:extra option
  -> index:int option -> deep:string option -> string list
  -> configuration list
(** [selected ~quick ~entrypoints ~extra ~index ~deep explicit]: the
    configurations which the command line selects: those of [runtest]
    ([quick]), or all of them, of [entrypoints] (all of them when empty),
    with the mutants of [extra] instead of theirs; or the one of [explicit],
    [ENTRYPOINT FUZZER-ARGUMENT...]. Then the [index]th one, or the deep
    fuzzing [deep]. *)

val dune_rules : string -> string
(** [dune_rules directory]: the rules of [directory]/fuzz.inc, [test] or
    [coverage]. *)
