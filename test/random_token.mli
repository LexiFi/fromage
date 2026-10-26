(** Tokens of every kind, to generate random token sequences. *)

val token : string -> Ocst.Token.any_kind -> Ocst.Token.any_token
(** [token text kind] is the token of [kind], without trivia, after checking
    that its text is [text]. *)

val keywords : Ocst.Token.any_token list
(** The keywords, from [and] to [with]. *)

val symbols : Ocst.Token.any_token list
(** The punctuation and the operators, from [(] to [let*]. *)

val literals : Ocst.Token.any_token list
(** The identifiers, labels, literals, quoted extensions and doc comments. *)

val all : Ocst.Token.any_token array
(** The tokens of [keywords], [symbols] and [literals]: one or more token of
    each kind except [EOF] and [ERROR]. *)

val random : Random.State.t -> Ocst.Token.any_token
(** A token of [all]. *)
