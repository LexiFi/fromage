(* Lexical edge cases: every token below must match the OCaml lexer. *)
(** A doc comment with a "string *)" and a quoted {|string *)|} inside. *)
(**)
(***)
(*)
  comment starting with a parenthesis star *)
(* a char in a comment: '"' and an escaped one '\"' *)
let ( >>= ) x f = f x

let ( let* ) x f = f x
let ( and* ) x y = (x, y)
let ( .%() ) a i = a.(i)
let ( .%()<- ) a i v = a.(i) <- v
let ( ~- ) x = x
let ( !! ) x = x
let ( ## ) x y = x y
let ( **. ) x y = x ** y
let ( +! ) x y = x + y
let ( land ) = ( land )
let f ~label ?opt () = ignore opt; label
let _ = f ~label:1 ?opt:(Some 2) ()
let chars = [ 'a'; '\n'; '\\'; '\''; '"'; '\065'; '\o101'; '\x41'; ' '; '\ ' ]
let strings =
  [ "escapes: \n \t \b \r \\ \" \' \065 \o101 \x41 \u{1F600}";
    "continued \
     line"
  ;
    {|quoted "string" with \n|}; {tag|quoted |} with a fake delimiter|tag}
  ]

let numbers =
  ( 1, 1_000, 0x1F, 0o17, 0b101, 1l, 2L, 3n,
    1.5, 1e10, 1.5e-3, 0x1p3, 0x1.8p-2, 1.
  )

let _ = [%ext.name "payload"] [@attr] [@@item_attr]
[@@@floating]
let _ = {%ext|quoted extension|}
let _ = {%ext.id tag|quoted extension|tag}
{%%item_ext|quoted item|}
let \#effect = 1
let _ = \#effect
type variants = [< `A | `B > `A ] * [> `A ] * < x : int >
let _ =
  object
    val x = 1
    method copy = {< x = 2 >}
  end

let _ = 1 :: []; ()
;;
# 42 "renamed.ml"
let after_directive = 0
let _ = x#method_call
let _ = 3 lsl 2 lor 1 asr 1 lxor 2 mod 3 land 1
