type t = Token.any_token list

exception Missing_trivia of
  { previous : Token.any_token; next : Token.any_token; }

exception Invalid_token of Token.any_token

let empty = []
let tokens stream = List.rev stream

let token_text token =
    String.concat "" (List.map Token.trivia_text token.Token.leading)
  ^ Token.text token

let source stream = String.concat "" (List.map token_text (tokens stream))

let same_kind left right =
  match left, right with
  | `ERROR left, `ERROR right ->
        left.Token.reason = right.Token.reason
      && left.Token.text = right.Token.text
  | _ -> left = right

let same_token left right =
    left.Token.leading = right.Token.leading
  && Token.text left = Token.text right && same_kind left.kind right.kind

let is_lexed_as stream =
  let expected = tokens stream @ [ Token.make `EOF ] in
  let actual = Cst_lexer.tokenize (source stream) in
  try List.for_all2 same_token expected actual with
    Invalid_argument _ -> false

let can_insert stream next = is_lexed_as (next :: stream)

let insert stream next =
  match stream with
  | [] when is_lexed_as [ next ] -> [ next ]
  | [] -> raise (Invalid_token next)
  | _ :: _ when can_insert stream next -> next :: stream
  | previous :: _ -> raise (Missing_trivia { previous; next })

(* Separators tried in turn when two tokens cannot be adjacent. A single space
   is not always enough: [' '] is a character literal, not two quotes. *)
let separators = [ " "; "  " ]

let with_separator separate stream =
  let rec loop =
    function
    | [] -> assert false
    | [ separator ] -> separate stream separator
    | separator :: others ->
        try separate stream separator with Missing_trivia _ -> loop others
  in
  loop separators

let insert_create_trivia stream next =
  match stream with
  | [] when is_lexed_as [ next ] -> [ next ]
  | [] -> raise (Invalid_token next)
  | _ when can_insert stream next -> next :: stream
  | _ ->
      with_separator
        ( fun stream separator ->
            insert
              stream
              { next with Token.leading = [ Token.Whitespace separator ] }
        )
        stream

let insert_group stream group =
  match group with
  | [] -> stream
  | first :: _ ->
      let stream' = List.rev_append group stream in
      if is_lexed_as stream' then stream'
      else if stream = [] then raise (Invalid_token first)
      else
        match stream with
        | previous :: _ -> raise (Missing_trivia { previous; next = first })
        | [] -> assert false

let insert_group_create_trivia stream group =
  match group with
  | [] -> stream
  | first :: rest ->
      try insert_group stream group with
      | Missing_trivia _ ->
          with_separator
            ( fun stream separator ->
                insert_group
                  stream
                  ( { first with Token.leading = [ Token.Whitespace separator ]
                    }
                  :: rest
                  )
            )
            stream
