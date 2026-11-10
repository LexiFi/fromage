(* The configuration of a file, as ocamlformat finds it (see config.mli). *)

type t = Format of { margin : int option } | Leave

(** [path], absolute, without [.] and [..]. *)
let absolute path =
  let path =
    if Filename.is_relative path then Filename.concat (Sys.getcwd ()) path
    else path
  in
  let segments =
    List.fold_left
      ( fun segments segment ->
          match segment, segments with
          | ("" | "."), _ -> segments
          | "..", _ :: parents -> parents
          | "..", [] -> []
          | segment, _ -> segment :: segments
      )
      [] (String.split_on_char '/' path)
  in
  "/" ^ String.concat "/" (List.rev segments)

let is_project_root ~root directory =
  match root with
  | Some root -> directory = root
  | None ->
      List.exists
        (fun witness -> Sys.file_exists (Filename.concat directory witness))
        [ ".git"; ".hg"; "dune-project" ]

(** The directories whose configuration applies to [file], the deepest
    first: up to the root of the project, or of the file system. *)
let directories ~root ~enable_outside_detected_project file =
  let rec up directory =
    if
        directory = Filename.dirname directory
      ||
        (is_project_root ~root directory && not enable_outside_detected_project)
    then [ directory ]
    else directory :: up (Filename.dirname directory)
  in
  up (Filename.dirname file)

(** The options of a [.ocamlformat] file, as pairs of a name and a value:
    lines [name = value], without the empty lines and the comments. *)
let options file =
  List.filter_map
    ( fun line ->
        let line = String.trim line in
        if line = "" || line.[0] = '#' then None
        else
          match String.index_opt line '=' with
          | Some index ->
              Some
                ( String.trim (String.sub line 0 index),
                  ( String.trim
                      ( String.sub
                          line (index + 1) (String.length line - index - 1)
                      )
                    ,
                    file
                  )
                )
          | None -> None
    )
    (In_channel.with_open_bin file In_channel.input_lines)

(** Whether [text] matches [pattern], where [*] and [?] match any characters
    but [/]. *)
let matches pattern text =
  let rec from p t =
    if p = String.length pattern then t = String.length text
    else
      match pattern.[p] with
      | '*' ->
            from (p + 1) t
          || (t < String.length text && text.[t] <> '/' && from p (t + 1))
      | '?' -> t < String.length text && text.[t] <> '/' && from (p + 1) (t + 1)
      | c -> t < String.length text && text.[t] = c && from (p + 1) (t + 1)
  in
  from 0 0

(** Whether [file] is listed in the listing file [listing]. *)
let listed listing file =
  let directory = Filename.dirname listing in
  List.exists
    ( fun line ->
        let line = String.trim line in
          line <> "" && line.[0] <> '#'
        && matches (absolute (Filename.concat directory line)) file
    )
    (In_channel.with_open_bin listing In_channel.input_lines)

let find ?root ~enable_outside_detected_project filename =
  let file = absolute filename
  and root = Option.map absolute root
  in
  let directories = directories ~root ~enable_outside_detected_project file in
  (* The files [name] of the directories, the deepest first. *)
  let existing name =
    List.filter
      Sys.file_exists
      (List.map (fun directory -> Filename.concat directory name) directories)
  in
  (* The options of the deeper files override the others. *)
  let options = List.concat_map options (existing ".ocamlformat") in
  let margin =
    Option.map
      ( fun (value, file) ->
          match int_of_string_opt value with
          | Some margin when margin > 0 -> margin
          | _ -> failwith (Printf.sprintf "%s: invalid margin: %S" file value)
      )
      (List.assoc_opt "margin" options)
  and disable =
    match List.assoc_opt "disable" options with
    | None | Some ("false", _) -> false
    | Some ("true", _) -> true
    | Some (value, file) ->
        failwith (Printf.sprintf "%s: invalid disable: %S" file value)
  in
  let listed_in name =
    List.exists (fun listing -> listed listing file) (existing name)
  in
  if
    if disable then listed_in ".ocamlformat-enable"
    else not (listed_in ".ocamlformat-ignore")
  then Format { margin }
  else Leave
