(* The points which no test covers, from the HTML report of bisect_ppx
   (argument 1: its directory): the lines which have some, with [▲] before
   each of them: [file:line: text]. The copies of the sources start with a line directive,
   which gives the file and the lines of the original. *)

let read_file filename = In_channel.with_open_bin filename In_channel.input_all

let unescape text =
  List.fold_left
    ( fun text (entity, character) ->
        String.concat
          character (Str.split_delim (Str.regexp_string entity) text)
    )
    text
    [ "&gt;", ">"; "&lt;", "<"; "&quot;", "\""; "&#39;", "'"; "&amp;", "&" ]

(* The line, with [▲] before each uncovered point, if it has some. *)
let marked line =
  let point = Str.regexp {|<span data-count="\([0-9]+\)">|} in
  let uncovered = ref false in
  let line =
    Str.global_substitute
      point
      ( fun line ->
          if
            int_of_string (Str.matched_group 1 line) = 0
          then begin uncovered := true; "\xe2\x96\xb2" end
          else ""
      )
      line
  in
  if !uncovered then
    Some
      ( String.trim
          (unescape (Str.global_replace (Str.regexp "<[^>]*>") "" line))
      )
  else None

let () =
  let directory = Sys.argv.(1) in
  let directive = Str.regexp {|# 1 "\([^"]*\)"|} in
  Sys.readdir directory |> Array.to_list |> List.sort compare
  |> List.filter (fun name -> Filename.check_suffix name ".html")
  |>
  List.iter
    ( fun name ->
        let lines =
          String.split_on_char '\n' (read_file (Filename.concat directory name))
        in
        (* The source starts after [<code class="ocaml">], with the directive. *)
        let rec source =
          function
          | line :: rest when
              Str.string_match (Str.regexp {|.*<code class="ocaml">|}) line 0
          ->
              let first =
                Str.replace_first
                  (Str.regexp {|.*<code class="ocaml">|}) "" line
              in
              first :: rest
          | _ :: rest -> source rest
          | [] -> []
        in
        match source lines with
        | first :: rest when Str.string_match directive first 0 ->
            let file = Filename.basename (Str.matched_group 1 first) in
            List.iteri
              ( fun index line ->
                  Option.iter
                    ( fun text ->
                        Printf.printf "%s:%d: %s\n" file (index + 1) text
                    )
                    (marked line)
              )
              rest
        | _ -> ()
    )
