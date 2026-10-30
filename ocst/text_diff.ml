(* The differences of two texts, line by line: see text_diff.mli. *)

let diff ?(context = 3) before after =
  let before = Array.of_list (String.split_on_char '\n' before)
  and after = Array.of_list (String.split_on_char '\n' after)
  in
  (* The lines common to the start and to the end are kept as they are, so
     that only the middle goes through the longest common subsequence. *)
  let rec prefix index =
    if
      index < Array.length before && index < Array.length after
      && before.(index) = after.(index)
    then prefix (index + 1)
    else index
  in
  let start = prefix 0 in
  let rec suffix count =
    if
      count < Array.length before - start && count < Array.length after - start
      &&
      before.(Array.length before - 1 - count)
      = after.(Array.length after - 1 - count)
    then suffix (count + 1)
    else count
  in
  let stop = suffix 0 in
  let removed = Array.sub before start (Array.length before - start - stop)
  and added = Array.sub after start (Array.length after - start - stop)
  in
  let n = Array.length removed
  and m = Array.length added
  in
  (* [common.(i).(j)]: the length of the longest common subsequence of the
     lines of [removed] from [i] and of [added] from [j]. *)
  let common = Array.make_matrix (n + 1) (m + 1) 0 in
  for i = n - 1 downto 0 do
    for j = m - 1 downto 0 do
      common.(i).(j)
      <-
      ( if removed.(i) = added.(j) then common.(i + 1).(j + 1) + 1
        else max common.(i + 1).(j) common.(i).(j + 1)
      )
    done
  done;
  (* The lines with their changes, ['-'] removed, ['+'] added, [' ']
     kept. *)
  let rec middle i j =
    if i < n && j < m && removed.(i) = added.(j) then
      (' ', removed.(i)) :: middle (i + 1) (j + 1)
    else if i < n && (j = m || common.(i + 1).(j) >= common.(i).(j + 1)) then
      ('-', removed.(i)) :: middle (i + 1) j
    else if j < m then ('+', added.(j)) :: middle i (j + 1)
    else []
  in
  let lines =
    Array.of_list
      ( List.map
          (fun line -> ' ', line) (Array.to_list (Array.sub before 0 start))
        @ middle 0 0
        @
        List.map
          (fun line -> ' ', line)
          (Array.to_list (Array.sub before (Array.length before - stop) stop))
      )
  in
  (* The changed lines and [context] lines around them, with [...] where
     lines are left out. *)
  let shown = Array.make (Array.length lines) false in
  Array.iteri
    ( fun index (kind, _) ->
        if kind <> ' ' then
          for
            near = max 0 (index - context)
            to min (Array.length lines - 1) (index + context)
          do
            shown.(near) <- true
          done
    )
    lines
  ;
  let buffer = Buffer.create 256 in
  Array.iteri
    ( fun index (kind, line) ->
        if shown.(index) then begin if index > 0 && not shown.(index - 1) then
            Buffer.add_string buffer "...\n"
          ;
          Buffer.add_char buffer kind;
          Buffer.add_char buffer ' ';
          Buffer.add_string buffer line;
          Buffer.add_char buffer '\n'
        end
    )
    lines
  ;
  Buffer.contents buffer
