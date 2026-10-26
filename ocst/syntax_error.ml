type t =
  { id : string; location : Location.t; message : string;
    notes : (Location.t * string) list;
  }

exception Error of t

(* [Location.print_loc], without the source excerpt. *)
let location_text (loc : Location.t) =
  let start = loc.loc_start
  and stop = loc.loc_end
  in
  let start_column = start.pos_cnum - start.pos_bol in
  let stop_column = stop.pos_cnum - stop.pos_bol in
  if start.pos_lnum = stop.pos_lnum then
    Printf.sprintf
      "File %S, line %d, characters %d-%d:" start.pos_fname
      start.pos_lnum start_column stop_column
  else
    Printf.sprintf
      "File %S, lines %d-%d, characters %d-%d:" start.pos_fname
      start.pos_lnum stop.pos_lnum start_column stop_column

let to_string { location; message; notes; _ } =
  String.concat
    "\n"
    ( (location_text location ^ "\nError: " ^ message)
      :: List.map (fun (loc, note) -> location_text loc ^ "\n  " ^ note) notes
    )
