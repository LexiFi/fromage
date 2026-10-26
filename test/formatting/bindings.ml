(* A binding whose expression is on several lines has a line break after [=],
   even for [function] and [fun]; [struct], [sig] and [object], which end
   with a keyword, start on the line of [=]. *)

let f =
  function
  | Aaaaaaaaaa -> aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
  | Bbbbbbbbbb -> bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb

let g x =
  function
  | Aaaaaaaaaa -> x
  | Bbbbbbbbbb -> x

let h =
  fun x ->
    aaaaaaaaaaaaaaaaaaaaaaaaaaaa bbbbbbbbbbbbbbbbbbbbbbbbbbbbbb ccccccccccccc x

let i = fun x -> x

let record = { field = (fun x -> x); other = 1 }

let record =
  { field =
      function
      | Aaaaaaaaaa -> aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
      | Bbbbbbbbbb -> bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb;
  }

module M =
  struct
    let x = 1
    let y = 2
  end

(* A pattern stays on one line when it can: lines of more even lengths do not
   make up for a bracket broken after [let]. *)
let location spans index =
  let { Token_position.start; stop } =
    spans.(min index (Array.length spans - 1))
  in
  start, stop
