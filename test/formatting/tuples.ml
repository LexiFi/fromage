(* The elements of a tuple are packed: as many per line as fit, an element on
   several lines on lines of its own. *)

let tuple =
  ( first_element, second_element, third_element, fourth_element,
    fifth_element, sixth_element, seventh_element
  )

let f =
  function
  | Exp_for
      ( for_, ext, index, equal, start, direction, stop,
        { left = do_;

          content = body;

          right = done_
        }
      )
  -> body
  | _ -> ()
