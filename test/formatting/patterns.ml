(* Patterns which are not in the other tests: exceptions, [lazy], ranges,
   arrays, aliases inside tuples and records, constants. *)

(* 1. Exception patterns. *)
let f1 =
  function
    exception Not_found -> 0
  | exception (Failure message_with_a_long_name) ->
      String.length message_with_a_long_name
  | x -> x

(* 2. [lazy] patterns. *)
let f2 (lazy x) = x
let f2_long
    ( lazy
        ( Constructor
            ( aaaaaaaaaaaaaaaa, bbbbbbbbbbbbbbbbbbbbbbbbb,
              ccccccccccccccccccccccc, dddddddddddddddddd
            )
        )
    )
  = aaaaaaaaaaaaaaaa

(* 3. Ranges and constants. *)
let f3 =
  function
    'a' .. 'z' | 'A' .. 'Z' -> 1
  | '0' .. '9' -> 2
  | -1 -> 3
  | 1.5 -> 4
  | "string" -> 5
  | _ -> 6

(* 4. Array patterns. *)
let f4 =
  function
    [||] -> 0
  | [| x |] -> x
  | [|
      aaaaaaaaaaaaaaaa; bbbbbbbbbbbbbbbbbbbbbbbbb;
      ccccccccccccccccccccccc; dddddddddddddddddd
    |]
  -> 4

(* 5. Aliases. *)
let f5
    ( (Aaaaaaaaaaaaaaaa x as first),
      (Bbbbbbbbbbbbbbbbbbbbbbbbb y as second), (Ccccccccccccc z as third)
    )
  = x

let f5_record
    { field_number_one = (Some x as first);
      field_number_two = (None | Some _) as second; _
    }
  = x

(* 6. Or-patterns in tuples and constructors. *)
let f6 =
  function
    (A | B), (C | D) -> 1
  | Some
      ( Aaaaaaaaaaaaaaaaaaaa | Bbbbbbbbbbbbbbbbbbbbbbbbbb
      | Cccccccccccccccccccccc | Ddddddd
      )
  -> 2
  | _ -> 3

(* 7. Polymorphic variants. *)
let f7 =
  function
    `A | `B x -> 1
  | #tags_with_a_long_name as tag -> 2
  | `Constructor
      (aaaaaaaaaaaaaaaa, bbbbbbbbbbbbbbbbbbbbbbbbb, ccccccccccccccccccccccc)
  -> 3
