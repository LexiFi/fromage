let f
    xxx xxxxxxxxx yyyy zzzzzzzz vvvvvvvvv gggggg hhhhhhhhh (aaaaaaaaa, bbbbbbb)
  : t * ttttttttt * int -> tttttt -> tttt
  =
    f
      xxxxxxxx xxxxxxxxxxxx xxxxxxxxxxxxx xxxxxxxxxxx xxxxxxxxxxx
      (g fffffffffff ffffffffffff ttttttttt gggggggg ggggggg ggggggggg)

let f
    xxx xxxxxxxxx yyyy zzzzzzzz vvvvvvvvv gggggg hhhhhhhhh (aaaaaaaaa, bbbbbbb)
    (aaaaaaaaa, bbbbbbb) (aaaaaaaaa, bbbbbbb) (aaaaaaaaa, bbbbbbb)
  =
    f
      xxxxxxxx xxxxxxxxxxxx xxxxxxxxxxxxx xxxxxxxxxxx xxxxxxxxxxx
      (g fffffffffff ffffffffffff ttttttttt gggggggg ggggggg ggggggggg)

let f xxx xxxxxxxxx yyyy zzzzzzzz vvvvvvvvv (aaaaaaaaa, bbbbbbb) =
  f
    xxxxxxxx xxxxxxxxxxxx xxxxxxxxxxxxx xxxxxxxxxxx xxxxxxxxxxx
    (g fffffffffff ffffffffffff ttttttttt gggggggg ggggggg ggggggggg)

let f =
  fun
    xxx xxxxxxxxx yyyy zzzzzzzz vvvvvvvvv gggggg
    gggggg gggggg gggggg gggggg gggggg hhhhhhhhh
    ( aaaaaaaaa, aaaaaaaaa, aaaaaaaaa, aaaaaaaaa,
      aaaaaaaaa, aaaaaaaaa, bbbbbbb, bbbbbbb
    )
    gggggg gggggg gggggg gggggg gggggg hhhhhhhhh
  ->
    f
      xxxxxxxx xxxxxxxxxxxx xxxxxxxxxxxxx xxxxxxxxxxx xxxxxxxxxxx
      (g fffffffffff ffffffffffff ttttttttt gggggggg ggggggg ggggggggg)

let f
    xxx xxxxxxxxx yyyy zzzzzzzz vvvvvvvvv gggggg hhhhhhhhh (aaaaaaaaa, bbbbbbb)
    (aaaaaaaaa, bbbbbbb) (aaaaaaaaa, bbbbbbb) (aaaaaaaaa, bbbbbbb)
  =
    let f
        xxx xxxxxxxxx yyyy zzzzzzzz vvvvvvvvv gggggg
        hhhhhhhhh (aaaaaaaaa, bbbbbbb) (aaaaaaaaa, bbbbbbb)
        (aaaaaaaaa, bbbbbbb) (aaaaaaaaa, bbbbbbb)
      =
        f
          xxxxxxxx xxxxxxxxxxxx xxxxxxxxxxxxx xxxxxxxxxxx xxxxxxxxxxx
          (g fffffffffff ffffffffffff ttttttttt gggggggg ggggggg ggggggggg)
    in
    let f =
      fun
        xxx xxxxxxxxx yyyy zzzzzzzz vvvvvvvvv gggggg
        gggggg gggggg gggggg gggggg gggggg hhhhhhhhh
        ( aaaaaaaaa, aaaaaaaaa, aaaaaaaaa,
          aaaaaaaaa, aaaaaaaaa, aaaaaaaaa, bbbbbbb
        )
      ->
        f
          xxxxxxxx xxxxxxxxxxxx xxxxxxxxxxxxx xxxxxxxxxxx xxxxxxxxxxx
          (g fffffffffff ffffffffffff ttttttttt gggggggg ggggggg ggggggggg)
        ;
        f
          xxxxxxxx xxxxxxxxxxxx xxxxxxxxxxxxx xxxxxxxxxxx xxxxxxxxxxx
          (g fffffffffff ffffffffffff ttttttttt gggggggg ggggggg ggggggggg)
    in
    let f =
      List.map
        ( fun
            xxx xxxxxxxxx yyyy zzzzzzzz vvvvvvvvv gggggg
            gggggg gggggg gggggg gggggg gggggg hhhhhhhhh
            ( ( aaaaaaaaa, aaaaaaaaa, aaaaaaaaa,
                aaaaaaaaa, aaaaaaaaa, aaaaaaaaa, bbbbbbb
              )
            )
          ->
            f
              xxxxxxxx xxxxxxxxxxxx xxxxxxxxxxxxx xxxxxxxxxxx xxxxxxxxxxx
              (g fffffffffff ffffffffffff ttttttttt gggggggg ggggggg ggggggggg)
        )
        li
    in
    let f =
      List.map
        begin fun
            xxx xxxxxxxxx yyyy zzzzzzzz vvvvvvvvv gggggg
            gggggg gggggg gggggg gggggg gggggg hhhhhhhhh
            ( aaaaaaaaa, aaaaaaaaa, aaaaaaaaa,
              aaaaaaaaa, aaaaaaaaa, aaaaaaaaa, bbbbbbb
            )
          ->
            f
              xxxxxxxx xxxxxxxxxxxx xxxxxxxxxxxxx xxxxxxxxxxx xxxxxxxxxxx
              (g fffffffffff ffffffffffff ttttttttt gggggggg ggggggg ggggggggg)
        end
        li
    in
    let f =
      List.map
        begin fun
            xxx
            ( aaaaaaaaa, aaaaaaaaa, aaaaaaaaa,
              aaaaaaaaa, aaaaaaaaa, aaaaaaaaa, bbbbbbb
            )
          -> f xxxxxxxx xxxxxxxxxxxx xxxxxxxxxxxxx xxxxxxxxxxx xxxxxxxxxxx
        end
        ( fun
            xxx
            ( aaaaaaaaa, aaaaaaaaa, aaaaaaaaa,
              aaaaaaaaa, aaaaaaaaa, aaaaaaaaa, bbbbbbb
            )
          -> f xxxxxxxx xxxxxxxxxxxx xxxxxxxxxxxxx xxxxxxxxxxx xxxxxxxxxxx
        )
        li
    in
    let f =
      List.map
        begin fun xxx xxxxxxxxx yyyy zzzzzzzz vvvvvvvvv gggggg gggggg ->
            f
              xxxxxxxx xxxxxxxxxxxx xxxxxxxxxxxxx xxxxxxxxxxx xxxxxxxxxxx
              (g fffffffffff ffffffffffff ttttttttt gggggggg ggggggg ggggggggg)
        end
        li
    in
    f
      xxxxxxxx xxxxxxxxxxxx xxxxxxxxxxxxx xxxxxxxxxxx xxxxxxxxxxx
      (g fffffffffff ffffffffffff ttttttttt gggggggg ggggggg ggggggggg)

let f x = match x with | Pattern -> value

let f x =
  match x with
  | Pattern -> value
  | Pattern -> value
  | Pattern -> value
  | Pattern -> value
  | Pattern -> value
  | Pattern -> value
  | Pattern -> value
  | Pattern -> value
  | Pattern ->
      f
        xxxxxxxx xxxxxxxxxxxx xxxxxxxxxxxxx xxxxxxxxxxx xxxxxxxxxxx
        (g fffffffffff ffffffffffff ttttttttt gggggggg ggggggg ggggggggg)
  | Pattern | Pattern | Pattern -> value
  (* A multiline pattern is started by its | and ended by ->, so they must be on the same indentation. The other | are internal and have to be indented. *)
  | Pattern | Pattern | Pattern | Pattern | Pattern | Pattern | Pattern
    | Pattern | Pattern | Pattern | Pattern | Pattern | Pattern | Pattern
  -> value
  | Pattern | Pattern | Pattern ->
      f
        xxxxxxxx xxxxxxxxxxxx xxxxxxxxxxxxx xxxxxxxxxxx xxxxxxxxxxx
        (g fffffffffff ffffffffffff ttttttttt gggggggg ggggggg ggggggggg)
  | Pattern | Pattern | Pattern | Pattern | Pattern | Pattern | Pattern
    | Pattern | Pattern | Pattern | Pattern | Pattern | Pattern | Pattern
  ->
      f
        xxxxxxxx xxxxxxxxxxxx xxxxxxxxxxxxx xxxxxxxxxxx xxxxxxxxxxx
        (g fffffffffff ffffffffffff ttttttttt gggggggg ggggggg ggggggggg)

  | Pattern (aaaaaaaaaa, bbbbbbbbb, cccccccccc)
    | Pattern (aaaaaaaaaa, bbbbbbbbb, cccccccccc)
    | Pattern (aaaaaaaaaa, bbbbbbbbb, cccccccccc) | Pattern | Pattern
    | Pattern | Pattern | Pattern | Pattern | Pattern | Pattern | Pattern
    | Record
      { aaaaaaaaaa; bbbbbbbbb; xxxxxxxxxxxxx = Pattern | Pattern | Pattern;
        xxxxxxxxxxxxx = Pattern | Pattern | Pattern;
        xxxxxxxxxxxxx = Pattern | Pattern | Pattern;
        xxxxxxxxxxxxx = Pattern | Pattern | Pattern;
      }
    | Pattern (aaaaaaaaaa, bbbbbbbbb, cccccccccc)
    | Pattern (aaaaaaaaaa, bbbbbbbbb, cccccccccc)
    | Pattern (aaaaaaaaaa, bbbbbbbbb, cccccccccc)
  ->
      f
        xxxxxxxx xxxxxxxxxxxx xxxxxxxxxxxxx xxxxxxxxxxx xxxxxxxxxxx
        (g fffffffffff ffffffffffff ttttttttt gggggggg ggggggg ggggggggg)

let a =
  f
    xxxxxxxx xxxxxxxxxxxx xxxxxxxxxxxxx xxxxxxxxxxx xxxxxxxxxxx
    (g fffffffffff ffffffffffff ttttttttt gggggggg ggggggg ggggggggg) xxxxxxxx
    xxxxxxxxxxxx xxxxxxxxxxxxx xxxxxxxxxxx xxxxx xxxxxxxxxxxx xxxxxxxxxxx

let a =
  f
    xxxxxxxx xxxxxxxxxxxx xxxxxxxxxxxxx xxxxxxxxxxx xxxxxxxxxxx
    ( g
        fffffffffff
        (g fffffffffff ffffffffffff ttttttttt gggggggg ggggggg ggggggggg)
        ffffffffffff ttttttttt
        (g fffffffffff ffffffffffff ttttttttt gggggggg ggggggg ggggggggg)
        gggggggg
        (g fffffffffff ffffffffffff ttttttttt gggggggg ggggggg ggggggggg)
        ggggggg ggggggggg
    )
    xxxxxxxx xxxxxxxxxxxx xxxxxxxxxxxxx
    xxxxxxxxxxx xxxxx xxxxxxxxxxxx xxxxxxxxxxx

let a =
  f
    xxxxxxxx xxxxxxxxxxxx xxxxxxxxxxxxx xxxxxxxxxxx xxxxxxxxxxx
    ( g
        fffffffffff
        (g fffffffffff ffffffffffff ttttttttt gggggggg ggggggg ggggggggg)
        ffffffffffff ttttttttt
        (g fffffffffff ffffffffffff ttttttttt gggggggg ggggggg ggggggggg)
        gggggggg
        (g fffffffffff ffffffffffff ttttttttt gggggggg ggggggg ggggggggg)
        ggggggg ggggggggg
    )
    ( g
        fffffffffff
        (g fffffffffff ffffffffffff ttttttttt gggggggg ggggggg ggggggggg)
        ffffffffffff ttttttttt
        (g fffffffffff ffffffffffff ttttttttt gggggggg ggggggg ggggggggg)
        gggggggg
        (g fffffffffff ffffffffffff ttttttttt gggggggg ggggggg ggggggggg)
        ggggggg ggggggggg
    )
    xxxxxxxx xxxxxxxxxxxx xxxxxxxxxxxxx
    xxxxxxxxxxx xxxxx xxxxxxxxxxxx xxxxxxxxxxx

let a =
    xxxxxxxx
      xxxxxxxxxxxx xxxxxxxxxxxxx xxxxxxxxxxx xxxxx xxxxxxxxxxxx xxxxxxxxxxx
  |>
  xxxxxxxx xxxxxxxxxxxx xxxxxxxxxxxxx xxxxxxxxxxx xxxxx xxxxxxxxxxxx xxxxxxxxxxx
  |>
  xxxxxxxx xxxxxxxxxxxx xxxxxxxxxxxxx xxxxxxxxxxx xxxxx xxxxxxxxxxxx xxxxxxxxxxx
  |>
  xxxxxxxx xxxxxxxxxxxx xxxxxxxxxxxxx xxxxxxxxxxx xxxxx xxxxxxxxxxxx xxxxxxxxxxx
  |>
  xxxxxxxx xxxxxxxxxxxx xxxxxxxxxxxxx xxxxxxxxxxx xxxxx xxxxxxxxxxxx xxxxxxxxxxx
  |>
  xxxxxxxx xxxxxxxxxxxx xxxxxxxxxxxxx xxxxxxxxxxx xxxxx xxxxxxxxxxxx xxxxxxxxxxx

let a =
    xxxxxxxx
      xxxxxxxxxxxx xxxxxxxxxxxxx xxxxxxxxxxx xxxxx xxxxxxxxxxxx xxxxxxxxxxx
  |> xxxxxxxx xxxxxxxxxxxx xxxxxxxxxxxxx xxxxxxxxxxx xxxxx xxxxxxxxxxxx xxxxxxx
  |>
  xxxxxxxx
    xxxxxxxxxxxx xxxxxxxxxxxxx xxxxxxxxxxx xxxxx xxxxxxxxxxxx
    xxxxxxxxxxx xxxxxxxx xxxxxxxxxxxx xxxxxxxxxxxxx xxxxxxxxxxx
    xxxxx xxxxxxxxxxxx xxxxxxxxxxx xxxxxxxx xxxxxxxxxxxx
    xxxxxxxxxxxxx xxxxxxxxxxx xxxxx xxxxxxxxxxxx xxxxxxxxxxx
  |> xxxxxxxx xxxxxxxxxxxx xxxxxxxxxxxxx xxxxxxxxxxx xxxxx xxxxxxxxxxxx xxxxxxx
  |> xxxxxxxx xxxxxxxxxxxx xxxxxxxxxxxxx xxxxxxxxxxx xxxxx xxxxxxxxxxxx xxxxxxxx
  |>
  xxxxxxxx xxxxxxxxxxxx xxxxxxxxxxxxx xxxxxxxxxxx xxxxx xxxxxxxxxxxx xxxxxxxxxx

let a =
    xxxxxxxx
      xxxxxxxxxxxx xxxxxxxxxxxxx xxxxxxxxxxx xxxxx xxxxxxxxxxxx xxxxxxxxxxx
  |>
  xxxxxxxx xxxxxxxxxxxx xxxxxxxxxxxxx xxxxxxxxxxx xxxxx xxxxxxxxxxxx xxxxxxxxxxx
  |>>>>>>>>>>>>>>>>>>=
  xxxxxxxx
    xxxxxxxxxxxx xxxxxxxxxxxxx xxxxxxxxxxx xxxxx xxxxxxxxxxxx
    xxxxxxxxxxx xxxxxxxx xxxxxxxxxxxx xxxxxxxxxxxxx xxxxxxxxxxx
    xxxxx xxxxxxxxxxxx xxxxxxxxxxx xxxxxxxx xxxxxxxxxxxx
    xxxxxxxxxxxxx xxxxxxxxxxx xxxxx xxxxxxxxxxxx xxxxxxxxxxx
  |>
  xxxxxxxx xxxxxxxxxxxx xxxxxxxxxxxxx xxxxxxxxxxx xxxxx xxxxxxxxxxxx xxxxxxxxxxx
  |>
  xxxxxxxx xxxxxxxxxxxx xxxxxxxxxxxxx xxxxxxxxxxx xxxxx xxxxxxxxxxxx xxxxxxxxxxx
  |>
  xxxxxxxx xxxxxxxxxxxx xxxxxxxxxxxxx xxxxxxxxxxx xxxxx xxxxxxxxxxxx xxxxxxxxxxx

let a =
    xxxxxxxx
      xxxxxxxxxxxx xxxxxxxxxxxxx xxxxxxxxxxx xxxxx xxxxxxxxxxxx xxxxxxxxxxx
  |> xxxxxxxx xxxxxxxxxxxx
  |> f xxxxxxxxxxxxx
  |> xxxxxxxxxxx xxxxx xxxxxxxxxxxx xxxxxxxxxxx
  |>>>>>>>>>>>>>>>>>>=
  xxxxxxxx
    xxxxxxxxxxxx xxxxxxxxxxxxx xxxxxxxxxxx xxxxx xxxxxxxxxxxx
    xxxxxxxxxxx xxxxxxxx xxxxxxxxxxxx xxxxxxxxxxxxx xxxxxxxxxxx
    xxxxx xxxxxxxxxxxx xxxxxxxxxxx xxxxxxxx xxxxxxxxxxxx
    xxxxxxxxxxxxx xxxxxxxxxxx xxxxx xxxxxxxxxxxx xxxxxxxxxxx
  |> f y
  |> xxxxxxxx xxxxxxxxxxxx xxxxxxxxxxxxx xxxxxxxxxxx
  |> xxxxx xxxxxxxxxxxx xxxxxxxxxxx
  |> f
  |> g
  |> h
  |> fffff ffffffff
  |>
  xxxxxxxx xxxxxxxxxxxx xxxxxxxxxxxxx xxxxxxxxxxx xxxxx xxxxxxxxxxxx xxxxxxxxxxx
  |>
  xxxxxxxx xxxxxxxxxxxx xxxxxxxxxxxxx xxxxxxxxxxx xxxxx xxxxxxxxxxxx xxxxxxxxxxx

let a =
  xxxxxxxx xxxxxxxxxxxx xxxxxxxxxxxxx xxxxxxxxxxx xxxxx xxxxxxxxxxxx xxxxxxxxxxx
  ;
  xxxxxxxx xxxxxxxxxxxx;
  f xxxxxxxxxxxxx;
  xxxxxxxxxxx xxxxx xxxxxxxxxxxx xxxxxxxxxxx;
  xxxxxxxx
    xxxxxxxxxxxx xxxxxxxxxxxxx xxxxxxxxxxx xxxxx xxxxxxxxxxxx
    xxxxxxxxxxx xxxxxxxx xxxxxxxxxxxx xxxxxxxxxxxxx xxxxxxxxxxx
    xxxxx xxxxxxxxxxxx xxxxxxxxxxx xxxxxxxx xxxxxxxxxxxx
    xxxxxxxxxxxxx xxxxxxxxxxx xxxxx xxxxxxxxxxxx xxxxxxxxxxx
  ;
  f y;
  xxxxxxxx xxxxxxxxxxxx xxxxxxxxxxxxx xxxxxxxxxxx;
  xxxxx xxxxxxxxxxxx xxxxxxxxxxx;
  f;
  g;
  h;
  fffff ffffffff;
  xxxxxxxx xxxxxxxxxxxx xxxxxxxxxxxxx xxxxxxxxxxx xxxxx xxxxxxxxxxxx xxxxxxxxxxx
  ;
  xxxxxxxx xxxxxxxxxxxx xxxxxxxxxxxxx xxxxxxxxxxx xxxxx xxxxxxxxxxxx xxxxxxxxxxx

let a =
  xxxxxxxx xxxxxxxxxxxx xxxxxxxxxxxxx xxxxxxxxxxx xxxxx xxxxxxxxxxxx xxxxxxxxxxx
  ;
  xxxxxxxx xxxxxxxxxxxx, f xxxxxxxxxxxxx,
  xxxxxxxxxxx xxxxx xxxxxxxxxxxx xxxxxxxxxxx,
  xxxxxxxx
    xxxxxxxxxxxx xxxxxxxxxxxxx xxxxxxxxxxx xxxxx xxxxxxxxxxxx
    xxxxxxxxxxx xxxxxxxx xxxxxxxxxxxx xxxxxxxxxxxxx xxxxxxxxxxx
    xxxxx xxxxxxxxxxxx xxxxxxxxxxx xxxxxxxx xxxxxxxxxxxx
    xxxxxxxxxxxxx xxxxxxxxxxx xxxxx xxxxxxxxxxxx xxxxxxxxxxx
  ,
  f y, xxxxxxxx xxxxxxxxxxxx xxxxxxxxxxxxx xxxxxxxxxxx,
  xxxxx xxxxxxxxxxxx xxxxxxxxxxx, f, g, h, fffff ffffffff,
  xxxxxxxx xxxxxxxxxxxx xxxxxxxxxxxxx xxxxxxxxxxx xxxxx xxxxxxxxxxxx xxxxxxxxxxx
  ,
  xxxxxxxx xxxxxxxxxxxx xxxxxxxxxxxxx xxxxxxxxxxx xxxxx xxxxxxxxxxxx xxxxxxxxxxx

let a =
  [ xxxxxxxx
      xxxxxxxxxxxx xxxxxxxxxxxxx xxxxxxxxxxx xxxxx xxxxxxxxxxxx xxxxxxxxxxx
  ;
    xxxxxxxx xxxxxxxxxxxx; f xxxxxxxxxxxxx;
    xxxxxxxxxxx xxxxx xxxxxxxxxxxx xxxxxxxxxxx;
    xxxxxxxx
      xxxxxxxxxxxx xxxxxxxxxxxxx xxxxxxxxxxx xxxxx xxxxxxxxxxxx
      xxxxxxxxxxx xxxxxxxx xxxxxxxxxxxx xxxxxxxxxxxxx xxxxxxxxxxx
      xxxxx xxxxxxxxxxxx xxxxxxxxxxx xxxxxxxx xxxxxxxxxxxx
      xxxxxxxxxxxxx xxxxxxxxxxx xxxxx xxxxxxxxxxxx xxxxxxxxxxx
  ;
    f y; xxxxxxxx xxxxxxxxxxxx xxxxxxxxxxxxx xxxxxxxxxxx;
    xxxxx xxxxxxxxxxxx xxxxxxxxxxx; f; g; h; fffff ffffffff;
    xxxxxxxx
      xxxxxxxxxxxx xxxxxxxxxxxxx xxxxxxxxxxx xxxxx xxxxxxxxxxxx xxxxxxxxxxx
  ;
    xxxxxxxx
      xxxxxxxxxxxx xxxxxxxxxxxxx xxxxxxxxxxx xxxxx xxxxxxxxxxxx xxxxxxxxxxx
  ]

let a =
  [|
    xxxxxxxx
      xxxxxxxxxxxx xxxxxxxxxxxxx xxxxxxxxxxx xxxxx xxxxxxxxxxxx xxxxxxxxxxx
  ;
    xxxxxxxx xxxxxxxxxxxx; f xxxxxxxxxxxxx;
    xxxxxxxxxxx xxxxx xxxxxxxxxxxx xxxxxxxxxxx;
    xxxxxxxx
      xxxxxxxxxxxx xxxxxxxxxxxxx xxxxxxxxxxx xxxxx xxxxxxxxxxxx
      xxxxxxxxxxx xxxxxxxx xxxxxxxxxxxx xxxxxxxxxxxxx xxxxxxxxxxx
      xxxxx xxxxxxxxxxxx xxxxxxxxxxx xxxxxxxx xxxxxxxxxxxx
      xxxxxxxxxxxxx xxxxxxxxxxx xxxxx xxxxxxxxxxxx xxxxxxxxxxx
  ;
    f y; xxxxxxxx xxxxxxxxxxxx xxxxxxxxxxxxx xxxxxxxxxxx;
    xxxxx xxxxxxxxxxxx xxxxxxxxxxx; f; g; h; fffff ffffffff;
    xxxxxxxx
      xxxxxxxxxxxx xxxxxxxxxxxxx xxxxxxxxxxx xxxxx xxxxxxxxxxxx xxxxxxxxxxx
  ;
    xxxxxxxx
      xxxxxxxxxxxx xxxxxxxxxxxxx xxxxxxxxxxx xxxxx xxxxxxxxxxxx xxxxxxxxxxx
  |]
