(* Linking this library makes the round-trip checks check the formatter
   [Ocst_pretty] too (see [Roundtrip.formatter]). *)

let () =
  Roundtrip.formatter :=
    Some
      { format_implementation = Ocst_pretty.implementation;
        format_interface = Ocst_pretty.interface;
        same_tokens =
          ( fun ~filename source formatted ->
              Result.map_error
                ( fun (difference : Ocst_pretty.Same_tokens.difference) ->
                    difference.summary, difference.details
                )
                (Ocst_pretty.Same_tokens.check ~filename source formatted)
          )
      ;
      }
