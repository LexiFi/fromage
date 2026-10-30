(* Linking this library makes the round-trip checks check the formatter
   [Ocst_pretty] too (see [Roundtrip.formatter]). *)

let () =
  Roundtrip.formatter :=
    Some
      { format_implementation = Ocst_pretty.implementation;
        format_interface = Ocst_pretty.interface
      }
