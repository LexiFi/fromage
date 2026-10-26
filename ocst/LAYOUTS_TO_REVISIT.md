# Layouts to revisit

The layouts of the formatter (`pretty/`) which are not decided yet, and may
change: each with the tests where it is shown, in `test/formatting/`. The
decided ones are in HACKING.md (Layout choices). Remove an entry once it is
decided, and move it to HACKING.md if it is unusual.

- Operators: an operator after an operand on several lines is alone on its
  line, and the next operand is in the column of the first one, not
  indented; it is heavy for `|>` pipelines whose steps are functions, and
  for `f @@ fun x ->` (`expressions_applications.ml`,
  `separators_after_long_parts.ml`).
- A function as the last argument is not hung on the line of the
  application (`List.iter (fun x ->`), so that an application and a
  function which both have many parameters stay readable
  (`expressions_applications.ml`).
- The parts which come after the important one: the parameters after a
  head on several lines start the next line (`] c2_short` then `x y`), as
  the name after type parameters on several lines; the important part
  comes last, and indenting it less is unnatural, but these heads are
  rarely on several lines (`binding_parameters.ml`, `type_heads.ml`).
