(* The heads of type declarations whose parameters are on several lines: the
   definition after [=] on the next line when it fits on one line, else [=]
   alone on its line and the definition on the next lines. *)

type
  ( 'aaaaaaaaaaaaa, 'bbbbbbbbbbbbbbbb,
    'ccccccccccccccc, 'ddddddddddddddd, 'eeeeeeeeeee
  ) abstract

type
  ( 'aaaaaaaaaaaaa, 'bbbbbbbbbbbbbbbb,
    'ccccccccccccccc, 'ddddddddddddddd, 'eeeeeeeeeee
  ) short_variant
  = A | B

type
  ( 'aaaaaaaaaaaaa, 'bbbbbbbbbbbbbbbb,
    'ccccccccccccccc, 'ddddddddddddddd, 'eeeeeeeeeee
  ) long_variant
  =
    Aaaaaaaaaaaaaaaaaaa of int | Bbbbbbbbbbbbbbbbbbbbbbbbbb
  | Ccccccccccccccccccccccc of string

type
  ( 'aaaaaaaaaaaaa, 'bbbbbbbbbbbbbbbb,
    'ccccccccccccccc, 'ddddddddddddddd, 'eeeeeeeeeee
  ) short_record
  = { a : int }

type
  ( 'aaaaaaaaaaaaa, 'bbbbbbbbbbbbbbbb,
    'ccccccccccccccc, 'ddddddddddddddd, 'eeeeeeeeeee
  ) long_record
  =
  { aaaaaaaaaaaaaaa : int; bbbbbbbbbbbbbbbbbbbbbbb : string;
    cccccccccccccccccc : float
  }

type
  ( 'aaaaaaaaaaaaa, 'bbbbbbbbbbbbbbbb,
    'ccccccccccccccc, 'ddddddddddddddd, 'eeeeeeeeeee
  ) private_manifest
  =
  private
  aaaaaaaaaaaaaaaaaaaaaa * bbbbbbbbbbbbbbbbbbbbbbbbb * cccccccccccccccccccccc

type
  ( 'aaaaaaaaaaaaa, 'bbbbbbbbbbbbbbbb,
    'ccccccccccccccc, 'ddddddddddddddd, 'eeeeeeeeeee
  ) synonym
  = ('aaaaaaaaaaaaa, 'bbbbbbbbbbbbbbbb) other
  = Aaaaaaaaaaaa | Bbbbbbbbbbbb

type
  ( 'aaaaaaaaaaaaa, 'bbbbbbbbbbbbbbbb,
    'ccccccccccccccc, 'ddddddddddddddd, 'eeeeeeeeeee
  ) open_
  = ..
