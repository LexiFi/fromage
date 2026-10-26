(* Variations of class specifications, for review (case 27 of unpacked.ml). *)

class
  ['aaaaaaaaaaaaaa, 'bbbbbbbbbbbbbbbbbbb, 'cccccccccccccccc, 'ddddddddddd] c1
  : object end

class c2
  : aaaaaaaaaaaaaaaaaaaaa -> bbbbbbbbbbbbbbbbbbbbbbbbbb
    -> cccccccccccccccccccccc
    -> object end

class
  ['aaaaaaaaaaaaaa, 'bbbbbbbbbbbbbbbbbbb, 'cccccccccccccccc, 'ddddddddddd] c3
  : aaaaaaaaaaaaaaaaaaaaa -> object method m : int end

class
  ['aaaaaaaaaaaaaa, 'bbbbbbbbbbbbbbbbbbb, 'cccccccccccccccc, 'ddddddddddd] c3
  : aaaaaaaaaaaaaaaaaaaaa
    ->
    object
      method m : int
      method m' : int
    end

class type
  ['aaaaaaaaaaaaaa, 'bbbbbbbbbbbbbbbbbbb, 'cccccccccccccccc, 'ddddddddddd] ct4
  = object method m : int end
