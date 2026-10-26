(* The parameters of a binding which do not fit on the line of its head are
   packed on the next lines, indented by 4 from the indentation of the line
   where the head starts: at 4 after a head on the line of its keyword, at 6
   after a head on the lines after its keyword (indented by 2). [=] and the
   constraints are indented by 2 from the keyword in all cases. *)

(* 1. A head on the line of its keyword. *)

let function_with_a_long_name
    argument_number_one argument_number_two
    argument_number_three argument_number_four
  = body

class c1
    argument_number_one argument_number_two
    argument_number_three argument_number_four
  = d

class ['a, 'b] c1_type_parameters
    argument_number_one argument_number_two
    argument_number_three argument_number_four
  = d

module F1
    (Aaaaaaaaaaaa : S) (Bbbbbbbbbbbbbbbbbb : S)
    (Ccccccccccccccc : S) (Dddddddddd : S)
  = struct end

class c1_method =
  object
    method method_with_a_long_name
        argument_number_one argument_number_two
        argument_number_three argument_number_four
      = body
  end

(* 2. A head on the lines after its keyword: type parameters on several
   lines. *)

(* 2a. Parameters packed. *)
class
  [ 'aaaaaaaaaaaaaa, 'bbbbbbbbbbbbbbbbbbb, 'cccccccccccccccc,
    'ddddddddddd, 'eeeeeeeee, 'fffffffffffffff
  ] c2
      aaaaaaaaaa bbbbbbbbbbbbbbbbb ccccccccccccc
      dddddddddddddd eeeeeeeeeeeeeeeeee ffffffffffff
  = d

(* 2b. Short parameters: after a head on several lines, they start the
   next line. *)
class
  [ 'aaaaaaaaaaaaaa, 'bbbbbbbbbbbbbbbbbbb, 'cccccccccccccccc,
    'ddddddddddd, 'eeeeeeeee, 'fffffffffffffff
  ] c2_short
      x y
  = d

(* 2c. No parameters. *)
class
  [ 'aaaaaaaaaaaaaa, 'bbbbbbbbbbbbbbbbbbb, 'cccccccccccccccc,
    'ddddddddddd, 'eeeeeeeee, 'fffffffffffffff
  ] c2_none
  = d

(* 2d. A parameter on several lines, alone on its lines. *)
class
  [ 'aaaaaaaaaaaaaa, 'bbbbbbbbbbbbbbbbbbb, 'cccccccccccccccc,
    'ddddddddddd, 'eeeeeeeee, 'fffffffffffffff
  ] c2_parameter
      aaaaaaaaaa
      { field_number_one; field_number_two;
        field_number_three; field_number_four
      }
      bbbbbbbbbbbbbbbbb
  = d

(* shorter *)
class ['aa, 'bb, 'ccccccc] c2_parameter
    aaaaaaaaaa
    { field_number_one; field_number_two; field_number_three;
      field_number_four; field_number_five
    }
    bbbbbbbbbbbbbbbbb
  = d

(* 2e. A constraint and a body on several lines. *)
class virtual
  [ 'aaaaaaaaaaaaaa, 'bbbbbbbbbbbbbbbbbbb, 'cccccccccccccccc,
    'ddddddddddd, 'eeeeeeeee, 'fffffffffffffff
  ] c2_constraint
      aaaaaaaaaa bbbbbbbbbbbbbbbbb ccccccccccccc
      dddddddddddddd eeeeeeeeeeeeeeeeee ffffffffffff
  : object method virtual m : int end
  =
    object
      method virtual m : int
      method n = 1
    end
