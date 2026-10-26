(* Variations of class declarations, for review (case 27 of unpacked.ml),
   then the objects. *)

class c1 = object end

class
  ['aaaaaaaaaaaaaa, 'bbbbbbbbbbbbbbbbbbb, 'cccccccccccccccc, 'ddddddddddd] c2
  = object end

class
  ['aaaaaaaaaaaaaa, 'bbbbbbbbbbbbbbbbbbb, 'cccccccccccccccc, 'ddddddddddd] c3
  =
    object
      val x = 1
      method m = x
    end

class
  ['aaaaaaaaaaaaaa, 'bbbbbbbbbbbbbbbbbbb, 'cccccccccccccccc, 'ddddddddddd] c4
  = d aaaaaaaaaaaaaaaaaaaa bbbbbbbbbbbbbbbbbbbbbbbbb cccccccccccccccccccccc

class virtual
  ['aaaaaaaaaaaaaa, 'bbbbbbbbbbbbbbbbbbb, 'cccccccccccccccc, 'ddddddddddd] c5
  = object end

class c6
    aaaaaaaaaaaaaaaaaaaa bbbbbbbbbbbbbbbbbbbbbbbbb
    cccccccccccccccccccccc ddddddddddd
  = object end

class c7
    (aaaaaaaaaaaaaaaaaaaa : int)
    (bbbbbbbbbbbbbbbbbbbbbbbbb : string) (c : float)
  = object end

class c8 =
  fun aaaaaaaaaaaaaaaaaaaa bbbbbbbbbbbbbbbbbbbbbbbbb cccccccccccccccccccccc ->
    object end

class c9
  : aaaaaaaaaaaaaaaaaaaaa -> bbbbbbbbbbbbbbbbbbbbbbbbbb
    -> cccccccccccccccccccccc
  = d

(* this is a bit ugly for one char name but thats far from typical *)
class c10 =
  d
    aaaaaaaaaaaaaaaaaaaa bbbbbbbbbbbbbbbbbbbbbbbbb
    cccccccccccccccccccccc ddddddd

class c11 = object end
and ['aaaaaaaaaaaaaa, 'bbbbbbbbbbbbbbbbbbb, 'cccccccccccccccc, 'ddddddddddd] c12
  = object end

class type
  ['aaaaaaaaaaaaaa, 'bbbbbbbbbbbbbbbbbbb, 'cccccccccccccccc, 'ddddddddddd] ct13
  = object end

class
  [ 'aaaaaaaaaaaaaa, 'bbbbbbbbbbbbbbbbbbb, 'cccccccccccccccc, 'ddddddddddd, 'e
  ] c14
  : aaaaaaaaaaaaaaaaaaaaa -> bbbbbbbbbbbbbbbbbbbbbbbbbb
  = d

class
  [ 'aaaaaaaaaaaaaa, 'bbbbbbbbbbbbbbbbbbb, 'cccccccccccccccc, 'ddddddddddd, 'e
  ] c15
      aaaaaaaaaa bbbbbbbbbbbbbbbbb
  = d

class
  [ 'aaaaaaaaaaaaaa, 'bbbbbbbbbbbbbbbbbbb, 'cccccccccccccccc,
    'ddddddddddd, 'eeeeeeeee, 'fffffffffffffff
  ] c15
      aaaaaaaaaa bbbbbbbbbbbbbbbbb ccccccccccccc
      dddddddddddddd eeeeeeeeeeeeeeeeee ffffffffffff
  = d

class virtual c16 aaaaaaaaaaaaaaaaaaaaa bbbbbbbbbbbbbbbbbbbbbbbbbb =
  object method virtual m : int end

(* {1 Objects} [new], method calls, overriding, initializers, mutable
   instance variables. *)

(* 1. [new] and method calls. *)
let x1 = new c1
let x1_arguments =
  new class_with_a_long_name
    argument_number_one argument_number_two
    argument_number_three argument_number_four

let x1_call =
  object_with_a_long_name#method_with_a_long_name
    argument_number_one argument_number_two argument_number_three

(* A chain of method calls is packed, the calls glued to each other, the
   lines after the first indented by 2, as the arguments of a function. *)
let x1_chain =
  (new c1)#first#second_method_with_a_long_name
    #third_method_with_a_long_name#fourth#fifth#sixth

(* 2. Overriding. *)
class c2 =
  object
    inherit!
      parent_class_with_a_long_name argument_number_one argument_number_two
    method! m =
      function_with_a_long_name
        argument_number_one argument_number_two
        argument_number_three argument_number_four
    val! v = 1
  end

(* 3. Initializers. *)
class c3 =
  object (self)
    val mutable count = 0
    initializer
      self#reset;
      function_with_a_long_name
        argument_number_one argument_number_two
        argument_number_three argument_number_four
    method reset = count <- 0
  end

(* 4. Mutable instance variables. *)
class c4 =
  object
    val mutable instance_variable_with_a_long_name =
      function_with_a_long_name
        argument_number_one argument_number_two
        argument_number_three argument_number_four
    method get = instance_variable_with_a_long_name
  end

(* 5. Private and virtual methods. *)
class virtual c5 =
  object
    method private virtual m :
      aaaaaaaaaaaaaaaaaaaa -> bbbbbbbbbbbbbbbbbbbbbbbb -> cccccccccccccccccccc
    method private n =
      function_with_a_long_name
        argument_number_one argument_number_two
        argument_number_three argument_number_four
  end

(* 6. Immediate objects. *)
let x6 =
  object (self : 'self)
    method m = 1
    method n =
      self#m
      +
      function_with_a_long_name
        argument_number_one argument_number_two
        argument_number_three argument_number_four
  end
