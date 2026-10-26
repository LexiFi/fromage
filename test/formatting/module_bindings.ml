(* Variations of module bindings with parameters, for review (case 24 of
   unpacked.ml): as a let binding, the parameters packed on the lines after
   the name, indented by 4, then [=] starting a line, indented by 2. *)

module F1 (A : S) = struct end

module F2
    (Aaaaaaaaaaaa : S) (Bbbbbbbbbbbbbbbbbb : S)
    (Ccccccccccccccc : S) (Dddddddddd : S)
  = struct end

module F3
    (Aaaaaaaaaaaa : S) (Bbbbbbbbbbbbbbbbbb : S)
    (Ccccccccccccccc : S) (Dddddddddd : S)
  =
  struct
    let x = 1
    let y = 2
  end

module F4
    (Aaaaaaaaaaaa : S) (Bbbbbbbbbbbbbbbbbb : S)
    (Ccccccccccccccc : S) (Dddddddddd : S)
  = G (Aaaaaaaaaaaa)

module F5
    (Aaaaaaaaaaaa : S) (Bbbbbbbbbbbbbbbbbb : S)
    (Ccccccccccccccc : S) (Dddddddddd : S)
  = Gggggggggggggggggg (Aaaaaaaaaaaa) (Bbbbbbbbbbbbbbbbbb) (Ccccccccccccccc)

module F6
    (Aaaaaaaaaaaa : S) (Bbbbbbbbbbbbbbbbbb : S)
    (Ccccccccccccccc : S) (Dddddddddd : S)
  : T
  = struct end

module F7
    (Aaaaaaaaaaaa : S) (Bbbbbbbbbbbbbbbbbb : S)
    (Ccccccccccccccc : S) (Dddddddddd : S)
  : sig val x : int end
  = struct let x = 1 end

module F8
    ( A :
      sig
        type t
        val compare : t -> t -> int
        val equal : t -> t -> bool
      end
    )
  = struct end

module F9 (A : S) (B : S)
  : T with type t = A.t and type u = B.u and type v = int
  = struct end

module F10 () = struct let x = 1 end

module rec R1 (Aaaaaaaaaaaa : S) (Bbbbbbbbbbbbbbbbbb : S) (Ccccccccccccccc : S)
  : T
  = struct end
and R2 (Aaaaaaaaaaaa : S) (Bbbbbbbbbbbbbbbbbb : S) (Ccccccccccccccc : S) (D : S)
  : T
  = struct end
