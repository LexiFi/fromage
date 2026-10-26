(** At the start of the file. *)

let x = 1

module Empty = struct (** In an empty structure. *) end

module M = struct
  (** At the start. *)

  let y = 2

  (** At the end. *)
end

(** At the end of the file. *)
