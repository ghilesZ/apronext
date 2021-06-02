include Domain.Make (struct
  type t = Box.t

  let man = Box.manager_alloc ()
end)

let diff (_b1 : t) (_b2 : t) : t = assert false
