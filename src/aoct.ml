open Apron

include Domain.Make(struct
  type t = Oct.t
  let man = Oct.manager_alloc ()
end)
