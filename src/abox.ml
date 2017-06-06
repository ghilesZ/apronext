(** Boxes abstract domain *)
module Domain = Domain.Make(struct
  type t = Box.t
  let man = Box.manager_alloc ()
end)

(** Instanciation of the Domain.Make functor *)
include Domain
