(** Octagons abstract domain *)

module Domain = Domain.Make(struct
  type t = Oct.t
  let man = Oct.manager_alloc ()
end)

(** Instanciation of the Domain.Make functor *)
include Domain
