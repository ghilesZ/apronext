(** Polyhedra abstract domain *)

module Domain = Domain.Make(struct
  type t = Polka.strict Polka.t
  let man = Polka.manager_alloc_strict()
end)

(** Instanciation of the Domain.Make functor *)
include Domain
