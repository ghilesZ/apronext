open Apron

module type ADomain = sig
  type t
  val man: t Manager.t
end

module Make(D:ADomain) = struct

  module A = Abstractext

  type t = D.t A.t

  let man = D.man

  let bottom = A.bottom man

  let top = A.top man

  let join = A.join man

  let meet = A.meet man

  let is_bottom = A.is_bottom man

  let is_top = A.is_top man

  let sat_lincons = A.sat_lincons man

  let sat_tcons = A.sat_tcons man

  let is_leq = A.is_leq man

  let is_eq = A.is_eq man

  let to_lincons_array = A.to_lincons_array man

  let to_tcons_array = A.to_tcons_array man

  let to_lincons_list e = to_lincons_array e |> Linconsext.array_to_list

  let to_tcons_list e = to_tcons_array e |> Tconsext.array_to_list
end
