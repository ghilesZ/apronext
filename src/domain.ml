open Apron

module type ADomain = sig
  type t
  val man: t Manager.t
end

module Make(D:ADomain) = struct

  module A = Abstractext

  include A

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

  let to_generator_array = A.to_generator_array man

  let to_lincons_list e = to_lincons_array e |> Linconsext.array_to_list

  let to_tcons_list e = to_tcons_array e |> Tconsext.array_to_list

  let to_generator_list e = to_generator_array e |> Generatorext.array_to_list

  let of_lincons_array = A.of_lincons_array man

  let of_tcons_array = A.of_tcons_array man

  (* let of_generator_array = A.of_generator_array man *)

  let of_lincons_list env l = of_lincons_array env (Linconsext.array_of_list l)

  let of_tcons_list env l = of_tcons_array env (Tconsext.array_of_list l)

  (* let of_generator_list e = of_generaofr_array e |> Generatorext.array_of_list *)

  let filter_tcons = A.filter_tcons man

  let filter_lincons = A.filter_lincons man

  let widening = A.widening man

  let print fmt a = A.print fmt a

  (* utilities*)
  let add_var abs typ v =
    let e = env abs in
    let ints,reals =
      if typ = Environmentext.INT then [|Var.of_string v|],[||]
      else [||],[|Var.of_string v|]
    in
    let env = Environment.add e ints reals in
    change_environment man abs env false

  let to_box abs =
    let env = env abs in
    let abs' = change_environment man abs env false in
    to_tcons_array abs' |> A.of_tcons_array (Box.manager_alloc ()) env

  let to_oct abs =
    let env = env abs in
    let abs' = change_environment man abs env false in
    to_tcons_array abs' |> A.of_tcons_array (Oct.manager_alloc ()) env

  let to_poly abs =
    let env = env abs in
    let abs' = change_environment man abs env false in
    to_tcons_array abs' |> A.of_tcons_array (Polka.manager_alloc_strict ()) env

end
