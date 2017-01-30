open Apron

module type ADomain = sig
  type t
  val man: t Manager.t
end

module Make(D:ADomain) = struct

  module A = Abstract1
  module G = Generatorext
  module T = Tconsext
  module L = Linconsext
  module E = Environmentext

  include A

  let filter_lincons man abs l =
    let ear = L.array_make abs.env 1 in
    L.array_set ear 0 l;
    meet_lincons_array man abs ear

  let filter_tcons man abs l =
    let ear = T.array_make abs.env 1 in
    T.array_set ear 0 l;
    meet_tcons_array man abs ear


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

  let to_lincons_list e = to_lincons_array e |> L.array_to_list

  let to_tcons_list e = to_tcons_array e |> T.array_to_list

  let to_generator_list e = to_generator_array e |> G.array_to_list

  let of_lincons_array = A.of_lincons_array man

  let of_tcons_array = A.of_tcons_array man

  (* let of_generator_array = A.of_generator_array man *)

  let of_lincons_list env l = of_lincons_array env (L.array_of_list l)

  let of_tcons_list env l = of_tcons_array env (T.array_of_list l)

  (* let of_generator_list e = of_generaofr_array e |> G.array_of_list *)

  let filter_tcons = filter_tcons man

  let filter_lincons = filter_lincons man

  let widening = A.widening man

  let print fmt a = A.print fmt a

  (* utilities*)
  let add_var abs typ v =
    let e = env abs in
    let ints,reals =
      if typ = E.INT then [|Var.of_string v|],[||]
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

  let make_of_float_points envl l =
    let open E in
    let env = List.fold_left (fun acc v -> add_real v acc) empty envl in
    let point_to_constr pt =
      let expr variable value =
        let variable = Var.of_string variable in
        let c = Texpr1.cst env (Coeff.s_of_float value) in
        Texpr1.(binop Sub (var env variable) c Texpr0.Real Texpr0.Near)
      in
      List.map2 (fun f v ->
          let e = expr v f in
          T.(make e EQ)
        ) pt envl
    in
    List.fold_left (fun abs pt ->
        let tc = point_to_constr pt in
        let abs' = List.fold_left filter_tcons (top env) tc in
        join abs abs'
      ) (bottom env) l

end
