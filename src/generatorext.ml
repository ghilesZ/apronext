(******************************************************************************)
(* This file is an extension for the Generator1 module from the apron Library *)
(******************************************************************************)
open Apron

(* It only adds function, nothing is removed *)
include Apron.Generator1
include Array_maker.GeneratorExt

type float_point = float array

(* Converts a Generator0 into an array of floats. *)
let to_float_array (gen : Generator0.t) size : float_point =
  let gen_lin = gen.Generator0.linexpr0 in
  Array.init size (fun i ->
      let coeff = Linexpr0.get_coeff gen_lin i in
      Coeffext.to_float coeff)

(* Converts a Generator1.earray into an array of float_points *)
let to_float_array (gens : earray) : float_point array =
  let size = Environmentext.size (array_get gens 0).env in
  let gen_tab = gens.generator0_array in
  Array.map (fun e -> to_float_array e size) gen_tab

(* constructs a new generator in opposite direction *)
let neg (d : t) : t =
  let d = copy d in
  iter (fun c v -> set_coeff d v (Coeff.neg c)) d ;
  d

(*returns a generator corresponding to a float point*)
let of_rational_point env coeffs =
  let l = Linexpr1.make env in
  let coeffs =
    List.mapi
      (fun i e -> (Coeff.s_of_mpqf e, Environmentext.var_of_dim env i))
      coeffs
  in
  Linexpr1.set_list l coeffs None ;
  make l VERTEX

(*returns a generator corresponding to a float point*)
let of_float_point env coeffs =
  let l = Linexpr1.make env in
  let coeffs =
    List.mapi
      (fun i e -> (Coeff.s_of_float e, Environmentext.var_of_dim env i))
      coeffs
  in
  Linexpr1.set_list l coeffs None ;
  make l VERTEX

let to_vertices2D (g : t) x y =
  let l = get_linexpr1 g in
  Linexpr1.(Coeffext.(to_float (get_coeff l x), to_float (get_coeff l y)))

let to_vertices2D_s (g : t) x y =
  to_vertices2D g (Var.of_string x) (Var.of_string y)

let to_vertices3D (g : t) x y z =
  let l = get_linexpr1 g in
  Linexpr1.(
    Coeffext.
      ( to_float (get_coeff l x)
      , to_float (get_coeff l y)
      , to_float (get_coeff l z) ))

let to_vertices3D_s (g : t) x y z =
  to_vertices3D g (Var.of_string x) (Var.of_string y) (Var.of_string z)
