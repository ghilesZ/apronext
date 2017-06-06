(*******************************************************************************)
(** This file is an extension for the Generator1 module from the apron Library *)
(*******************************************************************************)
open Apron

(** It only adds function, nothing is removed *)
include Apron.Generator1
include Array_maker.Make (struct

  open Generator1

  type elem = t
  type t = earray

  let get = array_get

  let set = array_set

  let length = array_length

  let make elem = array_make elem.env

  let empty = array_make (Environment.make [||] [||]) 0
end)

(** Converts a Generator1 into a float array array. *)
let to_float_array gens size =
  let scalar_to_float s =
    let res = match s with
      | Scalar.Mpqf x -> Mpqf.to_float x
      | Scalar.Float x -> x
      | Scalar.Mpfrf x -> Mpfrf.to_float ~round:Mpfr.Near x
    in res
  in
  let coeff_to_float = function
    | Coeff.Scalar x -> scalar_to_float x
    | Coeff.Interval i -> scalar_to_float i.Interval.inf
  in
  (* Converts a Generator0 into an float array. *)
  let to_float_array gen size =
    let tab = Array.make size 0. in
    let gen_lin = gen.Generator0.linexpr0 in
    for i=0 to (size-1) do
      let coeff = Linexpr0.get_coeff gen_lin i in
      tab.(i) <- coeff_to_float coeff
    done;
    tab
  in
  let gen_tab = gens.Generator1.generator0_array in
  let tab = Array.make (Array.length gen_tab) (Array.make size 0.) in
  for i=0 to ((Array.length gen_tab)-1) do
    tab.(i) <- to_float_array gen_tab.(i) size
  done;
  tab

(** constructs a new generator in opposite direction *)
let neg (d:Generator1.t) : Generator1.t =
  let d = Generator1.copy d in
  Generator1.iter (fun c v -> Generator1.set_coeff d v (Coeff.neg c)) d;
  d

(** returns a generator corresponding to a float point *)
let of_float_point env coeffs =
  let l = Linexpr1.make env in
  let coeffs = List.mapi (fun i e ->
                   (Coeff.s_of_float e), Environmentext.var_of_dim env i)
                         coeffs
  in
  Linexpr1.set_list l coeffs None;
  make l VERTEX
