(*****************************************************************************)
(** This file is an extension for the Abstract1 module from the apron Library.
    It is not meant to be used as it is but through the instanciated modules
    Abox, Apol and Aoct *)
(*****************************************************************************)

open Apron

(** Abstract domain signature used by the following functor *)
module type ADomain = sig
  (** Abstract element type *)
  type t

  (** Apron manager *)
  val man: t Manager.t
end

(** functor that allows hiding the use of the manager,
 It also adds few utilities to the Abstract1 module *)
module Make(D:ADomain) = struct
  (** This functor implements all the constraint/generator based
operations over abstract elements. These are generic and stand for
Boxes, Octagons and Polyhedra.
   *)

  (** Conventions :
  - functions ending with _s allow to use/return string instead of variables
  - functions ending with _f allow to use/return float instead of apron scalar
  - functions ending with _fs mix the two
   *)

  module A = Abstract1
  module G = Generatorext
  module T = Tconsext
  module L = Linconsext
  module E = Environmentext

  open A
  type t = D.t A.t

  let man = D.man

  (** Set-theoritic operations *)

  let bottom = bottom man

  let top = top man

  let join = join man

  let meet = meet man

  let widening = widening man

  (** predicates *)

  let is_bottom = is_bottom man

  let is_top = is_top man

  let is_leq = is_leq man

  let is_eq = is_eq man

  (** constraint satisfaction and filter *)

  let sat_lincons = sat_lincons man

  let sat_tcons = sat_tcons man

  let filter_lincons abs l =
    let ear = L.array_make abs.env 1 in
    L.array_set ear 0 l;
    meet_lincons_array man abs ear

  let filter_tcons abs l =
    let ear = T.array_make abs.env 1 in
    T.array_set ear 0 l;
    meet_tcons_array man abs ear

  (** of and to constraints/generator *)

  let to_lincons_array = to_lincons_array man

  let to_tcons_array = to_tcons_array man

  let to_generator_array = to_generator_array man

  let to_lincons_list e = to_lincons_array e |> L.array_to_list

  let to_tcons_list e = to_tcons_array e |> T.array_to_list

  let to_generator_list e = to_generator_array e |> G.array_to_list

  let of_generator_list (e : Environment.t) (g : Generator1.t list) =
    let open Generator1 in
    let lvertex,lray = List.partition (fun g -> get_typ g = VERTEX) g in
    let ofvertice v =
      Environmentext.fold (fun acc var ->
          let c = Texpr1.cst e (get_coeff v var) in
          assign_texpr man acc var c None
        ) (top e) e
    in
    let closed = join_array man (Array.of_list (List.map ofvertice g)) in
    Generatorext.array_of_list lray |> add_ray_array man closed

  let of_generator_array (e : Environment.t) g =
    of_generator_list e (Generatorext.array_to_list g)

  let of_lincons_array = of_lincons_array man

  let of_tcons_array = of_tcons_array man

  let of_lincons_list env l = of_lincons_array env (L.array_of_list l)

  let of_tcons_list env l = of_tcons_array env (T.array_of_list l)

  (** Environment and variable related operations *)

  let change_environment a e = change_environment man a e false

  (*FIXME : What is the purpose of None?*)
  let assign_texpr abs var texpr = assign_texpr man abs var texpr None

  (*FIXME : What is the purpose of None?*)
  let assign_linexpr abs var linexpr = assign_linexpr man abs var linexpr None

  let assign_f abs var f =
    let texpr = Texpr1.(Cst (Coeff.s_of_float f) |> of_expr E.empty) in
    assign_texpr abs var texpr

  let assign_fs abs var f = assign_f abs (Var.of_string var) f

  (* utilities*)
  let add_var abs typ v =
    let e = env abs in
    let ints,reals =
      if typ = Environment.INT then [|v|],[||]
      else [||],[|v|]
    in
    let env = Environment.add e ints reals in
    A.change_environment man abs env false

  let add_var_s abs typ v = add_var abs typ (Var.of_string v)

  let bound_variable abs v = bound_variable man abs v

  let bound_variable_f abs v =
    let scalar_to_float = function
      | Scalar.Mpqf x -> Mpqf.to_float x
      | Scalar.Float x -> x
      | Scalar.Mpfrf x -> Mpfrf.to_float ~round:Mpfr.Near x
    in
    let open Interval in
    let {inf;sup} = bound_variable abs v in
    (scalar_to_float inf),(scalar_to_float sup)

  let bound_variable_fs abs v = bound_variable_f abs (Var.of_string v)

  let is_bounded abs v =
    try
      E.iter (fun v ->
          let open Interval in
          let {inf;sup} = bound_variable abs v in
          if Scalar.is_infty inf <> 0 || Scalar.is_infty sup <> 0
          then raise Exit
        ) (env abs);
      true
    with Exit -> false

  let is_bounded_s abs v = is_bounded abs (Var.of_string v)

  (** Cross-domain conversion *)

  let to_box abs =
    let env = env abs in
    let abs' = A.change_environment man abs env false in
    to_tcons_array abs' |> A.of_tcons_array (Box.manager_alloc ()) env

  let to_oct abs =
    let env = env abs in
    let abs' = A.change_environment man abs env false in
    to_tcons_array abs' |> A.of_tcons_array (Oct.manager_alloc ()) env

  let to_poly abs =
    let env = env abs in
    let abs' = A.change_environment man abs env false in
    to_tcons_array abs' |> A.of_tcons_array (Polka.manager_alloc_strict ()) env

  (** Printing *)

  let print fmt a = print fmt a

end
