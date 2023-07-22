(** This file is an extension for the Abstract1 module from the apron Library.
    It is not meant to be used as it is but through the instanciated modules
    Abox, Apol and Aoct *)

open Apron

(** Abstract domain signature used by the following functor *)
module type ADomain = sig
  (** Abstract element type *)
  type t

  val man : t Manager.t
  (** Apron manager *)
end

(** functor that hides the use of managers. It also adds few utilities to the
    Abstract1 module *)
module Make (D : ADomain) = struct
  (** This functor implements all the constraint/generator based operations over
      abstract elements. These are generic and stand for Boxes, Octagons and
      Polyhedra. *)

  (** Conventions :

      - functions ending with _s allow to use/return string instead of variables
      - functions ending with _f allow to use/return float instead of scalar
      - functions ending with _fs mix the two *)

  module A = Abstract1
  module G = Generatorext
  module T = Tconsext
  module L = Linconsext
  module E = Environmentext
  module I = Intervalext

  type t = D.t A.t

  type box1 = A.box1 = {mutable interval_array: I.t array; mutable box1_env: E.t}

  (** Type of merge policies for binary operations on abstract elements defined
      on heterogeneous environment :

      the behaviour of binary operations on heterogeneous environment is
      specified using the optional ?env argument. Default behaviour (Fail) is to
      assume that both elements are defined on the same set of variables;
      otherwise, an Invalid_argument exception will be raised. When ?env is set
      to Meet the result is defined on the intersection of both environment *)
  type policy = Fail | Meet

  (** Set-theoritic operations *)

  let bottom : Environmentext.t -> t = A.bottom D.man

  let top : Environmentext.t -> t = A.top D.man

  let join ?merge:(m = Fail) a1 a2 =
    match m with
    | Fail -> A.join D.man a1 a2
    | Meet ->
        let e = E.meet a1.env a2.env in
        A.join D.man
          (A.change_environment D.man a1 e false)
          (A.change_environment D.man a2 e false)

  let meet ?merge:(m = Fail) a1 a2 =
    match m with
    | Fail -> A.meet D.man a1 a2
    | Meet ->
        let e = E.meet a1.env a2.env in
        A.meet D.man
          (A.change_environment D.man a1 e false)
          (A.change_environment D.man a2 e false)

  let widening : t -> t -> t = A.widening D.man

  (** predicates *)

  let is_bottom : t -> bool = A.is_bottom D.man

  let is_top : t -> bool = A.is_top D.man

  let is_leq : t -> t -> bool = A.is_leq D.man

  let is_eq : t -> t -> bool = A.is_eq D.man

  (** constraint satisfaction and filter *)

  let sat_lincons : t -> Linconsext.t -> bool = A.sat_lincons D.man

  let sat_tcons : t -> Tconsext.t -> bool = A.sat_tcons D.man

  let filter_lincons (abs : t) (l : Linconsext.t) : t =
    let ear = L.array_make abs.env 1 in
    L.array_set ear 0 l ;
    A.meet_lincons_array D.man abs ear

  let filter_tcons (abs : t) (l : Tconsext.t) : t =
    let ear = T.array_make abs.env 1 in
    T.array_set ear 0 l ;
    A.meet_tcons_array D.man abs ear

  (** of and to constraints/generator *)

  let to_lincons_array : t -> Linconsext.earray = A.to_lincons_array D.man

  let to_tcons_array : t -> Tconsext.earray = A.to_tcons_array D.man

  let to_generator_array : t -> Generatorext.earray = A.to_generator_array D.man

  let to_lincons_list (abs : t) = abs |> to_lincons_array |> L.array_to_list

  let to_tcons_list (abs : t) = abs |> to_tcons_array |> T.array_to_list

  let to_generator_list (abs : t) = abs |> to_generator_array |> G.array_to_list

  let of_generator_list (g : Generatorext.t list) : t =
    let open Generator1 in
    let e = get_env (List.hd g) in
    let v, lray = List.partition (fun g -> get_typ g = VERTEX) g in
    let of_vertice v =
      E.fold
        (fun acc var ->
          let c = Texprext.cst e (get_coeff v var) in
          A.assign_texpr D.man acc var c None )
        (top e) e
    in
    let closed = A.join_array D.man (Array.of_list (List.map of_vertice v)) in
    Generatorext.array_of_list lray |> A.add_ray_array D.man closed

  let of_generator_array g : t = of_generator_list (G.array_to_list g)

  let of_lincons_array : Environmentext.t -> Lincons1.earray -> t =
    A.of_lincons_array D.man

  let of_tcons_array : Environmentext.t -> Tcons1.earray -> t =
    A.of_tcons_array D.man

  let of_lincons_list env l = of_lincons_array env (L.array_of_list l)

  let of_tcons_list env l = of_tcons_array env (T.array_of_list l)

  let of_box : Environmentext.t -> Var.t array -> Intervalext.t array -> t =
    A.of_box D.man

  (** Environment and variable related operations *)

  let get_environment : t -> Environmentext.t = A.env

  let change_environment (abs : t) e : t =
    A.change_environment D.man abs e false

  (*FIXME : What is the purpose of None?*)
  let assign_texpr (abs : t) var texpr : t =
    A.assign_texpr D.man abs var texpr None

  (*FIXME : What is the purpose of None?*)
  let assign_linexpr (abs : t) var linexpr : t =
    A.assign_linexpr D.man abs var linexpr None

  let assign_interval (abs : t) var i : t =
    let texpr = Texprext.cst abs.env (Coeffext.of_interval i) in
    assign_texpr abs var texpr

  let assign_f (abs : t) var f : t =
    let texpr = Texprext.cst_f f |> Texprext.of_expr abs.env in
    assign_texpr abs var texpr

  let assign_fs (abs : t) var f : t = assign_f abs (Var.of_string var) f

  (* utilities*)
  let add_var (abs : t) typ v : t =
    let e = A.env abs in
    let ints, reals =
      if typ = Environment.INT then ([|v|], [||]) else ([||], [|v|])
    in
    let env = Environment.add e ints reals in
    A.change_environment D.man abs env false

  let add_var_s (abs : t) typ v : t = add_var abs typ (Var.of_string v)

  let bound_variable (abs : t) v = A.bound_variable D.man abs v

  let bound_variable_f (abs : t) v = bound_variable abs v |> I.to_float

  let bound_variable_s (abs : t) v = bound_variable abs (Var.of_string v)

  let bound_variable_fs (abs : t) v = bound_variable_f abs (Var.of_string v)

  let is_bounded_variable (abs : t) v = I.is_bounded (bound_variable abs v)

  let is_bounded_s (abs : t) v = is_bounded_variable abs (Var.of_string v)

  let is_bounded (abs : t) =
    let env = A.env abs in
    try
      E.iter (fun v -> if is_bounded_variable abs v |> not then raise Exit) env ;
      true
    with Exit -> false

  let to_box1 = A.to_box D.man

  (** Cross-domain conversion *)
  let to_box (abs : t) =
    let env = A.env abs in
    let abs' = A.change_environment D.man abs env false in
    to_tcons_array abs' |> A.of_tcons_array (Box.manager_alloc ()) env

  let to_oct (abs : t) =
    let env = A.env abs in
    to_lincons_array abs |> A.of_lincons_array (Oct.manager_alloc ()) env

  let to_poly (abs : t) =
    let env = A.env abs in
    let abs' = A.change_environment D.man abs env false in
    to_tcons_array abs' |> A.of_tcons_array (Polka.manager_alloc_strict ()) env

  (** Printing *)
  let print = A.print

  (** Pretty printing *)
  let pp_print fmt (a : t) =
    (* print fmt a *)
    let constraints = to_lincons_list a in
    Format.fprintf fmt "[|%a|]"
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt "; ")
         Linconsext.pp_print )
      constraints

  (** Projection on 2 dimensions *)
  let proj2D (abs : t) v1 v2 : t =
    let env = A.env abs in
    let add_var v res =
      if E.typ_of_var env v = E.INT then E.add_int v res else E.add_real v res
    in
    let new_env =
      if v1 = v2 then add_var v1 E.empty else add_var v1 E.empty |> add_var v2
    in
    change_environment abs new_env

  (** Projection on 3 dimensions *)
  let proj3D (abs : t) v1 v2 v3 : t =
    let env = A.env abs in
    let new_env = E.empty in
    let new_env =
      if Array.exists (( = ) v1) (E.get_ints env) then E.add_int v1 new_env
      else E.add_real v1 new_env
    in
    let new_env =
      if Array.exists (( = ) v2) (E.get_ints env) then E.add_int v2 new_env
      else E.add_real v2 new_env
    in
    let new_env =
      if Array.exists (( = ) v3) (E.get_ints env) then E.add_int v3 new_env
      else E.add_real v3 new_env
    in
    change_environment abs new_env

  (** Projection on 2 dimensions with string as variables *)
  let proj2D_s (abs : t) v1 v2 =
    proj2D abs (Var.of_string v1) (Var.of_string v2)

  (** Projection on 3 dimensions with string as variables *)
  let proj3D_s (abs : t) v1 v2 v3 =
    proj3D abs (Var.of_string v1) (Var.of_string v2) (Var.of_string v3)

  (** returns the vertices of an abstract element projected on 2 dimensions *)
  let to_vertices2D (abs : t) v1 v2 =
    let gen' = to_generator_array abs in
    let get_coord l = Linexpr1.(get_coeff l v1, get_coeff l v2) in
    Array.init (G.array_length gen') (fun i ->
        get_coord G.(get_linexpr1 (array_get gen' i)) )
    |> Array.to_list
    |> List.rev_map (fun (a, b) -> (Coeffext.to_float a, Coeffext.to_float b))

  (** returns the vertices of an abstract element projected on 2 dimensions *)
  let to_vertices2D_s (abs : t) v1 v2 =
    to_vertices2D abs (Var.of_string v1) (Var.of_string v2)
end
