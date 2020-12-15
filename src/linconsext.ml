(****************************************************************************)
(* This file is an extension for the Lincons1 module from the apron Library *)
(****************************************************************************)

(* It only adds function, nothing is removed *)
include Apron.Lincons1
include Array_maker.LinconsExt

(**********************)
(* Negation utilities *)
(**********************)
let neg_typ = function
  | EQ -> DISEQ
  | SUP -> SUPEQ
  | SUPEQ -> SUP
  | DISEQ -> EQ
  | _ -> assert false

let neg d =
  let d = copy d in
  set_cst d (Apron.Coeff.neg (get_cst d));
  iter (fun c v -> set_coeff d v (Apron.Coeff.neg c)) d;
  set_typ d (get_typ d |> neg_typ);
  d

(* split a = b into a >= b and a <= b*)
let spliteq c =
  if get_typ c = EQ then
    let c1 = copy c in
    set_typ c1 SUPEQ;
    let c2 = copy c1 in
    set_cst c2 (Apron.Coeff.neg (get_cst c2));
    iter (fun c v -> set_coeff c2 v (Apron.Coeff.neg c)) c2;
    c1,c2
  else raise (Invalid_argument "spliteq must take an equality constraint")

(* split a <> b into a > b or a < b*)
let splitdiseq c =
  if get_typ c = DISEQ then
    let c1 = copy c in
    set_typ c1 SUP;
    let c2 = copy c1 in
    set_cst c2 (Apron.Coeff.neg (get_cst c2));
    iter (fun c v -> set_coeff c2 v (Apron.Coeff.neg c)) c2;
    c1,c2
  else raise (Invalid_argument "splitdiseq must take a disequality constraint")

let print_typ fmt = function
  | EQ -> Format.fprintf fmt "="
  | SUP -> Format.fprintf fmt ">"
  | SUPEQ -> Format.fprintf fmt ">="
  | DISEQ -> Format.fprintf fmt "<>"
  | EQMOD _ -> Format.fprintf fmt "eqmod"

let pp_print fmt (c:t) =
  let open Apron in
  let left = ref[] in
  let right = ref[] in
  let is_neg c = Coeff.cmp c (Coeff.s_of_int 0) > 0 in
  iter (fun c v ->
      if is_neg c then right := (Coeff.neg c,v)::!right
      else left := (c,v)::!right
    ) c;
  let plus_sep fmt () = Format.fprintf fmt "+" in
  let positive fmt (c,v) = Format.fprintf fmt "%a%a" Coeff.print c Var.print v in
  let print_list = Format.pp_print_list ~pp_sep:plus_sep positive in
  match !left,!right with
  | [],r -> Format.fprintf fmt "%a %a 0" print_list r print_typ (neg_typ (get_typ c))
  | l,[] -> Format.fprintf fmt "%a %a 0" print_list l print_typ (get_typ c)
  | l,r -> Format.fprintf fmt "%a %a %a" print_list l print_typ (get_typ c) print_list r
