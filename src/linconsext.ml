(****************************************************************************)
(* This file is an extension for the Lincons1 module from the apron Library *)
(****************************************************************************)

(* It only adds function, nothing is removed *)
include Apron.Lincons1
include Array_maker.LinconsExt
module C = Coeffext

let is_strict l = match get_typ l with SUP | DISEQ -> true | _ -> false

let fold f g l =
  let acc = ref (g (get_cst l)) in
  iter (fun c v -> acc := f c v !acc) l ;
  !acc

let neg_typ = function
  | EQ -> DISEQ
  | SUP -> SUPEQ
  | SUPEQ -> SUP
  | DISEQ -> EQ
  | _ -> assert false

let neg d =
  let d = copy d in
  set_cst d (C.neg (get_cst d)) ;
  iter (fun c v -> set_coeff d v (C.neg c)) d ;
  set_typ d (get_typ d |> neg_typ) ;
  d

(* split a = b into a >= b and a <= b*)
let spliteq c =
  if get_typ c = EQ then (
    let c1 = copy c in
    set_typ c1 SUPEQ ;
    let c2 = copy c1 in
    set_cst c2 (C.neg (get_cst c2)) ;
    iter (fun c v -> set_coeff c2 v (C.neg c)) c2 ;
    (c1, c2) )
  else raise (Invalid_argument "spliteq must take an equality constraint")

(* split a <> b into a > b or a < b*)
let splitdiseq c =
  if get_typ c = DISEQ then (
    let c1 = copy c in
    set_typ c1 SUP ;
    let c2 = copy c1 in
    set_cst c2 (C.neg (get_cst c2)) ;
    iter (fun c v -> set_coeff c2 v (C.neg c)) c2 ;
    (c1, c2) )
  else raise (Invalid_argument "splitdiseq must take a disequality constraint")

let print_typ fmt = function
  | EQ -> Format.fprintf fmt "="
  | SUP -> Format.fprintf fmt ">"
  | SUPEQ -> Format.fprintf fmt ">="
  | DISEQ -> Format.fprintf fmt "<>"
  | EQMOD _ -> Format.fprintf fmt "eqmod"

let print_sym fmt = function
  | EQ -> Format.fprintf fmt "="
  | SUP -> Format.fprintf fmt "<"
  | SUPEQ -> Format.fprintf fmt "<="
  | DISEQ -> Format.fprintf fmt "<>"
  | EQMOD _ -> Format.fprintf fmt "eqmod"

let pp_monom fmt (c, v) =
  let open Apron in
  if c = C.one then Format.fprintf fmt "%a" Var.print v
  else Format.fprintf fmt "%a%a" C.print c Var.print v

let plus_sep fmt () = Format.fprintf fmt "+"

let print_list fmt l = Format.pp_print_list ~pp_sep:plus_sep pp_monom fmt l

let pp_print fmt (c : t) =
  let l = ref [] in
  let r = ref [] in
  iter
    (fun c v ->
      let s = C.sign c in
      if s < 0 then r := (C.neg c, v) :: !r else if s > 0 then l := (c, v) :: !l
      )
    c ;
  let l = List.rev !l in
  let r = List.rev !r in
  let cst = get_cst c in
  let t = get_typ c in
  match (l, r) with
  | [], r -> Format.fprintf fmt "%a%a%a" print_list r print_sym t C.print cst
  | l, [] ->
      Format.fprintf fmt "%a%a%a" print_list l print_typ t C.print
        (if C.is_zero cst then cst else C.neg cst)
  | l, r ->
      let s = C.sign cst in
      if s = 0 then
        Format.fprintf fmt "%a%a%a" print_list l print_typ t print_list r
      else
        Format.fprintf fmt "%a%a%a%s%a" print_list l print_typ t print_list r
          (if s > 0 then "" else "+")
          C.print (C.neg cst)
