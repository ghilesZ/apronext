(** This file is an extension for the Tcons1 module from the apron
Library *)

(** Note : It only adds function, nothing is removed. Extensions are at
the end of the module *)

open Apron
include Tcons1
include Array_maker.TconsExt

(***********************)
(** Negation utilities *)
(***********************)

(** type of constraint negation; i.e : EQ -> DISEQ | SUP -> SUPEQ *)
let neg_typ = function
  | EQ -> DISEQ
  | SUP -> SUPEQ
  | SUPEQ -> SUP
  | DISEQ -> EQ
  | _ -> assert false

(** constraints negation; e.g : a >= b -> a < b *)
let neg d =
  let d = copy d in set_typ d (get_typ d |> neg_typ);
  d

(** @deprecated : conversion to linear constraint *)
let to_lincons env tc =
  let to_string (tc:t) : string =
    let s = Format.(
      fprintf str_formatter "%a" print tc;
      flush_str_formatter ()
    ) in
    let s' = Bytes.of_string s in
    Bytes.iteri (fun i c ->
      if c = '<' && i < Bytes.length s' - 2 && s.[i+1] = '>' then begin
      Bytes.set s' i '<';
      Bytes.set s' (i+1)  '='
    end
    ) s';
    s' |> Bytes.to_string
  in
  let lin = Apron.Parser.lincons1_of_string env (to_string tc) in
  Apron.Lincons1.set_typ lin (Apron.Tcons1.get_typ tc);
  lin


(* (\* split a = into a >= b and a <= b*\) *)
(* let spliteq c = *)
(*   let c1 = copy c in *)
(*   set_typ c1 SUPEQ; *)
(*   let c2 = copy c1 in *)
(*   set_cst c2 (Apron.Coeff.neg (get_cst c2)); *)
(*   iter (fun c v -> set_coeff c2 v (Apron.Coeff.neg c)) c2; *)
(*   c1,c2 *)

(* (\* split a = into a > b or a < b*\) *)
(* let splitdiseq c = *)
(*   let c1 = copy c in *)
(*   set_typ c1 SUP; *)
(*   let c2 = copy c1 in *)
(*   set_cst c2 (Apron.Coeff.neg (get_cst c2)); *)
(*   iter (fun c v -> set_coeff c2 v (Apron.Coeff.neg c)) c2; *)
(*   c1,c2 *)
