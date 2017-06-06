(********************************************************************************)
(** This file is an extension for the Environment module from the apron Library *)
(********************************************************************************)

include Apron.Environment
(** It only adds function, nothing is removed *)

(** environment build from variable string names *)
let make_s ints reals =
  make
    (Array.map Apron.Var.of_string ints)
    (Array.map Apron.Var.of_string reals)

(** empty environment *)
let empty = make [||] [||]

(** adds a variable to an environment according to its type *)
let add_one (var,typ) e =
  match typ with
  | INT -> add e [|var|] [||]
  | REAL -> add e [||] [|var|]

(** same as add_one but with string instread of Var.t *)
let add_one_s (var,typ) e = add_one ((Apron.Var.of_string var),typ) e

(** adds an interger variable *)
let add_int v = add_one (v,INT)

(** same as add_one but with string instread of Var.t *)
let add_int_s v = add_one (Apron.Var.of_string v,INT)

(** adds a real variable *)
let add_real v = add_one (v,REAL)

(** adds a real variable *)
let add_real_s v = add_one (Apron.Var.of_string v,REAL)

(** join two environments *)
let join a b =
  let int_a,real_a = vars a and int_b,real_b = vars b in
  let collect set acc = Array.fold_left (fun acc x -> x::acc) [] set in
  let ints  = [] |> collect int_a |> collect int_b
  and reals = [] |> collect real_a |> collect real_b in
  let ints  =
    List.fold_left
      (fun acc x -> (
         if (not (mem_var acc x)) then add_int x acc
         else acc
       )
      ) empty ints
  in
  let intreals =
    List.fold_left
      (fun acc x -> (
         if (not (mem_var acc x)) then add_real x acc
         else acc
       )
      ) ints reals
  in intreals

(** Envrironment folding function *)
let fold f a e =
  let i,r = vars e in
  let part = Array.fold_left f a i in
  Array.fold_left f part r

(** Iterat a function over an environment *)
let iter f e   =
  fold (fun () x -> f x) () e

(** returns the array of integer variables *)
let get_ints e =
  let g,_ = vars e in g

(** returns the array of real variables *)
let get_reals e =
  let _,g = vars e in g
