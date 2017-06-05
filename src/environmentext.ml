include Apron.Environment

let empty = make [||] [||]

let add_one (var,typ) e =
  match typ with
  | INT -> add e [|Apron.Var.of_string var|] [||]
  | REAL -> add e [||] [|Apron.Var.of_string var|]

let add_int v = add_one (v,INT)

let add_real v = add_one (v,REAL)

let fold f a e =
  let i,r = vars e in
  let part = Array.fold_left f a i in
  Array.fold_left f part r

let iter f e   =
  fold (fun () x -> f x) () e

let get_ints e =
  let g,_ = vars e in g

let get_reals e =
  let _,g = vars e in g

let join_env a b =
  let int_a,real_a = vars a and int_b,real_b = vars b in
  let collect set acc = Array.fold_left (fun acc x -> x::acc) [] set in
  let ints  = [] |> collect int_a |> collect int_b
  and reals = [] |> collect real_a |> collect real_b in
  let ints  =
    List.fold_left
      (fun acc x -> (
         if (not (mem_var acc x)) then add_int (Apron.Var.to_string x) acc
         else acc
       )
      ) empty ints
  in
  let intreals =
    List.fold_left
      (fun acc x -> (
         if (not (mem_var acc x)) then add_real (Apron.Var.to_string x) acc
         else acc
       )
      ) ints reals
  in intreals
