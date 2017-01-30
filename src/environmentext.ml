include Apron.Environment

let empty = make [||] [||]

let add (var,typ) e =
  match typ with
  | INT -> add e [|Apron.Var.of_string var|] [||]
  | REAL -> add e [||] [|Apron.Var.of_string var|]

let add_int v = add (v,INT)

let add_real v = add (v,REAL)
