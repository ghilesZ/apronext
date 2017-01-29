let print_env fmt e =
  Environmentext.print fmt e

let _ =
  Environmentext.empty
  |> Environmentext.add_int (Apron.Var.of_string "v1")
  |> Environmentext.add_real (Apron.Var.of_string "v2")
  |> Format.printf "%a\n" print_env
