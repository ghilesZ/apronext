let tee f x = f x; x

let print_env fmt e =
  Environmentext.print fmt e

let test_env() =
  Environmentext.empty
  |> Environmentext.add_int (Apron.Var.of_string "v1")
  |> Environmentext.add_real (Apron.Var.of_string "v2")
  (* |> tee (Format.printf "%a\n" print_env) *)

let test_box ()=
  let b = Abox.top (test_env ()) in
  Format.printf "%a\n" Abox.print b


let _ =
  test_box()
