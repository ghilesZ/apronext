let env =  ["x";"y"]

let pts = [[0.;0.];[10.;20.];[20.;0.]]

let test_poly () =
  Apol.make_of_float_points env pts
  |> Format.printf "poly : %a\n%!" Apol.print

let test_box () =
  Abox.make_of_float_points env pts
  |> Format.printf "itv : %a\n%!" Abox.print

let test_oct () =
  Aoct.make_of_float_points env pts
  |> Format.printf "oct : %a\n%!" Aoct.print

let _ =
  test_poly();
  test_box ();
  test_oct ()
