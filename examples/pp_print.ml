open Apronext

let env = Environmentext.make_s [||] [|"x"; "y"; "z"|]

let gens =
  List.map
    (Generatorext.of_float_point env)
    [[0.; 0.; 0.]; [0.; 100.; 50.]; [50.; 200.; 100.]; [100.; 0.; 150.]]

let polyhedron = Apol.of_generator_list gens

let show_cons c =
  Format.printf "%a\n%a\n" Linconsext.print c Linconsext.pp_print c

let test_poly () =
  Format.printf "%a\n%a\n" Apol.print polyhedron Apol.pp_print polyhedron

let before_after str = Apron.Parser.lincons1_of_string env str |> show_cons

let test_lin () =
  before_after "x + y - 3 >= 0" ;
  before_after "x >= 1" ;
  before_after "x >= 200" ;
  before_after "x <= 400" ;
  before_after "x >= 0" ;
  before_after "x - y - 3 >= 0" ;
  before_after "x - y + 3 >= 0" ;
  before_after "y + x + 3 >= 0" ;
  before_after "y + x - z - 3 >= 0"

let _ = test_poly () ; test_lin ()
