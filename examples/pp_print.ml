open Apronext

let env = Environmentext.make_s [||] [|"x"; "y"; "z"|]

let gens =
  [ Generatorext.of_float_point env [0.; 0.; 0.]
  ; Generatorext.of_float_point env [0.; 100.; 50.]
  ; Generatorext.of_float_point env [50.; 200.; 100.]
  ; Generatorext.of_float_point env [100.; 0.; 150.] ]

let polyhedron = Apol.of_generator_list gens

let show_cons c =
  Format.printf "%a\n%a\n" Linconsext.print c Linconsext.pp_print c

let test_poly () =
  Format.printf "%a\n%a\n" Apol.print polyhedron Apol.pp_print polyhedron

let test_lin () =
  let lc1 = Apron.Parser.lincons1_of_string env "x + y - 3 >= 0" in
  let lc2 = Apron.Parser.lincons1_of_string env "x >= 0" in
  let lc3 = Apron.Parser.lincons1_of_string env "x >= 200" in
  let lc4 = Apron.Parser.lincons1_of_string env "x <= 400" in
  show_cons lc1;
  show_cons lc2;
  show_cons lc3;
  show_cons lc4


let _ = test_poly () ; test_lin ()
