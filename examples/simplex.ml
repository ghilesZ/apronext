open Apronext

let env = Environmentext.make_s [||] [|"x"; "y"|]

let gens =
  [ Generatorext.of_float_point env [0.; 0.]
  ; Generatorext.of_float_point env [0.; 100.]
  ; Generatorext.of_float_point env [50.; 200.]
  ; Generatorext.of_float_point env [100.; 0.] ]

let pol = Apol.of_generator_list gens

let () =
  let simplices = Apol.to_simplices pol in
  Format.printf "Decomposing %a\ninto:\n" Apol.pp_print pol;
  List.iter (Format.printf "%a\n" Apol.pp_print) simplices
