open Apronext

let env = Environmentext.make_s [|"x"; "z"|] [|"y"|]

let g1 =
  [ Generatorext.of_float_point env [0.; 0.; 0.]
  ; Generatorext.of_float_point env [0.; 100.; 50.]
  ; Generatorext.of_float_point env [50.; 200.; 100.]
  ; Generatorext.of_float_point env [100.; 0.; 150.] ]

let g2 =
  [ Generatorext.of_float_point env [10.; 10.; 10.]
  ; Generatorext.of_float_point env [20.; 300.; 40.]
  ; Generatorext.of_float_point env [30.; 180.; 50.]
  ; Generatorext.of_float_point env [80.; 10.; 80.] ]

let b1 = Abox.of_generator_list g1

let b2 = Abox.of_generator_list g2

let () =
  let diff = Abox.diff_float b1 b2 in
  Format.printf "@[%a@]@. - @[%a@]@.=\n" Abox.pp_print b1 Abox.pp_print b2 ;
  List.iter (Format.printf "%a@." Abox.pp_print) diff
