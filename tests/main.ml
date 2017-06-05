open Mystdlib

let build_env =
  let gen_id = counter 0 in
  fun dim -> iter (fun acc -> ("v_"^(soi @@ gen_id()))::acc) [] dim

let two_first = function
  | a::b::_ -> (a,b)
  | _ -> failwith "need two vars"


let three_first = function
  | a::b::c::_ -> (a,b,c)
  | _ -> failwith "need two vars"


let draw (a,b) (c,d) =
  let a = int_of_float a and b = int_of_float b
  and c = int_of_float c and d = int_of_float d in
  Graphics.draw_segments [|a,b,c,d|]

let test_pol env pts =
  let (a,b,c) = three_first env in
  let pol = Apol.make_of_float_points env pts in
  let (l,h) = Apol.bound_variable_fs pol c in
  for i = iof l to iof h do
    let cut = Apol.assign_float_s pol c (foi i) in
    Graphics.clear_graph ();
    Apol.draw2d draw (a,b) cut;
    Unix.sleepf 0.01
  done

let test_box env pts =
  let (a,b,c) = three_first env in
  let box = Abox.make_of_float_points env pts in
  let (l,h) = Abox.bound_variable_fs box c in
  for i = iof l to iof h do
    let cut = Abox.assign_float_s box c (foi i) in
    Graphics.clear_graph ();
    Abox.draw2d draw (a,b) cut;
    Unix.sleepf 0.005
  done

let test_oct env pts =
  let (a,b,c) = three_first env in
  let box = Aoct.make_of_float_points env pts in
  let (l,h) = Aoct.bound_variable_fs box c in
  for i = iof l to iof h do
    let cut = Aoct.assign_float_s box c (foi i) in
    Graphics.clear_graph ();
    Aoct.draw2d draw (a,b) cut;
    Unix.sleepf 0.01
  done

let _ =
  Graphics.open_graph " 800x800";
  Graphics.set_window_title "Apron domains";
  Graphics.loop_at_exit [] (fun _ -> ());
  Random.self_init();
  let dim = 3 in
  (* let nb_point = 12 in *)
  (* let rand_point () = *)
  (*   iter (fun acc -> ((Random.float 400.) +. 200.)::acc ) [] dim *)
  (* in *)
  (* let pts = iter (fun acc -> (rand_point ())::acc) [] nb_point in *)
  let pts = [[200.;200.;0.];
             [400.;200.;0.];
             [300.;400.;0.];
             [300.;300.;200.]]
  in
  test_oct (build_env dim) pts;
  List.iter (fun l ->
      let (a,b) = two_first l in
      let a = int_of_float a and b = int_of_float b in
      Graphics.fill_circle a b 4
    ) pts
