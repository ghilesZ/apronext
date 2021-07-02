include Domain.Make (struct
  type t = Box.t

  let man = Box.manager_alloc ()
end)

(** given a box, builds an iterator over triplets : variables, type, interval
    e.g: [let it = iterator b in it (); it (); it()].

    @raise Exit when all dimension have been iterated on. *)
let iterator b =
  let {box1_env; interval_array} = to_box1 b in
  let i = ref 0 in
  let len = Array.length interval_array in
  fun () ->
    if !i >= len then raise Exit
    else
      let v = E.var_of_dim box1_env !i in
      let typ = E.typ_of_var box1_env v in
      let itv = interval_array.(!i) in
      incr i ; (v, typ, itv)

(** Difference operator for boxes. WARNING, real variable have a floatting point
    semantics, e.g [\[0.;1.\] - \[0.5; 1.\]= \[0.; 0.499999999\]] *)
let diff_float (b1 : t) (b2 : t) : t list =
  let rec product f b = function
    | [] -> []
    | hd :: tl -> List.rev_map (f hd) b |> List.rev_append (product f b tl)
  in
  if meet b1 b2 |> is_bottom then [b1]
  else
    let next = iterator b2 in
    let rec aux a (acc : t list) =
      match next () with
      | v, t, i_b ->
          let i_a = bound_variable a v in
          let d =
            if t = INT then I.diff_int i_a i_b else I.diff_float i_a i_b
          in
          let assign b i = assign_interval b v i in
          aux (assign_interval a v i_b) (product assign d acc)
      | exception Exit -> acc
    in
    aux b1 [b1]

(** compact pretty printing *)
let pp_print fmt b =
  let it = iterator b in
  let rec loop acc =
    match it () with
    | v, t, i -> loop ((v, t, i) :: acc)
    | exception Exit -> List.rev acc
  in
  let vars = loop [] in
  let print_itv_int fmt i =
    let zinf = Scalarext.to_mpqf i.I.inf |> Mpqf.to_mpzf2 |> fst
    and zsup = Scalarext.to_mpqf i.I.sup |> Mpqf.to_mpzf2 |> fst in
    Format.fprintf fmt "[%a; %a]" Mpz.print zinf Mpz.print zsup
  in
  let itv_print = function E.INT -> print_itv_int | E.REAL -> I.print in
  Format.fprintf fmt "[|%a|]"
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt "; ")
       (fun fmt (v, t, i) ->
         Format.fprintf fmt "%a ∊ %a" Apron.Var.print v (itv_print t) i))
    vars
