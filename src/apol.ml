include Domain.Make (struct
  type t = Polka.strict Polka.t

  let man = Polka.manager_alloc_strict ()
end)

(** set difference *)
let diff (p1 : t) (p2 : t) : t list =
  let work acc a c =
    let neg_c = Linconsext.neg c in
    let a' = filter_lincons a c and s = filter_lincons a neg_c in
    if Abstractext.is_bottom man s then (a, acc) else (a', s :: acc)
  in
  snd
    (Linconsext.array_fold
       (fun (p1, acc) c ->
         if Linconsext.get_typ c = Apron.Lincons1.EQ then
           let c1, c2 = Linconsext.spliteq c in
           let a', acc' = work acc p1 c1 in
           work acc' a' c2
         else work acc p1 c)
       (p1, [])
       (A.to_lincons_array man p2))

(** symbolic join with irredundant result (no overlapp)*)
let join_irredundant (p1 : t) (p2 : t) : t list =
  let m = meet p1 p2 in
  if is_bottom m then [p1; p2] else p1 :: diff p1 m

(** weaker join operator in linear time *)
let weak_join (p1 : t) (p2 : t) : t =
  let l1 = to_lincons_array p1 in
  let l2 = to_lincons_array p2 in
  let sat =
    L.array_fold (fun acc c -> if sat_lincons p2 c then c :: acc else acc) [] l1
  in
  L.array_fold (fun acc c -> if sat_lincons p1 c then c :: acc else acc) sat l2
  |> of_lincons_list p1.env

(** decomposes a polyhedron p into a list of simplices p{_ 1};p{_ 2} ... p{_ n}
    s.t (join p{_ 1} (join p{_ 2} (... (join p{_ n-1} p{_ n})))) = p and all pi
    are simplices ie, their number of generator is less or equal to the number
    of dimensions + 1 *)
let to_simplices =
  let n_first l n =
    let rec loop acc n = function
      | [] -> invalid_arg "n_first: not enough elements in list"
      | h :: tl -> if n > 0 then loop (h :: acc) (n - 1) tl else List.rev acc
    in
    loop [] n l
  in
  fun (pol : t) : t list ->
    let env = get_environment pol in
    let nb = Environmentext.size env + 1 in
    let rec loop acc p =
      let gens = to_generator_list p in
      let nb_gen = List.length gens in
      if nb_gen <= nb then (* simplex *) p :: acc
      else
        let p' = of_generator_list (n_first gens nb) in
        List.fold_left loop (p' :: acc) (diff pol p')
    in
    loop [] pol
