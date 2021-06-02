open Apron
module S = Scalarext
include Interval

let join a b =
  { inf= (if Scalar.cmp a.inf b.inf < 0 then a.inf else b.inf)
  ; sup= (if Scalar.cmp a.sup b.sup > 0 then a.sup else b.sup) }

let meet a b : t =
  let inf = if Scalar.cmp a.inf b.inf > 0 then a.inf else b.inf in
  let sup = if Scalar.cmp a.sup b.sup < 0 then a.sup else b.sup in
  if Scalar.cmp inf sup < 0 then {inf; sup} else bottom

let diff a b : t list =
  match (Scalar.cmp a.inf b.inf < 0, Scalar.cmp b.sup a.sup < 0) with
  | true, true -> [{inf= a.inf; sup= b.inf}; {inf= b.sup; sup= a.sup}]
  | true, false -> [{inf= a.inf; sup= b.inf}]
  | false, true -> [{inf= b.sup; sup= a.sup}]
  | false, false -> []

let shrink_int {inf; sup} =
  let inf = Scalarext.ceil inf in
  let sup = Scalarext.floor sup in
  if Scalar.cmp inf sup < 0 then {inf; sup} else bottom

let shrink_float {inf; sup} =
  let inf = Scalarext.to_float_up inf |> Scalarext.of_float in
  let sup = Scalarext.to_float_down sup |> Scalarext.of_float in
  if Scalar.cmp inf sup < 0 then {inf; sup} else bottom

let to_float a = (S.to_float a.inf, S.to_float a.sup)

let to_mpqf a = (S.to_mpqf a.inf, S.to_mpqf a.sup)

(** scalar range of an interval *)
let range a = S.sub a.sup a.inf

(** same as range but result as an mpqf *)
let range_mpqf a = S.sub a.sup a.inf |> S.to_mpqf

(** midpoint of an interval *)
let mid a = S.add a.inf (S.div (S.sub a.sup a.inf) (S.of_int 2))

(** Random uniform value within an interval, according to the type *)
let spawn ({inf; sup} : t) =
  let inf = S.to_mpqf inf and sup = S.to_mpqf sup in
  let r = Mpqf.of_float (Random.float 1.) in
  Mpqf.(add inf (mul (sub sup inf) r))

(** returns true if and only if both bounds are finite *)
let is_bounded ({inf; sup} : t) =
  Scalar.is_infty inf = 0 && Scalar.is_infty sup = 0
