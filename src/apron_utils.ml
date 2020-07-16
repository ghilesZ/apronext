open Apron
open Scalar

let scalar_to_mpqf = function
  | Scalar.Mpqf x -> x
  | Scalar.Float x -> Mpqf.of_float x
  | Scalar.Mpfrf x -> Mpfrf.to_mpqf x

let scalar_to_float s =
  let res = match s with
    | Mpqf x -> Mpqf.to_float x
    | Float x -> x
    | Mpfrf x -> Mpfrf.to_float ~round:Mpfr.Near x
  in res

let coeff_to_float = function
  | Coeff.Scalar x -> scalar_to_float x
  | Coeff.Interval _ -> failwith "cant convert a coeff.interval to float"

let coeff_to_mpqf = function
  | Coeff.Scalar x -> scalar_to_mpqf x
  | Coeff.Interval i -> scalar_to_mpqf i.Interval.inf

let coeff_to_int x = coeff_to_float x |> int_of_float

(* join mpqf intervals*)
let join_mpqf_itv (l1,u1) (l2,u2) =
  (if Mpqf.cmp l1 l2 < 0 then l1 else l2),
  (if Mpqf.cmp u1 u2 > 0 then u1 else u2)

let itv_to_mpqf i = (scalar_to_mpqf i.Interval.inf, scalar_to_mpqf i.Interval.sup)

(* Compute the euclidian distance between two scalars *)
let diam inf sup =
  let mpqf_inf = scalar_to_mpqf inf in
  let mpqf_sup = scalar_to_mpqf sup in
  Mpqf.sub mpqf_sup mpqf_inf


let sqrt2 = 0.707106781186548

let sqrt2_mpqf = Mpqf.of_float sqrt2

(* Compute the sum of two scalars *)
let scalar_add sca sca' =
  let value = scalar_to_mpqf sca in
  let value' = scalar_to_mpqf sca' in
  let sum = Mpqf.add value value' in
  Scalar.of_mpqf sum

(* Compute the sum of two scalars *)
let scalar_mul_sqrt2 sca =
  let value = scalar_to_mpqf sca in
  let mult = Mpqf.mul value sqrt2_mpqf in
  Scalar.of_mpqf mult

(* Compute the medium value of two scalars *)
let mid inf sup =
  let mpqf_inf = scalar_to_mpqf inf
  and mpqf_sup = scalar_to_mpqf sup in
  let div_inf = Mpqf.div mpqf_inf (Mpqf.of_int 2)
  and div_sup = Mpqf.div mpqf_sup (Mpqf.of_int 2)
  in Scalar.of_mpqf (Mpqf.add div_inf div_sup)

(* Compute the middle value of an interval *)
let mid_interval itv =
  mid itv.Interval.inf itv.Interval.sup

(* Compute the diameter of an interval *)
let diam_interval itv =
  diam itv.Interval.inf itv.Interval.sup
