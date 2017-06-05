open Apron

let scalar_to_float = function
  | Scalar.Mpqf x -> Mpqf.to_float x
  | Scalar.Float x -> x
  | Scalar.Mpfrf x -> Mpfrf.to_float ~round:Mpfr.Near x

let coeff_to_float = function
  | Coeff.Scalar x -> scalar_to_float x
  | Coeff.Interval i -> scalar_to_float i.Interval.inf
