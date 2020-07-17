open Apron
module S = Scalarext
include Coeff

let to_float = function
  | Coeff.Scalar x -> S.to_float x
  | Coeff.Interval _ -> failwith "cant convert a coeff.interval to float"

let to_mpqf = function
  | Coeff.Scalar x -> S.to_mpqf x
  | Coeff.Interval _ -> failwith "cant convert a coeff.interval to mpqf"

let to_int x = to_float x |> int_of_float
