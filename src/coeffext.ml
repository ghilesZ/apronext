open Apron
module S = Scalarext
include Coeff

let zero = s_of_int 0

let one = s_of_int 1

let minus_one = s_of_int (-1)

let sign c = cmp c zero

let of_interval i = Interval i

let to_float = function
  | Scalar x -> S.to_float x
  | Interval _ -> invalid_arg "cant convert a coeff.interval to float"

let to_mpqf = function
  | Scalar x -> S.to_mpqf x
  | Interval _ -> invalid_arg "cant convert a coeff.interval to mpqf"

let to_int x = to_float x |> int_of_float
