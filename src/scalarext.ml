open Apron
include Scalar

let z1 : Mpz.f Mpzf.tt = Mpz.of_int 1

let one = of_int 1

let ceil = function
  | Mpqf x ->
      let x, y = Mpqf.to_mpzf2 x in
      Mpqf (Mpqf.of_mpz2 (Mpzf.cdiv_q x y) z1)
  | Mpfrf x ->
      let x = Mpfrf.to_mpqf x in
      let x, y = Mpqf.to_mpzf2 x in
      Mpqf (Mpqf.of_mpz2 (Mpzf.cdiv_q x y) z1)
  | Float x -> Float (ceil x)

let floor = function
  | Mpqf x ->
      let x, y = Mpqf.to_mpzf2 x in
      Mpqf (Mpqf.of_mpz2 (Mpzf.fdiv_q x y) z1)
  | Mpfrf x ->
      let x = Mpfrf.to_mpqf x in
      let x, y = Mpqf.to_mpzf2 x in
      Mpqf (Mpqf.of_mpz2 (Mpzf.fdiv_q x y) z1)
  | Float x -> Float (floor x)

let to_mpqf = function
  | Mpqf x -> x
  | Float x -> Mpqf.of_float x
  | Mpfrf x -> Mpfrf.to_mpqf x

let to_float = function
  | Mpqf x -> Mpqf.to_float x
  | Float x -> x
  | Mpfrf x -> Mpfrf.to_float ~round:Mpfr.Near x

let to_float_up = function
  | Mpqf x ->
      let close = Mpqf.to_float x in
      let closempqf = Mpqf.of_float close in
      if Mpqf.cmp x closempqf < 0 then close else Float.succ close
  | Float x -> x
  | Mpfrf x -> Mpfrf.to_float ~round:Mpfr.Up x

let to_float_down = function
  | Mpqf x ->
      let close = Mpqf.to_float x in
      let closempqf = Mpqf.of_float close in
      if Mpqf.cmp x closempqf < 0 then Float.pred close else close
  | Float x -> x
  | Mpfrf x -> Mpfrf.to_float ~round:Mpfr.Down x

(** scalar addition. result is automatically lifted to mpqf to avoid loss of
    precision *)
let add s1 s2 =
  let x1 = to_mpqf s1 and x2 = to_mpqf s2 in
  Mpqf (Mpqf.add x1 x2)

(** scalar substraction. result is automatically lifted to mpqf to avoid loss of
    precision *)
let sub s1 s2 =
  let x1 = to_mpqf s1 and x2 = to_mpqf s2 in
  Mpqf (Mpqf.sub x1 x2)

(** scalar multiplication. result is automatically lifted to mpqf to avoid loss
    of precision *)
let mul s1 s2 =
  let x1 = to_mpqf s1 and x2 = to_mpqf s2 in
  Mpqf (Mpqf.mul x1 x2)

(** scalar division. result is automatically lifted to mpqf to avoid loss of
    precision *)
let div s1 s2 =
  let x1 = to_mpqf s1 and x2 = to_mpqf s2 in
  Mpqf (Mpqf.div x1 x2)
