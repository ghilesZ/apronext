open Apron

type union_5 = Coeff.union_5 = Scalar of Scalar.t | Interval of Interval.t

and t = union_5

(** APRON Coefficients (either scalars or intervals) *)

val s_of_mpq : Mpq.t -> t

val s_of_mpqf : Mpqf.t -> t

val s_of_int : int -> t

val s_of_frac : int -> int -> t
(** Create a scalar coefficient of type [Mpqf.t] from resp.

    - A multi-precision rational [Mpq.t]
    - A multi-precision rational [Mpqf.t]
    - an integer
    - a fraction [x/y] *)

val s_of_float : float -> t
(** Create an interval coefficient of type [Float] with the given value *)

val s_of_mpfr : Mpfr.t -> t
(** Create an interval coefficient of type [Mpfr] with the given value *)

val i_of_scalar : Scalar.t -> Scalar.t -> t
(** Build an interval from a lower and an upper bound *)

val i_of_mpq : Mpq.t -> Mpq.t -> t

val i_of_mpqf : Mpqf.t -> Mpqf.t -> t

val i_of_int : int -> int -> t

val i_of_frac : int -> int -> int -> int -> t

val i_of_float : float -> float -> t

val i_of_mpfr : Mpfr.t -> Mpfr.t -> t
(** Create an interval coefficient from resp. two

    - multi-precision rationals [Mpq.t]
    - multi-precision rationals [Mpqf.t]
    - integers
    - fractions [x/y] and [z/w]
    - machine floats
    - Mpfr floats *)

val is_scalar : t -> bool

val is_interval : t -> bool

val cmp : t -> t -> int
(** Non Total Comparison:

    - If the 2 coefficients are both scalars, corresp. to Scalar.cmp
    - If the 2 coefficients are both intervals, corresp. to Interval.cmp
    - otherwise, -3 if the first is a scalar, 3 otherwise *)

val equal : t -> t -> bool
(** Equality test *)

val is_zero : t -> bool
(** Is the coefficient equal to scalar 0 or interval [0,0] ? *)

val equal_int : t -> int -> bool
(** Is the coefficient equal to scalar b or interval [b,b] ? *)

val neg : t -> t
(** Negation *)

val reduce : t -> t
(** Convert interval to scalar if possible *)

val print : Format.formatter -> t -> unit
(** Printing *)

(** {1 Extensions} *)

val sign : t -> int
(** Returns -1, 0, or 1 when the argument is respectively negative, null, or
    positive. *)

val zero : t
(** the constant 0 *)

val one : t
(** the constant 1 *)

val minus_one : t
(** the constant -1 *)

val of_interval : Interval.t -> t
(** Interval coeff *)

(** {2 conversion operators} *)

val to_float : t -> float
(** [to_float x] converts x to a float. Raises Invalid_arg if x is a scalar *)

val to_mpqf : t -> Mpqf.t
(** [to_float x] converts x to a Mpqf.t. Raises Invalid_arg if x is a scalar *)

val to_int : t -> int
(** [to_float x] converts x to an integer. Raises Invalid_arg if x is a scalar *)
