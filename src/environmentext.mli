(** This file is an extension for the Environment from the apron Library *)

(** Note : It only adds function, nothing is removed. Extensions are at the
    end of the module *)

open Apron

type t = Environment.t

type typvar = Environment.typvar = INT | REAL

external make : Var.t array -> Var.t array -> t
  = "camlidl_environment_ap_environment_make"
(** Making an environment from a set of integer and real variables. Raise
    [Failure] in case of name conflict. *)

external add : t -> Var.t array -> Var.t array -> t
  = "camlidl_environment_ap_environment_add"
(** Adding to an environment a set of integer and real variables. Raise
    [Failure] in case of name conflict. *)

external remove : t -> Var.t array -> t
  = "camlidl_environment_ap_environment_remove"
(** Remove from an environment a set of variables. Raise [Failure] in case of
    non-existing variables. *)

external rename : t -> Var.t array -> Var.t array -> t
  = "camlidl_environment_ap_environment_rename"
(** Renaming in an environment a set of variables. Raise [Failure] in case of
    interferences with the variables that are not renamed. *)

external rename_perm : t -> Var.t array -> Var.t array -> t * Dim.perm
  = "camlidl_environment_ap_environment_rename_perm"
(** Similar to previous function, but returns also the permutation on
    dimensions induced by the renaming. *)

external lce : t -> t -> t = "camlidl_environment_ap_environment_lce"
(** Compute the least common environment of 2 environment, that is, the
    environment composed of all the variables of the 2 environments. Raise
    [Failure] if the same variable has different types in the 2 environment.*)

external lce_change : t -> t -> t * Dim.change option * Dim.change option
  = "camlidl_environment_ap_environment_lce_change"
(** Similar to the previous function, but returns also the transformations
    required to convert from [e1] (resp. [e2]) to the lce. If [None] is
    returned, this means that [e1] (resp. [e2]) is identic to the lce.*)

external dimchange : t -> t -> Dim.change
  = "camlidl_environment_ap_environment_dimchange"
(** [dimchange e1 e2] computes the transformation for converting from an
    environment [e1] to a superenvironment [e2]. Raises [Failure] if [e2] is
    not a superenvironment.*)

external dimchange2 : t -> t -> Dim.change2
  = "camlidl_environment_ap_environment_dimchange2"
(** [dimchange2 e1 e2] computes the transformation for converting from an
    environment [e1] to a (compatible) environment [e2], by first adding
    (some) variables of [e2] and then removing (some) variables of [e1].
    Raises [Failure] if the two environments are incompatible. *)

external equal : t -> t -> bool = "camlidl_environment_ap_environment_equal"
(** Test equality if two environments *)

external compare : t -> t -> int
  = "camlidl_environment_ap_environment_compare"
(** Compare two environment. [compare env1 env2] return [-2] if the
    environements are not compatible (a variable has different types in the 2
    environements), [-1] if [env1] is a subset of env2, [0] if equality, [+1]
    if env1 is a superset of env2, and [+2] otherwise (the lce exists and is
    a strict superset of both) *)

external hash : t -> int = "camlidl_environment_ap_environment_hash"
(** Hashing function for environments *)

external dimension : t -> Dim.dimension
  = "camlidl_environment_ap_environment_dimension"
(** Return the dimension of the environment *)

external size : t -> int = "camlidl_environment_ap_environment_size"
(** Return the size of the environment *)

external mem_var : t -> Var.t -> bool
  = "camlidl_environment_ap_environment_mem_var"
(** Return true if the variable is present in the environment. *)

external typ_of_var : t -> Var.t -> typvar
  = "camlidl_environment_ap_environment_typ_of_var"
(** Return the type of variables in the environment. If the variable does not
    belong to the environment, raise a [Failure] exception. *)

external vars : t -> Var.t array * Var.t array
  = "camlidl_environment_ap_environment_vars"
(** Return the (lexicographically ordered) sets of integer and real variables
    in the environment *)

external var_of_dim : t -> Dim.t -> Var.t
  = "camlidl_environment_ap_environment_var_of_dim"
(** Return the variable corresponding to the given dimension in the
    environment. Raise [Failure] is the dimension is out of the range of the
    environment (greater than or equal to [dim env]) *)

external dim_of_var : t -> Var.t -> Dim.t
  = "camlidl_environment_ap_environment_dim_of_var"
(** Return the dimension associated to the given variable in the environment.
    Raise [Failure] if the variable does not belong to the environment. *)

val print :
     ?first:(unit, Format.formatter, unit) format
  -> ?sep:(unit, Format.formatter, unit) format
  -> ?last:(unit, Format.formatter, unit) format
  -> Format.formatter
  -> t
  -> unit
(** Printing *)

(* ====================================================================== *)
(** {3 Extensions} *)
(* ====================================================================== *)

val make_s : string array -> string array -> t
(** environment build from variable string names *)

val empty : t
(** empty environment *)

(** Exception to raise when adding a variable that alread exists in the
    environment *)
exception DuplicateName of string

val add_check : t -> Apron.Var.t array -> Apron.Var.t array -> t
(** same as add but first check if the variables exist in the environement
    and raises DuplicateName is it is the case.*)

val add_one : Apron.Var.t * typvar -> t -> t
(** adds one variable to an environment according to its type *)

val add_one_s : string * typvar -> t -> t
(** same as add_one but with string instread of Var.t *)

val add_int : Apron.Var.t -> t -> t
(** adds an integer variable *)

val add_int_s : string -> t -> t
(** same as add_one but with string instread of Var.t *)

val add_real : Apron.Var.t -> t -> t
(** adds a real variable *)

val add_real_s : string -> t -> t
(** same as add_real but with string instread of Var.t *)

val join : t -> t -> t
(** join of two environments *)

val meet : t -> t -> t
(** meet of two environments *)

val fold : ('a -> Apron.Var.t -> 'a) -> 'a -> t -> 'a
(** Envrironment folding function*)

val iter : (Apron.Var.t -> unit) -> t -> unit
(** Iteration of a function over an environment *)

val get_ints : t -> Apron.Var.t array
(** returns the array of integer variables *)

val get_reals : t -> Apron.Var.t array
(** returns the array of real variables *)
