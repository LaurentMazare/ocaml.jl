open! Base

type t =
  | Float of float
  | Bool of bool
  | Int of int
  | String of string
  | Array of t array
  | Tuple of t array
  | Float_array of float array
  | Symbol of string
  | Nothing
[@@deriving sexp]

val kind_str : t -> string
val to_int_exn : t -> int
val to_float_exn : t -> float
val to_bool_exn : t -> bool
val to_string_exn : t -> string
val tuple2_exn : t -> t * t
val tuple3_exn : t -> t * t * t
val tuple4_exn : t -> t * t * t * t
val tuple_exn : t -> t array
val array_any_exn : t -> t array
val is_nothing : t -> bool
