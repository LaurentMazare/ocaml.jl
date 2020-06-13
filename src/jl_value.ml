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

let kind_str = function
  | Float _ -> "float"
  | Bool _ -> "bool"
  | Int _ -> "int"
  | String _ -> "string"
  | Array _ -> "array"
  | Tuple _ -> "tuple"
  | Float_array _ -> "float_array"
  | Symbol _ -> "symbol"
  | Nothing -> "nothing"

let errorf fmt = Base.Printf.ksprintf failwith fmt

let type_errorf t ~expected =
  errorf "type mismatch, expected %s, got %s" expected (kind_str t)

let to_int_exn = function
  | Int i -> i
  | t -> type_errorf t ~expected:"int"

let to_float_exn = function
  | Float f -> f
  | t -> type_errorf t ~expected:"float"

let to_bool_exn = function
  | Bool b -> b
  | t -> type_errorf t ~expected:"bool"

let to_string_exn = function
  | String s -> s
  | t -> type_errorf t ~expected:"string"

let tuple2_exn = function
  | Tuple [| t1; t2 |] -> t1, t2
  | Tuple array -> errorf "expected a tuple of length 2, got %d" (Array.length array)
  | t -> type_errorf t ~expected:"tuple2"

let tuple3_exn = function
  | Tuple [| t1; t2; t3 |] -> t1, t2, t3
  | Tuple array -> errorf "expected a tuple of length 3, got %d" (Array.length array)
  | t -> type_errorf t ~expected:"tuple3"

let tuple4_exn = function
  | Tuple [| t1; t2; t3; t4 |] -> t1, t2, t3, t4
  | Tuple array -> errorf "expected a tuple of length 4, got %d" (Array.length array)
  | t -> type_errorf t ~expected:"tuple4"

let tuple_exn = function
  | Tuple ts -> ts
  | t -> type_errorf t ~expected:"tuple"

let array_any_exn = function
  | Array ts -> ts
  | t -> type_errorf t ~expected:"array_any"

let is_nothing = function
  | Nothing -> true
  | _ -> false
