open Base

val func :
  fn:
    (args:Jl_value.t array ->
    kwargs:(string, Jl_value.t, String.comparator_witness) Map.t ->
    Jl_value.t) ->
  name:string ->
  unit

val defunc : fn:Jl_value.t Defunc.t -> name:string -> unit

val no_arg : fn:(unit -> Jl_value.t) -> name:string -> unit
