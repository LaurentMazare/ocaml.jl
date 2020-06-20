open Base

val func
  :  string
  -> modl:Wrapper.Jl_module.t
  -> f:
       (args:Jl_value.t array
        -> kwargs:(string, Jl_value.t, String.comparator_witness) Map.t
        -> Jl_value.t)
  -> unit

val defunc : string -> modl:Wrapper.Jl_module.t -> Jl_value.t Defunc.t -> unit
val no_arg : string -> modl:Wrapper.Jl_module.t -> f:(unit -> Jl_value.t) -> unit
