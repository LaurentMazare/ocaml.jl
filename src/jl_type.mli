open! Base

type 'a t

val int64 : int t
val float64 : float t
val string : string t
val map : 'a t -> wrap:('b -> 'a) -> unwrap:('a -> 'b) -> 'b t
val wrap : 'a t -> 'a -> Jl_value.t
val unwrap : 'a t -> Jl_value.t -> 'a
val data_type : _ t -> Wrapper.Jl_datatype.t
val struct1 : ?modl:Wrapper.Jl_module.t -> string -> field:string * 'a t -> 'a t
