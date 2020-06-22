open! Base

type 'a t

val wrap : 'a t -> 'a -> Jl_value.t
val unwrap : 'a t -> Jl_value.t -> 'a
val data_type : _ t -> Wrapper.Jl_datatype.t
val map : 'a t -> wrap:('b -> 'a) -> unwrap:('a -> 'b) -> 'b t

(* Basic types *)
val int64 : int t
val float64 : float t
val string : string t

(* Structs *)
val struct1 : ?modl:Wrapper.Jl_module.t -> string -> field:string * 'a t -> 'a t

val struct2
  :  ?modl:Wrapper.Jl_module.t
  -> string
  -> field1:string * 'a t
  -> field2:string * 'b t
  -> ('a * 'b) t
