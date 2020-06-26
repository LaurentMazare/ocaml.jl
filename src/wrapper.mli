module Jl_sym : sig
  type t

  val create : string -> t
  val symbol_name : t -> string
end

module Jl_module : sig
  type t

  val create : Jl_sym.t -> parent:t -> t
  val set_module : t -> Jl_sym.t -> t -> unit
  val main : t
  val core : t
  val base : t
  val top : t
end

module Jl_datatype : sig
  type t

  val modl : t
  val string : t
  val bool : t
  val char : t
  val int8 : t
  val int16 : t
  val int32 : t
  val int64 : t
  val uint8 : t
  val uint16 : t
  val uint32 : t
  val uint64 : t
  val float16 : t
  val float32 : t
  val float64 : t
  val errorexception : t
  val any : t
  val tuple : t array -> t

  val create
    :  Jl_sym.t
    -> Jl_module.t
    -> super:t
    -> fields:
         [ `T0
         | `T1 of Jl_sym.t * t
         | `T2 of (Jl_sym.t * t) * (Jl_sym.t * t)
         | `T of (Jl_sym.t * t) array
         ]
    -> abstract:bool
    -> mutable_:bool
    -> ninitialized:int
    -> t

  val set_on_module : t -> Jl_sym.t -> Jl_module.t -> unit
end

module Jl_value : sig
  type t

  (* jl-values have to be protected from the gc. *)
  val nothing : t
  val emptytuple : t
  val true_ : t
  val false_ : t
  val error : string -> t
  val bool : bool -> t
  val float64 : float -> t
  val int : int -> t
  val int64 : Int64.t -> t
  val string : string -> t
  val struct0 : Jl_datatype.t -> t
  val struct1 : Jl_datatype.t -> t -> t
  val struct2 : Jl_datatype.t -> t -> t -> t
  val struct3 : Jl_datatype.t -> t -> t -> t -> t
  val struct4 : Jl_datatype.t -> t -> t -> t -> t -> t
  val struct_n : Jl_datatype.t -> t array -> t
  val tuple : t array -> t
  val tuple_map : 'a array -> f:('a -> t) -> t
  val tuple_with_type : Jl_datatype.t -> t array -> t
  val tuple_map_with_type : Jl_datatype.t -> 'a array -> f:('a -> t) -> t
  val array_any : t array -> t
  val array_any_map : 'a array -> f:('a -> t) -> t
  val is_nothing : t -> bool
  val is_array_any : t -> bool
  val is_bool : t -> bool
  val is_int : t -> bool
  val is_float : t -> bool
  val is_string : t -> bool
  val is_symbol : t -> bool
  val is_tuple : t -> bool
  val is_array : t -> bool
  val nfields : t -> int
  val get_field : t -> string -> t
  val get_nth_field : t -> int -> t
  val to_bool : t -> bool
  val to_int : t -> int
  val to_float : t -> float
  val to_string : t -> string
  val to_symbol : t -> string
  val to_tuple : t -> t array
  val to_tuple2 : t -> t * t
  val to_tuple3 : t -> t * t * t
  val to_tuple4 : t -> t * t * t * t
  val to_array_any : t -> t array
  val typeof : t -> Jl_datatype.t
  val typeof_str : t -> string
  val typeis : t -> Jl_datatype.t -> bool
  val type_error : t -> expected:string -> 'a
  val string_pp : t -> string
end

module Array : sig
  type t

  val create : length:int -> t
  val set : t -> int -> Jl_value.t -> unit
  val get : t -> int -> Jl_value.t
  val length : t -> int
end

module Jl_function : sig
  type t

  val get : Jl_module.t -> string -> t
  val call0 : t -> Jl_value.t
  val call1 : t -> Jl_value.t -> Jl_value.t
  val call2 : t -> Jl_value.t -> Jl_value.t -> Jl_value.t
  val call3 : t -> Jl_value.t -> Jl_value.t -> Jl_value.t -> Jl_value.t
  val call : t -> Jl_value.t list -> Jl_value.t
end

module Exception : sig
  val occurred : unit -> Jl_value.t option
  val current_exception : unit -> Jl_value.t
  val clear : unit -> unit
end

val eval_string : string -> Jl_value.t

val register_fn
  :  string
  -> modl:Jl_module.t
  -> f:(Jl_value.t -> Jl_value.t -> Jl_value.t)
  -> unit

val raise : string -> unit

module Gc : sig
  (* TODO: maybe we should have two distinct types, [Jl_value.unrooted]
     and [Jl_value.rooted] ?
     Using the ocaml type system to enforce some of the static analyzer
     annotations would be nice.
     https://docs.julialang.org/en/v1/devdocs/gc-sa/

     For the time being, we mostly rely on deactivating the gc.
  *)

  (** [with_frame ~n (fun protect -> ...)] creates a new GC frame where a jl-value
  can be created. All the intermediary [Jl_value.t] have to be protected by
  calling [protect] on them. [protect] can be called at most [n] times. *)
  val with_frame : n:int -> ((Jl_value.t -> Jl_value.t) -> Jl_value.t) -> Jl_value.t

  val run_with_no_gc : f:(unit -> 'a) -> 'a
  val protect : Jl_value.t -> unit
  val protect_data_type : Jl_datatype.t -> unit
end
