module Jl_sym : sig
  type t

  val create : string -> t
end

module Jl_module : sig
  type t

  val create : Jl_sym.t -> t
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

  val create
    :  Jl_sym.t
    -> Jl_module.t
    -> super:t
    -> fields:[ `T0 | `T1 of Jl_sym.t * t | `T2 of (Jl_sym.t * t) * (Jl_sym.t * t) ]
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
end

val eval_string : string -> Jl_value.t
val register_fn : string -> f:(Jl_value.t -> Jl_value.t -> Jl_value.t) -> unit

module Gc : sig
  (** [with_frame ~n (fun protect -> ...)] creates a new GC frame where a jl-value
  can be created. All the intermediary [Jl_value.t] have to be protected by
  calling [protect] on them. [protect] can be called at most [n] times. *)
  val with_frame : n:int -> ((Jl_value.t -> Jl_value.t) -> Jl_value.t) -> Jl_value.t
end
