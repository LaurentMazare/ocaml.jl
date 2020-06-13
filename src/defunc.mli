open Base

type _ t

module Of_julia : sig
  type 'a t = private
    { type_name : string
    ; conv : Jl_value.t -> 'a
    }

  val create : type_name:string -> conv:(Jl_value.t -> 'a) -> 'a t
end

include Applicative.S with type 'a t := 'a t
include Applicative.Let_syntax with type 'a t := 'a t

val params_docstring : ?docstring:string -> 'a t -> string
val no_arg : (unit -> 'a) -> 'a t

val apply
  :  'a t
  -> Jl_value.t array
  -> (string, Jl_value.t, String.comparator_witness) Map.t
  -> 'a

module Param : sig
  val positional : string -> 'a Of_julia.t -> docstring:string -> 'a t
  val keyword : ?default:'a -> string -> 'a Of_julia.t -> docstring:string -> 'a t
  val keyword_opt : string -> 'a Of_julia.t -> docstring:string -> 'a option t
  val int : int Of_julia.t
  val float : float Of_julia.t
  val bool : bool Of_julia.t
  val string : string Of_julia.t
  val jl_value : Jl_value.t Of_julia.t
  val pair : 'a Of_julia.t -> 'b Of_julia.t -> ('a * 'b) Of_julia.t

  val triple
    :  'a Of_julia.t
    -> 'b Of_julia.t
    -> 'c Of_julia.t
    -> ('a * 'b * 'c) Of_julia.t

  val quadruple
    :  'a Of_julia.t
    -> 'b Of_julia.t
    -> 'c Of_julia.t
    -> 'd Of_julia.t
    -> ('a * 'b * 'c * 'd) Of_julia.t

  val option : 'a Of_julia.t -> 'a option Of_julia.t
  val array : 'a Of_julia.t -> 'a array Of_julia.t
  val remaining_args : docstring:string -> Jl_value.t array t

  val remaining_kwargs
    :  docstring:string
    -> (string, Jl_value.t, String.comparator_witness) Map.t t
end
