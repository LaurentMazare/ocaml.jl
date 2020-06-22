open! Core_kernel
open! Jl

type t =
  { time_s : Time_ns.t Jl_type.t
  ; span_s : Time_ns.Span.t Jl_type.t
  ; ofday_s : Time_ns.Ofday.t Jl_type.t
  ; date_s : Date.t Jl_type.t
  }

val register : unit -> t
