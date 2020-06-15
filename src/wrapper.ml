open! Base
open! Ctypes
module C = Bindings.C (Jl_ctypes)

module Jl_sym = struct
  type t = C.Jl_sym.t

  let create = C.Jl_sym.create
end

module Jl_module = struct
  type t = C.Jl_module.t

  let create = C.Jl_module.create
  let set_module = C.Jl_module.set_const
  let base = !@C.Jl_module.base
  let core = !@C.Jl_module.core
  let main = !@C.Jl_module.main
  let top = !@C.Jl_module.top
end

module Svec = struct
  let empty = !@C.Jl_svec.empty
  let create1 = C.Jl_svec.create1
  let create2 = C.Jl_svec.create2
end

module Jl_datatype = struct
  type t = C.Jl_datatype.t

  let modl = !@C.Jl_datatype.modl
  let string = !@C.Jl_datatype.string
  let bool = !@C.Jl_datatype.bool
  let char = !@C.Jl_datatype.char
  let int8 = !@C.Jl_datatype.int8
  let int16 = !@C.Jl_datatype.int16
  let int32 = !@C.Jl_datatype.int32
  let int64 = !@C.Jl_datatype.int64
  let uint8 = !@C.Jl_datatype.uint8
  let uint16 = !@C.Jl_datatype.uint16
  let uint32 = !@C.Jl_datatype.uint32
  let uint64 = !@C.Jl_datatype.uint64
  let float16 = !@C.Jl_datatype.float16
  let float32 = !@C.Jl_datatype.float32
  let float64 = !@C.Jl_datatype.float64
  let any = !@C.Jl_datatype.any

  let create name module_ ~super ~fields ~abstract ~mutable_ ~ninitialized =
    let parameters = Svec.empty in
    let fnames, ftypes =
      match fields with
      | `T0 -> Svec.empty, Svec.empty
      | `T1 (s, t) -> Svec.create1 s, Svec.create1 t
      | `T2 ((s1, t1), (s2, t2)) -> Svec.create2 s1 s2, Svec.create2 t1 t2
    in
    C.Jl_datatype.create
      name
      module_
      super
      parameters
      fnames
      ftypes
      (if abstract then 1 else 0)
      (if mutable_ then 1 else 0)
      ninitialized

  let set_on_module t sym modl = C.Jl_module.set_const modl sym t
end
