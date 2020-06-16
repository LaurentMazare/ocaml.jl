open! Ctypes

module C (F : Cstubs.FOREIGN) = struct
  open! F

  module Jl_sym = struct
    type t = unit ptr

    let t : t typ = ptr (typedef void "jl_sym_t")
    let create = foreign "jl_symbol" (string @-> returning t)
  end

  module Jl_module = struct
    type t = unit ptr

    let t : t typ = ptr (typedef void "jl_module_t")
    let create = foreign "jl_new_module" (Jl_sym.t @-> returning t)
    let main = foreign_value "jl_main_module" t
    let core = foreign_value "jl_core_module" t
    let base = foreign_value "jl_base_module" t
    let top = foreign_value "jl_top_module" t
  end

  module Jl_svec = struct
    type t = unit ptr

    let t : t typ = ptr (typedef void "jl_svec_t")
    let empty = foreign_value "jl_emptysvec" t
    let create1 = foreign "jl_svec1" (ptr void @-> returning t)
    let create2 = foreign "jl_svec2" (ptr void @-> ptr void @-> returning t)
  end

  module Jl_datatype = struct
    type t = unit ptr

    let t : t typ = ptr (typedef void "jl_datatype_t")
    let modl = foreign_value "jl_module_type" t
    let string = foreign_value "jl_string_type" t
    let bool = foreign_value "jl_bool_type" t
    let char = foreign_value "jl_char_type" t
    let int8 = foreign_value "jl_int8_type" t
    let int16 = foreign_value "jl_int16_type" t
    let int32 = foreign_value "jl_int32_type" t
    let int64 = foreign_value "jl_int64_type" t
    let uint8 = foreign_value "jl_uint8_type" t
    let uint16 = foreign_value "jl_uint16_type" t
    let uint32 = foreign_value "jl_uint32_type" t
    let uint64 = foreign_value "jl_uint64_type" t
    let float16 = foreign_value "jl_float16_type" t
    let float32 = foreign_value "jl_float32_type" t
    let float64 = foreign_value "jl_float64_type" t
    let any = foreign_value "jl_any_type" t

    let create =
      foreign
        "jl_new_datatype"
        (Jl_sym.t (* name *)
        @-> Jl_module.t (* module *)
        @-> t (* super *)
        @-> Jl_svec.t (* parameters *)
        @-> Jl_svec.t (* fnames *)
        @-> Jl_svec.t (* ftypes *)
        @-> int (* abstract *)
        @-> int (* mutable *)
        @-> int (* ninitialized *)
        @-> returning t)
  end

  module Jl_value = struct
    type t = unit ptr

    let t : t typ = ptr (typedef void "jl_value_t")
  end

  let set_const =
    foreign "jl_set_const" (Jl_module.t @-> Jl_sym.t @-> Jl_value.t @-> returning void)

  let eval_string = foreign "jl_eval_string" (string @-> returning Jl_value.t)
end
