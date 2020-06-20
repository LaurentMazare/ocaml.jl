open! Ctypes

module C (F : Cstubs.FOREIGN) = struct
  open! F

  module Jl_sym = struct
    type t = unit ptr

    let t : t typ = ptr (typedef void "jl_sym_t")
    let create = foreign "jl_symbol" (string @-> returning t)
    let symbol_n = foreign "jl_symbol_n" (string @-> int @-> returning t)
    let symbol_name = foreign "jl_symbol_name" (t @-> returning string)
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
    let errorexception = foreign_value "jl_errorexception_type" t
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

    (* Types ? *)
    let array_any = foreign_value "jl_array_any_type" t

    (* Creating values *)
    let nothing = foreign_value "jl_nothing" t
    let true_ = foreign_value "jl_true" t
    let false_ = foreign_value "jl_false" t
    let emptytuple = foreign_value "jl_emptytuple" t
    let error_value = foreign "ml_jl_error_value" (string @-> returning t)
    let box_float64 = foreign "jl_box_float64" (float @-> returning t)
    let box_int8 = foreign "jl_box_int8" (int8_t @-> returning t)
    let box_int16 = foreign "jl_box_int16" (int16_t @-> returning t)
    let box_int32 = foreign "jl_box_int32" (int32_t @-> returning t)
    let box_int64 = foreign "jl_box_int64" (int64_t @-> returning t)
    let box_uint8 = foreign "jl_box_uint8" (uint8_t @-> returning t)
    let box_uint16 = foreign "jl_box_uint16" (uint16_t @-> returning t)
    let box_uint32 = foreign "jl_box_uint32" (uint32_t @-> returning t)
    let box_uint64 = foreign "jl_box_uint64" (uint64_t @-> returning t)
    let pchar_to_string = foreign "jl_pchar_to_string" (string @-> int @-> returning t)
    let new_struct0 = foreign "jl_new_struct" (Jl_datatype.t @-> returning t)
    let new_struct1 = foreign "jl_new_struct" (Jl_datatype.t @-> t @-> returning t)
    let new_struct2 = foreign "jl_new_struct" (Jl_datatype.t @-> t @-> t @-> returning t)

    let new_struct3 =
      foreign "jl_new_struct" (Jl_datatype.t @-> t @-> t @-> t @-> returning t)

    let new_struct4 =
      foreign "jl_new_struct" (Jl_datatype.t @-> t @-> t @-> t @-> t @-> returning t)

    let new_structv =
      foreign "jl_new_structv" (Jl_datatype.t @-> ptr t @-> int @-> returning t)

    (* Reading values *)
    let is_nothing = foreign "jl_is_nothing" (t @-> returning bool)
    let is_bool = foreign "jl_is_bool" (t @-> returning bool)
    let is_symbol = foreign "jl_is_symbol" (t @-> returning bool)
    let is_int8 = foreign "jl_is_int8" (t @-> returning bool)
    let is_int16 = foreign "jl_is_int16" (t @-> returning bool)
    let is_int32 = foreign "jl_is_int32" (t @-> returning bool)
    let is_int64 = foreign "jl_is_int64" (t @-> returning bool)
    let is_uint8 = foreign "jl_is_uint8" (t @-> returning bool)
    let is_uint16 = foreign "jl_is_uint16" (t @-> returning bool)
    let is_uint32 = foreign "jl_is_uint32" (t @-> returning bool)
    let is_uint64 = foreign "jl_is_uint64" (t @-> returning bool)
    let is_string = foreign "jl_is_string" (t @-> returning bool)
    let is_tuple = foreign "jl_is_tuple" (t @-> returning bool)
    let is_array = foreign "jl_is_array" (t @-> returning bool)
    let string_len = foreign "jl_string_len" (t @-> returning int)
    let string_data = foreign "jl_string_data" (t @-> returning (ptr char))
    let string_ptr = foreign "jl_string_ptr" (t @-> returning string)
    let unbox_float32 = foreign "jl_unbox_float32" (t @-> returning float)
    let unbox_float64 = foreign "jl_unbox_float64" (t @-> returning float)
    let unbox_int8 = foreign "jl_unbox_int8" (t @-> returning int8_t)
    let unbox_int16 = foreign "jl_unbox_int16" (t @-> returning int16_t)
    let unbox_int32 = foreign "jl_unbox_int32" (t @-> returning int32_t)
    let unbox_int64 = foreign "jl_unbox_int64" (t @-> returning int64_t)
    let unbox_uint8 = foreign "jl_unbox_uint8" (t @-> returning uint8_t)
    let unbox_uint16 = foreign "jl_unbox_uint16" (t @-> returning uint16_t)
    let unbox_uint32 = foreign "jl_unbox_uint32" (t @-> returning uint32_t)
    let unbox_uint64 = foreign "jl_unbox_uint64" (t @-> returning uint64_t)
    let get_nth_field = foreign "jl_get_nth_field" (t @-> int @-> returning t)
    let nfields = foreign "jl_nfields" (t @-> returning int)
    let get_field = foreign "jl_get_field" (t @-> string @-> returning t)
    let typeof = foreign "jl_typeof" (t @-> returning Jl_datatype.t)
    let typeof_str = foreign "jl_typeof_str" (t @-> returning string)
    let typeis = foreign "jl_typeis" (t @-> Jl_datatype.t @-> returning bool)
    let to_bool = foreign "ml_jl_to_bool" (t @-> returning bool)

    let apply_tuple_type_v =
      foreign "jl_apply_tuple_type_v" (ptr t @-> int @-> returning Jl_datatype.t)
  end

  module Jl_array = struct
    type t = unit ptr

    let t : t typ = ptr (typedef void "jl_array_t")
    let alloc_vec_any = foreign "jl_alloc_vec_any" (int @-> returning t)

    let array_ptr_set =
      foreign "jl_array_ptr_set" (t @-> int @-> Jl_value.t @-> returning void)

    let array_ptr_ref = foreign "jl_array_ptr_ref" (t @-> int @-> returning Jl_value.t)
    let array_len = foreign "jl_array_len" (t @-> returning int)
  end

  module Exception = struct
    let occurred = foreign "jl_exception_occurred" (void @-> returning Jl_value.t)
    let current_exception = foreign "jl_current_exception" (void @-> returning Jl_value.t)
    let clear = foreign "jl_exception_clear" (void @-> returning void)
  end

  module Gc = struct
    let enable = foreign "jl_gc_enable" (bool @-> returning bool)
  end

  let set_const =
    foreign "jl_set_const" (Jl_module.t @-> Jl_sym.t @-> Jl_value.t @-> returning void)

  let eval_string = foreign "jl_eval_string" (string @-> returning Jl_value.t)
  let raise = foreign "jl_error" (string @-> returning void)
  let gc_push_args = foreign "ml_jl_gc_push_args" (int @-> returning (ptr Jl_value.t))
  let gc_pop = foreign "ml_jl_gc_pop" (void @-> returning void)
end
