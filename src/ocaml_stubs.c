#include<stdio.h>
#include<stdint.h>
#include<caml/alloc.h>
#include<caml/callback.h>
#include<caml/memory.h>
#include<caml/mlvalues.h>
#include<caml/printexc.h>

#include "ocaml_stubs.h"

void ocaml_jl_start() {
  char *argv[2];
  argv[0] = "test";
  argv[1] = NULL;
  caml_startup(argv);
}

void ocaml_to_jl(value v, jl_value_t **jl_res) {
  CAMLparam1(v);
  CAMLlocal1(arg);
  if (v == Val_int(0)) {
    *jl_res = jl_nothing;
    return;
  }
  int tag = Tag_val(v);
  arg = Field(v, 0);
  if (tag == 0) {
    *jl_res = jl_box_float64(Double_val(arg));
  }
  else if (tag == 1) {
    *jl_res = Bool_val(arg) ? jl_true : jl_false;
  }
  else if (tag == 2) {
    long long int v = Long_val(arg);
    *jl_res = jl_box_int64(v);
  }
  else if (tag == 3) {
    *jl_res = jl_pchar_to_string(String_val(arg), caml_string_length(arg));
  }
  else if (tag == 4) {
    size_t len = Wosize_val(arg);
    *jl_res = (jl_value_t*)jl_alloc_vec_any(len);
    for (size_t i = 0; i < len; ++i) {
      jl_value_t *elem = 0;
      JL_GC_PUSH1(&elem);
      ocaml_to_jl(Field(arg, i), &elem);
      jl_array_ptr_set((jl_array_t*)*jl_res, i, elem);
      JL_GC_POP();
    }
  }
  else if (tag == 5) {
    size_t len = Wosize_val(arg);
    jl_datatype_t *dt = 0;
    jl_value_t **elems = 0;
    JL_GC_PUSHARGS(elems, 2*len);
    for (size_t i = 0; i < len; ++i) {
      ocaml_to_jl(Field(arg, i), elems + i);
      // Recompute the element type via jl_typeof, this is
      // certainly not optimal...
      *(elems + len + i) = jl_typeof(*(elems + i));
    }
    dt = jl_apply_tuple_type_v(elems + len, len);
    *jl_res = jl_new_structv(dt, elems, len);
    JL_GC_POP();
  }
  else if (tag == 6) {
    jl_errorf("TODO float array");
  }
  else if (tag == 7) {
    *jl_res = (jl_value_t*)jl_symbol_n(String_val(arg), caml_string_length(arg));
  }
  else
    jl_errorf("unexpected tag %d", tag);
}

value jl_to_ocaml(jl_value_t* v) {
  CAMLparam0();
  CAMLlocal2(res, arg);
  int tag = -1;
  if (jl_is_nothing(v)) {
    return Val_int(0);
  }
  if (jl_typeis(v, jl_float64_type)) {
    double d = jl_unbox_float64(v);
    arg = caml_copy_double(d);
    tag = 0;
  }
  else if (jl_typeis(v, jl_float32_type)) {
    double d = jl_unbox_float32(v);
    arg = caml_copy_double(d);
    tag = 0;
  }
  else if (jl_typeis(v, jl_bool_type)) {
    int8_t d = jl_unbox_bool(v);
    arg = d ? Val_true : Val_false;
    tag = 1;
  }
  else if (jl_typeis(v, jl_uint64_type)) {
    arg = Val_long(jl_unbox_uint64(v));
    tag = 2;
  }
  else if (jl_typeis(v, jl_uint32_type)) {
    arg = Val_long(jl_unbox_uint32(v));
    tag = 2;
  }
  else if (jl_typeis(v, jl_uint16_type)) {
    arg = Val_long(jl_unbox_uint16(v));
    tag = 2;
  }
  else if (jl_typeis(v, jl_uint8_type)) {
    arg = Val_long(jl_unbox_uint8(v));
    tag = 2;
  }
  else if (jl_typeis(v, jl_int64_type)) {
    arg = Val_long(jl_unbox_int64(v));
    tag = 2;
  }
  else if (jl_typeis(v, jl_int32_type)) {
    arg = Val_long(jl_unbox_int32(v));
    tag = 2;
  }
  else if (jl_typeis(v, jl_int16_type)) {
    arg = Val_long(jl_unbox_int16(v));
    tag = 2;
  }
  else if (jl_typeis(v, jl_int8_type)) {
    arg = Val_long(jl_unbox_int8(v));
    tag = 2;
  }
  else if (jl_typeis(v, jl_string_type)) {
    arg = caml_alloc_initialized_string(jl_string_len(v), jl_string_data(v));
    tag = 3;
  }
  else if (jl_typeis(v, jl_array_any_type)) {
    size_t len = jl_array_len(v);
    CAMLlocalN(elems, len);
    arg = caml_alloc(len, 0);
    for (size_t i = 0; i < len; ++i) {
      elems[i] = jl_to_ocaml(jl_array_ptr_ref(v, i));
      Field(arg, i) = elems[i];
    }
    tag = 4;
  }
  else if (jl_is_tuple(v)) {
    size_t len = jl_nfields(v);
    CAMLlocalN(elems, len);
    arg = caml_alloc(len, 0);
    for (size_t i = 0; i < len; ++i) {
      elems[i] = jl_to_ocaml(jl_fieldref(v, i));
      Field(arg, i) = elems[i];
    }
    tag = 5;
  }
  else if (jl_is_array(v) && ((jl_array_t*)v)->flags.ptrarray == 0 && jl_array_eltype(v) == jl_float64_type) {
    size_t len = jl_array_len(v);
    arg = caml_alloc(len, Double_array_tag);
    for (size_t i = 0; i < len; ++i) {
      Double_field(arg, i) = *((double*)jl_array_data(v) + i);
    }
    tag = 6;
  }
  else if (jl_is_symbol(v)) {
    arg = caml_copy_string(jl_symbol_name((jl_sym_t*)v));
    tag = 7;
  }
  if (tag == -1) {
    if (jl_is_array(v)) {
      jl_errorf("unsupported array type %s %s", jl_typeof_str(v), jl_typename_str(jl_array_eltype(v)));
    }
    jl_errorf("unsupported type %s", jl_typeof_str(v));
  }
  res = caml_alloc(1, tag);
  Store_field(res, 0, arg);
  CAMLreturn(res);
}

jl_value_t* named_fn(value* fn, jl_value_t* jl_args, jl_value_t* jl_kwargs) {
  CAMLparam0();
  CAMLlocal3(res, args, kwargs);
  args = jl_to_ocaml(jl_args);
  kwargs = jl_to_ocaml(jl_kwargs);
  res = caml_callback2_exn(*fn, args, kwargs);
  if(Is_exception_result(res)) {
    res = Extract_exception(res);
    jl_errorf("ocaml raised: %s", caml_format_exception(res));
  }
  jl_value_t *jl_res = 0;
  JL_GC_PUSH1(&jl_res);
  // What if the call to ocaml_to_jl triggers an exception?
  ocaml_to_jl(res, &jl_res);
  JL_GC_POP();
  return jl_res;
}
