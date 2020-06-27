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

jl_value_t *ml_jl_error_value(const char *fmt, ...) {
    va_list args;
    va_start(args, fmt);
    jl_value_t *e = jl_vexceptionf(jl_errorexception_type, fmt, args);
    va_end(args);
    return e;
}

jl_value_t **ml_jl_gc_push_args(int n) {
  jl_value_t **rts_var = (jl_value_t**)malloc((n + 2)*sizeof(jl_value_t*));
  rts_var += 2;
  ((void**)rts_var)[-2] = (void*)JL_GC_ENCODE_PUSHARGS(n);
  ((void**)rts_var)[-1] = jl_pgcstack;
  memset((void*)rts_var, 0, n * sizeof(jl_value_t*));
  jl_pgcstack = (jl_gcframe_t*)&(((void**)rts_var)[-2]);
}

void ml_jl_gc_pop() {
  jl_pgcstack = jl_pgcstack->prev;
}

int8_t ml_jl_to_bool(jl_value_t *t) {
  if (t == jl_true) return 1;
  if (t == jl_false) return 0;
  return jl_unbox_bool(t);
}

jl_module_t *ml_jl_new_module(jl_sym_t* name, jl_module_t* parent) {
  jl_module_t *modl = jl_new_module(name);
  modl->parent = parent;
  jl_set_const(parent, name, (jl_value_t*)modl);
  return modl;
}
