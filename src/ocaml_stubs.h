#ifndef _JL_OCAML_STUBS_
#define _JL_OCAML_STUBS_

#include<julia/julia.h>

// First argument is positional args, represented as a tuple or
// an array, the second argument is for keyword args represented
// as an array of pairs (symbol, value).
typedef jl_value_t* (*jl_fn_ptr)(jl_value_t*, jl_value_t*);

jl_value_t *ml_jl_error_value(const char*, ...);
jl_value_t **ml_jl_gc_push_args(int n);
void ml_jl_gc_pop();
int8_t ml_jl_to_bool(jl_value_t*);
#endif
