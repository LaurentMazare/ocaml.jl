#ifndef _JL_OCAML_STUBS_
#define _JL_OCAML_STUBS_

#include<julia/julia.h>

// First argument is positional args, represented as a tuple or
// an array, the second argument is for keyword args represented
// as an array of pairs (symbol, value).
typedef jl_value_t* (*jl_fn_ptr)(jl_value_t*, jl_value_t*);

jl_value_t *jl_error_value(const char*, ...);
#endif
