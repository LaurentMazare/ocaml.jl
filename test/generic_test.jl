module Caml
using Libdl

const shared_lib = "_build/default/test/ocaml.so"
lib = Libdl.dlopen(shared_lib)
caml_named_value = Libdl.dlsym(lib, :caml_named_value)
named_fn = Libdl.dlsym(lib, :named_fn)

function __init__()
    ccall(("ocaml_jl_start", shared_lib), Cvoid, ())
end

function fn(name::String)
    ptr = ccall(caml_named_value, Ptr{Cvoid}, (Cstring,), name)
    if ptr == C_NULL
        throw(ArgumentError("cannot find ocaml function '$name'"))
    end
    function (args...; kwargs...)
        kwargs = Any[(k.first, k.second) for k in kwargs]
        ccall(named_fn, Any, (Ptr{Cvoid}, Any, Any), ptr, args, kwargs)
    end
end

export fn
end

fn = Caml.fn("mycaml_fn")
fn(x=1, y=2)
println(fn((1, "foo", [1.2, "bar"])))
for i in 1:3
    println(fn(i, "foo", [1.2, "bar"]))
end

fn2 = Caml.fn("myother_fn")
println(fn2(4))
println(fn2(2; y="test"))
