module Caml
using Libdl

const shared_lib = "_build/default/test/ocaml_timens.so"
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

module T

using ..Caml

struct TimeNs
    ns_since_epoch::Int64
end

struct Span
    ns::Int64
end

of_string(str::String) = TimeNs(Main.ocaml_time_ns_of_string(str))
Base.show(io::IO, time::TimeNs) = print(io, Main.ocaml_time_ns_to_string(time.ns_since_epoch))

span_of_string(str::String) = Span(Main.ocaml_span_of_string(str))
Base.show(io::IO, span::Span) = print(io, Main.ocaml_span_to_string(span.ns))

now() = TimeNs(Main.ocaml_time_ns_now())

Base.:+(time::TimeNs, span::Span) = TimeNs(Main.ocaml_time_ns_add(time.ns_since_epoch, time.ns))
Base.:-(lhs::TimeNs, rhs::TimeNs) = Span(Main.ocaml_time_ns_diff(lhs.ns_since_epoch, rhs.ns_since_epoch))
end

time = T.of_string("2020-01-16 15:15:00.123456789+1")
println(time.ns_since_epoch)
println(time)
now = T.now()
println(now)
println(now - time)
println(Foo.MyStruct)
println(fieldnames(Foo.MyStruct), fieldtypes(Foo.MyStruct))
println(Main.fn_ocaml_time_ns_now)