using Libdl
const shared_lib = "_build/default/test/ocaml_timens.so"

function start_ocaml()
    lib = Libdl.dlopen(shared_lib)
    ccall(("ocaml_jl_start", shared_lib), Cvoid, ())
end

start_ocaml()

module T

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
println(Main.fn_ocaml_time_ns_now, Main.ocaml_time_ns_now)
now = Main.fn_ocaml_time_ns_now()
println(now)
now_str = Main.fn_ocaml_time_ns_to_string(now)
println("now: $now, now_str: $now_str")
