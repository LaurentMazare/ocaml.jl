using Libdl
const shared_lib = "_build/default/test/ocaml_timens.so"

function start_ocaml()
    lib = Libdl.dlopen(shared_lib)
    ccall(("ocaml_jl_start", shared_lib), Cvoid, ())
end

start_ocaml()

now = Time_ns.now()
println(now)
now_str = Time_ns.to_string(now)
println("now: $now, now_str: $now_str")

span = Time_ns.span_of_string("4m30s")
time = Time_ns.add(now, span)
time_str = Time_ns.to_string(time)
println("$time $time_str")
