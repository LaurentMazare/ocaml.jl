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

date = Time_ns.date_of_string("2020-01-16")
ofday = Time_ns.ofday_of_string("13:37")
println(Time_ns.of_date_ofday(date, ofday, "ldn"))

d1 = Time_ns.date_of_string("2020-01-16")
d2 = Time_ns.date_of_string("2020-01-23")
println(Time_ns.weekdays_between(d1, d2))

using InteractiveUtils
println(InteractiveUtils.varinfo())
println(InteractiveUtils.varinfo(Time_ns))
println(Time_ns.diff(time, now))
