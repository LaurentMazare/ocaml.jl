using Libdl
const shared_lib = "_build/default/test/ocaml.so"

function start_ocaml()
    lib = Libdl.dlopen(shared_lib)
    ccall(("ocaml_jl_start", shared_lib), Cvoid, ())
end

start_ocaml()

fn = Main.mycaml_fn
Main.mycaml_fn(x=1, y=2)
println(fn((1, "foo", [1.2, "bar"])))
for i in 1:3
    println(fn(i, "foo", [1.2, "bar"]))
end

fn2 = Main.myother_fn
println(fn2(4))
println(fn2(2; y="test"))

fn2 = Main.yetanother_fn
println(fn2(4))
println(typeof(fn2(4)))
println(fn2(4)[2])
