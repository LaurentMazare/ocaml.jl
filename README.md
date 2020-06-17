# ocaml.jl

Prototyping some Julia-OCaml bridge.

The examples can be run with:
```bash
> dune build test/ocaml.so
> julia test/generic_test.jl
```

An OCaml function can be exposed to Julia with the following
code:
```ocaml
let concat_fn =
  let%map_open.Jl x = positional "x" int ~docstring:"X"
  and y = keyword "y" string ~default:"foobar" ~docstring:"Y" in
  let res = List.init x ~f:(fun _ -> y) |> String.concat ~sep:"|" in
  Jl_value.String res

let () =
  Register.defunc ~fn:concat_fn ~name:"ocaml_concat"
```

This can then be called from Julia via:
```julia
fn2 = Caml.fn("myother_fn")
println(fn2(4))
println(fn2(2; y="test"))
```