open Base
open Jl

let mycaml_fn ~args:_ ~kwargs:_ =
  Jl_value.(
    array_any [| float64 42.; string "foobar"; tuple [| int 1337; string "baz" |] |])

let myother_fn =
  let%map_open.Jl x = positional "x" int ~docstring:"X"
  and y = keyword "y" string ~default:"foobar" ~docstring:"Y" in
  let res = List.init x ~f:(fun _ -> y) |> String.concat ~sep:"|" in
  Jl_value.string res

let yetanother_fn =
  let%map_open.Jl x = positional "x" int ~docstring:"X" in
  Stdio.printf "%d\n%!" x;
  Jl_value.(tuple [| nothing; int 1; int 2; int (x + 42) |])

let () =
  Stdio.printf "Hello from ocaml!\n%!";
  Register.func "mycaml_fn" ~f:mycaml_fn ~modl:Wrapper.Jl_module.main;
  Register.defunc "myother_fn" myother_fn ~modl:Wrapper.Jl_module.main;
  Register.defunc "yetanother_fn" yetanother_fn ~modl:Wrapper.Jl_module.main
