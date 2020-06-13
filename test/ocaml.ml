open Base
open Jl

let mycaml_fn ~args ~kwargs =
  Stdio.printf "args: %s\n%!"
    ([%sexp_of: Jl_value.t array] args |> Sexplib.Sexp.to_string);
  Stdio.printf "kwargs: %s\n%!"
    ( Map.to_alist kwargs |> [%sexp_of: (string * Jl_value.t) list]
    |> Sexplib.Sexp.to_string );
  Jl_value.Array
    [| Float 42.; String "foobar"; Tuple [| Int 1337; String "baz" |] |]

let myother_fn =
  let%map_open.Jl x = positional "x" int ~docstring:"X"
  and y = keyword "y" string ~default:"foobar" ~docstring:"Y" in
  let res = List.init x ~f:(fun _ -> y) |> String.concat ~sep:"|" in
  Jl_value.String res

let yetanother_fn =
  let%map_open.Jl x = positional "x" int ~docstring:"X" in
  Stdio.printf "%d\n%!" x;
  Jl_value.Tuple [| Nothing; Int 1; Int 2; Int (x + 42) |]

let () =
  Stdio.printf "Hello from ocaml!\n%!";
  Register.func ~fn:mycaml_fn ~name:"mycaml_fn";
  Register.defunc ~fn:myother_fn ~name:"myother_fn";
  Register.defunc ~fn:yetanother_fn ~name:"yetanother_fn"
