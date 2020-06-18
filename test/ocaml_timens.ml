open Core_kernel
open Jl

let wrap time = Jl_value.Int (Time_ns.to_int_ns_since_epoch time)
let wrap_span span = Jl_value.Int (Time_ns.Span.to_int_ns span)

let time_arg =
  Defunc.Of_julia.create ~type_name:"time" ~conv:(function
      | Int i -> Time_ns.of_int_ns_since_epoch i
      | jl_value -> failwithf "expected an int, got %s" (Jl_value.kind_str jl_value) ())

let span_arg =
  Defunc.Of_julia.create ~type_name:"span" ~conv:(function
      | Int i -> Time_ns.Span.of_int_ns i
      | jl_value -> failwithf "expected an int, got %s" (Jl_value.kind_str jl_value) ())

let time_ns_of_string =
  let%map_open.Jl str = positional "str" string ~docstring:"" in
  Time_ns.of_string str |> wrap

let time_ns_to_string =
  let%map_open.Jl time = positional "time" time_arg ~docstring:"" in
  Jl_value.String (Time_ns.to_string time)

let span_of_string =
  let%map_open.Jl str = positional "str" string ~docstring:"" in
  Time_ns.Span.of_string str |> wrap_span

let span_to_string =
  let%map_open.Jl span = positional "span" span_arg ~docstring:"" in
  Jl_value.String (Time_ns.Span.to_string span)

let time_ns_now () = Time_ns.now () |> wrap

let diff =
  let%map_open.Jl time1 = positional "time1" time_arg ~docstring:""
  and time2 = positional "time2" time_arg ~docstring:"" in
  Time_ns.diff time1 time2 |> wrap_span

let add =
  let%map_open.Jl time = positional "time" time_arg ~docstring:""
  and span = positional "span" span_arg ~docstring:"" in
  Time_ns.add time span |> wrap

let () =
  let modl = Wrapper.Jl_sym.create "Foo" |> Wrapper.Jl_module.create in
  Wrapper.(Jl_module.set_module Jl_module.main (Jl_sym.create "Foo") modl);
  let dt =
    Wrapper.Jl_datatype.create
      (Wrapper.Jl_sym.create "MyStruct")
      (* Using modl here results in a segfault. *)
      Wrapper.Jl_module.main
      ~super:Wrapper.Jl_datatype.any
      ~abstract:false
      ~fields:(`T1 (Wrapper.Jl_sym.create "ns", Wrapper.Jl_datatype.int64))
      ~mutable_:false
      ~ninitialized:0
  in
  Wrapper.(Jl_datatype.set_on_module dt (Jl_sym.create "MyStruct") modl);
  Wrapper.register_fn "fn_ocaml_time_ns_now" ~f:(fun _ _ ->
      (* This is unsafe, we should protect the value
         between the last two steps. *)
      Time_ns.now ()
      |> Time_ns.to_int_ns_since_epoch
      |> Wrapper.Jl_value.int
      |> Wrapper.Jl_value.struct1 dt);
  Register.defunc ~fn:time_ns_of_string ~name:"ocaml_time_ns_of_string";
  Register.defunc ~fn:time_ns_to_string ~name:"ocaml_time_ns_to_string";
  Register.defunc ~fn:span_of_string ~name:"ocaml_span_of_string";
  Register.defunc ~fn:span_to_string ~name:"ocaml_span_to_string";
  Register.defunc ~fn:diff ~name:"ocaml_time_ns_diff";
  Register.defunc ~fn:add ~name:"ocaml_time_ns_add";
  Register.no_arg ~fn:time_ns_now ~name:"ocaml_time_ns_now"
