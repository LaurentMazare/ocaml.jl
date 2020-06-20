open Core_kernel
open Jl

let span_ns ~modl =
  let span_ns =
    Jl_type.map Jl_type.int64 ~wrap:Time_ns.Span.to_int_ns ~unwrap:Time_ns.Span.of_int_ns
  in
  Jl_type.struct1 "Span" ~modl ~field:("ns", span_ns)

let time_ns ~modl =
  let time_ns =
    Jl_type.map
      Jl_type.int64
      ~wrap:Time_ns.to_int_ns_since_epoch
      ~unwrap:Time_ns.of_int_ns_since_epoch
  in
  Jl_type.struct1 "Time" ~modl ~field:("ns_since_epoch", time_ns)

let () =
  let modl_name = Wrapper.Jl_sym.create "Time_ns" in
  let modl = Wrapper.Jl_module.create modl_name ~parent:Wrapper.Jl_module.main in
  let time_ns = time_ns ~modl in
  let span_ns = span_ns ~modl in
  Register.no_arg "now" ~modl ~f:(fun () -> Time_ns.now () |> Jl_type.wrap time_ns);
  let time_arg = Defunc.Of_julia.of_type time_ns ~type_name:"time" in
  let span_arg = Defunc.Of_julia.of_type span_ns ~type_name:"span" in
  Register.defunc
    "of_string"
    ~modl
    (let%map_open.Jl time = positional "time" string ~docstring:"" in
     Time_ns.of_string time |> Jl_type.wrap time_ns);
  Register.defunc
    "to_string"
    ~modl
    (let%map_open.Jl time = positional "time" time_arg ~docstring:"" in
     Time_ns.to_string time |> Wrapper.Jl_value.string);
  Register.defunc
    "span_of_string"
    ~modl
    (let%map_open.Jl span = positional "span" string ~docstring:"" in
     Time_ns.Span.of_string span |> Jl_type.wrap span_ns);
  Register.defunc
    "span_to_string"
    ~modl
    (let%map_open.Jl span = positional "span" span_arg ~docstring:"" in
     Time_ns.Span.to_string span |> Wrapper.Jl_value.string);
  Register.defunc
    "add"
    ~modl
    (let%map_open.Jl time = positional "time" time_arg ~docstring:""
     and span = positional "span" span_arg ~docstring:"" in
     Time_ns.add time span |> Jl_type.wrap time_ns);
  Register.defunc
    "diff"
    ~modl
    (let%map_open.Jl time1 = positional "time1" time_arg ~docstring:""
     and time2 = positional "time2" time_arg ~docstring:"" in
     Time_ns.diff time1 time2 |> Jl_type.wrap span_ns)
