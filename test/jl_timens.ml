open Core_kernel
open Jl

let date_s ~modl =
  let date =
    Jl_type.map
      Jl_type.int64
      ~wrap:Date.Stable.V1.to_int
      ~unwrap:Date.Stable.V1.of_int_exn
  in
  Jl_type.struct1 "Date" ~modl ~field:("repr", date)

let ofday_s ~modl =
  let ofday =
    Jl_type.map
      Jl_type.int64
      ~wrap:(fun t -> Time_ns.Stable.Ofday.V1.to_int63 t |> Int63.to_int_exn)
      ~unwrap:(fun i -> Int63.of_int_exn i |> Time_ns.Stable.Ofday.V1.of_int63_exn)
  in
  Jl_type.struct1 "OfDay" ~modl ~field:("repr", ofday)

let span_s ~modl =
  let span =
    Jl_type.map Jl_type.int64 ~wrap:Time_ns.Span.to_int_ns ~unwrap:Time_ns.Span.of_int_ns
  in
  Jl_type.struct1 "Span" ~modl ~field:("ns", span)

let time_s ~modl =
  let time =
    Jl_type.map
      Jl_type.int64
      ~wrap:Time_ns.to_int_ns_since_epoch
      ~unwrap:Time_ns.of_int_ns_since_epoch
  in
  Jl_type.struct1 "Time" ~modl ~field:("ns_since_epoch", time)

let zone_arg =
  Defunc.Of_julia.create ~type_name:"zone" ~conv:(fun zone ->
      if Jl_value.is_string zone
      then Jl_value.to_string zone |> Timezone.find_exn
      else Jl_value.type_error zone ~expected:"string")

let arg jl_type ~type_name ~of_string =
  Defunc.Of_julia.create ~type_name ~conv:(fun jl_value ->
      if Jl_value.is_string jl_value
      then Jl_value.to_string jl_value |> of_string
      else Jl_type.unwrap jl_type jl_value)

type t =
  { time_s : Time_ns.t Jl_type.t
  ; span_s : Time_ns.Span.t Jl_type.t
  ; ofday_s : Time_ns.Ofday.t Jl_type.t
  ; date_s : Date.t Jl_type.t
  }

let register () =
  let modl_name = Wrapper.Jl_sym.create "Time_ns" in
  let modl = Wrapper.Jl_module.create modl_name ~parent:Wrapper.Jl_module.main in
  let time_s = time_s ~modl in
  let span_s = span_s ~modl in
  let date_s = date_s ~modl in
  let ofday_s = ofday_s ~modl in
  let time_arg = arg time_s ~type_name:"time" ~of_string:Time_ns.of_string in
  let span_arg = arg span_s ~type_name:"span" ~of_string:Time_ns.Span.of_string in
  let date_arg = arg date_s ~type_name:"date" ~of_string:Date.of_string in
  let ofday_arg = arg ofday_s ~type_name:"ofday" ~of_string:Time_ns.Ofday.of_string in
  let _ =
    Wrapper.eval_string
      "Base.show(io::IO, x::Time_ns.Time) = print(io, Time_ns.to_string(x))"
  in
  let _ =
    Wrapper.eval_string
      "Base.show(io::IO, x::Time_ns.Span) = print(io, Time_ns.span_to_string(x))"
  in
  let _ =
    Wrapper.eval_string
      "Base.show(io::IO, x::Time_ns.OfDay) = print(io, Time_ns.ofday_to_string(x))"
  in
  let _ =
    Wrapper.eval_string
      "Base.show(io::IO, x::Time_ns.Date) = print(io, Time_ns.date_to_string(x))"
  in
  Register.no_arg "now" ~modl ~f:(fun () -> Time_ns.now () |> Jl_type.wrap time_s);
  Register.defunc
    "of_string"
    ~modl
    (let%map_open.Jl time = positional "time" string ~docstring:"" in
     Time_ns.of_string time |> Jl_type.wrap time_s);
  Register.defunc
    "to_string"
    ~modl
    (let%map_open.Jl time = positional "time" time_arg ~docstring:"" in
     Time_ns.to_string time |> Wrapper.Jl_value.string);
  Register.defunc
    "span_of_string"
    ~modl
    (let%map_open.Jl span = positional "span" string ~docstring:"" in
     Time_ns.Span.of_string span |> Jl_type.wrap span_s);
  Register.defunc
    "span_to_string"
    ~modl
    (let%map_open.Jl span = positional "span" span_arg ~docstring:"" in
     Time_ns.Span.to_string span |> Wrapper.Jl_value.string);
  Register.defunc
    "date_of_string"
    ~modl
    (let%map_open.Jl date = positional "date" string ~docstring:"" in
     Date.of_string date |> Jl_type.wrap date_s);
  Register.defunc
    "date_to_string"
    ~modl
    (let%map_open.Jl date = positional "date" date_arg ~docstring:"" in
     Date.to_string date |> Wrapper.Jl_value.string);
  Register.defunc
    "ofday_of_string"
    ~modl
    (let%map_open.Jl ofday = positional "ofday" string ~docstring:"" in
     Time_ns.Ofday.of_string ofday |> Jl_type.wrap ofday_s);
  Register.defunc
    "ofday_to_string"
    ~modl
    (let%map_open.Jl ofday = positional "ofday" ofday_arg ~docstring:"" in
     Time_ns.Ofday.to_string ofday |> Wrapper.Jl_value.string);
  Register.defunc
    "add"
    ~modl
    (let%map_open.Jl time = positional "time" time_arg ~docstring:""
     and span = positional "span" span_arg ~docstring:"" in
     Time_ns.add time span |> Jl_type.wrap time_s);
  Register.defunc
    "diff"
    ~modl
    (let%map_open.Jl time1 = positional "time1" time_arg ~docstring:""
     and time2 = positional "time2" time_arg ~docstring:"" in
     Time_ns.diff time1 time2 |> Jl_type.wrap span_s);
  Register.defunc
    "weekdays_between"
    ~modl
    (let%map_open.Jl min = positional "start" date_arg ~docstring:""
     and max = positional "stop" date_arg ~docstring:"" in
     Date.weekdays_between ~min ~max
     |> Array.of_list
     |> Jl_value.array_any_map ~f:(Jl_type.wrap date_s));
  Register.defunc
    "dates_between"
    ~modl
    (let%map_open.Jl min = positional "start" date_arg ~docstring:""
     and max = positional "stop" date_arg ~docstring:"" in
     Date.dates_between ~min ~max
     |> Array.of_list
     |> Jl_value.array_any_map ~f:(Jl_type.wrap date_s));
  Register.defunc
    "to_date"
    ~modl
    (let%map_open.Jl time = positional "time" time_arg ~docstring:""
     and zone = positional "zone" zone_arg ~docstring:"" in
     Time_ns.to_date time ~zone |> Jl_type.wrap date_s);
  Register.defunc
    "to_ofday"
    ~modl
    (let%map_open.Jl time = positional "time" time_arg ~docstring:""
     and zone = positional "zone" zone_arg ~docstring:"" in
     Time_ns.to_ofday time ~zone |> Jl_type.wrap ofday_s);
  Register.defunc
    "of_date_ofday"
    ~modl
    (let%map_open.Jl date = positional "date" date_arg ~docstring:""
     and ofday = positional "ofday" ofday_arg ~docstring:""
     and zone = positional "zone" zone_arg ~docstring:"" in
     Time_ns.of_date_ofday date ofday ~zone |> Jl_type.wrap time_s);
  { time_s; date_s; ofday_s; span_s }
