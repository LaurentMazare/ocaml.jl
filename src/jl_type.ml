open! Base

type 'a t =
  { data_type : Wrapper.Jl_datatype.t
  ; wrap : 'a -> Jl_value.t
  ; unwrap : Jl_value.t -> 'a
  }

let data_type t = t.data_type
let wrap t = t.wrap
let unwrap t = t.unwrap

let int64 =
  { data_type = Wrapper.Jl_datatype.int64; wrap = Jl_value.int; unwrap = Jl_value.to_int }

let float64 =
  { data_type = Wrapper.Jl_datatype.float64
  ; wrap = Jl_value.float64
  ; unwrap = Jl_value.to_float
  }

let string =
  { data_type = Wrapper.Jl_datatype.string
  ; wrap = Jl_value.string
  ; unwrap = Jl_value.to_string
  }

let map t ~wrap ~unwrap =
  { data_type = t.data_type
  ; wrap = (fun x -> wrap x |> t.wrap)
  ; unwrap = (fun jl -> t.unwrap jl |> unwrap)
  }

let struct1 ?modl name ~field =
  let symbol = Wrapper.Jl_sym.create name in
  let field_name, field = field in
  let data_type =
    Wrapper.Jl_datatype.create
      symbol
      (* Using modl here results in a segfault. *)
      Wrapper.Jl_module.main
      ~super:Wrapper.Jl_datatype.any
      ~abstract:false
      ~fields:(`T1 (Wrapper.Jl_sym.create field_name, field.data_type))
      ~mutable_:false
      ~ninitialized:0
  in
  Option.iter modl ~f:(fun modl ->
      Wrapper.(Jl_datatype.set_on_module data_type symbol modl));
  let wrap t = field.wrap t |> Wrapper.Jl_value.struct1 data_type in
  let unwrap jl =
    if Wrapper.Jl_value.typeis jl data_type
    then Wrapper.Jl_value.get_nth_field jl 0 |> field.unwrap
    else Wrapper.Jl_value.type_error jl ~expected:name
  in
  { data_type; wrap; unwrap }

let struct2 ?modl name ~field1 ~field2 =
  let symbol = Wrapper.Jl_sym.create name in
  let data_type =
    let f (field_name, field) = Wrapper.Jl_sym.create field_name, field.data_type in
    Wrapper.Jl_datatype.create
      symbol
      (* Using modl here results in a segfault. *)
      Wrapper.Jl_module.main
      ~super:Wrapper.Jl_datatype.any
      ~abstract:false
      ~fields:(`T2 (f field1, f field2))
      ~mutable_:false
      ~ninitialized:0
  in
  Option.iter modl ~f:(fun modl ->
      Wrapper.(Jl_datatype.set_on_module data_type symbol modl));
  let _, field1 = field1 in
  let _, field2 = field2 in
  let wrap (t1, t2) =
    Wrapper.Jl_value.struct2 data_type (field1.wrap t1) (field2.wrap t2)
  in
  let unwrap jl =
    if Wrapper.Jl_value.typeis jl data_type
    then (
      let t1 = Wrapper.Jl_value.get_nth_field jl 0 |> field1.unwrap in
      let t2 = Wrapper.Jl_value.get_nth_field jl 1 |> field2.unwrap in
      t1, t2)
    else Wrapper.Jl_value.type_error jl ~expected:name
  in
  { data_type; wrap; unwrap }
