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

let tuple2 t1 t2 =
  let data_type = Wrapper.Jl_datatype.tuple [| t1.data_type; t2.data_type |] in
  let wrap (v1, v2) =
    Wrapper.Jl_value.tuple_with_type data_type [| t1.wrap v1; t2.wrap v2 |]
  in
  let unwrap jl =
    ( Wrapper.Jl_value.get_nth_field jl 0 |> t1.unwrap
    , Wrapper.Jl_value.get_nth_field jl 1 |> t2.unwrap )
  in
  { data_type; wrap; unwrap }

let tuple3 t1 t2 t3 =
  let data_type =
    Wrapper.Jl_datatype.tuple [| t1.data_type; t2.data_type; t3.data_type |]
  in
  let wrap (v1, v2, v3) =
    Wrapper.Jl_value.tuple_with_type data_type [| t1.wrap v1; t2.wrap v2; t3.wrap v3 |]
  in
  let unwrap jl =
    ( Wrapper.Jl_value.get_nth_field jl 0 |> t1.unwrap
    , Wrapper.Jl_value.get_nth_field jl 1 |> t2.unwrap
    , Wrapper.Jl_value.get_nth_field jl 2 |> t3.unwrap )
  in
  { data_type; wrap; unwrap }

let tuple4 t1 t2 t3 t4 =
  let data_type =
    Wrapper.Jl_datatype.tuple [| t1.data_type; t2.data_type; t3.data_type; t4.data_type |]
  in
  let wrap (v1, v2, v3, v4) =
    Wrapper.Jl_value.tuple_with_type
      data_type
      [| t1.wrap v1; t2.wrap v2; t3.wrap v3; t4.wrap v4 |]
  in
  let unwrap jl =
    ( Wrapper.Jl_value.get_nth_field jl 0 |> t1.unwrap
    , Wrapper.Jl_value.get_nth_field jl 1 |> t2.unwrap
    , Wrapper.Jl_value.get_nth_field jl 2 |> t3.unwrap
    , Wrapper.Jl_value.get_nth_field jl 3 |> t4.unwrap )
  in
  { data_type; wrap; unwrap }

let map t ~wrap ~unwrap =
  { data_type = t.data_type
  ; wrap = (fun x -> wrap x |> t.wrap)
  ; unwrap = (fun jl -> t.unwrap jl |> unwrap)
  }

let struct_dt ?modl name ~fields =
  let symbol = Wrapper.Jl_sym.create name in
  let data_type =
    Wrapper.Jl_datatype.create
      symbol
      (* Using modl here results in a segfault. *)
      Wrapper.Jl_module.main
      ~super:Wrapper.Jl_datatype.any
      ~abstract:false
      ~fields
      ~mutable_:false
      ~ninitialized:0
  in
  Option.iter modl ~f:(fun modl ->
      Wrapper.(Jl_datatype.set_on_module data_type symbol modl));
  data_type

let struct1 ?modl name ~field =
  let field_name, field = field in
  let data_type =
    struct_dt ?modl name ~fields:(`T1 (Wrapper.Jl_sym.create field_name, field.data_type))
  in
  let wrap t = field.wrap t |> Wrapper.Jl_value.struct1 data_type in
  let unwrap jl =
    if Wrapper.Jl_value.typeis jl data_type
    then Wrapper.Jl_value.get_nth_field jl 0 |> field.unwrap
    else Wrapper.Jl_value.type_error jl ~expected:name
  in
  { data_type; wrap; unwrap }

let struct2 ?modl name ~field1 ~field2 =
  let data_type =
    let f (field_name, field) = Wrapper.Jl_sym.create field_name, field.data_type in
    struct_dt ?modl name ~fields:(`T2 (f field1, f field2))
  in
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
