open! Base
open! Ctypes
module C = Bindings.C (Jl_ctypes)

module Jl_sym = struct
  type t = C.Jl_sym.t

  let create = C.Jl_sym.create
end

module Jl_module = struct
  type t = C.Jl_module.t

  let create = C.Jl_module.create
  let set_module = C.set_const
  let base = !@C.Jl_module.base
  let core = !@C.Jl_module.core
  let main = !@C.Jl_module.main
  let top = !@C.Jl_module.top
end

module Svec = struct
  let empty = !@C.Jl_svec.empty
  let create1 = C.Jl_svec.create1
  let create2 = C.Jl_svec.create2
end

module Jl_datatype = struct
  type t = C.Jl_datatype.t

  let modl = !@C.Jl_datatype.modl
  let string = !@C.Jl_datatype.string
  let bool = !@C.Jl_datatype.bool
  let char = !@C.Jl_datatype.char
  let int8 = !@C.Jl_datatype.int8
  let int16 = !@C.Jl_datatype.int16
  let int32 = !@C.Jl_datatype.int32
  let int64 = !@C.Jl_datatype.int64
  let uint8 = !@C.Jl_datatype.uint8
  let uint16 = !@C.Jl_datatype.uint16
  let uint32 = !@C.Jl_datatype.uint32
  let uint64 = !@C.Jl_datatype.uint64
  let float16 = !@C.Jl_datatype.float16
  let float32 = !@C.Jl_datatype.float32
  let float64 = !@C.Jl_datatype.float64
  let errorexception = !@C.Jl_datatype.errorexception
  let any = !@C.Jl_datatype.any

  let create name module_ ~super ~fields ~abstract ~mutable_ ~ninitialized =
    let parameters = Svec.empty in
    let fnames, ftypes =
      match fields with
      | `T0 -> Svec.empty, Svec.empty
      | `T1 (s, t) -> Svec.create1 s, Svec.create1 t
      | `T2 ((s1, t1), (s2, t2)) -> Svec.create2 s1 s2, Svec.create2 t1 t2
    in
    C.Jl_datatype.create
      name
      module_
      super
      parameters
      fnames
      ftypes
      (if abstract then 1 else 0)
      (if mutable_ then 1 else 0)
      ninitialized

  let set_on_module t sym modl = C.set_const modl sym t
end

module Jl_value = struct
  type t = C.Jl_value.t

  let nothing = !@C.Jl_value.nothing
  let true_ = !@C.Jl_value.true_
  let false_ = !@C.Jl_value.false_
  let emptytuple = !@C.Jl_value.emptytuple
  let error = C.Jl_value.error_value
  let float64 = C.Jl_value.box_float64
  let int v = Int64.of_int v |> C.Jl_value.box_int64
  let int64 = C.Jl_value.box_int64
  let string s = C.Jl_value.pchar_to_string s (String.length s)
  let struct0 = C.Jl_value.new_struct0
  let struct1 = C.Jl_value.new_struct1
  let struct2 = C.Jl_value.new_struct2
  let struct3 = C.Jl_value.new_struct3
  let struct4 = C.Jl_value.new_struct4
  let is_nothing t = C.Jl_value.is_nothing t <> 0
  let is_bool t = C.Jl_value.is_bool t <> 0
  let is_string t = C.Jl_value.is_string t <> 0
  let is_tuple t = C.Jl_value.is_tuple t <> 0
  let is_float _t = failwith "TODO"
  let get_field = C.Jl_value.get_field
  let nfields = C.Jl_value.nfields
  let get_nth_field = C.Jl_value.get_nth_field
  let to_float = C.Jl_value.unbox_float64

  let to_int t =
    if C.Jl_value.is_int8 t <> 0
    then C.Jl_value.unbox_int8 t
    else if C.Jl_value.is_int16 t <> 0
    then C.Jl_value.unbox_int16 t
    else if C.Jl_value.is_int32 t <> 0
    then C.Jl_value.unbox_int32 t |> Int32.to_int_exn
    else if C.Jl_value.is_int64 t <> 0
    then C.Jl_value.unbox_int64 t |> Int64.to_int_exn
    else failwith "not a supported int type"

  let is_int t =
    C.Jl_value.is_int8 t <> 0
    || C.Jl_value.is_int16 t <> 0
    || C.Jl_value.is_int32 t <> 0
    || C.Jl_value.is_int64 t <> 0

  let to_string t =
    let length = C.Jl_value.string_len t in
    let data = C.Jl_value.string_data t in
    string_from_ptr data ~length

  let bool = function
    | true -> true_
    | false -> false_

  let typeof_str = C.Jl_value.typeof_str
end

module Exception = struct
  let occurred () =
    let jl_value = C.Exception.occurred () in
    if is_null jl_value then None else Some jl_value

  let current_exception = C.Exception.current_exception
  let clear = C.Exception.clear
end

let eval_string = C.eval_string
let raise = C.raise
let funptrs = Queue.create ()

let register_fn name ~f =
  if not (String.for_all name ~f:(fun c -> Char.is_alphanum c || Char.( = ) c '_'))
  then Printf.failwithf "invalid name %s" name ();
  let f args kwargs =
    try f args kwargs with
    | exn -> Exn.to_string exn |> Jl_value.error
  in
  let fn =
    coerce
      (Foreign.funptr (C.Jl_value.t @-> C.Jl_value.t @-> returning C.Jl_value.t))
      (static_funptr (C.Jl_value.t @-> C.Jl_value.t @-> returning C.Jl_value.t))
      f
  in
  let fn_ptr_as_int =
    coerce
      (static_funptr (C.Jl_value.t @-> C.Jl_value.t @-> returning C.Jl_value.t))
      (ptr void)
      fn
    |> raw_address_of_ptr
    |> Nativeint.to_int_exn
  in
  Queue.enqueue funptrs fn;
  Printf.sprintf
    "%s = function(args...; kwargs...)\n\
    \        kwargs = Any[(k.first, k.second) for k in kwargs]\n\
    \        res = ccall(Ptr{Int}(%d), Any, (Any, Any), args, kwargs)\n\
    \        if typeof(res) == ErrorException \n\
    \          throw(res)\n\
    \        end\n\
    \        res\n\
    \    end"
    name
    fn_ptr_as_int
  |> eval_string
  |> (ignore : Jl_value.t -> unit)

module Gc = struct
  let with_frame ~n fn =
    let frame = CArray.from_ptr (C.gc_push_args n) n in
    let idx = ref 0 in
    let protect jl_value =
      CArray.set frame !idx jl_value;
      Int.incr idx;
      jl_value
    in
    Exn.protect ~f:(fun () -> fn protect) ~finally:C.gc_pop
end
