open! Base

let func ~fn ~name =
  if not (String.for_all name ~f:(fun c -> Char.is_alphanum c || Char.( = ) c '_'))
  then Printf.failwithf "invalid name %s" name ();
  let fn args kwargs =
    try
      let args =
        if Jl_value.is_tuple args
        then Jl_value.to_tuple args
        else if Jl_value.is_array_any args
        then Jl_value.to_array_any args
        else Jl_value.type_error args ~expected:"tuple or array"
      in
      let kwargs =
        if Jl_value.is_array_any kwargs
        then
          Jl_value.to_array_any kwargs
          |> Array.fold ~init:[] ~f:(fun acc pair ->
                 let symbol, value = Jl_value.to_tuple2 pair in
                 (Jl_value.to_symbol symbol, value) :: acc)
          |> Map.of_alist_exn (module String)
        else Jl_value.type_error kwargs ~expected:"array of pairs"
      in
      fn ~args ~kwargs
    with
    (* Pretty-printing the exception and running failwith ensures that the
       exception text is propagated to julia. *)
    | exn -> Exn.to_string exn |> failwith
  in
  Wrapper.register_fn name ~f:fn

let defunc ~fn ~name =
  let fn ~args ~kwargs = Defunc.apply fn args kwargs in
  func ~fn ~name

let no_arg ~fn ~name = defunc ~fn:(Defunc.no_arg fn) ~name

(* Force a dependency on named_fn to avoid the symbol not being linked. *)
external _name : unit -> unit = "named_fn"
