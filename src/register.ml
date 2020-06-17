open! Base

let func ~fn ~name =
  let fn args kwargs =
    try
      let args =
        match (args : Jl_value.t) with
        | Tuple args | Array args -> args
        | _ ->
          Printf.failwithf "expected a tuple as args, got %s" (Jl_value.kind_str args) ()
      in
      let kwargs =
        match (kwargs : Jl_value.t) with
        | Array pairs ->
          Array.fold pairs ~init:[] ~f:(fun acc pair ->
              match pair with
              | Tuple [| Symbol symbol; value |] -> (symbol, value) :: acc
              | _ ->
                Printf.failwithf "expected a pair, got %s" (Jl_value.kind_str pair) ())
          |> Map.of_alist_exn (module String)
        | _ ->
          Printf.failwithf
            "expected an array of pairs as kwargs, got %s"
            (Jl_value.kind_str kwargs)
            ()
      in
      fn ~args ~kwargs
    with
    (* Pretty-printing the exception and running failwith ensures that the
       exception text is propagated to julia. *)
    | exn -> Exn.to_string exn |> failwith
  in
  Wrapper.register_fn name ~f:(fun _ _ ->
      Stdio.printf "hello from ocaml!\n";
      Wrapper.Jl_value.nothing);
  Caml.Callback.register name (fn : Jl_value.t -> Jl_value.t -> Jl_value.t);
  Printf.sprintf "%s = Caml.fn(\"%s\")" name name
  |> Wrapper.eval_string
  |> (ignore : Wrapper.Jl_value.t -> unit)

let defunc ~fn ~name =
  let fn ~args ~kwargs = Defunc.apply fn args kwargs in
  func ~fn ~name

let no_arg ~fn ~name = defunc ~fn:(Defunc.no_arg fn) ~name

(* Force a dependency on named_fn to avoid the symbol not being linked. *)
external _name : unit -> unit = "named_fn"
