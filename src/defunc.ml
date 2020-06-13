(* This has been adapted from:
   https://github.com/janestreet/pythonlib/blob/master/src/defunc.ml
*)
open Base

module Of_julia = struct
  type 'a t =
    { type_name : string
    ; conv : Jl_value.t -> 'a
    }

  let create ~type_name ~conv = { type_name; conv }
end

module Docstring = struct
  type t = string
end

module Arg = struct
  type 'a t =
    { name : string
    ; of_julia : 'a Of_julia.t
    ; (* The parameters docstrings are not exposed at the moment, but could
         be useful when generating julia wrapping code.  *)
      docstring : Docstring.t
    ; kind : [ `positional | `keyword of 'a option ]
    }
end

module Opt_arg = struct
  type 'a t =
    { name : string
    ; of_julia : 'a Of_julia.t
    ; docstring : string
    }
end

module T0 = struct
  type _ t =
    | Return : 'a -> 'a t
    | Map : 'a t * ('a -> 'b) -> 'b t
    | Both : 'a t * 'b t -> ('a * 'b) t
    | Arg : 'a Arg.t -> 'a t
    | Opt_arg : 'a Opt_arg.t -> 'a option t
    | Remaining_args : Docstring.t -> Jl_value.t array t
    | Remaining_kwargs :
        Docstring.t
        -> (string, Jl_value.t, String.comparator_witness) Map.t t

  let return x = Return x
  let map t ~f = Map (t, f)
  let both t t' = Both (t, t')
  let apply f x = both f x |> map ~f:(fun (f, x) -> f x)
  let map = `Custom map
end

module T = struct
  include T0
  include Applicative.Make (T0)
end

include T

module Open_on_rhs_intf = struct
  module type S = Applicative.S
end

include Applicative.Make_let_syntax (T) (Open_on_rhs_intf) (T)

let valid_char c = Char.(is_alphanum c || c = '_')

let check_valid_arg_name name =
  if String.is_empty name
  then failwith "cannot use an empty name"
  else if String.exists name ~f:(fun c -> not (valid_char c))
  then Printf.failwithf "arg name %s contains some invalid characters" name ()
  else ()

let no_arg fn = return () |> map ~f:fn

module State = struct
  type t =
    { pos : int
    ; after_positional : bool
    ; after_keyword : bool
    }

  let init = { pos = 0; after_positional = false; after_keyword = false }
end

(* TODO: better wrapping for this exception. *)
let arg_errorf fmt = Base.Printf.ksprintf failwith fmt

let apply (type a) (t : a t) args kwargs =
  let try_of_julia v ~of_julia ~name =
    try of_julia.Of_julia.conv v with
    | e ->
      arg_errorf
        "error processing arg %s (%s): %s"
        name
        of_julia.type_name
        (Exn.to_string e)
  in
  let kwnames = Hash_set.create (module String) in
  let positional_arguments () =
    let rec loop : type a. a t -> string list = function
      | Return _ -> []
      | Map (t, _) -> loop t
      | Both (t, t') ->
        let args = loop t in
        let args' = loop t' in
        args @ args'
      | Arg { name; kind = `positional; _ } -> [ name ]
      | Arg { kind = `keyword _; _ } -> []
      | Opt_arg _ -> []
      | Remaining_args _ -> [ "other args" ]
      | Remaining_kwargs _ -> []
    in
    loop t
  in
  let rec loop : type a. a t -> state:State.t -> a * State.t =
   fun t ~state ->
    match t with
    | Return a -> a, state
    | Map (t, f) ->
      let v, state = loop t ~state in
      f v, state
    | Both (t, t') ->
      let v, state = loop t ~state in
      let v', state = loop t' ~state in
      (v, v'), state
    | Arg { name; of_julia; docstring = _; kind = `positional } ->
      if state.after_positional
      then arg_errorf "positional argument after catch-all (%s)" name;
      let pos = state.pos in
      if pos >= Array.length args
      then
        arg_errorf
          "not enough arguments (got %d, expected %s)"
          (Array.length args)
          (positional_arguments () |> String.concat ~sep:", ");
      try_of_julia args.(pos) ~of_julia ~name, { state with pos = pos + 1 }
    | Opt_arg { name; of_julia; docstring = _ } ->
      if state.after_keyword then arg_errorf "keyword argument after catch-all (%s)" name;
      if Hash_set.mem kwnames name
      then arg_errorf "multiple keyword arguments with name %s" name;
      Hash_set.add kwnames name;
      let v = Map.find kwargs name in
      Option.map v ~f:(try_of_julia ~of_julia ~name), state
    | Arg { name; of_julia; docstring = _; kind = `keyword default } ->
      if state.after_keyword then arg_errorf "keyword argument after catch-all (%s)" name;
      if Hash_set.mem kwnames name
      then arg_errorf "multiple keyword arguments with name %s" name;
      Hash_set.add kwnames name;
      (match Map.find kwargs name with
      | Some v -> try_of_julia v ~of_julia ~name, state
      | None ->
        (match default with
        | Some default -> default, state
        | None -> arg_errorf "missing keyword argument: %s" name))
    | Remaining_args _docstring ->
      if state.after_positional then arg_errorf "multiple catch-all";
      let total_args_len = Array.length args in
      let args = Array.sub args ~pos:state.pos ~len:(Array.length args - state.pos) in
      args, { state with pos = total_args_len; after_positional = true }
    | Remaining_kwargs _docstring ->
      if state.after_keyword then arg_errorf "multiple keyword catch-all";
      let remaining_kwargs =
        Map.filter_keys kwargs ~f:(fun key ->
            if Hash_set.mem kwnames key
            then false
            else (
              Hash_set.add kwnames key;
              true))
      in
      remaining_kwargs, { state with after_keyword = true }
  in
  let v, final_state = loop t ~state:State.init in
  Map.iter_keys kwargs ~f:(fun key ->
      if not (Hash_set.mem kwnames key)
      then arg_errorf "unexpected keyword argument %s" key);
  if final_state.pos <> Array.length args
  then
    arg_errorf
      "expected %d arguments (%s), got %d"
      final_state.pos
      (positional_arguments () |> String.concat ~sep:", ")
      (Array.length args);
  v

let params_docstring t =
  let sprintf = Printf.sprintf in
  let arg_docstring arg ~pos =
    match arg.Arg.kind with
    | `positional ->
      [ sprintf "    :param %s: (positional %d) %s" arg.name pos arg.docstring
      ; sprintf "    :type %s: %s" arg.name arg.of_julia.type_name
      ]
      |> String.concat ~sep:"\n"
    | `keyword default ->
      let default =
        match default with
        | None -> "mandatory keyword"
        | Some _ -> "keyword with default"
      in
      [ sprintf "    :param %s: (%s) %s" arg.name default arg.docstring
      ; sprintf "    :type %s: %s" arg.name arg.of_julia.type_name
      ]
      |> String.concat ~sep:"\n"
  in
  let opt_arg_docstring (arg : _ Opt_arg.t) =
    [ sprintf "    :param %s: (optional keyword) %s" arg.name arg.docstring
    ; sprintf "    :type %s: %s" arg.name arg.of_julia.type_name
    ]
    |> String.concat ~sep:"\n"
  in
  let remaining_args_docstring doc = sprintf "    :param other args: %s" doc in
  let remaining_kwargs_docstring doc = sprintf "    :param other keyword args: %s" doc in
  let rec loop : type a. a t -> pos:int -> string list * int =
   fun t ~pos ->
    match t with
    | Return _ -> [], pos
    | Map (t, _) -> loop t ~pos
    | Both (t1, t2) ->
      let params1, pos = loop t1 ~pos in
      let params2, pos = loop t2 ~pos in
      params1 @ params2, pos
    | Arg ({ kind = `positional; _ } as arg) -> [ arg_docstring arg ~pos ], pos + 1
    | Arg ({ kind = `keyword _; _ } as arg) -> [ arg_docstring arg ~pos ], pos
    | Opt_arg opt_arg -> [ opt_arg_docstring opt_arg ], pos
    | Remaining_args doc ->
      (* There should be no other positional arg past this one *)
      [ remaining_args_docstring doc ], Int.max_value_30_bits
    | Remaining_kwargs doc -> [ remaining_kwargs_docstring doc ], pos
  in
  let params, _pos = loop t ~pos:0 in
  if List.is_empty params then None else String.concat params ~sep:"\n\n" |> Option.some

let params_docstring ?docstring t =
  [ params_docstring t; docstring ]
  |> List.filter_opt
  |> String.concat ~sep:"\n\n"
  |> Printf.sprintf "\n%s"

module Param = struct
  let positional name of_julia ~docstring =
    check_valid_arg_name name;
    Arg { name; of_julia; docstring; kind = `positional }

  let keyword ?default name of_julia ~docstring =
    check_valid_arg_name name;
    Arg { name; of_julia; docstring; kind = `keyword default }

  let keyword_opt name of_julia ~docstring =
    check_valid_arg_name name;
    Opt_arg { name; of_julia; docstring }

  let int = Of_julia.create ~type_name:"int" ~conv:Jl_value.to_int_exn
  let float = Of_julia.create ~type_name:"float" ~conv:Jl_value.to_float_exn
  let bool = Of_julia.create ~type_name:"bool" ~conv:Jl_value.to_bool_exn
  let string = Of_julia.create ~type_name:"string" ~conv:Jl_value.to_string_exn
  let jl_value = Of_julia.create ~type_name:"any" ~conv:Fn.id

  let pair (o1 : _ Of_julia.t) (o2 : _ Of_julia.t) =
    Of_julia.create
      ~type_name:(Printf.sprintf "(%s, %s)" o1.type_name o2.type_name)
      ~conv:(fun jl_value ->
        let p1, p2 = Jl_value.tuple2_exn jl_value in
        o1.conv p1, o2.conv p2)

  let triple (o1 : _ Of_julia.t) (o2 : _ Of_julia.t) (o3 : _ Of_julia.t) =
    Of_julia.create
      ~type_name:(Printf.sprintf "(%s, %s, %s)" o1.type_name o2.type_name o3.type_name)
      ~conv:(fun jl_value ->
        let p1, p2, p3 = Jl_value.tuple3_exn jl_value in
        o1.conv p1, o2.conv p2, o3.conv p3)

  let quadruple
      (o1 : _ Of_julia.t)
      (o2 : _ Of_julia.t)
      (o3 : _ Of_julia.t)
      (o4 : _ Of_julia.t)
    =
    Of_julia.create
      ~type_name:
        (Printf.sprintf
           "(%s, %s, %s, %s)"
           o1.type_name
           o2.type_name
           o3.type_name
           o4.type_name)
      ~conv:(fun jl_value ->
        let p1, p2, p3, p4 = Jl_value.tuple4_exn jl_value in
        o1.conv p1, o2.conv p2, o3.conv p3, o4.conv p4)

  let option (o : _ Of_julia.t) =
    Of_julia.create
      ~type_name:(Printf.sprintf "(%s option)" o.type_name)
      ~conv:(fun jl_value ->
        if Jl_value.is_nothing jl_value then None else Some (o.conv jl_value))

  let array (o : _ Of_julia.t) =
    Of_julia.create ~type_name:(Printf.sprintf "[%s]" o.type_name) ~conv:(fun jl_value ->
        Jl_value.array_any_exn jl_value |> Array.map ~f:o.conv)

  let remaining_args ~docstring = Remaining_args docstring
  let remaining_kwargs ~docstring = Remaining_kwargs docstring
end
