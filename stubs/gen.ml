let () =
  let fmt file = Format.formatter_of_out_channel (open_out file) in
  let fmt_c = fmt "jl_ctypes.c" in
  let fmt_ml = fmt "jl_ctypes.ml" in
  Format.fprintf fmt_c "#include \"ocaml_stubs.h\"@.";
  Cstubs.write_c fmt_c ~prefix:"caml_" (module Bindings.C);
  Cstubs.write_ml fmt_ml ~prefix:"caml_" (module Bindings.C);
  flush_all ()
