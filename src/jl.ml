module Defunc = Defunc
module Jl_type = Jl_type
module Jl_value = Jl_value
module Register = Register
module Wrapper = Wrapper

module Let_syntax = struct
  include Defunc.Param

  module Let_syntax = struct
    include Defunc
    module Open_on_rhs = Defunc.Param
  end
end
