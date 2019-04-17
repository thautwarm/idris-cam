# internal objects, placed to ASTs in compiling time

rt_support = Dict{String, Any}(
    "prim-plus" => (+),
    "prim-minus" => (-),
    "prim-times" => (*),
    "prim-udiv" => div,
    "prim-sdiv" => div,
)