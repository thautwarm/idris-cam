# internal objects, placed to ASTs in compiling time
module Runtime
using MLStyle
using CamJulia.IdrisList

struct FakeFileHandler
    filename :: String
end

simple_open(filename::IdrisString) = FakeFileHandler(string(filename))
simple_read(f::FakeFileHandler) = open(f.filename) do f
    from_text(read(f, String))
end

export rt_support
rt_support = Dict{String, Any}(
    "idris-cam-rt.cmp" => (==),

    "prim-plus" => (+),
    "prim-minus" => (-),
    "prim-times" => (*),
    "prim-udiv" => div,
    "prim-sdiv" => div,
    "prim-urem" => rem,
    "prim-srem" => rem,
    "prim-eq" => (==),
    "prim-slt" => (<),
    "prim-sle" => (<=),
    "prim-sgt" => (>),
    "prim-sge" => (>=),
    "prim-and" => (&),
    "prim-or" => (|),

    # str methods1
    "prim-streq" => (==),
    "prim-strlen" => string_len,
    "prim-strindex" => getindex,
    "prim-strlt" => (<=),

    # effect
    "prim-external" => throw,
    "prim-readstr" => readline,
    "prim-writestr" => print,

    # conversion
    "prim-floatstr" => string,
    "prim-strfloat" => x -> parse(Float64, x),
    "prim-intstr" => string,
    "prim-strint" => x -> parse(Int, x),
    "prim-intch" => Char,
    "prim-chint" => Int,

    # str method2
    "prim-strhead" => string_head,
    "prim-strtail" => string_tail,
    "prim-strcons" => string_cons,
    "prim-strconcat" => string_concat,
    "prim-crash" => throw,

    "builtin-println" => println,
    "builtin-simple_open" => simple_open,
    "builtin-simple_read" => simple_read
)
end
