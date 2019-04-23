# internal objects, placed to ASTs in compiling time
module Runtime
using MLStyle
using CamJulia.IdrisList
using CamJulia.IdrisForeignCollections

struct FakeFileHandler
    filename :: String
end

simple_open(filename::IdrisString) = FakeFileHandler(string(filename))
simple_read(f::FakeFileHandler) = open(f.filename) do f
    from_text(read(f, String))
end

filesystem_pipe(::Nothing) = (1, 2)

@inline function is(a :: T , b :: T) where T
    a === b
end

is(_, _) = false

function to_list_str(s)
    from_text(string(s))
end

@inline function parse_from_list_str(::Type{T}, s::IdrisString) where T
    parse(T, string(s))
end

function parse_int_from_list_str(s::IdrisString)
    parse_from_list_str(Int, s)
end

function parse_double_from_list_str(s::IdrisString)
    parse_from_list_str(Float64, s)
end

export rt_support
rt_support = Dict{String, Any}(
    "idris-cam-rt.cmp" => (==),
    "idris-cam-rt.err" => throw,
    "idris-cam-rt.is" => is,

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
    "prim-floatstr" => to_list_str,
    "prim-strfloat" => x -> parse(Float64, x) ∘ string,
    "prim-intstr" => to_list_str,
    "prim-strint" => x -> parse(Int, x) ∘ string,
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
    "builtin-simple_read" => simple_read,
    "builtin-filesystem_pipe" => filesystem_pipe,

    # flist
    "builtin-reverse_flist" => reverse,
    "builtin-flist_to_native" => from_flist,
    "builtin-list_to_foreign" => to_flist,
    # string
    "builtin-to_str" => string,
    "builtin-fstr_to_native" => from_text,
    "builtin-str_to_foreign" => string,

)
end
