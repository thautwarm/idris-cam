# internal objects, placed to ASTs in compiling time
module Runtime
using MLStyle
using CamJulia.IdrisList
using CamJulia.IdrisForeignCollections

struct FileHandler
    filename :: String
    mode     :: String
end

simple_open(filename::IdrisString) = FileHandler(string(filename), "r")
simple_read(f::FileHandler) = open(f.filename) do f
    from_text(read(f, String))
end


read_all_text(f::IOStream) = read(f, String)

filesystem_pipe(::Nothing) = (1, 2)

@inline function is(a :: T , b :: T) where T
    a === b
end

is(_, _) = false

@inline function to_list_str(s)
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

@inline function module_property(m :: Module, s::String)
    getproperty(m, Symbol(s))
end

export rt_support
rt_support = Dict{String, Any}(
    "cam-rt.cmp" => (==),
    "cam-rt.err" => throw,
    "cam-rt.is" => is,

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

    # io
    "builtin-println" => println,
    "builtin-simple_open" => simple_open,
    "builtin-simple_read" => simple_read,
    "builtin-filesystem_pipe" => filesystem_pipe,
    "builtin-filesystem_open_file" => open,
    "builtin-filesystem_close_file" => close,
    "builtin-filesystem_read_all_text" => read_all_text,

    # flist
    "builtin-reverse_flist" => reverse,
    "builtin-flist_to_native" => from_flist,
    "builtin-list_to_foreign" => to_flist,

    # fvect
    "builtin-reverse_fvect" => reverse,
    "builtin-fvect_to_native" => from_fvect,
    "builtin-vect_to_foreign" => to_fvect,

    # fhvect
    "builtin-reverse_fhvect" => reverse,
    "builtin-fhvect_to_native" => from_fhvect,
    "builtin-hvect_to_foreign" => to_fhvect,

    # string
    "builtin-to_text" => string,
    "builtin-fstr_to_native" => from_text,
    "builtin-str_to_foreign" => string,

    "builtin-map_hvect" => map_hvect,
    "builtin-module_property" => module_property,

)
end

