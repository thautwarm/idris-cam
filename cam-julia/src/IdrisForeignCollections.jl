module IdrisForeignCollections
using CamJulia.IdrisList


FList = IdrisList

const HVECT_NIL = Symbol("Data.HVect.Nil")
const HVECT_CONS = Symbol("Data.HVect.::")

const VECT_NIL = Symbol("Data.Vect.Nil")
const VECT_CONS = Symbol("Data.Vect.::")

const LIST_NIL = Symbol("Prelude.List.Nil")
const LIST_CONS = Symbol("Prelude.List.::")

export from_flist, to_flist
export to_flist!
export map_hvect
export to_fvect, from_fvect
export to_fhvect, from_fhvect

function from_flist(jl_lst :: Vector{T}) where T
    foldr(jl_lst, init=LIST_NIL) do each, prev
         (LIST_CONS, each, prev)
    end
end


function to_flist!(c :: Vector{T}, flist::Symbol) where T
    nothing
end

function to_flist!(c :: Vector{T}, flist::Tuple{Symbol, T, A}) where {T, A}
    while flist !== LIST_NIL
        push!(c, flist[2])
        flist = flist[3]
    end
end

function to_flist(flist :: Symbol)
    []
end

function to_flist(flist::Tuple{Symbol, T, A}) where {T, A}
    c = T[]
    to_flist!(c, flist)
    c
end

function map_hvect(f, a::Symbol)
    a
end

function map_hvect(f, a :: Tuple{Symbol, T, A}) where {T, A}
    (a[1], f(a[2]), map_hvect(f, a[3]))
end

function from_fvect(n :: Int64, vec :: Vector{T}) where T
    prev = VECT_NIL
    for i = n:-1:1
        prev = (VECT_CONS, vec[i], prev)
    end
    prev
end

function to_fvect(n :: Int64, vec :: Symbol)
    []
end

function to_fvect(n :: Int64, vec :: Tuple{Symbol, T, A}) where {T, A}
    v = Vector{T}(UndefInitializer(), n)
    i = 1
    while vec[3] !== VECT_NIL
        v[i] = vec[2]
        vec = vec[3]
        i = i + 1
    end
    @assert vec[3] === VECT_NIL
    v[i] = vec[2]
    v
end


function from_fhvect(n :: Int64, vec :: Tuple) where T
    prev = HVECT_NIL
    for i = n:-1:1
        prev = (HVECT_CONS, vec[i], prev)
    end
    prev
end

function to_fhvect(n :: Int64, vec :: Symbol)
    ()
end

@generated function to_fhvect(n :: Int64, vec :: Tuple{Symbol, T, A}) where {T, A}
    node = vec
    vars = []
    while node <: Tuple
        ps = node.parameters
        push!(vars, gensym("elt" * string(ps[2])))
        node = ps[end]
    end

    tp = gensym("tp")
    expr = foldr(vars, init=Expr(:tuple, vars...)) do cur_var, prev
        :(let $cur_var = $tp[2], $tp=$tp[3]; $prev end)
    end
    :(let $tp = vec; $expr end)
end

end
