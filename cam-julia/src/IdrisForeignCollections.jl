module IdrisForeignCollections
using CamJulia.IdrisList


FList = IdrisList

const NIL = Symbol("Prelude.List.Nil")
const CONS = Symbol("Prelude.List.Cons")

export from_flist
export to_flist!
export to_flist

function from_flist(jl_lst :: Vector{T}) where T
    foldr(jl_lst, init=NIL) do each, prev
         (CONS, each, prev)
    end
end


function to_flist!(c :: Vector{T}, flist::Symbol) where T
    nothing
end

function to_flist!(c :: Vector{T}, flist::Tuple{Symbol, T, A}) where {T, A}
    while flist !== NIL
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

end