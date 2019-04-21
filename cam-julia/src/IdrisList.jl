module IdrisList
import Base: (>), (<), (>=), (<=), (==), (^), foreach
using MLStyle

@use Enum

export AbstractIdrisList, IdrisCons, IdrisNil

@data AbstractIdrisList{T} begin
    IdrisCons(head::T, tail::AbstractIdrisList{T})
    IdrisNil()
end

export IdrisString
IdrisString = AbstractIdrisList{Char}

function Base.collect(x::IdrisNil{A}) :: Vector{A} where A
    []
end

function Base.collect(x::IdrisCons{A}) :: Vector{A} where A
    vec :: Vector{A} = []
    foreach(x) do e
        push!(vec, e)
    end
    vec
end

Base.length(::IdrisNil{A}) where A = 0
function Base.length(xs::IdrisCons{A}) where A
    len = 1
    nil = IdrisNil{A}()
    while xs.tail !== nil
        len += 1
        xs = xs.tail
    end
    len
end

Base.getindex(xs::IdrisNil{A}, i::Int) where A =
    throw("Cannot index empty list")

function Base.getindex(xs::IdrisCons{A}, i::Int) where A
    nil = IdrisNil{A}()
    while i !== 0
        if xs.tail === nil
            throw("Cannot index empty list")
        end
        xs = xs.tail
        i -= 1
    end
    xs.head
end


function foreach(f::Function, xs::IdrisNil{A}) where A
    nothing
end

function foreach(f::Function, xs::IdrisCons{A}) where A
    f(xs.head)
    nil = IdrisNil{A}()
    if nil === xs.tail
        return
    end
    cur = xs.tail
    while cur.tail !== nil
        f(cur.head)
        cur = cur.tail
    end
    f(cur.head)
    return
end

Base.print(io::IO, x::IdrisNil{Char}) = Base.print(io, "")
Base.print(io::IO, x::IdrisCons{Char}) = foreach(x) do c
    print(io, c)
end

Base.show(io::IO, x::IdrisNil{Char}) = Base.show("")
Base.show(io::IO, x::IdrisCons{Char}) = Base.show(io, String(collect(x)))

Base.show(io::IO, x::IdrisNil{T}) where T = "[]"
function Base.show(io::IO, x::IdrisCons{T}) where T
    print(io, "[")
    print(io, x.head)
    foreach(x.tail) do each
        print(io, ", ")
        print(io, each)
    end
    print(io, "]")
end

export string_cons
function string_cons(c::Char, s::S) where S <: IdrisString
    IdrisCons(c, s)
end

^(c::Elt, s::S) where {Elt, S <: AbstractIdrisList{Elt}} = IdrisCons{Elt}(c, s)

export from_text
function from_text(s :: String)
    ret = IdrisNil{Char}()
    foreach(reverse(s)) do c
        ret = c ^ ret
    end
    ret
end

export string_head
string_head(::IdrisNil{Char}) = throw("Empty String cannot extract head")
string_head(xs::IdrisCons{Char}) :: Char = xs.head

export string_tail
string_tail(::IdrisNil{Char}) = throw("Empty String cannot extract tail")
string_tail(xs::IdrisCons{Char}) = xs.tail


export string_concat
string_concat(xs1::IdrisNil{Char}, xs2::IdrisString) = xs2
string_concat(xs1::IdrisCons{Char}, xs2::IdrisString) =
    IdrisCons(xs1.head, string_concat(xs1.tail, xs2))


export string_len
string_len(xs::IdrisNil{Char}) = 0
string_len(xs::IdrisCons{Char}) = length(xs)


const EQ = Int8(0)
const GT = Int8(1)
const LT = Int8(-1)
const PENDING = Int8(2)

export list_compare
function list_compare(xs1::AbstractIdrisList{A}, xs2::AbstractIdrisList{A}) where A
    ended = false
    res :: Union = PENDING
    while !ended
        ended, res = @match (xs1, xs2) begin
            (IdrisNil{A}, IdrisNil{A}) => (true, EQ)
            (IdrisNil{A}, _) => (true, LT)
            (_, IdrisNil{A}) => (true, GT)
            (IdrisCons(a1, s2), IdrisCons(a2, s2)) =>
                a1 < a2 ? (true, LT) : a1 > a2 ? (true, GT) : (false, PENDING)
        end
    end
    res
end

>(xs1::AbstractIdrisList, xs2::AbstractIdrisList) = list_compare(xs1, xs2) === GT
<(xs1::AbstractIdrisList, xs2::AbstractIdrisList) = list_compare(xs1, xs2) === LT
==(xs1::AbstractIdrisList, xs2::AbstractIdrisList) = list_compare(xs1, xs2) === EQ
>=(xs1::AbstractIdrisList, xs2::AbstractIdrisList) =
    let cmp = list_compare(xs1, xs2)
        cmp === EQ || cmp === GT
    end
<=(xs1::AbstractIdrisList, xs2::AbstractIdrisList) =
    let cmp = list_compare(xs1, xs2)
        cmp === EQ || cmp === LT
    end


# println(1 ^ 2 ^ IdrisNil{Int}())

# println('1' ^ '2' ^ IdrisNil{Char}())

end
