module CAM
using MLStyle
using MLStyle.Infras: @format, mangle
using CamJulia.Runtime
using CamJulia


@use Enum # replace `S() => ...` with `S => ...`

# held here until merged into MLStyle master.
def_pattern(CAM,
    predicate = (@λ begin
        :[$_ for $_ in $_] -> true
        _ -> false
    end),
    rewrite = (tag, case, mod) -> begin
        @match case begin
            :[$expr for $iter in $seq] => begin
                function (body)
                    iter_var = mangle(mod)
                    produced_elt_var = mangle(mod)
                    inner_test_var = mangle(mod)
                    seq_var = mangle(mod)
                    decons_elt = mk_pattern(iter_var, expr, mod)(iter)
                    decons_seq = mk_pattern(seq_var, seq, mod)(body)
                    @format [
                        seq_var, iter_var, inner_test_var, produced_elt_var,
                        body, tag, decons_elt, decons_seq, push!] quote
                        if tag isa Vector
                            let seq_var = [], inner_test_var = true
                                for iter_var in tag
                                    produced_elt_var = decons_elt
                                    if produced_elt_var === failed
                                        inner_test_var = false
                                        break
                                    end
                                    push!(seq_var, produced_elt_var)
                                end
                                if inner_test_var
                                    decons_seq
                                else
                                    failed
                                end
                            end
                        else
                            failed
                        end
                    end
                end
            end
        end

    end
)

export IR, Let, LetRec, If, While, Mutate, Fun
export App, Var, Block, Join, Proj
export BigIntConst, IntConst, DoubleConst
export StrConst, ChConst, BoolConst, NilConst
export Internal, Located, SymConst


@data IR begin
    Let(String, IR, IR)
    LetRec(Vector{Tuple{String, IR}}, IR)
    If(IR, IR, IR)
    While(IR, IR)
    Mutate(String, IR)
    Fun(Vector{String}, IR)
    App(IR, Vector{IR})
    Var(String)
    Block(Vector{IR})
    Join(Vector{IR})
    Proj(IR, IR)

    BigIntConst(IdrisIntegerT)
    IntConst(Int)
    DoubleConst(Float64)
    StrConst(String)
    ChConst(Char)
    BoolConst(Bool)
    SymConst(String)
    NilConst()

    Internal(String)
    Located(fname::Union{Nothing, String}, line::Int, column::Int, value::IR)
end

@active Sym(x :: String) begin
    Symbol(x)
end

@active Julia(x) begin
    ir_to_julia(x)
end

export ir_to_julia

@active LetToDef(tp) begin
    @match tp begin
        (Sym(fname), Fun([Sym(arg) for arg in args], Julia(body))) =>
        :($fname($(args...), ) = $body)
    end
end

@generated function projection(x, i :: Int)
    x <: Tuple ? :(x[i]) : :(if i === 1; x else throw("internal runtime error") end)
end

ir_to_julia(ir::IR) =
    @match ir begin
        SymConst(Sym(sym)) => QuoteNode(sym)
        Var(Sym(sym)) => sym
        # Let(Sym(fn_name), Fun([Sym(arg) for arg in args], Julia(fn_body)), Julia(body)) =>
        #     :(let $fn_name($(args...), ) = $fn_body; $body end)
        Let(Sym(s), Julia(value), Julia(body)) =>
            :(let $s = $value; $body end)

        # letrec: bindings must be all functions
        LetRec([LetToDef(tp) for tp in seq], Julia(value)) =>
            Expr(:block, seq..., value)
        If(Julia(cond), Julia(iftrue), Julia(iffalse)) =>
            :(if $cond; $iftrue else $iffalse end)
        While(Julia(cond), Julia(body)) =>
            :(while $cond; $body end)
        Mutate(Sym(s), Julia(value)) =>
            :($s = $value)
        Fun([Sym(s) for s in args], Julia(body)) =>
            :(function ($(args...), ) $body end)

        App(Internal("builtin-get_module"), [StrConst(Sym(c))]) => quote
            @eval import $c
            $c
        end

        App(Julia(fn), [Julia(arg) for arg in args]) =>
            :($fn($(args...), ))

        Block([Julia(expr) for expr in seq]) =>
            :(begin $(seq...) end)
        Join([Julia(elt) for elt in elts]) =>
            :($(elts...), )
        Proj(Julia(major), Julia(ith)) =>
            :($projection($major, $ith + 1))
            # :($major[$ith + 1])

        StrConst(c) => foldr(c, init=CamJulia.IdrisList.IdrisNil{Char}()) do each, prev
            each ^ prev
        end

        BigIntConst(c) || IntConst(c) || DoubleConst(c) ||
        # TODO: str const needs to be unescaped, e.g., '\\a' -> '\a'
        ChConst(c) || BoolConst(c) => c
        
        NilConst => :nothing # kind of tricky for Julia use :nothing to represent nothing in ASTs

        Internal(s) => rt_support[s]
        Located(fname, lineno, _, Julia(value)) => # colno doesn't have an effect in current julia.
            let lineno = LineNumberNode(lineno, fname)
                :(begin $lineno; $value end)
            end
        a => throw(a)
    end
end
