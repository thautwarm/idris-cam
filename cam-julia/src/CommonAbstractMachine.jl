module CAM
using MLStyle
using MLStyle.Infras: @format, mangle

include("Runtime.jl")


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

    BigIntConst(BigInt)
    IntConst(Int)
    DoubleConst(Float64)
    StrConst(String)
    ChConst(Char)
    BoolConst(Bool)
    NilConst()

    Internal(String)
    Located(fname::Union{Nothing, String}, line::Int, column::Int, value::IR)
end

@active Sym(x) begin
    Symbol(x)
end

@active Julia(x) begin
    ir_to_julia(x)
end


ir_to_julia(ir::IR) =
    @match ir begin
        Let(Sym(s), Julia(value), Julia(body)) =>
            :(let $s = $value; $body end)
        LetRec([(Sym(fst), Julia(snd)) for (:($fst = $snd)) in seq], Julia(value)) =>
            :(let $(seq...); $value end)
        If(Julia(cond), Julia(iftrue), Julia(iffalse)) =>
            :(if $cond; $iftrue else $iffalse end)
        While(Julia(cond), Julia(body)) =>
            :(while $cond; $body end)
        Mutate(Sym(s), Julia(value)) =>
            :($s = $value)
        Fun([Sym(s) for s in args], Julia(body)) =>
            :(function ($(args...), )  $body end)
        App(Julia(fn), [Julia(arg) for arg in args]) =>
            :($fn($(args...), ))
        Block([Julia(expr) for expr in seq]) =>
            :(begin $(seq...) end)
        Join([Julia(elt) for elt in elts]) =>
            :($(elts..., ))
        Proj(Julia(major), Julia(ith)) =>
            :($major[$ith])
        BigIntConst(c) || IntConst(c) || DoubleConst(c) ||
        # TODO: str const needs to be unescaped, e.g., '\\a' -> '\a'
        StrConst(c) || ChConst(c) || BoolConst(c) =>
            :($c)
        NilConst => :nothing # kind of tricky for Julia use :nothing to represent nothing in ASTs

        Internal(s) => rt_support[s]
        Located(fname, lineno, _, Julia(value)) => # colno doesn't have an effect in current julia.
            let lineno = LineNumberNode(lineno, fname)
                :(begin $lineno; $value end)
            end
    end



end