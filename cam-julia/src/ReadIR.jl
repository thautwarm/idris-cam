module ReadIR
using JSON2
using MLStyle
using CamJulia.CAM

export load_aeson, aeson_to_ir

function load_aeson(path)
    open(path) do f
        let s = read(f, String)
            JSON2.read(s, NamedTuple)
        end
    end
end

function aeson_to_ir(x::NamedTuple)
    f = @match x.tag begin
        "ComLet"    => com_let
        "ComLetrec" => com_letrec
        "ComIf"     => com_if
        "ComWhile"  => com_while
        "ComMutate" => com_mut
        "ComFun"    => com_fun
        "ComApp"    => com_app
        "ComVar"    => com_var
        "ComBlock"  => com_block
        "ComTuple"  => com_tuple
        "ComProj"   => com_proj
        "ComSymbol" => com_sym
        "ComInt"    => com_int
        "ComBigInt" => com_bigint
        "ComDouble" => com_double
        "ComCh"     => com_ch
        "ComBool"   => com_bool
        "ComStr"    => com_str
        "ComNil"    => nothing
        "ComInternal" => com_internal
        a => throw(a)
    end
    f === nothing ? NilConst() : f(x.contents)
end

@active Load(x) begin
    aeson_to_ir(x)
end

list_conv(xs) = map(aeson_to_ir, xs)


com_let = @λ [name, Load(value), Load(body)] ->
          Let(name, value, body)

com_letrec = @λ [
        [[fst, Load(snd)] for (fst, snd) in bindings],
        Load(body)
    ] -> LetRec(bindings, body)

com_if = @λ [Load(cond), Load(iftrue), Load(iffasle)] ->
            If(cond, iftrue, iffasle)

com_while = @λ [Load(cond), Load(body)] ->
               While(cond, body)

com_mut = @λ [varname, Load(value)] -> Mutate(varname, value)

com_fun = @λ [args, Load(body)] -> Fun(args, body)

com_app = @λ [Load(fn), [Load(arg) for arg in args]] -> App(fn, args)

com_var = Var

com_block  = @λ [Load(expr) for expr in blocks] -> Block(blocks)

com_tuple = @λ [Load(elt) for elt in elts] -> Join(elts)

com_proj = @λ [Load(major), Load(ith)] -> Proj(major, ith)

com_bigint = BigIntConst ∘ big

com_sym = SymConst

com_int = IntConst

com_str = StrConst

com_double = DoubleConst

com_ch =  @λ s -> ChConst(s[1])

com_bool = BoolConst

com_internal = Internal

end

