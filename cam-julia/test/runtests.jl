using CamJulia
using MLStyle

rmlines = @λ begin
    Expr(head, args...) -> Expr(head, filter(x -> x !== nothing, map(rmlines, args))...)
    ::LineNumberNode -> nothing
    a -> a
end

macro load_cam(path)
    aeson = CamJulia.ReadIR.load_aeson(path);
    ir = CamJulia.ReadIR.aeson_to_ir(aeson)
    x = CamJulia.CAM.ir_to_julia(ir)
    # @info rmlines(x)
    esc(x)
end

# @load_cam "./test.cam"
@load_cam "/tmp/idris19309-1"
