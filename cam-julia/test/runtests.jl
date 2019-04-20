using CamJulia

macro load_cam(path)
    aeson = CamJulia.ReadIR.load_aeson(path);
    ir = CamJulia.ReadIR.aeson_to_ir(aeson)
    x = CamJulia.CAM.ir_to_julia(ir)
    @info x
    esc(x)
end

# @load_cam "./test.cam"
@load_cam "/tmp/idris5492-1"