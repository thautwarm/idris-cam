using CamJulia

module A
end
macro load_cam(path)
    aeson = CamJulia.ReadIR.load_aeson(path);

    ir = CamJulia.ReadIR.aeson_to_ir(aeson)

    CamJulia.CAM.ir_to_julia(ir)
end

@load_cam "./test.cam"