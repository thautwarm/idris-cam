module CamJulia
export IdrisList, Runtime

export IdrisIntegerT
IdrisIntegerT = Int64


include("IdrisList.jl")
include("IdrisForeignCollections.jl")
include("Runtime.jl")
include("CommonAbstractMachine.jl")
include("ReadIR.jl")

macro load_cam(path)
    aeson = ReadIR.load_aeson(path);
    ir = ReadIR.aeson_to_ir(aeson)
    x = CAM.ir_to_julia(ir)
    esc(x)
end

end # module
