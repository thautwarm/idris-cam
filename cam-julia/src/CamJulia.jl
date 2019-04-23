module CamJulia
export IdrisList, Runtime

export IdrisIntegerT
IdrisIntegerT = Int64


include("IdrisList.jl")
include("IdrisForeignCollections.jl")
include("Runtime.jl")
include("CommonAbstractMachine.jl")
include("ReadIR.jl")

end # module
