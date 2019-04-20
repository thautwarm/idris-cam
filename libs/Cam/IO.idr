module IO
import Cam.FFI


public export
%inline
println : String -> IO ()
println s = camCall (String -> IO ()) (Builtin "println") s
