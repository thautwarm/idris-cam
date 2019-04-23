module IO
import Cam.FFI


public export
%inline
println : String -> IO ()
println s = camCall (String -> IO ()) (Builtin "println") s


public export
%inline
toStr : a -> ComRaw String
toStr a = unsafePerformIO $
            camCall (Ptr -> IO (ComRaw String)) (Builtin "to_str") (believe_me a)

