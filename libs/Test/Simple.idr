module Test.Simple
import Cam.FFI

%access export

%inline
capitalize : String -> FFI.IO String
capitalize s = camCall (String -> FFI.IO String) (Builtin "str_cap") s


testSimple : FFI.IO ()
testSimple = do
    a <- capitalize "a"
    pure ()