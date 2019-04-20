module Test.Simple
import Cam.FFI
import Cam.OS.FileSystem
import Cam.IO

%access export

%inline
capitalize : String -> FFI.IO String
capitalize s = camCall (String -> FFI.IO String) (Builtin "str_cap") s


testSimple : FFI.IO ()
testSimple = println $ show a
    where
        a : (Int, Int, Int)
        a = (1, 2, 3)