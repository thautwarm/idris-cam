module Test.Simple
import Cam.FFI
import Cam.OS.FileSystem
import Cam.IO
import Cam.Data.Collections
import Data.Vect
import Data.HVect

%hide HVect.index
%hide Vect.index

%access export

%inline
capitalize : String -> FFI.IO String
capitalize s = camCall (String -> FFI.IO String) (Builtin "str_cap") s


f : StaticSized c => TypeHolder c -> Nat
f d = typeSize d

testSimple : FFI.IO ()
testSimple = println $ show (size a)
    where
        a : Vect 3 Int
        a = [1, 2, 3]
        nnn : Nat
        nnn = f (MkTypeHolder (Vect 3 Int))
        e : Int
        e = index (the (Fin _) $ fromInteger 1) a

        hvec : HVect [Int, Double]
        hvec = [1, 2.0]

        hvecItem : Double
        hvecItem = index (the (Fin _) $ fromInteger 1) hvec