module Cam.FileSystem
import Cam.FFI
import Cam.Data.FCollections
import Data.Vect
import Data.HVect

public export
%inline

pipe : () -> FHVect [Int, Int]
pipe () = unsafePerformIO $
            camCall (() -> IO (FHVect [Int, Int])) (Builtin "filesystem_pipe") ()

public export
%inline
getcwd : () ->  ComRaw String
getcwd () = unsafePerformIO $
            camCall (() -> IO (ComRaw String)) (Builtin "filesystem_getcwd") ()