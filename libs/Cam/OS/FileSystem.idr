module Cam.FileSystem
import Cam.FFI

-- pipe : () -> (Int, Int)
-- pipe () =
--     where
--         f : () -> IO (ComRaw (Int, Int))
--         f () = camCall (() -> IO (ComRaw (Int, Int))) (Builtin "filesystem_pipe") ()


-- pipe : () -> (Int, Int)
-- pipe = unsafePerformIO . pipe'


getcwd : () -> String
getcwd = unsafePerformIO . getcwd'
    where
        getcwd' : () -> IO String
        getcwd' () = camCall (() -> IO String) (Builtin "filesystem_getcwd") ()

