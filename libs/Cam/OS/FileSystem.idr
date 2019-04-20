module Cam.FileSystem
import Cam.FFI

public export
%inline
pipe : () -> (Int, Int)
pipe = believe_me . unsafePerformIO . f
    where
        f : () -> IO (ComRaw (Int, Int))
        f () = camCall (() -> IO (ComRaw (Int, Int))) (Builtin "filesystem_pipe") ()


public export
%inline
getcwd : () -> String
getcwd = unsafePerformIO . getcwd'
    where
        getcwd' : () -> IO String
        getcwd' () = camCall (() -> IO String) (Builtin "filesystem_getcwd") ()

