module Cam.Platform
import Cam.FFI

public export
%inline
platformName : () -> String
platformName =
    unsafePerformIO . f
    where
        f : () -> IO String
        f () = camCall (() -> IO String) (Builtin "platform_name") ()


public export
%inline
platformSystem : () -> String
platformSystem =
    unsafePerformIO . f
    where
        f : () -> IO String
        f () = camCall (() -> IO String) (Builtin "platform_system") ()