module Cam.Platform
import Cam.FFI

public export
%inline
platformName : () -> ComRaw String
platformName () = unsafePerformIO $ camCall (() -> IO (ComRaw String)) (Builtin "platform_name") ()

public export
%inline
platformSystem : () -> ComRaw String
platformSystem () = unsafePerformIO $  camCall (() -> IO (ComRaw String)) (Builtin "platform_system") ()