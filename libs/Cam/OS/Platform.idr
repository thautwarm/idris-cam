module Cam.Platform
import Cam.FFI

public export
%inline
platformName : () -> Boxed String
platformName () = believe_me . unsafePerformIO $ fcall1 "platform_name" (believe_me ())

public export
%inline
platformSystem : () -> Boxed String
platformSystem () = believe_me . unsafePerformIO $ fcall1 "platform_system" (believe_me ())
