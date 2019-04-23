module Compat
import Cam.FFI


public export
interface Mapping a b where
    toForeigen : a -> b
    toNative   : b -> a

implementation Mapping (ComRaw Int) Int where
    toNative =  believe_me
    toForeigen = believe_me

implementation Mapping (ComRaw Double) Double where
    toNative =  believe_me
    toForeigen = believe_me

implementation Mapping (ComRaw Integer) Integer where
    toNative =  believe_me
    toForeigen = believe_me
