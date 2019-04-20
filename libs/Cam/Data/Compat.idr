module Compat
import Cam.FFI

public export
interface Mapping a b where
    trans : a -> b
