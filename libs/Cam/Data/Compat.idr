module Compat
import Cam.FFI

interface Mapping a b where
    trans : a -> b