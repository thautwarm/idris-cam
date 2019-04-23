module Compat
import Data.Vect
import Data.HVect
import Cam.FFI
import Cam.IO
import Cam.Data.Collections

%access export

public export
interface Mapping a b | a where
    toNative     : a -> b
    toForeigen   : b -> a

implementation Mapping (RawInt) Int where
    toNative =  believe_me
    toForeigen = believe_me

implementation Mapping (RawDouble) Double where
    toNative =  believe_me
    toForeigen = believe_me

implementation Mapping (RawInteger) Integer where
    toNative =  believe_me
    toForeigen = believe_me

implementation Mapping (RawUnit) Unit where
    toNative =  believe_me
    toForeigen = believe_me

implementation Mapping (RawChar) Char where
    toNative =  believe_me
    toForeigen = believe_me

implementation Mapping (FList t) (List (ComRaw t)) where
    toNative a = believe_me . unsafePerformIO $
                    camCall (FList t -> FFI.IO Ptr) (Builtin "flist_to_native") a
    toForeigen b = unsafePerformIO $
                    camCall (Ptr -> FFI.IO  (FList t)) (Builtin "list_to_foreign") (believe_me b)

implementation Mapping (FVect n t) (Vect n (ComRaw t)) where
    toNative {n} a = believe_me . unsafePerformIO $
                        camCall (Nat -> FVect n t -> FFI.IO Ptr) (Builtin "fvect_to_native") n a
    toForeigen {n} b = unsafePerformIO $
                        camCall (Nat -> Ptr -> FFI.IO  (FVect n t)) (Builtin "vect_to_foreign") n (believe_me b)


mapTypes : Vect n Type -> Vect n Type
mapTypes ts = map ComRaw ts

implementation Mapping (FHVect xs) (HVect (mapTypes xs)) where
    toNative {xs} a = believe_me . unsafePerformIO $
                        camCall (Nat -> FHVect xs -> FFI.IO Ptr) (Builtin "fhvect_to_native") (size xs) a
    toForeigen {xs} b = unsafePerformIO $
                        camCall (Nat -> Ptr -> FFI.IO  (FHVect xs)) (Builtin "hvect_to_foreign") (size xs) (believe_me b)

mutual
    implementation Mapping (ComRaw String) String where
        toNative a = unsafePerformIO $
                        camCall (ComRaw String -> FFI.IO String) (Builtin "fstr_to_native") a

        toForeigen b = unsafePerformIO $
                        camCall (String -> FFI.IO (ComRaw String)) (Builtin "str_to_foreign") b

    implementation Show (FList t) where
        show = toNative . toStr

    implementation Show (FVect n t) where
        show = toNative . toStr

    implementation Show (FHVect xs) where
        show = toNative . toStr