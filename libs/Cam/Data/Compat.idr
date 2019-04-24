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
    toForeign   : b -> a

implementation Mapping (RawInt) Int where
    toNative =  believe_me
    toForeign = believe_me

implementation Mapping (RawDouble) Double where
    toNative =  believe_me
    toForeign = believe_me

implementation Mapping (RawInteger) Integer where
    toNative =  believe_me
    toForeign = believe_me

implementation Mapping (RawUnit) Unit where
    toNative =  believe_me
    toForeign = believe_me

implementation Mapping (RawChar) Char where
    toNative =  believe_me
    toForeign = believe_me

implementation Mapping (FList t) (List (ComRaw t)) where
    toNative a = believe_me . unsafePerformIO $
                    camCall (FList t -> FFI.IO Ptr) (Builtin "flist_to_native") a
    toForeign b = unsafePerformIO $
                    camCall (Ptr -> FFI.IO  (FList t)) (Builtin "list_to_foreign") (believe_me b)

implementation Mapping (FVect n t) (Vect n (ComRaw t)) where
    toNative {n} a = believe_me . unsafePerformIO $
                        let sig = Integer -> FVect n t -> FFI.IO Ptr in
                        let n = the Integer $ fromNat n in
                        camCall sig (Builtin "fvect_to_native") n a
    toForeign {n} b = unsafePerformIO $
                        let sig = Integer -> Ptr -> FFI.IO  (FVect n t) in
                        let n = the Integer $ fromNat n in
                        camCall sig (Builtin "vect_to_foreign") n (believe_me b)

mapTypes : Vect n Type -> Vect n Type
mapTypes ts = map ComRaw ts

implementation Mapping (FHVect xs) (HVect (mapTypes xs)) where
    toNative {xs} a = believe_me . unsafePerformIO $
                        let sig = Integer -> FHVect xs -> FFI.IO Ptr in
                        let n = the Integer . fromNat $ size xs in
                        camCall sig (Builtin "fhvect_to_native") n a
    toForeign {xs} b = unsafePerformIO $
                        let sig = Integer -> Ptr -> FFI.IO  (FHVect xs) in
                        let n = the Integer . fromNat $ size xs in
                        camCall sig (Builtin "hvect_to_foreign") n (believe_me b)

implementation Mapping (ComRaw String) String where
    toNative a = unsafePerformIO $
                        camCall (ComRaw String -> FFI.IO String) (Builtin "fstr_to_native") a

    toForeign b = unsafePerformIO $
                        camCall (String -> FFI.IO (ComRaw String)) (Builtin "str_to_foreign") b

implementation Show (FList t) where
    show = toNative . toStr

implementation Show (FVect n t) where
    show = toNative . toStr

implementation Show (FHVect xs) where
    show = toNative . toStr                        

public export
data CamModule : String -> Type where
  TheModule : (s : String) -> CamModule s

%inline
camImport : CamModule s -> FFI.IO (ComRaw Ptr)
camImport {s} _ =
    camCall (String -> FFI.IO (ComRaw Ptr)) (Builtin "get_module") s

%inline
camImportFrom : ComRaw Ptr -> String -> ComRaw Ptr
camImportFrom p s =
  unsafePerformIO $
    let fieldname = toForeign s in
    camCall (ComRaw Ptr -> ComRaw String -> FFI.IO (ComRaw Ptr)) (Builtin "module_property") p fieldname
