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

implementation Mapping (Boxed Int) Int where
    toNative =  believe_me
    toForeign = believe_me

implementation Mapping (Boxed Double) Double where
    toNative =  believe_me
    toForeign = believe_me

implementation Mapping (Boxed Integer) Integer where
    toNative =  believe_me
    toForeign = believe_me

implementation Mapping (Boxed Unit) Unit where
    toNative =  believe_me
    toForeign = believe_me

implementation Mapping (Boxed Char) Char where
    toNative =  believe_me
    toForeign = believe_me

implementation Mapping (FList t) (List (Boxed t)) where
    toNative a = believe_me . unsafePerformIO $ fcall1 "flist_to_native" $ believe_me a
    toForeign b = believe_me . unsafePerformIO $ fcall1 "list_to_foreign" $ believe_me b

implementation Mapping (FVect n t) (Vect n (Boxed t)) where
    toNative {n} a = believe_me . unsafePerformIO $
                        let sig = Integer -> FVect n t -> FFI.IO Ptr in
                        let n = believe_me . the Integer $ fromNat n in
                        fcall2 "fvect_to_native" n $ believe_me a

    toForeign {n} b = believe_me . unsafePerformIO $
                        let sig = Integer -> Ptr -> FFI.IO  (FVect n t) in
                        let n = believe_me . the Integer $ fromNat n in
                        fcall2 "vect_to_foreign" n $ believe_me b

mapTypes : Vect n Type -> Vect n Type
mapTypes ts = map Boxed ts

implementation Mapping (FHVect xs) (HVect (mapTypes xs)) where
    toNative {xs} a = believe_me . unsafePerformIO $
                        let sig = Integer -> FHVect xs -> FFI.IO Ptr in
                        let n = believe_me . the Integer . fromNat $ size xs in
                        fcall2 "fhvect_to_native" n $ believe_me a
    toForeign {xs} b = believe_me . unsafePerformIO $
                        let sig = Integer -> Ptr -> FFI.IO  (FHVect xs) in
                        let n = believe_me . the Integer . fromNat $ size xs in
                        fcall2 "hvect_to_foreign" n $ believe_me b

implementation Mapping (Boxed String) String where
    toNative a = believe_me . unsafePerformIO $ fcall1 "fstr_to_native" $ believe_me a
    toForeign b = believe_me . unsafePerformIO $ fcall1 "str_to_foreign" $ believe_me b

implementation Show (FList t) where
    show = toNative . toText

implementation Show (FVect n t) where
    show = toNative . toText

implementation Show (FHVect xs) where
    show = toNative . toText

public export
data FModuleSpec : String -> Type where
  TheModule : (s : String) -> FModuleSpec s

||| foreign module
public export
FModule : String -> Type
FModule s = Boxed (FModuleSpec s)

%inline
camImport : FModuleSpec s -> FFI.IO (FModule s)
camImport {s} _ = believe_me $ fcall1 "get_module" $ believe_me s

%inline
camImportFrom : FModule mod_name -> String -> FFI.IO Unsafe
camImportFrom {mod_name} m str =
    let fieldname = believe_me $ the (Boxed String) $ toForeign str in
    fcall2 "module_property" (believe_me m) fieldname

%inline
unsafe : a -> Unsafe
unsafe a = unsafePerformIO $ fcall1 "identity" (believe_me a)

%inline
unsafeCall : Unsafe -> Unsafe -> FFI.IO Unsafe
unsafeCall f args = fcall2 "unsafe_call" f args



implicit iList2FList : List (Boxed a) -> FList a
iList2FList = toForeign

implicit iVect2FVect : Vect n (Boxed a) -> FVect n a
iVect2FVect = toForeign

implicit iHVect2FHVect : {xs1: Vect n Type} ->  HVect (map Boxed xs1) -> FHVect xs1
iHVect2FHVect = toForeign

implicit iString2Text : String -> Boxed String
iString2Text = toForeign

implicit iFList2List : FList a -> List (Boxed a)
iFList2List = toNative

implicit iFVect2Vect : FVect n a -> Vect n (Boxed a)
iFVect2Vect = toNative

implicit iFHVect2HVect : {xs1: Vect n Type} ->  FHVect xs1 -> HVect (map Boxed xs1)
iFHVect2HVect = toNative

implicit iText2String : Boxed String -> String
iText2String = toNative



