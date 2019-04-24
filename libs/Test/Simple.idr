module Test.Simple
import Cam.FFI
import Cam.OS.FileSystem
import Cam.IO
import Cam.Data.Collections
import Cam.Data.FCollections
import Cam.Data.Compat
import Data.Vect
import Data.HVect

%hide HVect.index
%hide Vect.index
%hide Vect.reverse

%access export

%inline
capitalize : String -> FFI.IO String
capitalize s = camCall (String -> FFI.IO String) (Builtin "str_cap") s


f : StaticSized c => TypeHolder c -> Nat
f d = typeSize d

testSimple : FFI.IO ()
testSimple = do
      mlstyle <- camImport $ TheModule "MLStyle"
      fprintln mlstyle
      mlstyle_gen_match <- camImportFrom mlstyle "gen_match"
      fprintln mlstyle_gen_match
      println $ show hvec2
      println $ show a
      println $ show reva
      println $ show e
      println $ show hvec
      println $ show hvecItem
      println $ "test flist reverse :" ++ show flist_rev
      println $ "test flist to list :" ++ show lst2
      println $ "test fhvect:" ++ show fhvect
  where
      lst : List RawInteger
      lst = map toForeign [1, 2, 3]

      a : Vect 3 Int
      a = [1, 2, 3]
      nnn : Nat
      nnn = f (MkTypeHolder (Vect 3 Int))

      reva : Vect 3 Int
      reva = reverse a

      e : Int
      e = index (the (Fin _) $ fromInteger 1) a

      hvec : HVect [Int, Double]
      hvec = [1, 2.0]
            
      hvecItem : Double
      hvecItem = index (the (Fin _) $ fromInteger 1) hvec

      hvec2 : HVect [Double, Int]
      hvec2 = reverse hvec

      flist : FList Integer
      flist = toForeign lst
      
      lst2  : List Integer
      lst2 = map toNative (toNative flist)

      flist_item : ComRaw Integer
      flist_item = index 2 flist

      flist_rev: FList Integer
      flist_rev = reverse flist

      flist_size: Nat
      flist_size = size flist
  
      toFI : Integer -> ComRaw Int
      toFI = toForeign . the Int . fromInteger

      fvect: FVect 2 Int
      fvect = toForeign (with Vect [toFI 1, toFI 3])

      fvect_item : ComRaw Int
      fvect_item = index (the (Fin _) $ fromInteger 1) fvect

      fvect_rev: FVect 2 Int
      fvect_rev = reverse fvect

      fhvect : FHVect [String, Int]
      fhvect = toForeign $ the (HVect _) [toForeign "1", toFI 3]

      fhvect_item : ComRaw String
      fhvect_item = index (the (Fin _) $ fromInteger 0) fhvect

      fhvect_rev: FHVect [Int, String]
      fhvect_rev = reverse fhvect

      hvect_f : HVect [RawInt, ComRaw String]
      hvect_f = toNative fhvect_rev
