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

f : StaticSized c => TypeHolder c -> Nat
f d = typeSize d

testSimple : FFI.IO ()
testSimple = do
      mlstyle <- camImport $ TheModule "MLStyle"
      fprintln mlstyle           -- fprintln works for only foreign objects
      gen_match <- camImportFrom mlstyle "gen_match"
      println gen_match       -- println works for all objects
      file <- openFile "./text.txt" "r"
      text <- readAllText file
      closeFile file
      fprintln $ text
      println $ show hvec2
      putStrLn $ show a           -- putStrLn works for only String
      println $ show reva
      println $ show e
      println $ show hvec
      println $ show hvecItem
      println $ "test flist reverse :" ++ show flist_rev
      println $ "test flist to list :" ++ show lst2
      println $ "test fhvect:" ++ show fhvect
  where
      lst : List(Boxed Integer)
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

      flist_item : Boxed Integer
      flist_item = index 2 flist

      flist_rev: FList Integer
      flist_rev = reverse flist

      flist_size: Nat
      flist_size = size flist

      toFI : Integer -> Boxed Int
      toFI = toForeign . the Int . fromInteger

      fvect: FVect 2 Int
      fvect = toForeign (with Vect [toFI 1, toFI 3])

      fvect_item : Boxed Int
      fvect_item = index (the (Fin _) $ fromInteger 1) fvect

      fvect_rev: FVect 2 Int
      fvect_rev = reverse fvect

      fhvect : FHVect [String, Int]
      fhvect = toForeign $ the (HVect _) [toForeign "1", toFI 3]

      fhvect_item : Boxed String
      fhvect_item = index (the (Fin _) $ fromInteger 0) fhvect

      fhvect_rev: FHVect [Int, String]
      fhvect_rev = reverse fhvect

      hvect_f : HVect [Boxed Int, Boxed String]
      hvect_f = toNative fhvect_rev
