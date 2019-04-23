{-
Foreign collections
  - FList   : foreign List
  - FVect   : foreign Vector
  - FHVect  : foreign Heterogeneous Vector

If your language supports some of them, just make sure
the relatedc builtins functions should be implemented in
your language, like
  python: `cam-python/idris_cam/rumtime.py'
  julia:  `cam-julia/src/Runtime.jl`
-}

module FCollections
import Data.Vect
import Data.HVect
import Cam.Data.Collections
import Cam.FFI
import Cam.IO

%access public export
%hide Vect.index
%hide HVect.index

instantiate : String -> Ptr
instantiate s = unsafePerformIO $ camCall (FFI.IO Ptr) (Builtin s)

-- maybe overflow
indexRawOF : String -> Int -> Ptr -> Ptr
indexRawOF s i v = let builtin = Builtin s in
                   let sig = Int -> Ptr -> FFI.IO Ptr in
                   let box = camCall sig builtin i v in
                   unsafePerformIO box

indexRaw : String -> Integer -> Ptr -> Ptr
indexRaw s i v = let builtin = Builtin s in
                  let sig = Integer -> Ptr -> FFI.IO Ptr in
                  let box = camCall sig builtin i v in
                  unsafePerformIO box

-- using `nat` data structure to index
indexRawNat : String -> Nat -> Ptr -> Ptr
indexRawNat s i v = let builtin = Builtin s in
                  let sig = Nat -> Ptr -> FFI.IO Ptr in
                  let box = camCall sig builtin i v in
                  unsafePerformIO box

-- Implementations for FList


sizeFList : FList a -> Integer
sizeFList lst = assert_total $
    let builtin = Builtin "size_flist" in
    let box = camCall (FList a -> FFI.IO Integer) builtin lst in
    unsafePerformIO box

implementation Sized (FList t) where
 -- It seems that using `size = sizeFlist` causes bad properties, not sure yet
  size lst = fromInteger $ sizeFList lst

implementation Indexable (FList t) Integer where
  eltype {t} _ _ = ComRaw t
  index {t} i xs = believe_me e
    where
      src : Ptr
      src = believe_me xs
      e : Ptr
      e = indexRaw "index_flist" i src


reverseFList : FList a -> FList a
reverseFList lst = let builtin = Builtin "reverse_flist" in
                   let box = camCall (FList a -> FFI.IO (FList a)) builtin lst in
                   unsafePerformIO box


implementation Reversable (FList a) (FList a) where
  reverse = reverseFList

-- Implementations for FVect

sizeFVect : FVect n a -> Nat
sizeFVect {n} _ = n

implementation StaticSized (FVect n a) where
  typeSize {n} _ = n

implementation Sized (FVect n a) where
  size vec = sizeFVect vec

implementation Indexable (FVect n t) (Fin n) where
  eltype {t} _ _ = ComRaw t
  index fin vec = believe_me e
    where
      i : Integer
      i = finToInteger fin
      src : Ptr
      src = believe_me vec
      e : Ptr
      e = indexRaw "index_fvect" i src

reverseFVect : FVect n a -> FVect n a
reverseFVect {n} vec = let builtin = Builtin "reverse_fvect" in
                       let box = camCall (FVect n a -> FFI.IO (FVect n a)) builtin vec in
                       unsafePerformIO box

implementation Reversable (FVect n a) (FVect n a) where
  reverse vec = reverseFVect vec


-- Implementations for FHVect

sizeFHVect : {xs: Vect n Type} -> FHVect xs -> Nat
sizeFHVect {n} _ = n

implementation StaticSized (FHVect xs) where
  typeSize {xs} _ = size xs

implementation Sized (FHVect xs) where
  size vec = sizeFHVect vec


-- It's tricky here for there're still some bugs in Idris.
-- related: https://github.com/idris-lang/Idris-dev/issues/3136


implementation Indexable (FHVect xs) (Fin (size xs)) where
  eltype {xs} fin _ = ComRaw (index fin xs)
  index fin vec = believe_me e
    where
      i : Integer
      i = finToInteger fin
      src : Ptr
      src = believe_me vec
      e : Ptr
      e = indexRaw "index_fhvect" i src

reverseFHVect : FHVect xs -> FHVect (reverseVect xs)
reverseFHVect {xs} vec = let builtin = Builtin "reverse_fvect" in
                         let box = camCall (FHVect xs -> FFI.IO(FHVect (reverseVect xs))) builtin vec in
                         unsafePerformIO box

implementation Reversable (FHVect xs) (FHVect (reverseVect xs)) where
  reverse vec = reverseFHVect vec
