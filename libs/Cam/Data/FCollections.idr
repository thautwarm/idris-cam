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

-- maybe overflow
%inline
indexRawOF : String -> Int -> Unsafe -> Unsafe
indexRawOF s i v = let builtin = Builtin s in
                   let i = believe_me i in
                   let box = fcall FFun2 builtin i v in
                   believe_me $ unsafePerformIO box

%inline
indexRaw : String -> Integer -> Unsafe -> Unsafe
indexRaw s i v = let builtin = Builtin s in
                 let i = believe_me i in
                 let box = fcall FFun2 builtin i v in
                 believe_me $ unsafePerformIO box


-- using `nat` data structure to index
%inline
indexRawNat : String -> Nat -> Unsafe -> Unsafe
indexRawNat s i v = let builtin = Builtin s in
                    let i = believe_me i in
                    let box = fcall FFun2 builtin i v in
                    believe_me $ unsafePerformIO box

-- Implementations for FList
%inline
sizeFList : FList a -> Integer
sizeFList lst = assert_total $
    let builtin = Builtin "size_flist" in
    let box = fcall FFun1 builtin (believe_me lst) in
    believe_me $ unsafePerformIO box

implementation Sized (FList t) where
 -- It seems that using `size = sizeFlist` causes bad properties, not sure yet
  size lst = fromInteger $ sizeFList lst

implementation Indexable (FList t) Integer where
  eltype {t} _ _ = Boxed t
  index {t} i xs = believe_me e
    where
      src : Unsafe
      src = believe_me xs
      e : Unsafe
      e = indexRaw "index_flist" i src


reverseFList : FList a -> FList a
reverseFList lst = let builtin = Builtin "reverse_flist" in
                   let box = fcall FFun1 builtin (believe_me lst) in
                   believe_me $ unsafePerformIO box

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
  eltype {t} _ _ = Boxed t
  index fin vec = believe_me e
    where
      i : Integer
      i = finToInteger fin
      src : Unsafe
      src = believe_me vec
      e : Unsafe
      e = indexRaw "index_fvect" i src

reverseFVect : FVect n a -> FVect n a
reverseFVect {n} vec = let builtin = Builtin "reverse_fvect" in
                       let box = fcall FFun1 builtin $ believe_me vec in
                       believe_me $ unsafePerformIO box

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
  eltype {xs} fin _ = Boxed (index fin xs)
  index fin vec = believe_me e
    where
      i : Integer
      i = finToInteger fin
      src : Unsafe
      src = believe_me vec
      e : Unsafe
      e = indexRaw "index_fhvect" i src

reverseFHVect : FHVect xs -> FHVect (reverseVect xs)
reverseFHVect {xs} vec = let builtin = Builtin "reverse_fvect" in
                         let box = fcall FFun1 builtin $ believe_me vec in
                         believe_me $ unsafePerformIO box

implementation Reversable (FHVect xs) (FHVect (reverseVect xs)) where
  reverse vec = reverseFHVect vec
