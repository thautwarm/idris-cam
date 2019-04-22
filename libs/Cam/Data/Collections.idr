{-
Extensions for collection interfaces/traits.

Check following std modules as well:
      Prelude.Foldable
    , Prelude.Functor
    , Prelude.Traversable
    , Prelude.WellFounded (Sized)

-}
module Collections

import Data.Vect
import Data.HVect
import Prelude

%hide index
%hide reverse
%access public export


data TypeHolder : Type -> Type where
    MkTypeHolder : (a : Type) -> TypeHolder a

interface StaticSized c where
  typeSize : (TypeHolder c) -> Nat

interface Indexable c i where
  eltype : i -> c -> Type
  index  : (instI: i) -> (instC: c) -> eltype instI instC

interface Reversable c1 c2 | c1 where
   reverse : c1 -> c2



||| implementations for some builtin collection types

StaticSized (Vect n t) where
    typeSize {n} _ = n

Sized (Vect n t) where
     size {n} _ = n

Indexable (Vect n t) (Fin n) where
    eltype {t} _ _ = t
    index FZ (x :: xs) = x
    index (FS k) (x :: xs) = index k xs

Reversable (Vect n t) (Vect n t) where
  reverse {n} xs = rewrite plusCommutative Z n in rev {k1=n} {k2=Z} xs [] where
    rev : {k1: Nat} -> {k2: Nat} -> Vect k1 t -> Vect k2 t -> Vect (k1 + k2) t
    rev [] xs = xs
    rev {k1 = S k1} {k2 = k2} (x::xs1) xs2 =
      rewrite plusSuccRightSucc k1 k2 in
        rev {k1=k1} {k2=S k2} xs1 (x::xs2)

StaticSized (HVect xs) where
  typeSize {xs} _ = size xs

Sized (HVect xs) where
  size {xs} _ = size xs


Indexable (HVect xs) (Fin (size xs)) where
    eltype {xs} fin _ = index fin xs
    index FZ (x::xs) = x
    index (FS k) (x :: xs) = index k xs

