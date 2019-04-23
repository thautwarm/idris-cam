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

implementation StaticSized (Vect n t) where
    typeSize {n} _ = n

implementation Sized (Vect n t) where
     size {n} _ = n

implementation Indexable (Vect n t) (Fin n) where
    eltype {t} _ _ = t
    index FZ (x :: xs) = x
    index (FS k) (x :: xs) = index k xs

-- plusZCommu : (n: Nat) -> n = plus n Z
-- plusZCommu Z = Refl
-- plusZCommu (S k) = cong (plusZCommu k)

-- thanks to ice1000, this is extremely crucial.
appendLast : Vect a n -> n -> Vect (S a) n
appendLast [] x = x :: []
appendLast (y :: xs) x = y :: appendLast xs x

reverseVect : Vect a n -> Vect a n
reverseVect [] = []
reverseVect (x :: xs) = (reverseVect xs) `appendLast` x

implementation Reversable (Vect n t) (Vect n t) where
  reverse = reverseVect

implementation StaticSized (HVect xs) where
  typeSize {xs} _ = size xs

implementation Sized (HVect xs) where
  size {xs} _ = size xs

implementation Indexable (HVect xs) (Fin (size xs)) where
    eltype {xs} fin _ = index fin xs
    index FZ (x::xs) = x
    index (FS k) (x :: xs) = index k xs

popPush : (x: t) -> (ys: Vect n2 t) -> (xs: Vect n1 t) -> xs ++ x :: ys = (xs `appendLast` x) ++ ys
popPush x ys [] = Refl
popPush x ys (y :: xs) = rewrite popPush x ys xs in Refl

vectReverseProp : (x: t) -> (ys: Vect n2 t) -> (xs: Vect n1 t) -> reverseVect xs ++ x :: ys = reverseVect (x :: xs) ++ ys
vectReverseProp x ys xs =
      rewrite popPush x ys (reverseVect xs) in Refl

vectAppendLastProp : (x: t) -> (xs : Vect m t) ->  x :: (xs ++ []) = (x :: xs) ++ []
vectAppendLastProp x xs = Refl

zeroVectCommutativeProp : (vec: Vect n t) -> vec = vec ++ []
zeroVectCommutativeProp [] = Refl
zeroVectCommutativeProp (x::xs) = 
  rewrite zeroVectCommutativeProp xs in vectAppendLastProp x xs

-- convert implcit vars into the explicit ones and it works, which
--- is the 2nd suggestion from ice1000.
appendHList : (xs1 : Vect n1 Type) -> (xs2 : Vect n2 Type) ->
              HVect xs1 -> HVect xs2 -> HVect (reverseVect xs1 ++ xs2)
appendHList [] typs [] lst = lst
appendHList (x1::xs1) xs2 (elt1::lst1) lst2 =
    rewrite sym(popPush x1 xs2 (reverseVect xs1)) in
    appendHList xs1 (x1::xs2) lst1 (elt1::lst2)

implementation Reversable (HVect xs) (HVect (reverseVect xs)) where
  reverse {xs} lst =
    rewrite zeroVectCommutativeProp (reverseVect xs) in
    appendHList xs [] lst (with HVect [])
