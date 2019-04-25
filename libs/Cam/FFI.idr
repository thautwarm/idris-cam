module FFI
import Data.HVect
import Data.Vect

%hide IO
%access public export


data UnsafeRaw;    
data Boxed t = MkBoxed t
    
Unsafe : Type
Unsafe = Boxed UnsafeRaw

mutual
    
    data FList : Type -> Type where
        MockFList : (t: Type) -> FList t

    -- sized vector
    data FVect : Nat -> Type -> Type where
        MockFVect : (n: Nat) -> (t: Type) -> FVect n t

    -- Heterogeneous Vector.
    -- In many language, it's called `tuple`.
    data FHVect : Vect n Type -> Type where
        MockFHVect : (xs: Vect n Type) -> FHVect xs

    data CamTypes : Type -> Type where
        Cam_Unsafe   : CamTypes Unsafe

    data ForeignName
       = Builtin String
       | Library String String

    FFICam : FFI
    FFICam = MkFFI CamTypes ForeignName String

    public export
    IO : Type -> Type
    IO = IO' FFICam

FFun1 : Type
FFun1 = Unsafe -> IO Unsafe

FFun2 : Type
FFun2 = Unsafe -> Unsafe -> IO Unsafe

FFun3 : Type
FFun3 = Unsafe -> Unsafe -> Unsafe -> IO Unsafe

||| call foreign language apis
%inline
fcall: (ty : Type) -> (fname : ForeignName) -> {auto fty : FTy FFICam [] ty} -> ty
fcall ty fname = foreign FFICam fname ty

%inline
fcall1 : String -> Unsafe -> IO Unsafe
fcall1 n src = fcall FFun1 (Builtin n) src

%inline
fcall2 : String -> Unsafe ->  Unsafe -> IO Unsafe
fcall2 n src1 src2 = fcall FFun2 (Builtin n) src1 src2

%inline
fcall3 : String -> Unsafe ->  Unsafe -> Unsafe -> IO Unsafe
fcall3 n src1 src2 src3 = fcall FFun3 (Builtin n) src1 src2 src3

%inline
fassert: a -> b -> IO ()
fassert a b =
        let a = believe_me a in
        let b = believe_me b in
        believe_me $ fcall FFun2 (Builtin "cam-assert") a b
