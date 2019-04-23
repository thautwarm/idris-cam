module FFI
import Data.Fin
%hide IO


public export
record UInt where
    -- Then we should instance some type class for
    -- UInt or other custom primitive types.
    -- Note!!! The impl for those primitive types
    -- could use foreign functions, which makes it
    -- super fast!
    constructor MkUInt
    i : Int


public export
unsigned : Int -> UInt
unsigned = MkUInt

mutual
    public export
    data ComFn t = MkComFn t

    public export
    data ComRaw t = MkComRaw t

    public export
    data ComFunTypes : Type -> Type where
        Com_FunType       : Com s -> ComFunTypes t -> ComFunTypes (s -> t)
        Com_FnIOType      : Com t -> ComFunTypes (IO t)
        Com_FnBase        : Com t -> ComFunTypes t

    public export
    data Com : Type -> Type where
        -- native
        Com_Unit    : Com ()
        Com_Str     : Com String
        Com_Double  : Com Double -- bit -> type
        Com_BigInt  : Com Integer
        Com_Int     : Com Int    -- bit -> type
        Com_UInt    : Com UInt   -- an example of extensive primitive type impl
        Com_Ptr     : Com Ptr
        Com_Fun     : ComFunTypes a -> Com (ComFn a)
        Com_Char    : Com Char
        Com_Raw     : Com (ComRaw a)
        -- special
        Com_Nat    : Com Nat
        Com_Fin    : Com (Fin n)


    public export
    data ForeignName
       = Builtin String
       | Library String String


    public export
    FFICam : FFI
    FFICam = MkFFI Com ForeignName String

    public export
    IO : Type -> Type
    IO = IO' FFICam


public export
%inline
camCall: (ty : Type) -> (fname : ForeignName) -> {auto fty : FTy FFICam [] ty} -> ty
camCall ty fname = foreign FFICam fname ty
