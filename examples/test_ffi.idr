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

public export
data Com : Type -> Type where
    ComUnit   : Com ()
    ComStr    : Com String
    ComDouble : Com Double -- bit -> type
    ComInt    : Com Int    -- bit -> type
    ComUInt   : Com UInt   -- an example of extensive primitive type impl
    ComPtr    : Com Ptr
    ComChar   : Com Char

public export
FFICam : FFI
FFICam = MkFFI Com String String

public export
CamIO : Type -> Type
CamIO = IO' FFICam

readFile : String -> CamIO String
readFile = foreign FFICam "read" (String -> CamIO String)


do_fopen : String -> String -> CamIO Ptr
do_fopen f m
   = foreign FFICam "open" (String -> String -> CamIO Ptr) f m


getErrno : CamIO Int
getErrno = foreign FFICam "idris_errno" (CamIO Int)


main : CamIO ()
main = do
    f <- do_fopen "a" "rb"
    pure $ ()
