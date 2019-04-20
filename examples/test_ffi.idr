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
    data Com_Fn t = MkComFn t

    public export
    data Com_Raw t = MkComRaw t

    public export
    data Com_FunTypes : Type -> Type where
        Com_Fun       : Com s -> Com_FunTypes t -> Com_FunTypes (s -> t)
        Com_FnIO      : Com t -> Com_FunTypes (CamIO t)
        Com_FnBase    : Com t -> Com_FunTypes t

    public export
    data Com : Type -> Type where
        ComUnit   : Com ()
        ComStr    : Com String
        ComDouble : Com Double -- bit -> type
        ComInt    : Com Int    -- bit -> type
        ComUInt   : Com UInt   -- an example of extensive primitive type impl
        ComPtr    : Com Ptr
        ComFun    : Com_FunTypes a -> Com (Com_Fn a)
        ComChar   : Com Char
        ComRaw    : Com (Com_Raw a)

    public export
    data ForeignName
       = Builtin String
       | Library String String


    public export
    FFICam : FFI
    FFICam = MkFFI Com ForeignName String

    public export
    CamIO : Type -> Type
    CamIO = IO' FFICam


%inline
camCall: (ty : Type) -> (fname : ForeignName) -> {auto fty : FTy FFICam [] ty} -> ty
camCall ty fname = foreign FFICam fname ty

println : String -> CamIO ()
println s = camCall (String -> CamIO ()) (Builtin "println") s

data FileHandler;

openFile : String -> CamIO (Com_Raw FileHandler)
openFile filename = camCall (String -> CamIO (Com_Raw FileHandler)) (Builtin "simple_open") filename

readFile : Com_Raw FileHandler -> CamIO String
readFile handle = camCall (Com_Raw FileHandler -> CamIO String) (Builtin "simple_read") handle


main : CamIO ()
main = do
    fh <- openFile "./examples/a.idr"
    s <- readFile fh
    println s
