{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module IRTS.CodegenPython(codegenPython) where
import Data.Map.Strict (Map)
import Prelude hiding (writeFile)
import qualified Data.Map.Strict as Map

import Idris.Core.TT
import IRTS.Lang
import IRTS.Simplified
import IRTS.CodegenCommon
import Data.Aeson.Text (encodeToLazyText)
import Data.Text.Lazy.IO (writeFile)
import Data.Aeson

import GHC.Generics
import Control.Arrow

data ComIR
    = ComLet String ComIR ComIR
    | ComLetrec [(String, ComIR)] ComIR
    | ComIf ComIR ComIR ComIR
    | ComWhile ComIR ComIR
    | ComMutate String ComIR
    | ComFun [String] ComIR
    | ComApp ComIR [ComIR]
    | ComVar String
    | ComBlock [ComIR]
    | ComTuple [ComIR]
    | ComProj ComIR ComIR

    | ComBigInt Integer
    | ComInt Int
    | ComDouble Double
    | ComStr String
    | ComCh Char
    | ComBool Bool
    | ComNil
    | ComInternal String
    deriving (Show, Eq, Ord, Generic)

instance ToJSON ComIR where
    toEncoding = genericToEncoding defaultOptions

asThunk :: ComIR -> ComIR
asThunk a = ComFun [] a

codegenPython :: CodeGenerator
codegenPython ci =
    writeFile filename (encodeToLazyText . toJSON $ ir)
    where
        filename = outputFile ci
        ir = mkDecls . fmap snd $ simpleDecls ci
        t = map (showCG . fst) . simpleDecls $ ci

localName i = show $ Loc i

decl :: SDecl -> (String, ComIR)
decl (SFun n ns _ body) =
    (showCG n, ComFun (fmap showCG ns) bindLocals)
    where
        bindLocals = foldr reducer (toIR body) $ zip infty ns
        reducer (i, n) inside = ComLet (localName i)  (toIR n) inside
        infty :: [Int]
        infty = [0..]

mkDecls :: [SDecl] -> ComIR -- top level let recs
mkDecls xs =
    ComLetrec (fmap decl xs) (ComInt 0)

pythonForce = ComInternal "idris-python-rt.force"
pythonCall = ComInternal "idris-python-rt.call"
pythonProj = ComInternal "idris-python-rt.proj"
pythonErr = ComInternal "idris-python-rt.err"
pythonCmp = ComInternal "idris-python-rt.cmp"

class HasIR a where
    toIR :: a -> ComIR

instance HasIR LVar where
    toIR = ComVar . show

instance HasIR  Name where
    toIR = ComVar . showCG

instance HasIR Const where
    toIR = \case
        I i -> ComInt i
        Fl d -> ComDouble d
        Ch c -> ComCh c
        Str s -> ComStr s
        BI i -> ComBigInt i
        a -> error $ "not impl: " ++ show a


instance HasIR SExp where
    toIR = \case
        SV var -> ComVar $ show var
        SApp _ name args   -> ComApp (toIR name) $ fmap toIR args
        SLet var v body    -> ComLet (show var) (toIR v) (toIR body)
        SUpdate var body   -> ComMutate (show var) (toIR body)
        SCon _ i name []   -> ComInt i
        SCon _ i name args -> ComTuple $ ComInt i : fmap toIR args
        SCase _ var' alts  -> foldr (flip reducer) ComNil alts where
                var = toIR var'
                reducer s = \case
                    SConCase _ i n ns body
                        | null ns ->
                            ComIf (ComApp pythonCmp [var, ComInt i]) s $ toIR body
                        | otherwise ->
                            ComIf (ComApp pythonCmp [ComProj var (ComInt 0), ComInt i]) s $ toIR body
                    SConstCase const body ->
                        ComIf (ComApp pythonCmp [var, toIR const]) s $ toIR body
                    SDefaultCase body -> toIR body
        -- temporarily I treat SChkCase as SCase, for I don't know their diffs accurately.
        SChkCase var' alts  -> foldr (flip reducer) ComNil alts where
                var = toIR var'
                reducer s = \case
                    SConCase _ i n ns body
                        | null ns ->
                            ComIf (ComApp pythonCmp [var, ComInt i]) s $ toIR body
                        | otherwise ->
                            ComIf (ComApp pythonCmp [ComProj var (ComInt 0), ComInt i]) s $ toIR body
                    SConstCase const body ->
                        ComIf (ComApp pythonCmp [var, toIR const]) s $ toIR body
                    SDefaultCase body -> toIR body
        SProj var i       -> ComProj (toIR var) (ComInt i)
        SConst const      -> toIR const
        SForeign a b c    -> error $ "SForeign " ++ show a ++ " " ++ show b ++ " " ++ show c
        SOp primfn vars   -> ComApp (toIR primfn) $ fmap toIR vars
        SNothing          -> ComNil -- will never be inspected
        SError s          -> ComApp pythonErr [ComStr s]

instance HasIR PrimFn where
    toIR = \case
        LPlus _ -> ComInternal "prim-plus"
        LMinus _ -> ComInternal "prim-minus"
        LTimes _ -> ComInternal "prim-times"
        LUDiv _ -> ComInternal "prim-udiv"
        LSDiv _ -> ComInternal "prim-sdiv"
        LURem _ -> ComInternal "prim-urem"
        LSRem _ -> ComInternal "prim-srem"
        LAnd _ -> ComInternal "prim-and"
        LOr _ -> ComInternal "prim-or"
        LXOr _ -> ComInternal "prim-xor"
        LCompl _ -> ComInternal "prim-compl"
        LSHL _ -> ComInternal "prim-shl"
        LLSHR _ -> ComInternal "prim-lshr"
        LASHR _ -> ComInternal "prim-ashr"
        LEq _ -> ComInternal "prim-eq"
        LLt _ -> ComInternal "prim-lt"
        LLe _ -> ComInternal "prim-le"
        LGt _ -> ComInternal "prim-gt"
        LGe _ -> ComInternal "prim-ge"
        LSLt _ -> ComInternal "prim-slt"
        LSLe _ -> ComInternal "prim-sle"
        LSGt _ -> ComInternal "prim-sgt"
        LSGe _ -> ComInternal "prim-sge"
        LSExt _ _ -> ComInternal "prim-sext"
        LZExt _ _ -> ComInternal "prim-zext"
        LTrunc _ _ -> ComInternal "prim-trunc"
        LStrConcat  -> ComInternal "prim-strconcat"
        LStrLt  -> ComInternal "prim-strlt"
        LStrEq  -> ComInternal "prim-streq"
        LStrLen  -> ComInternal "prim-strlen"
        LIntFloat _ -> ComInternal "prim-intfloat"
        LFloatInt _ -> ComInternal "prim-floatint"
        LIntStr _ -> ComInternal "prim-intstr"
        LStrInt _ -> ComInternal "prim-strint"
        LFloatStr  -> ComInternal "prim-floatstr"
        LStrFloat  -> ComInternal "prim-strfloat"
        LChInt _ -> ComInternal "prim-chint"
        LIntCh _ -> ComInternal "prim-intch"
        LBitCast _ _ -> ComInternal "prim-bitcast"
        LFExp  -> ComInternal "prim-fexp"
        LFLog  -> ComInternal "prim-flog"
        LFSin  -> ComInternal "prim-fsin"
        LFCos  -> ComInternal "prim-fcos"
        LFTan  -> ComInternal "prim-ftan"
        LFASin  -> ComInternal "prim-fasin"
        LFACos  -> ComInternal "prim-facos"
        LFATan  -> ComInternal "prim-fatan"
        LFATan2  -> ComInternal "prim-fatan2"
        LFSqrt  -> ComInternal "prim-fsqrt"
        LFFloor  -> ComInternal "prim-ffloor"
        LFCeil  -> ComInternal "prim-fceil"
        LFNegate  -> ComInternal "prim-fnegate"
        LStrHead  -> ComInternal "prim-strhead"
        LStrTail  -> ComInternal "prim-strtail"
        LStrCons  -> ComInternal "prim-strcons"
        LStrIndex  -> ComInternal "prim-strindex"
        LStrRev  -> ComInternal "prim-strrev"
        LStrSubstr  -> ComInternal "prim-strsubstr"
        LReadStr  -> ComInternal "prim-readstr"
        LWriteStr  -> ComInternal "prim-writestr"
        LSystemInfo  -> ComInternal "prim-systeminfo"
        LFork  -> ComInternal "prim-fork"
        LPar  -> ComInternal "prim-par"
        LExternal _ -> ComInternal "prim-external"
        LCrash  -> ComInternal "prim-crash"
        LNoOp  -> ComInternal "prim-noop"