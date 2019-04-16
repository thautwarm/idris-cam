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
    writeFile (outputFile ci) (encodeToLazyText . toJSON $ ir)
    where ir = mkDecls . fmap snd $ simpleDecls ci

decl :: SDecl -> (String, ComIR)
decl (SFun n ns _ body) = (showCG n, ComFun (fmap showCG ns) $ toIR body)

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
        _ -> error "not impl"

instance HasIR PrimFn where
    toIR = \case
        LPlus _ -> ComInternal "prim-op-plus"
        LMinus _ -> ComInternal "prim-op-minus"
        LTimes _ -> ComInternal "prim-op-times"
        LUDiv _ -> ComInternal "prim-op-udiv"
        LSDiv _ -> ComInternal "prim-op-sdiv"
        LURem _ -> ComInternal "prim-op-urem"
        LSRem _ -> ComInternal "prim-op-srem"
        LAnd _ -> ComInternal "prim-op-and"
        LOr  _ -> ComInternal "prim-op-or"
        LXOr _ -> ComInternal "prim-op-xor"
        LCompl _ -> ComInternal "prim-op-compl"
        LSHL   _ -> ComInternal "prim-op-shl"
        LLSHR   _ -> ComInternal "prim-op-lshr"
        LASHR   _ -> ComInternal "prim-op-ashr"
        LEq   _ -> ComInternal "prim-op-eq"
        LLt   _ -> ComInternal "prim-op-lt"
        LLe   _ -> ComInternal "prim-op-Le"
        LGt   _ -> ComInternal "prim-op-gt"
        LGe   _ -> ComInternal "prim-op-ge"
        -- LSLt, LSLe, LSGt, LSGe, LSExt, LZExt, LTrunc
        LStrConcat -> ComInternal "prim-op-str_concat"
        -- LIntFloat, LFloatInt, LIntStr, LStrInt, LFloatStr, LStrFloat,
        -- LChInt, LIntCh, LBitCast
        -- 实锤idris弱类型语言
        LFExp -> ComInternal "prim-op-exp"
        LFNegate -> ComInternal "prim-op-neg"
        LExternal a -> ComApp (ComInternal "prim-op-external") [toIR a]
        a -> error $ "太多了, 8写了: " ++ show a


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