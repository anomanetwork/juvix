module Juvix.Compiler.Tree.Pretty.Base
  ( module Juvix.Compiler.Tree.Pretty.Base,
    module Juvix.Compiler.Tree.Pretty.Options,
  )
where

import Data.ByteString qualified as BS
import Data.Foldable
import Data.HashMap.Strict qualified as HashMap
import Data.List.NonEmpty qualified as NonEmpty
import Juvix.Compiler.Core.Pretty.Base qualified as Core
import Juvix.Compiler.Internal.Data.Name
import Juvix.Compiler.Tree.Data.InfoTable
import Juvix.Compiler.Tree.Extra.Type.Base
import Juvix.Compiler.Tree.Language
import Juvix.Compiler.Tree.Language.Value
import Juvix.Compiler.Tree.Pretty.Extra
import Juvix.Compiler.Tree.Pretty.Options
import Juvix.Data.CodeAnn
import Juvix.Extra.Strings qualified as Str

doc :: (PrettyCode c) => Options -> c -> Doc Ann
doc opts =
  run
    . runReader opts
    . ppCode

class PrettyCode c where
  ppCode :: (Member (Reader Options) r) => c -> Sem r (Doc Ann)

wrapCore ::
  forall r' c.
  (Member (Reader Options) r') =>
  (forall r. (Member (Reader Core.Options) r) => c -> Sem r (Doc Ann)) ->
  c ->
  Sem r' (Doc Ann)
wrapCore f c = do
  opts <- ask
  return $ run $ runReader (toCoreOptions opts) $ f c

ppConstrName :: (Member (Reader Options) r) => Tag -> Sem r (Doc Ann)
ppConstrName tag = do
  tagNames <- asks (^. optTagNames)
  maybe
    (wrapCore Core.ppCode tag)
    (return . annotate (AnnKind KNameConstructor) . pretty . quoteName)
    (HashMap.lookup tag tagNames)

ppIndName :: (Member (Reader Options) r) => Symbol -> Sem r (Doc Ann)
ppIndName sym = do
  symNames <- asks (^. optSymbolNames)
  let iname = fromMaybe impossible $ HashMap.lookup sym symNames
  return $ annotate (AnnKind KNameInductive) (pretty (quoteName iname))

ppFunName :: (Member (Reader Options) r) => Symbol -> Sem r (Doc Ann)
ppFunName sym = do
  symNames <- asks (^. optSymbolNames)
  maybe
    ( return $
        annotate (AnnKind KNameFunction) $
          pretty ("unnamed_function_" ++ show sym :: String)
    )
    (return . annotate (AnnKind KNameFunction) . pretty . quoteFunName . quoteName)
    (HashMap.lookup sym symNames)

instance PrettyCode BuiltinDataTag where
  ppCode = wrapCore Core.ppCode

instance PrettyCode Tag where
  ppCode = ppConstrName

instance PrettyCode Constr where
  ppCode (Constr tag args) = do
    n' <- ppConstrName tag
    args' <- mapM (ppRightExpression appFixity) args
    return $ foldl' (<+>) n' args'

instance PrettyCode Closure where
  ppCode (Closure sym args) = do
    n' <- ppFunName sym
    args' <- mapM (ppRightExpression appFixity) args
    return $ foldl' (<+>) n' args'

instance PrettyCode Value where
  ppCode = \case
    ValInteger i ->
      return $ integer i
    ValField i ->
      return $ integer i
    ValBool True ->
      return $ annotate (AnnKind KNameConstructor) (pretty (Str.true_ :: String))
    ValBool False ->
      return $ annotate (AnnKind KNameConstructor) (pretty (Str.false_ :: String))
    ValString txt ->
      return $ annotate AnnLiteralString (pretty (show txt :: String))
    ValUnit ->
      return $ annotate (AnnKind KNameConstructor) (pretty (Str.unit :: String))
    ValVoid ->
      return $ annotate (AnnKind KNameConstructor) (pretty (Str.void :: String))
    ValConstr c ->
      ppCode c
    ValClosure cl ->
      ppCode cl
    ValUInt8 i ->
      return $ integer i
    ValByteArray bs -> ppCode bs

instance PrettyCode TypeInductive where
  ppCode :: (Member (Reader Options) r) => TypeInductive -> Sem r (Doc Ann)
  ppCode TypeInductive {..} = ppIndName _typeInductiveSymbol

instance PrettyCode TypeConstr where
  ppCode :: (Member (Reader Options) r) => TypeConstr -> Sem r (Doc Ann)
  ppCode TypeConstr {..} = do
    iname <- ppIndName _typeConstrInductive
    cname <- ppConstrName _typeConstrTag
    args <- mapM ppCode _typeConstrFields
    return $ iname <> kwColon <> cname <> parens (hsep (punctuate comma args))

instance PrettyCode TypeFun where
  ppCode :: (Member (Reader Options) r) => TypeFun -> Sem r (Doc Ann)
  ppCode TypeFun {..} = do
    l <-
      if
          | null (NonEmpty.tail _typeFunArgs) ->
              ppLeftExpression funFixity (head _typeFunArgs)
          | otherwise -> do
              args <- mapM ppCode _typeFunArgs
              return $ parens $ hsep $ punctuate comma (toList args)
    r <- ppRightExpression funFixity _typeFunTarget
    return $ l <+> kwArrow <+> r

instance PrettyCode Type where
  ppCode :: (Member (Reader Options) r) => Type -> Sem r (Doc Ann)
  ppCode = \case
    TyDynamic ->
      return $ annotate (AnnKind KNameInductive) Str.mul
    t@(TyInteger {})
      | t == mkTypeUInt8 ->
          return $ annotate (AnnKind KNameInductive) Str.uint8
    TyInteger {} -> return $ annotate (AnnKind KNameInductive) Str.integer
    TyField {} ->
      return $ annotate (AnnKind KNameInductive) Str.field
    TyByteArray {} ->
      return $ annotate (AnnKind KNameInductive) Str.byteArray
    TyRandomGenerator {} ->
      return $ annotate (AnnKind KNameInductive) Str.randomGenerator
    TyBool {} ->
      return $ annotate (AnnKind KNameInductive) Str.bool
    TyString ->
      return $ annotate (AnnKind KNameInductive) Str.string
    TyUnit ->
      return $ annotate (AnnKind KNameInductive) Str.unit
    TyVoid ->
      return $ annotate (AnnKind KNameInductive) Str.void
    TyInductive x ->
      ppCode x
    TyConstr x ->
      ppCode x
    TyFun x ->
      ppCode x

ppOffsetRef :: Text -> OffsetRef -> Sem r (Doc Ann)
ppOffsetRef str OffsetRef {..} =
  return $ maybe (variable str <> lbracket <> integer _offsetRefOffset <> rbracket) (variable . quoteName) _offsetRefName

instance PrettyCode RefTemp where
  ppCode = ppOffsetRef Str.tmp . (^. refTempOffsetRef)

instance PrettyCode DirectRef where
  ppCode = \case
    ArgRef roff -> ppOffsetRef Str.arg roff
    TempRef roff -> ppCode roff

instance PrettyCode Field where
  ppCode :: (Member (Reader Options) r) => Field -> Sem r (Doc Ann)
  ppCode Field {..} = do
    dr <- ppCode _fieldRef
    ctr <- ppConstrName _fieldTag
    return $ dr <> dot <> ctr <> lbracket <> integer _fieldOffset <> rbracket

instance PrettyCode MemRef where
  ppCode :: (Member (Reader Options) r) => MemRef -> Sem r (Doc Ann)
  ppCode = \case
    DRef dr -> ppCode dr
    ConstrRef fld -> ppCode fld

instance PrettyCode Constant where
  ppCode = \case
    ConstInt v ->
      return $ annotate AnnLiteralInteger (pretty v)
    ConstField v ->
      return $ annotate AnnLiteralInteger (pretty v <> "F")
    ConstBool True ->
      return $ annotate (AnnKind KNameConstructor) Str.true_
    ConstBool False ->
      return $ annotate (AnnKind KNameConstructor) Str.false_
    ConstString txt ->
      return $ annotate AnnLiteralString (pretty (show txt :: String))
    ConstUnit {} ->
      return $ annotate (AnnKind KNameConstructor) Str.unit
    ConstVoid {} ->
      return $ annotate (AnnKind KNameConstructor) Str.void
    ConstUInt8 v ->
      return $ annotate AnnLiteralInteger (pretty v <> "u8")
    ConstByteArray v -> do
      ctorOp <- ppCode OpByteArrayFromListUInt8
      bs <- ppCode v
      return (ctorOp <> parens bs)

instance PrettyCode ByteString where
  ppCode bs = do
    ppBytes <- mapM ppCode (ConstUInt8 <$> BS.unpack bs)
    return (toListCtors ppBytes)
    where
      toListCtors :: [Doc Ann] -> Doc Ann
      toListCtors = \case
        [] -> nodeAllocCtor Str.nil []
        (x : xs) -> nodeAllocCtor Str.cons [x, toListCtors xs]

      nodeAllocCtor :: Text -> [Doc Ann] -> Doc Ann
      nodeAllocCtor n args = primitive Str.instrAlloc <> brackets (pretty n) <> parens (ppCodeArgs' args)

instance PrettyCode BoolOp where
  ppCode op = return $ primitive $ case op of
    OpIntLt -> Str.instrLt
    OpIntLe -> Str.instrLe
    OpEq -> Str.instrEq

instance PrettyCode BinaryOp where
  ppCode = \case
    OpBool x -> ppCode x
    op ->
      return $ primitive $ case op of
        OpIntAdd -> Str.instrAdd
        OpIntSub -> Str.instrSub
        OpIntMul -> Str.instrMul
        OpIntDiv -> Str.instrDiv
        OpIntMod -> Str.instrMod
        OpFieldAdd -> Str.fadd
        OpFieldSub -> Str.fsub
        OpFieldMul -> Str.fmul
        OpFieldDiv -> Str.fdiv
        OpStrConcat -> Str.instrStrConcat

instance PrettyCode BinaryOpcode where
  ppCode = \case
    PrimBinop x -> ppCode x
    OpSeq -> return $ primitive Str.sseq_

instance PrettyCode NodeBinop where
  ppCode NodeBinop {..} = do
    op <- ppCode _nodeBinopOpcode
    arg1 <- ppCode _nodeBinopArg1
    arg2 <- ppCode _nodeBinopArg2
    return $ op <> parens (arg1 <> comma <+> arg2)

instance PrettyCode UnaryOp where
  ppCode op = return $ primitive $ case op of
    OpShow -> Str.instrShow
    OpStrToInt -> Str.instrStrToInt
    OpFieldToInt -> Str.instrFieldToInt
    OpIntToField -> Str.instrIntToField
    OpArgsNum -> Str.instrArgsNum
    OpIntToUInt8 -> Str.instrIntToUInt8
    OpUInt8ToInt -> Str.instrUInt8ToInt

instance PrettyCode ByteArrayOp where
  ppCode =
    return . \case
      OpByteArrayFromListUInt8 -> Str.instrByteArrayFromListUInt8
      OpByteArrayLength -> Str.instrByteArrayLength

instance PrettyCode CairoOp where
  ppCode op = return $ primitive $ case op of
    OpCairoPoseidon -> Str.instrPoseidon
    OpCairoEc -> Str.instrEcOp
    OpCairoRandomEcPoint -> Str.cairoRandomEcPoint

instance PrettyCode AnomaOp where
  ppCode op = return . primitive $ case op of
    OpAnomaGet -> Str.anomaGet
    OpAnomaEncode -> Str.anomaEncode
    OpAnomaDecode -> Str.anomaDecode
    OpAnomaVerifyDetached -> Str.anomaVerifyDetached
    OpAnomaSign -> Str.anomaSign
    OpAnomaVerifyWithMessage -> Str.anomaVerifyWithMessage
    OpAnomaSignDetached -> Str.anomaSignDetached
    OpAnomaByteArrayFromAnomaContents -> Str.anomaByteArrayFromAnomaContents
    OpAnomaByteArrayToAnomaContents -> Str.anomaByteArrayToAnomaContents
    OpAnomaSha256 -> Str.anomaSha256
    OpAnomaResourceCommitment -> Str.anomaResourceCommitment
    OpAnomaResourceNullifier -> Str.anomaResourceNullifier
    OpAnomaResourceKind -> Str.anomaResourceKind
    OpAnomaResourceDelta -> Str.anomaResourceDelta
    OpAnomaActionDelta -> Str.anomaActionDelta
    OpAnomaActionsDelta -> Str.anomaActionsDelta
    OpAnomaProveAction -> Str.anomaProveAction
    OpAnomaProveDelta -> Str.anomaProveDelta
    OpAnomaZeroDelta -> Str.anomaZeroDelta
    OpAnomaAddDelta -> Str.anomaAddDelta
    OpAnomaSubDelta -> Str.anomaSubDelta
    OpAnomaRandomGeneratorInit -> Str.anomaRandomGeneratorInit
    OpAnomaRandomNextBytes -> Str.anomaRandomNextBytes
    OpAnomaRandomSplit -> Str.anomaRandomSplit
    OpAnomaIsCommitment -> Str.anomaIsCommitment
    OpAnomaIsNullifier -> Str.anomaIsNullifier
    OpAnomaSetToList -> Str.anomaSetToList
    OpAnomaSetFromList -> Str.anomaSetFromList

instance PrettyCode UnaryOpcode where
  ppCode = \case
    PrimUnop x -> ppCode x
    OpAssert -> return $ primitive Str.instrAssert
    OpTrace -> return $ primitive Str.instrTrace
    OpFail -> return $ primitive Str.instrFailure

instance PrettyCode NodeUnop where
  ppCode NodeUnop {..} = do
    op <- ppCode _nodeUnopOpcode
    arg <- ppCode _nodeUnopArg
    return $ op <> parens arg

instance PrettyCode NodeCairo where
  ppCode NodeCairo {..} = do
    op <- ppCode _nodeCairoOpcode
    args <- ppCodeArgs _nodeCairoArgs
    return $ op <> parens args

instance PrettyCode NodeAnoma where
  ppCode NodeAnoma {..} = do
    op <- ppCode _nodeAnomaOpcode
    args <- ppCodeArgs _nodeAnomaArgs
    return (op <> parens args)

instance PrettyCode NodeByteArray where
  ppCode NodeByteArray {..} = do
    op <- ppCode _nodeByteArrayOpcode
    args <- ppCodeArgs _nodeByteArrayArgs
    return (op <> parens args)

instance PrettyCode NodeConstant where
  ppCode NodeConstant {..} = ppCode _nodeConstant

instance PrettyCode NodeMemRef where
  ppCode NodeMemRef {..} = ppCode _nodeMemRef

ppCodeArgs :: (Member (Reader Options) r) => [Node] -> Sem r (Doc Ann)
ppCodeArgs args = do
  args' <- mapM ppCode args
  return $ ppCodeArgs' args'

ppCodeArgs' :: [Doc Ann] -> Doc Ann
ppCodeArgs' args = hsep $ punctuate comma args

instance PrettyCode NodeAllocConstr where
  ppCode NodeAllocConstr {..} = do
    c <- ppConstrName _nodeAllocConstrTag
    args <- ppCodeArgs _nodeAllocConstrArgs
    return $ primitive Str.instrAlloc <> brackets c <> parens args

instance PrettyCode NodeAllocClosure where
  ppCode NodeAllocClosure {..} = do
    fn <- ppFunName _nodeAllocClosureFunSymbol
    args <- ppCodeArgs _nodeAllocClosureArgs
    return $ primitive Str.instrCalloc <> brackets fn <> parens args

instance PrettyCode NodeExtendClosure where
  ppCode NodeExtendClosure {..} = do
    args <- ppCodeArgs (_nodeExtendClosureFun : toList _nodeExtendClosureArgs)
    return $ primitive Str.instrCextend <> parens args

instance PrettyCode NodeCall where
  ppCode NodeCall {..} = case _nodeCallType of
    CallFun sym -> do
      fn <- ppFunName sym
      args <- ppCodeArgs _nodeCallArgs
      return $ primitive Str.instrCall <> brackets fn <> parens args
    CallClosure cl -> do
      args <- ppCodeArgs (cl : _nodeCallArgs)
      return $ primitive Str.instrCall <> parens args

instance PrettyCode NodeCallClosures where
  ppCode NodeCallClosures {..} = do
    args <- ppCodeArgs (_nodeCallClosuresFun : toList _nodeCallClosuresArgs)
    return $ primitive Str.instrCcall <> parens args

instance PrettyCode NodeBranch where
  ppCode NodeBranch {..} = do
    arg <- ppCode _nodeBranchArg
    br1 <- ppCode _nodeBranchTrue
    br2 <- ppCode _nodeBranchFalse
    let true_ = annotate (AnnKind KNameConstructor) Str.true_
        false_ = annotate (AnnKind KNameConstructor) Str.false_
    return $
      primitive Str.instrBr
        <> parens arg
        <+> braces' (true_ <> colon <+> br1 <> line <> false_ <> colon <+> br2)

instance PrettyCode CaseBranch where
  ppCode :: (Member (Reader Options) r) => CaseBranch -> Sem r (Doc Ann)
  ppCode CaseBranch {..} = do
    name <- ppConstrName _caseBranchTag
    br <- ppCode _caseBranchBody
    let br' = if _caseBranchSave then primitive Str.save <+> braces' br else br
    return $ name <> colon <+> br'

ppDefaultBranch :: (Member (Reader Options) r) => Node -> Sem r (Doc Ann)
ppDefaultBranch br = do
  br' <- ppCode br
  return $ annotate (AnnKind KNameConstructor) Str.default_ <> colon <+> br'

instance PrettyCode NodeCase where
  ppCode NodeCase {..} = do
    ind <- ppIndName _nodeCaseInductive
    arg <- ppCode _nodeCaseArg
    brs <- mapM ppCode _nodeCaseBranches
    def <- maybe (return Nothing) (fmap Just . ppDefaultBranch) _nodeCaseDefault
    let args' = vsep (brs <> (maybeToList def))
    return $ primitive Str.case_ <> brackets ind <> parens arg <+> braces' args'

instance PrettyCode NodeSave where
  ppCode :: (Member (Reader Options) r) => NodeSave -> Sem r (Doc Ann)
  ppCode NodeSave {..} = do
    arg <- ppCode _nodeSaveArg
    body <- ppCode _nodeSaveBody
    let name =
          case _nodeSaveTempVar ^. tempVarName of
            Just n -> brackets (variable (quoteName n))
            Nothing -> mempty
    return $ primitive Str.save <> name <> parens arg <+> braces' body

instance PrettyCode Node where
  ppCode = \case
    Binop x -> ppCode x
    Unop x -> ppCode x
    ByteArray x -> ppCode x
    Anoma x -> ppCode x
    Cairo x -> ppCode x
    Constant x -> ppCode x
    MemRef x -> ppCode x
    AllocConstr x -> ppCode x
    AllocClosure x -> ppCode x
    ExtendClosure x -> ppCode x
    Call x -> ppCode x
    CallClosures x -> ppCode x
    Branch x -> ppCode x
    Case x -> ppCode x
    Save x -> ppCode x

instance (PrettyCode a) => PrettyCode [a] where
  ppCode x = do
    cs <- mapM ppCode x
    return $ encloseSep "[" "]" ", " cs

ppFunInfo :: (Member (Reader Options) r) => (t -> Sem r (Doc Ann)) -> FunctionInfo' t e -> Sem r (Doc Ann)
ppFunInfo ppCode' FunctionInfo {..} = do
  argtys <- mapM ppCode (take _functionArgsNum (typeArgs _functionType))
  let argnames = map (fmap (variable . quoteName)) _functionArgNames
      args = zipWithExact (\mn ty -> maybe mempty (\n -> n <+> colon <> space) mn <> ty) argnames argtys
  targetty <- ppCode (if _functionArgsNum == 0 then _functionType else typeTarget _functionType)
  c <- ppCode' _functionCode
  return $
    keyword Str.function
      <+> annotate (AnnKind KNameFunction) (pretty (quoteFunName $ quoteName _functionName))
        <> parens (hsep (punctuate comma args))
      <+> colon
      <+> targetty
      <+> braces' c

ppFunSig :: (Member (Reader Options) r) => FunctionInfo' t e -> Sem r (Doc Ann)
ppFunSig FunctionInfo {..} = do
  argtys <- mapM ppCode (typeArgs _functionType)
  targetty <- ppCode (typeTarget _functionType)
  return $
    keyword Str.function
      <+> annotate (AnnKind KNameFunction) (pretty (quoteFunName $ quoteName _functionName))
        <> parens (hsep (punctuate comma argtys))
      <+> colon
      <+> targetty
        <> semi

instance PrettyCode ConstructorInfo where
  ppCode ConstructorInfo {..} = do
    ty <- ppCode _constructorType
    return $ annotate (AnnKind KNameConstructor) (pretty (quoteName _constructorName)) <+> colon <+> ty

ppInductive :: (Member (Reader Options) r) => InfoTable' t e -> InductiveInfo -> Sem r (Doc Ann)
ppInductive tab InductiveInfo {..} = do
  ctrs <- mapM (ppCode . lookupTabConstrInfo tab) _inductiveConstructors
  return $ kwInductive <+> annotate (AnnKind KNameInductive) (pretty (quoteName _inductiveName)) <+> braces' (vcat (map (<> semi) ctrs))

ppInfoTable :: (Member (Reader Options) r) => (t -> Sem r (Doc Ann)) -> InfoTable' t e -> Sem r (Doc Ann)
ppInfoTable ppCode' tab@InfoTable {..} = do
  inds <- mapM (ppInductive tab) (sortOn (^. inductiveLocation) $ HashMap.elems (filterOutBuiltins _infoInductives))
  funsigs <- mapM ppFunSig (sortOn (^. functionLocation) $ HashMap.elems _infoFunctions)
  funs <- mapM (ppFunInfo ppCode') (sortOn (^. functionLocation) $ HashMap.elems _infoFunctions)
  return $ vcat (map (<> line) inds) <> line <> vcat funsigs <> line <> line <> vcat (map (<> line) funs)
  where
    filterOutBuiltins :: HashMap Symbol InductiveInfo -> HashMap Symbol InductiveInfo
    filterOutBuiltins =
      HashMap.filter
        ( \ii -> case ii ^. inductiveConstructors of
            BuiltinTag _ : _ -> False
            UserTag {} : _ -> True
            [] -> True
        )

instance PrettyCode InfoTable where
  ppCode = ppInfoTable ppCode

{--------------------------------------------------------------------------------}
{- helper functions -}

ppRightExpression ::
  (PrettyCode a, HasAtomicity a, Member (Reader Options) r) =>
  Fixity ->
  a ->
  Sem r (Doc Ann)
ppRightExpression = ppLRExpression isRightAssoc

ppLeftExpression ::
  (PrettyCode a, HasAtomicity a, Member (Reader Options) r) =>
  Fixity ->
  a ->
  Sem r (Doc Ann)
ppLeftExpression = ppLRExpression isLeftAssoc

ppLRExpression ::
  (HasAtomicity a, PrettyCode a, Member (Reader Options) r) =>
  (Fixity -> Bool) ->
  Fixity ->
  a ->
  Sem r (Doc Ann)
ppLRExpression associates fixlr e =
  parensIf (atomParens associates (atomicity e) fixlr)
    <$> ppCode e
