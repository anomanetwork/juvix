module Juvix.Compiler.Core.Translation.FromSource
  ( module Juvix.Parser.Error,
    runParser,
    runParserMain,
    setupMainFunction,
  )
where

import Control.Monad.Combinators.NonEmpty qualified as NonEmpty
import Control.Monad.Trans.Class (lift)
import Data.HashMap.Strict qualified as HashMap
import Data.List.NonEmpty (fromList)
import Data.List.NonEmpty qualified as NonEmpty
import Juvix.Compiler.Core.Data.InfoTable
import Juvix.Compiler.Core.Data.InfoTableBuilder
import Juvix.Compiler.Core.Error
import Juvix.Compiler.Core.Extra
import Juvix.Compiler.Core.Info qualified as Info
import Juvix.Compiler.Core.Info.LocationInfo as LocationInfo
import Juvix.Compiler.Core.Info.NameInfo as NameInfo
import Juvix.Compiler.Core.Transformation.Eta
import Juvix.Compiler.Core.Translation.FromSource.Lexer
import Juvix.Data.CodeAnn (Ann)
import Juvix.Data.Field
import Juvix.Data.PPOutput (ppOutput)
import Juvix.Extra.Strings qualified as Str
import Juvix.Parser.Error
import Text.Megaparsec qualified as P

-- | Note: only new symbols and tags that are not in the InfoTable already will be
-- generated during parsing
runParser :: Path Abs File -> ModuleId -> InfoTable -> Text -> Either JuvixError (InfoTable, Maybe Node)
runParser fileName mid tab input_ =
  case run $
    runError @CoreError $
      runInfoTableBuilder (Module mid tab mempty mempty Nothing) $
        P.runParserT parseToplevel (fromAbsFile fileName) input_ of
    Left err -> Left (JuvixError err)
    Right (_, Left err) -> Left (JuvixError (MegaparsecError err))
    Right (md, Right r) -> Right (md ^. moduleInfoTable, r)

runParserMain :: Path Abs File -> ModuleId -> InfoTable -> Text -> Either JuvixError InfoTable
runParserMain fileName mid tab input_ =
  case runParser fileName mid tab input_ of
    Left err -> Left err
    Right (tab', Nothing) -> Right tab'
    Right (tab', Just node) -> Right $ setupMainFunction mid tab' node

setupMainFunction :: ModuleId -> InfoTable -> Node -> InfoTable
setupMainFunction mid tab node =
  tab
    { _infoMain = Just sym,
      _identContext = HashMap.insert sym node (tab ^. identContext),
      _infoIdentifiers = HashMap.insert sym info (tab ^. infoIdentifiers)
    }
  where
    symId = nextSymbolId tab
    sym = Symbol mid symId
    info =
      IdentifierInfo
        { _identifierName = freshIdentName' tab "main",
          _identifierLocation = Nothing,
          _identifierSymbol = sym,
          _identifierArgsNum = 0,
          _identifierType = mkDynamic',
          _identifierBuiltin = Nothing,
          _identifierIsExported = True,
          _identifierPragmas = mempty,
          _identifierArgNames = []
        }

throwCoreError ::
  (Members '[Error CoreError, InfoTableBuilder] r) =>
  Location ->
  Doc Ann ->
  ParsecS r a
throwCoreError i msg =
  lift $ throwError (CoreError (ppOutput msg) Nothing i)

guardSymbolNotDefined ::
  (Members '[Error CoreError, InfoTableBuilder] r) =>
  Symbol ->
  ParsecS r () ->
  ParsecS r ()
guardSymbolNotDefined sym err = do
  b <- lift $ checkSymbolDefined sym
  when b err

parseToplevel ::
  (Members '[Error CoreError, InfoTableBuilder] r) =>
  ParsecS r (Maybe Node)
parseToplevel = do
  lift declareIOBuiltins
  lift declareBoolBuiltins
  lift declareNatBuiltins
  lift declareMaybeBuiltins
  lift declareListBuiltins
  space
  P.endBy statement (kw delimSemicolon)
  r <- optional expression
  P.eof
  return r

statement ::
  (Members '[Error CoreError, InfoTableBuilder] r) =>
  ParsecS r ()
statement = statementBuiltin <|> void statementDef <|> statementInductive

statementBuiltin ::
  (Members '[Error CoreError, InfoTableBuilder] r) =>
  ParsecS r ()
statementBuiltin = do
  ((), i) <- interval $ kw kwBuiltin
  sym <- statementDef
  ii <- lift $ getIdentifierInfo sym
  if
      | ii ^. identifierName == Str.natPlus ->
          lift $ registerIdent (ii ^. identifierName) ii {_identifierBuiltin = Just BuiltinNatPlus}
      | ii ^. identifierName == Str.natSub ->
          lift $ registerIdent (ii ^. identifierName) ii {_identifierBuiltin = Just BuiltinNatSub}
      | ii ^. identifierName == Str.natMul ->
          lift $ registerIdent (ii ^. identifierName) ii {_identifierBuiltin = Just BuiltinNatMul}
      | ii ^. identifierName == Str.natDiv ->
          lift $ registerIdent (ii ^. identifierName) ii {_identifierBuiltin = Just BuiltinNatDiv}
      | ii ^. identifierName == Str.natMod ->
          lift $ registerIdent (ii ^. identifierName) ii {_identifierBuiltin = Just BuiltinNatMod}
      | ii ^. identifierName == Str.natUDiv ->
          lift $ registerIdent (ii ^. identifierName) ii {_identifierBuiltin = Just BuiltinNatUDiv}
      | ii ^. identifierName == Str.natLe ->
          lift $ registerIdent (ii ^. identifierName) ii {_identifierBuiltin = Just BuiltinNatLe}
      | ii ^. identifierName == Str.natLt ->
          lift $ registerIdent (ii ^. identifierName) ii {_identifierBuiltin = Just BuiltinNatLt}
      | ii ^. identifierName == Str.natEq ->
          lift $ registerIdent (ii ^. identifierName) ii {_identifierBuiltin = Just BuiltinNatEq}
      | otherwise -> throwCoreError i "unrecorgnized builtin definition"

statementDef ::
  (Members '[Error CoreError, InfoTableBuilder] r) =>
  ParsecS r Symbol
statementDef = do
  kw kwDef
  (txt, i) <- identifierL
  r <- lift (getIdent txt)
  case r of
    Just (IdentFun sym) -> do
      guardSymbolNotDefined
        sym
        (throwCoreError i ("duplicate definition of: " <> fromText txt))
      tab <- (^. moduleInfoTable) <$> lift getModule
      mty <- optional typeAnnotation
      let fi = fromMaybe impossible $ HashMap.lookup sym (tab ^. infoIdentifiers)
          ty = fromMaybe (fi ^. identifierType) mty
      unless (isDynamic (fi ^. identifierType) || ty == fi ^. identifierType) $
        throwCoreError i "type signature doesn't match earlier definition"
      parseDefinition sym ty
      return sym
    Just IdentInd {} ->
      throwCoreError i ("duplicate identifier: " <> fromText txt)
    Just IdentConstr {} ->
      throwCoreError i ("duplicate identifier: " <> fromText txt)
    Nothing -> do
      mty <- optional typeAnnotation
      sym <- lift freshSymbol
      let ty = fromMaybe mkDynamic' mty
          info =
            IdentifierInfo
              { _identifierName = txt,
                _identifierLocation = Just i,
                _identifierSymbol = sym,
                _identifierType = ty,
                _identifierArgsNum = 0,
                _identifierIsExported = False,
                _identifierBuiltin = Nothing,
                _identifierPragmas = mempty,
                _identifierArgNames = []
              }
      lift $ registerIdent txt info
      void $ optional (parseDefinition sym ty)
      return sym

parseDefinition ::
  (Members '[Error CoreError, InfoTableBuilder] r) =>
  Symbol ->
  Type ->
  ParsecS r ()
parseDefinition sym ty = do
  kw kwAssign
  (node, i) <- interval expression
  lift $ registerIdentNode sym node
  let (is, _) = unfoldLambdas node
  when
    ( length is > length (typeArgs ty)
        && not (isDynamic (typeTarget ty))
    )
    $ throwCoreError i "type mismatch: too many lambdas"
  lift $ setIdentArgs sym (map (^. lambdaLhsBinder) is)

statementInductive ::
  (Members '[Error CoreError, InfoTableBuilder] r) =>
  ParsecS r ()
statementInductive = do
  kw kwInductive
  (txt, i) <- identifierL
  idt <- lift $ getIdent txt
  when (isJust idt) $
    throwCoreError i ("duplicate identifier: " <> fromText txt)
  mty <- optional typeAnnotation
  sym <- lift freshSymbol
  let ii =
        InductiveInfo
          { _inductiveName = txt,
            _inductiveLocation = Just i,
            _inductiveSymbol = sym,
            _inductiveKind = fromMaybe (mkUniv' 0) mty,
            _inductiveConstructors = [],
            _inductiveParams = [],
            _inductivePositive = True,
            _inductiveBuiltin = Nothing,
            _inductivePragmas = mempty
          }
  lift $ registerInductive txt ii
  ctrs <- braces $ P.sepEndBy (constrDecl sym) (kw delimSemicolon)
  lift $ registerInductive txt ii {_inductiveConstructors = map (^. constructorTag) ctrs}

constrDecl ::
  (Members '[Error CoreError, InfoTableBuilder] r) =>
  Symbol ->
  ParsecS r ConstructorInfo
constrDecl symInd = do
  (txt, i) <- identifierL
  idt <- lift $ getIdent txt
  when (isJust idt) $
    throwCoreError i ("duplicate identifier: " <> fromText txt)
  tag <- lift freshTag
  ty <- typeAnnotation
  let argsNum = length (typeArgs ty)
      ci =
        ConstructorInfo
          { _constructorName = txt,
            _constructorLocation = Just i,
            _constructorTag = tag,
            _constructorArgsNum = argsNum,
            _constructorArgNames = replicate argsNum Nothing,
            _constructorType = ty,
            _constructorInductive = symInd,
            _constructorFixity = Nothing,
            _constructorBuiltin = Nothing,
            _constructorPragmas = mempty
          }
  lift $ registerConstructor txt ci
  return ci

typeAnnotation ::
  (Members '[Error CoreError, InfoTableBuilder] r) =>
  ParsecS r Type
typeAnnotation = do
  kw kwColon
  expression

expression ::
  (Members '[Error CoreError, InfoTableBuilder] r) =>
  ParsecS r Node
expression = do
  node <- expr 0 mempty
  md <- lift getModule
  return $ etaExpandApps md node

expr ::
  (Members '[Error CoreError, InfoTableBuilder] r) =>
  -- | current de Bruijn index, i.e., the number of binders upwards
  Index ->
  -- | reverse de Bruijn indices (de Bruijn levels)
  HashMap Text Level ->
  ParsecS r Node
expr = typeExpr

bracedExpr ::
  (Members '[Error CoreError, InfoTableBuilder] r) =>
  Index ->
  HashMap Text Level ->
  ParsecS r Node
bracedExpr varsNum vars = braces (expr varsNum vars) <|> expr varsNum vars

typeAnnot ::
  (Members '[Error CoreError, InfoTableBuilder] r) =>
  Index ->
  HashMap Text Level ->
  ParsecS r Node
typeAnnot varsNum vars = do
  kw kwColon
  expr varsNum vars

typeExpr ::
  (Members '[Error CoreError, InfoTableBuilder] r) =>
  Index ->
  HashMap Text Level ->
  ParsecS r Node
typeExpr varsNum vars = seqqExpr varsNum vars >>= typeExpr' varsNum vars

typeExpr' ::
  (Members '[Error CoreError, InfoTableBuilder] r) =>
  Index ->
  HashMap Text Level ->
  Node ->
  ParsecS r Node
typeExpr' varsNum vars node =
  typeFunExpr' varsNum vars node
    <|> return node

typeFunExpr' ::
  (Members '[Error CoreError, InfoTableBuilder] r) =>
  Index ->
  HashMap Text Level ->
  Node ->
  ParsecS r Node
typeFunExpr' varsNum vars l = do
  kw kwRightArrow
  r <- typeExpr (varsNum + 1) vars
  return $ mkPi' l r

seqqExpr ::
  (Members '[Error CoreError, InfoTableBuilder] r) =>
  Index ->
  HashMap Text Level ->
  ParsecS r Node
seqqExpr varsNum vars = do
  node <- ioExpr varsNum vars
  seqqExpr' varsNum vars node <|> return node

seqqExpr' ::
  (Members '[Error CoreError, InfoTableBuilder] r) =>
  Index ->
  HashMap Text Level ->
  Node ->
  ParsecS r Node
seqqExpr' varsNum vars node = do
  kw kwSeqq
  node' <- seqqExpr varsNum vars
  return $ mkBuiltinApp' OpSeq [node, node']

ioExpr ::
  (Members '[Error CoreError, InfoTableBuilder] r) =>
  Index ->
  HashMap Text Level ->
  ParsecS r Node
ioExpr varsNum vars = cmpExpr varsNum vars >>= ioExpr' varsNum vars

ioExpr' ::
  (Members '[Error CoreError, InfoTableBuilder] r) =>
  Index ->
  HashMap Text Level ->
  Node ->
  ParsecS r Node
ioExpr' varsNum vars node =
  bindExpr' varsNum vars node
    <|> seqExpr' varsNum vars node
    <|> return node

bindExpr' ::
  (Members '[Error CoreError, InfoTableBuilder] r) =>
  Index ->
  HashMap Text Level ->
  Node ->
  ParsecS r Node
bindExpr' varsNum vars node = do
  kw kwBindOperator
  node' <- cmpExpr varsNum vars
  ioExpr' varsNum vars (mkConstr Info.empty (BuiltinTag TagBind) [node, node'])

seqExpr' ::
  (Members '[Error CoreError, InfoTableBuilder] r) =>
  Index ->
  HashMap Text Level ->
  Node ->
  ParsecS r Node
seqExpr' varsNum vars node = do
  ((), i) <- interval (kw kwSeq)
  node' <- cmpExpr (varsNum + 1) vars
  ioExpr' varsNum vars $
    mkConstr
      Info.empty
      (BuiltinTag TagBind)
      [node, mkLambda mempty (Binder "_" (Just i) mkDynamic') node']

cmpExpr ::
  (Members '[Error CoreError, InfoTableBuilder] r) =>
  Index ->
  HashMap Text Level ->
  ParsecS r Node
cmpExpr varsNum vars = arithExpr varsNum vars >>= cmpExpr' varsNum vars

cmpExpr' ::
  (Members '[Error CoreError, InfoTableBuilder] r) =>
  Index ->
  HashMap Text Level ->
  Node ->
  ParsecS r Node
cmpExpr' varsNum vars node =
  eqExpr' varsNum vars node
    <|> ltExpr' varsNum vars node
    <|> leExpr' varsNum vars node
    <|> gtExpr' varsNum vars node
    <|> geExpr' varsNum vars node
    <|> return node

eqExpr' ::
  (Members '[Error CoreError, InfoTableBuilder] r) =>
  Index ->
  HashMap Text Level ->
  Node ->
  ParsecS r Node
eqExpr' varsNum vars node = do
  kw kwEq
  node' <- arithExpr varsNum vars
  return $ mkBuiltinApp' OpEq [node, node']

ltExpr' ::
  (Members '[Error CoreError, InfoTableBuilder] r) =>
  Index ->
  HashMap Text Level ->
  Node ->
  ParsecS r Node
ltExpr' varsNum vars node = do
  kw kwLt
  node' <- arithExpr varsNum vars
  return $ mkBuiltinApp' OpIntLt [node, node']

leExpr' ::
  (Members '[Error CoreError, InfoTableBuilder] r) =>
  Index ->
  HashMap Text Level ->
  Node ->
  ParsecS r Node
leExpr' varsNum vars node = do
  kw kwLe
  node' <- arithExpr varsNum vars
  return $ mkBuiltinApp' OpIntLe [node, node']

gtExpr' ::
  (Members '[Error CoreError, InfoTableBuilder] r) =>
  Index ->
  HashMap Text Level ->
  Node ->
  ParsecS r Node
gtExpr' varsNum vars node = do
  kw kwGt
  node' <- arithExpr varsNum vars
  return $ mkBuiltinApp' OpIntLt [node', node]

geExpr' ::
  (Members '[Error CoreError, InfoTableBuilder] r) =>
  Index ->
  HashMap Text Level ->
  Node ->
  ParsecS r Node
geExpr' varsNum vars node = do
  kw kwGe
  node' <- arithExpr varsNum vars
  return $ mkBuiltinApp' OpIntLe [node', node]

arithExpr ::
  (Members '[Error CoreError, InfoTableBuilder] r) =>
  Index ->
  HashMap Text Level ->
  ParsecS r Node
arithExpr varsNum vars = factorExpr varsNum vars >>= arithExpr' varsNum vars

arithExpr' ::
  (Members '[Error CoreError, InfoTableBuilder] r) =>
  Index ->
  HashMap Text Level ->
  Node ->
  ParsecS r Node
arithExpr' varsNum vars node =
  plusExpr' varsNum vars node
    <|> minusExpr' varsNum vars node
    <|> return node

plusExpr' ::
  (Members '[Error CoreError, InfoTableBuilder] r) =>
  Index ->
  HashMap Text Level ->
  Node ->
  ParsecS r Node
plusExpr' varsNum vars node = do
  kw kwPlus
  node' <- factorExpr varsNum vars
  arithExpr' varsNum vars (mkBuiltinApp' OpIntAdd [node, node'])

minusExpr' ::
  (Members '[Error CoreError, InfoTableBuilder] r) =>
  Index ->
  HashMap Text Level ->
  Node ->
  ParsecS r Node
minusExpr' varsNum vars node = do
  kw kwMinus
  node' <- factorExpr varsNum vars
  arithExpr' varsNum vars (mkBuiltinApp' OpIntSub [node, node'])

factorExpr ::
  (Members '[Error CoreError, InfoTableBuilder] r) =>
  Index ->
  HashMap Text Level ->
  ParsecS r Node
factorExpr varsNum vars = appExpr varsNum vars >>= factorExpr' varsNum vars

factorExpr' ::
  (Members '[Error CoreError, InfoTableBuilder] r) =>
  Index ->
  HashMap Text Level ->
  Node ->
  ParsecS r Node
factorExpr' varsNum vars node =
  mulExpr' varsNum vars node
    <|> divExpr' varsNum vars node
    <|> modExpr' varsNum vars node
    <|> return node

mulExpr' ::
  (Members '[Error CoreError, InfoTableBuilder] r) =>
  Index ->
  HashMap Text Level ->
  Node ->
  ParsecS r Node
mulExpr' varsNum vars node = do
  kw kwMul
  node' <- appExpr varsNum vars
  factorExpr' varsNum vars (mkBuiltinApp' OpIntMul [node, node'])

divExpr' ::
  (Members '[Error CoreError, InfoTableBuilder] r) =>
  Index ->
  HashMap Text Level ->
  Node ->
  ParsecS r Node
divExpr' varsNum vars node = do
  kw kwDiv
  node' <- appExpr varsNum vars
  factorExpr' varsNum vars (mkBuiltinApp' OpIntDiv [node, node'])

modExpr' ::
  (Members '[Error CoreError, InfoTableBuilder] r) =>
  Index ->
  HashMap Text Level ->
  Node ->
  ParsecS r Node
modExpr' varsNum vars node = do
  kw kwMod
  node' <- appExpr varsNum vars
  factorExpr' varsNum vars (mkBuiltinApp' OpIntMod [node, node'])

appExpr ::
  (Members '[Error CoreError, InfoTableBuilder] r) =>
  Index ->
  HashMap Text Level ->
  ParsecS r Node
appExpr varsNum vars = builtinAppExpr varsNum vars <|> atoms varsNum vars

builtinAppExpr ::
  (Members '[Error CoreError, InfoTableBuilder] r) =>
  Index ->
  HashMap Text Level ->
  ParsecS r Node
builtinAppExpr varsNum vars = do
  op <-
    (kw kwEq $> OpEq)
      <|> (kw kwLt $> OpIntLt)
      <|> (kw kwLe $> OpIntLe)
      <|> (kw kwPlus $> OpIntAdd)
      <|> (kw kwMinus $> OpIntSub)
      <|> (kw kwDiv $> OpIntDiv)
      <|> (kw kwMul $> OpIntMul)
      <|> (kw kwMod $> OpIntMod)
      <|> (kw kwFieldAdd $> OpFieldAdd)
      <|> (kw kwFieldSub $> OpFieldSub)
      <|> (kw kwFieldMul $> OpFieldMul)
      <|> (kw kwFieldDiv $> OpFieldDiv)
      <|> (kw kwShow $> OpShow)
      <|> (kw kwStrConcat $> OpStrConcat)
      <|> (kw kwStrToInt $> OpStrToInt)
      <|> (kw kwAssert $> OpAssert)
      <|> (kw kwSeqq $> OpSeq)
      <|> (kw kwTrace $> OpTrace)
      <|> (kw kwFail $> OpFail)
      <|> (kw kwPoseidon $> OpPoseidonHash)
      <|> (kw kwEcOp $> OpEc)
      <|> (kw kwRandomEcPoint $> OpRandomEcPoint)
      <|> (kw kwAnomaEncode $> OpAnomaEncode)
      <|> (kw kwAnomaDecode $> OpAnomaDecode)
      <|> (kw kwAnomaSign $> OpAnomaSign)
      <|> (kw kwAnomaVerifyWithMessage $> OpAnomaVerifyWithMessage)
      <|> (kw kwAnomaSignDetached $> OpAnomaSignDetached)
      <|> (kw kwAnomaVerifyDetached $> OpAnomaVerifyDetached)
      <|> (kw kwByteArrayFromListByte $> OpByteArrayFromListByte)
      <|> (kw kwByteArrayLength $> OpByteArrayLength)
      <|> (kw kwAnomaSha256 $> OpAnomaSha256)
      <|> (kw kwAnomaResourceCommitment $> OpAnomaResourceCommitment)
      <|> (kw kwAnomaResourceNullifier $> OpAnomaResourceNullifier)
      <|> (kw kwAnomaResourceKind $> OpAnomaResourceKind)
      <|> (kw kwAnomaResourceDelta $> OpAnomaResourceDelta)
      <|> (kw kwAnomaActionDelta $> OpAnomaActionDelta)
      <|> (kw kwAnomaActionsDelta $> OpAnomaActionsDelta)
      <|> (kw kwAnomaProveAction $> OpAnomaProveAction)
      <|> (kw kwAnomaProveDelta $> OpAnomaProveDelta)
      <|> (kw kwAnomaZeroDelta $> OpAnomaZeroDelta)
      <|> (kw kwAnomaAddDelta $> OpAnomaAddDelta)
      <|> (kw kwAnomaSubDelta $> OpAnomaSubDelta)
      <|> (kw kwAnomaRandomGeneratorInit $> OpAnomaRandomGeneratorInit)
      <|> (kw kwAnomaRandomNextBytes $> OpAnomaRandomNextBytes)
      <|> (kw kwAnomaRandomSplit $> OpAnomaRandomSplit)
      <|> (kw kwAnomaIsCommitment $> OpAnomaIsCommitment)
      <|> (kw kwAnomaIsNullifier $> OpAnomaIsNullifier)
      <|> (kw kwAnomaSetToList $> OpAnomaSetToList)
      <|> (kw kwAnomaSetFromList $> OpAnomaSetFromList)

  args <- P.many (atom varsNum vars)
  return $ mkBuiltinApp' op args

atoms ::
  (Members '[Error CoreError, InfoTableBuilder] r) =>
  Index ->
  HashMap Text Level ->
  ParsecS r Node
atoms varsNum vars = do
  es <- NonEmpty.some (atom varsNum vars)
  return $ mkApps' (head es) (NonEmpty.tail es)

atom ::
  (Members '[Error CoreError, InfoTableBuilder] r) =>
  Index ->
  HashMap Text Level ->
  ParsecS r Node
atom varsNum vars =
  exprConstField
    <|> exprConstUInt8
    <|> exprConstInt
    <|> exprConstString
    <|> exprUniverse
    <|> exprDynamic
    <|> exprBottom
    <|> exprPi varsNum vars
    <|> exprLambda varsNum vars
    <|> exprLetrecMany varsNum vars
    <|> exprLetrecOne varsNum vars
    <|> exprLet varsNum vars
    <|> exprCase varsNum vars
    <|> exprMatch varsNum vars
    <|> exprIf varsNum vars
    <|> parens (expr varsNum vars)
    <|> exprNamed varsNum vars

exprConstInt :: ParsecS r Node
exprConstInt = P.try $ do
  WithLoc i n <- integer
  return $ mkConstant (Info.singleton (LocationInfo i)) (ConstInteger n)

exprConstString :: ParsecS r Node
exprConstString = P.try $ do
  (s, i) <- string
  return $ mkConstant (Info.singleton (LocationInfo i)) (ConstString s)

exprConstField :: ParsecS r Node
exprConstField = P.try $ do
  (n, i) <- field
  return $ mkConstant (Info.singleton (LocationInfo i)) (ConstField (fieldFromInteger defaultFieldSize n))

exprConstUInt8 :: ParsecS r Node
exprConstUInt8 = P.try $ do
  (n, i) <- uint8
  return $ mkConstant (Info.singleton (LocationInfo i)) (ConstUInt8 (fromIntegral n))

exprUniverse :: ParsecS r Type
exprUniverse = do
  kw kwType
  level <- optional (number 0 128) -- TODO: global Limits.hs file
  return $ mkUniv' (maybe 0 (^. withLocParam) level)

exprDynamic :: ParsecS r Type
exprDynamic = kw kwAny $> mkDynamic'

exprBottom :: (Members '[Error CoreError, InfoTableBuilder] r) => ParsecS r Node
exprBottom = do
  (ty, loc) <- interval $ do
    kw kwBottom
    fromMaybe mkDynamic' <$> optional (kw kwColon >> expression)
  return (mkAxiom loc ty)

parseLocalName :: ParsecS r (Text, Location)
parseLocalName = parseWildcardName <|> parseIdentName
  where
    parseWildcardName :: ParsecS r (Text, Location)
    parseWildcardName = do
      ((), i) <- interval (kw kwWildcard)
      return ("_", i)

    parseIdentName :: ParsecS r (Text, Location)
    parseIdentName = identifierL

parseLocalBinder ::
  forall r.
  (Members '[Error CoreError, InfoTableBuilder] r) =>
  Index ->
  HashMap Text Level ->
  ParsecS r ((Text, Location), Type)
parseLocalBinder varsNum vars = parseBinder <|> parseName
  where
    parseBinder :: ParsecS r ((Text, Location), Type)
    parseBinder = do
      lparen
      n <- parseLocalName
      kw kwColon
      ty <- expr varsNum vars
      rparen
      return (n, ty)

    parseName :: ParsecS r ((Text, Location), Type)
    parseName = do
      n <- parseLocalName
      return (n, mkDynamic')

exprPi ::
  (Members '[Error CoreError, InfoTableBuilder] r) =>
  Index ->
  HashMap Text Level ->
  ParsecS r Node
exprPi varsNum vars = do
  kw kwPi
  (name, loc) <- parseLocalName
  ty <- typeAnnot varsNum vars
  kw kwComma
  let vars' = HashMap.insert name varsNum vars
      bi = Binder name (Just loc) ty
  body <- expr (varsNum + 1) vars'
  return $ mkPi mempty bi body

exprLambda ::
  (Members '[Error CoreError, InfoTableBuilder] r) =>
  Index ->
  HashMap Text Level ->
  ParsecS r Node
exprLambda varsNum vars = do
  lambda
  ((name, loc), mty) <- lambdaName
  let vars' = HashMap.insert name varsNum vars
      bi = Binder name (Just loc) (fromMaybe mkDynamic' mty)
  body <- bracedExpr (varsNum + 1) vars'
  return $ mkLambda mempty bi body
  where
    lambdaName =
      parens
        ( do
            n <- parseLocalName
            ty <- typeAnnot varsNum vars
            return (n, Just ty)
        )
        <|> (\n -> (n, Nothing)) <$> parseLocalName

exprLetrecOne ::
  (Members '[Error CoreError, InfoTableBuilder] r) =>
  Index ->
  HashMap Text Level ->
  ParsecS r Node
exprLetrecOne varsNum vars = do
  kw kwLetRec
  (name, loc) <- parseLocalName
  mty <- optional (kw kwColon >> expr varsNum vars)
  kw kwAssign
  let vars' = HashMap.insert name varsNum vars
  value <- bracedExpr (varsNum + 1) vars'
  kw kwIn
  body <- bracedExpr (varsNum + 1) vars'
  let item :: LetItem
      item = LetItem (Binder name (Just loc) (fromMaybe mkDynamic' mty)) value
  return $ mkLetRec mempty (pure item) body

exprLetrecMany ::
  (Members '[Error CoreError, InfoTableBuilder] r) =>
  Index ->
  HashMap Text Level ->
  ParsecS r Node
exprLetrecMany varsNum vars = do
  (defNames, i) <- P.try (kw kwLetRec >> interval letrecNames)
  when (null defNames) $
    throwCoreError i "expected at least one identifier name in letrec signature"
  let (vars', varsNum') = foldl' (\(vs, k) txt -> (HashMap.insert txt k vs, k + 1)) (vars, varsNum) defNames
  defs <- letrecDefs defNames varsNum vars varsNum' vars'
  kw kwIn
  body <- bracedExpr varsNum' vars'
  return $ mkLetRec mempty defs body

letrecNames :: ParsecS r (NonEmpty Text)
letrecNames = P.between (symbol "[") (symbol "]") (NonEmpty.some identifier)

letrecDefs ::
  forall r.
  (Members '[Error CoreError, InfoTableBuilder] r) =>
  NonEmpty Text ->
  Index ->
  HashMap Text Level ->
  Index ->
  HashMap Text Level ->
  ParsecS r (NonEmpty LetItem)
letrecDefs names varsNum0 vars0 varsNum vars = forM names letrecItem
  where
    letrecItem :: Text -> ParsecS r LetItem
    letrecItem n = do
      (txt, i) <- identifierL
      mty <- optional (typeAnnot varsNum0 vars0)
      when (n /= txt) $
        throwCoreError i "identifier name doesn't match letrec signature"
      kw kwAssign
      v <- bracedExpr varsNum vars
      kw delimSemicolon
      let ty = fromMaybe mkDynamic' mty
      return $ LetItem (Binder txt (Just i) ty) v

exprLet ::
  (Members '[Error CoreError, InfoTableBuilder] r) =>
  Index ->
  HashMap Text Level ->
  ParsecS r Node
exprLet varsNum vars = do
  kw kwLet
  (name, loc) <- parseLocalName
  mty <- optional (typeAnnot varsNum vars)
  kw kwAssign
  value <- bracedExpr varsNum vars
  kw kwIn
  let vars' = HashMap.insert name varsNum vars
      binder = Binder name (Just loc) (fromMaybe mkDynamic' mty)
  body <- bracedExpr (varsNum + 1) vars'
  return $ mkLet mempty binder value body

exprCase ::
  (Members '[Error CoreError, InfoTableBuilder] r) =>
  Index ->
  HashMap Text Level ->
  ParsecS r Node
exprCase varsNum vars = do
  ((), i) <- interval $ kw kwCase
  value <- bracedExpr varsNum vars
  kw kwOf
  braces (exprCase' i value varsNum vars)
    <|> exprCase' i value varsNum vars

exprCase' ::
  (Members '[Error CoreError, InfoTableBuilder] r) =>
  Interval ->
  Node ->
  Index ->
  HashMap Text Level ->
  ParsecS r Node
exprCase' i value varsNum vars = do
  bs <- P.sepEndBy (caseBranchP varsNum vars) (kw delimSemicolon)
  let bss = map fromLeft' $ filter isLeft bs
  let def' = map fromRight' $ filter isRight bs
  case bss of
    CaseBranch {..} : _ -> do
      ci <- lift $ getConstructorInfo _caseBranchTag
      let sym = ci ^. constructorInductive
      case def' of
        [def] ->
          return $ mkCase' sym value bss (Just def)
        [] ->
          return $ mkCase' sym value bss Nothing
        _ ->
          throwCoreError i "multiple default branches"
    [] ->
      case def' of
        [_] ->
          throwCoreError i "case with only the default branch not allowed"
        [] ->
          throwCoreError i "case without branches not allowed"
        _ ->
          throwCoreError i "multiple default branches"

caseBranchP ::
  (Members '[Error CoreError, InfoTableBuilder] r) =>
  Index ->
  HashMap Text Level ->
  ParsecS r (Either CaseBranch Node)
caseBranchP varsNum vars =
  (caseDefaultBranch varsNum vars <&> Right)
    <|> (caseMatchingBranch varsNum vars <&> Left)

caseDefaultBranch ::
  (Members '[Error CoreError, InfoTableBuilder] r) =>
  Index ->
  HashMap Text Level ->
  ParsecS r Node
caseDefaultBranch varsNum vars = do
  kw kwWildcard
  kw kwAssign
  bracedExpr varsNum vars

parseCaseBranchBinders ::
  (Members '[Error CoreError, InfoTableBuilder] r) =>
  Index ->
  HashMap Text Level ->
  ParsecS r [((Text, Location), Type)]
parseCaseBranchBinders varsNum vars = do
  mb <- optional (parseLocalBinder varsNum vars)
  case mb of
    Just b@((name, _), _) ->
      (b :)
        <$> parseCaseBranchBinders (varsNum + 1) (HashMap.insert name varsNum vars)
    Nothing ->
      return []

caseMatchingBranch ::
  (Members '[Error CoreError, InfoTableBuilder] r) =>
  Index ->
  HashMap Text Level ->
  ParsecS r CaseBranch
caseMatchingBranch varsNum vars = do
  (txt, i) <- identifierL
  r <- lift (getIdent txt)
  case r of
    Just IdentFun {} ->
      throwCoreError i ("not a constructor: " <> fromText txt)
    Just IdentInd {} ->
      throwCoreError i ("not a constructor: " <> fromText txt)
    Just (IdentConstr tag) -> do
      bs :: [((Text, Location), Type)] <- parseCaseBranchBinders varsNum vars
      let bindersNum = length bs
      ci <- lift $ getConstructorInfo tag
      when
        (ci ^. constructorArgsNum /= bindersNum)
        (throwCoreError i "wrong number of constructor arguments")
      kw kwAssign
      let vars' =
            fst $
              foldl'
                ( \(vs, k) ((name, _), _) ->
                    (HashMap.insert name k vs, k + 1)
                )
                (vars, varsNum)
                bs
      br <- bracedExpr (varsNum + bindersNum) vars'
      let info = setInfoName (ci ^. constructorName) mempty
          binders =
            zipWith
              ( \((name, loc), ty) ty' ->
                  Binder name (Just loc) (if isDynamic ty then ty' else ty)
              )
              bs
              (typeArgs (ci ^. constructorType) ++ repeat mkDynamic')
      return $ CaseBranch info tag binders bindersNum br
    Nothing ->
      throwCoreError i ("undeclared identifier: " <> fromText txt)

exprIf ::
  (Members '[Error CoreError, InfoTableBuilder] r) =>
  Index ->
  HashMap Text Level ->
  ParsecS r Node
exprIf varsNum vars = do
  kw kwIf
  value <- bracedExpr varsNum vars
  kw kwThen
  br1 <- bracedExpr varsNum vars
  kw kwElse
  br2 <- bracedExpr varsNum vars
  sym <- lift getBoolSymbol
  return $ mkIf mempty sym value br1 br2

exprMatch ::
  (Members '[Error CoreError, InfoTableBuilder] r) =>
  Index ->
  HashMap Text Level ->
  ParsecS r Node
exprMatch varsNum vars = do
  kw kwMatch
  vals <- P.sepBy (exprMatchValue varsNum vars) (kw kwComma)
  kw kwWith
  mty <- optional (typeAnnot varsNum vars)
  let rty = fromMaybe mkDynamic' mty
  braces (exprMatch' vals rty varsNum vars)
    <|> exprMatch' vals rty varsNum vars

exprMatchValue ::
  (Members '[Error CoreError, InfoTableBuilder] r) =>
  Index ->
  HashMap Text Level ->
  ParsecS r (Node, Type)
exprMatchValue varsNum vars = parens (exprMatchValue' varsNum vars) <|> exprMatchValue' varsNum vars

exprMatchValue' ::
  (Members '[Error CoreError, InfoTableBuilder] r) =>
  Index ->
  HashMap Text Level ->
  ParsecS r (Node, Type)
exprMatchValue' varsNum vars = do
  val <- expr varsNum vars
  mty <- optional (typeAnnot varsNum vars)
  return (val, fromMaybe mkDynamic' mty)

exprMatch' ::
  (Members '[Error CoreError, InfoTableBuilder] r) =>
  [(Node, Type)] ->
  Type ->
  Index ->
  HashMap Text Level ->
  ParsecS r Node
exprMatch' vals rty varsNum vars = do
  let values = map fst vals
      types = map snd vals
  bs <- P.sepEndBy (matchBranch (length values) varsNum vars) (kw delimSemicolon)
  return $ mkMatch' (fromList types) rty (fromList values) bs

matchBranch ::
  (Members '[Error CoreError, InfoTableBuilder] r) =>
  Int ->
  Index ->
  HashMap Text Level ->
  ParsecS r MatchBranch
matchBranch patsNum varsNum vars = do
  (pats, i) <- interval $ branchPatterns varsNum vars
  rhs <- branchRhs i pats patsNum varsNum vars
  return $ MatchBranch Info.empty (fromList pats) rhs

branchRhs ::
  (Members '[Error CoreError, InfoTableBuilder] r) =>
  Interval ->
  [Pattern] ->
  Int ->
  Index ->
  HashMap Text Level ->
  ParsecS r MatchBranchRhs
branchRhs i pats patsNum varsNum vars =
  branchRhsExpr i pats patsNum varsNum vars
    <|> branchRhsIf i pats patsNum varsNum vars

updateVarsByPatternBinders ::
  [Pattern] ->
  Index ->
  HashMap Text Level ->
  (Index, HashMap Text Level)
updateVarsByPatternBinders pats varsNum vars =
  let pis :: [Binder]
      pis = concatMap getPatternBinders pats
      (vars', varsNum') =
        foldl'
          ( \(vs, k) name ->
              (HashMap.insert name k vs, k + 1)
          )
          (vars, varsNum)
          (map (^. binderName) pis)
   in (varsNum', vars')

branchRhsExpr ::
  (Members '[Error CoreError, InfoTableBuilder] r) =>
  Interval ->
  [Pattern] ->
  Int ->
  Index ->
  HashMap Text Level ->
  ParsecS r MatchBranchRhs
branchRhsExpr i pats patsNum varsNum vars = do
  kw kwAssign
  unless (length pats == patsNum) $
    throwCoreError i "wrong number of patterns"
  let (varsNum', vars') = updateVarsByPatternBinders pats varsNum vars
  br <- bracedExpr varsNum' vars'
  return $ MatchBranchRhsExpression br

branchRhsIf ::
  (Members '[Error CoreError, InfoTableBuilder] r) =>
  Interval ->
  [Pattern] ->
  Int ->
  Index ->
  HashMap Text Level ->
  ParsecS r MatchBranchRhs
branchRhsIf i pats patsNum varsNum vars = do
  ifs <- sideIfs i pats patsNum varsNum vars
  return $ MatchBranchRhsIfs ifs

sideIfs ::
  (Members '[Error CoreError, InfoTableBuilder] r) =>
  Interval ->
  [Pattern] ->
  Int ->
  Index ->
  HashMap Text Level ->
  ParsecS r (NonEmpty SideIfBranch)
sideIfs i pats patsNum varsNum vars = do
  let (varsNum', vars') = updateVarsByPatternBinders pats varsNum vars
  cond <- branchCond varsNum' vars'
  kw kwAssign
  unless (length pats == patsNum) $
    throwCoreError i "wrong number of patterns"
  br <- bracedExpr varsNum' vars'
  conds <- optional (sideIfs i pats patsNum varsNum vars)
  return $ SideIfBranch Info.empty cond br :| maybe [] toList conds

branchCond ::
  (Members '[Error CoreError, InfoTableBuilder] r) =>
  Index ->
  HashMap Text Level ->
  ParsecS r Node
branchCond varsNum vars = do
  kw kwIf
  expr varsNum vars

branchPatterns ::
  (Members '[Error CoreError, InfoTableBuilder] r) =>
  Index ->
  HashMap Text Level ->
  ParsecS r [Pattern]
branchPatterns varsNum vars = do
  (pat, (varsNum', vars')) <- branchPattern varsNum vars
  pats <- (kw kwComma >> branchPatterns varsNum' vars') <|> return []
  return (pat : pats)

branchPattern ::
  (Members '[Error CoreError, InfoTableBuilder] r) =>
  Index ->
  HashMap Text Level ->
  ParsecS r (Pattern, (Index, HashMap Text Level))
branchPattern varsNum vars =
  parens (branchPattern varsNum vars)
    <|> branchPatternWildcard varsNum vars
    <|> branchPattern' varsNum vars

branchPatternWildcard ::
  (Members '[Error CoreError, InfoTableBuilder] r) =>
  Index ->
  HashMap Text Level ->
  ParsecS r (Pattern, (Index, HashMap Text Level))
branchPatternWildcard varsNum vars = do
  kw kwWildcard
  mty <- optional (kw kwColon >> expr varsNum vars)
  let binder = Binder "_" Nothing (fromMaybe mkDynamic' mty)
  return (PatWildcard (PatternWildcard mempty binder), (varsNum + 1, vars))

branchPattern' ::
  (Members '[Error CoreError, InfoTableBuilder] r) =>
  Index ->
  HashMap Text Level ->
  ParsecS r (Pattern, (Index, HashMap Text Level))
branchPattern' varsNum vars = do
  (txt, i) <- identifierL
  r <- lift (getIdent txt)
  case r of
    Just (IdentConstr tag) -> do
      (ps, (varsNum', vars')) <- constrArgPatterns (varsNum + 1) vars
      mty <- optional (typeAnnot varsNum vars)
      ci <- lift $ getConstructorInfo tag
      when
        (ci ^. constructorArgsNum /= length ps)
        (throwCoreError i "wrong number of constructor arguments")
      let info = setInfoName (ci ^. constructorName) Info.empty
          ty = fromMaybe mkDynamic' mty
          binder = Binder "_" (Just i) ty
          pat =
            PatConstr
              PatternConstr
                { _patternConstrInfo = info,
                  _patternConstrBinder = binder,
                  _patternConstrTag = tag,
                  _patternConstrArgs = ps,
                  _patternConstrFixity = ci ^. constructorFixity
                }
      return (pat, (varsNum', vars'))
    _ -> do
      let vars1 = HashMap.insert txt varsNum vars
      mp <- optional (symbolAt >> parens (branchPattern (varsNum + 1) vars1))
      mty <- optional (typeAnnot varsNum vars)
      let ty = fromMaybe mkDynamic' mty
      case mp of
        Just (pat, r') ->
          case pat of
            PatWildcard p ->
              return (PatWildcard $ over patternWildcardBinder adjustBinder p, r')
            PatConstr p ->
              return (PatConstr $ over patternConstrBinder adjustBinder p, r')
          where
            adjustBinder :: Binder -> Binder
            adjustBinder b =
              set
                binderName
                txt
                ( set
                    binderLocation
                    (Just i)
                    (over binderType (if isDynamic ty then id else const ty) b)
                )
        Nothing -> do
          let binder = Binder txt (Just i) ty
          return (PatWildcard (PatternWildcard mempty binder), (varsNum + 1, vars1))

constrArgPatterns ::
  (Members '[Error CoreError, InfoTableBuilder] r) =>
  Index ->
  HashMap Text Level ->
  ParsecS r ([Pattern], (Index, HashMap Text Level))
constrArgPatterns varsNum vars = do
  mr <- optional (branchPattern varsNum vars)
  case mr of
    Just (pat, (varsNum', vars')) -> do
      (pats, (varsNum'', vars'')) <- constrArgPatterns varsNum' vars'
      return (pat : pats, (varsNum'', vars''))
    Nothing ->
      return ([], (varsNum, vars))

exprNamed ::
  (Members '[Error CoreError, InfoTableBuilder] r) =>
  Index ->
  HashMap Text Level ->
  ParsecS r Node
exprNamed varsNum vars = do
  (txt, i) <- identifierL
  case txt of
    "Int" -> return mkTypeInteger'
    "Field" -> return mkTypeField'
    "String" -> return mkTypeString'
    "UInt8" -> return mkTypeUInt8'
    "ByteArray" -> return mkDynamic'
    _ ->
      case HashMap.lookup txt vars of
        Just k -> do
          return $ mkVar (Info.insert (LocationInfo i) (Info.singleton (NameInfo txt))) (varsNum - k - 1)
        Nothing -> do
          r <- lift (getIdent txt)
          case r of
            Just (IdentFun sym) -> do
              return $ mkIdent (Info.insert (LocationInfo i) (Info.singleton (NameInfo txt))) sym
            Just (IdentInd sym) -> do
              return $ mkTypeConstr (Info.insert (LocationInfo i) (Info.singleton (NameInfo txt))) sym []
            Just (IdentConstr tag) -> do
              return $ mkConstr (Info.insert (LocationInfo i) (Info.singleton (NameInfo txt))) tag []
            Nothing ->
              lift $
                throw
                  CoreError
                    { _coreErrorMsg = ppOutput $ "undeclared identifier: " <> fromText txt,
                      _coreErrorNode = Nothing,
                      _coreErrorLoc = i
                    }
