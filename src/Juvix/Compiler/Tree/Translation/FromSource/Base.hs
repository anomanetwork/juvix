{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid restricted extensions" #-}
{-# HLINT ignore "Avoid restricted flags" #-}

module Juvix.Compiler.Tree.Translation.FromSource.Base
  ( module Juvix.Compiler.Tree.Translation.FromSource.Base,
    LocalNameMap,
    LocalParams' (..),
    localParamsNameMap,
    localParamsTempIndex,
  )
where

import Control.Monad.Trans.Class (lift)
import Data.HashMap.Strict qualified as HashMap
import Data.List.NonEmpty qualified as NonEmpty
import Juvix.Compiler.Tree.Data.InfoTableBuilder.Base
import Juvix.Compiler.Tree.Data.Module.Base
import Juvix.Compiler.Tree.Extra.Type
import Juvix.Compiler.Tree.Keywords.Base
import Juvix.Compiler.Tree.Language.Base
import Juvix.Compiler.Tree.Translation.FromSource.Lexer.Base
import Juvix.Compiler.Tree.Translation.FromSource.Sig
import Juvix.Data.Field
import Juvix.Parser.Error
import Text.Megaparsec qualified as P

-- | Because the effects are inside the parser monad, it is difficult to use
-- `local` with the `Reader` effect. Hence, we use the `State` effect instead
-- with this `localS` function.
localS :: (Member (State s) r) => (s -> s) -> ParsecS r a -> ParsecS r a
localS update a = do
  s <- lift get
  lift $ put (update s)
  a' <- a
  lift $ put s
  return a'

runParserS :: ParserSig t e d -> Path Abs File -> Text -> Either MegaparsecError (Module'' t e)
runParserS sig fileName input_ = (^. stateModule) <$> runParserS' sig (mkBuilderState (emptyModule mid)) fileName input_
  where
    mid =
      ModuleId
        { _moduleIdPath = nonEmptyToTopModulePathKey (pure (toFilePath fileName)),
          _moduleIdPackageId =
            PackageId
              { _packageIdName = "$",
                _packageIdVersion = SemVer 1 0 0 Nothing Nothing
              }
        }

runParserS' :: forall t e d. ParserSig t e d -> BuilderState' t e -> Path Abs File -> Text -> Either MegaparsecError (BuilderState' t e)
runParserS' sig bs fileName input_ = case runParserS'' (parseToplevel @t @e @d) sig bs fileName input_ of
  Left e -> Left e
  Right (bs', ()) -> Right bs'

runParserS'' ::
  forall t e d a.
  ParsecS '[Reader (ParserSig t e d), InfoTableBuilder' t e, State (LocalParams' d)] a ->
  ParserSig t e d ->
  BuilderState' t e ->
  Path Abs File ->
  Text ->
  Either MegaparsecError (BuilderState' t e, a)
runParserS'' parser sig bs fileName input_ =
  case run
    . evalState emptyLocalParams
    . runInfoTableBuilder' bs
    . runReader sig
    $ P.runParserT parser (toFilePath fileName) input_ of
    (_, Left err) -> Left (MegaparsecError err)
    (bs', Right x) -> Right (bs', x)

createBuiltinConstr ::
  Symbol ->
  BuiltinDataTag ->
  Text ->
  Type ->
  Location ->
  ConstructorInfo
createBuiltinConstr sym btag name ty i =
  let n = builtinConstrArgsNum btag
   in ConstructorInfo
        { _constructorName = name,
          _constructorLocation = Just i,
          _constructorTag = BuiltinTag btag,
          _constructorType = ty,
          _constructorArgsNum = n,
          _constructorArgNames = replicate n Nothing,
          _constructorInductive = sym,
          _constructorRepresentation = MemRepConstr,
          _constructorFixity = Nothing
        }

declareBuiltins :: forall t e r. (Members '[InfoTableBuilder' t e] r) => ParsecS r ()
declareBuiltins = do
  loc <- curLoc
  let i = mkInterval loc loc
  sym <- lift $ freshSymbol' @t @e
  let tyio = mkTypeInductive sym
      constrs =
        [ createBuiltinConstr sym TagReturn (show TagReturn) (mkTypeFun [TyDynamic] tyio) i,
          createBuiltinConstr sym TagBind (show TagBind) (mkTypeFun [tyio, mkTypeFun [TyDynamic] tyio] tyio) i,
          createBuiltinConstr sym TagWrite (show TagWrite) (mkTypeFun [TyDynamic] tyio) i,
          createBuiltinConstr sym TagReadLn (show TagReadLn) tyio i
        ]
  lift $
    registerInductive' @t @e
      ( InductiveInfo
          { _inductiveName = "IO",
            _inductiveSymbol = sym,
            _inductiveLocation = Just i,
            _inductiveKind = TyDynamic,
            _inductiveConstructors = map (^. constructorTag) constrs,
            _inductiveRepresentation = IndRepStandard
          }
      )
  lift $ mapM_ (registerConstr' @t @e) constrs

parseToplevel ::
  forall t e d.
  ParsecS '[Reader (ParserSig t e d), InfoTableBuilder' t e, State (LocalParams' d)] ()
parseToplevel = do
  declareBuiltins @t @e
  space
  P.many $ statement @t @e @d
  P.eof

statement ::
  forall t e d r.
  (Members '[Reader (ParserSig t e d), InfoTableBuilder' t e, State (LocalParams' d)] r) =>
  ParsecS r ()
statement = statementFunction @t @e @d <|> statementInductive @t @e @d

statementFunction ::
  forall t e d r.
  (Members '[Reader (ParserSig t e d), InfoTableBuilder' t e, State (LocalParams' d)] r) =>
  ParsecS r ()
statementFunction = do
  kw kwFun
  off <- P.getOffset
  (txt, i) <- identifierL @t @e @d
  idt <- lift $ getIdent' @t @e txt
  sym <- case idt of
    Nothing -> lift $ freshSymbol' @t @e
    Just (IdentFwd sym) -> return sym
    _ -> parseFailure off ("duplicate identifier: " ++ fromText txt)
  when (txt == "main") $
    lift (registerMain' @t @e sym)
  args <- functionArguments @t @e @d
  let argtys = map snd args
      argnames = map fst args
  mrty <- optional $ typeAnnotation @t @e @d
  ec <- lift $ emptyCode @t @e @d
  ee <- lift $ emptyExtra @t @e @d
  let fi0 =
        FunctionInfo
          { _functionName = txt,
            _functionSymbol = sym,
            _functionLocation = Just i,
            _functionCode = ec,
            _functionArgsNum = length argtys,
            _functionArgNames = argnames,
            _functionType = mkTypeFun argtys (fromMaybe TyDynamic mrty),
            _functionExtra = ee
          }
  lift $ registerFunction' @t @e fi0
  mcode <-
    (kw delimSemicolon $> Nothing)
      <|> optional (braces (functionBody @t @e @d (parseCode @t @e @d) argnames))
  let fi = fi0 {_functionCode = fromMaybe ec mcode}
  case idt of
    Just (IdentFwd _) -> do
      when (isNothing mcode) $
        parseFailure off ("duplicate forward declaration of " ++ fromText txt)
      fi' <- lift $ getFunctionInfo' @t @e sym
      unless
        ( fi' ^. functionArgsNum == fi ^. functionArgsNum
            && isSubtype (fi' ^. functionType) (fi ^. functionType)
        )
        $ parseFailure off "function definition does not match earlier declaration"
      lift $ registerFunction' fi
    _ -> do
      lift $ registerFunction' fi
      when (isNothing mcode) $
        lift (registerForward' @t @e txt sym)

statementInductive ::
  forall t e d r.
  (Members '[Reader (ParserSig t e d), InfoTableBuilder' t e] r) =>
  ParsecS r ()
statementInductive = do
  kw kwInductive
  off <- P.getOffset
  (txt, i) <- identifierL @t @e @d
  idt <- lift $ getIdent' @t @e txt
  when (isJust idt) $
    parseFailure off ("duplicate identifier: " ++ fromText txt)
  sym <- lift $ freshSymbol' @t @e
  let ii =
        InductiveInfo
          { _inductiveName = txt,
            _inductiveLocation = Just i,
            _inductiveSymbol = sym,
            _inductiveKind = TyDynamic,
            _inductiveConstructors = [],
            _inductiveRepresentation = IndRepStandard
          }
  lift $ registerInductive' @t @e ii
  ctrs <- braces $ P.sepEndBy (constrDecl @t @e @d sym) (kw delimSemicolon)
  lift $ registerInductive' @t @e ii {_inductiveConstructors = map (^. constructorTag) ctrs}

functionArguments ::
  forall t e d r.
  (Members '[Reader (ParserSig t e d), InfoTableBuilder' t e] r) =>
  ParsecS r [(Maybe Text, Type)]
functionArguments = do
  lparen
  args <- P.sepBy (parseArgument @t @e @d) comma
  rparen
  return args

constrDecl ::
  forall t e d r.
  (Members '[Reader (ParserSig t e d), InfoTableBuilder' t e] r) =>
  Symbol ->
  ParsecS r ConstructorInfo
constrDecl symInd = do
  off <- P.getOffset
  (txt, i) <- identifierL @t @e @d
  idt <- lift $ getIdent' @t @e txt
  when (isJust idt) $
    parseFailure off ("duplicate identifier: " ++ fromText txt)
  tag <- lift $ freshTag' @t @e
  ty <- typeAnnotation @t @e @d
  let ty' = uncurryType ty
      argsNum = length (typeArgs ty')
      ci =
        ConstructorInfo
          { _constructorName = txt,
            _constructorLocation = Just i,
            _constructorTag = tag,
            _constructorArgsNum = argsNum,
            _constructorArgNames = replicate argsNum Nothing,
            _constructorType = ty',
            _constructorInductive = symInd,
            _constructorRepresentation = MemRepConstr,
            _constructorFixity = Nothing
          }
  lift $ registerConstr' @t @e ci
  return ci

typeAnnotation ::
  forall t e d r.
  (Members '[Reader (ParserSig t e d), InfoTableBuilder' t e] r) =>
  ParsecS r Type
typeAnnotation = do
  kw kwColon
  parseType @t @e @d

parseArgument ::
  forall t e d r.
  (Members '[Reader (ParserSig t e d), InfoTableBuilder' t e] r) =>
  ParsecS r (Maybe Text, Type)
parseArgument = do
  n <- optional $ P.try $ do
    txt <- identifier @t @e @d
    kw kwColon
    return txt
  ty <- parseType @t @e @d
  return (n, ty)

parseType ::
  forall t e d r.
  (Members '[Reader (ParserSig t e d), InfoTableBuilder' t e] r) =>
  ParsecS r Type
parseType = do
  tys <- typeArguments @t @e @d
  off <- P.getOffset
  typeFun' @t @e @d tys
    <|> do
      unless (null (NonEmpty.tail tys)) $
        parseFailure off "expected \"->\""
      return (head tys)

typeFun' ::
  forall t e d r.
  (Members '[Reader (ParserSig t e d), InfoTableBuilder' t e] r) =>
  NonEmpty Type ->
  ParsecS r Type
typeFun' tyargs = do
  kw kwRightArrow
  TyFun . TypeFun tyargs <$> parseType @t @e @d

typeArguments ::
  forall t e d r.
  (Members '[Reader (ParserSig t e d), InfoTableBuilder' t e] r) =>
  ParsecS r (NonEmpty Type)
typeArguments = do
  parens (P.sepBy1 (parseType @t @e @d) comma <&> NonEmpty.fromList)
    <|> (typeDynamic <&> NonEmpty.singleton)
    <|> (typeNamed @t @e @d <&> NonEmpty.singleton)

typeDynamic :: ParsecS r Type
typeDynamic = kw kwStar $> TyDynamic

typeNamed ::
  forall t e d r.
  (Members '[Reader (ParserSig t e d), InfoTableBuilder' t e] r) =>
  ParsecS r Type
typeNamed = do
  off <- P.getOffset
  txt <- identifier @t @e @d
  case txt of
    "integer" -> return mkTypeInteger
    "field" -> return TyField
    "bool" -> return mkTypeBool
    "string" -> return TyString
    "unit" -> return TyUnit
    "uint8" -> return mkTypeUInt8
    "bytearray" -> return TyByteArray
    _ -> do
      idt <- lift $ getIdent' @t @e txt
      case idt of
        Just (IdentInd sym) -> return (mkTypeInductive sym)
        _ -> parseFailure off ("not a type: " ++ fromText txt)

constant :: ParsecS r Constant
constant = fieldValue <|> uint8Value <|> integerValue <|> boolValue <|> stringValue <|> unitValue <|> voidValue

fieldValue :: ParsecS r Constant
fieldValue = P.try $ do
  (i, _) <- field
  return $ ConstField (fieldFromInteger defaultFieldSize i)

uint8Value :: ParsecS r Constant
uint8Value = P.try $ do
  (i, _) <- uint8
  return $ ConstUInt8 (fromInteger i)

integerValue :: ParsecS r Constant
integerValue = do
  i <- (^. withLocParam) <$> integer
  return $ ConstInt i

boolValue :: ParsecS r Constant
boolValue =
  (kw kwTrue $> ConstBool True)
    <|> (kw kwFalse $> ConstBool False)

stringValue :: ParsecS r Constant
stringValue = do
  (s, _) <- string
  return $ ConstString s

unitValue :: ParsecS r Constant
unitValue = kw kwUnit $> ConstUnit

voidValue :: ParsecS r Constant
voidValue = kw kwVoid $> ConstVoid

functionBody ::
  forall t e d r.
  (Members '[Reader (ParserSig t e d), InfoTableBuilder' t e, State (LocalParams' d)] r) =>
  ParsecS r t ->
  [Maybe Text] ->
  ParsecS r t
functionBody parseCode' argnames = do
  sig <- lift $ ask @(ParserSig t e d)
  let updateNames :: LocalNameMap d -> LocalNameMap d
      updateNames names =
        foldr
          (\(mname, idx) names' -> maybe names' (updateWithArgRef names' idx) mname)
          names
          (zip argnames [0 ..])
      updateWithArgRef :: LocalNameMap d -> Int -> Text -> LocalNameMap d
      updateWithArgRef names idx name = HashMap.insert name ((sig ^. parserSigArgRef) idx (Just name)) names
  localS (over localParamsNameMap updateNames) parseCode'

memRef ::
  forall t e r.
  (Members '[Reader (ParserSig t e DirectRef), InfoTableBuilder' t e, State (LocalParams' DirectRef)] r) =>
  ParsecS r MemRef
memRef = do
  r <- directRef @t @e
  parseField @t @e @DirectRef r <|> return (DRef r)

directRef :: forall t e r. (Members '[Reader (ParserSig t e DirectRef), State (LocalParams' DirectRef)] r) => ParsecS r DirectRef
directRef = argRef <|> tempRef <|> namedRef @t @e

argRef :: ParsecS r DirectRef
argRef = do
  kw kwArg
  off <- (^. withLocParam) <$> brackets integer
  return $ ArgRef (OffsetRef (fromInteger off) Nothing)

tempRef :: ParsecS r DirectRef
tempRef = do
  kw kwTmp
  off <- (^. withLocParam) <$> brackets integer
  return $ mkTempRef (OffsetRef (fromInteger off) Nothing)

namedRef' :: forall t e d r. (Members '[Reader (ParserSig t e d), State (LocalParams' d)] r) => (Int -> Text -> ParsecS r d) -> ParsecS r d
namedRef' f = do
  off <- P.getOffset
  txt <- identifier @t @e @d
  mr <- lift $ gets (HashMap.lookup txt . (^. localParamsNameMap))
  case mr of
    Just r -> return r
    Nothing -> f off txt

namedRef :: forall t e d r. (Members '[Reader (ParserSig t e d), State (LocalParams' d)] r) => ParsecS r d
namedRef = namedRef' @t @e @d (\off _ -> parseFailure off "undeclared identifier")

parseField ::
  forall t e d r.
  (Members '[Reader (ParserSig t e d), InfoTableBuilder' t e] r) =>
  DirectRef ->
  ParsecS r MemRef
parseField dref = do
  dot
  tag <- constrTag @t @e @d
  off <- (^. withLocParam) <$> brackets integer
  return $ ConstrRef (Field Nothing tag dref (fromInteger off))

constrTag ::
  forall t e d r.
  (Members '[Reader (ParserSig t e d), InfoTableBuilder' t e] r) =>
  ParsecS r Tag
constrTag = do
  off <- P.getOffset
  txt <- identifier @t @e @d
  idt <- lift $ getIdent' @t @e txt
  case idt of
    Just (IdentConstr tag) -> return tag
    _ -> parseFailure off "expected a constructor"

indSymbol ::
  forall t e d r.
  (Members '[Reader (ParserSig t e d), InfoTableBuilder' t e] r) =>
  ParsecS r Symbol
indSymbol = do
  off <- P.getOffset
  txt <- identifier @t @e @d
  idt <- lift $ getIdent' @t @e txt
  case idt of
    Just (IdentInd sym) -> return sym
    _ -> parseFailure off "expected an inductive type"

funSymbol ::
  forall t e d r.
  (Members '[Reader (ParserSig t e d), InfoTableBuilder' t e] r) =>
  ParsecS r Symbol
funSymbol = do
  off <- P.getOffset
  txt <- identifier @t @e @d
  idt <- lift $ getIdent' @t @e txt
  case idt of
    Just (IdentFwd sym) -> return sym
    Just (IdentFun sym) -> return sym
    _ -> parseFailure off "expected a function"
