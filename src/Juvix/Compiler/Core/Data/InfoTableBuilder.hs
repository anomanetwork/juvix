module Juvix.Compiler.Core.Data.InfoTableBuilder
  ( module Juvix.Compiler.Core.Data.InfoTable,
    module Juvix.Compiler.Core.Data.InfoTableBuilder,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Core.Data.InfoTable
import Juvix.Compiler.Core.Extra.Base
import Juvix.Compiler.Core.Info.NameInfo
import Juvix.Compiler.Core.Language

data InfoTableBuilder m a where
  FreshSymbol :: InfoTableBuilder m Symbol
  FreshTag :: InfoTableBuilder m Tag
  RegisterIdent :: Text -> IdentifierInfo -> InfoTableBuilder m ()
  RegisterConstructor :: Text -> ConstructorInfo -> InfoTableBuilder m ()
  RegisterInductive :: Text -> InductiveInfo -> InfoTableBuilder m ()
  RegisterIdentNode :: Symbol -> Node -> InfoTableBuilder m ()
  RegisterMain :: Symbol -> InfoTableBuilder m ()
  OverIdentArgsInfo :: Symbol -> ([ArgumentInfo] -> [ArgumentInfo]) -> InfoTableBuilder m ()
  GetIdent :: Text -> InfoTableBuilder m (Maybe IdentKind)
  GetInfoTable :: InfoTableBuilder m InfoTable

makeSem ''InfoTableBuilder

getConstructorInfo :: (Member InfoTableBuilder r) => Tag -> Sem r ConstructorInfo
getConstructorInfo tag = do
  tab <- getInfoTable
  return $ fromJust (HashMap.lookup tag (tab ^. infoConstructors))

getInductiveInfo :: (Member InfoTableBuilder r) => Symbol -> Sem r InductiveInfo
getInductiveInfo sym = do
  tab <- getInfoTable
  return $ fromJust (HashMap.lookup sym (tab ^. infoInductives))

getIdentifierInfo :: (Member InfoTableBuilder r) => Symbol -> Sem r IdentifierInfo
getIdentifierInfo sym = do
  tab <- getInfoTable
  return $ fromJust (HashMap.lookup sym (tab ^. infoIdentifiers))

getBoolSymbol :: (Member InfoTableBuilder r) => Sem r Symbol
getBoolSymbol = do
  ci <- getConstructorInfo (BuiltinTag TagTrue)
  return $ ci ^. constructorInductive

getIOSymbol :: (Member InfoTableBuilder r) => Sem r Symbol
getIOSymbol = do
  ci <- getConstructorInfo (BuiltinTag TagWrite)
  return $ ci ^. constructorInductive

getNatSymbol :: (Member InfoTableBuilder r) => Sem r Symbol
getNatSymbol = do
  tab <- getInfoTable
  return $ fromJust (lookupBuiltinInductive tab BuiltinNat) ^. inductiveSymbol

checkSymbolDefined :: (Member InfoTableBuilder r) => Symbol -> Sem r Bool
checkSymbolDefined sym = do
  tab <- getInfoTable
  return $ HashMap.member sym (tab ^. identContext)

setIdentArgsInfo :: (Member InfoTableBuilder r) => Symbol -> [ArgumentInfo] -> Sem r ()
setIdentArgsInfo sym = overIdentArgsInfo sym . const

runInfoTableBuilder :: forall r a. InfoTable -> Sem (InfoTableBuilder ': r) a -> Sem r (InfoTable, a)
runInfoTableBuilder tab =
  runState tab
    . reinterpret interp
  where
    interp :: InfoTableBuilder m b -> Sem (State InfoTable : r) b
    interp = \case
      FreshSymbol -> do
        s <- get
        modify' (over infoNextSymbol (+ 1))
        return (s ^. infoNextSymbol)
      FreshTag -> do
        s <- get
        modify' (over infoNextTag (+ 1))
        return (UserTag (s ^. infoNextTag))
      RegisterIdent idt ii -> do
        let sym = ii ^. identifierSymbol
        let identKind = IdentFun (ii ^. identifierSymbol)
        whenJust
          (ii ^. identifierBuiltin)
          (\b -> modify' (over infoBuiltins (HashMap.insert (BuiltinsFunction b) identKind)))
        modify' (over infoIdentifiers (HashMap.insert sym ii))
        modify' (over identMap (HashMap.insert idt identKind))
      RegisterConstructor idt ci -> do
        let tag = ci ^. constructorTag
        let identKind = IdentConstr tag
        whenJust
          (ci ^. constructorBuiltin)
          (\b -> modify' (over infoBuiltins (HashMap.insert (BuiltinsConstructor b) identKind)))
        modify' (over infoConstructors (HashMap.insert tag ci))
        modify' (over identMap (HashMap.insert idt identKind))
      RegisterInductive idt ii -> do
        let sym = ii ^. inductiveSymbol
        let identKind = IdentInd sym
        whenJust
          (ii ^. inductiveBuiltin)
          (\b -> modify' (over infoBuiltins (HashMap.insert (builtinTypeToPrim b) identKind)))
        modify' (over infoInductives (HashMap.insert sym ii))
        modify' (over identMap (HashMap.insert idt identKind))
      RegisterIdentNode sym node ->
        modify' (over identContext (HashMap.insert sym node))
      RegisterMain sym -> do
        modify' (set infoMain (Just sym))
      OverIdentArgsInfo sym f -> do
        argsInfo <- f <$> gets (^. infoIdentifiers . at sym . _Just . identifierArgsInfo)
        modify' (set (infoIdentifiers . at sym . _Just . identifierArgsInfo) argsInfo)
        modify' (set (infoIdentifiers . at sym . _Just . identifierArgsNum) (length argsInfo))
        modify' (over infoIdentifiers (HashMap.adjust (over identifierType (expandType (map (^. argumentType) argsInfo))) sym))
      GetIdent txt -> do
        s <- get
        return $ HashMap.lookup txt (s ^. identMap)
      GetInfoTable ->
        get

--------------------------------------------
-- Builtin declarations
--------------------------------------------

createBuiltinConstr ::
  Symbol ->
  Tag ->
  Text ->
  Type ->
  Maybe BuiltinConstructor ->
  ConstructorInfo
createBuiltinConstr sym tag nameTxt ty cblt =
  ConstructorInfo
    { _constructorName = nameTxt,
      _constructorLocation = Nothing,
      _constructorTag = tag,
      _constructorType = ty,
      _constructorArgsNum = length (typeArgs ty),
      _constructorInductive = sym,
      _constructorBuiltin = cblt
    }

builtinConstrs ::
  Symbol ->
  Type ->
  [(Tag, Text, Type -> Type, Maybe BuiltinConstructor)] ->
  [ConstructorInfo]
builtinConstrs sym ty ctrs =
  map (\(tag, name, fty, cblt) -> createBuiltinConstr sym tag name (fty ty) cblt) ctrs

declareInductiveBuiltins ::
  (Member InfoTableBuilder r) =>
  Text ->
  Maybe BuiltinType ->
  [(Tag, Text, Type -> Type, Maybe BuiltinConstructor)] ->
  Sem r ()
declareInductiveBuiltins indName blt ctrs = do
  sym <- freshSymbol
  let ty = mkTypeConstr (setInfoName indName mempty) sym []
      constrs = builtinConstrs sym ty ctrs
  registerInductive
    indName
    ( InductiveInfo
        { _inductiveName = indName,
          _inductiveLocation = Nothing,
          _inductiveSymbol = sym,
          _inductiveKind = mkDynamic',
          _inductiveConstructors = constrs,
          _inductivePositive = True,
          _inductiveParams = [],
          _inductiveBuiltin = blt
        }
    )
  mapM_ (\ci -> registerConstructor (ci ^. constructorName) ci) constrs

builtinIOConstrs :: [(Tag, Text, Type -> Type, Maybe BuiltinConstructor)]
builtinIOConstrs =
  [ (BuiltinTag TagReturn, "return", mkPi' mkDynamic', Nothing),
    (BuiltinTag TagBind, "bind", \ty -> mkPi' ty (mkPi' (mkPi' mkDynamic' ty) ty), Nothing),
    (BuiltinTag TagWrite, "write", mkPi' mkDynamic', Nothing),
    (BuiltinTag TagReadLn, "readLn", id, Nothing)
  ]

declareIOBuiltins :: (Member InfoTableBuilder r) => Sem r ()
declareIOBuiltins =
  declareInductiveBuiltins
    "IO"
    (Just (BuiltinTypeAxiom BuiltinIO))
    builtinIOConstrs

declareBoolBuiltins :: (Member InfoTableBuilder r) => Sem r ()
declareBoolBuiltins =
  declareInductiveBuiltins
    "Bool"
    (Just (BuiltinTypeInductive BuiltinBool))
    [ (BuiltinTag TagTrue, "true", const mkTypeBool', Just BuiltinBoolTrue),
      (BuiltinTag TagFalse, "false", const mkTypeBool', Just BuiltinBoolFalse)
    ]

declareNatBuiltins :: (Member InfoTableBuilder r) => Sem r ()
declareNatBuiltins = do
  tagZero <- freshTag
  tagSuc <- freshTag
  declareInductiveBuiltins
    "Nat"
    (Just (BuiltinTypeInductive BuiltinNat))
    [ (tagZero, "zero", id, Just BuiltinNatZero),
      (tagSuc, "suc", \x -> mkPi' x x, Just BuiltinNatSuc)
    ]
