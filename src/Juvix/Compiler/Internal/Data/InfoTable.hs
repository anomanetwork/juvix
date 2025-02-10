module Juvix.Compiler.Internal.Data.InfoTable
  ( module Juvix.Compiler.Store.Internal.Language,
    computeInternalModule,
    computeInternalModuleInfoTable,
    extendWithReplExpression,
    lookupConstructor,
    lookupConstructorArgTypes,
    lookupFunctionMaybe,
    lookupFunction,
    lookupConstructorReturnType,
    lookupInductive,
    lookupAxiom,
    lookupInductiveType,
    lookupConstructorType,
    getAxiomBuiltinInfo,
    getFunctionBuiltinInfo,
    mkConstructorEntries,
    functionInfoFromFunctionDef,
    inductiveInfoFromInductiveDef,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Internal.Extra
import Juvix.Compiler.Internal.Extra.CoercionInfo
import Juvix.Compiler.Internal.Extra.InstanceInfo
import Juvix.Compiler.Internal.Pretty (ppTrace)
import Juvix.Compiler.Store.Internal.Data.FunctionsTable
import Juvix.Compiler.Store.Internal.Data.TypesTable
import Juvix.Compiler.Store.Internal.Language
import Juvix.Prelude

functionInfoFromFunctionDef :: Bool -> FunctionDef -> FunctionInfo
functionInfoFromFunctionDef isLocal FunctionDef {..} =
  FunctionInfo
    { _functionInfoName = _funDefName,
      _functionInfoType = _funDefType,
      _functionInfoArgsInfo = _funDefArgsInfo,
      _functionInfoBuiltin = _funDefBuiltin,
      _functionInfoIsInstanceCoercion = _funDefIsInstanceCoercion,
      _functionInfoTerminating = _funDefTerminating,
      _functionInfoPragmas = _funDefPragmas,
      _functionInfoIsLocal = isLocal
    }

inductiveInfoFromInductiveDef :: InductiveDef -> InductiveInfo
inductiveInfoFromInductiveDef d@InductiveDef {..} =
  InductiveInfo
    { _inductiveInfoName = _inductiveName,
      _inductiveInfoType = _inductiveType,
      _inductiveInfoLoc = getLoc d,
      _inductiveInfoBuiltin = _inductiveBuiltin,
      _inductiveInfoParameters = _inductiveParameters,
      _inductiveInfoConstructors = map (^. inductiveConstructorName) _inductiveConstructors,
      _inductiveInfoPositive = _inductivePositive,
      _inductiveInfoTrait = _inductiveTrait,
      _inductiveInfoPragmas = _inductivePragmas
    }

extendWithReplExpression :: Expression -> InfoTable -> InfoTable
extendWithReplExpression e =
  over
    infoFunctions
    ( HashMap.union
        ( HashMap.fromList
            [ (f ^. funDefName, functionInfoFromFunctionDef True f)
              | f <- letFunctionDefs e
            ]
        )
    )

letFunctionDefs :: (HasExpressions a) => a -> [FunctionDef]
letFunctionDefs e =
  concat
    [ concatMap (toList . flattenClause) _letClauses
      | Let {..} <- letDefs e
    ]
  where
    flattenClause :: LetClause -> NonEmpty FunctionDef
    flattenClause = \case
      LetFunDef f -> pure f
      LetMutualBlock (MutualBlockLet fs) -> fs

computeInternalModule :: InternalModuleTable -> TypeCheckingTables -> Module -> InternalModule
computeInternalModule itab tabs m@Module {..} =
  InternalModule
    { _internalModuleId = _moduleId,
      _internalModuleName = _moduleName,
      _internalModuleImports =
        map
          ( (^. internalModuleId)
              . lookupInternalModule itab
              . (^. importModuleName)
          )
          (_moduleBody ^. moduleImports),
      _internalModuleInfoTable = computeInternalModuleInfoTable m,
      _internalModuleTypeCheckingTables = tabs
    }

computeInternalModuleInfoTable :: Module -> InfoTable
computeInternalModuleInfoTable m = InfoTable {..}
  where
    mutuals :: [MutualStatement]
    mutuals =
      [ d
        | MutualBlock b <- ss,
          d <- toList b
      ]

    inductives :: [InductiveDef]
    inductives =
      [ d
        | StatementInductive d <- mutuals
      ]

    _infoInductives :: HashMap Name InductiveInfo
    _infoInductives =
      HashMap.fromList
        [ (d ^. inductiveName, inductiveInfoFromInductiveDef d)
          | d <- inductives
        ]

    _infoConstructors :: HashMap Name ConstructorInfo
    _infoConstructors =
      HashMap.fromList
        [ e
          | d <- inductives,
            e <- mkConstructorEntries d
        ]

    _infoFunctions :: HashMap Name FunctionInfo
    _infoFunctions =
      HashMap.fromList $
        [ (f ^. funDefName, functionInfoFromFunctionDef False f)
          | StatementFunction f <- mutuals
        ]
          <> [ (f ^. funDefName, functionInfoFromFunctionDef True f)
               | s <- ss,
                 f <- letFunctionDefs s
             ]

    _infoAxioms :: HashMap Name AxiomInfo
    _infoAxioms =
      HashMap.fromList
        [ (d ^. axiomName, AxiomInfo d)
          | StatementAxiom d <- mutuals
        ]

    _infoBuiltins :: HashMap BuiltinPrim Name
    _infoBuiltins =
      HashMap.fromList $
        mapMaybe goInd (HashMap.elems _infoInductives)
          <> mapMaybe goConstr (HashMap.elems _infoConstructors)
          <> mapMaybe goFun (HashMap.elems _infoFunctions)
          <> mapMaybe goAxiom (HashMap.elems _infoAxioms)
      where
        goInd :: InductiveInfo -> Maybe (BuiltinPrim, Name)
        goInd InductiveInfo {..} =
          _inductiveInfoBuiltin
            >>= (\b -> Just (BuiltinsInductive b, _inductiveInfoName))

        goConstr :: ConstructorInfo -> Maybe (BuiltinPrim, Name)
        goConstr ConstructorInfo {..} =
          _constructorInfoBuiltin
            >>= (\b -> Just (BuiltinsConstructor b, _constructorInfoName))

        goFun :: FunctionInfo -> Maybe (BuiltinPrim, Name)
        goFun FunctionInfo {..} =
          _functionInfoBuiltin
            >>= (\b -> Just (BuiltinsFunction b, _functionInfoName))

        goAxiom :: AxiomInfo -> Maybe (BuiltinPrim, Name)
        goAxiom AxiomInfo {..} =
          _axiomInfoDef ^. axiomBuiltin
            >>= (\b -> Just (BuiltinsAxiom b, _axiomInfoDef ^. axiomName))

    ss :: [MutualBlock]
    ss = m ^. moduleBody . moduleStatements

lookupConstructor :: forall r. (Member (Reader InfoTable) r) => Name -> Sem r ConstructorInfo
lookupConstructor f = do
  err <- impossibleErr
  HashMap.lookupDefault err f <$> asks (^. infoConstructors)
  where
    impossibleErr :: Sem r a
    impossibleErr = do
      tbl <- asks (^. infoConstructors)
      return
        . error
        $ "impossible: "
          <> ppTrace f
          <> " is not in the InfoTable\n"
          <> "\nThe registered constructors are: "
          <> ppTrace (HashMap.keys tbl)

lookupConstructorArgTypes :: (Member (Reader InfoTable) r) => Name -> Sem r ([InductiveParameter], [FunctionParameter])
lookupConstructorArgTypes = fmap constructorArgTypes . lookupConstructor

lookupInductive :: forall r. (Member (Reader InfoTable) r) => InductiveName -> Sem r InductiveInfo
lookupInductive f = do
  err <- impossibleErr
  HashMap.lookupDefault err f <$> asks (^. infoInductives)
  where
    impossibleErr :: Sem r a
    impossibleErr = do
      tbl <- asks (^. infoInductives)
      return
        . error
        $ "impossible: "
          <> ppTrace f
          <> " is not in the InfoTable\n"
          <> "\nThe registered inductives are: "
          <> ppTrace (HashMap.keys tbl)

lookupFunctionMaybe :: forall r. (Member (Reader InfoTable) r) => Name -> Sem r (Maybe FunctionInfo)
lookupFunctionMaybe f = HashMap.lookup f <$> asks (^. infoFunctions)

lookupFunction :: forall r. (Member (Reader InfoTable) r) => Name -> Sem r FunctionInfo
lookupFunction f = do
  err <- impossibleErr
  fromMaybe err <$> lookupFunctionMaybe f
  where
    impossibleErr :: Sem r a
    impossibleErr = do
      tbl <- asks (^. infoFunctions)
      return
        . error
        $ "impossible: "
          <> ppTrace f
          <> " is not in the InfoTable\n"
          <> ppTrace (getLoc f)
          <> "\nThe registered functions are: "
          <> ppTrace (HashMap.keys tbl)

lookupAxiom :: (Member (Reader InfoTable) r) => Name -> Sem r AxiomInfo
lookupAxiom f = HashMap.lookupDefault (error ("impossible: couldn't find axiom " <> ppTrace f)) f <$> asks (^. infoAxioms)

lookupInductiveType :: (Member (Reader InfoTable) r) => Name -> Sem r Expression
lookupInductiveType v = fullInductiveType <$> lookupInductive v

lookupConstructorType :: (Member (Reader InfoTable) r) => ConstrName -> Sem r Expression
lookupConstructorType = fmap constructorType . lookupConstructor

lookupConstructorReturnType :: (Member (Reader InfoTable) r) => ConstrName -> Sem r Expression
lookupConstructorReturnType = fmap constructorReturnType . lookupConstructor

getAxiomBuiltinInfo :: (Member (Reader InfoTable) r) => Name -> Sem r (Maybe BuiltinAxiom)
getAxiomBuiltinInfo n = do
  maybeAxiomInfo <- HashMap.lookup n <$> asks (^. infoAxioms)
  return $ case maybeAxiomInfo of
    Just axiomInfo -> axiomInfo ^. axiomInfoDef . axiomBuiltin
    Nothing -> Nothing

getFunctionBuiltinInfo :: (Member (Reader InfoTable) r) => Name -> Sem r (Maybe BuiltinFunction)
getFunctionBuiltinInfo n = do
  maybeFunInfo <- HashMap.lookup n <$> asks (^. infoFunctions)
  return $ case maybeFunInfo of
    Just funInfo -> funInfo ^. functionInfoBuiltin
    Nothing -> Nothing

mkConstructorEntries :: InductiveDef -> [(ConstructorName, ConstructorInfo)]
mkConstructorEntries d =
  [ (c ^. inductiveConstructorName, ConstructorInfo {..})
    | let _constructorInfoInductive = d ^. inductiveName
          n = length (d ^. inductiveConstructors)
          _constructorInfoInductiveParameters = d ^. inductiveParameters
          builtins = maybe (replicate n Nothing) (map Just . builtinConstructors) (d ^. inductiveBuiltin),
      (_constructorInfoBuiltin, c) <- zipExact builtins (d ^. inductiveConstructors),
      let _constructorInfoType = c ^. inductiveConstructorType,
      let _constructorInfoName = c ^. inductiveConstructorName,
      let _constructorInfoTrait = d ^. inductiveTrait,
      let _constructorInfoRecord = c ^. inductiveConstructorIsRecord,
      let _constructorSelfRecursiveArgs = selfRecursiveArgs (c ^. inductiveConstructorType)
  ]
  where
    selfRecursiveArgs :: Expression -> [Bool]
    selfRecursiveArgs constrTy = constructorArgs constrTy ^.. each . paramType . to (== ty)
      where
        ty :: Expression
        ty =
          foldExplicitApplication
            (toExpression (d ^. inductiveName))
            (d ^.. inductiveParameters . each . inductiveParamName . to toExpression)
