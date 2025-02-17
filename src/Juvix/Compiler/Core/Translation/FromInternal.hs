{-# LANGUAGE ImpredicativeTypes #-}

module Juvix.Compiler.Core.Translation.FromInternal where

import Data.HashMap.Strict qualified as HashMap
import Data.List.NonEmpty qualified as NonEmpty
import Juvix.Compiler.Core.Data
import Juvix.Compiler.Core.Extra
import Juvix.Compiler.Core.Info qualified as Info
import Juvix.Compiler.Core.Info.LocationInfo
import Juvix.Compiler.Core.Info.NameInfo
import Juvix.Compiler.Core.Info.PragmaInfo
import Juvix.Compiler.Core.Translation.FromInternal.Builtins.Int
import Juvix.Compiler.Core.Translation.FromInternal.Builtins.Nat
import Juvix.Compiler.Core.Translation.FromInternal.Data
import Juvix.Compiler.Internal.Data.Name
import Juvix.Compiler.Internal.Extra qualified as Internal
import Juvix.Compiler.Internal.Pretty (ppPrint)
import Juvix.Compiler.Internal.Pretty qualified as Internal
import Juvix.Compiler.Internal.Translation.Extra qualified as Internal
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking qualified as InternalTyped
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Error
import Juvix.Compiler.Store.Extra qualified as Store
import Juvix.Compiler.Store.Language qualified as Store
import Juvix.Data.Loc qualified as Loc
import Juvix.Extra.Strings qualified as Str

data PreInductiveDef = PreInductiveDef
  { _preInductiveInternal :: Internal.InductiveDef,
    _preInductiveInfo :: InductiveInfo
  }

data PreFunctionDef = PreFunctionDef
  { _preFunInternal :: Internal.FunctionDef,
    _preFunSym :: Symbol,
    _preFunType :: Type
  }

data PreMutual = PreMutual
  { _preInductives :: [PreInductiveDef],
    _preFunctions :: [PreFunctionDef]
  }

emptyPreMutual :: PreMutual
emptyPreMutual =
  PreMutual
    { _preInductives = [],
      _preFunctions = []
    }

makeLenses ''PreMutual

-- | Translation of a Name into the identifier index used in the Core InfoTable
mkIdentIndex :: Name -> Text
mkIdentIndex = show . (^. Internal.nameId)

computeImplicitArgs :: Internal.Expression -> [Bool]
computeImplicitArgs = \case
  Internal.ExpressionFunction Internal.Function {..}
    | _functionLeft ^. Internal.paramImplicit == Implicit ->
        True : computeImplicitArgs _functionRight
    | otherwise ->
        False : computeImplicitArgs _functionRight
  _ -> []

fromInternal ::
  (Members '[NameIdGen, Reader Store.ModuleTable, Error JuvixError] k) =>
  InternalTyped.InternalTypedResult ->
  Sem k CoreResult
fromInternal i = mapError (JuvixError . ErrBadScope) $ do
  importTab <- asks Store.getInternalModuleTable
  coreImportsTab <- asks Store.computeCombinedCoreInfoTable
  let imd = i ^. InternalTyped.resultInternalModule
      md =
        Module
          { _moduleId = imd ^. Internal.internalModuleId,
            _moduleInfoTable = mempty,
            _moduleImports = imd ^. Internal.internalModuleImports,
            _moduleImportsTable = coreImportsTab,
            _moduleSHA256 = ""
          }
      tabs = i ^. InternalTyped.resultTypeCheckingTables
  res <-
    execInfoTableBuilder md
      . runReader (tabs ^. InternalTyped.typeCheckingTablesFunctionsTable)
      . runReader (tabs ^. InternalTyped.typeCheckingTablesTypesTable)
      $ do
        when
          (isNothing (coreImportsTab ^. infoLiteralIntToNat))
          reserveLiteralIntToNatSymbol
        when
          (isNothing (coreImportsTab ^. infoLiteralIntToInt))
          reserveLiteralIntToIntSymbol
        let resultModule = i ^. InternalTyped.resultModule
            resultTable =
              i ^. InternalTyped.resultInternalModule . Internal.internalModuleInfoTable
                <> Internal.computeCombinedInfoTable importTab
        runReader resultTable $
          goModule resultModule
        md' <- getModule
        when (InternalTyped.getInternalTypedResultIsMainFile i) $
          forM_ (md' ^. moduleInfoTable . infoIdentifiers) $ \f -> do
            when (f ^. identifierName == Str.main) $
              registerMain (f ^. identifierSymbol)
        when
          (isNothing (lookupBuiltinInductive md' BuiltinBool))
          declareBoolBuiltins
        when (isNothing (coreImportsTab ^. infoLiteralIntToNat)) $
          setupLiteralIntToNat literalIntToNatNode
        when (isNothing (coreImportsTab ^. infoLiteralIntToInt)) $
          setupLiteralIntToInt literalIntToIntNode
  return $
    CoreResult
      { _coreResultModule = res,
        _coreResultInternalTypedResult = i
      }

fromInternalExpression :: (Members '[NameIdGen, Error BadScope] r) => Internal.InternalModuleTable -> CoreResult -> Internal.Expression -> Sem r Node
fromInternalExpression importTab res exp = do
  let mtab =
        res ^. coreResultInternalTypedResult . InternalTyped.resultInternalModule . Internal.internalModuleInfoTable
          <> Internal.computeCombinedInfoTable importTab
      tabs = res ^. coreResultInternalTypedResult . InternalTyped.resultTypeCheckingTables
  fmap snd
    . runReader mtab
    . runInfoTableBuilder (res ^. coreResultModule)
    . runReader (tabs ^. InternalTyped.typeCheckingTablesFunctionsTable)
    . runReader (tabs ^. InternalTyped.typeCheckingTablesTypesTable)
    $ fromTopIndex (goExpression exp)

goModule ::
  forall r.
  (Members '[InfoTableBuilder, Reader InternalTyped.TypesTable, Reader InternalTyped.FunctionsTable, Reader Internal.InfoTable, NameIdGen, Error BadScope] r) =>
  Internal.Module ->
  Sem r ()
goModule m = do
  mapM_ goMutualBlock (m ^. Internal.moduleBody . Internal.moduleStatements)

-- | predefine an inductive definition
preInductiveDef ::
  forall r.
  (Members '[InfoTableBuilder, Reader InternalTyped.TypesTable, Reader InternalTyped.FunctionsTable, Reader Internal.InfoTable, NameIdGen, Error BadScope] r) =>
  Internal.InductiveDef ->
  Sem r PreInductiveDef
preInductiveDef i = do
  sym <- freshSymbol
  let _inductiveName = i ^. Internal.inductiveName . nameText
  params' <- fromTopIndex $ forM (i ^. Internal.inductiveParameters) $ \p -> do
    ty' <- goExpression (p ^. Internal.inductiveParamType)
    return
      ParameterInfo
        { _paramName = p ^. Internal.inductiveParamName . nameText,
          _paramLocation = Just $ getLoc p,
          _paramIsImplicit = False,
          _paramKind = ty'
        }
  let info =
        InductiveInfo
          { _inductiveLocation = Just $ i ^. Internal.inductiveName . nameLoc,
            _inductiveSymbol = sym,
            _inductiveKind = mkSmallUniv,
            _inductiveConstructors = [],
            _inductiveParams = params',
            _inductivePositive = i ^. Internal.inductivePositive,
            _inductiveBuiltin = BuiltinTypeInductive <$> i ^. Internal.inductiveBuiltin,
            _inductivePragmas = i ^. Internal.inductivePragmas,
            _inductiveName
          }
      idx = mkIdentIndex (i ^. Internal.inductiveName)
  -- The inductive needs to be registered before translating the constructors,
  -- because their types refer to the inductive
  registerInductive idx info
  return
    PreInductiveDef
      { _preInductiveInfo = info,
        _preInductiveInternal = i
      }

goInductiveDef ::
  forall r.
  (Members '[InfoTableBuilder, Reader InternalTyped.TypesTable, Reader InternalTyped.FunctionsTable, Reader Internal.InfoTable, NameIdGen, Error BadScope] r) =>
  PreInductiveDef ->
  Sem r ()
goInductiveDef PreInductiveDef {..} = do
  let i = _preInductiveInternal
      info = _preInductiveInfo
      idx = mkIdentIndex (i ^. Internal.inductiveName)
      sym = info ^. inductiveSymbol
  ctorInfos <- mapM (goConstructor sym) (i ^. Internal.inductiveConstructors)
  let ctorTags = map (^. constructorTag) ctorInfos
  registerInductive idx (set inductiveConstructors ctorTags info)

goConstructor ::
  forall r.
  (Members '[InfoTableBuilder, Reader Internal.InfoTable, Reader InternalTyped.TypesTable, Reader InternalTyped.FunctionsTable, NameIdGen, Error BadScope] r) =>
  Symbol ->
  Internal.ConstructorDef ->
  Sem r ConstructorInfo
goConstructor sym ctor = do
  mblt <- mBuiltin
  tag <- ctorTag mblt
  ty <- ctorType
  argsNum' <- argsNum
  let _constructorName = ctorName ^. nameText
      info =
        ConstructorInfo
          { _constructorLocation = Just $ ctorName ^. nameLoc,
            _constructorTag = tag,
            _constructorType = ty,
            _constructorArgsNum = argsNum',
            _constructorArgNames = replicate argsNum' Nothing,
            _constructorInductive = sym,
            _constructorBuiltin = mblt,
            _constructorFixity = ctorName ^. nameFixity,
            _constructorPragmas = ctor ^. Internal.inductiveConstructorPragmas,
            _constructorName
          }

  registerConstructor (mkIdentIndex (ctor ^. Internal.inductiveConstructorName)) info
  return info
  where
    mBuiltin :: Sem r (Maybe Internal.BuiltinConstructor)
    mBuiltin =
      (^. Internal.constructorInfoBuiltin)
        . HashMap.lookupDefault impossible (ctor ^. Internal.inductiveConstructorName)
        <$> asks (^. Internal.infoConstructors)

    ctorName :: Internal.Name
    ctorName = ctor ^. Internal.inductiveConstructorName

    ctorTag :: Maybe Internal.BuiltinConstructor -> Sem r Tag
    ctorTag = \case
      Nothing -> freshTag
      Just b -> case b of
        Internal.BuiltinBoolTrue -> return (BuiltinTag TagTrue)
        Internal.BuiltinBoolFalse -> return (BuiltinTag TagFalse)
        Internal.BuiltinMkEq -> freshTag
        Internal.BuiltinNatZero -> freshTag
        Internal.BuiltinNatSuc -> freshTag
        Internal.BuiltinIntOfNat -> freshTag
        Internal.BuiltinIntNegSuc -> freshTag
        Internal.BuiltinListNil -> freshTag
        Internal.BuiltinListCons -> freshTag
        Internal.BuiltinMaybeNothing -> freshTag
        Internal.BuiltinMaybeJust -> freshTag
        Internal.BuiltinPairConstr -> freshTag
        Internal.BuiltinMkPoseidonState -> freshTag
        Internal.BuiltinMkEcPoint -> freshTag
        Internal.BuiltinMkAnomaAction -> freshTag
        Internal.BuiltinMkAnomaResource -> freshTag
        Internal.BuiltinMkOrd -> freshTag
        Internal.BuiltinOrderingLT -> freshTag
        Internal.BuiltinOrderingGT -> freshTag
        Internal.BuiltinOrderingEQ -> freshTag

    ctorType :: Sem r Type
    ctorType =
      runReader
        initIndexTable
        ( Internal.lookupConstructorType ctorName
            >>= goType
        )

    argsNum :: Sem r Int
    argsNum = do
      (indParams, ctorArgs) <- InternalTyped.lookupConstructorArgTypes ctorName
      return (length indParams + length ctorArgs)

goMutualBlock ::
  forall r.
  (Members '[InfoTableBuilder, Reader InternalTyped.TypesTable, Reader InternalTyped.FunctionsTable, Reader Internal.InfoTable, NameIdGen, Error BadScope] r) =>
  Internal.MutualBlock ->
  Sem r ()
goMutualBlock (Internal.MutualBlock m) = preMutual m >>= goMutual
  where
    preMutual :: NonEmpty Internal.MutualStatement -> Sem r PreMutual
    preMutual stmts = do
      let (inds, funs) = partition isTypDef (toList stmts)
      -- types must be pre-registered first to avoid crashing on unknown types
      -- when pre-registering functions/axioms
      execState emptyPreMutual $ mapM_ step (inds ++ funs)
      where
        isTypDef :: Internal.MutualStatement -> Bool
        isTypDef = \case
          Internal.StatementInductive {} -> True
          Internal.StatementFunction {} -> False
          Internal.StatementAxiom a
            | isJust (builtinInductive a) -> True
            | exprIsType (a ^. Internal.axiomType) -> True
            | otherwise -> False
            where
              exprIsType :: Internal.Expression -> Bool
              exprIsType = \case
                Internal.ExpressionUniverse {} -> True
                Internal.ExpressionFunction (Internal.Function l r) -> exprIsType (l ^. Internal.paramType) && exprIsType r
                Internal.ExpressionIden {} -> False
                Internal.ExpressionApplication {} -> False
                Internal.ExpressionLiteral {} -> False
                Internal.ExpressionHole {} -> False
                Internal.ExpressionInstanceHole {} -> False
                Internal.ExpressionLet {} -> False
                Internal.ExpressionSimpleLambda {} -> False
                Internal.ExpressionLambda {} -> False
                Internal.ExpressionCase {} -> False

        step :: Internal.MutualStatement -> Sem (State PreMutual ': r) ()
        step = \case
          Internal.StatementFunction f -> do
            p <- preFunctionDef f
            modify' (over preFunctions (p :))
          Internal.StatementInductive i -> do
            p <- preInductiveDef i
            modify' (over preInductives (p :))
          Internal.StatementAxiom a -> do
            goAxiomInductive a
            goAxiomDef a

    goMutual :: PreMutual -> Sem r ()
    goMutual PreMutual {..} = do
      forM_ _preInductives goInductiveDef
      forM_ _preFunctions goFunctionDef

preFunctionDef ::
  forall r.
  (Members '[InfoTableBuilder, Reader Internal.InfoTable, Reader InternalTyped.TypesTable, Reader InternalTyped.FunctionsTable, NameIdGen, Error BadScope] r) =>
  Internal.FunctionDef ->
  Sem r PreFunctionDef
preFunctionDef f = do
  sym <- freshSymbol
  funTy <- fromTopIndex (goType (f ^. Internal.funDefType))
  let _identifierName = f ^. Internal.funDefName . nameText
      implArgs = computeImplicitArgs (f ^. Internal.funDefType)
      info =
        IdentifierInfo
          { _identifierName = normalizeBuiltinName (f ^. Internal.funDefBuiltin) (f ^. Internal.funDefName . nameText),
            _identifierLocation = Just $ f ^. Internal.funDefName . nameLoc,
            _identifierSymbol = sym,
            _identifierType = funTy,
            -- _identiferArgsNum needs to match the number of lambdas in the
            -- body. This needs to be filled in later (in goFunctionDef).
            _identifierArgsNum = 0,
            _identifierIsExported = False,
            _identifierBuiltin = f ^. Internal.funDefBuiltin,
            _identifierPragmas =
              adjustPragmas' implArgs (f ^. Internal.funDefPragmas),
            _identifierArgNames = argnames
          }
  case f ^. Internal.funDefBuiltin of
    Just b
      | isIgnoredBuiltin b -> return ()
    _ -> do
      registerIdent (mkIdentIndex (f ^. Internal.funDefName)) info
  return
    PreFunctionDef
      { _preFunInternal = f,
        _preFunSym = sym,
        _preFunType = funTy
      }
  where
    argnames :: [Maybe Text]
    argnames = case f ^. Internal.funDefPragmas . pragmasArgNames of
      Just argns ->
        map Just (argns ^. pragmaArgNames)
      Nothing ->
        map
          getPatternName
          (fst (Internal.unfoldLambda (f ^. Internal.funDefBody)))

    normalizeBuiltinName :: Maybe BuiltinFunction -> Text -> Text
    normalizeBuiltinName blt name = case blt of
      Just b
        | isNatBuiltin b -> show (pretty b)
      _ -> case name of
        ">" -> Str.natGt
        ">=" -> Str.natGe
        _ -> name

    getPatternName :: Internal.PatternArg -> Maybe Text
    getPatternName pat = case pat ^. Internal.patternArgName of
      Just n -> Just (n ^. nameText)
      Nothing -> case pat ^. Internal.patternArgPattern of
        Internal.PatternVariable n -> Just (n ^. nameText)
        _ -> Nothing

goFunctionDef ::
  forall r.
  (Members '[InfoTableBuilder, Reader Internal.InfoTable, Reader InternalTyped.TypesTable, Reader InternalTyped.FunctionsTable, NameIdGen, Error BadScope] r) =>
  PreFunctionDef ->
  Sem r ()
goFunctionDef PreFunctionDef {..} = do
  let f = _preFunInternal
      sym = _preFunSym
      ty = _preFunType
  mbody <- case _preFunInternal ^. Internal.funDefBuiltin of
    Just b
      | isIgnoredBuiltin b -> return Nothing
    _ -> Just <$> fromTopIndex (mkFunBody ty f)
  forM_ mbody (registerIdentNode sym)
  forM_ mbody setIdentArgsInfo'
  where
    setIdentArgsInfo' :: Node -> Sem r ()
    setIdentArgsInfo' node = do
      let (is, _) = unfoldLambdas node
      setIdentArgs _preFunSym (map (^. lambdaLhsBinder) is)

goType ::
  forall r.
  (Members '[InfoTableBuilder, Reader Internal.InfoTable, Reader InternalTyped.TypesTable, Reader InternalTyped.FunctionsTable, NameIdGen, Reader IndexTable, Error BadScope] r) =>
  Internal.Expression ->
  Sem r Type
goType ty = do
  normTy <- InternalTyped.strongNormalize'' ty
  squashApps <$> goExpression (normTy ^. Internal.normalizedExpression)

mkFunBody ::
  forall r.
  (Members '[InfoTableBuilder, Reader InternalTyped.TypesTable, Reader InternalTyped.FunctionsTable, Reader Internal.InfoTable, Reader IndexTable, NameIdGen, Error BadScope] r) =>
  Type -> -- converted type of the function
  Internal.FunctionDef ->
  Sem r Node
mkFunBody ty f =
  mkBody
    (WithLoc (getLoc f) (ppPrint f))
    ty
    (f ^. Internal.funDefName . nameLoc)
    (pure (Internal.unfoldLambda (f ^. Internal.funDefBody)))

mkBody ::
  forall r.
  (Members '[InfoTableBuilder, Reader InternalTyped.TypesTable, Reader InternalTyped.FunctionsTable, Reader Internal.InfoTable, Reader IndexTable, NameIdGen, Error BadScope] r) =>
  WithLoc Text -> -- The printed lambda for error message
  Type -> -- type of the function
  Location ->
  NonEmpty ([Internal.PatternArg], Internal.Expression) ->
  Sem r Node
mkBody ppLam ty loc clauses
  | nPatterns == 0 = goExpression (snd (head clauses))
  | otherwise = do
      let values = map (mkVar Info.empty) vs
          argtys = take nPatterns (typeArgs ty)
          argbinders = take nPatterns (typeArgsBinders ty)
          values' = map fst $ filter (isInductive . snd) (zipExact values argtys)
          matchArgtys = shiftMatchTypeArg <$> indexFrom 0 argtys
          matchTypeTarget = typeTarget ty
          matchIndArgTys = filter isInductive matchArgtys
          matchReturnType' = mkPis (drop nPatterns (typeArgsBinders ty)) matchTypeTarget
      case values' of
        [] -> do
          vars <- asks (^. indexTableVars)
          varsNum <- asks (^. indexTableVarsNum)
          let (pats, body) = head clauses
              (vars', varsNum') =
                foldl'
                  (\(vrs, k) pat -> (addPatternVariableNames pat k vrs, k + 1))
                  (vars, varsNum)
                  pats
          body' <-
            local
              (set indexTableVars vars' . set indexTableVarsNum varsNum')
              (goExpression body)
          return $ foldr (mkLambda mempty) body' argbinders
        _ : _ -> do
          varsNum <- asks (^. indexTableVarsNum)
          ms <- underBinders nPatterns (mapM (uncurry (goClause varsNum)) clauses)
          let i = setInfoLocation loc mempty
              match = mkMatch i (nonEmpty' matchIndArgTys) matchReturnType' (nonEmpty' values') (toList ms)
          return $ foldr (mkLambda mempty) match argbinders
  where
    -- Assumption: All clauses have the same number of patterns
    nPatterns :: Int
    nPatterns = checkPatternsNum (length (fst (head clauses))) (NonEmpty.tail (fmap fst clauses))

    vs :: [Index]
    vs = reverse (take nPatterns [0 ..])

    -- The types of arguments in the match must be shifted due to the
    -- difference in level between when the type argument in bound (in a
    -- surrounding lambda) and when it is used in the match constructor.
    --
    -- For example:
    --
    --    f : {A : Type} -> A -> List A -> A -> A -> List A;
    --    f _ _ _ := \ { _ := nil };
    --
    -- f has the following type, with indices:
    --
    --    A -> A$0 -> List A$1 -> A$2 -> A$3 -> A$4 -> List A$5
    --
    -- Is translated to the following match (omitting the translation of the body):
    --
    --    λ(? : Type)
    --      λ(? : A$0)
    --        λ(? : List A$1)
    --          λ(? : A$2)
    --            λ(? : A$3)
    --              match (?$2 : List A$4) with : (A$4 → List A$5)
    --
    -- The return type (A$4 -> List A$5) already has the correct indices
    -- relative to the match node. However the type of the match argument has
    -- been shifted by the number of pattern binders below it.
    shiftMatchTypeArg :: Indexed Type -> Type
    shiftMatchTypeArg (Indexed idx ty') = shift (nPatterns - idx) ty'

    checkPatternsNum :: Int -> [[a]] -> Int
    checkPatternsNum len = \case
      [] -> len
      ps : pats | length ps == len -> checkPatternsNum len pats
      _ ->
        error $
          "internal-to-core: all clauses must have the same number of patterns. Offending lambda at"
            <> ppPrint (getLoc ppLam)
            <> "\n"
            <> (ppLam ^. withLocParam)

    goClause :: Level -> [Internal.PatternArg] -> Internal.Expression -> Sem r MatchBranch
    goClause lvl pats body = goPatternArgs lvl (Internal.CaseBranchRhsExpression body) pats ptys
      where
        ptys :: [Type]
        ptys = take (length pats) (typeArgs ty)

goCase ::
  forall r.
  (Members '[InfoTableBuilder, Reader InternalTyped.TypesTable, Reader InternalTyped.FunctionsTable, Reader Internal.InfoTable, Reader IndexTable, NameIdGen, Error BadScope] r) =>
  Internal.Case ->
  Sem r Node
goCase c = do
  let loc = getLoc c
      i = setInfoLocation loc mempty
  expr <- goExpression (c ^. Internal.caseExpression)
  ty <- goType (fromJust $ c ^. Internal.caseExpressionType)
  case ty of
    NTyp {} -> do
      branches <- toList <$> mapM (goCaseBranch ty) (c ^. Internal.caseBranches)
      rty <- goType (fromJust $ c ^. Internal.caseExpressionWholeType)
      return (mkMatch i (pure ty) rty (pure expr) branches)
    _ ->
      -- If the type of the value matched on is not an inductive type, then the
      -- case expression has one branch with a variable pattern.
      case c ^. Internal.caseBranches of
        Internal.CaseBranch {..} :| _ ->
          case _caseBranchPattern ^. Internal.patternArgPattern of
            Internal.PatternVariable name -> do
              vars <- asks (^. indexTableVars)
              varsNum <- asks (^. indexTableVarsNum)
              let vars' = addPatternVariableNames _caseBranchPattern varsNum vars
              rhs <-
                local
                  (set indexTableVars vars')
                  (underBinders 1 (goCaseBranchRhs _caseBranchRhs))
              case rhs of
                MatchBranchRhsExpression body ->
                  return $ mkLet i (Binder (name ^. nameText) (Just $ name ^. nameLoc) ty) expr body
                _ ->
                  impossible
            _ ->
              impossible
  where
    goCaseBranch :: Type -> Internal.CaseBranch -> Sem r MatchBranch
    goCaseBranch ty b = goPatternArgs 0 (b ^. Internal.caseBranchRhs) [b ^. Internal.caseBranchPattern] [ty]

goCaseBranchRhs ::
  forall r.
  (Members '[InfoTableBuilder, Reader InternalTyped.TypesTable, Reader InternalTyped.FunctionsTable, Reader Internal.InfoTable, Reader IndexTable, NameIdGen, Error BadScope] r) =>
  Internal.CaseBranchRhs ->
  Sem r MatchBranchRhs
goCaseBranchRhs = \case
  Internal.CaseBranchRhsExpression e -> MatchBranchRhsExpression <$> goExpression e
  Internal.CaseBranchRhsIf Internal.SideIfs {..} -> case _sideIfElse of
    Just elseBranch -> do
      branches <- toList <$> mapM goSideIfBranch _sideIfBranches
      elseBranch' <- goExpression elseBranch
      boolSym <- getBoolSymbol
      return $ MatchBranchRhsExpression $ mkIfs' boolSym branches elseBranch'
      where
        goSideIfBranch :: Internal.SideIfBranch -> Sem r (Node, Node)
        goSideIfBranch Internal.SideIfBranch {..} = do
          cond <- goExpression _sideIfBranchCondition
          body <- goExpression _sideIfBranchBody
          return (cond, body)
    Nothing -> do
      branches <- mapM goSideIfBranch _sideIfBranches
      return $ MatchBranchRhsIfs branches
      where
        goSideIfBranch :: Internal.SideIfBranch -> Sem r SideIfBranch
        goSideIfBranch Internal.SideIfBranch {..} = do
          cond <- goExpression _sideIfBranchCondition
          body <- goExpression _sideIfBranchBody
          return $
            SideIfBranch
              { _sideIfBranchInfo = setInfoLocation (getLoc _sideIfBranchCondition) mempty,
                _sideIfBranchCondition = cond,
                _sideIfBranchBody = body
              }

goLambda ::
  forall r.
  (Members '[InfoTableBuilder, Reader InternalTyped.TypesTable, Reader InternalTyped.FunctionsTable, Reader Internal.InfoTable, Reader IndexTable, NameIdGen, Error BadScope] r) =>
  Internal.Lambda ->
  Sem r Node
goLambda l = do
  ty <- goType (fromJust (l ^. Internal.lambdaType))
  mkBody (WithLoc (getLoc l) (ppPrint l)) ty (getLoc l) (fmap (\c -> (toList (c ^. Internal.lambdaPatterns), c ^. Internal.lambdaBody)) (l ^. Internal.lambdaClauses))

goLet ::
  forall r.
  (Members '[InfoTableBuilder, Reader InternalTyped.TypesTable, Reader InternalTyped.FunctionsTable, Reader Internal.InfoTable, Reader IndexTable, NameIdGen, Error BadScope] r) =>
  Internal.Let ->
  Sem r Node
goLet l = goClauses (toList (l ^. Internal.letClauses))
  where
    goClauses :: [Internal.LetClause] -> Sem r Node
    goClauses = \case
      [] -> goExpression (l ^. Internal.letExpression)
      c : cs -> case c of
        Internal.LetFunDef f -> goNonRecFun f
        Internal.LetMutualBlock m -> goMutual m
        where
          goNonRecFun :: Internal.FunctionDef -> Sem r Node
          goNonRecFun f =
            do
              funTy <- goType (f ^. Internal.funDefType)
              funBody <- mkFunBody funTy f
              rest <- localAddName (f ^. Internal.funDefName) (goClauses cs)
              let implArgs = computeImplicitArgs (f ^. Internal.funDefType)
                  name = f ^. Internal.funDefName . nameText
                  loc = f ^. Internal.funDefName . nameLoc
                  pragmas = adjustPragmas' implArgs (f ^. Internal.funDefPragmas)
                  info = setInfoPragma pragmas mempty
                  body = modifyInfo (setInfoPragma pragmas) funBody
              return $ mkLet info (Binder name (Just loc) funTy) body rest

          goMutual :: Internal.MutualBlockLet -> Sem r Node
          goMutual (Internal.MutualBlockLet funs) = do
            let lfuns = toList funs
                names = map (^. Internal.funDefName) lfuns
                tys = map (^. Internal.funDefType) lfuns
                implArgs = map (computeImplicitArgs . (^. Internal.funDefType)) lfuns
                pragmas = zipWith adjustPragmas' implArgs (map (^. Internal.funDefPragmas) lfuns)
            tys' <- mapM goType tys
            localAddNames names $ do
              vals' <- sequence [mkFunBody (shift (length names) ty) f | (ty, f) <- zipExact tys' lfuns]
              let items = nonEmpty' (zipWith3Exact (\ty n v -> LetItem (Binder (n ^. nameText) (Just $ n ^. nameLoc) ty) v) tys' names vals')
              rest <- goClauses cs
              return (mkLetRec (setInfoPragmas pragmas mempty) items rest)

builtinInductive :: Internal.AxiomDef -> Maybe (forall r. (Members '[InfoTableBuilder] r) => Sem r ())
builtinInductive a =
  case a ^. Internal.axiomBuiltin of
    Nothing -> Nothing
    Just b ->
      case b of
        Internal.BuiltinNatPrint -> Nothing
        Internal.BuiltinStringPrint -> Nothing
        Internal.BuiltinBoolPrint -> Nothing
        Internal.BuiltinIOSequence -> Nothing
        Internal.BuiltinIOReadline -> Nothing
        Internal.BuiltinField -> Just (registerInductiveAxiom (Just BuiltinField) [])
        Internal.BuiltinString -> Just (registerInductiveAxiom (Just BuiltinString) [])
        Internal.BuiltinIO -> Just (registerInductiveAxiom (Just BuiltinIO) builtinIOConstrs)
        Internal.BuiltinTrace -> Nothing
        Internal.BuiltinFail -> Nothing
        Internal.BuiltinStringConcat -> Nothing
        Internal.BuiltinStringEq -> Nothing
        Internal.BuiltinStringToNat -> Nothing
        Internal.BuiltinNatToString -> Nothing
        Internal.BuiltinIntToString -> Nothing
        Internal.BuiltinIntPrint -> Nothing
        Internal.BuiltinFieldEq -> Nothing
        Internal.BuiltinFieldAdd -> Nothing
        Internal.BuiltinFieldSub -> Nothing
        Internal.BuiltinFieldMul -> Nothing
        Internal.BuiltinFieldDiv -> Nothing
        Internal.BuiltinFieldFromInt -> Nothing
        Internal.BuiltinFieldToNat -> Nothing
        Internal.BuiltinAnomaGet -> Nothing
        Internal.BuiltinAnomaEncode -> Nothing
        Internal.BuiltinAnomaDecode -> Nothing
        Internal.BuiltinAnomaVerifyDetached -> Nothing
        Internal.BuiltinAnomaSign -> Nothing
        Internal.BuiltinAnomaSignDetached -> Nothing
        Internal.BuiltinAnomaVerifyWithMessage -> Nothing
        Internal.BuiltinAnomaByteArrayToAnomaContents -> Nothing
        Internal.BuiltinAnomaByteArrayFromAnomaContents -> Nothing
        Internal.BuiltinAnomaSha256 -> Nothing
        Internal.BuiltinAnomaDelta -> Just (registerInductiveAxiom (Just BuiltinAnomaDelta) [])
        Internal.BuiltinAnomaKind -> Just (registerInductiveAxiom (Just BuiltinAnomaKind) [])
        Internal.BuiltinAnomaResourceCommitment -> Nothing
        Internal.BuiltinAnomaResourceNullifier -> Nothing
        Internal.BuiltinAnomaResourceDelta -> Nothing
        Internal.BuiltinAnomaResourceKind -> Nothing
        Internal.BuiltinAnomaActionDelta -> Nothing
        Internal.BuiltinAnomaActionsDelta -> Nothing
        Internal.BuiltinAnomaProveDelta -> Nothing
        Internal.BuiltinAnomaProveAction -> Nothing
        Internal.BuiltinAnomaZeroDelta -> Nothing
        Internal.BuiltinAnomaAddDelta -> Nothing
        Internal.BuiltinAnomaSubDelta -> Nothing
        Internal.BuiltinAnomaRandomGenerator -> Just (registerInductiveAxiom (Just BuiltinAnomaRandomGenerator) [])
        Internal.BuiltinAnomaRandomGeneratorInit -> Nothing
        Internal.BuiltinAnomaRandomNextBytes -> Nothing
        Internal.BuiltinAnomaRandomSplit -> Nothing
        Internal.BuiltinAnomaIsCommitment -> Nothing
        Internal.BuiltinAnomaIsNullifier -> Nothing
        Internal.BuiltinAnomaSet -> Just (registerInductiveAxiom (Just BuiltinAnomaSet) [])
        Internal.BuiltinAnomaSetToList -> Nothing
        Internal.BuiltinAnomaSetFromList -> Nothing
        Internal.BuiltinPoseidon -> Nothing
        Internal.BuiltinEcOp -> Nothing
        Internal.BuiltinRandomEcPoint -> Nothing
        Internal.BuiltinByte -> Just (registerInductiveAxiom (Just BuiltinByte) [])
        Internal.BuiltinByteEq -> Nothing
        Internal.BuiltinByteToNat -> Nothing
        Internal.BuiltinByteFromNat -> Nothing
        Internal.BuiltinByteArray -> Just (registerInductiveAxiom (Just BuiltinByteArray) [])
        Internal.BuiltinByteArrayFromListByte -> Nothing
        Internal.BuiltinByteArrayLength -> Nothing
  where
    registerInductiveAxiom :: forall r. (Members '[InfoTableBuilder] r) => Maybe BuiltinAxiom -> [(Tag, Text, Type -> Type, Maybe BuiltinConstructor)] -> Sem r ()
    registerInductiveAxiom ax ctrs = do
      sym <- freshSymbol
      let name = a ^. Internal.axiomName . nameText
          ty = mkTypeConstr (setInfoName name mempty) sym []
          ctrs' = builtinConstrs sym ty ctrs
      let _inductiveName = a ^. Internal.axiomName . nameText
          info =
            InductiveInfo
              { _inductiveLocation = Just $ a ^. Internal.axiomName . nameLoc,
                _inductiveSymbol = sym,
                _inductiveKind = mkSmallUniv,
                _inductiveConstructors = map (^. constructorTag) ctrs',
                _inductiveParams = [],
                _inductivePositive = False,
                _inductiveBuiltin = BuiltinTypeAxiom <$> ax,
                _inductivePragmas = a ^. Internal.axiomPragmas,
                _inductiveName
              }
      registerInductive (mkIdentIndex (a ^. Internal.axiomName)) info
      mapM_ (\ci -> registerConstructor (ci ^. constructorName) ci) ctrs'

goAxiomInductive ::
  forall r.
  (Members '[InfoTableBuilder, Reader InternalTyped.TypesTable, Reader InternalTyped.FunctionsTable, Reader Internal.InfoTable, NameIdGen] r) =>
  Internal.AxiomDef ->
  Sem r ()
goAxiomInductive a = case builtinInductive a of
  Nothing -> return ()
  Just m -> m

fromTopIndex :: Sem (Reader IndexTable ': r) a -> Sem r a
fromTopIndex = runReader initIndexTable

goAxiomDef ::
  forall r.
  (Members '[InfoTableBuilder, Reader InternalTyped.TypesTable, Reader InternalTyped.FunctionsTable, Reader Internal.InfoTable, NameIdGen, Error BadScope] r) =>
  Internal.AxiomDef ->
  Sem r ()
goAxiomDef a = maybe goAxiomNotBuiltin builtinBody (a ^. Internal.axiomBuiltin)
  where
    goAxiomNotBuiltin :: Sem r ()
    goAxiomNotBuiltin = axiomType' >>= registerAxiomDef . mkAxiom (getLoc a)

    builtinBody :: Internal.BuiltinAxiom -> Sem r ()
    builtinBody = \case
      Internal.BuiltinNatPrint -> do
        natName <- getNatName
        natSym <- getNatSymbol
        registerAxiomDef $ writeLambda (mkTypeConstr (setInfoName natName mempty) natSym [])
      Internal.BuiltinStringPrint -> registerAxiomDef $ writeLambda mkTypeString'
      Internal.BuiltinBoolPrint -> registerAxiomDef $ writeLambda mkTypeBool'
      Internal.BuiltinIOSequence -> return ()
      Internal.BuiltinIOReadline ->
        registerAxiomDef
          ( mkLambda'
              mkTypeString'
              ( mkConstr'
                  (BuiltinTag TagBind)
                  [ mkConstr' (BuiltinTag TagReadLn) [],
                    mkVar' 0
                  ]
              )
          )
      Internal.BuiltinStringConcat ->
        registerAxiomDef (mkLambda' mkTypeString' (mkLambda' mkTypeString' (mkBuiltinApp' OpStrConcat [mkVar' 1, mkVar' 0])))
      Internal.BuiltinStringEq ->
        registerAxiomDef (mkLambda' mkTypeString' (mkLambda' mkTypeString' (mkBuiltinApp' OpEq [mkVar' 1, mkVar' 0])))
      Internal.BuiltinStringToNat -> do
        boolSym <- getBoolSymbol
        registerAxiomDef
          ( mkLambda'
              mkTypeString'
              ( mkLet'
                  mkTypeInteger'
                  (mkBuiltinApp' OpStrToInt [mkVar' 0])
                  ( mkIf'
                      boolSym
                      (mkBuiltinApp' OpIntLt [mkVar' 0, mkConstant' (ConstInteger 0)])
                      (mkBuiltinApp' OpFail [mkConstant' (ConstString "stringToNat: negative value")])
                      (mkVar' 0)
                  )
              )
          )
      Internal.BuiltinNatToString -> do
        natName <- getNatName
        natSym <- getNatSymbol
        registerAxiomDef (mkLambda' (mkTypeConstr (setInfoName natName mempty) natSym []) (mkBuiltinApp' OpShow [mkVar' 0]))
      Internal.BuiltinField -> return ()
      Internal.BuiltinFieldEq ->
        registerAxiomDef (mkLambda' mkTypeField' (mkLambda' mkTypeField' (mkBuiltinApp' OpEq [mkVar' 1, mkVar' 0])))
      Internal.BuiltinFieldAdd ->
        registerAxiomDef (mkLambda' mkTypeField' (mkLambda' mkTypeField' (mkBuiltinApp' OpFieldAdd [mkVar' 1, mkVar' 0])))
      Internal.BuiltinFieldSub ->
        registerAxiomDef (mkLambda' mkTypeField' (mkLambda' mkTypeField' (mkBuiltinApp' OpFieldSub [mkVar' 1, mkVar' 0])))
      Internal.BuiltinFieldMul ->
        registerAxiomDef (mkLambda' mkTypeField' (mkLambda' mkTypeField' (mkBuiltinApp' OpFieldMul [mkVar' 1, mkVar' 0])))
      Internal.BuiltinFieldDiv ->
        registerAxiomDef (mkLambda' mkTypeField' (mkLambda' mkTypeField' (mkBuiltinApp' OpFieldDiv [mkVar' 1, mkVar' 0])))
      Internal.BuiltinFieldFromInt ->
        registerAxiomDef (mkLambda' mkTypeInteger' (mkBuiltinApp' OpFieldFromInt [mkVar' 0]))
      Internal.BuiltinFieldToNat ->
        registerAxiomDef (mkLambda' mkTypeField' (mkBuiltinApp' OpFieldToInt [mkVar' 0]))
      Internal.BuiltinString -> return ()
      Internal.BuiltinIO -> return ()
      Internal.BuiltinTrace ->
        registerAxiomDef (mkLambda' mkSmallUniv (mkLambda' (mkVar' 0) (mkBuiltinApp' OpTrace [mkVar' 0])))
      Internal.BuiltinFail ->
        registerAxiomDef (mkLambda' mkSmallUniv (mkLambda' (mkVar' 0) (mkBuiltinApp' OpFail [mkVar' 0])))
      Internal.BuiltinIntToString -> do
        intName <- getIntName
        intSym <- getIntSymbol
        registerAxiomDef (mkLambda' (mkTypeConstr (setInfoName intName mempty) intSym []) (mkBuiltinApp' OpShow [mkVar' 0]))
      Internal.BuiltinIntPrint -> do
        intName <- getIntName
        intSym <- getIntSymbol
        registerAxiomDef $ writeLambda (mkTypeConstr (setInfoName intName mempty) intSym [])
      Internal.BuiltinAnomaGet ->
        registerAxiomDef
          ( mkLambda'
              mkSmallUniv
              ( mkLambda'
                  mkSmallUniv
                  (mkLambda' (mkVar' 0) (mkBuiltinApp' OpAnomaGet [mkVar' 0]))
              )
          )
      Internal.BuiltinAnomaEncode ->
        registerAxiomDef
          ( mkLambda'
              mkSmallUniv
              (mkLambda' (mkVar' 0) (mkBuiltinApp' OpAnomaEncode [mkVar' 0]))
          )
      Internal.BuiltinAnomaDecode -> do
        natType <- getNatType
        registerAxiomDef
          ( mkLambda'
              mkSmallUniv
              (mkLambda' natType (mkBuiltinApp' OpAnomaDecode [mkVar' 0]))
          )
      Internal.BuiltinAnomaVerifyDetached -> do
        natType <- getNatType
        registerAxiomDef
          ( mkLambda'
              mkSmallUniv
              ( mkLambda'
                  natType
                  ( mkLambda'
                      (mkVar' 0)
                      ( mkLambda'
                          natType
                          (mkBuiltinApp' OpAnomaVerifyDetached [mkVar' 2, mkVar' 1, mkVar' 0])
                      )
                  )
              )
          )
      Internal.BuiltinAnomaSign -> do
        natType <- getNatType
        registerAxiomDef
          ( mkLambda'
              mkSmallUniv
              ( mkLambda'
                  (mkVar' 0)
                  ( mkLambda'
                      natType
                      (mkBuiltinApp' OpAnomaSign [mkVar' 1, mkVar' 0])
                  )
              )
          )
      Internal.BuiltinAnomaSignDetached -> do
        natType <- getNatType
        registerAxiomDef
          ( mkLambda'
              mkSmallUniv
              ( mkLambda'
                  (mkVar' 0)
                  ( mkLambda'
                      natType
                      (mkBuiltinApp' OpAnomaSignDetached [mkVar' 1, mkVar' 0])
                  )
              )
          )
      Internal.BuiltinAnomaVerifyWithMessage -> do
        natType <- getNatType
        registerAxiomDef
          ( mkLambda'
              mkSmallUniv
              ( mkLambda'
                  natType
                  ( mkLambda'
                      natType
                      (mkBuiltinApp' OpAnomaVerifyWithMessage [mkVar' 1, mkVar' 0])
                  )
              )
          )
      Internal.BuiltinAnomaByteArrayToAnomaContents ->
        registerAxiomDef (mkLambda' mkDynamic' (mkBuiltinApp' OpAnomaByteArrayToAnomaContents [mkVar' 0]))
      Internal.BuiltinAnomaByteArrayFromAnomaContents -> do
        natType <- getNatType
        registerAxiomDef
          ( mkLambda'
              natType
              ( mkLambda'
                  natType
                  (mkBuiltinApp' OpAnomaByteArrayFromAnomaContents [mkVar' 1, mkVar' 0])
              )
          )
      Internal.BuiltinAnomaSha256 -> do
        natType <- getNatType
        registerAxiomDef
          ( mkLambda'
              natType
              (mkBuiltinApp' OpAnomaSha256 [mkVar' 0])
          )
      Internal.BuiltinAnomaDelta -> return ()
      Internal.BuiltinAnomaKind -> return ()
      Internal.BuiltinAnomaResourceCommitment -> do
        resourceType <- getAnomaResourceType
        registerAxiomDef
          ( mkLambda'
              resourceType
              (mkBuiltinApp' OpAnomaResourceCommitment [mkVar' 0])
          )
      Internal.BuiltinAnomaResourceNullifier -> do
        resourceType <- getAnomaResourceType
        registerAxiomDef
          ( mkLambda'
              resourceType
              (mkBuiltinApp' OpAnomaResourceNullifier [mkVar' 0])
          )
      Internal.BuiltinAnomaResourceKind -> do
        resourceType <- getAnomaResourceType
        registerAxiomDef
          ( mkLambda'
              resourceType
              (mkBuiltinApp' OpAnomaResourceKind [mkVar' 0])
          )
      Internal.BuiltinAnomaResourceDelta -> do
        resourceType <- getAnomaResourceType
        registerAxiomDef
          ( mkLambda'
              resourceType
              (mkBuiltinApp' OpAnomaResourceDelta [mkVar' 0])
          )
      Internal.BuiltinAnomaActionDelta -> do
        actionType <- getAnomaActionType
        registerAxiomDef
          ( mkLambda'
              actionType
              (mkBuiltinApp' OpAnomaActionDelta [mkVar' 0])
          )
      Internal.BuiltinAnomaActionsDelta -> do
        registerAxiomDef
          ( mkLambda'
              mkDynamic'
              (mkBuiltinApp' OpAnomaActionsDelta [mkVar' 0])
          )
      Internal.BuiltinAnomaProveAction -> do
        actionType <- getAnomaActionType
        registerAxiomDef
          ( mkLambda'
              actionType
              (mkBuiltinApp' OpAnomaProveAction [mkVar' 0])
          )
      Internal.BuiltinAnomaProveDelta -> do
        registerAxiomDef
          ( mkLambda'
              mkDynamic'
              (mkBuiltinApp' OpAnomaProveDelta [mkVar' 0])
          )
      Internal.BuiltinAnomaZeroDelta -> do
        registerAxiomDef (mkBuiltinApp' OpAnomaZeroDelta [])
      Internal.BuiltinAnomaAddDelta -> do
        registerAxiomDef
          ( mkLambda'
              mkDynamic'
              ( mkLambda'
                  mkDynamic'
                  (mkBuiltinApp' OpAnomaAddDelta [mkVar' 1, mkVar' 0])
              )
          )
      Internal.BuiltinAnomaSubDelta -> do
        registerAxiomDef
          ( mkLambda'
              mkDynamic'
              ( mkLambda'
                  mkDynamic'
                  (mkBuiltinApp' OpAnomaSubDelta [mkVar' 1, mkVar' 0])
              )
          )
      Internal.BuiltinAnomaRandomGenerator -> return ()
      Internal.BuiltinAnomaRandomGeneratorInit -> do
        natType <- getNatType
        registerAxiomDef
          ( mkLambda'
              natType
              (mkBuiltinApp' OpAnomaRandomGeneratorInit [mkVar' 0])
          )
      Internal.BuiltinAnomaRandomNextBytes -> do
        natType <- getNatType
        registerAxiomDef
          ( mkLambda'
              natType
              ( mkLambda'
                  mkDynamic'
                  (mkBuiltinApp' OpAnomaRandomNextBytes [mkVar' 1, mkVar' 0])
              )
          )
      Internal.BuiltinAnomaRandomSplit -> do
        registerAxiomDef
          ( mkLambda'
              mkDynamic'
              (mkBuiltinApp' OpAnomaRandomSplit [mkVar' 0])
          )
      Internal.BuiltinAnomaIsCommitment -> do
        natType <- getNatType
        registerAxiomDef
          ( mkLambda'
              natType
              (mkBuiltinApp' OpAnomaIsCommitment [mkVar' 0])
          )
      Internal.BuiltinAnomaIsNullifier -> do
        natType <- getNatType
        registerAxiomDef
          ( mkLambda'
              natType
              (mkBuiltinApp' OpAnomaIsNullifier [mkVar' 0])
          )
      Internal.BuiltinAnomaSet -> return ()
      Internal.BuiltinAnomaSetToList -> do
        registerAxiomDef
          ( mkLambda'
              mkSmallUniv
              ( mkLambda'
                  mkDynamic'
                  (mkBuiltinApp' OpAnomaSetToList [mkVar' 0])
              )
          )
      Internal.BuiltinAnomaSetFromList -> do
        registerAxiomDef
          ( mkLambda'
              mkSmallUniv
              ( mkLambda'
                  mkDynamic'
                  (mkBuiltinApp' OpAnomaSetFromList [mkVar' 0])
              )
          )
      Internal.BuiltinPoseidon -> do
        psName <- getPoseidonStateName
        psSym <- getPoseidonStateSymbol
        let ty = mkTypeConstr (setInfoName psName mempty) psSym []
        registerAxiomDef (mkLambda' ty (mkBuiltinApp' OpPoseidonHash [mkVar' 0]))
      Internal.BuiltinEcOp -> do
        ptName <- getEcPointName
        ptSym <- getEcPointSymbol
        let ty = mkTypeConstr (setInfoName ptName mempty) ptSym []
        registerAxiomDef
          . mkLambda' ty
          . mkLambda' mkTypeField'
          . mkLambda' ty
          $ mkBuiltinApp' OpEc [mkVar' 2, mkVar' 1, mkVar' 0]
      Internal.BuiltinRandomEcPoint -> do
        registerAxiomDef (mkBuiltinApp' OpRandomEcPoint [])
      Internal.BuiltinByte -> return ()
      Internal.BuiltinByteEq ->
        registerAxiomDef (mkLambda' mkTypeUInt8' (mkLambda' mkTypeUInt8' (mkBuiltinApp' OpEq [mkVar' 1, mkVar' 0])))
      Internal.BuiltinByteToNat ->
        registerAxiomDef (mkLambda' mkTypeUInt8' (mkBuiltinApp' OpUInt8ToInt [mkVar' 0]))
      Internal.BuiltinByteFromNat ->
        registerAxiomDef (mkLambda' mkTypeInteger' (mkBuiltinApp' OpUInt8FromInt [mkVar' 0]))
      Internal.BuiltinByteArray -> return ()
      Internal.BuiltinByteArrayFromListByte ->
        registerAxiomDef (mkLambda' mkDynamic' (mkBuiltinApp' OpByteArrayFromListByte [mkVar' 0]))
      Internal.BuiltinByteArrayLength ->
        registerAxiomDef (mkLambda' mkTypeInteger' (mkBuiltinApp' OpByteArrayLength [mkVar' 0]))

    axiomType' :: Sem r Type
    axiomType' = fromTopIndex (goType (a ^. Internal.axiomType))

    writeLambda :: Type -> Node
    writeLambda ty = mkLambda' ty (mkConstr' (BuiltinTag TagWrite) [mkVar' 0])

    getNatName :: Sem r Text
    getNatName = (^. inductiveName) <$> getBuiltinInductiveInfo BuiltinNat

    getNatType :: Sem r Type
    getNatType = do
      natName <- getNatName
      natSym <- getNatSymbol
      return (mkTypeConstr (setInfoName natName mempty) natSym [])

    getIntName :: Sem r Text
    getIntName = (^. inductiveName) <$> getBuiltinInductiveInfo BuiltinInt

    getPoseidonStateName :: Sem r Text
    getPoseidonStateName = (^. inductiveName) <$> getBuiltinInductiveInfo BuiltinPoseidonState

    getEcPointName :: Sem r Text
    getEcPointName = (^. inductiveName) <$> getBuiltinInductiveInfo BuiltinEcPoint

    getAnomaResourceName :: Sem r Text
    getAnomaResourceName = (^. inductiveName) <$> getBuiltinInductiveInfo BuiltinAnomaResource

    getAnomaActionName :: Sem r Text
    getAnomaActionName = (^. inductiveName) <$> getBuiltinInductiveInfo BuiltinAnomaAction

    getAnomaResourceType :: Sem r Type
    getAnomaResourceType = do
      resourceName <- getAnomaResourceName
      resourceSymbol <- getAnomaResourceSymbol
      return (mkTypeConstr (setInfoName resourceName mempty) resourceSymbol [])

    getAnomaActionType :: Sem r Type
    getAnomaActionType = do
      actionName <- getAnomaActionName
      actionSymbol <- getAnomaActionSymbol
      return (mkTypeConstr (setInfoName actionName mempty) actionSymbol [])

    registerAxiomDef :: Node -> Sem r ()
    registerAxiomDef body = do
      let name = a ^. Internal.axiomName
      sym <- freshSymbol
      ty <- axiomType'
      let _identifierName = name ^. nameText
          info =
            IdentifierInfo
              { _identifierLocation = Just $ name ^. nameLoc,
                _identifierSymbol = sym,
                _identifierType = ty,
                _identifierArgsNum = 0,
                _identifierIsExported = False,
                _identifierBuiltin = Nothing,
                _identifierPragmas = a ^. Internal.axiomPragmas,
                _identifierArgNames = [],
                _identifierName
              }
      registerIdent (mkIdentIndex name) info
      registerIdentNode sym body
      let (is, _) = unfoldLambdas body
      setIdentArgs sym (map (^. lambdaLhsBinder) is)

fromPatternArg ::
  forall r.
  (Members '[InfoTableBuilder, Reader InternalTyped.TypesTable, Reader InternalTyped.FunctionsTable, Reader Internal.InfoTable, State IndexTable, NameIdGen, Error BadScope] r) =>
  Internal.PatternArg ->
  Sem r Pattern
fromPatternArg pa = case pa ^. Internal.patternArgName of
  Just pan -> do
    ty <- getPatternType pan
    subPat $ Just (pan, ty)
  Nothing ->
    subPat Nothing
  where
    subPat :: Maybe (Name, Type) -> Sem r Pattern
    subPat asPat = fromPattern asPat (pa ^. Internal.patternArgPattern)

    getPatternType :: Name -> Sem r Type
    getPatternType n = do
      ty <- asks (fromJust . HashMap.lookup (n ^. nameId) . (^. InternalTyped.typesTable))
      idt :: IndexTable <- get
      runReader idt (goType ty)

    fromPattern :: Maybe (Name, Type) -> Internal.Pattern -> Sem r Pattern
    fromPattern asPat = \case
      Internal.PatternWildcardConstructor {} -> impossible
      Internal.PatternVariable n -> do
        ty <- getPatternType n
        varsNum <- (^. indexTableVarsNum) <$> get
        modify
          ( over indexTableVarsNum (+ 1)
              . over indexTableVars (HashMap.insert (n ^. nameId) varsNum)
          )
        case asPat of
          Just (pan, _) -> modify (over indexTableVars (HashMap.insert (pan ^. nameId) varsNum))
          _ -> return ()
        return $ PatWildcard (PatternWildcard mempty (Binder (n ^. nameText) (Just (n ^. nameLoc)) ty))
      Internal.PatternConstructorApp c -> do
        idt :: IndexTable <- get
        ctorTy <- runReader idt $ goType (fromJust (c ^. Internal.constrAppType))
        let varsNum = idt ^. indexTableVarsNum
        case asPat of
          Just (pan, _) -> modify (over indexTableVars (HashMap.insert (pan ^. nameId) varsNum))
          _ -> return ()
        (indParams, _) <- InternalTyped.lookupConstructorArgTypes ctrName
        let nParams = length indParams
        -- + 1 for the as-pattern
        modify (over indexTableVarsNum (+ (nParams + 1)))
        patternArgs <- mapM fromPatternArg params
        let indArgs = replicate nParams (wildcard mkSmallUniv)
            args = indArgs ++ patternArgs
        m <- getIdent identIndex
        case m of
          Just (IdentConstr tag) ->
            return $
              PatConstr
                PatternConstr
                  { _patternConstrInfo = setInfoName (ctrName ^. nameText) mempty,
                    _patternConstrFixity = ctrName ^. nameFixity,
                    _patternConstrBinder = binder ctorTy,
                    _patternConstrTag = tag,
                    _patternConstrArgs = args
                  }
          Just _ -> error ("internal to core: not a constructor " <> txt)
          Nothing -> error ("internal to core: undeclared identifier: " <> txt)
        where
          ctrName :: Name
          ctrName = c ^. Internal.constrAppConstructor

          params :: [Internal.PatternArg]
          params = (c ^. Internal.constrAppParameters)

          identIndex :: Text
          identIndex = mkIdentIndex (c ^. Internal.constrAppConstructor)

          binder :: Type -> Binder
          binder ctorTy = case asPat of
            Just (pan, ty) -> Binder (pan ^. nameText) (Just (pan ^. nameLoc)) ty
            _ -> Binder "_" Nothing ctorTy

          txt :: Text
          txt = c ^. Internal.constrAppConstructor . Internal.nameText

          wildcard :: Type -> Pattern
          wildcard ty = PatWildcard (PatternWildcard Info.empty (Binder "_" Nothing ty))

goPatternArgs ::
  forall r.
  (Members '[InfoTableBuilder, Reader InternalTyped.TypesTable, Reader InternalTyped.FunctionsTable, Reader Internal.InfoTable, Reader IndexTable, NameIdGen, Error BadScope] r) =>
  Level -> -- the level of the first binder for the matched value
  Internal.CaseBranchRhs ->
  [Internal.PatternArg] ->
  [Type] -> -- types of the patterns
  Sem r MatchBranch
goPatternArgs lvl0 body pats0 = go lvl0 [] pats0
  where
    -- `lvl` is the level of the lambda-bound variable corresponding to the current pattern
    go :: Level -> [Pattern] -> [Internal.PatternArg] -> [Type] -> Sem r MatchBranch
    go lvl pats ps ptys = case (ps, ptys) of
      -- The pattern has an inductive type, so can be matched on
      (p : ps', NTyp {} : ptys') -> do
        itb :: IndexTable <- ask
        (itb', pat) <- runState itb (fromPatternArg p)
        local
          (const itb')
          (go (lvl + 1) (pat : pats) ps' ptys')
      (p : ps', _ : ptys') ->
        -- The pattern does not have an inductive type, so is excluded from the match
        case p ^. Internal.patternArgPattern of
          Internal.PatternVariable {} -> do
            vars <- asks (^. indexTableVars)
            let vars' = addPatternVariableNames p lvl vars
            local
              (set indexTableVars vars')
              (go (lvl + 1) pats ps' ptys')
          _ ->
            impossible
      ([], []) -> do
        body' <- goCaseBranchRhs body
        let info = setInfoLocation (getLocSpan (nonEmpty' pats0)) Info.empty
        return $ MatchBranch info (nonEmpty' (reverse pats)) body'
      _ ->
        impossible

addPatternVariableNames ::
  Internal.PatternArg ->
  Level ->
  HashMap NameId Level ->
  HashMap NameId Level
addPatternVariableNames p lvl vars =
  foldl'
    (\vs -> maybe vs (\name -> HashMap.insert (name ^. nameId) lvl vs))
    vars
    (getPatternArgVars p)
  where
    getPatternArgVars :: Internal.PatternArg -> [Maybe Name]
    getPatternArgVars pa =
      pa ^. Internal.patternArgName : getPatternVars (pa ^. Internal.patternArgPattern)

    getPatternVars :: Internal.Pattern -> [Maybe Name]
    getPatternVars = \case
      Internal.PatternVariable n -> [Just n]
      Internal.PatternConstructorApp {} -> impossible
      Internal.PatternWildcardConstructor {} -> impossible

goIden ::
  forall r.
  (Members '[InfoTableBuilder, Reader InternalTyped.TypesTable, Reader InternalTyped.FunctionsTable, Reader Internal.InfoTable, Reader IndexTable, Error BadScope] r) =>
  Internal.Iden ->
  Sem r Node
goIden i = do
  let undeclared =
        error
          ( "internal to core: undeclared identifier: "
              <> txt
              <> "\nat "
              <> Internal.ppTrace (getLoc i)
          )
  case i of
    Internal.IdenVar n -> do
      let err = throw (BadScope n)
      k <- fromMaybeM err (HashMap.lookup id_ <$> asks (^. indexTableVars))
      varsNum <- asks (^. indexTableVarsNum)
      return (mkVar (setInfoLocation (n ^. nameLoc) (Info.singleton (NameInfo (n ^. nameText)))) (varsNum - k - 1))
    Internal.IdenFunction n -> do
      funInfoBuiltin <- Internal.getFunctionBuiltinInfo n
      case funInfoBuiltin of
        Just Internal.BuiltinBoolIf -> error "internal to core: if must be called with 3 arguments"
        Just Internal.BuiltinBoolOr -> error "internal to core: || must be called with 2 arguments"
        Just Internal.BuiltinBoolAnd -> error "internal to core: && must be called with 2 arguments"
        Just Internal.BuiltinSeq -> error "internal to core: seq must be called with 2 arguments"
        _ -> return ()
      -- if the function was defined by a let, then in Core it is stored in a variable
      vars <- asks (^. indexTableVars)
      case HashMap.lookup id_ vars of
        Nothing -> do
          m <- getIdent identIndex
          return $ case m of
            Just (IdentFun sym) -> mkIdent (setInfoLocation (n ^. nameLoc) (Info.singleton (NameInfo (n ^. namePretty)))) sym
            Just _ -> error ("internal to core: not a function: " <> txt)
            Nothing -> undeclared
        Just k -> do
          varsNum <- asks (^. indexTableVarsNum)
          return (mkVar (setInfoLocation (n ^. nameLoc) (Info.singleton (NameInfo (n ^. nameText)))) (varsNum - k - 1))
    Internal.IdenInductive n -> do
      m <- getIdent identIndex
      return $ case m of
        Just (IdentInd sym) -> mkTypeConstr (setInfoLocation (n ^. nameLoc) (Info.singleton (NameInfo (n ^. namePretty)))) sym []
        Just _ -> error ("internal to core: not an inductive: " <> txt)
        Nothing -> undeclared
    Internal.IdenConstructor n -> do
      m <- getIdent identIndex
      return $ case m of
        Just (IdentConstr tag) -> mkConstr (setInfoLocation (n ^. nameLoc) (Info.singleton (NameInfo (n ^. namePretty)))) tag []
        Just _ -> error ("internal to core: not a constructor " <> txt)
        Nothing -> undeclared
    Internal.IdenAxiom n -> do
      axiomInfoBuiltin <- Internal.getAxiomBuiltinInfo n
      case axiomInfoBuiltin of
        Just Internal.BuiltinIOSequence -> error "internal to core: >> must be called with 2 arguments"
        _ -> return ()
      m <- getIdent identIndex
      return $ case m of
        Just (IdentFun sym) -> mkIdent (setInfoLocation (n ^. nameLoc) (Info.singleton (NameInfo (n ^. namePretty)))) sym
        Just (IdentInd sym) -> mkTypeConstr (setInfoLocation (n ^. nameLoc) (Info.singleton (NameInfo (n ^. namePretty)))) sym []
        Just _ -> error ("internal to core: not an axiom: " <> txt)
        Nothing -> undeclared
  where
    identIndex :: Text
    identIndex = mkIdentIndex (Internal.getName i)

    id_ :: NameId
    id_ = Internal.getName i ^. nameId

    txt :: Text
    txt = Internal.ppTrace (Internal.getName i)

goExpression ::
  forall r.
  (Members '[InfoTableBuilder, Reader InternalTyped.TypesTable, Reader InternalTyped.FunctionsTable, Reader Internal.InfoTable, Reader IndexTable, NameIdGen, Error BadScope] r) =>
  Internal.Expression ->
  Sem r Node
goExpression = \case
  Internal.ExpressionLet l -> goLet l
  Internal.ExpressionLiteral l -> do
    md <- getModule
    return (goLiteral (fromJust $ getInfoLiteralIntToNat md) (fromJust $ getInfoLiteralIntToInt md) l)
  Internal.ExpressionIden i -> goIden i
  Internal.ExpressionApplication a -> goApplication a
  Internal.ExpressionSimpleLambda l -> goSimpleLambda l
  Internal.ExpressionLambda l -> goLambda l
  Internal.ExpressionCase l -> goCase l
  e@Internal.ExpressionFunction {} -> goFunction (Internal.unfoldFunType e)
  Internal.ExpressionHole h -> error ("internal to core: goExpression hole: " <> show (Loc.getLoc h))
  Internal.ExpressionInstanceHole h -> error ("internal to core: goExpression instance hole: " <> show (Loc.getLoc h))
  Internal.ExpressionUniverse {} -> return mkSmallUniv

goFunction ::
  forall r.
  (Members '[InfoTableBuilder, Reader InternalTyped.TypesTable, Reader InternalTyped.FunctionsTable, Reader Internal.InfoTable, Reader IndexTable, NameIdGen, Error BadScope] r) =>
  ([Internal.FunctionParameter], Internal.Expression) ->
  Sem r Node
goFunction (params, returnTypeExpr) = go params
  where
    go :: [Internal.FunctionParameter] -> Sem r Node
    go = \case
      param : params' -> do
        paramTy <- goType (param ^. Internal.paramType)
        let paramBinder =
              Binder
                { _binderName = maybe "?" (^. nameText) $ param ^. Internal.paramName,
                  _binderLocation = fmap (^. nameLoc) $ param ^. Internal.paramName,
                  _binderType = paramTy
                }
        case param ^. Internal.paramName of
          Nothing -> mkPi mempty paramBinder <$> underBinders 1 (go params')
          Just vn -> mkPi mempty paramBinder <$> localAddName vn (go params')
      [] ->
        goType returnTypeExpr

goSimpleLambda ::
  forall r.
  (Members '[InfoTableBuilder, Reader InternalTyped.TypesTable, Reader InternalTyped.FunctionsTable, Reader Internal.InfoTable, Reader IndexTable, NameIdGen, Error BadScope] r) =>
  Internal.SimpleLambda ->
  Sem r Node
goSimpleLambda l = do
  let var = l ^. Internal.slambdaBinder . Internal.sbinderVar
  ty <- goType (l ^. Internal.slambdaBinder . Internal.sbinderType)
  let loc = var ^. nameLoc
      name = var ^. nameText
  localAddName var (mkLambda mempty (Binder name (Just loc) ty) <$> goExpression (l ^. Internal.slambdaBody))

goApplication ::
  forall r.
  (Members '[InfoTableBuilder, Reader InternalTyped.TypesTable, Reader InternalTyped.FunctionsTable, Reader Internal.InfoTable, Reader IndexTable, NameIdGen, Error BadScope] r) =>
  Internal.Application ->
  Sem r Node
goApplication a = do
  let (f, args) = second toList (Internal.unfoldApplication a)
      exprArgs :: Sem r [Node]
      exprArgs = mapM goExpression args

      app :: Sem r Node
      app = do
        fExpr <- goExpression f
        mkApps' fExpr <$> exprArgs

  case f of
    Internal.ExpressionIden (Internal.IdenAxiom n) -> do
      axiomInfoBuiltin <- Internal.getAxiomBuiltinInfo n
      case axiomInfoBuiltin of
        Just Internal.BuiltinNatPrint -> app
        Just Internal.BuiltinStringPrint -> app
        Just Internal.BuiltinBoolPrint -> app
        Just Internal.BuiltinString -> app
        Just Internal.BuiltinIO -> app
        Just Internal.BuiltinIOSequence -> do
          ioSym <- getIOSymbol
          as <- exprArgs
          return $ case as of
            (arg1 : arg2 : xs) ->
              mkApps'
                ( mkConstr'
                    (BuiltinTag TagBind)
                    [arg1, mkLambda' (mkTypeConstr (setInfoName Str.io mempty) ioSym []) (shift 1 arg2)]
                )
                xs
            _ -> error "internal to core: >> must be called with 2 arguments"
        Just Internal.BuiltinIOReadline -> app
        Just Internal.BuiltinStringConcat -> app
        Just Internal.BuiltinStringEq -> app
        Just Internal.BuiltinStringToNat -> app
        Just Internal.BuiltinNatToString -> app
        Just Internal.BuiltinTrace -> app
        Just Internal.BuiltinFail -> app
        Just Internal.BuiltinIntToString -> app
        Just Internal.BuiltinIntPrint -> app
        Just Internal.BuiltinField -> app
        Just Internal.BuiltinFieldEq -> app
        Just Internal.BuiltinFieldAdd -> app
        Just Internal.BuiltinFieldSub -> app
        Just Internal.BuiltinFieldMul -> app
        Just Internal.BuiltinFieldDiv -> app
        Just Internal.BuiltinFieldFromInt -> do
          as <- exprArgs
          case as of
            [x] -> return $ mkBuiltinApp' OpFieldFromInt [x]
            _ -> app
        Just Internal.BuiltinFieldToNat -> app
        Just Internal.BuiltinAnomaGet -> app
        Just Internal.BuiltinAnomaEncode -> app
        Just Internal.BuiltinAnomaDecode -> app
        Just Internal.BuiltinAnomaVerifyDetached -> app
        Just Internal.BuiltinAnomaSign -> app
        Just Internal.BuiltinAnomaSignDetached -> app
        Just Internal.BuiltinAnomaVerifyWithMessage -> app
        Just Internal.BuiltinAnomaByteArrayToAnomaContents -> app
        Just Internal.BuiltinAnomaByteArrayFromAnomaContents -> app
        Just Internal.BuiltinAnomaSha256 -> app
        Just Internal.BuiltinAnomaDelta -> app
        Just Internal.BuiltinAnomaKind -> app
        Just Internal.BuiltinAnomaResourceCommitment -> app
        Just Internal.BuiltinAnomaResourceNullifier -> app
        Just Internal.BuiltinAnomaResourceDelta -> app
        Just Internal.BuiltinAnomaResourceKind -> app
        Just Internal.BuiltinAnomaActionDelta -> app
        Just Internal.BuiltinAnomaActionsDelta -> app
        Just Internal.BuiltinAnomaZeroDelta -> app
        Just Internal.BuiltinAnomaAddDelta -> app
        Just Internal.BuiltinAnomaSubDelta -> app
        Just Internal.BuiltinAnomaProveAction -> app
        Just Internal.BuiltinAnomaProveDelta -> app
        Just Internal.BuiltinAnomaRandomGenerator -> app
        Just Internal.BuiltinAnomaRandomGeneratorInit -> app
        Just Internal.BuiltinAnomaRandomNextBytes -> app
        Just Internal.BuiltinAnomaRandomSplit -> app
        Just Internal.BuiltinAnomaIsCommitment -> app
        Just Internal.BuiltinAnomaIsNullifier -> app
        Just Internal.BuiltinAnomaSet -> app
        Just Internal.BuiltinAnomaSetToList -> app
        Just Internal.BuiltinAnomaSetFromList -> app
        Just Internal.BuiltinPoseidon -> app
        Just Internal.BuiltinEcOp -> app
        Just Internal.BuiltinRandomEcPoint -> app
        Just Internal.BuiltinByte -> app
        Just Internal.BuiltinByteEq -> app
        Just Internal.BuiltinByteToNat -> app
        Just Internal.BuiltinByteFromNat -> app
        Just Internal.BuiltinByteArray -> app
        Just Internal.BuiltinByteArrayFromListByte -> app
        Just Internal.BuiltinByteArrayLength -> app
        Nothing -> app
    Internal.ExpressionIden (Internal.IdenFunction n) -> do
      funInfoBuiltin <- Internal.getFunctionBuiltinInfo n
      case funInfoBuiltin of
        Just Internal.BuiltinBoolIf -> do
          sym <- getBoolSymbol
          as <- exprArgs
          case as of
            (_ : v : b1 : b2 : xs) -> return (mkApps' (mkIf' sym v b1 b2) xs)
            _ -> error "internal to core: if must be called with 3 arguments"
        Just Internal.BuiltinBoolOr -> do
          sym <- getBoolSymbol
          as <- exprArgs
          case as of
            (x : y : xs) -> return (mkApps' (mkIf' sym x (mkConstr' (BuiltinTag TagTrue) []) y) xs)
            _ -> error "internal to core: || must be called with 2 arguments"
        Just Internal.BuiltinBoolAnd -> do
          sym <- getBoolSymbol
          as <- exprArgs
          case as of
            (x : y : xs) -> return (mkApps' (mkIf' sym x y (mkConstr' (BuiltinTag TagFalse) [])) xs)
            _ -> error "internal to core: && must be called with 2 arguments"
        Just Internal.BuiltinSeq -> do
          as <- exprArgs
          case as of
            (_ : _ : arg1 : arg2 : xs) ->
              return (mkApps' (mkBuiltinApp' OpSeq [arg1, arg2]) xs)
            _ -> error "internal to core: seq must be called with 2 arguments"
        Just Internal.BuiltinAssert -> do
          as <- exprArgs
          case as of
            (x : xs) -> return (mkApps' (mkBuiltinApp' OpAssert [x]) xs)
            _ -> error "internal to core: assert must be called with 1 argument"
        _ -> app
    _ -> app

goLiteral :: Symbol -> Symbol -> Internal.LiteralLoc -> Node
goLiteral intToNat intToInt l = case l ^. withLocParam of
  Internal.LitString s -> mkLitConst (ConstString s)
  Internal.LitNumeric i -> mkLitConst (ConstInteger i)
  Internal.LitInteger i -> mkApp' (mkIdent' intToInt) (mkLitConst (ConstInteger i))
  Internal.LitNatural i -> mkApp' (mkIdent' intToNat) (mkLitConst (ConstInteger i))
  where
    mkLitConst :: ConstantValue -> Node
    mkLitConst = mkConstant (Info.singleton (LocationInfo (l ^. withLocInt)))
