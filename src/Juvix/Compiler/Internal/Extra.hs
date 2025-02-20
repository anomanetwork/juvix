module Juvix.Compiler.Internal.Extra
  ( module Juvix.Compiler.Internal.Extra,
    module Juvix.Compiler.Internal.Extra.Base,
    module Juvix.Compiler.Internal.Language,
    module Juvix.Compiler.Internal.Extra.Clonable,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Internal.Extra.Base
import Juvix.Compiler.Internal.Extra.Clonable
import Juvix.Compiler.Internal.Extra.DependencyBuilder
import Juvix.Compiler.Internal.Language
import Juvix.Compiler.Store.Internal.Data.InfoTable
import Juvix.Prelude

-- This is a hack to adjust location. It works only for identifiers. It should
-- change the location of an arbitrary given expression to the given location.
adjustLocation :: Interval -> Expression -> Expression
adjustLocation loc = \case
  ExpressionIden iden -> ExpressionIden (set (idenName . nameLoc) loc iden)
  eh -> eh

constructorArgTypes :: ConstructorInfo -> ([InductiveParameter], [FunctionParameter])
constructorArgTypes i =
  ( i ^. constructorInfoInductiveParameters,
    constructorArgs (i ^. constructorInfoType)
  )

constructorReturnType :: ConstructorInfo -> Expression
constructorReturnType info =
  let inductiveParams = fst (constructorArgTypes info)
      paramNames = inductiveParams ^.. each . inductiveParamName
      ind = ExpressionIden (IdenInductive (info ^. constructorInfoInductive))
      saturatedTy = foldExplicitApplication ind (map (ExpressionIden . IdenVar) paramNames)
   in saturatedTy

fullInductiveType :: InductiveInfo -> Expression
fullInductiveType info =
  let ps = info ^. inductiveInfoParameters
   in foldr
        (\p k -> p ^. inductiveParamType --> k)
        (info ^. inductiveInfoType)
        ps

constructorType :: ConstructorInfo -> Expression
constructorType info =
  let (inductiveParams, constrArgs) = constructorArgTypes info
      args =
        map inductiveToFunctionParam inductiveParams
          ++ constrArgs
      saturatedTy = constructorReturnType info
   in foldFunType args saturatedTy

inductiveToFunctionParam :: InductiveParameter -> FunctionParameter
inductiveToFunctionParam InductiveParameter {..} =
  FunctionParameter
    { _paramName = Just _inductiveParamName,
      _paramImplicit = Implicit,
      _paramType = _inductiveParamType
    }

patternArgFromVar :: IsImplicit -> VarName -> PatternArg
patternArgFromVar i v =
  PatternArg
    { _patternArgIsImplicit = i,
      _patternArgName = Nothing,
      _patternArgPattern = PatternVariable v
    }

-- | Given `mkApplicative`, returns {{mkApplicative {{funct}}}} var_pure var_ap, [var_pure, var_ap]
genConstructorPattern ::
  (Members '[NameIdGen] r) =>
  Interval ->
  IsImplicit ->
  ConstructorInfo ->
  Sem r (PatternArg, [VarName])
genConstructorPattern loc traitImplicity info =
  genConstructorPattern' traitImplicity loc (info ^. constructorInfoName) (snd (constructorArgTypes info))

-- | Given `mkPair`, returns (mkPair a b, [a, b])
genConstructorPattern' ::
  (Members '[NameIdGen] r) =>
  IsImplicit ->
  Interval ->
  Name ->
  [FunctionParameter] ->
  Sem r (PatternArg, [VarName])
genConstructorPattern' traitImplicity loc concstrName cargs = do
  vars :: [(IsImplicit, VarName)] <- runStreamOf allWords . forM cargs $ \p -> do
    varTxt <- maybe yield return (p ^? paramName . _Just . nameText)
    var <- freshVar loc varTxt
    return (p ^. paramImplicit, var)
  return (mkConstructorVarPattern traitImplicity concstrName vars, snd <$> vars)

mkConstructorVarPattern :: IsImplicit -> Name -> [(IsImplicit, VarName)] -> PatternArg
mkConstructorVarPattern impl c vars =
  PatternArg
    { _patternArgIsImplicit = impl,
      _patternArgName = Nothing,
      _patternArgPattern =
        PatternConstructorApp
          ConstructorApp
            { _constrAppConstructor = c,
              _constrAppType = Nothing,
              _constrAppParameters = map (uncurry patternArgFromVar) vars
            }
    }

-- | Generates a projection function for the given constructor and field index.
genFieldProjection ::
  forall r.
  (Members '[NameIdGen] r) =>
  ProjectionKind ->
  FunctionName ->
  Expression ->
  Maybe BuiltinFunction ->
  [ArgInfo] ->
  Maybe Pragmas ->
  ConstructorInfo ->
  Int ->
  Sem r FunctionDef
genFieldProjection kind _funDefName _funDefType _funDefBuiltin _funDefArgsInfo mpragmas info fieldIx = do
  body' <- genBody
  cloneFunctionDefSameName
    FunctionDef
      { _funDefTerminating = False,
        _funDefIsInstanceCoercion =
          if
              | kind == ProjectionCoercion -> Just IsInstanceCoercionCoercion
              | otherwise -> Nothing,
        _funDefPragmas =
          maybe
            (mempty {_pragmasInline = Just InlineAlways})
            (over pragmasInline (maybe (Just InlineAlways) Just))
            mpragmas,
        _funDefBody = body',
        _funDefDocComment = Nothing,
        _funDefType,
        _funDefName,
        _funDefBuiltin,
        _funDefArgsInfo
      }
  where
    constructorImplicity :: IsImplicit
    constructorImplicity
      | info ^. constructorInfoTrait = ImplicitInstance
      | otherwise = Explicit

    genBody :: Sem r Expression
    genBody = do
      (pat, vars) <- genConstructorPattern (getLoc _funDefName) constructorImplicity info
      let body = toExpression (vars !! fieldIx)
          cl =
            LambdaClause
              { _lambdaPatterns = pure pat,
                _lambdaBody = body
              }
      return . ExpressionLambda $
        Lambda
          { _lambdaType = Nothing,
            _lambdaClauses = pure cl
          }

-- | Generates definitions for each variable in a given pattern.
genPatternDefs ::
  forall r.
  (Members '[NameIdGen] r) =>
  Name ->
  PatternArg ->
  Sem r [FunctionDef]
genPatternDefs valueName pat =
  execOutputList (goPatternArg pat)
  where
    goPatternArg :: PatternArg -> Sem (Output FunctionDef ': r) ()
    goPatternArg PatternArg {..} = do
      whenJust _patternArgName goPatternVariable
      goPattern _patternArgPattern

    goPattern :: Pattern -> Sem (Output FunctionDef ': r) ()
    goPattern = \case
      PatternVariable x -> goPatternVariable x
      PatternWildcardConstructor {} -> return ()
      PatternConstructorApp x -> goPatternConstructorApp x

    goPatternVariable :: VarName -> Sem (Output FunctionDef ': r) ()
    goPatternVariable var = do
      h <- freshHole (getLoc valueName)
      let var' = set nameKind KNameLocal var
          body =
            ExpressionCase
              Case
                { _caseExpression = ExpressionIden (IdenFunction valueName),
                  _caseExpressionType = Nothing,
                  _caseExpressionWholeType = Nothing,
                  _caseBranches =
                    pure $
                      CaseBranch
                        { _caseBranchPattern = pat,
                          _caseBranchRhs =
                            CaseBranchRhsExpression (ExpressionIden (IdenVar var'))
                        }
                }
      body' <- clone body
      output $
        FunctionDef
          { _funDefTerminating = False,
            _funDefIsInstanceCoercion = Nothing,
            _funDefPragmas = mempty,
            _funDefBody = body',
            _funDefDocComment = Nothing,
            _funDefType = ExpressionHole h,
            _funDefName = var,
            _funDefBuiltin = Nothing,
            _funDefArgsInfo = []
          }

    goPatternConstructorApp :: ConstructorApp -> Sem (Output FunctionDef ': r) ()
    goPatternConstructorApp ConstructorApp {..} = do
      forM_ _constrAppParameters goPatternArg

buildLetMutualBlocks ::
  NonEmpty PreLetStatement ->
  NonEmpty (SCC PreLetStatement)
buildLetMutualBlocks ss = nonEmpty' . mapMaybe nameToPreStatement $ scomponents
  where
    -- TODO buildDependencyInfoLet is repeating too much work when there are big nested lets
    depInfo = buildDependencyInfoLet ss

    scomponents :: [SCC Name] = buildSCCs depInfo

    statementsByName :: HashMap Name PreLetStatement
    statementsByName = HashMap.fromList (map mkAssoc (toList ss))
      where
        mkAssoc :: PreLetStatement -> (Name, PreLetStatement)
        mkAssoc s = case s of
          PreLetFunctionDef i -> (i ^. funDefName, s)

    getStmt :: Name -> Maybe PreLetStatement
    getStmt n = statementsByName ^. at n

    nameToPreStatement :: SCC Name -> Maybe (SCC PreLetStatement)
    nameToPreStatement = nonEmptySCC . fmap getStmt
      where
        nonEmptySCC :: SCC (Maybe a) -> Maybe (SCC a)
        nonEmptySCC = \case
          AcyclicSCC a -> AcyclicSCC <$> a
          CyclicSCC p -> CyclicSCC . toList <$> nonEmpty (catMaybes p)

mkLetClauses :: NonEmpty PreLetStatement -> NonEmpty LetClause
mkLetClauses pre = goSCC <$> buildLetMutualBlocks pre
  where
    goSCC :: SCC PreLetStatement -> LetClause
    goSCC = \case
      AcyclicSCC (PreLetFunctionDef f) -> LetFunDef f
      CyclicSCC fs -> LetMutualBlock (MutualBlockLet fs')
        where
          fs' :: NonEmpty FunctionDef
          fs' = nonEmpty' (map getFun fs)
            where
              getFun :: PreLetStatement -> FunctionDef
              getFun = \case
                PreLetFunctionDef f -> f

inlineLet :: forall r. (Members '[NameIdGen] r) => Let -> Sem r Expression
inlineLet l = do
  (lclauses, subs) <-
    runOutputList
      . execState (mempty @Subs)
      $ forM (l ^. letClauses) helper
  body' <- substitutionE subs (l ^. letExpression)
  return $ case nonEmpty lclauses of
    Nothing -> body'
    Just cl' ->
      ExpressionLet
        Let
          { _letClauses = cl',
            _letExpression = body'
          }
  where
    helper :: forall r'. (r' ~ (State Subs ': Output LetClause ': r)) => LetClause -> Sem r' ()
    helper c = do
      subs <- get
      c' <- substitutionE subs c
      case subsClause c' of
        Nothing -> output c'
        Just (n, b) -> modify' @Subs (set (at n) (Just b))

    subsClause :: LetClause -> Maybe (Name, Expression)
    subsClause = \case
      LetMutualBlock {} -> Nothing
      LetFunDef f -> mkAssoc f
      where
        mkAssoc :: FunctionDef -> Maybe (Name, Expression)
        mkAssoc = \case
          FunctionDef
            { _funDefType = ExpressionHole {},
              _funDefBody = body,
              _funDefName = name,
              _funDefArgsInfo = []
            } -> Just (name, body)
          _ -> Nothing

cloneFunctionDefSameName :: (Members '[NameIdGen] r) => FunctionDef -> Sem r FunctionDef
cloneFunctionDefSameName f = do
  f' <- clone f
  return (set funDefName (f ^. funDefName) f')

subsInstanceHoles :: forall r a. (HasExpressions a, Member NameIdGen r) => HashMap InstanceHole Expression -> a -> Sem r a
subsInstanceHoles s = umapM helper
  where
    helper :: Expression -> Sem r Expression
    helper le = case le of
      -- TODO: The location of the hole should be preserved
      ExpressionInstanceHole h ->
        adjustLocation (getLoc h) <$> clone (fromMaybe e (s ^. at h))
      _ -> return e
      where
        e = toExpression le

subsHoles :: forall r a. (HasExpressions a, Member NameIdGen r) => HashMap Hole Expression -> a -> Sem r a
subsHoles s = umapM helper
  where
    helper :: Expression -> Sem r Expression
    helper le = case le of
      -- TODO: The location of the hole should be preserved
      ExpressionHole h ->
        adjustLocation (getLoc h) <$> clone (fromMaybe e (s ^. at h))
      _ -> return e
      where
        e = toExpression le

substitutionE :: forall r expr. (Member NameIdGen r, HasExpressions expr) => Subs -> expr -> Sem r expr
substitutionE m expr
  | null m = pure expr
  | otherwise = umapM go expr
  where
    go :: Expression -> Sem r Expression
    go = \case
      ExpressionIden i -> goName (i ^. idenName)
      e -> return (toExpression e)

    goName :: Name -> Sem r Expression
    goName n =
      case HashMap.lookup n m of
        Just e -> clone e
        Nothing -> return (toExpression n)

substituteIndParams ::
  forall r expr.
  (Member NameIdGen r, HasExpressions expr) =>
  [(InductiveParameter, Expression)] ->
  expr ->
  Sem r expr
substituteIndParams = substitutionE . HashMap.fromList . map (first (^. inductiveParamName))

getInductiveKind :: InductiveDef -> Expression
getInductiveKind InductiveDef {..} =
  foldr
    ( \p f ->
        ExpressionFunction $
          Function
            (FunctionParameter (Just (p ^. inductiveParamName)) Explicit (p ^. inductiveParamType))
            f
    )
    _inductiveType
    _inductiveParameters
