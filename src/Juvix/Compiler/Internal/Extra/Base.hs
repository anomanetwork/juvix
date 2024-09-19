{-# OPTIONS_GHC -Wno-orphans #-}

module Juvix.Compiler.Internal.Extra.Base where

import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Juvix.Compiler.Internal.Data.LocalVars
import Juvix.Compiler.Internal.Language
import Juvix.Compiler.Internal.Pretty
import Juvix.Prelude

type Rename = HashMap VarName VarName

type Subs = HashMap VarName Expression

instance PrettyCode Subs where
  ppCode :: forall r. (Member (Reader Options) r) => Subs -> Sem r (Doc Ann)
  ppCode m = do
    assocs' <- mapM ppAssoc (HashMap.toList m)
    return (braces (align (concatWith (\a b -> a <> kwSemicolon <> hardline <+> b) assocs')))
    where
      ppAssoc :: (VarName, Expression) -> Sem r (Doc Ann)
      ppAssoc (v, el) = do
        v' <- ppCode v
        el' <- ppCode el
        return (v' <+> kwMapsto <+> el')

data ApplicationArg = ApplicationArg
  { _appArgIsImplicit :: IsImplicit,
    _appArg :: Expression
  }
  deriving stock (Generic, Data)

makeLenses ''ApplicationArg

instance HasLoc ApplicationArg where
  getLoc = getLoc . (^. appArg)

class IsExpression a where
  toExpression :: a -> Expression

instance Plated Expression where
  plate :: Traversal' Expression Expression
  plate f e = case e of
    ExpressionApplication a -> ExpressionApplication <$> directExpressions f a
    ExpressionFunction fun -> ExpressionFunction <$> directExpressions f fun
    ExpressionSimpleLambda l -> ExpressionSimpleLambda <$> directExpressions f l
    ExpressionLambda l -> ExpressionLambda <$> directExpressions f l
    ExpressionLet l -> ExpressionLet <$> directExpressions f l
    ExpressionCase c -> ExpressionCase <$> directExpressions f c
    ExpressionIden {} -> pure e
    ExpressionLiteral {} -> pure e
    ExpressionUniverse {} -> pure e
    ExpressionHole {} -> pure e
    ExpressionInstanceHole {} -> pure e

class HasExpressions a where
  -- | Traverses itself if `a` is an Expression. Otherwise traverses children `Expression`s (not transitive).
  directExpressions :: Traversal' a Expression

directExpressions_ :: forall f a. (HasExpressions a, Applicative f) => (Expression -> f ()) -> a -> f ()
directExpressions_ f = void . directExpressions (\e -> e <$ f e)

instance HasExpressions Expression where
  directExpressions = id

instance (HasExpressions a, Traversable l) => HasExpressions (l a) where
  directExpressions f = traverse (directExpressions f)

instance HasExpressions LambdaClause where
  directExpressions f l = do
    _lambdaPatterns <- directExpressions f (l ^. lambdaPatterns)
    _lambdaBody <- directExpressions f (l ^. lambdaBody)
    pure LambdaClause {..}

instance HasExpressions Lambda where
  directExpressions f l = do
    _lambdaClauses <- directExpressions f (l ^. lambdaClauses)
    _lambdaType <- directExpressions f (l ^. lambdaType)
    pure Lambda {..}

instance HasExpressions ConstructorApp where
  directExpressions f a = do
    let _constrAppConstructor = a ^. constrAppConstructor
    _constrAppType <- directExpressions f (a ^. constrAppType)
    _constrAppParameters <- directExpressions f (a ^. constrAppParameters)
    pure ConstructorApp {..}

instance HasExpressions PatternArg where
  directExpressions f a = do
    let _patternArgIsImplicit = a ^. patternArgIsImplicit
        _patternArgName = a ^. patternArgName
    _patternArgPattern <- directExpressions f (a ^. patternArgPattern)
    pure PatternArg {..}

instance HasExpressions WildcardConstructor where
  directExpressions _ WildcardConstructor {..} = do
    pure
      WildcardConstructor
        { _wildcardConstructor
        }

instance HasExpressions Pattern where
  directExpressions f p = case p of
    PatternVariable {} -> pure p
    PatternConstructorApp a -> PatternConstructorApp <$> directExpressions f a
    PatternWildcardConstructor {} -> pure p

instance HasExpressions SideIfBranch where
  directExpressions f b = do
    _sideIfBranchCondition <- directExpressions f (b ^. sideIfBranchCondition)
    _sideIfBranchBody <- directExpressions f (b ^. sideIfBranchBody)
    pure SideIfBranch {..}

instance HasExpressions SideIfs where
  directExpressions f b = do
    _sideIfBranches <- directExpressions f (b ^. sideIfBranches)
    _sideIfElse <- directExpressions f (b ^. sideIfElse)
    pure SideIfs {..}

instance HasExpressions CaseBranchRhs where
  directExpressions f = \case
    CaseBranchRhsExpression e -> CaseBranchRhsExpression <$> directExpressions f e
    CaseBranchRhsIf e -> CaseBranchRhsIf <$> directExpressions f e

instance HasExpressions CaseBranch where
  directExpressions f b = do
    _caseBranchPattern <- directExpressions f (b ^. caseBranchPattern)
    _caseBranchRhs <- directExpressions f (b ^. caseBranchRhs)
    pure CaseBranch {..}

instance HasExpressions Case where
  directExpressions f l = do
    _caseBranches <- directExpressions f (l ^. caseBranches)
    _caseExpression <- directExpressions f (l ^. caseExpression)
    _caseExpressionType <- directExpressions f (l ^. caseExpressionType)
    _caseExpressionWholeType <- directExpressions f (l ^. caseExpressionWholeType)
    pure Case {..}

instance HasExpressions MutualBlock where
  directExpressions f (MutualBlock defs) =
    MutualBlock <$> directExpressions f defs

instance HasExpressions MutualBlockLet where
  directExpressions f (MutualBlockLet defs) =
    MutualBlockLet <$> directExpressions f defs

instance HasExpressions LetClause where
  directExpressions f = \case
    LetFunDef d -> LetFunDef <$> directExpressions f d
    LetMutualBlock b -> LetMutualBlock <$> directExpressions f b

instance HasExpressions Let where
  directExpressions f l = do
    _letClauses :: NonEmpty LetClause <- directExpressions f (l ^. letClauses)
    _letExpression <- directExpressions f (l ^. letExpression)
    pure Let {..}

instance HasExpressions TypedExpression where
  directExpressions f a = do
    _typedExpression <- directExpressions f (a ^. typedExpression)
    _typedType <- directExpressions f (a ^. typedType)
    pure TypedExpression {..}

instance HasExpressions SimpleBinder where
  directExpressions f (SimpleBinder v ty) = do
    ty' <- directExpressions f ty
    pure (SimpleBinder v ty')

instance HasExpressions SimpleLambda where
  directExpressions f (SimpleLambda bi b) = do
    bi' <- directExpressions f bi
    b' <- directExpressions f b
    pure (SimpleLambda bi' b')

instance HasExpressions FunctionParameter where
  directExpressions f FunctionParameter {..} = do
    ty' <- directExpressions f _paramType
    pure
      FunctionParameter
        { _paramType = ty',
          _paramName,
          _paramImplicit
        }

instance HasExpressions Function where
  directExpressions f (Function l r) = do
    l' <- directExpressions f l
    r' <- directExpressions f r
    pure (Function l' r')

instance HasExpressions ApplicationArg where
  directExpressions f ApplicationArg {..} = do
    arg' <- directExpressions f _appArg
    pure
      ApplicationArg
        { _appArg = arg',
          _appArgIsImplicit
        }

instance HasExpressions Application where
  directExpressions f (Application l r i) = do
    l' <- directExpressions f l
    r' <- directExpressions f r
    pure (Application l' r' i)

letDefs :: (HasExpressions a) => a -> [Let]
letDefs a = [l | ExpressionLet l <- a ^.. allExpressions]

instance HasExpressions ArgInfo where
  directExpressions f ArgInfo {..} = do
    d' <- directExpressions f _argInfoDefault
    return
      ArgInfo
        { _argInfoDefault = d',
          _argInfoName
        }

instance HasExpressions FunctionDef where
  directExpressions f FunctionDef {..} = do
    body' <- directExpressions f _funDefBody
    ty' <- directExpressions f _funDefType
    infos' <- directExpressions f _funDefArgsInfo
    pure
      FunctionDef
        { _funDefBody = body',
          _funDefType = ty',
          _funDefArgsInfo = infos',
          _funDefTerminating,
          _funDefIsInstanceCoercion,
          _funDefName,
          _funDefBuiltin,
          _funDefPragmas,
          _funDefDocComment
        }

instance HasExpressions MutualStatement where
  directExpressions f = \case
    StatementFunction d -> StatementFunction <$> directExpressions f d
    StatementInductive d -> StatementInductive <$> directExpressions f d
    StatementAxiom d -> StatementAxiom <$> directExpressions f d

instance HasExpressions AxiomDef where
  directExpressions f AxiomDef {..} = do
    ty' <- directExpressions f _axiomType
    pure
      AxiomDef
        { _axiomType = ty',
          _axiomName,
          _axiomBuiltin,
          _axiomPragmas,
          _axiomDocComment
        }

instance HasExpressions InductiveParameter where
  directExpressions f InductiveParameter {..} = do
    ty' <- directExpressions f _inductiveParamType
    pure
      InductiveParameter
        { _inductiveParamType = ty',
          _inductiveParamName
        }

instance HasExpressions InductiveDef where
  directExpressions f InductiveDef {..} = do
    params' <- directExpressions f _inductiveParameters
    constrs' <- directExpressions f _inductiveConstructors
    ty' <- directExpressions f _inductiveType
    pure
      InductiveDef
        { _inductiveParameters = params',
          _inductiveConstructors = constrs',
          _inductiveType = ty',
          _inductiveName,
          _inductiveBuiltin,
          _inductivePositive,
          _inductiveTrait,
          _inductivePragmas,
          _inductiveDocComment
        }

instance HasExpressions ConstructorDef where
  directExpressions f ConstructorDef {..} = do
    ty' <- directExpressions f _inductiveConstructorType
    pure
      ConstructorDef
        { _inductiveConstructorType = ty',
          _inductiveConstructorName,
          _inductiveConstructorIsRecord,
          _inductiveConstructorPragmas,
          _inductiveConstructorDocComment
        }

typeAbstraction :: IsImplicit -> Name -> FunctionParameter
typeAbstraction i var = FunctionParameter (Just var) i (ExpressionUniverse (SmallUniverse (getLoc var)))

mkFunction :: Expression -> Expression -> Function
mkFunction a = Function (unnamedParameter a)

unnamedParameter' :: IsImplicit -> Expression -> FunctionParameter
unnamedParameter' impl ty =
  FunctionParameter
    { _paramName = Nothing,
      _paramImplicit = impl,
      _paramType = ty
    }

unnamedParameter :: Expression -> FunctionParameter
unnamedParameter = unnamedParameter' Explicit

singletonRename :: VarName -> VarName -> Rename
singletonRename = HashMap.singleton

renameKind :: NameKind -> [Name] -> Subs
renameKind k l = HashMap.fromList [(n, toExpression (set nameKind k n)) | n <- l]

renameToSubsE :: Rename -> Subs
renameToSubsE = fmap (ExpressionIden . IdenVar)

inductiveTypeVarsAssoc :: (Foldable f) => InductiveDef -> f a -> HashMap VarName a
inductiveTypeVarsAssoc def l
  | length vars < n = impossible
  | otherwise = HashMap.fromList (zip vars (toList l))
  where
    n = length l
    vars :: [VarName]
    vars = def ^.. inductiveParameters . each . inductiveParamName

substitutionApp :: (Maybe Name, Expression) -> Subs
substitutionApp (mv, ty) = case mv of
  Nothing -> mempty
  Just v -> HashMap.singleton v ty

localsToSubsE :: LocalVars -> Subs
localsToSubsE l = ExpressionIden . IdenVar <$> l ^. localTyMap

subsKind :: [Name] -> NameKind -> Subs
subsKind uids k =
  HashMap.fromList
    [ (s, toExpression s') | s <- uids, let s' = toExpression (set nameKind k s)
    ]

data Expr
  = Val Int
  | Neg Expr
  | Add Expr Expr
  deriving stock (Eq, Ord, Show, Data)

instance Plated Expr where
  plate f (Neg e) = Neg <$> f e
  plate f (Add a b) = Add <$> f a <*> f b
  plate _ a = pure a

-- | A Fold over all subexressions, including self
allExpressions :: (HasExpressions expr) => Fold expr Expression
allExpressions = cosmosOn directExpressions

smallUniverseE :: Interval -> Expression
smallUniverseE = ExpressionUniverse . SmallUniverse

-- | [a, b] c ==> a -> (b -> c)
foldFunType :: [FunctionParameter] -> Expression -> Expression
foldFunType l r = go l
  where
    go :: [FunctionParameter] -> Expression
    go = \case
      [] -> r
      arg : args -> ExpressionFunction (Function arg (go args))

foldFunTypeExplicit :: [Expression] -> Expression -> Expression
foldFunTypeExplicit = foldFunType . map unnamedParameter

viewConstructorType :: Expression -> ([FunctionParameter], Expression)
viewConstructorType = unfoldFunType

constructorArgs :: Expression -> [FunctionParameter]
constructorArgs = fst . viewConstructorType

unfoldLambdaClauses :: Expression -> Maybe (NonEmpty (NonEmpty PatternArg, Expression))
unfoldLambdaClauses t = do
  ExpressionLambda Lambda {..} <- return t
  let mkClause :: LambdaClause -> (NonEmpty PatternArg, Expression)
      mkClause LambdaClause {..} = first (appendList _lambdaPatterns) (unfoldLambda _lambdaBody)
  return (mkClause <$> _lambdaClauses)

mkLambda :: NonEmpty LambdaClause -> Expression
mkLambda c =
  ExpressionLambda
    Lambda
      { _lambdaType = Nothing,
        _lambdaClauses = c
      }

-- Unfolds *single* clause lambdas
unfoldLambda :: Expression -> ([PatternArg], Expression)
unfoldLambda t = case t of
  ExpressionLambda Lambda {..}
    | LambdaClause {..} :| [] <- _lambdaClauses ->
        first (toList _lambdaPatterns <>) (unfoldLambda _lambdaBody)
  _ -> ([], t)

-- | a -> (b -> c)  ==> ([a, b], c)
unfoldFunType :: Expression -> ([FunctionParameter], Expression)
unfoldFunType t = case t of
  ExpressionFunction (Function l r) -> first (l :) (unfoldFunType r)
  _ -> ([], t)

unfoldTypeAbsType :: Expression -> ([VarName], Expression)
unfoldTypeAbsType t = case t of
  ExpressionFunction (Function (FunctionParameter (Just var) _ _) r) ->
    first (var :) (unfoldTypeAbsType r)
  _ -> ([], t)

foldExplicitApplication :: Expression -> [Expression] -> Expression
foldExplicitApplication f = foldApplication f . map (ApplicationArg Explicit)

foldApplication' :: Expression -> NonEmpty ApplicationArg -> Application
foldApplication' f (arg :| args) =
  let ApplicationArg i a = arg
   in go (Application f a i) args
  where
    go :: Application -> [ApplicationArg] -> Application
    go acc = \case
      [] -> acc
      ApplicationArg i a : as -> go (Application (ExpressionApplication acc) a i) as

foldApplication :: Expression -> [ApplicationArg] -> Expression
foldApplication f args = case nonEmpty args of
  Nothing -> f
  Just args' -> ExpressionApplication (foldApplication' f args')

unfoldApplication' :: Application -> (Expression, NonEmpty ApplicationArg)
unfoldApplication' (Application l' r' i') = second (|: (ApplicationArg i' r')) (unfoldExpressionApp l')

unfoldExpressionApp :: Expression -> (Expression, [ApplicationArg])
unfoldExpressionApp = swap . run . runAccumListReverse . go
  where
    go :: Expression -> Sem '[Accum ApplicationArg] Expression
    go = \case
      ExpressionApplication (Application l r i) -> do
        accum (ApplicationArg i r)
        go l
      e -> return e

unfoldApplication :: Application -> (Expression, NonEmpty Expression)
unfoldApplication = fmap (fmap (^. appArg)) . unfoldApplication'

unfoldExplicitApplication :: Application -> (Expression, [Expression])
unfoldExplicitApplication =
  fmap
    ( fmap (^. appArg)
        . filter (\x -> x ^. appArgIsImplicit == Explicit)
        . toList
    )
    . unfoldApplication'

-- | A fold over all transitive children, including self
patternCosmos :: SimpleFold Pattern Pattern
patternCosmos f p = case p of
  PatternVariable {} -> f p
  PatternWildcardConstructor {} -> f p
  PatternConstructorApp ConstructorApp {..} ->
    f p *> do
      args' <- traverse (traverseOf patternArgPattern (patternCosmos f)) _constrAppParameters
      pure $
        PatternConstructorApp
          ConstructorApp
            { _constrAppParameters = args',
              _constrAppConstructor,
              _constrAppType
            }

patternArgNameFold :: SimpleFold (Maybe Name) Pattern
patternArgNameFold f = \case
  Nothing -> mempty
  Just n -> Const (getConst (f (PatternVariable n)))

-- | A fold over all transitive children, including self
patternArgCosmos :: SimpleFold PatternArg Pattern
patternArgCosmos f p = do
  _patternArgPattern <- patternCosmos f (p ^. patternArgPattern)
  _patternArgName <- patternArgNameFold f (p ^. patternArgName)
  pure PatternArg {..}
  where
    _patternArgIsImplicit = p ^. patternArgIsImplicit

-- | A fold over all transitive children, excluding self
patternSubCosmos :: SimpleFold Pattern Pattern
patternSubCosmos f p = case p of
  PatternVariable {} -> pure p
  PatternWildcardConstructor {} -> pure p
  PatternConstructorApp ConstructorApp {..} -> do
    args' <- traverse (patternArgCosmos f) _constrAppParameters
    pure $
      PatternConstructorApp
        ConstructorApp
          { _constrAppParameters = args',
            _constrAppConstructor,
            _constrAppType
          }

viewAppArgAsPattern :: ApplicationArg -> Maybe PatternArg
viewAppArgAsPattern a = do
  p' <- viewExpressionAsPattern (a ^. appArg)
  return
    ( PatternArg
        { _patternArgIsImplicit = a ^. appArgIsImplicit,
          _patternArgName = Nothing,
          _patternArgPattern = p'
        }
    )

viewApp :: Expression -> (Expression, [ApplicationArg])
viewApp e =
  case e of
    ExpressionApplication (Application l r i) ->
      second (`snoc` ApplicationArg i r) (viewApp l)
    _ -> (e, [])

viewExpressionAsPattern :: Expression -> Maybe Pattern
viewExpressionAsPattern e = case viewApp e of
  (f, args)
    | Just c <- getConstructor f -> do
        args' <- mapM viewAppArgAsPattern args
        Just $ PatternConstructorApp (ConstructorApp c args' Nothing)
  (f, [])
    | Just v <- getVariable f -> Just (PatternVariable v)
  _ -> Nothing
  where
    getConstructor :: Expression -> Maybe ConstructorName
    getConstructor f = case f of
      ExpressionIden (IdenConstructor n) -> Just n
      _ -> Nothing
    getVariable :: Expression -> Maybe VarName
    getVariable f = case f of
      ExpressionIden (IdenVar n) -> Just n
      _ -> Nothing

instance IsExpression Iden where
  toExpression = ExpressionIden

instance IsExpression Expression where
  toExpression = id

instance IsExpression Case where
  toExpression = ExpressionCase

instance IsExpression Hole where
  toExpression = ExpressionHole

instance IsExpression Name where
  toExpression n = ExpressionIden (mkIden n)
    where
      mkIden = case n ^. nameKind of
        KNameConstructor -> IdenConstructor
        KNameInductive -> IdenInductive
        KNameFunction -> IdenFunction
        KNameAxiom -> IdenAxiom
        KNameLocal -> IdenVar
        KNameLocalModule -> impossible
        KNameTopModule -> impossible
        KNameFixity -> impossible
        KNameAlias -> impossible

instance IsExpression SmallUniverse where
  toExpression = ExpressionUniverse

instance IsExpression Application where
  toExpression = ExpressionApplication

instance IsExpression Function where
  toExpression = ExpressionFunction

instance IsExpression ConstructorApp where
  toExpression (ConstructorApp c args _) =
    foldApplication (toExpression c) (map toApplicationArg args)

instance IsExpression WildcardConstructor where
  toExpression = toExpression . (^. wildcardConstructor)

toApplicationArg :: PatternArg -> ApplicationArg
toApplicationArg p =
  set appArgIsImplicit (p ^. patternArgIsImplicit) (helper (p ^. patternArgPattern))
  where
    helper :: Pattern -> ApplicationArg
    helper = \case
      PatternVariable v -> ApplicationArg Explicit (toExpression v)
      PatternConstructorApp a -> ApplicationArg Explicit (toExpression a)
      PatternWildcardConstructor a -> ApplicationArg Explicit (toExpression a)

expressionArrow :: (IsExpression a, IsExpression b) => IsImplicit -> a -> b -> Expression
expressionArrow isImpl a b =
  ExpressionFunction
    ( Function
        ( FunctionParameter
            { _paramName = Nothing,
              _paramImplicit = isImpl,
              _paramType = toExpression a
            }
        )
        (toExpression b)
    )

infixr 0 <>-->

(<>-->) :: (IsExpression a, IsExpression b) => a -> b -> Expression
(<>-->) = expressionArrow Implicit

infixr 0 -->

(-->) :: (IsExpression a, IsExpression b) => a -> b -> Expression
(-->) = expressionArrow Explicit

infix 4 ===

(===) :: (IsExpression a, IsExpression b) => a -> b -> Bool
a === b = (toExpression a ==% toExpression b) mempty

leftEq :: (IsExpression a, IsExpression b) => a -> b -> HashSet Name -> Bool
leftEq a b free =
  isRight
    . run
    . runError @Text
    . runReader free
    . evalState (mempty @(HashMap Name Name))
    $ matchExpressions (toExpression a) (toExpression b)

clauseLhsAsExpression :: Name -> [PatternArg] -> Expression
clauseLhsAsExpression clName pats =
  foldApplication (toExpression clName) (map toApplicationArg pats)

infix 4 ==%

(==%) :: (IsExpression a, IsExpression b) => a -> b -> HashSet Name -> Bool
(==%) a b free = leftEq a b free || leftEq b a free

infixl 9 @@?

(@@?) :: (IsExpression a, IsExpression b) => a -> b -> IsImplicit -> Expression
a @@? b = toExpression . Application (toExpression a) (toExpression b)

infixl 9 @@

(@@) :: (IsExpression a, IsExpression b) => a -> b -> Expression
a @@ b = toExpression (Application (toExpression a) (toExpression b) Explicit)

-- | \{p := b}
(==>) :: (IsExpression a) => PatternArg -> a -> Expression
p ==> b =
  ExpressionLambda
    Lambda
      { _lambdaClauses =
          pure
            LambdaClause
              { _lambdaPatterns = pure p,
                _lambdaBody = toExpression b
              },
        _lambdaType = Nothing
      }

freshFunVar :: (Member NameIdGen r) => Interval -> Text -> Sem r VarName
freshFunVar i n = set nameKind KNameFunction <$> freshVar i n

freshVar :: (Member NameIdGen r) => Interval -> Text -> Sem r VarName
freshVar _nameLoc n = do
  uid <- freshNameId
  return
    Name
      { _nameId = uid,
        _nameText = n,
        _nameKind = KNameLocal,
        _nameKindPretty = KNameLocal,
        _namePretty = n,
        _nameFixity = Nothing,
        _nameLoc
      }

mkCaseBranch :: PatternArg -> Expression -> CaseBranch
mkCaseBranch pat body =
  CaseBranch
    { _caseBranchPattern = pat,
      _caseBranchRhs = CaseBranchRhsExpression body
    }

mkCase :: Expression -> NonEmpty CaseBranch -> Expression
mkCase cexpr clauses =
  toExpression
    Case
      { _caseExpression = cexpr,
        _caseBranches = clauses,
        _caseExpressionType = Nothing,
        _caseExpressionWholeType = Nothing
      }

mkVarPattern :: VarName -> IsImplicit -> PatternArg
mkVarPattern var impl = PatternArg impl Nothing (PatternVariable var)

genWildcard :: forall r'. (Members '[NameIdGen] r') => Interval -> IsImplicit -> Sem r' PatternArg
genWildcard loc impl = do
  var <- varFromWildcard (Wildcard loc)
  return (mkVarPattern var impl)

freshInstanceHole :: (Members '[NameIdGen] r) => Interval -> Sem r InstanceHole
freshInstanceHole l = mkInstanceHole l <$> freshNameId

freshHole :: (Members '[NameIdGen] r) => Interval -> Sem r Hole
freshHole l = mkHole l <$> freshNameId

mkFreshHole :: (Members '[NameIdGen] r) => Interval -> Sem r Expression
mkFreshHole l = ExpressionHole <$> freshHole l

matchExpressions ::
  forall r.
  (Members '[State (HashMap Name Name), Reader (HashSet VarName), Error Text] r) =>
  Expression ->
  Expression ->
  Sem r ()
matchExpressions = go
  where
    -- Soft free vars are allowed to be matched
    isSoftFreeVar :: VarName -> Sem r Bool
    isSoftFreeVar = asks . HashSet.member
    go :: Expression -> Expression -> Sem r ()
    go a b = case (a, b) of
      (ExpressionIden ia, ExpressionIden ib) -> case (ia, ib) of
        (IdenVar va, IdenVar vb) -> do
          addIfFreeVar va vb
          addIfFreeVar vb va
          unlessM (matchVars va vb) err
        (_, _) -> unless (ia == ib) err
      (ExpressionIden (IdenVar va), ExpressionHole h) -> goHole va h
      (ExpressionHole h, ExpressionIden (IdenVar vb)) -> goHole vb h
      (ExpressionIden {}, _) -> err
      (_, ExpressionIden {}) -> err
      (ExpressionApplication ia, ExpressionApplication ib) ->
        goApp ia ib
      (ExpressionApplication {}, _) -> err
      (_, ExpressionApplication {}) -> err
      (ExpressionLambda ia, ExpressionLambda ib) ->
        goLambda ia ib
      (ExpressionLambda {}, _) -> err
      (_, ExpressionLambda {}) -> err
      (ExpressionCase {}, ExpressionCase {}) -> error "not implemented"
      (ExpressionCase {}, _) -> err
      (_, ExpressionCase {}) -> err
      (ExpressionUniverse ia, ExpressionUniverse ib) ->
        unless (ia == ib) err
      (ExpressionUniverse {}, _) -> err
      (_, ExpressionUniverse {}) -> err
      (ExpressionFunction ia, ExpressionFunction ib) ->
        goFunction ia ib
      (ExpressionFunction {}, _) -> err
      (_, ExpressionFunction {}) -> err
      (ExpressionSimpleLambda {}, ExpressionSimpleLambda {}) -> error "not implemented"
      (ExpressionSimpleLambda {}, _) -> err
      (_, ExpressionSimpleLambda {}) -> err
      (ExpressionLiteral ia, ExpressionLiteral ib) ->
        unless (ia == ib) err
      (ExpressionLiteral {}, _) -> err
      (_, ExpressionLiteral {}) -> err
      (ExpressionLet {}, ExpressionLet {}) -> error "not implemented"
      (_, ExpressionLet {}) -> err
      (ExpressionLet {}, _) -> err
      (ExpressionHole _, ExpressionHole _) -> return ()
      (ExpressionInstanceHole _, ExpressionInstanceHole _) -> return ()
      (_, ExpressionInstanceHole {}) -> err
      (ExpressionInstanceHole {}, _) -> err

    err :: Sem r a
    err = throw @Text "Expression mismatch"

    matchVars :: Name -> Name -> Sem r Bool
    matchVars va vb = do
      let eq = va == vb
      uni <- (== Just vb) <$> gets @(HashMap Name Name) (^. at va)
      return (uni || eq)

    goHole :: Name -> Hole -> Sem r ()
    goHole var h = do
      whenM (gets @(HashMap Name Name) (HashMap.member var)) err
      let vh = varFromHole h
      addName var vh

    addIfFreeVar :: VarName -> VarName -> Sem r ()
    addIfFreeVar va vb = whenM (isSoftFreeVar va) (addName va vb)

    goLambda :: Lambda -> Lambda -> Sem r ()
    goLambda = error "TODO not implemented yet"

    goApp :: Application -> Application -> Sem r ()
    goApp (Application al ar aim) (Application bl br bim) = do
      unless (aim == bim) err
      go al bl
      go ar br

    goFunction :: Function -> Function -> Sem r ()
    goFunction (Function al ar) (Function bl br) = do
      matchFunctionParameter al bl
      matchExpressions ar br

addName :: (Member (State (HashMap Name Name)) r) => Name -> Name -> Sem r ()
addName na nb = modify (HashMap.insert na nb)

matchFunctionParameter ::
  forall r.
  (Members '[State (HashMap Name Name), Reader (HashSet VarName), Error Text] r) =>
  FunctionParameter ->
  FunctionParameter ->
  Sem r ()
matchFunctionParameter pa pb = do
  goParamName (pa ^. paramName) (pb ^. paramName)
  goParamImplicit (pa ^. paramImplicit) (pb ^. paramImplicit)
  goParamType (pa ^. paramType) (pb ^. paramType)
  where
    goParamType :: Expression -> Expression -> Sem r ()
    goParamType ua ub = matchExpressions ua ub
    goParamImplicit :: IsImplicit -> IsImplicit -> Sem r ()
    goParamImplicit ua ub = unless (ua == ub) (throw @Text "implicit mismatch")
    goParamName :: Maybe VarName -> Maybe VarName -> Sem r ()
    goParamName (Just va) (Just vb) = addName va vb
    goParamName _ _ = return ()

isSmallUniverse' :: Expression -> Bool
isSmallUniverse' = \case
  ExpressionUniverse {} -> True
  _ -> False

-- | This function does not take into account synonyms (won't work with e.g.
-- `Type' : Type := Type`).
isTypeConstructor :: Expression -> Bool
isTypeConstructor = isSmallUniverse' . snd . unfoldFunType

explicitApplicationArg :: (IsExpression expr) => expr -> ApplicationArg
explicitApplicationArg e =
  ApplicationArg
    { _appArgIsImplicit = Explicit,
      _appArg = toExpression e
    }

explicitPatternArg :: Pattern -> PatternArg
explicitPatternArg _patternArgPattern =
  PatternArg
    { _patternArgName = Nothing,
      _patternArgIsImplicit = Explicit,
      _patternArgPattern
    }

simpleFunDef :: Name -> Expression -> Expression -> FunctionDef
simpleFunDef funName ty body =
  FunctionDef
    { _funDefName = funName,
      _funDefType = ty,
      _funDefIsInstanceCoercion = Nothing,
      _funDefPragmas = mempty,
      _funDefArgsInfo = mempty,
      _funDefTerminating = False,
      _funDefBuiltin = Nothing,
      _funDefBody = body,
      _funDefDocComment = Nothing
    }

umapM :: (Monad m, HasExpressions expr) => (Expression -> m Expression) -> expr -> m expr
umapM = transformMOn directExpressions
