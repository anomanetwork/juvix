module Juvix.Compiler.Internal.Language
  ( module Juvix.Compiler.Internal.Language,
    module Juvix.Compiler.Internal.Data.Name,
    module Juvix.Data.WithLoc,
    module Juvix.Data.IsImplicit,
    module Juvix.Data.Universe,
    module Juvix.Data.Hole,
    module Juvix.Compiler.Concrete.Data.Builtins,
    module Juvix.Data.InstanceHole,
  )
where

import Juvix.Compiler.Concrete.Data.Builtins
import Juvix.Compiler.Internal.Data.Name
import Juvix.Data.CodeAnn
import Juvix.Data.Hole
import Juvix.Data.InstanceHole
import Juvix.Data.IsImplicit
import Juvix.Data.Universe hiding (smallUniverse)
import Juvix.Data.WithLoc
import Juvix.Extra.Serialize
import Juvix.Prelude

type Module = Module' MutualBlock

type PreModule = Module' PreStatement

type ModuleBody = ModuleBody' MutualBlock

type PreModuleBody = ModuleBody' PreStatement

newtype PreLetStatement
  = PreLetFunctionDef FunctionDef

-- | Traits that support builtin deriving
data DerivingTrait
  = DerivingEq
  | DerivingOrd
  deriving stock (Generic, Data, Bounded, Enum, Show)

derivingTraitFromBuiltinMap :: HashMap BuiltinPrim DerivingTrait
derivingTraitFromBuiltinMap = hashMap [(toBuiltinPrim d, d) | d <- allElements]

derivingTraitFromBuiltin :: (IsBuiltin builtin) => builtin -> Maybe DerivingTrait
derivingTraitFromBuiltin b = derivingTraitFromBuiltinMap ^. at (toBuiltinPrim b)

instance IsBuiltin DerivingTrait where
  toBuiltinPrim :: DerivingTrait -> BuiltinPrim
  toBuiltinPrim = \case
    DerivingEq -> toBuiltinPrim BuiltinEq
    DerivingOrd -> toBuiltinPrim BuiltinOrd

instance Pretty DerivingTrait where
  pretty = pretty . toBuiltinPrim

instance PrettyCodeAnn DerivingTrait where
  ppCodeAnn = annotateKind KNameInductive . pretty

data PreStatement
  = PreFunctionDef FunctionDef
  | PreInductiveDef InductiveDef
  | PreAxiomDef AxiomDef

data Module' stmt = Module
  { _moduleId :: ModuleId,
    _moduleName :: Name,
    _moduleBody :: ModuleBody' stmt,
    _modulePragmas :: Pragmas
  }
  deriving stock (Data, Generic)

newtype Import = Import
  { _importModuleName :: Name
  }
  deriving stock (Data, Generic)

instance Serialize Import

instance NFData Import

data ModuleBody' stmt = ModuleBody
  { _moduleImports :: [Import],
    _moduleStatements :: [stmt]
  }
  deriving stock (Data, Generic)

data MutualStatement
  = StatementInductive InductiveDef
  | StatementFunction FunctionDef
  | StatementAxiom AxiomDef
  deriving stock (Generic, Data)

newtype MutualBlock = MutualBlock
  { _mutualStatements :: NonEmpty MutualStatement
  }
  deriving stock (Generic, Data)

newtype MutualBlockLet = MutualBlockLet
  { _mutualLet :: NonEmpty FunctionDef
  }
  deriving stock (Eq, Generic, Data)

instance Hashable MutualBlockLet

instance Serialize MutualBlockLet

instance NFData MutualBlockLet

data AxiomDef = AxiomDef
  { _axiomName :: AxiomName,
    _axiomBuiltin :: Maybe BuiltinAxiom,
    _axiomType :: Expression,
    _axiomPragmas :: Pragmas,
    _axiomDocComment :: Maybe Text
  }
  deriving stock (Data, Generic)

instance Serialize AxiomDef

instance NFData AxiomDef

data IsInstanceCoercion
  = IsInstanceCoercionInstance
  | IsInstanceCoercionCoercion
  deriving stock (Eq, Generic, Data)

instance Hashable IsInstanceCoercion

instance Serialize IsInstanceCoercion

instance NFData IsInstanceCoercion

data FunctionDef = FunctionDef
  { _funDefName :: FunctionName,
    _funDefType :: Expression,
    _funDefBody :: Expression,
    _funDefTerminating :: Bool,
    _funDefIsInstanceCoercion :: Maybe IsInstanceCoercion,
    _funDefBuiltin :: Maybe BuiltinFunction,
    _funDefArgsInfo :: [ArgInfo],
    _funDefPragmas :: Pragmas,
    _funDefDocComment :: Maybe Text
  }
  deriving stock (Eq, Generic, Data)

instance Hashable FunctionDef

instance Serialize FunctionDef

instance NFData FunctionDef

data Iden
  = IdenFunction Name
  | IdenConstructor Name
  | IdenVar VarName
  | IdenAxiom Name
  | IdenInductive Name
  deriving stock (Eq, Generic, Data)

getName :: Iden -> Name
getName = \case
  IdenFunction n -> n
  IdenConstructor n -> n
  IdenVar n -> n
  IdenAxiom n -> n
  IdenInductive n -> n

instance Hashable Iden

instance Serialize Iden

instance NFData Iden

data TypedExpression = TypedExpression
  { _typedType :: Expression,
    _typedExpression :: Expression
  }
  deriving stock (Generic, Data)

data LetClause
  = -- | Non-recursive let definition
    LetFunDef FunctionDef
  | LetMutualBlock MutualBlockLet
  deriving stock (Eq, Generic, Data)

instance Hashable LetClause

instance Serialize LetClause

instance NFData LetClause

data Let = Let
  { _letClauses :: NonEmpty LetClause,
    _letExpression :: Expression
  }
  deriving stock (Eq, Generic, Data)

instance Hashable Let

instance Serialize Let

instance NFData Let

type LiteralLoc = WithLoc Literal

data Literal
  = LitString Text
  | -- | `LitNumeric` represents a numeric literal of undetermined type
    LitNumeric Integer
  | -- | `LitInteger` represents a literal with trait `Integral`
    LitInteger Integer
  | -- | `LitNatural` represents a literal with trait `FromNatural`
    LitNatural Integer
  deriving stock (Show, Eq, Ord, Generic, Data)

instance Hashable Literal

instance Serialize Literal

instance NFData Literal

data Expression
  = ExpressionIden Iden
  | ExpressionApplication Application
  | ExpressionFunction Function
  | ExpressionLiteral LiteralLoc
  | ExpressionHole Hole
  | ExpressionInstanceHole InstanceHole
  | ExpressionLet Let
  | ExpressionUniverse SmallUniverse
  | ExpressionSimpleLambda SimpleLambda
  | ExpressionLambda Lambda
  | ExpressionCase Case
  deriving stock (Eq, Generic, Data)

instance Hashable Expression

instance Serialize Expression

instance NFData Expression

data SimpleBinder = SimpleBinder
  { _sbinderVar :: VarName,
    _sbinderType :: Expression
  }
  deriving stock (Eq, Generic, Data)

instance Serialize SimpleBinder

instance NFData SimpleBinder

data SimpleLambda = SimpleLambda
  { _slambdaBinder :: SimpleBinder,
    _slambdaBody :: Expression
  }
  deriving stock (Eq, Generic, Data)

instance Serialize SimpleLambda

instance NFData SimpleLambda

data SideIfBranch = SideIfBranch
  { _sideIfBranchCondition :: Expression,
    _sideIfBranchBody :: Expression
  }
  deriving stock (Eq, Generic, Data)

instance Serialize SideIfBranch

instance NFData SideIfBranch

instance Hashable SideIfBranch

data SideIfs = SideIfs
  { _sideIfBranches :: NonEmpty SideIfBranch,
    _sideIfElse :: Maybe Expression
  }
  deriving stock (Eq, Generic, Data)

instance Serialize SideIfs

instance NFData SideIfs

instance Hashable SideIfs

data CaseBranchRhs
  = CaseBranchRhsExpression Expression
  | CaseBranchRhsIf SideIfs
  deriving stock (Eq, Generic, Data)

instance Serialize CaseBranchRhs

instance NFData CaseBranchRhs

instance Hashable CaseBranchRhs

data CaseBranch = CaseBranch
  { _caseBranchPattern :: PatternArg,
    _caseBranchRhs :: CaseBranchRhs
  }
  deriving stock (Eq, Generic, Data)

instance Hashable CaseBranch

instance Serialize CaseBranch

instance NFData CaseBranch

data Case = Case
  { _caseExpression :: Expression,
    -- | The type of the cased expression. The typechecker fills this field
    _caseExpressionType :: Maybe Expression,
    -- | The type of the whole case expression. The typechecker fills this field
    _caseExpressionWholeType :: Maybe Expression,
    _caseBranches :: NonEmpty CaseBranch
  }
  deriving stock (Eq, Generic, Data)

instance Hashable Case

instance Serialize Case

instance NFData Case

data Lambda = Lambda
  { _lambdaClauses :: NonEmpty LambdaClause,
    -- | The typechecker fills this field
    _lambdaType :: Maybe Expression
  }
  deriving stock (Eq, Generic, Data)

data LambdaClause = LambdaClause
  { _lambdaPatterns :: NonEmpty PatternArg,
    _lambdaBody :: Expression
  }
  deriving stock (Eq, Generic, Data)

instance Hashable Lambda

instance Hashable LambdaClause

instance Hashable SimpleBinder

instance Hashable SimpleLambda

instance Serialize Lambda

instance NFData Lambda

instance Serialize LambdaClause

instance NFData LambdaClause

data Application = Application
  { _appLeft :: Expression,
    _appRight :: Expression,
    _appImplicit :: IsImplicit
  }
  deriving stock (Data, Generic)

instance Serialize Application

instance NFData Application

-- TODO: Eq and Hashable instances ignore the _appImplicit field
instance Eq Application where
  (Application l r _) == (Application l' r' _) = (l == l') && (r == r')

instance Hashable Application where
  hashWithSalt salt (Application l r _) = hashWithSalt salt (l, r)

-- | Fully applied constructor in a pattern.
data ConstructorApp = ConstructorApp
  { _constrAppConstructor :: Name,
    _constrAppParameters :: [PatternArg],
    -- | The type checker fills this field
    _constrAppType :: Maybe Expression
  }
  deriving stock (Eq, Generic, Data)

instance Hashable ConstructorApp

instance Serialize ConstructorApp

instance NFData ConstructorApp

data PatternArg = PatternArg
  { _patternArgIsImplicit :: IsImplicit,
    _patternArgName :: Maybe VarName,
    _patternArgPattern :: Pattern
  }
  deriving stock (Eq, Generic, Data)

instance Hashable PatternArg

instance Serialize PatternArg

instance NFData PatternArg

newtype WildcardConstructor = WildcardConstructor
  { _wildcardConstructor :: ConstrName
  }
  deriving stock (Eq, Generic, Data)

instance Hashable WildcardConstructor

instance Serialize WildcardConstructor

instance NFData WildcardConstructor

data Pattern
  = PatternVariable VarName
  | -- | PatternWildcardConstructor gets removed by the arity checker
    PatternWildcardConstructor WildcardConstructor
  | PatternConstructorApp ConstructorApp
  deriving stock (Eq, Generic, Data)

instance Hashable Pattern

instance Serialize Pattern

instance NFData Pattern

data InductiveParameter = InductiveParameter
  { _inductiveParamName :: VarName,
    _inductiveParamType :: Expression
  }
  deriving stock (Eq, Data, Generic)

instance Serialize InductiveParameter

instance NFData InductiveParameter

data InductiveDef = InductiveDef
  { _inductiveName :: InductiveName,
    _inductiveBuiltin :: Maybe BuiltinInductive,
    -- The universe of the inductive type, not the full kind
    _inductiveType :: Expression,
    _inductiveParameters :: [InductiveParameter],
    _inductiveConstructors :: [ConstructorDef],
    _inductivePositive :: Bool,
    _inductiveTrait :: Bool,
    _inductivePragmas :: Pragmas,
    _inductiveDocComment :: Maybe Text
  }
  deriving stock (Data)

data ConstructorDef = ConstructorDef
  { _inductiveConstructorName :: ConstrName,
    _inductiveConstructorType :: Expression,
    _inductiveConstructorIsRecord :: Bool,
    _inductiveConstructorPragmas :: Pragmas,
    _inductiveConstructorDocComment :: Maybe Text
  }
  deriving stock (Data)

data ArgInfo = ArgInfo
  { _argInfoDefault :: Maybe Expression,
    _argInfoName :: Maybe Name
  }
  deriving stock (Eq, Generic, Data)

emptyArgInfo :: ArgInfo
emptyArgInfo =
  ArgInfo
    { _argInfoDefault = Nothing,
      _argInfoName = Nothing
    }

instance Hashable ArgInfo

instance Serialize ArgInfo

instance NFData ArgInfo

data FunctionParameter = FunctionParameter
  { _paramName :: Maybe VarName,
    _paramImplicit :: IsImplicit,
    _paramType :: Expression
  }
  deriving stock (Eq, Generic, Data)

instance Hashable FunctionParameter

instance Serialize FunctionParameter

instance NFData FunctionParameter

data Function = Function
  { _functionLeft :: FunctionParameter,
    _functionRight :: Expression
  }
  deriving stock (Eq, Generic, Data)

instance Hashable Function

instance Serialize Function

instance NFData Function

newtype ModuleIndex = ModuleIndex
  { _moduleIxModule :: Module
  }
  deriving stock (Data)

-- | An expression that maybe has been normalized
data NormalizedExpression = NormalizedExpression
  { _normalizedExpression :: Expression,
    _normalizedExpressionOriginal :: Expression
  }

makePrisms ''Expression
makePrisms ''Iden
makePrisms ''MutualStatement

makeLenses ''SideIfBranch
makeLenses ''SideIfs
makeLenses ''CaseBranchRhs
makeLenses ''ModuleIndex
makeLenses ''ArgInfo
makeLenses ''WildcardConstructor
makeLenses ''Case
makeLenses ''CaseBranch
makeLenses ''Module'
makeLenses ''Let
makeLenses ''MutualBlockLet
makeLenses ''MutualBlock
makeLenses ''PatternArg
makeLenses ''Import
makeLenses ''FunctionDef
makeLenses ''InductiveDef
makeLenses ''AxiomDef
makeLenses ''ModuleBody'
makeLenses ''Application
makeLenses ''TypedExpression
makeLenses ''Function
makeLenses ''SimpleLambda
makeLenses ''SimpleBinder
makeLenses ''Lambda
makeLenses ''LambdaClause
makeLenses ''FunctionParameter
makeLenses ''InductiveParameter
makeLenses ''ConstructorDef
makeLenses ''ConstructorApp
makeLenses ''NormalizedExpression

instance Eq ModuleIndex where
  (==) = (==) `on` (^. moduleIxModule . moduleName)

instance Hashable ModuleIndex where
  hashWithSalt s = hashWithSalt s . (^. moduleIxModule . moduleName)

deriving newtype instance (Eq Import)

deriving newtype instance (Hashable Import)

instance HasAtomicity Case where
  atomicity = const Atom

instance HasAtomicity Application where
  atomicity = const (Aggregate appFixity)

instance HasAtomicity SimpleLambda where
  atomicity = const Atom

instance HasAtomicity Let where
  atomicity = const (Aggregate letFixity)

instance HasAtomicity Literal where
  atomicity = \case
    LitNumeric {} -> Atom
    LitNatural {} -> Atom
    LitInteger {} -> Atom
    LitString {} -> Atom

instance HasAtomicity Lambda where
  atomicity = const Atom

instance HasAtomicity Expression where
  atomicity e = case e of
    ExpressionIden {} -> Atom
    ExpressionApplication a -> atomicity a
    ExpressionLiteral l -> atomicity l
    ExpressionLet l -> atomicity l
    ExpressionHole {} -> Atom
    ExpressionInstanceHole {} -> Atom
    ExpressionUniverse u -> atomicity u
    ExpressionFunction f -> atomicity f
    ExpressionSimpleLambda l -> atomicity l
    ExpressionLambda l -> atomicity l
    ExpressionCase l -> atomicity l

instance HasAtomicity Function where
  atomicity = const (Aggregate funFixity)

instance HasAtomicity ConstructorApp where
  atomicity ConstructorApp {..}
    | null _constrAppParameters = Atom
    | otherwise = Aggregate appFixity

instance HasAtomicity PatternArg where
  atomicity p
    | isImplicitOrInstance (p ^. patternArgIsImplicit) = Atom
    | isJust (p ^. patternArgName) = Atom
    | otherwise = atomicity (p ^. patternArgPattern)

instance HasAtomicity Pattern where
  atomicity p = case p of
    PatternConstructorApp a -> atomicity a
    PatternVariable {} -> Atom
    PatternWildcardConstructor {} -> Atom

instance HasLoc Module where
  getLoc m = getLoc (m ^. moduleName) <>? fmap getLocSpan (nonEmpty (m ^. moduleBody . moduleStatements))

instance HasLoc MutualBlock where
  getLoc = getLocSpan . (^. mutualStatements)

instance HasLoc MutualStatement where
  getLoc = \case
    StatementInductive i -> getLoc i
    StatementFunction f -> getLoc f
    StatementAxiom a -> getLoc a

instance HasLoc InductiveDef where
  getLoc d =
    getLoc (d ^. inductiveName)
      <>? (getLoc . (^. last1) <$> (nonEmpty (d ^. inductiveConstructors)))

instance HasLoc NormalizedExpression where
  getLoc = getLoc . (^. normalizedExpressionOriginal)

instance HasLoc AxiomDef where
  getLoc a = getLoc (a ^. axiomName) <> getLoc (a ^. axiomType)

instance HasLoc ConstructorDef where
  getLoc ConstructorDef {..} =
    getLoc _inductiveConstructorName <> getLoc _inductiveConstructorType

instance HasLoc InductiveParameter where
  getLoc InductiveParameter {..} = getLoc _inductiveParamName <> getLoc _inductiveParamType

instance HasLoc FunctionParameter where
  getLoc f = v (getLoc (f ^. paramType))
    where
      v = case getLoc <$> f ^. paramName of
        Nothing -> id
        Just i -> (i <>)

instance HasLoc Function where
  getLoc (Function l r) = getLoc l <> getLoc r

instance HasLoc Application where
  getLoc (Application l r _) = getLoc l <> getLoc r

instance HasLoc SimpleBinder where
  getLoc l = getLoc (l ^. sbinderVar) <> getLoc (l ^. sbinderType)

instance HasLoc SimpleLambda where
  getLoc l = getLoc (l ^. slambdaBinder) <> getLoc (l ^. slambdaBody)

instance HasLoc LambdaClause where
  getLoc (LambdaClause ps e) = getLocSpan ps <> getLoc e

instance HasLoc Lambda where
  getLoc l = getLocSpan (l ^. lambdaClauses)

instance HasLoc FunctionDef where
  getLoc f = getLoc (f ^. funDefName) <> getLoc (f ^. funDefBody)

instance HasLoc MutualBlockLet where
  getLoc (MutualBlockLet defs) = getLocSpan defs

instance HasLoc LetClause where
  getLoc = \case
    LetFunDef f -> getLoc f
    LetMutualBlock f -> getLoc f

instance HasLoc Let where
  getLoc l = getLocSpan (l ^. letClauses) <> getLoc (l ^. letExpression)

instance HasLoc SideIfBranch where
  getLoc SideIfBranch {..} =
    getLoc _sideIfBranchCondition
      <> getLoc _sideIfBranchBody

instance HasLoc SideIfs where
  getLoc SideIfs {..} =
    getLocSpan _sideIfBranches
      <>? (getLoc <$> _sideIfElse)

instance HasLoc CaseBranchRhs where
  getLoc = \case
    CaseBranchRhsExpression e -> getLoc e
    CaseBranchRhsIf e -> getLoc e

instance HasLoc CaseBranch where
  getLoc c = getLoc (c ^. caseBranchPattern) <> getLoc (c ^. caseBranchRhs)

instance HasLoc Case where
  getLoc c = getLocSpan (c ^. caseBranches)

instance HasLoc Expression where
  getLoc = \case
    ExpressionIden i -> getLoc i
    ExpressionApplication a -> getLoc a
    ExpressionLiteral l -> getLoc l
    ExpressionHole h -> getLoc h
    ExpressionInstanceHole h -> getLoc h
    ExpressionLet l -> getLoc l
    ExpressionUniverse u -> getLoc u
    ExpressionFunction u -> getLoc u
    ExpressionSimpleLambda l -> getLoc l
    ExpressionLambda l -> getLoc l
    ExpressionCase l -> getLoc l

instance HasLoc Iden where
  getLoc = \case
    IdenFunction f -> getLoc f
    IdenConstructor c -> getLoc c
    IdenVar v -> getLoc v
    IdenAxiom a -> getLoc a
    IdenInductive a -> getLoc a

instance HasLoc WildcardConstructor where
  getLoc WildcardConstructor {..} = getLoc _wildcardConstructor

instance HasLoc Pattern where
  getLoc = \case
    PatternVariable v -> getLoc v
    PatternConstructorApp a -> getLoc a
    PatternWildcardConstructor a -> getLoc a

instance HasLoc PatternArg where
  getLoc a = fmap getLoc (a ^. patternArgName) ?<> getLoc (a ^. patternArgPattern)

instance HasLoc ConstructorApp where
  getLoc ConstructorApp {..} =
    case last <$> nonEmpty _constrAppParameters of
      Just p -> getLoc _constrAppConstructor <> getLoc p
      Nothing -> getLoc _constrAppConstructor

idenName :: Lens' Iden Name
idenName f = \case
  IdenFunction g -> IdenFunction <$> f g
  IdenConstructor c -> IdenConstructor <$> f c
  IdenVar v -> IdenVar <$> f v
  IdenInductive i -> IdenInductive <$> f i
  IdenAxiom a -> IdenAxiom <$> f a
