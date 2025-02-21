{-# LANGUAGE UndecidableInstances #-}

module Juvix.Compiler.Concrete.Language.Base
  ( module Juvix.Compiler.Concrete.Language.Base,
    module Juvix.Data.FixityInfo,
    module Juvix.Compiler.Concrete.Data.IsOpenShort,
    module Juvix.Compiler.Concrete.Data.LocalModuleOrigin,
    module Juvix.Data.IteratorInfo,
    module Juvix.Compiler.Concrete.Data.IfBranchKind,
    module Juvix.Compiler.Concrete.Data.Name,
    module Juvix.Compiler.Concrete.Data.Stage,
    module Juvix.Compiler.Concrete.Data.NameRef,
    module Juvix.Data.Keyword,
    module Juvix.Compiler.Concrete.Data.Builtins,
    module Juvix.Compiler.Concrete.Data.Literal,
    module Juvix.Data,
    module Juvix.Compiler.Concrete.Data.VisibilityAnn,
    module Juvix.Compiler.Concrete.Data.PublicAnn,
    module Juvix.Compiler.Concrete.Data.ModuleIsTop,
    module Juvix.Data.Fixity,
  )
where

import Juvix.Compiler.Backend.Markdown.Data.Types (Mk)
import Juvix.Compiler.Concrete.Data.Builtins
import Juvix.Compiler.Concrete.Data.IfBranchKind
import Juvix.Compiler.Concrete.Data.IsOpenShort
import Juvix.Compiler.Concrete.Data.Literal
import Juvix.Compiler.Concrete.Data.LocalModuleOrigin
import Juvix.Compiler.Concrete.Data.ModuleIsTop
import Juvix.Compiler.Concrete.Data.Name
import Juvix.Compiler.Concrete.Data.NameRef
import Juvix.Compiler.Concrete.Data.PublicAnn
import Juvix.Compiler.Concrete.Data.ScopedName qualified as S
import Juvix.Compiler.Concrete.Data.Stage
import Juvix.Compiler.Concrete.Data.VisibilityAnn
import Juvix.Data
import Juvix.Data.Fixity
import Juvix.Data.FixityInfo (Arity (..), FixityInfo)
import Juvix.Data.IteratorInfo
import Juvix.Data.Keyword
import Juvix.Extra.Serialize as Ser
import Juvix.Prelude hiding (show)
import Juvix.Prelude.Pretty (Pretty, pretty)

type Delims = Irrelevant (Maybe (KeywordRef, KeywordRef))

type RecordUpdateExtraType :: Stage -> GHCType
type family RecordUpdateExtraType s = res | res -> s where
  RecordUpdateExtraType 'Parsed = ()
  RecordUpdateExtraType 'Scoped = RecordUpdateExtra

type ReservedInductiveDefType :: Stage -> GHCType
type family ReservedInductiveDefType s = res | res -> s where
  ReservedInductiveDefType 'Parsed = ReservedInductiveDef
  ReservedInductiveDefType 'Scoped = Void

type FieldArgIxType :: Stage -> GHCType
type family FieldArgIxType s = res | res -> s where
  FieldArgIxType 'Parsed = ()
  FieldArgIxType 'Scoped = Int

type SideIfBranchConditionType :: Stage -> IfBranchKind -> GHCType
type family SideIfBranchConditionType s k = res where
  SideIfBranchConditionType s 'BranchIfBool = ExpressionType s
  SideIfBranchConditionType _ 'BranchIfElse = ()

type IfBranchConditionType :: Stage -> IfBranchKind -> GHCType
type family IfBranchConditionType s k = res where
  IfBranchConditionType s 'BranchIfBool = ExpressionType s
  IfBranchConditionType _ 'BranchIfElse = Irrelevant KeywordRef

type ModuleIdType :: Stage -> ModuleIsTop -> GHCType
type family ModuleIdType s t = res where
  ModuleIdType 'Parsed _ = ()
  ModuleIdType 'Scoped 'ModuleLocal = ()
  ModuleIdType 'Scoped 'ModuleTop = ModuleId

type PunSymbolType :: Stage -> GHCType
type family PunSymbolType s = res | res -> s where
  PunSymbolType 'Parsed = ()
  PunSymbolType 'Scoped = ScopedIden

type SymbolType :: Stage -> GHCType
type family SymbolType s = res | res -> s where
  SymbolType 'Parsed = Symbol
  SymbolType 'Scoped = S.Symbol

type FunctionSymbolType :: Stage -> GHCType
type family FunctionSymbolType s = res | res -> s where
  FunctionSymbolType 'Parsed = FunctionDefNameParsed
  FunctionSymbolType 'Scoped = FunctionDefNameScoped

type IdentifierType :: Stage -> GHCType
type family IdentifierType s = res | res -> s where
  IdentifierType 'Parsed = Name
  IdentifierType 'Scoped = ScopedIden

type HoleType :: Stage -> GHCType
type family HoleType s = res | res -> s where
  HoleType 'Parsed = KeywordRef
  HoleType 'Scoped = Hole

type PatternAtomIdenType :: Stage -> GHCType
type family PatternAtomIdenType s = res | res -> s where
  PatternAtomIdenType 'Parsed = Name
  PatternAtomIdenType 'Scoped = PatternScopedIden

type ExpressionType :: Stage -> GHCType
type family ExpressionType s = res | res -> s where
  ExpressionType 'Parsed = ExpressionAtoms 'Parsed
  ExpressionType 'Scoped = Expression

type PatternAtomType :: Stage -> GHCType
type family PatternAtomType s = res | res -> s where
  PatternAtomType 'Parsed = PatternAtom 'Parsed
  PatternAtomType 'Scoped = PatternArg

type PatternParensType :: Stage -> GHCType
type family PatternParensType s = res | res -> s where
  PatternParensType 'Parsed = PatternAtoms 'Parsed
  PatternParensType 'Scoped = PatternArg

type PatternAtType :: Stage -> GHCType
type family PatternAtType s = res | res -> s where
  PatternAtType 'Parsed = PatternBinding
  PatternAtType 'Scoped = PatternArg

type NameSignatureType :: Stage -> GHCType
type family NameSignatureType s = res | res -> s where
  NameSignatureType 'Parsed = ()
  NameSignatureType 'Scoped = NameSignature 'Scoped

type ModulePathType :: Stage -> ModuleIsTop -> GHCType
type family ModulePathType s t = res | res -> t s where
  ModulePathType 'Parsed 'ModuleTop = TopModulePath
  ModulePathType 'Scoped 'ModuleTop = S.TopModulePath
  ModulePathType 'Parsed 'ModuleLocal = Symbol
  ModulePathType 'Scoped 'ModuleLocal = S.Symbol

type OpenModuleNameType :: Stage -> IsOpenShort -> GHCType
type family OpenModuleNameType s short = res where
  OpenModuleNameType s 'OpenFull = ModuleNameType s
  OpenModuleNameType _ 'OpenShort = ()

type ModuleNameType :: Stage -> GHCType
type family ModuleNameType s = res | res -> s where
  ModuleNameType 'Parsed = Name
  ModuleNameType 'Scoped = S.Name

type ModuleInductiveType :: ModuleIsTop -> GHCType
type family ModuleInductiveType t = res | res -> t where
  ModuleInductiveType 'ModuleTop = ()
  ModuleInductiveType 'ModuleLocal = LocalModuleOrigin

type ModuleEndType :: ModuleIsTop -> GHCType
type family ModuleEndType t = res | res -> t where
  ModuleEndType 'ModuleTop = ()
  ModuleEndType 'ModuleLocal = KeywordRef

-- | We keep the exact source of the pragma text. This is necessary, because
-- pragmas are supposed to be backwards-compatible. Unrecognised pragmas
-- should be ignored, but they still need to be printed out when
-- pretty-printing. Also, we probably don't want to impose pragma formatting
-- choices on the user.
type ParsedPragmas = WithLoc (WithSource Pragmas)

data NameItem (s :: Stage) = NameItem
  { -- | The symbol cannot be omitted for explicit arguments
    _nameItemSymbol :: Maybe (SymbolType s),
    -- | NOTE the index is wrt to the block, not the whole signature
    _nameItemIndex :: Int,
    _nameItemImplicit :: IsImplicit,
    _nameItemType :: ExpressionType s,
    _nameItemDefault :: Maybe (ArgDefault s)
  }
  deriving stock (Generic)

instance Serialize (NameItem 'Scoped)

instance NFData (NameItem 'Scoped)

instance Serialize (NameItem 'Parsed)

instance NFData (NameItem 'Parsed)

data NameBlock (s :: Stage) = NameBlock
  { _nameBlockItems :: NonEmpty (NameItem s),
    _nameBlockImplicit :: IsImplicit
  }
  deriving stock (Generic)

instance Serialize (NameBlock 'Scoped)

instance NFData (NameBlock 'Scoped)

instance Serialize (NameBlock 'Parsed)

instance NFData (NameBlock 'Parsed)

-- | Two consecutive blocks should have different implicitness
newtype NameSignature (s :: Stage) = NameSignature
  { _nameSignatureArgs :: [NameBlock s]
  }
  deriving stock (Generic)

instance Serialize (NameSignature 'Scoped)

instance NFData (NameSignature 'Scoped)

instance Serialize (NameSignature 'Parsed)

instance NFData (NameSignature 'Parsed)

newtype RecordNameSignature s = RecordNameSignature
  { _recordNames :: HashMap Symbol (NameItem s)
  }
  deriving stock (Generic)

instance Serialize (RecordNameSignature 'Scoped)

instance NFData (RecordNameSignature 'Scoped)

instance Serialize (RecordNameSignature 'Parsed)

instance NFData (RecordNameSignature 'Parsed)

data RecordInfo = RecordInfo
  { _recordInfoConstructor :: S.Symbol,
    _recordInfoSignature :: RecordNameSignature 'Parsed
  }
  deriving stock (Generic)

instance Serialize RecordInfo

instance NFData RecordInfo

data Argument (s :: Stage)
  = ArgumentSymbol (SymbolType s)
  | ArgumentWildcard Wildcard
  deriving stock (Generic)

instance Serialize (Argument 'Scoped)

instance NFData (Argument 'Scoped)

instance Serialize (Argument 'Parsed)

instance NFData (Argument 'Parsed)

deriving stock instance Show (Argument 'Parsed)

deriving stock instance Show (Argument 'Scoped)

deriving stock instance Eq (Argument 'Parsed)

deriving stock instance Eq (Argument 'Scoped)

deriving stock instance Ord (Argument 'Parsed)

deriving stock instance Ord (Argument 'Scoped)

newtype Statements (s :: Stage) = Statements
  { _statements :: [Statement s]
  }

data Statement (s :: Stage)
  = StatementSyntax (SyntaxDef s)
  | StatementFunctionDef (FunctionDef s)
  | StatementDeriving (Deriving s)
  | StatementImport (Import s)
  | StatementInductive (InductiveDef s)
  | StatementReservedInductive (ReservedInductiveDefType s)
  | StatementModule (Module s 'ModuleLocal)
  | StatementOpenModule (OpenModule s 'OpenFull)
  | StatementAxiom (AxiomDef s)
  | StatementProjectionDef (ProjectionDef s)
  deriving stock (Generic)

instance Serialize (Statement 'Scoped)

instance NFData (Statement 'Scoped)

deriving stock instance Show (Statement 'Parsed)

deriving stock instance Show (Statement 'Scoped)

deriving stock instance Eq (Statement 'Parsed)

deriving stock instance Eq (Statement 'Scoped)

deriving stock instance Ord (Statement 'Parsed)

deriving stock instance Ord (Statement 'Scoped)

data ReservedInductiveDef = ReservedInductiveDef
  { _reservedInductiveDef :: InductiveDef 'Parsed,
    _reservedInductiveConstructors :: NonEmpty S.Symbol,
    _reservedInductiveDefModule :: Module 'Parsed 'ModuleLocal
  }
  deriving stock (Show, Eq, Ord)

data ProjectionDef s = ProjectionDef
  { _projectionConstructor :: SymbolType s,
    _projectionField :: SymbolType s,
    _projectionType :: ExpressionType s,
    _projectionKind :: ProjectionKind,
    _projectionFieldIx :: Int,
    _projectionFieldBuiltin :: Maybe (WithLoc BuiltinFunction),
    _projectionDoc :: Maybe (Judoc s),
    _projectionPragmas :: Maybe ParsedPragmas
  }
  deriving stock (Generic)

instance Serialize (ProjectionDef 'Scoped)

instance NFData (ProjectionDef 'Scoped)

deriving stock instance Show (ProjectionDef 'Parsed)

deriving stock instance Show (ProjectionDef 'Scoped)

deriving stock instance Eq (ProjectionDef 'Parsed)

deriving stock instance Eq (ProjectionDef 'Scoped)

deriving stock instance Ord (ProjectionDef 'Parsed)

deriving stock instance Ord (ProjectionDef 'Scoped)

data Import (s :: Stage) = Import
  { _importKw :: KeywordRef,
    _importModulePath :: ModulePathType s 'ModuleTop,
    _importAsName :: Maybe (ModulePathType s 'ModuleTop),
    _importUsingHiding :: Maybe (UsingHiding s),
    _importPublic :: PublicAnn,
    _importOpen :: Maybe (OpenModule s 'OpenShort)
  }
  deriving stock (Generic)

instance Serialize (Import 'Scoped)

instance NFData (Import 'Scoped)

deriving stock instance Show (Import 'Parsed)

deriving stock instance Show (Import 'Scoped)

deriving stock instance Eq (Import 'Parsed)

deriving stock instance Eq (Import 'Scoped)

deriving stock instance Ord (Import 'Parsed)

deriving stock instance Ord (Import 'Scoped)

data AliasDef (s :: Stage) = AliasDef
  { _aliasDefSyntaxKw :: Irrelevant KeywordRef,
    _aliasDefAliasKw :: Irrelevant KeywordRef,
    _aliasDefDoc :: Maybe (Judoc s),
    _aliasDefName :: SymbolType s,
    _aliasDefAsName :: IdentifierType s
  }
  deriving stock (Generic)

instance Serialize (AliasDef 'Scoped)

instance NFData (AliasDef 'Scoped)

instance Serialize (AliasDef 'Parsed)

instance NFData (AliasDef 'Parsed)

deriving stock instance (Show (AliasDef 'Parsed))

deriving stock instance (Show (AliasDef 'Scoped))

deriving stock instance (Eq (AliasDef 'Parsed))

deriving stock instance (Eq (AliasDef 'Scoped))

deriving stock instance (Ord (AliasDef 'Parsed))

deriving stock instance (Ord (AliasDef 'Scoped))

data ParsedIteratorInfo = ParsedIteratorInfo
  { _parsedIteratorInfoInitNum :: Maybe (WithLoc Int),
    _parsedIteratorInfoRangeNum :: Maybe (WithLoc Int),
    _parsedIteratorInfoBraces :: Irrelevant (KeywordRef, KeywordRef)
  }
  deriving stock (Show, Eq, Ord, Generic)

instance Serialize ParsedIteratorInfo

instance NFData ParsedIteratorInfo

data SyntaxDef (s :: Stage)
  = SyntaxFixity (FixitySyntaxDef s)
  | SyntaxOperator (OperatorSyntaxDef s)
  | SyntaxIterator (IteratorSyntaxDef s)
  | SyntaxAlias (AliasDef s)
  deriving stock (Generic)

instance NFData (SyntaxDef 'Scoped)

instance Serialize (SyntaxDef 'Scoped)

deriving stock instance (Show (SyntaxDef 'Parsed))

deriving stock instance (Show (SyntaxDef 'Scoped))

deriving stock instance (Eq (SyntaxDef 'Parsed))

deriving stock instance (Eq (SyntaxDef 'Scoped))

deriving stock instance (Ord (SyntaxDef 'Parsed))

deriving stock instance (Ord (SyntaxDef 'Scoped))

data ParsedFixityFields (s :: Stage) = ParsedFixityFields
  { _fixityFieldsAssoc :: Maybe BinaryAssoc,
    _fixityFieldsPrecSame :: Maybe (IdentifierType s),
    _fixityFieldsPrecBelow :: Maybe [IdentifierType s],
    _fixityFieldsPrecAbove :: Maybe [IdentifierType s],
    _fixityFieldsBraces :: Irrelevant (KeywordRef, KeywordRef)
  }
  deriving stock (Generic)

instance NFData (ParsedFixityFields 'Scoped)

instance Serialize (ParsedFixityFields 'Scoped)

deriving stock instance (Show (ParsedFixityFields 'Parsed))

deriving stock instance (Show (ParsedFixityFields 'Scoped))

deriving stock instance (Eq (ParsedFixityFields 'Parsed))

deriving stock instance (Eq (ParsedFixityFields 'Scoped))

deriving stock instance (Ord (ParsedFixityFields 'Parsed))

deriving stock instance (Ord (ParsedFixityFields 'Scoped))

data ParsedFixityInfo (s :: Stage) = ParsedFixityInfo
  { _fixityParsedArity :: WithLoc Arity,
    _fixityFields :: Maybe (ParsedFixityFields s)
  }
  deriving stock (Generic)

instance NFData (ParsedFixityInfo 'Scoped)

instance Serialize (ParsedFixityInfo 'Scoped)

deriving stock instance (Show (ParsedFixityInfo 'Parsed))

deriving stock instance (Show (ParsedFixityInfo 'Scoped))

deriving stock instance (Eq (ParsedFixityInfo 'Parsed))

deriving stock instance (Eq (ParsedFixityInfo 'Scoped))

deriving stock instance (Ord (ParsedFixityInfo 'Parsed))

deriving stock instance (Ord (ParsedFixityInfo 'Scoped))

data FixitySyntaxDef (s :: Stage) = FixitySyntaxDef
  { _fixitySymbol :: SymbolType s,
    _fixityDoc :: Maybe (Judoc s),
    _fixityInfo :: ParsedFixityInfo s,
    _fixityKw :: KeywordRef,
    _fixityAssignKw :: KeywordRef,
    _fixitySyntaxKw :: KeywordRef
  }
  deriving stock (Generic)

instance NFData (FixitySyntaxDef 'Scoped)

instance Serialize (FixitySyntaxDef 'Scoped)

deriving stock instance (Show (FixitySyntaxDef 'Parsed))

deriving stock instance (Show (FixitySyntaxDef 'Scoped))

deriving stock instance (Eq (FixitySyntaxDef 'Parsed))

deriving stock instance (Eq (FixitySyntaxDef 'Scoped))

deriving stock instance (Ord (FixitySyntaxDef 'Parsed))

deriving stock instance (Ord (FixitySyntaxDef 'Scoped))

data FixityDef = FixityDef
  { _fixityDefSymbol :: S.Symbol,
    _fixityDefFixity :: Fixity,
    -- | Used internally for printing parentheses.
    _fixityDefPrec :: Int
  }
  deriving stock (Show, Eq, Ord, Generic)

instance Serialize FixityDef

instance NFData FixityDef

data OperatorSyntaxDef (s :: Stage) = OperatorSyntaxDef
  { _opSymbol :: IdentifierType s,
    _opFixity :: IdentifierType s,
    _opDoc :: Maybe (Judoc s),
    _opKw :: KeywordRef,
    _opSyntaxKw :: KeywordRef
  }
  deriving stock (Generic)

deriving stock instance Show (OperatorSyntaxDef 'Parsed)

deriving stock instance Show (OperatorSyntaxDef 'Scoped)

deriving stock instance Eq (OperatorSyntaxDef 'Parsed)

deriving stock instance Eq (OperatorSyntaxDef 'Scoped)

deriving stock instance Ord (OperatorSyntaxDef 'Parsed)

deriving stock instance Ord (OperatorSyntaxDef 'Scoped)

instance Serialize (OperatorSyntaxDef 'Parsed)

instance NFData (OperatorSyntaxDef 'Parsed)

instance Serialize (OperatorSyntaxDef 'Scoped)

instance NFData (OperatorSyntaxDef 'Scoped)

data IteratorSyntaxDef (s :: Stage) = IteratorSyntaxDef
  { _iterSymbol :: IdentifierType s,
    _iterInfo :: Maybe ParsedIteratorInfo,
    _iterDoc :: Maybe (Judoc s),
    _iterSyntaxKw :: KeywordRef,
    _iterIteratorKw :: KeywordRef
  }
  deriving stock (Generic)

deriving stock instance Show (IteratorSyntaxDef 'Parsed)

deriving stock instance Show (IteratorSyntaxDef 'Scoped)

deriving stock instance Eq (IteratorSyntaxDef 'Parsed)

deriving stock instance Eq (IteratorSyntaxDef 'Scoped)

deriving stock instance Ord (IteratorSyntaxDef 'Parsed)

deriving stock instance Ord (IteratorSyntaxDef 'Scoped)

instance Serialize (IteratorSyntaxDef 'Parsed)

instance NFData (IteratorSyntaxDef 'Parsed)

instance Serialize (IteratorSyntaxDef 'Scoped)

instance NFData (IteratorSyntaxDef 'Scoped)

data ArgDefault (s :: Stage) = ArgDefault
  { _argDefaultAssign :: Irrelevant KeywordRef,
    _argDefaultValue :: ExpressionType s
  }
  deriving stock (Generic)

instance Serialize (ArgDefault 'Scoped)

instance NFData (ArgDefault 'Scoped)

instance Serialize (ArgDefault 'Parsed)

instance NFData (ArgDefault 'Parsed)

deriving stock instance Show (ArgDefault 'Parsed)

deriving stock instance Show (ArgDefault 'Scoped)

deriving stock instance Eq (ArgDefault 'Parsed)

deriving stock instance Eq (ArgDefault 'Scoped)

deriving stock instance Ord (ArgDefault 'Parsed)

deriving stock instance Ord (ArgDefault 'Scoped)

-- | This type makes explicit that only instance arguments are allowed to not
-- have a name
data SigArgNames (s :: Stage)
  = SigArgNamesInstance
  | SigArgNames (NonEmpty (Argument s))
  deriving stock (Generic)

instance Serialize (SigArgNames 'Scoped)

instance NFData (SigArgNames 'Scoped)

instance Serialize (SigArgNames 'Parsed)

instance NFData (SigArgNames 'Parsed)

deriving stock instance Show (SigArgNames 'Parsed)

deriving stock instance Show (SigArgNames 'Scoped)

deriving stock instance Eq (SigArgNames 'Parsed)

deriving stock instance Eq (SigArgNames 'Scoped)

deriving stock instance Ord (SigArgNames 'Parsed)

deriving stock instance Ord (SigArgNames 'Scoped)

data SigArg (s :: Stage) = SigArg
  { _sigArgDelims :: Irrelevant (KeywordRef, KeywordRef),
    _sigArgImplicit :: IsImplicit,
    _sigArgNames :: SigArgNames s,
    _sigArgColon :: Maybe (Irrelevant KeywordRef),
    -- | The type is only optional for implicit arguments. Omitting the rhs is
    -- equivalent to writing `: Type`.
    _sigArgType :: Maybe (ExpressionType s),
    _sigArgDefault :: Maybe (ArgDefault s)
  }
  deriving stock (Generic)

instance Serialize (SigArg 'Scoped)

instance NFData (SigArg 'Scoped)

instance Serialize (SigArg 'Parsed)

instance NFData (SigArg 'Parsed)

deriving stock instance Show (SigArg 'Parsed)

deriving stock instance Show (SigArg 'Scoped)

deriving stock instance Eq (SigArg 'Parsed)

deriving stock instance Eq (SigArg 'Scoped)

deriving stock instance Ord (SigArg 'Parsed)

deriving stock instance Ord (SigArg 'Scoped)

data TypeSig (s :: Stage) = TypeSig
  { _typeSigArgs :: [SigArg s],
    _typeSigColonKw :: Irrelevant (Maybe KeywordRef),
    _typeSigRetType :: Maybe (ExpressionType s)
  }
  deriving stock (Generic)

instance Serialize (TypeSig 'Scoped)

instance NFData (TypeSig 'Scoped)

instance Serialize (TypeSig 'Parsed)

instance NFData (TypeSig 'Parsed)

deriving stock instance Show (TypeSig 'Parsed)

deriving stock instance Show (TypeSig 'Scoped)

deriving stock instance Eq (TypeSig 'Parsed)

deriving stock instance Eq (TypeSig 'Scoped)

deriving stock instance Ord (TypeSig 'Parsed)

deriving stock instance Ord (TypeSig 'Scoped)

data FunctionClause (s :: Stage) = FunctionClause
  { _clausenPipeKw :: Irrelevant KeywordRef,
    _clausenPatterns :: NonEmpty (PatternAtomType s),
    _clausenAssignKw :: Irrelevant KeywordRef,
    _clausenBody :: ExpressionType s
  }
  deriving stock (Generic)

instance Serialize (FunctionClause 'Scoped)

instance NFData (FunctionClause 'Scoped)

instance Serialize (FunctionClause 'Parsed)

instance NFData (FunctionClause 'Parsed)

deriving stock instance Show (FunctionClause 'Parsed)

deriving stock instance Show (FunctionClause 'Scoped)

deriving stock instance Eq (FunctionClause 'Parsed)

deriving stock instance Eq (FunctionClause 'Scoped)

deriving stock instance Ord (FunctionClause 'Parsed)

deriving stock instance Ord (FunctionClause 'Scoped)

data FunctionDefBody (s :: Stage)
  = SigBodyExpression (ExpressionType s)
  | SigBodyClauses (NonEmpty (FunctionClause s))
  deriving stock (Generic)

instance Serialize (FunctionDefBody 'Scoped)

instance NFData (FunctionDefBody 'Scoped)

instance Serialize (FunctionDefBody 'Parsed)

instance NFData (FunctionDefBody 'Parsed)

deriving stock instance Show (FunctionDefBody 'Parsed)

deriving stock instance Show (FunctionDefBody 'Scoped)

deriving stock instance Eq (FunctionDefBody 'Parsed)

deriving stock instance Eq (FunctionDefBody 'Scoped)

deriving stock instance Ord (FunctionDefBody 'Parsed)

deriving stock instance Ord (FunctionDefBody 'Scoped)

data Deriving (s :: Stage) = Deriving
  { _derivingPragmas :: Maybe ParsedPragmas,
    _derivingKw :: KeywordRef,
    _derivingFunLhs :: FunctionLhs s
  }
  deriving stock (Generic)

instance Serialize (Deriving 'Scoped)

instance NFData (Deriving 'Scoped)

instance Serialize (Deriving 'Parsed)

instance NFData (Deriving 'Parsed)

deriving stock instance Show (Deriving 'Parsed)

deriving stock instance Show (Deriving 'Scoped)

deriving stock instance Eq (Deriving 'Parsed)

deriving stock instance Eq (Deriving 'Scoped)

deriving stock instance Ord (Deriving 'Parsed)

deriving stock instance Ord (Deriving 'Scoped)

data FunctionDefNameParsed
  = FunctionDefNamePattern (PatternAtom 'Parsed)
  | FunctionDefName Symbol
  deriving stock (Eq, Ord, Show, Generic)

instance Serialize FunctionDefNameParsed

instance NFData FunctionDefNameParsed

data FunctionDefNameScoped = FunctionDefNameScoped
  { _functionDefNameScoped :: S.Symbol,
    _functionDefNamePattern :: Maybe PatternArg
  }
  deriving stock (Eq, Ord, Show, Generic)

instance Serialize FunctionDefNameScoped

instance NFData FunctionDefNameScoped

data FunctionIsTop
  = FunctionTop
  | FunctionNotTop
  deriving stock (Eq, Ord, Show, Generic)

instance Serialize FunctionIsTop

instance NFData FunctionIsTop

data FunctionDef (s :: Stage) = FunctionDef
  { _functionDefDoc :: Maybe (Judoc s),
    _functionDefPragmas :: Maybe ParsedPragmas,
    _functionDefLhs :: FunctionLhs s,
    _functionDefBody :: FunctionDefBody s
  }
  deriving stock (Generic)

instance Serialize (FunctionDef 'Scoped)

instance NFData (FunctionDef 'Scoped)

instance Serialize (FunctionDef 'Parsed)

instance NFData (FunctionDef 'Parsed)

deriving stock instance Show (FunctionDef 'Parsed)

deriving stock instance Show (FunctionDef 'Scoped)

deriving stock instance Eq (FunctionDef 'Parsed)

deriving stock instance Eq (FunctionDef 'Scoped)

deriving stock instance Ord (FunctionDef 'Parsed)

deriving stock instance Ord (FunctionDef 'Scoped)

data AxiomDef (s :: Stage) = AxiomDef
  { _axiomKw :: Irrelevant KeywordRef,
    _axiomDoc :: Maybe (Judoc s),
    _axiomPragmas :: Maybe ParsedPragmas,
    _axiomName :: SymbolType s,
    _axiomTypeSig :: TypeSig s,
    _axiomBuiltin :: Maybe (WithLoc BuiltinAxiom)
  }
  deriving stock (Generic)

instance Serialize (AxiomDef 'Scoped)

instance NFData (AxiomDef 'Scoped)

deriving stock instance Show (AxiomDef 'Parsed)

deriving stock instance Show (AxiomDef 'Scoped)

deriving stock instance Eq (AxiomDef 'Parsed)

deriving stock instance Eq (AxiomDef 'Scoped)

deriving stock instance Ord (AxiomDef 'Parsed)

deriving stock instance Ord (AxiomDef 'Scoped)

type InductiveConstructorName s = SymbolType s

type InductiveName s = SymbolType s

data ConstructorDef (s :: Stage) = ConstructorDef
  { _constructorPipe :: Irrelevant (Maybe KeywordRef),
    _constructorName :: InductiveConstructorName s,
    _constructorInductiveName :: InductiveName s,
    _constructorDoc :: Maybe (Judoc s),
    _constructorPragmas :: Maybe ParsedPragmas,
    _constructorRhs :: ConstructorRhs s
  }
  deriving stock (Generic)

instance Serialize (ConstructorDef 'Scoped)

instance NFData (ConstructorDef 'Scoped)

deriving stock instance Show (ConstructorDef 'Parsed)

deriving stock instance Show (ConstructorDef 'Scoped)

deriving stock instance Eq (ConstructorDef 'Parsed)

deriving stock instance Eq (ConstructorDef 'Scoped)

deriving stock instance Ord (ConstructorDef 'Parsed)

deriving stock instance Ord (ConstructorDef 'Scoped)

data RecordUpdateField (s :: Stage)
  = RecordUpdateFieldAssign (RecordUpdateFieldItemAssign s)
  | RecordUpdateFieldPun (RecordUpdatePun s)
  deriving stock (Generic)

instance Serialize (RecordUpdateField 'Scoped)

instance NFData (RecordUpdateField 'Scoped)

instance Serialize (RecordUpdateField 'Parsed)

instance NFData (RecordUpdateField 'Parsed)

deriving stock instance Show (RecordUpdateField 'Parsed)

deriving stock instance Show (RecordUpdateField 'Scoped)

deriving stock instance Eq (RecordUpdateField 'Parsed)

deriving stock instance Eq (RecordUpdateField 'Scoped)

deriving stock instance Ord (RecordUpdateField 'Parsed)

deriving stock instance Ord (RecordUpdateField 'Scoped)

data RecordUpdateFieldItemAssign (s :: Stage) = RecordUpdateFieldItemAssign
  { _fieldUpdateName :: Symbol,
    _fieldUpdateArgIx :: FieldArgIxType s,
    _fieldUpdateAssignKw :: Irrelevant (KeywordRef),
    _fieldUpdateValue :: ExpressionType s
  }
  deriving stock (Generic)

instance Serialize (RecordUpdateFieldItemAssign 'Scoped)

instance NFData (RecordUpdateFieldItemAssign 'Scoped)

instance Serialize (RecordUpdateFieldItemAssign 'Parsed)

instance NFData (RecordUpdateFieldItemAssign 'Parsed)

deriving stock instance Show (RecordUpdateFieldItemAssign 'Parsed)

deriving stock instance Show (RecordUpdateFieldItemAssign 'Scoped)

deriving stock instance Eq (RecordUpdateFieldItemAssign 'Parsed)

deriving stock instance Eq (RecordUpdateFieldItemAssign 'Scoped)

deriving stock instance Ord (RecordUpdateFieldItemAssign 'Parsed)

deriving stock instance Ord (RecordUpdateFieldItemAssign 'Scoped)

data RecordField (s :: Stage) = RecordField
  { _fieldName :: SymbolType s,
    _fieldIsImplicit :: IsImplicitField,
    _fieldTypeSig :: TypeSig s,
    _fieldType :: ExpressionType s,
    _fieldBuiltin :: Maybe (WithLoc BuiltinFunction),
    _fieldDoc :: Maybe (Judoc s),
    _fieldPragmas :: Maybe ParsedPragmas
  }
  deriving stock (Generic)

instance Serialize (RecordField 'Scoped)

instance NFData (RecordField 'Scoped)

deriving stock instance Show (RecordField 'Parsed)

deriving stock instance Show (RecordField 'Scoped)

deriving stock instance Eq (RecordField 'Parsed)

deriving stock instance Eq (RecordField 'Scoped)

deriving stock instance Ord (RecordField 'Parsed)

deriving stock instance Ord (RecordField 'Scoped)

newtype RhsAdt (s :: Stage) = RhsAdt
  { _rhsAdtArguments :: [ExpressionType s]
  }
  deriving stock (Generic)

instance Serialize (RhsAdt 'Scoped)

instance NFData (RhsAdt 'Scoped)

deriving stock instance Show (RhsAdt 'Parsed)

deriving stock instance Show (RhsAdt 'Scoped)

deriving stock instance Eq (RhsAdt 'Parsed)

deriving stock instance Eq (RhsAdt 'Scoped)

deriving stock instance Ord (RhsAdt 'Parsed)

deriving stock instance Ord (RhsAdt 'Scoped)

data RhsRecord (s :: Stage) = RhsRecord
  { _rhsRecordDelim :: Irrelevant (Maybe KeywordRef, KeywordRef, KeywordRef),
    _rhsRecordStatements :: [RecordStatement s]
  }
  deriving stock (Generic)

instance Serialize (RhsRecord 'Scoped)

instance NFData (RhsRecord 'Scoped)

deriving stock instance Show (RhsRecord 'Parsed)

deriving stock instance Show (RhsRecord 'Scoped)

deriving stock instance Eq (RhsRecord 'Parsed)

deriving stock instance Eq (RhsRecord 'Scoped)

deriving stock instance Ord (RhsRecord 'Parsed)

deriving stock instance Ord (RhsRecord 'Scoped)

newtype RhsGadt (s :: Stage) = RhsGadt
  { _rhsGadtTypeSig :: TypeSig s
  }
  deriving stock (Generic)

instance Serialize (RhsGadt 'Scoped)

instance NFData (RhsGadt 'Scoped)

deriving stock instance Show (RhsGadt 'Parsed)

deriving stock instance Show (RhsGadt 'Scoped)

deriving stock instance Eq (RhsGadt 'Parsed)

deriving stock instance Eq (RhsGadt 'Scoped)

deriving stock instance Ord (RhsGadt 'Parsed)

deriving stock instance Ord (RhsGadt 'Scoped)

data ConstructorRhs (s :: Stage)
  = ConstructorRhsGadt (RhsGadt s)
  | ConstructorRhsRecord (RhsRecord s)
  | ConstructorRhsAdt (RhsAdt s)
  deriving stock (Generic)

instance Serialize (ConstructorRhs 'Scoped)

instance NFData (ConstructorRhs 'Scoped)

deriving stock instance Show (ConstructorRhs 'Parsed)

deriving stock instance Show (ConstructorRhs 'Scoped)

deriving stock instance Eq (ConstructorRhs 'Parsed)

deriving stock instance Eq (ConstructorRhs 'Scoped)

deriving stock instance Ord (ConstructorRhs 'Parsed)

deriving stock instance Ord (ConstructorRhs 'Scoped)

data InductiveParametersRhs (s :: Stage) = InductiveParametersRhs
  { _inductiveParametersColon :: Irrelevant KeywordRef,
    _inductiveParametersType :: ExpressionType s
  }
  deriving stock (Generic)

instance Serialize (InductiveParametersRhs 'Scoped)

instance NFData (InductiveParametersRhs 'Scoped)

deriving stock instance Show (InductiveParametersRhs 'Parsed)

deriving stock instance Show (InductiveParametersRhs 'Scoped)

deriving stock instance Eq (InductiveParametersRhs 'Parsed)

deriving stock instance Eq (InductiveParametersRhs 'Scoped)

deriving stock instance Ord (InductiveParametersRhs 'Parsed)

deriving stock instance Ord (InductiveParametersRhs 'Scoped)

data InductiveParameters (s :: Stage) = InductiveParameters
  { _inductiveParametersNames :: NonEmpty (SymbolType s),
    _inductiveParametersRhs :: Maybe (InductiveParametersRhs s)
  }
  deriving stock (Generic)

instance Serialize (InductiveParameters 'Scoped)

instance NFData (InductiveParameters 'Scoped)

deriving stock instance Show (InductiveParameters 'Parsed)

deriving stock instance Show (InductiveParameters 'Scoped)

deriving stock instance Eq (InductiveParameters 'Parsed)

deriving stock instance Eq (InductiveParameters 'Scoped)

deriving stock instance Ord (InductiveParameters 'Parsed)

deriving stock instance Ord (InductiveParameters 'Scoped)

data InductiveDef (s :: Stage) = InductiveDef
  { _inductiveKw :: Irrelevant KeywordRef,
    _inductiveAssignKw :: Irrelevant KeywordRef,
    _inductiveBuiltin :: Maybe (WithLoc BuiltinInductive),
    _inductiveDoc :: Maybe (Judoc s),
    _inductivePragmas :: Maybe ParsedPragmas,
    _inductiveName :: InductiveName s,
    _inductiveParameters :: [InductiveParameters s],
    _inductiveType :: Maybe (ExpressionType s),
    _inductiveTypeApplied :: ExpressionType s,
    _inductiveConstructors :: NonEmpty (ConstructorDef s),
    _inductivePositive :: Maybe KeywordRef,
    _inductiveTrait :: Maybe KeywordRef,
    _inductiveWithModule :: Maybe (WithModule s)
  }
  deriving stock (Generic)

instance Serialize (InductiveDef 'Scoped)

instance NFData (InductiveDef 'Scoped)

deriving stock instance Show (InductiveDef 'Parsed)

deriving stock instance Show (InductiveDef 'Scoped)

deriving stock instance Eq (InductiveDef 'Parsed)

deriving stock instance Eq (InductiveDef 'Scoped)

deriving stock instance Ord (InductiveDef 'Parsed)

deriving stock instance Ord (InductiveDef 'Scoped)

data PatternApp = PatternApp
  { _patAppLeft :: PatternArg,
    _patAppRight :: PatternArg
  }
  deriving stock (Show, Eq, Ord, Generic)

instance Serialize PatternApp

instance NFData PatternApp

data PatternInfixApp = PatternInfixApp
  { _patInfixLeft :: PatternArg,
    _patInfixConstructor :: ScopedIden,
    _patInfixRight :: PatternArg
  }
  deriving stock (Show, Eq, Ord, Generic)

instance Serialize PatternInfixApp

instance NFData PatternInfixApp

data PatternPostfixApp = PatternPostfixApp
  { _patPostfixParameter :: PatternArg,
    _patPostfixConstructor :: ScopedIden
  }
  deriving stock (Show, Eq, Ord, Generic)

instance Serialize PatternPostfixApp

instance NFData PatternPostfixApp

data PatternArg = PatternArg
  { _patternArgIsImplicit :: IsImplicit,
    _patternArgName :: Maybe S.Symbol,
    _patternArgPattern :: Pattern
  }
  deriving stock (Show, Eq, Ord, Generic)

instance Serialize PatternArg

instance NFData PatternArg

data Pattern
  = PatternVariable (SymbolType 'Scoped)
  | PatternConstructor ScopedIden
  | PatternWildcardConstructor (WildcardConstructor 'Scoped)
  | PatternApplication PatternApp
  | PatternList (ListPattern 'Scoped)
  | PatternInfixApplication PatternInfixApp
  | PatternPostfixApplication PatternPostfixApp
  | PatternWildcard Wildcard
  | PatternEmpty Interval
  | PatternRecord (RecordPattern 'Scoped)
  deriving stock (Show, Eq, Ord, Generic)

instance Serialize Pattern

instance NFData Pattern

data PatternScopedIden
  = PatternScopedVar S.Symbol
  | PatternScopedConstructor ScopedIden
  deriving stock (Show, Ord, Eq)

data PatternBinding = PatternBinding
  { _patternBindingName :: Symbol,
    _patternBindingAtKw :: Irrelevant KeywordRef,
    _patternBindingPattern :: PatternAtom 'Parsed
  }
  deriving stock (Ord, Eq, Show, Generic)

instance Serialize PatternBinding

instance NFData PatternBinding

data ListPattern (s :: Stage) = ListPattern
  { _listpBracketL :: Irrelevant KeywordRef,
    _listpBracketR :: Irrelevant KeywordRef,
    _listpItems :: [PatternParensType s]
  }
  deriving stock (Generic)

instance Serialize (ListPattern 'Scoped)

instance NFData (ListPattern 'Scoped)

instance Serialize (ListPattern 'Parsed)

instance NFData (ListPattern 'Parsed)

deriving stock instance Show (ListPattern 'Parsed)

deriving stock instance Show (ListPattern 'Scoped)

deriving stock instance Eq (ListPattern 'Parsed)

deriving stock instance Eq (ListPattern 'Scoped)

deriving stock instance Ord (ListPattern 'Parsed)

deriving stock instance Ord (ListPattern 'Scoped)

data RecordPatternAssign (s :: Stage) = RecordPatternAssign
  { _recordPatternAssignKw :: Irrelevant KeywordRef,
    _recordPatternAssignField :: Symbol,
    _recordPatternAssignFieldIx :: FieldArgIxType s,
    _recordPatternAssignPattern :: PatternParensType s
  }
  deriving stock (Generic)

instance Serialize (RecordPatternAssign 'Scoped)

instance NFData (RecordPatternAssign 'Scoped)

instance Serialize (RecordPatternAssign 'Parsed)

instance NFData (RecordPatternAssign 'Parsed)

deriving stock instance Show (RecordPatternAssign 'Parsed)

deriving stock instance Show (RecordPatternAssign 'Scoped)

deriving stock instance Eq (RecordPatternAssign 'Parsed)

deriving stock instance Eq (RecordPatternAssign 'Scoped)

deriving stock instance Ord (RecordPatternAssign 'Parsed)

deriving stock instance Ord (RecordPatternAssign 'Scoped)

data PatternFieldPun (s :: Stage) = PatternFieldPun
  { _fieldPunIx :: FieldArgIxType s,
    _fieldPunField :: SymbolType s
  }
  deriving stock (Generic)

instance Serialize (PatternFieldPun 'Scoped)

instance NFData (PatternFieldPun 'Scoped)

instance Serialize (PatternFieldPun 'Parsed)

instance NFData (PatternFieldPun 'Parsed)

deriving stock instance Show (PatternFieldPun 'Parsed)

deriving stock instance Show (PatternFieldPun 'Scoped)

deriving stock instance Eq (PatternFieldPun 'Parsed)

deriving stock instance Eq (PatternFieldPun 'Scoped)

deriving stock instance Ord (PatternFieldPun 'Parsed)

deriving stock instance Ord (PatternFieldPun 'Scoped)

data RecordPatternItem (s :: Stage)
  = RecordPatternItemFieldPun (PatternFieldPun s)
  | RecordPatternItemAssign (RecordPatternAssign s)
  deriving stock (Generic)

instance Serialize (RecordPatternItem 'Scoped)

instance NFData (RecordPatternItem 'Scoped)

instance Serialize (RecordPatternItem 'Parsed)

instance NFData (RecordPatternItem 'Parsed)

deriving stock instance Show (RecordPatternItem 'Parsed)

deriving stock instance Show (RecordPatternItem 'Scoped)

deriving stock instance Eq (RecordPatternItem 'Parsed)

deriving stock instance Eq (RecordPatternItem 'Scoped)

deriving stock instance Ord (RecordPatternItem 'Parsed)

deriving stock instance Ord (RecordPatternItem 'Scoped)

data RecordPattern (s :: Stage) = RecordPattern
  { _recordPatternConstructor :: IdentifierType s,
    _recordPatternItems :: [RecordPatternItem s]
  }
  deriving stock (Generic)

instance Serialize (RecordPattern 'Scoped)

instance NFData (RecordPattern 'Scoped)

instance Serialize (RecordPattern 'Parsed)

instance NFData (RecordPattern 'Parsed)

deriving stock instance Show (RecordPattern 'Parsed)

deriving stock instance Show (RecordPattern 'Scoped)

deriving stock instance Eq (RecordPattern 'Parsed)

deriving stock instance Eq (RecordPattern 'Scoped)

deriving stock instance Ord (RecordPattern 'Parsed)

deriving stock instance Ord (RecordPattern 'Scoped)

data WildcardConstructor (s :: Stage) = WildcardConstructor
  { _wildcardConstructor :: IdentifierType s,
    _wildcardConstructorAtKw :: Irrelevant KeywordRef,
    _wildcardConstructorDelims :: Irrelevant (KeywordRef, KeywordRef)
  }
  deriving stock (Generic)

instance Serialize (WildcardConstructor 'Scoped)

instance NFData (WildcardConstructor 'Scoped)

instance Serialize (WildcardConstructor 'Parsed)

instance NFData (WildcardConstructor 'Parsed)

deriving stock instance Show (WildcardConstructor 'Parsed)

deriving stock instance Show (WildcardConstructor 'Scoped)

deriving stock instance Eq (WildcardConstructor 'Parsed)

deriving stock instance Eq (WildcardConstructor 'Scoped)

deriving stock instance Ord (WildcardConstructor 'Parsed)

deriving stock instance Ord (WildcardConstructor 'Scoped)

data PatternAtom (s :: Stage)
  = PatternAtomIden (PatternAtomIdenType s)
  | PatternAtomWildcard Wildcard
  | PatternAtomEmpty Interval
  | PatternAtomList (ListPattern s)
  | PatternAtomWildcardConstructor (WildcardConstructor s)
  | PatternAtomRecord (RecordPattern s)
  | PatternAtomParens (PatternParensType s)
  | PatternAtomBraces (PatternParensType s)
  | PatternAtomDoubleBraces (PatternParensType s)
  | PatternAtomAt (PatternAtType s)
  deriving stock (Generic)

instance Serialize (PatternAtom 'Parsed)

instance NFData (PatternAtom 'Parsed)

deriving stock instance Show (PatternAtom 'Parsed)

deriving stock instance Show (PatternAtom 'Scoped)

deriving stock instance Eq (PatternAtom 'Parsed)

deriving stock instance Eq (PatternAtom 'Scoped)

deriving stock instance Ord (PatternAtom 'Parsed)

deriving stock instance Ord (PatternAtom 'Scoped)

data PatternAtoms (s :: Stage) = PatternAtoms
  { _patternAtoms :: NonEmpty (PatternAtom s),
    _patternAtomsLoc :: Irrelevant Interval
  }
  deriving stock (Generic)

instance Serialize (PatternAtoms 'Parsed)

instance NFData (PatternAtoms 'Parsed)

deriving stock instance Show (PatternAtoms 'Parsed)

deriving stock instance Show (PatternAtoms 'Scoped)

deriving stock instance Eq (PatternAtoms 'Parsed)

deriving stock instance Eq (PatternAtoms 'Scoped)

deriving stock instance Ord (PatternAtoms 'Parsed)

deriving stock instance Ord (PatternAtoms 'Scoped)

type FunctionName s = SymbolType s

type LocalModuleName s = SymbolType s

data MarkdownInfo = MarkdownInfo
  { _markdownInfo :: Mk,
    _markdownInfoBlockLengths :: [Int]
  }
  deriving stock (Show, Eq, Ord, Generic)

instance Serialize MarkdownInfo

instance NFData MarkdownInfo

data Module (s :: Stage) (t :: ModuleIsTop) = Module
  { _moduleKw :: KeywordRef,
    _modulePath :: ModulePathType s t,
    _moduleDoc :: Maybe (Judoc s),
    _modulePragmas :: Maybe ParsedPragmas,
    _moduleBody :: [Statement s],
    _moduleKwEnd :: ModuleEndType t,
    _moduleOrigin :: ModuleInductiveType t,
    _moduleId :: ModuleIdType s t,
    _moduleMarkdownInfo :: Maybe MarkdownInfo
  }
  deriving stock (Generic)

instance NFData (Module 'Scoped 'ModuleLocal)

instance Serialize (Module 'Scoped 'ModuleLocal)

deriving stock instance Show (Module 'Parsed 'ModuleTop)

deriving stock instance Show (Module 'Scoped 'ModuleTop)

deriving stock instance Show (Module 'Parsed 'ModuleLocal)

deriving stock instance Show (Module 'Scoped 'ModuleLocal)

deriving stock instance Eq (Module 'Parsed 'ModuleTop)

deriving stock instance Eq (Module 'Scoped 'ModuleTop)

deriving stock instance Eq (Module 'Parsed 'ModuleLocal)

deriving stock instance Eq (Module 'Scoped 'ModuleLocal)

deriving stock instance Ord (Module 'Parsed 'ModuleTop)

deriving stock instance Ord (Module 'Scoped 'ModuleTop)

deriving stock instance Ord (Module 'Parsed 'ModuleLocal)

deriving stock instance Ord (Module 'Scoped 'ModuleLocal)

data WithModule (s :: Stage) = WithModule
  { _withModuleWithKw :: Irrelevant KeywordRef,
    _withModuleEndKw :: Irrelevant KeywordRef,
    _withModuleBody :: [Statement s]
  }
  deriving stock (Generic)

instance Serialize (WithModule 'Scoped)

instance NFData (WithModule 'Scoped)

deriving stock instance Show (WithModule 'Parsed)

deriving stock instance Show (WithModule 'Scoped)

deriving stock instance Eq (WithModule 'Parsed)

deriving stock instance Eq (WithModule 'Scoped)

deriving stock instance Ord (WithModule 'Parsed)

deriving stock instance Ord (WithModule 'Scoped)

data HidingItem (s :: Stage) = HidingItem
  { _hidingSymbol :: SymbolType s,
    _hidingModuleKw :: Maybe KeywordRef
  }
  deriving stock (Generic)

instance Serialize (HidingItem 'Scoped)

instance NFData (HidingItem 'Scoped)

instance Serialize (HidingItem 'Parsed)

instance NFData (HidingItem 'Parsed)

deriving stock instance Show (HidingItem 'Parsed)

deriving stock instance Show (HidingItem 'Scoped)

deriving stock instance Eq (HidingItem 'Parsed)

deriving stock instance Eq (HidingItem 'Scoped)

deriving stock instance Ord (HidingItem 'Parsed)

deriving stock instance Ord (HidingItem 'Scoped)

data UsingItem (s :: Stage) = UsingItem
  { _usingSymbol :: SymbolType s,
    _usingModuleKw :: Maybe KeywordRef,
    _usingAsKw :: Irrelevant (Maybe KeywordRef),
    _usingAs :: Maybe (SymbolType s)
  }
  deriving stock (Generic)

instance Serialize (UsingItem 'Scoped)

instance NFData (UsingItem 'Scoped)

instance Serialize (UsingItem 'Parsed)

instance NFData (UsingItem 'Parsed)

deriving stock instance Show (UsingItem 'Parsed)

deriving stock instance Show (UsingItem 'Scoped)

deriving stock instance Eq (UsingItem 'Parsed)

deriving stock instance Eq (UsingItem 'Scoped)

deriving stock instance Ord (UsingItem 'Parsed)

deriving stock instance Ord (UsingItem 'Scoped)

data UsingList (s :: Stage) = UsingList
  { _usingKw :: Irrelevant KeywordRef,
    _usingBraces :: Irrelevant (KeywordRef, KeywordRef),
    _usingList :: NonEmpty (UsingItem s)
  }
  deriving stock (Generic)

instance Serialize (UsingList 'Scoped)

instance NFData (UsingList 'Scoped)

instance Serialize (UsingList 'Parsed)

instance NFData (UsingList 'Parsed)

deriving stock instance Show (UsingList 'Parsed)

deriving stock instance Show (UsingList 'Scoped)

deriving stock instance Eq (UsingList 'Parsed)

deriving stock instance Eq (UsingList 'Scoped)

deriving stock instance Ord (UsingList 'Parsed)

deriving stock instance Ord (UsingList 'Scoped)

data HidingList (s :: Stage) = HidingList
  { _hidingKw :: Irrelevant KeywordRef,
    _hidingBraces :: Irrelevant (KeywordRef, KeywordRef),
    _hidingList :: NonEmpty (HidingItem s)
  }
  deriving stock (Generic)

instance Serialize (HidingList 'Scoped)

instance NFData (HidingList 'Scoped)

instance Serialize (HidingList 'Parsed)

instance NFData (HidingList 'Parsed)

deriving stock instance Show (HidingList 'Parsed)

deriving stock instance Show (HidingList 'Scoped)

deriving stock instance Eq (HidingList 'Parsed)

deriving stock instance Eq (HidingList 'Scoped)

deriving stock instance Ord (HidingList 'Parsed)

deriving stock instance Ord (HidingList 'Scoped)

data UsingHiding (s :: Stage)
  = Using (UsingList s)
  | Hiding (HidingList s)
  deriving stock (Generic)

instance Serialize (UsingHiding 'Scoped)

instance NFData (UsingHiding 'Scoped)

instance Serialize (UsingHiding 'Parsed)

instance NFData (UsingHiding 'Parsed)

deriving stock instance Show (UsingHiding 'Parsed)

deriving stock instance Show (UsingHiding 'Scoped)

deriving stock instance Eq (UsingHiding 'Parsed)

deriving stock instance Eq (UsingHiding 'Scoped)

deriving stock instance Ord (UsingHiding 'Parsed)

deriving stock instance Ord (UsingHiding 'Scoped)

getNameRefId :: forall c. (SingI c) => RefNameType c -> S.NameId
getNameRefId = case sing :: S.SIsConcrete c of
  S.SConcrete -> (^. S.nameId)
  S.SNotConcrete -> (^. S.nameId)

data OpenModule (s :: Stage) (short :: IsOpenShort) = OpenModule
  { _openModuleKw :: KeywordRef,
    _openModuleName :: OpenModuleNameType s short,
    _openModuleUsingHiding :: Maybe (UsingHiding s),
    _openModulePublic :: PublicAnn
  }
  deriving stock (Generic)

instance Serialize (OpenModule 'Scoped 'OpenFull)

instance Serialize (OpenModule 'Scoped 'OpenShort)

instance NFData (OpenModule 'Scoped 'OpenFull)

instance NFData (OpenModule 'Scoped 'OpenShort)

instance Serialize (OpenModule 'Parsed 'OpenFull)

instance Serialize (OpenModule 'Parsed 'OpenShort)

instance NFData (OpenModule 'Parsed 'OpenShort)

instance NFData (OpenModule 'Parsed 'OpenFull)

deriving stock instance Show (OpenModule 'Parsed 'OpenShort)

deriving stock instance Show (OpenModule 'Parsed 'OpenFull)

deriving stock instance Show (OpenModule 'Scoped 'OpenShort)

deriving stock instance Show (OpenModule 'Scoped 'OpenFull)

deriving stock instance Eq (OpenModule 'Parsed 'OpenShort)

deriving stock instance Eq (OpenModule 'Parsed 'OpenFull)

deriving stock instance Eq (OpenModule 'Scoped 'OpenShort)

deriving stock instance Eq (OpenModule 'Scoped 'OpenFull)

deriving stock instance Ord (OpenModule 'Parsed 'OpenShort)

deriving stock instance Ord (OpenModule 'Parsed 'OpenFull)

deriving stock instance Ord (OpenModule 'Scoped 'OpenShort)

deriving stock instance Ord (OpenModule 'Scoped 'OpenFull)

data ScopedIden = ScopedIden
  { _scopedIdenFinal :: S.Name,
    _scopedIdenAlias :: Maybe S.Name
  }
  deriving stock (Show, Eq, Ord, Generic)

instance Serialize ScopedIden

instance NFData ScopedIden

data Expression
  = ExpressionIdentifier ScopedIden
  | ExpressionParensIdentifier ScopedIden
  | ExpressionApplication Application
  | ExpressionInfixApplication InfixApplication
  | ExpressionPostfixApplication PostfixApplication
  | ExpressionList (List 'Scoped)
  | ExpressionCase (Case 'Scoped)
  | ExpressionIf (If 'Scoped)
  | ExpressionLambda (Lambda 'Scoped)
  | ExpressionLet (Let 'Scoped)
  | ExpressionDo (Do 'Scoped)
  | ExpressionUniverse Universe
  | ExpressionLiteral LiteralLoc
  | ExpressionFunction (Function 'Scoped)
  | ExpressionHole (HoleType 'Scoped)
  | ExpressionInstanceHole (HoleType 'Scoped)
  | ExpressionRecordUpdate RecordUpdateApp
  | ExpressionParensRecordUpdate ParensRecordUpdate
  | ExpressionBraces (WithLoc Expression)
  | ExpressionDoubleBraces (DoubleBracesExpression 'Scoped)
  | ExpressionIterator (Iterator 'Scoped)
  | ExpressionNamedApplication (NamedApplication 'Scoped)
  deriving stock (Show, Eq, Ord, Generic)

instance Serialize Expression

instance NFData Expression

data DoubleBracesExpression (s :: Stage) = DoubleBracesExpression
  { _doubleBracesExpression :: ExpressionType s,
    _doubleBracesDelims :: Irrelevant (KeywordRef, KeywordRef)
  }
  deriving stock (Generic)

instance Serialize (DoubleBracesExpression 'Scoped)

instance NFData (DoubleBracesExpression 'Scoped)

instance Serialize (DoubleBracesExpression 'Parsed)

instance NFData (DoubleBracesExpression 'Parsed)

deriving stock instance Show (DoubleBracesExpression 'Parsed)

deriving stock instance Show (DoubleBracesExpression 'Scoped)

deriving stock instance Eq (DoubleBracesExpression 'Parsed)

deriving stock instance Eq (DoubleBracesExpression 'Scoped)

deriving stock instance Ord (DoubleBracesExpression 'Parsed)

deriving stock instance Ord (DoubleBracesExpression 'Scoped)

instance HasAtomicity (Lambda s) where
  atomicity = const Atom

data FunctionParameter (s :: Stage)
  = FunctionParameterName (SymbolType s)
  | FunctionParameterWildcard KeywordRef
  deriving stock (Generic)

instance Serialize (FunctionParameter 'Scoped)

instance NFData (FunctionParameter 'Scoped)

instance Serialize (FunctionParameter 'Parsed)

instance NFData (FunctionParameter 'Parsed)

deriving stock instance Show (FunctionParameter 'Parsed)

deriving stock instance Show (FunctionParameter 'Scoped)

deriving stock instance Eq (FunctionParameter 'Parsed)

deriving stock instance Eq (FunctionParameter 'Scoped)

deriving stock instance Ord (FunctionParameter 'Parsed)

deriving stock instance Ord (FunctionParameter 'Scoped)

data FunctionParameters (s :: Stage) = FunctionParameters
  { _paramNames :: [FunctionParameter s],
    _paramImplicit :: IsImplicit,
    _paramDelims :: Delims,
    _paramColon :: Irrelevant (Maybe KeywordRef),
    _paramType :: ExpressionType s
  }
  deriving stock (Generic)

instance Serialize (FunctionParameters 'Scoped)

instance NFData (FunctionParameters 'Scoped)

instance Serialize (FunctionParameters 'Parsed)

instance NFData (FunctionParameters 'Parsed)

deriving stock instance Show (FunctionParameters 'Parsed)

deriving stock instance Show (FunctionParameters 'Scoped)

deriving stock instance Eq (FunctionParameters 'Parsed)

deriving stock instance Eq (FunctionParameters 'Scoped)

deriving stock instance Ord (FunctionParameters 'Parsed)

deriving stock instance Ord (FunctionParameters 'Scoped)

-- | Function *type* representation
data Function (s :: Stage) = Function
  { _funParameters :: FunctionParameters s,
    _funKw :: KeywordRef,
    _funReturn :: ExpressionType s
  }
  deriving stock (Generic)

instance Serialize (Function 'Scoped)

instance NFData (Function 'Scoped)

instance Serialize (Function 'Parsed)

instance NFData (Function 'Parsed)

deriving stock instance Show (Function 'Parsed)

deriving stock instance Show (Function 'Scoped)

deriving stock instance Eq (Function 'Parsed)

deriving stock instance Eq (Function 'Scoped)

deriving stock instance Ord (Function 'Parsed)

deriving stock instance Ord (Function 'Scoped)

data Lambda (s :: Stage) = Lambda
  { _lambdaKw :: KeywordRef,
    _lambdaBraces :: Irrelevant (KeywordRef, KeywordRef),
    _lambdaClauses :: NonEmpty (LambdaClause s)
  }
  deriving stock (Generic)

instance Serialize (Lambda 'Scoped)

instance NFData (Lambda 'Scoped)

instance Serialize (Lambda 'Parsed)

instance NFData (Lambda 'Parsed)

deriving stock instance Show (Lambda 'Parsed)

deriving stock instance Show (Lambda 'Scoped)

deriving stock instance Eq (Lambda 'Parsed)

deriving stock instance Eq (Lambda 'Scoped)

deriving stock instance Ord (Lambda 'Parsed)

deriving stock instance Ord (Lambda 'Scoped)

data LambdaClause (s :: Stage) = LambdaClause
  { _lambdaPipe :: Irrelevant (Maybe KeywordRef),
    _lambdaParameters :: NonEmpty (PatternAtomType s),
    _lambdaAssignKw :: Irrelevant KeywordRef,
    _lambdaBody :: ExpressionType s
  }
  deriving stock (Generic)

instance Serialize (LambdaClause 'Scoped)

instance NFData (LambdaClause 'Scoped)

instance Serialize (LambdaClause 'Parsed)

instance NFData (LambdaClause 'Parsed)

deriving stock instance Show (LambdaClause 'Parsed)

deriving stock instance Show (LambdaClause 'Scoped)

deriving stock instance Eq (LambdaClause 'Parsed)

deriving stock instance Eq (LambdaClause 'Scoped)

deriving stock instance Ord (LambdaClause 'Parsed)

deriving stock instance Ord (LambdaClause 'Scoped)

data Application = Application
  { _applicationFunction :: Expression,
    _applicationParameter :: Expression
  }
  deriving stock (Show, Eq, Ord, Generic)

instance Serialize Application

instance NFData Application

data InfixApplication = InfixApplication
  { _infixAppLeft :: Expression,
    _infixAppOperator :: ScopedIden,
    _infixAppRight :: Expression
  }
  deriving stock (Show, Eq, Ord, Generic)

instance Serialize InfixApplication

instance NFData InfixApplication

data PostfixApplication = PostfixApplication
  { _postfixAppParameter :: Expression,
    _postfixAppOperator :: ScopedIden
  }
  deriving stock (Show, Eq, Ord, Generic)

instance Serialize PostfixApplication

instance NFData PostfixApplication

data LetStatement (s :: Stage)
  = LetFunctionDef (FunctionDef s)
  | LetAliasDef (AliasDef s)
  | LetOpen (OpenModule s 'OpenFull)
  deriving stock (Generic)

instance Serialize (LetStatement 'Scoped)

instance NFData (LetStatement 'Scoped)

instance Serialize (LetStatement 'Parsed)

instance NFData (LetStatement 'Parsed)

deriving stock instance Show (LetStatement 'Parsed)

deriving stock instance Show (LetStatement 'Scoped)

deriving stock instance Eq (LetStatement 'Parsed)

deriving stock instance Eq (LetStatement 'Scoped)

deriving stock instance Ord (LetStatement 'Parsed)

deriving stock instance Ord (LetStatement 'Scoped)

data DoLet (s :: Stage) = DoLet
  { _doLetKw :: Irrelevant KeywordRef,
    _doLetStatements :: NonEmpty (LetStatement s),
    _doLetInKw :: Irrelevant KeywordRef
  }
  deriving stock (Generic)

instance Serialize (DoLet 'Scoped)

instance NFData (DoLet 'Scoped)

instance Serialize (DoLet 'Parsed)

instance NFData (DoLet 'Parsed)

deriving stock instance Show (DoLet 'Parsed)

deriving stock instance Show (DoLet 'Scoped)

deriving stock instance Eq (DoLet 'Parsed)

deriving stock instance Eq (DoLet 'Scoped)

deriving stock instance Ord (DoLet 'Parsed)

deriving stock instance Ord (DoLet 'Scoped)

data Let (s :: Stage) = Let
  { _letKw :: KeywordRef,
    _letInKw :: Irrelevant KeywordRef,
    _letFunDefs :: NonEmpty (LetStatement s),
    _letExpression :: ExpressionType s
  }
  deriving stock (Generic)

instance Serialize (Let 'Scoped)

instance NFData (Let 'Scoped)

instance Serialize (Let 'Parsed)

instance NFData (Let 'Parsed)

deriving stock instance Show (Let 'Parsed)

deriving stock instance Show (Let 'Scoped)

deriving stock instance Eq (Let 'Parsed)

deriving stock instance Eq (Let 'Scoped)

deriving stock instance Ord (Let 'Parsed)

deriving stock instance Ord (Let 'Scoped)

data SideIfBranch (s :: Stage) (k :: IfBranchKind) = SideIfBranch
  { _sideIfBranchPipe :: Irrelevant (Maybe KeywordRef),
    _sideIfBranchKw :: Irrelevant KeywordRef,
    _sideIfBranchCondition :: SideIfBranchConditionType s k,
    _sideIfBranchAssignKw :: Irrelevant KeywordRef,
    _sideIfBranchBody :: ExpressionType s
  }
  deriving stock (Generic)

instance Serialize (SideIfBranch 'Scoped 'BranchIfBool)

instance Serialize (SideIfBranch 'Scoped 'BranchIfElse)

instance NFData (SideIfBranch 'Scoped 'BranchIfBool)

instance NFData (SideIfBranch 'Scoped 'BranchIfElse)

instance Serialize (SideIfBranch 'Parsed 'BranchIfBool)

instance Serialize (SideIfBranch 'Parsed 'BranchIfElse)

instance NFData (SideIfBranch 'Parsed 'BranchIfElse)

instance NFData (SideIfBranch 'Parsed 'BranchIfBool)

deriving stock instance Show (SideIfBranch 'Parsed 'BranchIfElse)

deriving stock instance Show (SideIfBranch 'Parsed 'BranchIfBool)

deriving stock instance Show (SideIfBranch 'Scoped 'BranchIfElse)

deriving stock instance Show (SideIfBranch 'Scoped 'BranchIfBool)

deriving stock instance Eq (SideIfBranch 'Parsed 'BranchIfElse)

deriving stock instance Eq (SideIfBranch 'Parsed 'BranchIfBool)

deriving stock instance Eq (SideIfBranch 'Scoped 'BranchIfElse)

deriving stock instance Eq (SideIfBranch 'Scoped 'BranchIfBool)

deriving stock instance Ord (SideIfBranch 'Parsed 'BranchIfElse)

deriving stock instance Ord (SideIfBranch 'Parsed 'BranchIfBool)

deriving stock instance Ord (SideIfBranch 'Scoped 'BranchIfElse)

deriving stock instance Ord (SideIfBranch 'Scoped 'BranchIfBool)

data SideIfs (s :: Stage) = SideIfs
  { _sideIfBranches :: NonEmpty (SideIfBranch s 'BranchIfBool),
    _sideIfElse :: Maybe (SideIfBranch s 'BranchIfElse)
  }
  deriving stock (Generic)

instance Serialize (SideIfs 'Scoped)

instance NFData (SideIfs 'Scoped)

instance Serialize (SideIfs 'Parsed)

instance NFData (SideIfs 'Parsed)

deriving stock instance Show (SideIfs 'Parsed)

deriving stock instance Show (SideIfs 'Scoped)

deriving stock instance Eq (SideIfs 'Parsed)

deriving stock instance Eq (SideIfs 'Scoped)

deriving stock instance Ord (SideIfs 'Parsed)

deriving stock instance Ord (SideIfs 'Scoped)

data RhsExpression (s :: Stage) = RhsExpression
  { _rhsExpressionAssignKw :: Irrelevant KeywordRef,
    _rhsExpression :: ExpressionType s
  }
  deriving stock (Generic)

instance Serialize (RhsExpression 'Scoped)

instance NFData (RhsExpression 'Scoped)

instance Serialize (RhsExpression 'Parsed)

instance NFData (RhsExpression 'Parsed)

deriving stock instance Show (RhsExpression 'Parsed)

deriving stock instance Show (RhsExpression 'Scoped)

deriving stock instance Eq (RhsExpression 'Parsed)

deriving stock instance Eq (RhsExpression 'Scoped)

deriving stock instance Ord (RhsExpression 'Parsed)

deriving stock instance Ord (RhsExpression 'Scoped)

data CaseBranchRhs (s :: Stage)
  = CaseBranchRhsExpression (RhsExpression s)
  | CaseBranchRhsIf (SideIfs s)
  deriving stock (Generic)

instance Serialize (CaseBranchRhs 'Scoped)

instance NFData (CaseBranchRhs 'Scoped)

instance Serialize (CaseBranchRhs 'Parsed)

instance NFData (CaseBranchRhs 'Parsed)

deriving stock instance Show (CaseBranchRhs 'Parsed)

deriving stock instance Show (CaseBranchRhs 'Scoped)

deriving stock instance Eq (CaseBranchRhs 'Parsed)

deriving stock instance Eq (CaseBranchRhs 'Scoped)

deriving stock instance Ord (CaseBranchRhs 'Parsed)

deriving stock instance Ord (CaseBranchRhs 'Scoped)

data CaseBranch (s :: Stage) = CaseBranch
  { _caseBranchPipe :: Irrelevant (Maybe KeywordRef),
    _caseBranchPattern :: PatternParensType s,
    _caseBranchRhs :: CaseBranchRhs s
  }
  deriving stock (Generic)

instance Serialize (CaseBranch 'Scoped)

instance NFData (CaseBranch 'Scoped)

instance Serialize (CaseBranch 'Parsed)

instance NFData (CaseBranch 'Parsed)

deriving stock instance Show (CaseBranch 'Parsed)

deriving stock instance Show (CaseBranch 'Scoped)

deriving stock instance Eq (CaseBranch 'Parsed)

deriving stock instance Eq (CaseBranch 'Scoped)

deriving stock instance Ord (CaseBranch 'Parsed)

deriving stock instance Ord (CaseBranch 'Scoped)

data Case (s :: Stage) = Case
  { _caseKw :: KeywordRef,
    _caseOfKw :: KeywordRef,
    _caseExpression :: ExpressionType s,
    _caseBranches :: NonEmpty (CaseBranch s)
  }
  deriving stock (Generic)

instance Serialize (Case 'Scoped)

instance NFData (Case 'Scoped)

instance Serialize (Case 'Parsed)

instance NFData (Case 'Parsed)

deriving stock instance Show (Case 'Parsed)

deriving stock instance Show (Case 'Scoped)

deriving stock instance Eq (Case 'Parsed)

deriving stock instance Eq (Case 'Scoped)

deriving stock instance Ord (Case 'Parsed)

deriving stock instance Ord (Case 'Scoped)

data NewCaseBranch (s :: Stage) = NewCaseBranch
  { _newCaseBranchPipe :: Irrelevant (Maybe KeywordRef),
    _newCaseBranchAssignKw :: Irrelevant KeywordRef,
    _newCaseBranchPattern :: PatternParensType s,
    _newCaseBranchExpression :: ExpressionType s
  }
  deriving stock (Generic)

instance Serialize (NewCaseBranch 'Scoped)

instance NFData (NewCaseBranch 'Scoped)

instance Serialize (NewCaseBranch 'Parsed)

instance NFData (NewCaseBranch 'Parsed)

deriving stock instance Show (NewCaseBranch 'Parsed)

deriving stock instance Show (NewCaseBranch 'Scoped)

deriving stock instance Eq (NewCaseBranch 'Parsed)

deriving stock instance Eq (NewCaseBranch 'Scoped)

deriving stock instance Ord (NewCaseBranch 'Parsed)

deriving stock instance Ord (NewCaseBranch 'Scoped)

data NewCase (s :: Stage) = NewCase
  { _newCaseKw :: KeywordRef,
    _newCaseOfKw :: KeywordRef,
    _newCaseExpression :: ExpressionType s,
    _newCaseBranches :: NonEmpty (NewCaseBranch s)
  }
  deriving stock (Generic)

instance Serialize (NewCase 'Scoped)

instance NFData (NewCase 'Scoped)

instance Serialize (NewCase 'Parsed)

instance NFData (NewCase 'Parsed)

deriving stock instance Show (NewCase 'Parsed)

deriving stock instance Show (NewCase 'Scoped)

deriving stock instance Eq (NewCase 'Parsed)

deriving stock instance Eq (NewCase 'Scoped)

deriving stock instance Ord (NewCase 'Parsed)

deriving stock instance Ord (NewCase 'Scoped)

data IfBranch (s :: Stage) (k :: IfBranchKind) = IfBranch
  { _ifBranchPipe :: Irrelevant KeywordRef,
    _ifBranchAssignKw :: Irrelevant KeywordRef,
    _ifBranchCondition :: IfBranchConditionType s k,
    _ifBranchExpression :: ExpressionType s
  }
  deriving stock (Generic)

instance Serialize (IfBranch 'Scoped 'BranchIfBool)

instance Serialize (IfBranch 'Scoped 'BranchIfElse)

instance NFData (IfBranch 'Scoped 'BranchIfBool)

instance NFData (IfBranch 'Scoped 'BranchIfElse)

instance Serialize (IfBranch 'Parsed 'BranchIfBool)

instance Serialize (IfBranch 'Parsed 'BranchIfElse)

instance NFData (IfBranch 'Parsed 'BranchIfElse)

instance NFData (IfBranch 'Parsed 'BranchIfBool)

deriving stock instance Show (IfBranch 'Parsed 'BranchIfElse)

deriving stock instance Show (IfBranch 'Parsed 'BranchIfBool)

deriving stock instance Show (IfBranch 'Scoped 'BranchIfElse)

deriving stock instance Show (IfBranch 'Scoped 'BranchIfBool)

deriving stock instance Eq (IfBranch 'Parsed 'BranchIfElse)

deriving stock instance Eq (IfBranch 'Parsed 'BranchIfBool)

deriving stock instance Eq (IfBranch 'Scoped 'BranchIfElse)

deriving stock instance Eq (IfBranch 'Scoped 'BranchIfBool)

deriving stock instance Ord (IfBranch 'Parsed 'BranchIfElse)

deriving stock instance Ord (IfBranch 'Parsed 'BranchIfBool)

deriving stock instance Ord (IfBranch 'Scoped 'BranchIfElse)

deriving stock instance Ord (IfBranch 'Scoped 'BranchIfBool)

data If (s :: Stage) = If
  { _ifKw :: KeywordRef,
    _ifBranches :: [IfBranch s 'BranchIfBool],
    _ifBranchElse :: IfBranch s 'BranchIfElse
  }
  deriving stock (Generic)

instance Serialize (If 'Scoped)

instance NFData (If 'Scoped)

instance Serialize (If 'Parsed)

instance NFData (If 'Parsed)

deriving stock instance Show (If 'Parsed)

deriving stock instance Show (If 'Scoped)

deriving stock instance Eq (If 'Parsed)

deriving stock instance Eq (If 'Scoped)

deriving stock instance Ord (If 'Parsed)

deriving stock instance Ord (If 'Scoped)

data Initializer (s :: Stage) = Initializer
  { _initializerPattern :: PatternParensType s,
    _initializerAssignKw :: Irrelevant KeywordRef,
    _initializerExpression :: ExpressionType s
  }
  deriving stock (Generic)

instance Serialize (Initializer 'Scoped)

instance NFData (Initializer 'Scoped)

instance Serialize (Initializer 'Parsed)

instance NFData (Initializer 'Parsed)

deriving stock instance Show (Initializer 'Parsed)

deriving stock instance Show (Initializer 'Scoped)

deriving stock instance Eq (Initializer 'Parsed)

deriving stock instance Eq (Initializer 'Scoped)

deriving stock instance Ord (Initializer 'Parsed)

deriving stock instance Ord (Initializer 'Scoped)

data Range (s :: Stage) = Range
  { _rangePattern :: PatternParensType s,
    _rangeInKw :: Irrelevant KeywordRef,
    _rangeExpression :: ExpressionType s
  }
  deriving stock (Generic)

instance Serialize (Range 'Scoped)

instance NFData (Range 'Scoped)

instance Serialize (Range 'Parsed)

instance NFData (Range 'Parsed)

deriving stock instance Show (Range 'Parsed)

deriving stock instance Show (Range 'Scoped)

deriving stock instance Eq (Range 'Parsed)

deriving stock instance Eq (Range 'Scoped)

deriving stock instance Ord (Range 'Parsed)

deriving stock instance Ord (Range 'Scoped)

data Iterator s = Iterator
  { _iteratorName :: IdentifierType s,
    _iteratorInitializers :: [Initializer s],
    _iteratorRanges :: [Range s],
    _iteratorBody :: ExpressionType s,
    -- | Was the body enclosed in braces?
    _iteratorBodyBraces :: Bool,
    -- | Due to limitations of the pretty printing algorithm, we store whether
    -- the iterator was surrounded by parentheses in the code.
    _iteratorParens :: Bool
  }
  deriving stock (Generic)

instance Serialize (Iterator 'Scoped)

instance NFData (Iterator 'Scoped)

instance Serialize (Iterator 'Parsed)

instance NFData (Iterator 'Parsed)

deriving stock instance Show (Iterator 'Parsed)

deriving stock instance Show (Iterator 'Scoped)

deriving stock instance Eq (Iterator 'Parsed)

deriving stock instance Eq (Iterator 'Scoped)

deriving stock instance Ord (Iterator 'Parsed)

deriving stock instance Ord (Iterator 'Scoped)

data List (s :: Stage) = List
  { _listBracketL :: Irrelevant KeywordRef,
    _listBracketR :: Irrelevant KeywordRef,
    _listItems :: [ExpressionType s]
  }
  deriving stock (Generic)

instance Serialize (List 'Scoped)

instance NFData (List 'Scoped)

instance Serialize (List 'Parsed)

instance NFData (List 'Parsed)

deriving stock instance Show (List 'Parsed)

deriving stock instance Show (List 'Scoped)

deriving stock instance Eq (List 'Parsed)

deriving stock instance Eq (List 'Scoped)

deriving stock instance Ord (List 'Parsed)

deriving stock instance Ord (List 'Scoped)

data NamedArgumentAssign (s :: Stage) = NamedArgumentAssign
  { _namedArgName :: SymbolType s,
    _namedArgAssignKw :: Irrelevant KeywordRef,
    _namedArgValue :: ExpressionType s
  }
  deriving stock (Generic)

instance Serialize (NamedArgumentAssign 'Scoped)

instance NFData (NamedArgumentAssign 'Scoped)

instance Serialize (NamedArgumentAssign 'Parsed)

instance NFData (NamedArgumentAssign 'Parsed)

deriving stock instance Show (NamedArgumentAssign 'Parsed)

deriving stock instance Show (NamedArgumentAssign 'Scoped)

deriving stock instance Eq (NamedArgumentAssign 'Parsed)

deriving stock instance Eq (NamedArgumentAssign 'Scoped)

deriving stock instance Ord (NamedArgumentAssign 'Parsed)

deriving stock instance Ord (NamedArgumentAssign 'Scoped)

data ArgumentBlock (s :: Stage) = ArgumentBlock
  { _argBlockDelims :: Irrelevant (Maybe (KeywordRef, KeywordRef)),
    _argBlockImplicit :: IsImplicit,
    _argBlockArgs :: NonEmpty (NamedArgumentAssign s)
  }
  deriving stock (Generic)

instance Serialize (ArgumentBlock 'Scoped)

instance NFData (ArgumentBlock 'Scoped)

instance Serialize (ArgumentBlock 'Parsed)

instance NFData (ArgumentBlock 'Parsed)

deriving stock instance Show (ArgumentBlock 'Parsed)

deriving stock instance Show (ArgumentBlock 'Scoped)

deriving stock instance Eq (ArgumentBlock 'Parsed)

deriving stock instance Eq (ArgumentBlock 'Scoped)

deriving stock instance Ord (ArgumentBlock 'Parsed)

deriving stock instance Ord (ArgumentBlock 'Scoped)

data RecordUpdateExtra = RecordUpdateExtra
  { _recordUpdateExtraConstructor :: S.Symbol,
    -- | Implicitly bound fields sorted by index
    _recordUpdateExtraVars :: IntMap (IsImplicit, S.Symbol)
  }
  deriving stock (Generic)

instance Serialize RecordUpdateExtra

instance NFData RecordUpdateExtra

newtype ParensRecordUpdate = ParensRecordUpdate
  { _parensRecordUpdate :: RecordUpdate 'Scoped
  }
  deriving stock (Show, Eq, Ord, Generic)

instance Serialize ParensRecordUpdate

instance NFData ParensRecordUpdate

data RecordUpdate (s :: Stage) = RecordUpdate
  { _recordUpdateAtKw :: Irrelevant KeywordRef,
    _recordUpdateDelims :: Irrelevant (KeywordRef, KeywordRef),
    _recordUpdateTypeName :: IdentifierType s,
    _recordUpdateExtra :: Irrelevant (RecordUpdateExtraType s),
    _recordUpdateFields :: [RecordUpdateField s]
  }
  deriving stock (Generic)

instance Serialize (RecordUpdate 'Scoped)

instance NFData (RecordUpdate 'Scoped)

instance Serialize (RecordUpdate 'Parsed)

instance NFData (RecordUpdate 'Parsed)

deriving stock instance Show (RecordUpdate 'Parsed)

deriving stock instance Show (RecordUpdate 'Scoped)

deriving stock instance Eq (RecordUpdate 'Parsed)

deriving stock instance Eq (RecordUpdate 'Scoped)

deriving stock instance Ord (RecordUpdate 'Parsed)

deriving stock instance Ord (RecordUpdate 'Scoped)

data RecordUpdateApp = RecordUpdateApp
  { _recordAppUpdate :: RecordUpdate 'Scoped,
    _recordAppExpression :: Expression
  }
  deriving stock (Show, Eq, Ord, Generic)

instance Serialize RecordUpdateApp

instance NFData RecordUpdateApp

newtype NamedArgumentFunctionDef (s :: Stage) = NamedArgumentFunctionDef
  { _namedArgumentFunctionDef :: FunctionDef s
  }
  deriving stock (Generic)

instance Serialize (NamedArgumentFunctionDef 'Scoped)

instance NFData (NamedArgumentFunctionDef 'Scoped)

instance Serialize (NamedArgumentFunctionDef 'Parsed)

instance NFData (NamedArgumentFunctionDef 'Parsed)

deriving stock instance Show (NamedArgumentFunctionDef 'Parsed)

deriving stock instance Show (NamedArgumentFunctionDef 'Scoped)

deriving stock instance Eq (NamedArgumentFunctionDef 'Parsed)

deriving stock instance Eq (NamedArgumentFunctionDef 'Scoped)

deriving stock instance Ord (NamedArgumentFunctionDef 'Parsed)

deriving stock instance Ord (NamedArgumentFunctionDef 'Scoped)

data RecordUpdatePun (s :: Stage) = RecordUpdatePun
  { _recordUpdatePunSymbol :: Symbol,
    _recordUpdatePunReferencedSymbol :: PunSymbolType s,
    _recordUpdatePunFieldIndex :: FieldArgIxType s
  }
  deriving stock (Generic)

instance Serialize (RecordUpdatePun 'Scoped)

instance NFData (RecordUpdatePun 'Scoped)

instance Serialize (RecordUpdatePun 'Parsed)

instance NFData (RecordUpdatePun 'Parsed)

deriving stock instance Show (RecordUpdatePun 'Parsed)

deriving stock instance Show (RecordUpdatePun 'Scoped)

deriving stock instance Eq (RecordUpdatePun 'Parsed)

deriving stock instance Eq (RecordUpdatePun 'Scoped)

deriving stock instance Ord (RecordUpdatePun 'Parsed)

deriving stock instance Ord (RecordUpdatePun 'Scoped)

data NamedArgumentPun (s :: Stage) = NamedArgumentPun
  { _namedArgumentPunSymbol :: Symbol,
    _namedArgumentReferencedSymbol :: PunSymbolType s
  }
  deriving stock (Generic)

instance Serialize (NamedArgumentPun 'Scoped)

instance NFData (NamedArgumentPun 'Scoped)

instance Serialize (NamedArgumentPun 'Parsed)

instance NFData (NamedArgumentPun 'Parsed)

deriving stock instance Show (NamedArgumentPun 'Parsed)

deriving stock instance Show (NamedArgumentPun 'Scoped)

deriving stock instance Eq (NamedArgumentPun 'Parsed)

deriving stock instance Eq (NamedArgumentPun 'Scoped)

deriving stock instance Ord (NamedArgumentPun 'Parsed)

deriving stock instance Ord (NamedArgumentPun 'Scoped)

data NamedArgument (s :: Stage)
  = NamedArgumentFunction (NamedArgumentFunctionDef s)
  | NamedArgumentItemPun (NamedArgumentPun s)
  deriving stock (Generic)

instance Serialize (NamedArgument 'Scoped)

instance NFData (NamedArgument 'Scoped)

instance Serialize (NamedArgument 'Parsed)

instance NFData (NamedArgument 'Parsed)

deriving stock instance Show (NamedArgument 'Parsed)

deriving stock instance Show (NamedArgument 'Scoped)

deriving stock instance Eq (NamedArgument 'Parsed)

deriving stock instance Eq (NamedArgument 'Scoped)

deriving stock instance Ord (NamedArgument 'Parsed)

deriving stock instance Ord (NamedArgument 'Scoped)

data NamedApplication (s :: Stage) = NamedApplication
  { _namedApplicationName :: IdentifierType s,
    _namedApplicationAtKw :: Irrelevant KeywordRef,
    _namedApplicationArguments :: [NamedArgument s]
  }
  deriving stock (Generic)

instance Serialize (NamedApplication 'Scoped)

instance NFData (NamedApplication 'Scoped)

instance Serialize (NamedApplication 'Parsed)

instance NFData (NamedApplication 'Parsed)

deriving stock instance Show (NamedApplication 'Parsed)

deriving stock instance Show (NamedApplication 'Scoped)

deriving stock instance Eq (NamedApplication 'Parsed)

deriving stock instance Eq (NamedApplication 'Scoped)

deriving stock instance Ord (NamedApplication 'Parsed)

deriving stock instance Ord (NamedApplication 'Scoped)

data RecordSyntaxDef (s :: Stage)
  = RecordSyntaxOperator (OperatorSyntaxDef s)
  | RecordSyntaxIterator (IteratorSyntaxDef s)
  deriving stock (Generic)

instance Serialize (RecordSyntaxDef 'Scoped)

instance NFData (RecordSyntaxDef 'Scoped)

deriving stock instance Show (RecordSyntaxDef 'Parsed)

deriving stock instance Show (RecordSyntaxDef 'Scoped)

deriving stock instance Eq (RecordSyntaxDef 'Parsed)

deriving stock instance Eq (RecordSyntaxDef 'Scoped)

deriving stock instance Ord (RecordSyntaxDef 'Parsed)

deriving stock instance Ord (RecordSyntaxDef 'Scoped)

data RecordStatement (s :: Stage)
  = RecordStatementField (RecordField s)
  | RecordStatementSyntax (RecordSyntaxDef s)
  deriving stock (Generic)

instance Serialize (RecordStatement 'Scoped)

instance NFData (RecordStatement 'Scoped)

deriving stock instance Show (RecordStatement 'Parsed)

deriving stock instance Show (RecordStatement 'Scoped)

deriving stock instance Eq (RecordStatement 'Parsed)

deriving stock instance Eq (RecordStatement 'Scoped)

deriving stock instance Ord (RecordStatement 'Parsed)

deriving stock instance Ord (RecordStatement 'Scoped)

data Do (s :: Stage) = Do
  { _doKeyword :: Irrelevant KeywordRef,
    _doDelims :: Irrelevant (KeywordRef, KeywordRef),
    _doStatements :: NonEmpty (DoStatement s)
  }
  deriving stock (Generic)

instance Serialize (Do 'Parsed)

instance Serialize (Do 'Scoped)

instance NFData (Do 'Scoped)

instance NFData (Do 'Parsed)

deriving stock instance Show (Do 'Parsed)

deriving stock instance Show (Do 'Scoped)

deriving stock instance Eq (Do 'Parsed)

deriving stock instance Eq (Do 'Scoped)

deriving stock instance Ord (Do 'Parsed)

deriving stock instance Ord (Do 'Scoped)

data DoBind (s :: Stage) = DoBind
  { _doBindPattern :: PatternParensType s,
    _doBindArrowKw :: Irrelevant KeywordRef,
    _doBindExpression :: ExpressionType s
  }
  deriving stock (Generic)

instance Serialize (DoBind 'Scoped)

instance Serialize (DoBind 'Parsed)

instance NFData (DoBind 'Scoped)

instance NFData (DoBind 'Parsed)

deriving stock instance Show (DoBind 'Parsed)

deriving stock instance Show (DoBind 'Scoped)

deriving stock instance Eq (DoBind 'Parsed)

deriving stock instance Eq (DoBind 'Scoped)

deriving stock instance Ord (DoBind 'Parsed)

deriving stock instance Ord (DoBind 'Scoped)

data DoStatement (s :: Stage)
  = DoStatementBind (DoBind s)
  | DoStatementLet (DoLet s)
  | DoStatementExpression (ExpressionType s)
  deriving stock (Generic)

instance Serialize (DoStatement 'Scoped)

instance Serialize (DoStatement 'Parsed)

instance NFData (DoStatement 'Scoped)

instance NFData (DoStatement 'Parsed)

deriving stock instance Show (DoStatement 'Parsed)

deriving stock instance Show (DoStatement 'Scoped)

deriving stock instance Eq (DoStatement 'Parsed)

deriving stock instance Eq (DoStatement 'Scoped)

deriving stock instance Ord (DoStatement 'Parsed)

deriving stock instance Ord (DoStatement 'Scoped)

-- | Expressions without application
data ExpressionAtom (s :: Stage)
  = AtomIdentifier (IdentifierType s)
  | AtomLambda (Lambda s)
  | AtomList (List s)
  | AtomCase (Case s)
  | AtomIf (If s)
  | AtomHole (HoleType s)
  | AtomInstanceHole (HoleType s)
  | AtomDoubleBraces (DoubleBracesExpression s)
  | AtomBraces (WithLoc (ExpressionType s))
  | AtomDo (Do s)
  | AtomLet (Let s)
  | AtomRecordUpdate (RecordUpdate s)
  | AtomUniverse Universe
  | AtomFunction (Function s)
  | AtomFunArrow KeywordRef
  | AtomLiteral LiteralLoc
  | AtomParens (ExpressionType s)
  | AtomIterator (Iterator s)
  | AtomNamedApplication (NamedApplication s)
  deriving stock (Generic)

instance Serialize (ExpressionAtom 'Parsed)

instance NFData (ExpressionAtom 'Parsed)

deriving stock instance Show (ExpressionAtom 'Parsed)

deriving stock instance Show (ExpressionAtom 'Scoped)

deriving stock instance Eq (ExpressionAtom 'Parsed)

deriving stock instance Eq (ExpressionAtom 'Scoped)

deriving stock instance Ord (ExpressionAtom 'Parsed)

deriving stock instance Ord (ExpressionAtom 'Scoped)

data ExpressionAtoms (s :: Stage) = ExpressionAtoms
  { _expressionAtoms :: NonEmpty (ExpressionAtom s),
    _expressionAtomsLoc :: Irrelevant Interval
  }
  deriving stock (Generic)

instance Serialize (ExpressionAtoms 'Parsed)

instance NFData (ExpressionAtoms 'Parsed)

deriving stock instance Show (ExpressionAtoms 'Parsed)

deriving stock instance Show (ExpressionAtoms 'Scoped)

deriving stock instance Eq (ExpressionAtoms 'Parsed)

deriving stock instance Eq (ExpressionAtoms 'Scoped)

deriving stock instance Ord (ExpressionAtoms 'Parsed)

deriving stock instance Ord (ExpressionAtoms 'Scoped)

newtype Judoc (s :: Stage) = Judoc
  { _judocGroups :: NonEmpty (JudocGroup s)
  }
  deriving newtype (Semigroup, Generic)

instance Serialize (Judoc 'Scoped)

instance NFData (Judoc 'Scoped)

instance Serialize (Judoc 'Parsed)

instance NFData (Judoc 'Parsed)

deriving stock instance Show (Judoc 'Parsed)

deriving stock instance Show (Judoc 'Scoped)

deriving stock instance Eq (Judoc 'Parsed)

deriving stock instance Eq (Judoc 'Scoped)

deriving stock instance Ord (Judoc 'Parsed)

deriving stock instance Ord (Judoc 'Scoped)

data Example (s :: Stage) = Example
  { _exampleId :: NameId,
    _exampleLoc :: Interval,
    _exampleExpression :: ExpressionType s
  }
  deriving stock (Generic)

instance Serialize (Example 'Scoped)

instance NFData (Example 'Scoped)

instance Serialize (Example 'Parsed)

instance NFData (Example 'Parsed)

deriving stock instance Show (Example 'Parsed)

deriving stock instance Show (Example 'Scoped)

deriving stock instance Eq (Example 'Parsed)

deriving stock instance Eq (Example 'Scoped)

deriving stock instance Ord (Example 'Parsed)

deriving stock instance Ord (Example 'Scoped)

data JudocBlockParagraph (s :: Stage) = JudocBlockParagraph
  { _judocBlockParagraphStart :: KeywordRef,
    _judocBlockParagraphBlocks :: [JudocBlock s],
    _judocBlockParagraphEnd :: KeywordRef
  }
  deriving stock (Generic)

instance Serialize (JudocBlockParagraph 'Scoped)

instance NFData (JudocBlockParagraph 'Scoped)

instance Serialize (JudocBlockParagraph 'Parsed)

instance NFData (JudocBlockParagraph 'Parsed)

deriving stock instance Show (JudocBlockParagraph 'Parsed)

deriving stock instance Show (JudocBlockParagraph 'Scoped)

deriving stock instance Eq (JudocBlockParagraph 'Parsed)

deriving stock instance Eq (JudocBlockParagraph 'Scoped)

deriving stock instance Ord (JudocBlockParagraph 'Parsed)

deriving stock instance Ord (JudocBlockParagraph 'Scoped)

data JudocGroup (s :: Stage)
  = JudocGroupBlock (JudocBlockParagraph s)
  | JudocGroupLines (NonEmpty (JudocBlock s))
  deriving stock (Generic)

instance Serialize (JudocGroup 'Scoped)

instance NFData (JudocGroup 'Scoped)

instance Serialize (JudocGroup 'Parsed)

instance NFData (JudocGroup 'Parsed)

deriving stock instance Show (JudocGroup 'Parsed)

deriving stock instance Show (JudocGroup 'Scoped)

deriving stock instance Eq (JudocGroup 'Parsed)

deriving stock instance Eq (JudocGroup 'Scoped)

deriving stock instance Ord (JudocGroup 'Parsed)

deriving stock instance Ord (JudocGroup 'Scoped)

newtype JudocBlock (s :: Stage)
  = JudocLines (NonEmpty (JudocLine s))
  deriving stock (Generic)

instance Serialize (JudocBlock 'Scoped)

instance NFData (JudocBlock 'Scoped)

instance Serialize (JudocBlock 'Parsed)

instance NFData (JudocBlock 'Parsed)

deriving stock instance Show (JudocBlock 'Parsed)

deriving stock instance Show (JudocBlock 'Scoped)

deriving stock instance Eq (JudocBlock 'Parsed)

deriving stock instance Eq (JudocBlock 'Scoped)

deriving stock instance Ord (JudocBlock 'Parsed)

deriving stock instance Ord (JudocBlock 'Scoped)

data JudocLine (s :: Stage) = JudocLine
  { _judocLineDelim :: Maybe KeywordRef,
    _judocLineAtoms :: NonEmpty (WithLoc (JudocAtom s))
  }
  deriving stock (Generic)

instance Serialize (JudocLine 'Scoped)

instance NFData (JudocLine 'Scoped)

instance Serialize (JudocLine 'Parsed)

instance NFData (JudocLine 'Parsed)

deriving stock instance Show (JudocLine 'Parsed)

deriving stock instance Show (JudocLine 'Scoped)

deriving stock instance Eq (JudocLine 'Parsed)

deriving stock instance Eq (JudocLine 'Scoped)

deriving stock instance Ord (JudocLine 'Parsed)

deriving stock instance Ord (JudocLine 'Scoped)

data JudocAtom (s :: Stage)
  = JudocExpression (ExpressionType s)
  | JudocText Text
  deriving stock (Generic)

instance Serialize (JudocAtom 'Scoped)

instance NFData (JudocAtom 'Scoped)

instance Serialize (JudocAtom 'Parsed)

instance NFData (JudocAtom 'Parsed)

deriving stock instance Show (JudocAtom 'Parsed)

deriving stock instance Show (JudocAtom 'Scoped)

deriving stock instance Eq (JudocAtom 'Parsed)

deriving stock instance Eq (JudocAtom 'Scoped)

deriving stock instance Ord (JudocAtom 'Parsed)

deriving stock instance Ord (JudocAtom 'Scoped)

data FunctionLhs (s :: Stage) = FunctionLhs
  { _funLhsBuiltin :: Maybe (WithLoc BuiltinFunction),
    _funLhsTerminating :: Maybe KeywordRef,
    _funLhsInstance :: Maybe KeywordRef,
    _funLhsCoercion :: Maybe KeywordRef,
    _funLhsName :: FunctionSymbolType s,
    _funLhsTypeSig :: TypeSig s,
    _funLhsIsTop :: FunctionIsTop
  }
  deriving stock (Generic)

instance Serialize (FunctionLhs 'Scoped)

instance NFData (FunctionLhs 'Scoped)

instance Serialize (FunctionLhs 'Parsed)

instance NFData (FunctionLhs 'Parsed)

deriving stock instance Show (FunctionLhs 'Parsed)

deriving stock instance Show (FunctionLhs 'Scoped)

deriving stock instance Eq (FunctionLhs 'Parsed)

deriving stock instance Eq (FunctionLhs 'Scoped)

deriving stock instance Ord (FunctionLhs 'Parsed)

deriving stock instance Ord (FunctionLhs 'Scoped)

makeLenses ''SideIfs
makeLenses ''ReservedInductiveDef
makeLenses ''RecordUpdatePun
makeLenses ''RecordUpdateFieldItemAssign
makeLenses ''FunctionDefNameScoped
makeLenses ''TypeSig
makeLenses ''FunctionLhs
makeLenses ''Statements
makeLenses ''NamedArgumentFunctionDef
makeLenses ''NamedArgumentPun
makeLenses ''SideIfBranch
makeLenses ''RhsExpression
makeLenses ''PatternArg
makeLenses ''WildcardConstructor
makeLenses ''DoubleBracesExpression
makeLenses ''PatternFieldPun
makeLenses ''RecordPatternAssign
makeLenses ''RecordPattern
makeLenses ''ParensRecordUpdate
makeLenses ''RecordUpdateExtra
makeLenses ''RecordUpdate
makeLenses ''RecordUpdateApp
makeLenses ''RecordUpdateField
makeLenses ''ProjectionDef
makeLenses ''ScopedIden
makeLenses ''FixityDef
makeLenses ''RecordField
makeLenses ''RhsRecord
makeLenses ''RhsAdt
makeLenses ''RhsGadt
makeLenses ''List
makeLenses ''ListPattern
makeLenses ''UsingItem
makeLenses ''HidingItem
makeLenses ''HidingList
makeLenses ''UsingList
makeLenses ''JudocLine
makeLenses ''Example
makeLenses ''Lambda
makeLenses ''LambdaClause
makeLenses ''Judoc
makeLenses ''JudocBlockParagraph
makeLenses ''Function
makeLenses ''InductiveDef
makeLenses ''WithModule
makeLenses ''PostfixApplication
makeLenses ''InfixApplication
makeLenses ''Application
makeLenses ''Let
makeLenses ''FunctionParameters
makeLenses ''Import
makeLenses ''OperatorSyntaxDef
makeLenses ''IteratorSyntaxDef
makeLenses ''ConstructorDef
makeLenses ''Module
makeLenses ''SigArg
makeLenses ''ArgDefault
makeLenses ''FunctionDef
makeLenses ''AxiomDef
makeLenses ''InductiveParameters
makeLenses ''InductiveParametersRhs
makeLenses ''OpenModule
makeLenses ''PatternApp
makeLenses ''PatternInfixApp
makeLenses ''PatternPostfixApp
makeLenses ''Case
makeLenses ''CaseBranch
makeLenses ''If
makeLenses ''IfBranch
makeLenses ''PatternBinding
makeLenses ''PatternAtoms
makeLenses ''ExpressionAtoms
makeLenses ''Iterator
makeLenses ''Initializer
makeLenses ''Range
makeLenses ''ArgumentBlock
makeLenses ''NamedArgumentAssign
makeLenses ''NamedArgument
makeLenses ''NamedApplication
makeLenses ''AliasDef
makeLenses ''FixitySyntaxDef
makeLenses ''ParsedFixityInfo
makeLenses ''ParsedFixityFields
makeLenses ''NameSignature
makeLenses ''RecordNameSignature
makeLenses ''NameBlock
makeLenses ''NameItem
makeLenses ''RecordInfo
makeLenses ''MarkdownInfo
makeLenses ''Deriving

makePrisms ''NamedArgument
makePrisms ''Statement
makePrisms ''ConstructorRhs
makePrisms ''FunctionDefNameParsed

functionDefBuiltin :: Lens' (FunctionDef s) (Maybe (WithLoc BuiltinFunction))
functionDefBuiltin = functionDefLhs . funLhsBuiltin

functionDefTerminating :: Lens' (FunctionDef s) (Maybe KeywordRef)
functionDefTerminating = functionDefLhs . funLhsTerminating

functionDefInstance :: Lens' (FunctionDef s) (Maybe KeywordRef)
functionDefInstance = functionDefLhs . funLhsInstance

functionDefCoercion :: Lens' (FunctionDef s) (Maybe KeywordRef)
functionDefCoercion = functionDefLhs . funLhsCoercion

functionDefName :: Lens' (FunctionDef s) (FunctionSymbolType s)
functionDefName = functionDefLhs . funLhsName

functionDefTypeSig :: Lens' (FunctionDef s) (TypeSig s)
functionDefTypeSig = functionDefLhs . funLhsTypeSig

fixityFieldHelper :: SimpleGetter (ParsedFixityFields s) (Maybe a) -> SimpleGetter (ParsedFixityInfo s) (Maybe a)
fixityFieldHelper l = to (^? fixityFields . _Just . l . _Just)

fixityAssoc :: SimpleGetter (ParsedFixityInfo s) (Maybe (BinaryAssoc))
fixityAssoc = fixityFieldHelper fixityFieldsAssoc

fixityPrecSame :: SimpleGetter (ParsedFixityInfo s) (Maybe (IdentifierType s))
fixityPrecSame = fixityFieldHelper fixityFieldsPrecSame

fixityPrecAbove :: SimpleGetter (ParsedFixityInfo s) (Maybe [IdentifierType s])
fixityPrecAbove = fixityFieldHelper fixityFieldsPrecAbove

fixityPrecBelow :: SimpleGetter (ParsedFixityInfo s) (Maybe [IdentifierType s])
fixityPrecBelow = fixityFieldHelper fixityFieldsPrecBelow

instance (SingI s) => HasLoc (LetStatement s) where
  getLoc = \case
    LetOpen o -> getLoc o
    LetAliasDef d -> getLoc d
    LetFunctionDef d -> getLoc d

instance (SingI s) => HasLoc (DoBind s) where
  getLoc DoBind {..} =
    getLocPatternParensType _doBindPattern
      <> getLocExpressionType _doBindExpression

instance (SingI s) => HasLoc (DoLet s) where
  getLoc DoLet {..} =
    getLoc _doLetKw
      <> getLoc (last _doLetStatements)

instance (SingI s) => HasLoc (DoStatement s) where
  getLoc = \case
    DoStatementExpression e -> getLocExpressionType e
    DoStatementBind b -> getLoc b
    DoStatementLet b -> getLoc b

instance (SingI s) => HasLoc (AliasDef s) where
  getLoc AliasDef {..} = getLoc _aliasDefSyntaxKw <> getLocIdentifierType _aliasDefAsName

instance HasLoc (ParsedFixityFields s) where
  getLoc d = getLoc l <> getLoc r
    where
      (l, r) = d ^. fixityFieldsBraces . unIrrelevant

instance HasLoc (ParsedFixityInfo s) where
  getLoc def = getLoc (def ^. fixityParsedArity) <>? (getLoc <$> def ^. fixityFields)

instance HasLoc (FixitySyntaxDef s) where
  getLoc def = getLoc (def ^. fixitySyntaxKw) <> getLoc (def ^. fixityInfo)

instance (SingI s) => HasLoc (SyntaxDef s) where
  getLoc = \case
    SyntaxFixity t -> getLoc t
    SyntaxOperator t -> getLoc t
    SyntaxIterator t -> getLoc t
    SyntaxAlias t -> getLoc t

instance (SingI s) => HasLoc (NamedArgumentAssign s) where
  getLoc NamedArgumentAssign {..} = getLocSymbolType _namedArgName <> getLocExpressionType _namedArgValue

instance (SingI s) => HasLoc (ArgumentBlock s) where
  getLoc ArgumentBlock {..} = case d of
    Just (l, r) -> getLoc l <> getLoc r
    Nothing -> getLocSpan _argBlockArgs
    where
      Irrelevant d = _argBlockDelims

instance HasAtomicity (ArgumentBlock s) where
  atomicity = const Atom

instance HasAtomicity (NamedApplication s) where
  atomicity = const (Aggregate updateFixity)

instance HasAtomicity (Do s) where
  atomicity = const Atom

instance HasAtomicity Expression where
  atomicity e = case e of
    ExpressionIdentifier {} -> Atom
    ExpressionHole {} -> Atom
    ExpressionDo d -> atomicity d
    ExpressionInstanceHole {} -> Atom
    ExpressionParensIdentifier {} -> Atom
    ExpressionApplication {} -> Aggregate appFixity
    ExpressionInfixApplication a -> Aggregate (getFixity a)
    ExpressionPostfixApplication a -> Aggregate (getFixity a)
    ExpressionLambda l -> atomicity l
    ExpressionLiteral l -> atomicity l
    ExpressionLet l -> atomicity l
    ExpressionBraces {} -> Atom
    ExpressionDoubleBraces {} -> Atom
    ExpressionList {} -> Atom
    ExpressionUniverse {} -> Atom
    ExpressionFunction {} -> Aggregate funFixity
    ExpressionCase c -> atomicity c
    ExpressionIf x -> atomicity x
    ExpressionIterator i -> atomicity i
    ExpressionNamedApplication i -> atomicity i
    ExpressionRecordUpdate {} -> Aggregate updateFixity
    ExpressionParensRecordUpdate {} -> Atom

expressionAtomicity :: forall s. (SingI s) => ExpressionType s -> Atomicity
expressionAtomicity e = case sing :: SStage s of
  SParsed -> atomicity e
  SScoped -> atomicity e

instance HasAtomicity (Iterator s) where
  atomicity = const Atom

instance HasAtomicity (Case s) where
  atomicity = const Atom

instance HasAtomicity (If s) where
  atomicity = const Atom

instance HasAtomicity (Let 'Scoped) where
  atomicity l = atomicity (l ^. letExpression)

instance HasAtomicity (PatternAtom 'Parsed) where
  atomicity = const Atom

instance (SingI s) => HasAtomicity (FunctionParameters s) where
  atomicity p
    | not (null (p ^. paramNames))
        || p ^. paramImplicit == Implicit
        || p ^. paramImplicit == ImplicitInstance =
        Atom
    | otherwise = case sing :: SStage s of
        SParsed -> atomicity (p ^. paramType)
        SScoped -> atomicity (p ^. paramType)

instance Pretty ScopedIden where
  pretty = pretty . (^. scopedIdenSrcName)

instance HasLoc ScopedIden where
  getLoc = getLoc . (^. scopedIdenSrcName)

instance (SingI s) => HasLoc (InductiveParameters s) where
  getLoc i = getLocSymbolType (i ^. inductiveParametersNames . _head1) <>? (getLocExpressionType <$> (i ^? inductiveParametersRhs . _Just . inductiveParametersType))

getLocTypeSig :: (SingI s) => TypeSig s -> Maybe Interval
getLocTypeSig TypeSig {..} =
  (getLocSpan <$> nonEmpty _typeSigArgs)
    ?<>? (getLocExpressionType <$> _typeSigRetType)

instance HasLoc (RhsRecord s) where
  getLoc RhsRecord {..} =
    let (kat, kl, kr) = _rhsRecordDelim ^. unIrrelevant
     in (getLoc <$> kat) ?<> getLoc kl <> getLoc kr

instance (SingI s) => HasLoc (RhsGadt s) where
  getLoc RhsGadt {..} = fromJust (getLocTypeSig _rhsGadtTypeSig)

getLocRhsAdt :: (SingI s) => RhsAdt s -> Maybe Interval
getLocRhsAdt RhsAdt {..} = getLocSpan' getLocExpressionType <$> nonEmpty _rhsAdtArguments

getLocConstructorRhs :: (SingI s) => ConstructorRhs s -> Maybe Interval
getLocConstructorRhs = \case
  ConstructorRhsGadt a -> Just (getLoc a)
  ConstructorRhsAdt a -> getLocRhsAdt a
  ConstructorRhsRecord a -> Just (getLoc a)

instance (SingI s) => HasLoc (ConstructorDef s) where
  getLoc ConstructorDef {..} =
    (getLoc <$> (_constructorPipe ^. unIrrelevant))
      ?<> ( getLocSymbolType _constructorName
              <>? getLocConstructorRhs _constructorRhs
          )

instance HasLoc (WithModule s) where
  getLoc WithModule {..} = getLoc _withModuleWithKw <> getLoc _withModuleEndKw

instance HasLoc (InductiveDef s) where
  getLoc i =
    ( (getLoc <$> i ^. inductivePositive)
        ?<> getLoc (i ^. inductiveKw)
    )
      <>? (getLoc <$> i ^. inductiveWithModule)

instance (SingI s) => HasLoc (AxiomDef s) where
  getLoc m = getLoc (m ^. axiomKw) <> getLocExpressionType (fromJust (m ^. axiomTypeSig . typeSigRetType))

getLocPublicAnn :: PublicAnn -> Maybe Interval
getLocPublicAnn p = getLoc <$> p ^? _Public

instance HasLoc (OpenModule s short) where
  getLoc OpenModule {..} =
    getLoc _openModuleKw
      <>? fmap getLoc _openModuleUsingHiding
      <>? getLocPublicAnn _openModulePublic

instance (SingI s) => HasLoc (ProjectionDef s) where
  getLoc = getLocSymbolType . (^. projectionConstructor)

getLocFunctionSymbolType :: forall s. (SingI s) => FunctionSymbolType s -> Interval
getLocFunctionSymbolType = case sing :: SStage s of
  SParsed -> getLoc
  SScoped -> getLoc

instance HasLoc FunctionDefNameScoped where
  getLoc FunctionDefNameScoped {..} =
    getLoc _functionDefNameScoped
      <>? (getLoc <$> _functionDefNamePattern)

instance HasLoc FunctionDefNameParsed where
  getLoc = \case
    FunctionDefNamePattern a -> getLoc a
    FunctionDefName s -> getLoc s

instance (SingI s) => HasLoc (FunctionLhs s) where
  getLoc FunctionLhs {..} =
    (getLoc <$> _funLhsBuiltin)
      ?<> (getLoc <$> _funLhsTerminating)
      ?<> ( getLocFunctionSymbolType _funLhsName
              <>? (getLocExpressionType <$> _funLhsTypeSig ^. typeSigRetType)
          )

instance (SingI s) => HasLoc (Deriving s) where
  getLoc Deriving {..} =
    getLoc _derivingKw
      <> getLoc _derivingFunLhs

instance HasLoc (Statement 'Scoped) where
  getLoc :: Statement 'Scoped -> Interval
  getLoc = \case
    StatementSyntax t -> getLoc t
    StatementReservedInductive t -> absurd t
    StatementDeriving t -> getLoc t
    StatementFunctionDef t -> getLoc t
    StatementImport t -> getLoc t
    StatementInductive t -> getLoc t
    StatementModule t -> getLoc t
    StatementOpenModule t -> getLoc t
    StatementAxiom t -> getLoc t
    StatementProjectionDef t -> getLoc t

instance HasLoc Application where
  getLoc (Application l r) = getLoc l <> getLoc r

instance HasLoc InfixApplication where
  getLoc (InfixApplication l _ r) = getLoc l <> getLoc r

instance HasLoc PostfixApplication where
  getLoc (PostfixApplication l o) = getLoc l <> getLoc o

instance (SingI s) => HasLoc (LambdaClause s) where
  getLoc c =
    let locparams = case sing :: SStage s of
          SParsed -> getLocSpan (c ^. lambdaParameters)
          SScoped -> getLocSpan (c ^. lambdaParameters)
     in fmap getLoc (c ^. lambdaPipe . unIrrelevant)
          ?<> locparams
          <> getLocExpressionType (c ^. lambdaBody)

instance HasLoc (Lambda s) where
  getLoc l = getLoc (l ^. lambdaKw) <> getLoc (l ^. lambdaBraces . unIrrelevant . _2)

instance (SingI s) => HasLoc (FunctionParameter s) where
  getLoc = \case
    FunctionParameterName n -> getLocSymbolType n
    FunctionParameterWildcard w -> getLoc w

instance (SingI s) => HasLoc (FunctionParameters s) where
  getLoc p = case p ^. paramDelims . unIrrelevant of
    Nothing -> (getLoc <$> listToMaybe (p ^. paramNames)) ?<> getLocExpressionType (p ^. paramType)
    Just (l, r) -> getLoc l <> getLoc r

instance (SingI s) => HasLoc (Function s) where
  getLoc f = getLoc (f ^. funParameters) <> getLocExpressionType (f ^. funReturn)

instance (SingI s) => HasLoc (Let s) where
  getLoc l = getLoc (l ^. letKw) <> getLocExpressionType (l ^. letExpression)

instance (SingI s) => HasLoc (SideIfBranch s k) where
  getLoc SideIfBranch {..} =
    (getLoc <$> _sideIfBranchPipe ^. unIrrelevant)
      ?<> getLocExpressionType _sideIfBranchBody

instance (SingI s) => HasLoc (SideIfs s) where
  getLoc SideIfs {..} =
    getLocSpan _sideIfBranches
      <>? (getLoc <$> _sideIfElse)

instance (SingI s) => HasLoc (RhsExpression s) where
  getLoc RhsExpression {..} =
    getLoc _rhsExpressionAssignKw
      <> getLocExpressionType _rhsExpression

instance (SingI s) => HasLoc (CaseBranchRhs s) where
  getLoc = \case
    CaseBranchRhsExpression e -> getLoc e
    CaseBranchRhsIf e -> getLoc e

instance (SingI s) => HasLoc (CaseBranch s) where
  getLoc c = case c ^. caseBranchPipe . unIrrelevant of
    Nothing -> branchLoc
    Just p -> getLoc p <> branchLoc
    where
      branchLoc :: Interval
      branchLoc = getLoc (c ^. caseBranchRhs)

instance (SingI s) => HasLoc (IfBranch s k) where
  getLoc c = getLoc (c ^. ifBranchPipe) <> getLocExpressionType (c ^. ifBranchExpression)

instance (SingI s) => HasLoc (Case s) where
  getLoc c = getLoc (c ^. caseKw) <> getLoc (c ^. caseBranches . to last)

instance (SingI s) => HasLoc (If s) where
  getLoc c = getLoc (c ^. ifKw) <> getLoc (c ^. ifBranchElse)

instance HasLoc (List s) where
  getLoc List {..} = getLoc _listBracketL <> getLoc _listBracketR

instance (SingI s) => HasLoc (NamedArgumentFunctionDef s) where
  getLoc (NamedArgumentFunctionDef f) = getLoc f

instance (SingI s) => HasLoc (NamedArgument s) where
  getLoc = \case
    NamedArgumentFunction f -> getLoc f
    NamedArgumentItemPun f -> getLoc f

instance HasLoc (RecordUpdatePun s) where
  getLoc RecordUpdatePun {..} = getLocSymbolType _recordUpdatePunSymbol

instance HasLoc (NamedArgumentPun s) where
  getLoc NamedArgumentPun {..} = getLocSymbolType _namedArgumentPunSymbol

instance (SingI s) => HasLoc (NamedApplication s) where
  getLoc NamedApplication {..} = getLocIdentifierType _namedApplicationName

instance (SingI s) => HasLoc (RecordUpdateField s) where
  getLoc = \case
    RecordUpdateFieldAssign a -> getLoc a
    RecordUpdateFieldPun a -> getLoc a

instance (SingI s) => HasLoc (RecordUpdateFieldItemAssign s) where
  getLoc f = getLocSymbolType (f ^. fieldUpdateName) <> getLocExpressionType (f ^. fieldUpdateValue)

instance HasLoc (RecordUpdate s) where
  getLoc r = getLoc (r ^. recordUpdateAtKw) <> getLoc (r ^. recordUpdateDelims . unIrrelevant . _2)

instance HasLoc RecordUpdateApp where
  getLoc r = getLoc (r ^. recordAppExpression) <> getLoc (r ^. recordAppUpdate)

instance HasLoc ParensRecordUpdate where
  getLoc = getLoc . (^. parensRecordUpdate)

instance HasLoc (DoubleBracesExpression s) where
  getLoc DoubleBracesExpression {..} =
    let (l, r) = _doubleBracesDelims ^. unIrrelevant
     in getLoc l <> getLoc r

instance HasAtomicity (DoubleBracesExpression s) where
  atomicity = const Atom

instance HasLoc (Do s) where
  getLoc Do {..} =
    getLoc _doKeyword <> getLoc (_doDelims ^. unIrrelevant . _2)

instance HasLoc Expression where
  getLoc = \case
    ExpressionIdentifier i -> getLoc i
    ExpressionParensIdentifier i -> getLoc i
    ExpressionApplication i -> getLoc i
    ExpressionInfixApplication i -> getLoc i
    ExpressionPostfixApplication i -> getLoc i
    ExpressionLambda i -> getLoc i
    ExpressionList l -> getLoc l
    ExpressionCase i -> getLoc i
    ExpressionIf x -> getLoc x
    ExpressionLet i -> getLoc i
    ExpressionDo i -> getLoc i
    ExpressionUniverse i -> getLoc i
    ExpressionLiteral i -> getLoc i
    ExpressionFunction i -> getLoc i
    ExpressionHole i -> getLoc i
    ExpressionInstanceHole i -> getLoc i
    ExpressionBraces i -> getLoc i
    ExpressionDoubleBraces i -> getLoc i
    ExpressionIterator i -> getLoc i
    ExpressionNamedApplication i -> getLoc i
    ExpressionRecordUpdate i -> getLoc i
    ExpressionParensRecordUpdate i -> getLoc i

getLocIdentifierType :: forall s. (SingI s) => IdentifierType s -> Interval
getLocIdentifierType e = case sing :: SStage s of
  SParsed -> getLoc e
  SScoped -> getLoc e

instance (SingI s) => HasLoc (Iterator s) where
  getLoc Iterator {..} = getLocIdentifierType _iteratorName <> getLocExpressionType _iteratorBody

instance HasLoc (HidingList s) where
  getLoc HidingList {..} =
    let rbra = _hidingBraces ^. unIrrelevant . _2
     in getLoc (_hidingKw ^. unIrrelevant) <> getLoc rbra

instance HasLoc (UsingList s) where
  getLoc UsingList {..} =
    let rbra = _usingBraces ^. unIrrelevant . _2
     in getLoc (_usingKw ^. unIrrelevant) <> getLoc rbra

instance HasLoc (UsingHiding s) where
  getLoc = \case
    Using u -> getLoc u
    Hiding u -> getLoc u

instance (SingI s) => HasLoc (Import s) where
  getLoc Import {..} =
    let sLoc = case sing :: SStage s of
          SParsed ->
            getLoc _importKw
              <> getLoc _importModulePath
              <>? (getLoc <$> _importOpen)
          SScoped ->
            getLoc _importKw
              <> getLoc _importModulePath
              <>? (getLoc <$> _importOpen)
     in sLoc <>? fmap getLoc (_importPublic ^? _Public)

instance (SingI s, SingI t) => HasLoc (Module s t) where
  getLoc m = case sing :: SStage s of
    SParsed -> case sing :: SModuleIsTop t of
      SModuleLocal -> getLoc (m ^. modulePath)
      SModuleTop -> getLoc (m ^. modulePath)
    SScoped -> case sing :: SModuleIsTop t of
      SModuleLocal -> getLoc (m ^. modulePath)
      SModuleTop -> getLoc (m ^. modulePath)

getLocSymbolType :: forall s. (SingI s) => SymbolType s -> Interval
getLocSymbolType = case sing :: SStage s of
  SParsed -> getLoc
  SScoped -> getLoc

getLocHoleType :: forall s. (SingI s) => HoleType s -> Interval
getLocHoleType = case sing :: SStage s of
  SParsed -> getLoc
  SScoped -> getLoc

getLocExpressionType :: forall s. (SingI s) => ExpressionType s -> Interval
getLocExpressionType = case sing :: SStage s of
  SParsed -> getLoc
  SScoped -> getLoc

instance (SingI s) => HasLoc (ArgDefault s) where
  getLoc ArgDefault {..} = getLoc _argDefaultAssign <> getLocExpressionType _argDefaultValue

instance HasLoc (SigArg s) where
  getLoc SigArg {..} = getLoc l <> getLoc r
    where
      Irrelevant (l, r) = _sigArgDelims

instance (SingI s) => HasLoc (FunctionClause s) where
  getLoc FunctionClause {..} =
    getLoc _clausenPipeKw
      <> getLocExpressionType _clausenBody

instance (SingI s) => HasLoc (FunctionDefBody s) where
  getLoc = \case
    SigBodyExpression e -> getLocExpressionType e
    SigBodyClauses cl -> getLocSpan cl

instance (SingI s) => HasLoc (FunctionDef s) where
  getLoc FunctionDef {..} =
    let FunctionLhs {..} = _functionDefLhs
     in (getLoc <$> _functionDefDoc)
          ?<> (getLoc <$> _functionDefPragmas)
          ?<> (getLoc <$> _funLhsBuiltin)
          ?<> (getLoc <$> _funLhsTerminating)
          ?<> (getLocFunctionSymbolType _funLhsName)
          <> getLoc _functionDefBody

instance HasLoc (Example s) where
  getLoc e = e ^. exampleLoc

instance HasLoc (Judoc s) where
  getLoc (Judoc j) = getLocSpan j

instance HasLoc (JudocBlockParagraph s) where
  getLoc p = getLoc (p ^. judocBlockParagraphStart) <> getLoc (p ^. judocBlockParagraphEnd)

instance HasLoc (JudocGroup s) where
  getLoc = \case
    JudocGroupBlock l -> getLoc l
    JudocGroupLines l -> getLocSpan l

instance HasLoc (JudocBlock s) where
  getLoc = \case
    JudocLines ls -> getLocSpan ls

instance HasLoc PatternScopedIden where
  getLoc = \case
    PatternScopedVar v -> getLoc v
    PatternScopedConstructor c -> getLoc c

instance HasLoc PatternBinding where
  getLoc PatternBinding {..} = getLoc _patternBindingName <> getLoc _patternBindingPattern

instance HasLoc (ListPattern s) where
  getLoc l = getLoc (l ^. listpBracketL) <> getLoc (l ^. listpBracketR)

getLocPatternParensType :: forall s. (SingI s) => PatternParensType s -> Interval
getLocPatternParensType = case sing :: SStage s of
  SScoped -> getLoc
  SParsed -> getLoc

getLocPatternAtomType :: forall s. (SingI s) => PatternAtomType s -> Interval
getLocPatternAtomType = case sing :: SStage s of
  SScoped -> getLoc
  SParsed -> getLoc

instance (SingI s) => HasLoc (RecordPatternAssign s) where
  getLoc a =
    getLoc (a ^. recordPatternAssignField)
      <> getLocPatternParensType (a ^. recordPatternAssignPattern)

instance (SingI s) => HasLoc (PatternFieldPun s) where
  getLoc f = getLocSymbolType (f ^. fieldPunField)

instance (SingI s) => HasLoc (RecordPatternItem s) where
  getLoc = \case
    RecordPatternItemAssign a -> getLoc a
    RecordPatternItemFieldPun a -> getLoc a

instance (SingI s) => HasLoc (RecordPattern s) where
  getLoc r = getLocIdentifierType (r ^. recordPatternConstructor) <>? (getLocSpan <$> nonEmpty (r ^. recordPatternItems))

instance (SingI s) => HasLoc (WildcardConstructor s) where
  getLoc WildcardConstructor {..} =
    getLocIdentifierType _wildcardConstructor

instance (SingI s) => HasLoc (PatternAtom s) where
  getLoc = \case
    PatternAtomIden i -> getLocIden i
    PatternAtomWildcard w -> getLoc w
    PatternAtomWildcardConstructor w -> getLoc w
    PatternAtomEmpty i -> i
    PatternAtomList l -> getLoc l
    PatternAtomParens p -> getLocParens p
    PatternAtomBraces p -> getLocParens p
    PatternAtomDoubleBraces p -> getLocParens p
    PatternAtomAt p -> getLocAt p
    PatternAtomRecord p -> getLoc p
    where
      getLocAt :: forall r. (SingI r) => PatternAtType r -> Interval
      getLocAt p = case sing :: SStage r of
        SParsed -> getLoc p
        SScoped -> getLoc p
      getLocIden :: forall r. (SingI r) => PatternAtomIdenType r -> Interval
      getLocIden p = case sing :: SStage r of
        SParsed -> getLoc p
        SScoped -> getLoc p
      getLocParens :: forall r. (SingI r) => PatternParensType r -> Interval
      getLocParens p = case sing :: SStage r of
        SParsed -> getLoc p
        SScoped -> getLoc p

instance HasLoc (JudocLine s) where
  getLoc (JudocLine delim atoms) = fmap getLoc delim ?<> getLocSpan atoms

instance HasLoc (PatternAtoms s) where
  getLoc = getLoc . (^. patternAtomsLoc)

instance HasLoc PatternArg where
  getLoc a = fmap getLoc (a ^. patternArgName) ?<> getLoc (a ^. patternArgPattern)

instance HasLoc PatternInfixApp where
  getLoc (PatternInfixApp l _ r) =
    getLoc l <> getLoc r

instance HasLoc PatternPostfixApp where
  getLoc (PatternPostfixApp l _) = getLoc l

instance HasLoc PatternApp where
  getLoc (PatternApp l r) = getLoc l <> getLoc r

instance HasLoc Pattern where
  getLoc = \case
    PatternVariable v -> getLoc v
    PatternWildcardConstructor v -> getLoc v
    PatternConstructor c -> getLoc c
    PatternApplication a -> getLoc a
    PatternWildcard w -> getLoc w
    PatternList w -> getLoc w
    PatternEmpty i -> i
    PatternInfixApplication i -> getLoc i
    PatternPostfixApplication i -> getLoc i
    PatternRecord i -> getLoc i

instance (SingI s) => HasLoc (ExpressionAtom s) where
  getLoc = \case
    AtomIdentifier i -> getLocIdentifierType i
    AtomLambda x -> getLoc x
    AtomList x -> getLoc x
    AtomCase x -> getLoc x
    AtomIf x -> getLoc x
    AtomHole x -> getLocHoleType x
    AtomInstanceHole x -> getLocHoleType x
    AtomDoubleBraces x -> getLoc x
    AtomBraces x -> getLoc x
    AtomDo x -> getLoc x
    AtomLet x -> getLoc x
    AtomRecordUpdate x -> getLoc x
    AtomUniverse x -> getLoc x
    AtomFunction x -> getLoc x
    AtomFunArrow x -> getLoc x
    AtomLiteral x -> getLoc x
    AtomParens x -> getLocExpressionType x
    AtomIterator x -> getLoc x
    AtomNamedApplication x -> getLoc x

instance HasLoc (ExpressionAtoms s) where
  getLoc = getLoc . (^. expressionAtomsLoc)

instance HasAtomicity (ExpressionAtoms 'Parsed) where
  atomicity ExpressionAtoms {..} = case _expressionAtoms of
    (_ :| []) -> Atom
    (_ :| _)
      | any isArrow _expressionAtoms -> Aggregate funFixity
      | otherwise -> Aggregate appFixity
      where
        isArrow :: ExpressionAtom s -> Bool
        isArrow = \case
          AtomFunArrow {} -> True
          _ -> False

data ApeLeaf
  = ApeLeafExpression Expression
  | ApeLeafFunctionParams (FunctionParameters 'Scoped)
  | ApeLeafFunctionKw KeywordRef
  | ApeLeafPattern Pattern
  | ApeLeafPatternArg PatternArg
  | ApeLeafAtom (AnyStage ExpressionAtom)

_SyntaxAlias :: Traversal' (SyntaxDef s) (AliasDef s)
_SyntaxAlias f x = case x of
  SyntaxAlias r -> SyntaxAlias <$> f r
  _ -> pure x

_RecordStatementField :: Traversal' (RecordStatement s) (RecordField s)
_RecordStatementField f x = case x of
  RecordStatementField p -> RecordStatementField <$> f p
  _ -> pure x

symbolParsed :: forall s. (SingI s) => SymbolType s -> Symbol
symbolParsed sym = case sing :: SStage s of
  SParsed -> sym
  SScoped -> sym ^. S.nameConcrete

getFunctionSymbol :: forall s. (SingI s) => FunctionSymbolType s -> SymbolType s
getFunctionSymbol sym = case sing :: SStage s of
  SParsed -> case sym of
    FunctionDefName p -> p
    FunctionDefNamePattern {} -> impossibleError "invalid call"
  SScoped -> sym ^. functionDefNameScoped

modulePathTypeKey :: forall s. (SingI s) => ModulePathType s 'ModuleTop -> TopModulePathKey
modulePathTypeKey p = case sing :: SStage s of
  SParsed -> topModulePathKey p
  SScoped -> topModulePathKey (p ^. S.nameConcrete)

functionSymbolPattern :: forall s. (SingI s) => FunctionSymbolType s -> Maybe (PatternAtomType s)
functionSymbolPattern f = case sing :: SStage s of
  SParsed -> f ^? _FunctionDefNamePattern
  SScoped -> f ^. functionDefNamePattern

withFunctionSymbol :: forall s a. (SingI s) => a -> (SymbolType s -> a) -> FunctionSymbolType s -> a
withFunctionSymbol a f sym = case sing :: SStage s of
  SParsed -> maybe a f (sym ^? _FunctionDefName)
  SScoped -> f (sym ^. functionDefNameScoped)

namedArgumentSymbolParsed :: (SingI s) => SimpleGetter (NamedArgument s) Symbol
namedArgumentSymbolParsed = to $ \case
  NamedArgumentItemPun a -> a ^. namedArgumentPunSymbol
  NamedArgumentFunction a -> symbolParsed (getFunctionSymbol (a ^. namedArgumentFunctionDef . functionDefName))

namedArgumentSymbol :: Lens' (NamedArgument 'Parsed) Symbol
namedArgumentSymbol f = \case
  NamedArgumentItemPun a -> NamedArgumentItemPun <$> (namedArgumentPunSymbol f a)
  NamedArgumentFunction a -> do
    a' <- f (a ^?! namedArgumentFunctionDef . functionDefName . _FunctionDefName)
    return $ NamedArgumentFunction (over namedArgumentFunctionDef (set functionDefName (FunctionDefName a')) a)

scopedIdenSrcName :: Lens' ScopedIden S.Name
scopedIdenSrcName f n = case n ^. scopedIdenAlias of
  Nothing -> scopedIdenFinal f n
  Just a -> do
    a' <- f a
    pure (set scopedIdenAlias (Just a') n)

fromParsedIteratorInfo :: Maybe ParsedIteratorInfo -> IteratorInfo
fromParsedIteratorInfo = \case
  Nothing -> emptyIteratorInfo
  Just ParsedIteratorInfo {..} ->
    IteratorInfo
      { _iteratorInfoInitNum = (^. withLocParam) <$> _parsedIteratorInfoInitNum,
        _iteratorInfoRangeNum = (^. withLocParam) <$> _parsedIteratorInfoRangeNum
      }

nameBlockSymbols :: forall s. Traversal' (NameBlock s) (SymbolType s)
nameBlockSymbols = nameBlockItems . each . nameItemSymbol . _Just

instance HasFixity PostfixApplication where
  getFixity (PostfixApplication _ op) = fromMaybe impossible (op ^. scopedIdenSrcName . S.nameFixity)

instance HasFixity InfixApplication where
  getFixity (InfixApplication _ op _) = fromMaybe impossible (op ^. scopedIdenSrcName . S.nameFixity)

instance HasFixity PatternInfixApp where
  getFixity (PatternInfixApp _ op _) = fromMaybe impossible (op ^. scopedIdenSrcName . S.nameFixity)

instance HasFixity PatternPostfixApp where
  getFixity (PatternPostfixApp _ op) = fromMaybe impossible (op ^. scopedIdenSrcName . S.nameFixity)

instance HasAtomicity (ListPattern s) where
  atomicity = const Atom

instance HasAtomicity (RecordPattern s) where
  atomicity = const Atom

instance HasAtomicity (WildcardConstructor s) where
  atomicity = const Atom

instance HasAtomicity Pattern where
  atomicity e = case e of
    PatternVariable {} -> Atom
    PatternWildcardConstructor a -> atomicity a
    PatternConstructor {} -> Atom
    PatternApplication {} -> Aggregate appFixity
    PatternInfixApplication a -> Aggregate (getFixity a)
    PatternPostfixApplication p -> Aggregate (getFixity p)
    PatternWildcard {} -> Atom
    PatternList l -> atomicity l
    PatternEmpty {} -> Atom
    PatternRecord r -> atomicity r

instance (SingI s) => HasLoc (OperatorSyntaxDef s) where
  getLoc OperatorSyntaxDef {..} = getLoc _opSyntaxKw <> getLocIdentifierType _opSymbol

instance (SingI s) => HasLoc (IteratorSyntaxDef s) where
  getLoc IteratorSyntaxDef {..} = getLoc _iterSyntaxKw <> getLocIdentifierType _iterSymbol
