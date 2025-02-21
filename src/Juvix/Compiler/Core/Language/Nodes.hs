-- | Polymorphic Node types
module Juvix.Compiler.Core.Language.Nodes
  ( module Juvix.Compiler.Core.Language.Base,
    module Juvix.Compiler.Core.Language.Primitives,
    module Juvix.Compiler.Core.Language.Builtins,
    module Juvix.Compiler.Core.Language.Nodes,
  )
where

import Data.Serialize
import Juvix.Compiler.Core.Language.Base
import Juvix.Compiler.Core.Language.Builtins
import Juvix.Compiler.Core.Language.Primitives
import Juvix.Data.Field

-- | De Bruijn index of a locally bound variable.
data Var' i = Var
  { _varInfo :: i,
    _varIndex :: !Index
  }
  deriving stock (Generic)

-- | Global identifier of a function (with corresponding `Node` in the global
-- context).
data Ident' i = Ident
  { _identInfo :: i,
    _identSymbol :: !Symbol
  }
  deriving stock (Generic)

data Constant' i = Constant
  { _constantInfo :: i,
    _constantValue :: !ConstantValue
  }
  deriving stock (Generic)

data ConstantValue
  = ConstInteger !Integer
  | ConstField !FField
  | ConstString !Text
  | ConstUInt8 !Word8
  | ConstByteArray !ByteString
  deriving stock (Eq, Generic)

-- | Info about a single binder. Associated with Lambda, Pi, Let, Case or Match.
data Binder' ty = Binder
  { _binderName :: Text,
    _binderLocation :: Maybe Location,
    _binderType :: ty
  }
  deriving stock (Generic)

-- Other things we might need in the future:
-- - ConstFloat or ConstFixedPoint

data App' i a = App
  { _appInfo :: i,
    _appLeft :: !a,
    _appRight :: !a
  }
  deriving stock (Generic)

data Apps' i f a = Apps
  { _appsInfo :: i,
    _appsFun :: !f,
    _appsArgs :: !(NonEmpty a)
  }
  deriving stock (Generic)

-- | A builtin application. A builtin has no corresponding Node. It is treated
-- specially by the evaluator and the code generator. For example, basic
-- arithmetic operations go into `Builtin`. The number of arguments supplied
-- must be equal to the number of arguments expected by the builtin operation
-- (this simplifies evaluation and code generation). If you need partial
-- application, eta-expand with lambdas, e.g., eta-expand `(+) 2` to `\x -> (+)
-- 2 x`. See Transformation/Eta.hs.
data BuiltinApp' i a = BuiltinApp
  { _builtinAppInfo :: i,
    _builtinAppOp :: !BuiltinOp,
    _builtinAppArgs :: ![a]
  }
  deriving stock (Generic)

-- | A data constructor application. The number of arguments supplied must be
-- equal to the number of arguments expected by the constructor.
data Constr' i a = Constr
  { _constrInfo :: i,
    _constrTag :: !Tag,
    _constrArgs :: ![a]
  }
  deriving stock (Generic)

-- | Useful for unfolding lambdas
data LambdaLhs' i ty = LambdaLhs
  { _lambdaLhsInfo :: i,
    _lambdaLhsBinder :: Binder' ty
  }

data Lambda' i a ty = Lambda
  { _lambdaInfo :: i,
    _lambdaBinder :: Binder' ty,
    _lambdaBody :: !a
  }
  deriving stock (Generic)

-- | `let x := value in body` is not reducible to lambda + application for the
-- purposes of ML-polymorphic / dependent type checking or code generation!
data Let' i a ty = Let
  { _letInfo :: i,
    _letItem :: {-# UNPACK #-} !(LetItem' a ty),
    _letBody :: !a
  }
  deriving stock (Generic)

data LetItem' a ty = LetItem
  { _letItemBinder :: Binder' ty,
    _letItemValue :: a
  }
  deriving stock (Generic)

-- | Represents a block of mutually recursive local definitions. Both in the
-- body and in the values `length _letRecValues` implicit binders are introduced
-- which hold the functions/values being defined. The last item in _letRecValues
-- has index $0 in the body. The types of the binders (in LetItems) are *not* in
-- the scope of the binders, i.e., the binders of the values cannot refer to the
-- values.
data LetRec' i a ty = LetRec
  { _letRecInfo :: i,
    _letRecValues :: !(NonEmpty (LetItem' a ty)),
    _letRecBody :: !a
  }
  deriving stock (Generic)

-- | One-level case matching on the tag of a data constructor: `Case value
-- branches default`. `Case` is lazy: only the selected branch is evaluated.
data Case' i bi a ty = Case
  { _caseInfo :: i,
    _caseInductive :: Symbol,
    _caseValue :: !a,
    _caseBranches :: ![CaseBranch' bi a ty],
    _caseDefault :: !(Maybe a)
  }
  deriving stock (Generic)

-- | `CaseBranch tag binders bindersNum branch`
-- - `binders` are the arguments of the constructor tagged with `tag`,
--  length of `binders` is equal to `bindersNum`
data CaseBranch' i a ty = CaseBranch
  { _caseBranchInfo :: i,
    _caseBranchTag :: !Tag,
    _caseBranchBinders :: [Binder' ty],
    _caseBranchBindersNum :: !Int,
    _caseBranchBody :: !a
  }
  deriving stock (Generic)

-- | A special form of `Case` for the booleans. Used only in Core.Stripped.
data If' i a = If
  { _ifInfo :: i,
    _ifValue :: !a,
    _ifTrue :: !a,
    _ifFalse :: !a
  }
  deriving stock (Generic)

-- | Complex pattern match. `Match` is lazy: only the selected branch is evaluated.
data Match' i a = Match
  { _matchInfo :: i,
    _matchValueTypes :: !(NonEmpty a),
    _matchReturnType :: !a,
    _matchValues :: !(NonEmpty a),
    _matchBranches :: ![MatchBranch' i a]
  }
  deriving stock (Generic)

-- | The patterns introduce binders from left to right, with the binder for a
-- constructor before the binders for the subpatterns, e.g., matching on the
-- patterns '(C (D x) y) u@(E _ z)' with body 'f x y z u' may be represented by
-- a 'MatchBranch' with '_matchBranchPatterns' equal to '[PatConstr Ctag
-- [PatConstr Dtag [PatWildcard], PatWildcard], PatConstr Etag [PatWildcard,
-- PatWildcard]]' and '_matchBranchBody' equal to 'App (App (App (Ident f) (Var
-- 4)) (Var 3)) (Var 0) (Var 2)'. So the de Bruijn indices increase from right
-- to left.
data MatchBranch' i a = MatchBranch
  { _matchBranchInfo :: i,
    _matchBranchPatterns :: !(NonEmpty (Pattern' i a)),
    _matchBranchRhs :: !(MatchBranchRhs' i a)
  }
  deriving stock (Generic)

data Pattern' i a
  = PatWildcard (PatternWildcard' i a)
  | PatConstr (PatternConstr' i a)
  deriving stock (Generic)

data PatternWildcard' i a = PatternWildcard
  { _patternWildcardInfo :: i,
    _patternWildcardBinder :: Binder' a
  }
  deriving stock (Generic)

data PatternConstr' i a = PatternConstr
  { _patternConstrInfo :: i,
    _patternConstrFixity :: Maybe Fixity,
    _patternConstrBinder :: Binder' a,
    _patternConstrTag :: !Tag,
    _patternConstrArgs :: ![Pattern' i a]
  }
  deriving stock (Generic)

data MatchBranchRhs' i a
  = MatchBranchRhsExpression !a
  | MatchBranchRhsIfs !(NonEmpty (SideIfBranch' i a))
  deriving stock (Generic)

data SideIfBranch' i a = SideIfBranch
  { _sideIfBranchInfo :: i,
    _sideIfBranchCondition :: !a,
    _sideIfBranchBody :: !a
  }
  deriving stock (Generic)

-- | Useful for unfolding Pi
data PiLhs' i a = PiLhs
  { _piLhsInfo :: i,
    _piLhsBinder :: Binder' a
  }
  deriving stock (Generic)

-- | Dependent Pi-type. Compilation-time only. Pi implicitly introduces a binder
-- in the body, exactly like Lambda. So `Pi info ty body` is `Pi x : ty .
-- body` in more familiar notation, but references to `x` in `body` are via de
-- Bruijn index. For example, Pi A : Type . A -> A translates to (omitting
-- Infos): Pi (Univ level) (Pi (Var 0) (Var 1)).
data Pi' i a = Pi
  { _piInfo :: i,
    _piBinder :: Binder' a,
    _piBody :: !a
  }
  deriving stock (Generic)

-- | Universe. Compilation-time only.
data Univ' i = Univ
  { _univInfo :: i,
    _univLevel :: !Int
  }
  deriving stock (Generic)

-- | Type constructor application. Compilation-time only.
data TypeConstr' i a = TypeConstr
  { _typeConstrInfo :: i,
    _typeConstrSymbol :: !Symbol,
    _typeConstrArgs :: ![a]
  }
  deriving stock (Generic)

-- | A primitive type.
data TypePrim' i = TypePrim
  { _typePrimInfo :: i,
    _typePrimPrimitive :: Primitive
  }
  deriving stock (Generic)

-- | Dynamic type. A Node with a dynamic type has an unknown type. Useful
-- for transformations that introduce partial type information, e.g., one can
-- have types `* -> *` and `* -> * -> Nat` where `*` is the dynamic type.
newtype DynamicTy' i = DynamicTy
  { _dynamicTyInfo :: i
  }
  deriving stock (Generic)

-- | A fail node.
data Bottom' i a = Bottom
  { _bottomInfo :: i,
    _bottomType :: !a
  }
  deriving stock (Generic)

{-------------------------------------------------------------------}
{- Typeclass instances -}

instance (Serialize i) => Serialize (Var' i)

instance (NFData i) => NFData (Var' i)

instance (Serialize i) => Serialize (Ident' i)

instance (NFData i) => NFData (Ident' i)

instance Serialize ConstantValue

instance NFData ConstantValue

instance (Serialize i) => Serialize (Constant' i)

instance (NFData i) => NFData (Constant' i)

instance (Serialize i, Serialize a) => Serialize (App' i a)

instance (NFData i, NFData a) => NFData (App' i a)

instance (Serialize i, Serialize f, Serialize a) => Serialize (Apps' i f a)

instance (NFData i, NFData f, NFData a) => NFData (Apps' i f a)

instance (Serialize i, Serialize a) => Serialize (BuiltinApp' i a)

instance (NFData i, NFData a) => NFData (BuiltinApp' i a)

instance (Serialize i, Serialize a) => Serialize (Constr' i a)

instance (NFData i, NFData a) => NFData (Constr' i a)

instance (Serialize ty) => Serialize (Binder' ty)

instance (NFData ty) => NFData (Binder' ty)

instance (Serialize i, Serialize a, Serialize ty) => Serialize (Lambda' i a ty)

instance (NFData i, NFData a, NFData ty) => NFData (Lambda' i a ty)

instance (Serialize a, Serialize ty) => Serialize (LetItem' a ty)

instance (NFData a, NFData ty) => NFData (LetItem' a ty)

instance (Serialize i, Serialize a, Serialize ty) => Serialize (Let' i a ty)

instance (NFData i, NFData a, NFData ty) => NFData (Let' i a ty)

instance (Serialize i, Serialize a, Serialize ty) => Serialize (LetRec' i a ty)

instance (NFData i, NFData a, NFData ty) => NFData (LetRec' i a ty)

instance (Serialize bi, Serialize a, Serialize ty) => Serialize (CaseBranch' bi a ty)

instance (NFData bi, NFData a, NFData ty) => NFData (CaseBranch' bi a ty)

instance (Serialize i, Serialize bi, Serialize a, Serialize ty) => Serialize (Case' i bi a ty)

instance (NFData i, NFData bi, NFData a, NFData ty) => NFData (Case' i bi a ty)

instance (Serialize i, Serialize a) => Serialize (If' i a)

instance (NFData i, NFData a) => NFData (If' i a)

instance (Serialize i, Serialize a) => Serialize (Pi' i a)

instance (NFData i, NFData a) => NFData (Pi' i a)

instance (Serialize i) => Serialize (Univ' i)

instance (NFData i) => NFData (Univ' i)

instance (NFData i, NFData a) => NFData (PatternWildcard' i a)

instance (NFData i, NFData a) => NFData (PatternConstr' i a)

instance (NFData i, NFData a) => NFData (Pattern' i a)

instance (NFData i, NFData a) => NFData (MatchBranchRhs' i a)

instance (NFData i, NFData a) => NFData (SideIfBranch' i a)

instance (NFData i, NFData a) => NFData (MatchBranch' i a)

instance (NFData i, NFData a) => NFData (Match' i a)

instance (Serialize i) => Serialize (TypePrim' i)

instance (NFData i) => NFData (TypePrim' i)

instance (Serialize i, Serialize a) => Serialize (TypeConstr' i a)

instance (NFData i, NFData a) => NFData (TypeConstr' i a)

instance (Serialize i) => Serialize (DynamicTy' i)

instance (NFData i) => NFData (DynamicTy' i)

instance (Serialize i, Serialize a) => Serialize (Bottom' i a)

instance (NFData i, NFData a) => NFData (Bottom' i a)

instance HasAtomicity (Var' i) where
  atomicity _ = Atom

instance HasAtomicity (Ident' i) where
  atomicity _ = Atom

instance HasAtomicity (Constant' i) where
  atomicity _ = Atom

instance HasAtomicity (App' i a) where
  atomicity _ = Aggregate appFixity

instance HasAtomicity (Apps' f i a) where
  atomicity _ = Aggregate appFixity

instance HasAtomicity (BuiltinApp' i a) where
  atomicity BuiltinApp {..}
    | null _builtinAppArgs = Atom
    | otherwise = Aggregate lambdaFixity

instance HasAtomicity (Constr' i a) where
  atomicity Constr {..}
    | null _constrArgs = Atom
    | otherwise = Aggregate lambdaFixity

instance HasAtomicity (Lambda' i a ty) where
  atomicity _ = Aggregate lambdaFixity

instance HasAtomicity (Let' i a ty) where
  atomicity _ = Aggregate lambdaFixity

instance HasAtomicity (LetRec' i a ty) where
  atomicity _ = Aggregate lambdaFixity

instance HasAtomicity (Case' i bi a ty) where
  atomicity _ = Aggregate lambdaFixity

instance HasAtomicity (If' i a) where
  atomicity _ = Aggregate lambdaFixity

instance HasAtomicity (Match' i a) where
  atomicity _ = Aggregate lambdaFixity

instance HasAtomicity (PatternWildcard' i a) where
  atomicity _ = Atom

instance HasAtomicity (PatternConstr' i a) where
  atomicity PatternConstr {..}
    | null _patternConstrArgs = Atom
    | otherwise = Aggregate appFixity

instance HasAtomicity (Pattern' i a) where
  atomicity = \case
    PatWildcard x -> atomicity x
    PatConstr x -> atomicity x

instance HasAtomicity (Pi' i a) where
  atomicity _ = Aggregate funFixity

instance HasAtomicity (Univ' i) where
  atomicity _ = Atom

instance HasAtomicity (TypePrim' i) where
  atomicity _ = Atom

instance HasAtomicity (TypeConstr' i a) where
  atomicity _ = Aggregate lambdaFixity

instance HasAtomicity (DynamicTy' i) where
  atomicity _ = Atom

instance HasAtomicity (Bottom' i a) where
  atomicity _ = Atom

lambdaFixity :: Fixity
lambdaFixity =
  Fixity
    { _fixityPrecedence = PrecNat 0,
      _fixityArity = OpUnary AssocPostfix,
      _fixityId = Nothing
    }

makeLenses ''LambdaLhs'
makeLenses ''PiLhs'
makeLenses ''Binder'
makeLenses ''Var'
makeLenses ''Ident'
makeLenses ''Constant'
makeLenses ''App'
makeLenses ''BuiltinApp'
makeLenses ''Constr'
makeLenses ''Let'
makeLenses ''LetRec'
makeLenses ''Case'
makeLenses ''CaseBranch'
makeLenses ''Match'
makeLenses ''MatchBranch'
makeLenses ''MatchBranchRhs'
makeLenses ''PatternWildcard'
makeLenses ''PatternConstr'
makeLenses ''SideIfBranch'
makeLenses ''Pi'
makeLenses ''Lambda'
makeLenses ''Univ'
makeLenses ''TypeConstr'
makeLenses ''DynamicTy'
makeLenses ''LetItem'

instance (Eq ty) => Eq (Binder' ty) where
  (==) = eqOn (^. binderType)

instance Eq (Var' i) where
  (Var _ idx1) == (Var _ idx2) = idx1 == idx2

instance Ord (Var' i) where
  compare = compare `on` (^. varIndex)

instance Eq (Ident' i) where
  (Ident _ sym1) == (Ident _ sym2) = sym1 == sym2

instance Eq (Constant' i) where
  (Constant _ v1) == (Constant _ v2) = v1 == v2

instance (Eq a) => Eq (App' i a) where
  (App _ l1 r1) == (App _ l2 r2) = l1 == l2 && r1 == r2

instance (Eq f, Eq a) => Eq (Apps' i f a) where
  (Apps _ op1 args1) == (Apps _ op2 args2) = op1 == op2 && args1 == args2

instance (Eq a) => Eq (BuiltinApp' i a) where
  (BuiltinApp _ op1 args1) == (BuiltinApp _ op2 args2) = op1 == op2 && args1 == args2

instance (Eq a) => Eq (Constr' i a) where
  (Constr _ tag1 args1) == (Constr _ tag2 args2) = tag1 == tag2 && args1 == args2

instance (Eq a) => Eq (Case' i bi a ty) where
  (Case _ sym1 v1 bs1 def1) == (Case _ sym2 v2 bs2 def2) = sym1 == sym2 && v1 == v2 && bs1 == bs2 && def1 == def2

instance (Eq a) => Eq (If' i a) where
  (If _ v1 b1 c1) == (If _ v2 b2 c2) = v1 == v2 && b1 == b2 && c1 == c2

instance (Eq a) => Eq (CaseBranch' i a ty) where
  (==) =
    eqOn (^. caseBranchTag)
      ..&&.. eqOn (^. caseBranchBody)

instance (Eq a) => Eq (Match' i a) where
  (Match _ _ _ vs1 bs1) == (Match _ _ _ vs2 bs2) = vs1 == vs2 && bs1 == bs2

instance Eq (PatternWildcard' i a) where
  _ == _ = True

instance Eq (Univ' i) where
  (Univ _ l1) == (Univ _ l2) = l1 == l2

instance (Eq a) => Eq (TypeConstr' i a) where
  (TypeConstr _ sym1 args1) == (TypeConstr _ sym2 args2) = sym1 == sym2 && args1 == args2

instance Eq (TypePrim' i) where
  (TypePrim _ p1) == (TypePrim _ p2) = p1 == p2

instance Eq (DynamicTy' i) where
  DynamicTy {} == DynamicTy {} = True

instance Eq (Bottom' i a) where
  Bottom {} == Bottom {} = True

deriving stock instance (Eq a) => Eq (Pattern' i a)

instance (Eq a, Eq ty) => Eq (LetItem' a ty) where
  (==) = eqOn (^. letItemValue) ..&&.. eqOn (^. letItemBinder)

instance (Eq a, Eq ty) => Eq (Lambda' i a ty) where
  (Lambda _ bd1 body1) == (Lambda _ bd2 body2) = bd1 == bd2 && body1 == body2

instance (Eq a, Eq ty) => Eq (Let' i a ty) where
  (==) =
    eqOn (^. letItem)
      ..&&.. eqOn (^. letBody)

instance (Eq a, Eq ty) => Eq (LetRec' i a ty) where
  (==) =
    eqOn (^. letRecBody)
      ..&&.. eqOn (^. letRecValues)

instance (Eq a) => Eq (Pi' i a) where
  (==) =
    eqOn (^. piBinder . binderType)
      ..&&.. eqOn (^. piBody)

instance (Eq a) => Eq (MatchBranchRhs' i a) where
  (MatchBranchRhsExpression e1) == (MatchBranchRhsExpression e2) = e1 == e2
  (MatchBranchRhsIfs ifs1) == (MatchBranchRhsIfs ifs2) = ifs1 == ifs2
  _ == _ = False

instance (Eq a) => Eq (MatchBranch' i a) where
  (MatchBranch _ pats1 b1) == (MatchBranch _ pats2 b2) = pats1 == pats2 && b1 == b2

instance (Eq a) => Eq (PatternConstr' i a) where
  (PatternConstr _ _ _ tag1 ps1) == (PatternConstr _ _ _ tag2 ps2) = tag1 == tag2 && ps1 == ps2

instance (Eq a) => Eq (SideIfBranch' i a) where
  (SideIfBranch _ c1 b1) == (SideIfBranch _ c2 b2) = c1 == c2 && b1 == b2

instance Hashable (Ident' i) where
  hashWithSalt s = hashWithSalt s . (^. identSymbol)

instance Hashable (Var' i) where
  hashWithSalt s = hashWithSalt s . (^. varIndex)
