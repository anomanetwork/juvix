module Juvix.Compiler.Tree.Language.Type where

import Juvix.Compiler.Core.Language.Base

data Type
  = TyDynamic
  | TyInteger TypeInteger
  | TyBool TypeBool
  | TyString
  | TyField
  | TyByteArray
  | TyRandomGenerator
  | TyUnit
  | TyVoid
  | TyInductive TypeInductive
  | -- | TyConstr represents the type of a specific constructor. It is a subtype
    -- of an appropriate TyInductive.
    TyConstr TypeConstr
  | TyFun TypeFun
  deriving stock (Eq, Generic)

data TypeInteger = TypeInteger
  { _typeIntegerMinValue :: Maybe Integer,
    _typeIntegerMaxValue :: Maybe Integer
  }
  deriving stock (Eq, Generic)

data TypeBool = TypeBool
  { _typeBoolTrueTag :: Tag,
    _typeBoolFalseTag :: Tag
  }
  deriving stock (Generic)

instance Eq TypeBool where
  _ == _ = True

newtype TypeInductive = TypeInductive
  { _typeInductiveSymbol :: Symbol
  }
  deriving stock (Eq, Generic)

data TypeConstr = TypeConstr
  { _typeConstrInductive :: Symbol,
    _typeConstrTag :: Tag,
    _typeConstrFields :: [Type]
  }
  deriving stock (Generic)

instance Eq TypeConstr where
  (TypeConstr _ tag1 _) == (TypeConstr _ tag2 _) = tag1 == tag2

data TypeFun = TypeFun
  { _typeFunArgs :: NonEmpty Type,
    _typeFunTarget :: Type
  }
  deriving stock (Eq, Generic)

instance Serialize TypeInteger

instance Serialize TypeBool

instance Serialize TypeInductive

instance Serialize TypeConstr

instance Serialize TypeFun

instance Serialize Type

makeLenses ''TypeInteger
makeLenses ''TypeBool
makeLenses ''TypeInductive
makeLenses ''TypeConstr
makeLenses ''TypeFun

instance HasAtomicity TypeInteger where
  atomicity _ = Atom

instance HasAtomicity TypeBool where
  atomicity _ = Atom

instance HasAtomicity TypeInductive where
  atomicity _ = Atom

instance HasAtomicity TypeConstr where
  atomicity _ = Atom

instance HasAtomicity TypeFun where
  atomicity _ = Aggregate funFixity

instance HasAtomicity Type where
  atomicity = \case
    TyDynamic -> Atom
    TyInteger x -> atomicity x
    TyBool x -> atomicity x
    TyString -> Atom
    TyField -> Atom
    TyUnit -> Atom
    TyVoid -> Atom
    TyByteArray -> Atom
    TyRandomGenerator -> Atom
    TyInductive x -> atomicity x
    TyConstr x -> atomicity x
    TyFun x -> atomicity x

typeArgs :: Type -> [Type]
typeArgs = \case
  TyFun x -> toList (x ^. typeFunArgs)
  _ -> []

typeTarget :: Type -> Type
typeTarget ty = case ty of
  TyFun x -> x ^. typeFunTarget
  _ -> ty
