module Juvix.Compiler.Core.Language.Stripped.Type where

import Juvix.Compiler.Core.Language.Base
import Juvix.Compiler.Core.Language.Primitives

data Type
  = TyDynamic
  | TyPrim Primitive
  | TyApp TypeApp
  | TyFun TypeFun
  deriving stock (Eq, Generic)

instance Serialize Type

data TypeApp = TypeApp
  { _typeAppName :: Text,
    _typeAppLocation :: Maybe Location,
    _typeAppSymbol :: Symbol,
    _typeAppArgs :: [Type]
  }
  deriving stock (Eq, Generic)

instance Serialize TypeApp

data TypeFun = TypeFun
  { _typeFunLeft :: Type,
    _typeFunRight :: Type
  }
  deriving stock (Eq, Generic)

instance Serialize TypeFun

makeLenses ''TypeApp
makeLenses ''TypeFun

instance HasAtomicity TypeApp where
  atomicity TypeApp {..}
    | null _typeAppArgs = Atom
    | otherwise = Aggregate appFixity

instance HasAtomicity TypeFun where
  atomicity _ = Aggregate funFixity

instance HasAtomicity Type where
  atomicity = \case
    TyDynamic -> Atom
    TyPrim {} -> Atom
    TyApp x -> atomicity x
    TyFun x -> atomicity x

unfoldType :: Type -> (Type, [Type])
unfoldType = \case
  TyFun (TypeFun l r) ->
    let (tgt, args) = unfoldType r
     in (tgt, l : args)
  ty -> (ty, [])

-- argument types
typeArgs :: Type -> [Type]
typeArgs = snd . unfoldType

-- target type
typeTarget :: Type -> Type
typeTarget = fst . unfoldType

typeArgsNum :: Type -> Int
typeArgsNum = length . typeArgs

mkFunType :: [Type] -> Type -> Type
mkFunType tyargs target = case tyargs of
  [] -> target
  ty : tyargs' -> TyFun (TypeFun ty (mkFunType tyargs' target))
