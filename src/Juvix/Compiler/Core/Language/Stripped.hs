module Juvix.Compiler.Core.Language.Stripped
  ( module Juvix.Compiler.Core.Language.Base,
    module Juvix.Compiler.Core.Language.Nodes,
    module Juvix.Compiler.Core.Language.Stripped.Type,
    module Juvix.Compiler.Core.Language.Stripped,
  )
where

{- Stripped program tree datatype -}

import Juvix.Compiler.Core.Language.Base
import Juvix.Compiler.Core.Language.Nodes
import Juvix.Compiler.Core.Language.Stripped.Type

{---------------------------------------------------------------------------------}

data VarInfo = VarInfo
  { _varInfoName :: Maybe Name,
    _varInfoType :: Type -- TyDynamic if not available
  }

data IdentInfo = IdentInfo
  { _identInfoName :: Maybe Name,
    _identInfoType :: Type
  }

data ConstrInfo = ConstrInfo
  { _constrInfoName :: Maybe Name,
    _constrInfoType :: Type
  }

data LetInfo = LetInfo
  { _letInfoBinderName :: Maybe Name,
    _letInfoBinderType :: Type
  }

data CaseBranchInfo = CaseBranchInfo
  { _caseBranchInfoBinderNames :: [Maybe Name],
    _caseBranchInfoBinderTypes :: [Type],
    _caseBranchInfoConstrName :: Maybe Name,
    _caseBranchInfoConstrType :: Type
  }

{---------------------------------------------------------------------------------}

type instance FVar 'Stripped = Var' VarInfo

type instance FIdent 'Stripped = Ident' IdentInfo

type instance FConstant 'Stripped = Constant' ()

type instance FApps 'Stripped = Apps' Fun () Node

data Fun = FunVar Var | FunIdent Ident
  deriving stock (Eq)

type instance FBuiltinApp 'Stripped = BuiltinApp' () Node

type instance FConstr 'Stripped = Constr' ConstrInfo Node

type instance FLet 'Stripped = Let' LetInfo Node

type instance FCase 'Stripped = Case' () CaseBranchInfo Node

type instance FNode 'Stripped = Node

{---------------------------------------------------------------------------------}

type Var = FVar 'Stripped

type Ident = FIdent 'Stripped

type Constant = FConstant 'Stripped

type Apps = FApps 'Stripped

type BuiltinApp = FBuiltinApp 'Stripped

type Constr = FConstr 'Stripped

type Let = FLet 'Stripped

type Case = FCase 'Stripped

type CaseBranch = CaseBranch' CaseBranchInfo Node

{---------------------------------------------------------------------------------}

data Node
  = NVar Var
  | NIdt Ident
  | NCst Constant
  | NApp Apps
  | NBlt BuiltinApp
  | NCtr Constr
  | NLet Let
  | NCase Case
  deriving stock (Eq)

instance HasAtomicity Node where
  atomicity = \case
    NVar x -> atomicity x
    NIdt x -> atomicity x
    NCst x -> atomicity x
    NApp x -> atomicity x
    NBlt x -> atomicity x
    NCtr x -> atomicity x
    NLet x -> atomicity x
    NCase x -> atomicity x

makeLenses ''VarInfo
makeLenses ''IdentInfo
makeLenses ''ConstrInfo
makeLenses ''LetInfo
makeLenses ''CaseBranchInfo
