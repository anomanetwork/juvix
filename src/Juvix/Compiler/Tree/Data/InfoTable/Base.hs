module Juvix.Compiler.Tree.Data.InfoTable.Base
  ( module Juvix.Compiler.Tree.Data.InfoTable.Base,
    module Juvix.Compiler.Tree.Language.Rep,
    module Juvix.Compiler.Tree.Language.Type,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Tree.Language
import Juvix.Compiler.Tree.Language.Rep
import Juvix.Compiler.Tree.Language.Type

data InfoTable' code extra = InfoTable
  { _infoFunctions :: HashMap Symbol (FunctionInfo' code extra),
    _infoConstrs :: HashMap Tag ConstructorInfo,
    _infoInductives :: HashMap Symbol InductiveInfo,
    _infoMainFunction :: Maybe Symbol
  }
  deriving stock (Generic)

data FunctionInfo' code extra = FunctionInfo
  { _functionName :: Text,
    _functionLocation :: Maybe Location,
    _functionSymbol :: Symbol,
    -- | `_functionArgsNum` may be different from `length (typeArgs
    -- (_functionType))` only if it is 0 (the "function" takes zero arguments)
    -- and the result is a function.
    _functionArgsNum :: Int,
    -- | length _functionArgNames == _functionArgsNum
    _functionArgNames :: [Maybe Text],
    _functionType :: Type,
    _functionExtra :: extra,
    _functionCode :: code
  }
  deriving stock (Generic)

data ConstructorInfo = ConstructorInfo
  { _constructorName :: Text,
    _constructorLocation :: Maybe Location,
    _constructorTag :: Tag,
    -- | `_constructorArgsNum` should always be equal to `length (typeArgs
    -- (_constructorType))`. It is stored separately mainly for the benefit of
    -- the interpreter (so it does not have to recompute it every time).
    _constructorArgsNum :: Int,
    -- | length _constructorArgNames == _constructorArgsNum
    _constructorArgNames :: [Maybe Text],
    -- | Constructor types are assumed to be fully uncurried, i.e., `uncurryType
    -- _constructorType == _constructorType`
    _constructorType :: Type,
    _constructorInductive :: Symbol,
    _constructorRepresentation :: MemRep,
    _constructorFixity :: Maybe Fixity
  }
  deriving stock (Generic)

data InductiveInfo = InductiveInfo
  { _inductiveName :: Text,
    _inductiveLocation :: Maybe Location,
    _inductiveSymbol :: Symbol,
    _inductiveKind :: Type,
    _inductiveConstructors :: [Tag],
    _inductiveRepresentation :: IndRep
  }
  deriving stock (Generic)

instance (Serialize code, Serialize extra) => Serialize (FunctionInfo' code extra)

instance Serialize ConstructorInfo

instance Serialize InductiveInfo

instance (Serialize code, Serialize extra) => Serialize (InfoTable' code extra)

makeLenses ''InfoTable'
makeLenses ''FunctionInfo'
makeLenses ''ConstructorInfo
makeLenses ''InductiveInfo

emptyInfoTable :: InfoTable' a e
emptyInfoTable =
  InfoTable
    { _infoFunctions = mempty,
      _infoConstrs = mempty,
      _infoInductives = mempty,
      _infoMainFunction = Nothing
    }

lookupTabFunInfo' :: InfoTable' a e -> Symbol -> Maybe (FunctionInfo' a e)
lookupTabFunInfo' infoTable sym = HashMap.lookup sym (infoTable ^. infoFunctions)

lookupTabConstrInfo' :: InfoTable' a e -> Tag -> Maybe ConstructorInfo
lookupTabConstrInfo' infoTable tag = HashMap.lookup tag (infoTable ^. infoConstrs)

lookupTabInductiveInfo' :: InfoTable' a e -> Symbol -> Maybe InductiveInfo
lookupTabInductiveInfo' infoTable sym = HashMap.lookup sym (infoTable ^. infoInductives)

lookupTabFunInfo :: InfoTable' a e -> Symbol -> FunctionInfo' a e
lookupTabFunInfo infoTable sym = fromMaybe (error "invalid function symbol") (lookupTabFunInfo' infoTable sym)

lookupTabConstrInfo :: InfoTable' a e -> Tag -> ConstructorInfo
lookupTabConstrInfo infoTable tag = fromMaybe (error "invalid constructor tag") (lookupTabConstrInfo' infoTable tag)

lookupTabInductiveInfo :: InfoTable' a e -> Symbol -> InductiveInfo
lookupTabInductiveInfo infoTable sym = fromMaybe (error "invalid inductive symbol") (lookupTabInductiveInfo' infoTable sym)

nextSymbolId :: InfoTable' a e -> Word
nextSymbolId tab = maximum (0 : map (^. symbolId) (HashMap.keys (tab ^. infoFunctions) ++ HashMap.keys (tab ^. infoInductives))) + 1

nextUserTag :: InfoTable' a e -> Word
nextUserTag tab = maximum (0 : mapMaybe getUserTagId (HashMap.keys (tab ^. infoConstrs))) + 1

instance Semigroup (InfoTable' code extra) where
  t1 <> t2 =
    InfoTable
      { _infoFunctions = t1 ^. infoFunctions <> t2 ^. infoFunctions,
        _infoInductives = t1 ^. infoInductives <> t2 ^. infoInductives,
        _infoConstrs = t1 ^. infoConstrs <> t2 ^. infoConstrs,
        _infoMainFunction = t2 ^. infoMainFunction <|> t1 ^. infoMainFunction
      }

instance Monoid (InfoTable' code extra) where
  mempty =
    InfoTable
      { _infoFunctions = mempty,
        _infoInductives = mempty,
        _infoConstrs = mempty,
        _infoMainFunction = Nothing
      }
