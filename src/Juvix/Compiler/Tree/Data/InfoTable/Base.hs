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

data InductiveInfo = InductiveInfo
  { _inductiveName :: Text,
    _inductiveLocation :: Maybe Location,
    _inductiveSymbol :: Symbol,
    _inductiveKind :: Type,
    _inductiveConstructors :: [Tag],
    _inductiveRepresentation :: IndRep
  }

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

lookupFunInfo :: InfoTable' a e -> Symbol -> FunctionInfo' a e
lookupFunInfo infoTable sym = fromMaybe (error "invalid function symbol") (HashMap.lookup sym (infoTable ^. infoFunctions))

lookupConstrInfo :: InfoTable' a e -> Tag -> ConstructorInfo
lookupConstrInfo infoTable tag = fromMaybe (error "invalid constructor tag") (HashMap.lookup tag (infoTable ^. infoConstrs))

lookupInductiveInfo :: InfoTable' a e -> Symbol -> InductiveInfo
lookupInductiveInfo infoTable sym = fromMaybe (error "invalid inductive symbol") (HashMap.lookup sym (infoTable ^. infoInductives))

getNextSymbolId :: InfoTable' a e -> Word
getNextSymbolId tab = maximum (0 : map (^. symbolId) (HashMap.keys (tab ^. infoFunctions) ++ HashMap.keys (tab ^. infoInductives))) + 1

getNextUserTag :: InfoTable' a e -> Word
getNextUserTag tab = maximum (0 : mapMaybe getUserTagId (HashMap.keys (tab ^. infoConstrs))) + 1
