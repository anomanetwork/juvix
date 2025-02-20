module Juvix.Compiler.Core.Data.Stripped.InfoTable where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Core.Language.Stripped

data InfoTable = InfoTable
  { _infoMain :: Maybe Symbol,
    _infoFunctions :: HashMap Symbol FunctionInfo,
    _infoInductives :: HashMap Symbol InductiveInfo,
    _infoConstructors :: HashMap Tag ConstructorInfo
  }
  deriving stock (Generic)

instance Serialize InfoTable

data FunctionInfo = FunctionInfo
  { _functionName :: Text,
    _functionLocation :: Maybe Location,
    _functionSymbol :: Symbol,
    -- _functionBody has `_functionArgsNum` free variables corresponding to the
    -- function arguments
    _functionBody :: Node,
    _functionType :: Type,
    -- a function can have 0 arguments
    _functionArgsNum :: Int,
    _functionArgsInfo :: [ArgumentInfo],
    _functionIsExported :: Bool
  }
  deriving stock (Generic)

instance Serialize FunctionInfo

data ArgumentInfo = ArgumentInfo
  { _argumentName :: Text,
    _argumentLocation :: Maybe Location,
    _argumentType :: Type
  }
  deriving stock (Generic)

instance Serialize ArgumentInfo

data InductiveInfo = InductiveInfo
  { _inductiveName :: Text,
    _inductiveLocation :: Maybe Location,
    _inductiveSymbol :: Symbol,
    _inductiveKind :: Type,
    _inductiveConstructors :: [Tag],
    _inductiveParams :: [ParameterInfo]
  }
  deriving stock (Generic)

instance Serialize InductiveInfo

data ConstructorInfo = ConstructorInfo
  { _constructorName :: Text,
    _constructorLocation :: Maybe Location,
    _constructorInductive :: Symbol,
    _constructorTag :: Tag,
    _constructorType :: Type,
    _constructorArgNames :: [Maybe Text],
    -- | _constructorArgsNum == length _constructorArgNames == length (typeArgs _constructorType)
    _constructorArgsNum :: Int,
    _constructorFixity :: Maybe Fixity
  }
  deriving stock (Generic)

instance Serialize ConstructorInfo

data ParameterInfo = ParameterInfo
  { _paramName :: Text,
    _paramLocation :: Maybe Location,
    _paramKind :: Type,
    _paramIsImplicit :: Bool
  }
  deriving stock (Generic)

instance Serialize ParameterInfo

makeLenses ''InfoTable
makeLenses ''FunctionInfo
makeLenses ''ArgumentInfo
makeLenses ''InductiveInfo
makeLenses ''ConstructorInfo
makeLenses ''ParameterInfo

instance Semigroup InfoTable where
  tab1 <> tab2 =
    InfoTable
      { _infoMain = tab1 ^. infoMain <|> tab2 ^. infoMain,
        _infoFunctions = tab1 ^. infoFunctions <> tab2 ^. infoFunctions,
        _infoInductives = tab1 ^. infoInductives <> tab2 ^. infoInductives,
        _infoConstructors = tab1 ^. infoConstructors <> tab2 ^. infoConstructors
      }

instance Monoid InfoTable where
  mempty =
    InfoTable
      { _infoMain = Nothing,
        _infoFunctions = mempty,
        _infoInductives = mempty,
        _infoConstructors = mempty
      }

lookupTabConstructorInfo' :: InfoTable -> Tag -> Maybe ConstructorInfo
lookupTabConstructorInfo' tab tag = HashMap.lookup tag (tab ^. infoConstructors)

lookupTabFunInfo' :: InfoTable -> Symbol -> Maybe FunctionInfo
lookupTabFunInfo' tab sym = HashMap.lookup sym (tab ^. infoFunctions)

lookupTabInductiveInfo' :: InfoTable -> Symbol -> Maybe InductiveInfo
lookupTabInductiveInfo' tab sym = HashMap.lookup sym (tab ^. infoInductives)

lookupTabConstructorInfo :: InfoTable -> Tag -> ConstructorInfo
lookupTabConstructorInfo tab tag = fromJust $ HashMap.lookup tag (tab ^. infoConstructors)
