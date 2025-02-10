module Juvix.Compiler.Core.Data.Stripped.InfoTable where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Core.Language.Stripped

data InfoTable = InfoTable
  { _infoMain :: Maybe Symbol,
    _infoFunctions :: HashMap Symbol FunctionInfo,
    _infoInductives :: HashMap Symbol InductiveInfo,
    _infoConstructors :: HashMap Tag ConstructorInfo,
    -- Used only by the JuvixTree evaluator
    _infoFieldSize :: Natural
  }

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

data ArgumentInfo = ArgumentInfo
  { _argumentName :: Text,
    _argumentLocation :: Maybe Location,
    _argumentType :: Type
  }

data InductiveInfo = InductiveInfo
  { _inductiveName :: Text,
    _inductiveLocation :: Maybe Location,
    _inductiveSymbol :: Symbol,
    _inductiveKind :: Type,
    _inductiveConstructors :: [Tag],
    _inductiveParams :: [ParameterInfo]
  }

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

data ParameterInfo = ParameterInfo
  { _paramName :: Text,
    _paramLocation :: Maybe Location,
    _paramKind :: Type,
    _paramIsImplicit :: Bool
  }

makeLenses ''InfoTable
makeLenses ''FunctionInfo
makeLenses ''ArgumentInfo
makeLenses ''InductiveInfo
makeLenses ''ConstructorInfo
makeLenses ''ParameterInfo

lookupConstructorInfo :: InfoTable -> Tag -> ConstructorInfo
lookupConstructorInfo tab tag = fromJust $ HashMap.lookup tag (tab ^. infoConstructors)
