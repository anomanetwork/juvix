module Juvix.Compiler.Pipeline.EntryPoint
  ( module Juvix.Compiler.Pipeline.EntryPoint,
    module Juvix.Compiler.Pipeline.Package.Base,
  )
where

import Juvix.Compiler.Backend
import Juvix.Compiler.Pipeline.Package.Base
import Juvix.Compiler.Pipeline.Root.Base
import Juvix.Data.Field
import Juvix.Prelude

-- | An option specifying how symbols should be pruned in the Internal to Core translation
data SymbolPruningMode
  = FilterUnreachable
  | KeepAll
  deriving stock (Eq, Show)

data Pipeline
  = PipelineEval
  | PipelineExec
  | PipelineTypecheck
  deriving stock (Eq, Show, Generic)

instance Serialize Pipeline

instance NFData Pipeline

-- | A module in _entryModulePath is the unit of compilation
data EntryPoint = EntryPoint
  { _entryPointRoot :: Path Abs Dir,
    -- | initial root for the path resolver. Usually it should be equal to
    -- _entryPointRoot. It only differs for `juvix repl`.
    _entryPointResolverRoot :: Path Abs Dir,
    _entryPointBuildDir :: BuildDir,
    _entryPointNoTermination :: Bool,
    _entryPointNoPositivity :: Bool,
    _entryPointNoCoverage :: Bool,
    _entryPointNoStdlib :: Bool,
    _entryPointSomeRoot :: SomeRoot,
    _entryPointPackageId :: PackageId,
    _entryPointStdin :: Maybe Text,
    _entryPointTarget :: Maybe Target,
    _entryPointDebug :: Bool,
    _entryPointUnsafe :: Bool,
    -- | Skip the correctness check in the Core pipeline part.
    _entryPointNoCheck :: Bool,
    _entryPointUnrollLimit :: Int,
    _entryPointOptimizationLevel :: Int,
    _entryPointInliningDepth :: Int,
    _entryPointGenericOptions :: GenericOptions,
    _entryPointModulePath :: Maybe (Path Abs File),
    _entryPointMainFile :: Maybe (Path Abs File),
    _entryPointSymbolPruningMode :: SymbolPruningMode,
    _entryPointOffline :: Bool,
    _entryPointFieldSize :: Natural,
    _entryPointIsabelleOnlyTypes :: Bool,
    _entryPointPipeline :: Maybe Pipeline,
    _entryPointSHA256 :: Maybe Text
  }
  deriving stock (Eq, Show)

makeLenses ''EntryPoint

entryPointPackageType :: Lens' EntryPoint PackageType
entryPointPackageType = entryPointSomeRoot . someRootType

defaultUnrollLimit :: Int
defaultUnrollLimit = 140

defaultOptimizationLevel :: Int
defaultOptimizationLevel = 1

defaultInliningDepth :: Int
defaultInliningDepth = 3

-- | Don't use this without updating the options
defaultEntryPoint :: PackageId -> Root -> Maybe (Path Abs File) -> EntryPoint
defaultEntryPoint pkg root mainFile =
  (defaultEntryPointNoFile pkg root)
    { _entryPointModulePath = mainFile,
      _entryPointMainFile = mainFile
    }

defaultEntryPointNoFile :: PackageId -> Root -> EntryPoint
defaultEntryPointNoFile pkg root =
  EntryPoint
    { _entryPointRoot = root ^. rootRootDir,
      _entryPointResolverRoot = root ^. rootRootDir,
      _entryPointBuildDir = DefaultBuildDir,
      _entryPointNoTermination = False,
      _entryPointNoPositivity = False,
      _entryPointNoCoverage = False,
      _entryPointNoStdlib = False,
      _entryPointStdin = Nothing,
      _entryPointSomeRoot = root ^. rootSomeRoot,
      _entryPointPackageId = pkg,
      _entryPointGenericOptions = defaultGenericOptions,
      _entryPointTarget = Nothing,
      _entryPointDebug = False,
      _entryPointUnsafe = False,
      _entryPointNoCheck = False,
      _entryPointUnrollLimit = defaultUnrollLimit,
      _entryPointOptimizationLevel = defaultOptimizationLevel,
      _entryPointInliningDepth = defaultInliningDepth,
      _entryPointModulePath = Nothing,
      _entryPointMainFile = Nothing,
      _entryPointSymbolPruningMode = FilterUnreachable,
      _entryPointOffline = False,
      _entryPointFieldSize = defaultFieldSize,
      _entryPointIsabelleOnlyTypes = False,
      _entryPointPipeline = Nothing,
      _entryPointSHA256 = Nothing
    }
