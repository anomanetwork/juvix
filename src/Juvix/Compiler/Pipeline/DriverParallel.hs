module Juvix.Compiler.Pipeline.DriverParallel
  ( compileInParallel,
    ModuleInfoCache,
    evalModuleInfoCacheParallel,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Effectful.Concurrent
import Juvix.Compiler.Concrete.Data.Highlight.Builder (HighlightBuilder)
import Juvix.Compiler.Concrete.Language
import Juvix.Compiler.Concrete.Translation.FromSource.TopModuleNameChecker
import Juvix.Compiler.Concrete.Translation.ImportScanner (ImportScanStrategy)
import Juvix.Compiler.Pipeline
import Juvix.Compiler.Pipeline.Driver (processModule)
import Juvix.Compiler.Pipeline.Driver qualified as Driver
import Juvix.Compiler.Pipeline.JvoCache
import Juvix.Compiler.Pipeline.Loader.PathResolver
import Juvix.Compiler.Pipeline.ModuleInfoCache
import Juvix.Compiler.Store.Language qualified as Store
import Juvix.Prelude
import Parallel.ParallelTemplate
import Parallel.ProgressLog

data CompileResult = CompileResult
  { _compileResultModuleTable :: Store.ModuleTable,
    _compileResultChanged :: Bool
  }

makeLenses ''CompileResult

type Node = EntryIndex

mkNodesIndex ::
  forall r.
  (Members '[Files, PathResolver, Reader EntryPoint] r) =>
  ImportTree ->
  Sem r (NodesIndex ImportNode Node)
mkNodesIndex tree =
  NodesIndex
    . hashMap
    <$> sequence
      [ mkAssoc fromNode
        | fromNode <- HashMap.keys (tree ^. importTree)
      ]
  where
    mkAssoc :: ImportNode -> Sem r (ImportNode, EntryIndex)
    mkAssoc p = do
      i <- mkEntryIndex p
      return (p, i)

mkDependencies :: ImportTree -> Dependencies ImportNode
mkDependencies tree =
  Dependencies
    { _dependenciesTable = tree ^. importTree,
      _dependenciesTableReverse = tree ^. importTreeReverse
    }

getNodePath :: Node -> ImportNode
getNodePath = (^. entryIxImportNode)

getNodeName :: Node -> Text
getNodeName = toFilePath . (^. importNodeAbsFile) . getNodePath

-- | Compiles the whole project in parallel (i.e. all modules in the ImportTree).
compileInParallel ::
  forall r.
  ( Members
      '[ Concurrent,
         IOE,
         ModuleInfoCache,
         JvoCache,
         TaggedLock,
         Files,
         TopModuleNameChecker,
         Error JuvixError,
         Reader EntryPoint,
         PathResolver,
         Reader NumThreads,
         Reader ImportTree
       ]
      r
  ) =>
  Sem r (HashMap ImportNode (PipelineResult Store.ModuleInfo))
compileInParallel = do
  -- At the moment we compile everything, so the EntryIndex is ignored, but in
  -- principle we could only compile what is reachable from the given EntryIndex
  t <- ask
  idx <- mkNodesIndex t
  numWorkers <- ask >>= numThreads
  let args :: CompileArgs r ImportNode Node (PipelineResult Store.ModuleInfo)
      args =
        CompileArgs
          { _compileArgsNodesIndex = idx,
            _compileArgsNodeName = getNodeName,
            _compileArgsPreProcess = Just preLoadFromJvoFile,
            _compileArgsDependencies = mkDependencies t,
            _compileArgsNumWorkers = numWorkers,
            _compileArgsCompileNode = compileNode
          }
  compile args

compileNode ::
  (Members '[ModuleInfoCache, PathResolver] r) =>
  EntryIndex ->
  Sem r (PipelineResult Store.ModuleInfo)
compileNode e =
  withResolverRoot (e ^. entryIxImportNode . importNodePackageRoot)
    . fmap force
    $ processModule e

instance Semigroup CompileResult where
  sconcat l =
    CompileResult
      { _compileResultChanged = any (^. compileResultChanged) l,
        _compileResultModuleTable = sconcatMap (^. compileResultModuleTable) l
      }

instance Monoid CompileResult where
  mempty =
    CompileResult
      { _compileResultChanged = False,
        _compileResultModuleTable = mempty
      }

evalModuleInfoCacheParallel ::
  forall r a.
  ( Members
      '[ Reader EntryPoint,
         HighlightBuilder,
         IOE,
         Reader ImportTree,
         Concurrent,
         TaggedLock,
         TopModuleNameChecker,
         Error JuvixError,
         PathResolver,
         Reader ImportScanStrategy,
         Reader NumThreads,
         Reader PipelineOptions,
         Logger,
         Files
       ]
      r
  ) =>
  Sem (ModuleInfoCache ': ProgressLog ': JvoCache ': r) a ->
  Sem r a
evalModuleInfoCacheParallel = Driver.evalModuleInfoCacheSetup (const (void compileInParallel))
