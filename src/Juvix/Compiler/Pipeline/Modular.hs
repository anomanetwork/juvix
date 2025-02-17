module Juvix.Compiler.Pipeline.Modular where

import Juvix.Compiler.Core.Data.Module qualified as Core
import Juvix.Compiler.Core.Data.Module.Base
import Juvix.Compiler.Core.Data.TransformationId qualified as Core
import Juvix.Compiler.Pipeline qualified as Pipeline
import Juvix.Compiler.Pipeline.EntryPoint
import Juvix.Compiler.Pipeline.Modular.Cache
import Juvix.Compiler.Pipeline.Modular.Result
import Juvix.Compiler.Store.Backend.Module qualified as Stored
import Juvix.Compiler.Tree.Pipeline qualified as Tree
import Juvix.Extra.Serialize qualified as Serialize
import Juvix.Prelude
import Path qualified

processModule ::
  (Members '[Files, Error JuvixError, Reader EntryPoint, ModuleCache (Module' t)] r) =>
  ModuleId ->
  Sem r (PipelineResult (Module' t))
processModule = cacheGet

processModuleCacheMiss ::
  forall t t' r.
  ( Monoid t',
    Serialize t',
    Members '[Files, TaggedLock, Error JuvixError, Reader EntryPoint, ModuleCache (Module' t')] r
  ) =>
  ModuleTable' t ->
  (Module' t -> Sem r (Module' t')) ->
  ModuleId ->
  Sem r (PipelineResult (Module' t'))
processModuleCacheMiss mt f mid = do
  entry <- ask
  let root = entry ^. entryPointRoot
      opts = Stored.fromEntryPoint entry
      buildDir = resolveAbsBuildDir root (entry ^. entryPointBuildDir)
      sourcePath = fromJust (entry ^. entryPointModulePath)
      relPath =
        fromJust
          . replaceExtension (Stored.getOptionsExtension opts)
          . fromJust
          $ stripProperPrefix $(mkAbsDir "/") sourcePath
      subdir = Stored.getOptionsSubdir opts
      absPath = buildDir Path.</> subdir Path.</> relPath
      md0 = lookupModuleTable mt mid
      sha256 = md0 ^. moduleSHA256
  res <- processImports (md0 ^. moduleImports)
  let changed = res ^. pipelineResultChanged
      imports = res ^. pipelineResult
  if
      | changed ->
          recompile opts absPath imports md0
      | otherwise -> do
          mmd :: Maybe (Stored.Module' t') <- Serialize.loadFromFile absPath
          case mmd of
            Just md
              | md ^. Stored.moduleSHA256 == sha256
                  && md ^. Stored.moduleOptions == opts -> do
                  return
                    PipelineResult
                      { _pipelineResult = Stored.toCoreModule imports md,
                        _pipelineResultChanged = False
                      }
            _ ->
              recompile opts absPath imports md0
  where
    recompile :: Stored.Options -> Path Abs File -> [Module' t'] -> Module' t -> Sem r (PipelineResult (Module' t'))
    recompile opts absPath imports md0 = do
      md :: Module' t' <- f md0
      let md' = md {_moduleImportsTable = mconcatMap computeCombinedInfoTable imports}
      Serialize.saveToFile absPath (Stored.fromCoreModule opts md')
      return
        PipelineResult
          { _pipelineResult = md',
            _pipelineResultChanged = True
          }

processImports ::
  (Members '[Files, Error JuvixError, Reader EntryPoint, ModuleCache (Module' t)] r) =>
  [ModuleId] ->
  Sem r (PipelineResult [Module' t])
processImports mids = do
  res <- mapM processModule mids
  return
    PipelineResult
      { _pipelineResult = map (^. pipelineResult) res,
        _pipelineResultChanged = any (^. pipelineResultChanged) res
      }

processCoreToTree ::
  (Members '[Files, TaggedLock, Error JuvixError, Reader EntryPoint, ModuleCache Tree.Module] r) =>
  Core.TransformationId ->
  Core.ModuleTable ->
  ModuleId ->
  Sem r (PipelineResult Tree.Module)
processCoreToTree checkId mt mid = do
  processModuleCacheMiss mt (Pipeline.coreToTree checkId) mid

runModularPipeline ::
  forall t t' r.
  (Serialize t', Monoid t', Members '[Files, TaggedLock, Error JuvixError, Reader EntryPoint] r) =>
  (forall r'. (Members '[Error JuvixError, Reader EntryPoint] r') => Module' t -> Sem r' (Module' t')) ->
  ModuleTable' t ->
  Sem r (ModuleTable' t')
runModularPipeline f mt = do
  tab <-
    evalCacheEmpty
      (processModuleCacheMiss mt f)
      $ mapM (fmap (^. pipelineResult) . processModule . (^. moduleId)) (mt ^. moduleTable)
  return $ ModuleTable tab

runModularCoreToTree ::
  (Members '[Files, TaggedLock, Error JuvixError, Reader EntryPoint] r) =>
  Core.TransformationId ->
  Core.ModuleTable ->
  Sem r Tree.ModuleTable
runModularCoreToTree checkId mt =
  runModularPipeline (Pipeline.coreToTree checkId) mt
