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
import Juvix.Data.SHA256 qualified as SHA256
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
  (Serialize t', Members '[Files, TaggedLock, Error JuvixError, Reader EntryPoint, ModuleCache (Module' t')] r) =>
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
  mmd :: Maybe (Stored.Module' t') <- Serialize.loadFromFile absPath
  case mmd of
    Just md
      | md ^. Stored.moduleSHA256 == sha256
          && md ^. Stored.moduleOptions == opts -> do
          pure (PipelineResult (toModule md) False)
    _ -> do
      md :: Module' t' <- f md0
      let md' = md & moduleSHA256 .~ sha256
      Serialize.saveToFile absPath md
      cachePut mid (PipelineResult md)
      pure (PipelineResult md)

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
  (Monoid t', Members '[Files, TaggedLock, Error JuvixError, Reader EntryPoint] r) =>
  (Module' t -> Sem r (PipelineResult (Module' t'))) ->
  ModuleTable' t ->
  Sem r (ModuleTable' t')
runModularPipeline f mt = do
  undefined
