module Juvix.Compiler.Pipeline.Modular where

import Data.List.Singletons (type (++))
import Juvix.Compiler.Backend
import Juvix.Compiler.Core.Data.Module qualified as Core
import Juvix.Compiler.Core.Data.Module.Base
import Juvix.Compiler.Core.Data.Stripped.Module qualified as Stripped
import Juvix.Compiler.Core.Data.TransformationId qualified as Core
import Juvix.Compiler.Pipeline qualified as Pipeline
import Juvix.Compiler.Pipeline.EntryPoint
import Juvix.Compiler.Pipeline.Modular.Result
import Juvix.Compiler.Store.Backend.Module qualified as Stored
import Juvix.Compiler.Tree.Pipeline qualified as Tree
import Juvix.Extra.Serialize qualified as Serialize
import Juvix.Prelude
import Juvix.Prelude.Pretty
import Path qualified

type ModularEff r =
  '[ Files,
     TaggedLock,
     Reader EntryPoint,
     Error JuvixError
   ]
    ++ r

type ModuleCache m = Cache ModuleId (PipelineResult m)

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
  Target ->
  ModuleTable' t ->
  (Module' t -> Sem r (Module' t')) ->
  ModuleId ->
  Sem r (PipelineResult (Module' t'))
processModuleCacheMiss midTarget mt f mid = do
  entry <- ask
  let root = entry ^. entryPointRoot
      opts = Stored.fromEntryPoint entry
      buildDir = resolveAbsBuildDir root (entry ^. entryPointBuildDir)
      relPath =
        relFile
          ( sanitizeFilename (unpack $ prettyText mid)
              <> getTargetExtension midTarget
          )
      subdir = Stored.getOptionsSubdir midTarget opts
      absPath = buildDir Path.</> subdir Path.</> relPath
      md0 = lookupModuleTable mt mid
      sha256 = fromJust (md0 ^. moduleSHA256)
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
                  && md ^. Stored.moduleOptions == opts
                  && md ^. Stored.moduleId == mid -> do
                  return
                    PipelineResult
                      { _pipelineResult = Stored.toBaseModule imports md,
                        _pipelineResultChanged = False
                      }
              | otherwise -> recompile opts absPath imports md0
            _ ->
              recompile opts absPath imports md0
  where
    recompile :: Stored.Options -> Path Abs File -> [Module' t'] -> Module' t -> Sem r (PipelineResult (Module' t'))
    recompile opts absPath imports md0 = do
      md :: Module' t' <- f md0
      massert (md ^. moduleId == mid)
      massert (md ^. moduleSHA256 == md0 ^. moduleSHA256)
      let md' = md {_moduleImportsTable = mconcatMap computeCombinedInfoTable imports}
      Serialize.saveToFile absPath (Stored.fromBaseModule opts md')
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

processModuleTable ::
  forall t t' r.
  (Serialize t', Monoid t', Members '[Files, TaggedLock, Error JuvixError, Reader EntryPoint] r) =>
  Target ->
  (Module' t -> Sem r (Module' t')) ->
  ModuleTable' t ->
  Sem r (ModuleTable' t')
processModuleTable midTarget f mt = do
  tab <-
    evalCacheEmpty
      (processModuleCacheMiss midTarget mt (inject . f))
      $ mapM (fmap (^. pipelineResult) . processModule . (^. moduleId)) (mt ^. moduleTable)
  return $ ModuleTable tab

modularCoreToStripped ::
  (Members '[Files, TaggedLock, Error JuvixError, Reader EntryPoint] r) =>
  Core.TransformationId ->
  Core.ModuleTable ->
  Sem r Stripped.ModuleTable
modularCoreToStripped checkId mt =
  processModuleTable TargetStripped (Pipeline.storedCoreToStripped checkId) mt

modularCoreToTree ::
  (Members '[Files, TaggedLock, Error JuvixError, Reader EntryPoint] r) =>
  Core.TransformationId ->
  Core.ModuleTable ->
  Sem r Tree.ModuleTable
modularCoreToTree checkId =
  modularCoreToStripped checkId >=> modularStrippedToTree

modularStrippedToTree ::
  (Members '[Files, TaggedLock, Error JuvixError, Reader EntryPoint] r) =>
  Stripped.ModuleTable ->
  Sem r Tree.ModuleTable
modularStrippedToTree mt =
  processModuleTable TargetTree Pipeline.strippedCoreToTree mt
