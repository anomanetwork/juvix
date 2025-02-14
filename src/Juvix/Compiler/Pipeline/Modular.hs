module Juvix.Compiler.Pipeline.Modular where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Core.Data.Module qualified as Core
import Juvix.Compiler.Core.Data.Module.Base (Module', ModuleTable')
import Juvix.Compiler.Core.Data.TransformationId qualified as Core
import Juvix.Compiler.Pipeline qualified as Pipeline
import Juvix.Compiler.Pipeline.EntryPoint
import Juvix.Compiler.Pipeline.Modular.Cache
import Juvix.Compiler.Pipeline.Modular.Result
import Juvix.Compiler.Tree.Pipeline qualified as Tree
import Juvix.Prelude

processModule ::
  (Members '[Files, Error JuvixError, Reader EntryPoint, ModuleCache t] r) =>
  ModuleId ->
  Sem r (PipelineResult (Module' t))
processModule = cacheGet

processModuleCacheMiss ::
  forall t r.
  (Members '[Files, Error JuvixError, Reader EntryPoint, ModuleCache t] r) =>
  (ModuleId -> Sem r (Module' t)) ->
  ModuleId ->
  Sem r (PipelineResult (Module' t))
processModuleCacheMiss f mid = do
  undefined

processModuleTable ::
  forall t t' r.
  (Monoid t', Members '[Files, Error JuvixError, Reader EntryPoint] r) =>
  (Module' t -> Sem r (PipelineResult (Module' t))) ->
  ModuleTable' t ->
  Sem r (ModuleTable' t')
processModuleTable f mt = do
  undefined

processCoreToTree ::
  (Members '[Files, Error JuvixError, Reader EntryPoint] r) =>
  Core.TransformationId ->
  Core.Module ->
  Sem r (PipelineResult Tree.Module)
processCoreToTree checkId md =
  processModuleCacheMiss (Pipeline.coreToTree checkId) md
