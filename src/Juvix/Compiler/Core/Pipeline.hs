module Juvix.Compiler.Core.Pipeline
  ( module Juvix.Compiler.Core.Pipeline,
    module Juvix.Compiler.Core.Data.InfoTable,
  )
where

import Juvix.Compiler.Core.Data.InfoTable
import Juvix.Compiler.Core.Data.TransformationId.Base (pipeline)
import Juvix.Compiler.Core.Options
import Juvix.Compiler.Core.Transformation
import Juvix.Compiler.Pipeline.EntryPoint (EntryPoint, entryPointNoCheck, entryPointPipeline)
import Juvix.Compiler.Pipeline.EntryPoint qualified as EntryPoint

toTypechecked :: (Members '[Error JuvixError, Reader EntryPoint] r) => Module -> Sem r Module
toTypechecked = mapReader fromEntryPoint . applyTransformations toTypecheckTransformations

-- | Perform transformations on Core before storage
toStored' :: (Members '[Error JuvixError, Reader EntryPoint] r) => PipelineId -> Module -> Sem r Module
toStored' pid = mapReader fromEntryPoint . applyTransformations (pipeline pid)

toStored :: (Members '[Error JuvixError, Reader EntryPoint] r) => Module -> Sem r Module
toStored md = do
  pid <- asks (^. entryPointPipeline)
  toStored' (maybe PipelineTypecheck toCorePipeline pid) md
  where
    toCorePipeline :: EntryPoint.Pipeline -> PipelineId
    toCorePipeline = \case
      EntryPoint.PipelineEval -> PipelineEval
      EntryPoint.PipelineExec -> PipelineExec
      EntryPoint.PipelineTypecheck -> PipelineTypecheck

-- | Perform transformations on stored Core necessary before the translation to
-- Core.Stripped
toStripped :: (Members '[Error JuvixError, Reader EntryPoint] r) => TransformationId -> Module -> Sem r Module
toStripped checkId md = do
  noCheck <- asks (^. entryPointNoCheck)
  let checkId' = if noCheck then IdentityTrans else checkId
  mapReader fromEntryPoint $
    applyTransformations (toStrippedTransformations checkId') md

checkModule :: (Members '[Error JuvixError, Reader EntryPoint] r) => TransformationId -> Module -> Sem r ()
checkModule checkId md = do
  noCheck <- asks (^. entryPointNoCheck)
  let checkId' = if noCheck then IdentityTrans else checkId
  mapReader fromEntryPoint $
    void (applyTransformations [CombineInfoTables, FilterUnreachable, checkId'] md)
