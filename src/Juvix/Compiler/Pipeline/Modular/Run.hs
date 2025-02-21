module Juvix.Compiler.Pipeline.Modular.Run where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Concrete.Data.Highlight
import Juvix.Compiler.Core qualified as Core
import Juvix.Compiler.Pipeline qualified as Pipeline
import Juvix.Compiler.Pipeline.EntryPoint
import Juvix.Compiler.Pipeline.Modular
import Juvix.Compiler.Pipeline.Run qualified as Pipeline
import Juvix.Compiler.Store.Extra qualified as Store
import Juvix.Compiler.Store.Language qualified as Store
import Juvix.Prelude

runIOEitherPipeline ::
  forall a r.
  (Members '[TaggedLock, EmbedIO] r) =>
  EntryPoint ->
  Sem (ModularEff r) a ->
  Sem r (Either JuvixError a)
runIOEitherPipeline entry a =
  evalHighlightBuilder
    . runJuvixError
    . runReader entry
    . runFilesIO
    $ inject a

runIOEitherModular ::
  forall a r.
  (Members '[TaggedLock, EmbedIO, Logger, Reader Pipeline.PipelineOptions] r) =>
  Maybe Core.TransformationId ->
  EntryPoint ->
  (Core.ModuleTable -> Sem (ModularEff r) a) ->
  Sem r (Either JuvixError (ModuleId, a))
runIOEitherModular mcheckId entry f = do
  r <- Pipeline.runIOEither entry Pipeline.upToStoredCore
  case r of
    Left e -> return $ Left e
    Right (_, res) -> do
      let md = res ^. Pipeline.pipelineResult . Core.coreResultModule
          mtab =
            over Core.moduleTable (HashMap.insert (md ^. Core.moduleId) md)
              . Store.toCoreModuleTable (res ^. Pipeline.pipelineResultImportTables)
              . HashMap.elems
              $ res ^. Pipeline.pipelineResultImports . Store.moduleTable
      merror <- case mcheckId of
        Nothing -> return $ Right ()
        Just checkId -> runIOEitherPipeline entry (Core.checkModule checkId md)
      case merror of
        Left e -> return $ Left e
        Right () -> do
          ea <- runIOEitherPipeline entry (inject (f mtab))
          case ea of
            Left e -> return $ Left e
            Right a ->
              return $ Right (md ^. Core.moduleId, a)
