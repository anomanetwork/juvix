module Casm.Compilation.Base
  ( module Casm.Compilation.Base,
    cairoVmPrecondition,
  )
where

import Base
import Casm.Run.Base
import Juvix.Compiler.Backend
import Juvix.Compiler.Casm.Data.Result
import Juvix.Compiler.Casm.Pretty
import Juvix.Compiler.Core qualified as Core
import Juvix.Data.Field
import Juvix.Data.PPOutput

compileAssertion ::
  Path Abs Dir ->
  Bool ->
  Bool ->
  Int ->
  Path Abs File ->
  Maybe (Path Abs File) ->
  Path Abs File ->
  (String -> IO ()) ->
  Assertion
compileAssertion = compileAssertionEntry (\e -> e {_entryPointFieldSize = cairoFieldSize})

compileAssertionEntry ::
  (EntryPoint -> EntryPoint) ->
  Path Abs Dir ->
  Bool ->
  Bool ->
  Int ->
  Path Abs File ->
  Maybe (Path Abs File) ->
  Path Abs File ->
  (String -> IO ()) ->
  Assertion
compileAssertionEntry adjustEntry root' bInterp bRunVM optLevel mainFile inputFile expectedFile step = do
  step "Translate to JuvixCore"
  entryPoint <- adjustEntry <$> testDefaultEntryPointIO root' mainFile
  let entryPoint' =
        entryPoint
          { _entryPointOptimizationLevel = optLevel,
            _entryPointPipeline = Just PipelineExec,
            _entryPointTarget = Just TargetCairo
          }
  PipelineResult {..} <- snd <$> testRunIO entryPoint' upToStoredCore
  step "Translate to CASM"
  case run $ runError @JuvixError $ runReader entryPoint' $ storedCoreToCasm (Core.combineInfoTables (_pipelineResult ^. Core.coreResultModule)) of
    Left err -> assertFailure (prettyString (fromJuvixError @GenericError err))
    Right Result {..} -> do
      withTempDir'
        ( \dirPath -> do
            let tmpFile = dirPath <//> $(mkRelFile "tmp.out")
            step "Pretty print"
            writeFileEnsureLn tmpFile (toPlainText $ ppProgram _resultCode)
        )
      casmRunAssertion' entryPoint' bInterp bRunVM _resultLabelInfo _resultCode _resultBuiltins _resultOutputSize inputFile expectedFile step

compileErrorAssertion :: Path Abs Dir -> Path Abs File -> (String -> IO ()) -> Assertion
compileErrorAssertion root' mainFile step = do
  step "Translate to JuvixCore"
  entryPoint <- testDefaultEntryPointIO root' mainFile
  let entryPoint' = entryPoint {_entryPointFieldSize = cairoFieldSize}
  PipelineResult {..} <- snd <$> testRunIO entryPoint' upToStoredCore
  step "Translate to CASM"
  case run $ runError @JuvixError $ runReader entryPoint $ storedCoreToCasm (_pipelineResult ^. Core.coreResultModule) of
    Left {} -> assertBool "" True
    Right {} -> assertFailure "no error"
