module Commands.Dev.Core.Compile.Base where

import Commands.Base
import Commands.Dev.Core.Compile.Options
import Commands.Extra.Compile qualified as Compile
import Data.Aeson qualified as JSON
import Juvix.Compiler.Asm.Data.Module qualified as Asm
import Juvix.Compiler.Asm.Pretty qualified as Asm
import Juvix.Compiler.Backend qualified as Backend
import Juvix.Compiler.Backend.C qualified as C
import Juvix.Compiler.Backend.Rust.Data.Result qualified as Rust
import Juvix.Compiler.Casm.Data.Result qualified as Casm
import Juvix.Compiler.Casm.Pretty qualified as Casm
import Juvix.Compiler.Core.Data.Module qualified as Core
import Juvix.Compiler.Core.Data.TransformationId qualified as Core
import Juvix.Compiler.Nockma.Pretty qualified as Nockma
import Juvix.Compiler.Nockma.Translation.FromTree qualified as Nockma
import Juvix.Compiler.Reg.Data.Module qualified as Reg
import Juvix.Compiler.Reg.Pretty qualified as Reg
import Juvix.Compiler.Tree.Data.Module qualified as Tree
import Juvix.Compiler.Tree.Pretty qualified as Tree

data PipelineArg = PipelineArg
  { _pipelineArgOptions :: CompileOptions,
    _pipelineArgModule :: Core.Module
  }

outputAnomaResult :: (Members '[EmbedIO, App] r) => Path Abs File -> Nockma.AnomaResult -> Sem r ()
outputAnomaResult nockmaFile Nockma.AnomaResult {..} = do
  let code = Nockma.ppSerialize _anomaClosure
      prettyNockmaFile = replaceExtensions' [".pretty", ".nockma"] nockmaFile
  writeFileEnsureLn nockmaFile code
  writeFileEnsureLn prettyNockmaFile (Nockma.ppPrint _anomaClosure)

getEntry :: (Members '[EmbedIO, App, TaggedLock] r) => PipelineArg -> Sem r EntryPoint
getEntry PipelineArg {..} = do
  ep <- getEntryPoint (Just (_pipelineArgOptions ^. compileInputFile))
  return
    ep
      { _entryPointTarget = Just (getTarget (_pipelineArgOptions ^. compileTarget)),
        _entryPointDebug = _pipelineArgOptions ^. compileDebug,
        _entryPointUnsafe = _pipelineArgOptions ^. compileUnsafe,
        _entryPointOptimizationLevel = fromMaybe defaultOptLevel (_pipelineArgOptions ^. compileOptimizationLevel),
        _entryPointInliningDepth = _pipelineArgOptions ^. compileInliningDepth
      }
  where
    getTarget :: CompileTarget -> Backend.Target
    getTarget = \case
      AppTargetWasm32Wasi -> Backend.TargetCWasm32Wasi
      AppTargetNative64 -> Backend.TargetCNative64
      AppTargetCore -> Backend.TargetCore
      AppTargetAsm -> Backend.TargetAsm
      AppTargetReg -> Backend.TargetReg
      AppTargetTree -> Backend.TargetTree
      AppTargetAnoma -> Backend.TargetAnoma
      AppTargetCasm -> Backend.TargetCairo
      AppTargetCairo -> Backend.TargetCairo
      AppTargetRiscZeroRust -> Backend.TargetRust

    defaultOptLevel :: Int
    defaultOptLevel
      | _pipelineArgOptions ^. compileDebug = 0
      | otherwise = defaultOptimizationLevel

runCPipeline ::
  forall r.
  (Members '[EmbedIO, App, TaggedLock] r) =>
  PipelineArg ->
  Sem r ()
runCPipeline pa@PipelineArg {..} = do
  entryPoint <- getEntry pa
  C.MiniCResult {..} <-
    getRight
      . run
      . runReader entryPoint
      . runError @JuvixError
      $ coreToMiniC _pipelineArgModule
  inputfile <- getMainFile (Just (_pipelineArgOptions ^. compileInputFile))
  cFile <- inputCFile inputfile
  writeFileEnsureLn cFile _resultCCode
  outfile <- Compile.outputFile _pipelineArgOptions
  Compile.runCommand
    _pipelineArgOptions
      { _compileInputFile = AppPath (preFileFromAbs cFile) False,
        _compileOutputFile = Just (AppPath (preFileFromAbs outfile) False)
      }
  where
    inputCFile :: Path Abs File -> Sem r (Path Abs File)
    inputCFile inputFileCompile = do
      buildDir <- askBuildDir
      ensureDir buildDir
      return (buildDir <//> replaceExtension' ".c" (filename inputFileCompile))

runAsmPipeline :: (Members '[EmbedIO, App, TaggedLock] r) => PipelineArg -> Sem r ()
runAsmPipeline pa@PipelineArg {..} = do
  entryPoint <- getEntry pa
  asmFile <- Compile.outputFile _pipelineArgOptions
  r <-
    runReader entryPoint
      . runError @JuvixError
      . coreToAsm
      $ _pipelineArgModule
  md' <- getRight r
  let code = Asm.ppPrint md' (Asm.computeCombinedInfoTable md')
  writeFileEnsureLn asmFile code

runRegPipeline :: (Members '[EmbedIO, App, TaggedLock] r) => PipelineArg -> Sem r ()
runRegPipeline pa@PipelineArg {..} = do
  entryPoint <- getEntry pa
  regFile <- Compile.outputFile _pipelineArgOptions
  r <-
    runReader entryPoint
      . runError @JuvixError
      . coreToReg
      $ _pipelineArgModule
  md' <- getRight r
  let code = Reg.ppPrint md' (Reg.computeCombinedInfoTable md')
  writeFileEnsureLn regFile code

runTreePipeline :: (Members '[EmbedIO, App, TaggedLock] r) => PipelineArg -> Sem r ()
runTreePipeline pa@PipelineArg {..} = do
  entryPoint <- getEntry pa
  treeFile <- Compile.outputFile _pipelineArgOptions
  r <-
    runReader entryPoint
      . runError @JuvixError
      . coreToTree Core.IdentityTrans
      $ _pipelineArgModule
  md' <- getRight r
  let code = Tree.ppPrint md' (Tree.computeCombinedInfoTable md')
  writeFileEnsureLn treeFile code

runAnomaPipeline :: (Members '[EmbedIO, App, TaggedLock] r) => PipelineArg -> Sem r ()
runAnomaPipeline pa@PipelineArg {..} = do
  entryPoint <- getEntry pa
  nockmaFile <- Compile.outputFile _pipelineArgOptions
  r <-
    runReader entryPoint
      . runError @JuvixError
      . coreToAnoma
      $ _pipelineArgModule
  res <- getRight r
  outputAnomaResult nockmaFile res

runCasmPipeline :: (Members '[EmbedIO, App, TaggedLock] r) => PipelineArg -> Sem r ()
runCasmPipeline pa@PipelineArg {..} = do
  entryPoint <- getEntry pa
  casmFile <- Compile.outputFile _pipelineArgOptions
  r <-
    runReader entryPoint
      . runError @JuvixError
      . coreToCasm
      $ _pipelineArgModule
  Casm.Result {..} <- getRight r
  writeFileEnsureLn casmFile (toPlainText $ Casm.ppProgram _resultCode)

runCairoPipeline :: (Members '[EmbedIO, App, TaggedLock] r) => PipelineArg -> Sem r ()
runCairoPipeline pa@PipelineArg {..} = do
  entryPoint <- getEntry pa
  cairoFile <- Compile.outputFile _pipelineArgOptions
  r <-
    runReader entryPoint
      . runError @JuvixError
      . coreToCairo
      $ _pipelineArgModule
  res <- getRight r
  liftIO $ JSON.encodeFile (toFilePath cairoFile) res

runRiscZeroRustPipeline :: (Members '[EmbedIO, App, TaggedLock] r) => PipelineArg -> Sem r ()
runRiscZeroRustPipeline pa@PipelineArg {..} = do
  entryPoint <- getEntry pa
  rustFile <- Compile.outputFile _pipelineArgOptions
  r <-
    runReader entryPoint
      . runError @JuvixError
      . coreToRiscZeroRust
      $ _pipelineArgModule
  Rust.Result {..} <- getRight r
  writeFileEnsureLn rustFile _resultRustCode
