module Rust.Compilation.Base where

import Base
import Data.FileEmbed
import Juvix.Compiler.Backend (Target (TargetRust))
import Juvix.Compiler.Backend.Rust.Data.Result
import Juvix.Compiler.Backend.Rust.Pretty
import Juvix.Compiler.Core qualified as Core
import System.Process qualified as P

precondition :: Assertion
precondition = do
  assertCmdExists $(mkRelFile "rustc")

compileAssertion ::
  Path Abs Dir ->
  Int ->
  Path Abs File ->
  Path Abs File ->
  (String -> IO ()) ->
  Assertion
compileAssertion root' optLevel mainFile expectedFile step = do
  step "Translate to JuvixCore"
  entryPoint <-
    set entryPointTarget (Just TargetRust)
      . set entryPointPipeline (Just PipelineExec)
      . set entryPointOptimizationLevel optLevel
      <$> testDefaultEntryPointIO root' mainFile
  PipelineResult {..} <- snd <$> testRunIO entryPoint upToStoredCore
  step "Translate to Rust"
  case run $ runError @JuvixError $ runReader entryPoint $ storedCoreToRust (Core.combineInfoTables (_pipelineResult ^. Core.coreResultModule)) of
    Left err -> assertFailure (prettyString (fromJuvixError @GenericError err))
    Right Result {..} -> do
      withTempDir'
        ( \dirPath -> do
            let inputFile = dirPath <//> $(mkRelFile "Program.rs")
            writeFileEnsureLn inputFile _resultRustCode

            expected <- readFile expectedFile

            let executeNative :: Path Abs File -> IO Text
                executeNative outputFile = readProcess (toFilePath outputFile) [] ""

            step "Compile Rust to native code"
            actualNative <- rustcCompile (nativeArgs optLevel) inputFile $(mkRelFile "Program") executeNative step
            step "Compare expected and actual program output"
            assertEqDiffText ("check: native output = " <> toFilePath expectedFile) actualNative expected
        )

rustcCompile ::
  (Path Abs File -> Path Abs File -> [String]) ->
  Path Abs File ->
  Path Rel File ->
  (Path Abs File -> IO Text) ->
  (String -> IO ()) ->
  IO Text
rustcCompile mkRustcArgs inputFile outputFile execute step =
  withTempDir'
    ( \dirPath -> do
        let outputFile' = dirPath <//> outputFile
        step "Rust compilation"
        P.callProcess
          "rustc"
          (mkRustcArgs outputFile' inputFile)
        step "Execution"
        execute outputFile'
    )

nativeArgs :: Int -> Path Abs File -> Path Abs File -> [String]
nativeArgs optLevel outputFile inputFile =
  [ "-o",
    toFilePath outputFile,
    "-C",
    "opt-level="
      <> show optLevel,
    "-L",
    juvixLibraryDir,
    toFilePath inputFile
  ]
  where
    juvixLibraryDir :: FilePath
    juvixLibraryDir =
      if
          | optLevel > 0 ->
              $(makeRelativeToProject "runtime/rust/juvix/target/release" >>= strToExp)
          | otherwise ->
              $(makeRelativeToProject "runtime/rust/juvix/target/debug" >>= strToExp)
