module Rust.RiscZero.Base where

import Base
import Data.FileEmbed qualified as FE
import Data.Text qualified as T
import Juvix.Compiler.Backend
import Juvix.Compiler.Backend.Rust.Data.Result
import Juvix.Compiler.Backend.Rust.Pretty
import Juvix.Compiler.Core qualified as Core
import System.Directory
import System.Environment qualified as E
import System.Process qualified as P

precondition :: Assertion
precondition = do
  assertCmdExists $(mkRelFile "cargo")
  assertCmdExists $(mkRelFile "r0vm")

compileAssertion ::
  IO (Path Abs Dir) ->
  Path Abs Dir ->
  Int ->
  Path Abs File ->
  Path Abs File ->
  (String -> IO ()) ->
  Assertion
compileAssertion tmpDir' root' optLevel mainFile expectedFile step = do
  tmpDir <- tmpDir'
  step "Translate to JuvixCore"
  entryPoint <-
    set entryPointTarget (Just TargetRust)
      . set entryPointPipeline (Just PipelineExec)
      . set entryPointOptimizationLevel optLevel
      <$> testDefaultEntryPointIO root' mainFile
  PipelineResult {..} <- snd <$> testRunIO entryPoint upToStoredCore
  step "Translate to RISC0 Rust"
  case run $ runError @JuvixError $ runReader entryPoint $ storedCoreToRiscZeroRust (Core.combineInfoTables (_pipelineResult ^. Core.coreResultModule)) of
    Left err -> assertFailure (prettyString (fromJuvixError @GenericError err))
    Right Result {..} -> do
      let outDir = tmpDir <//> $(mkRelDir "out")
      unlessM (doesDirectoryExist (toFilePath outDir)) $ do
        writeDirFiles riscZeroDir outDir
        let outJuvixSourceDir :: Path Abs Dir = outDir <//> $(mkRelDir "juvix") <//> $(mkRelDir "src")
        writeDirFiles rustRuntimeSourceDir outJuvixSourceDir
        writeFile (outDir <//> $(mkRelDir "juvix") <//> $(mkRelFile "Cargo.toml")) rustRuntimeToml
      let outFile :: Path Abs File =
            outDir
              <//> $(mkRelDir "methods")
              <//> $(mkRelDir "guest")
              <//> $(mkRelDir "src")
              <//> $(mkRelFile "main.rs")
      writeFileEnsureLn outFile _resultRustCode

      expected <- readFile expectedFile

      step "Compile Rust to RISC0"
      actualOutput <- cargoCompile outDir
      step "Compare expected and actual program output"
      assertEqDiffText ("check: native output = " <> toFilePath expectedFile) actualOutput expected
  where
    riscZeroDir :: [(FilePath, ByteString)]
    riscZeroDir = $(FE.makeRelativeToProject "runtime/rust/risc0" >>= FE.embedDir)

    rustRuntimeSourceDir :: [(FilePath, ByteString)]
    rustRuntimeSourceDir = $(FE.makeRelativeToProject "runtime/rust/juvix/src" >>= FE.embedDir)

    rustRuntimeToml :: ByteString
    rustRuntimeToml = $(FE.makeRelativeToProject "runtime/rust/juvix/Cargo.toml" >>= FE.embedFile)

    writeDirFiles :: [(FilePath, ByteString)] -> Path Abs Dir -> IO ()
    writeDirFiles fs dir = do
      let fs' = map (first relFile) fs
      forM_ (first (dir <//>) <$> fs') (uncurry writeFile)

    cargoCompile :: Path Abs Dir -> IO Text
    cargoCompile outDir =
      withCurrentDirectory (toFilePath outDir) $ do
        step "Rust compilation"
        P.callProcess
          "cargo"
          ["build"]
        step "Execution"
        let hostFile =
              outDir
                <//> $(mkRelDir "target")
                <//> $(mkRelDir "debug")
                <//> $(mkRelFile "host")
        executeRISC0 hostFile

    executeRISC0 :: Path Abs File -> IO Text
    executeRISC0 outputFile = do
      env <- E.getEnvironment
      out <- readProcessWithEnv (("RISC0_DEV_MODE", "1") : env) (toFilePath outputFile) [] ""
      return $ T.unlines $ filter (not . T.isPrefixOf "WARNING:") $ T.lines out
