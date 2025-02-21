module Core.Compile.Base where

import Asm.Compile.Base qualified as Asm
import Base
import Core.Eval.Base
import Core.Eval.Positive qualified as Eval
import GHC.Base (seq)
import Juvix.Compiler.Asm.Pretty qualified as Asm
import Juvix.Compiler.Core.Data.Module
import Juvix.Compiler.Core.Pipeline
import Juvix.Compiler.Core.Translation.FromSource
import Juvix.Compiler.Pipeline.EntryPoint qualified as EntryPoint
import Juvix.Data.PPOutput

newtype Test = Test
  { _testEval :: Eval.PosTest
  }

fromTest :: Test -> TestTree
fromTest = mkTest . toTestDescr

root :: Path Abs Dir
root = relToProject $(mkRelDir "tests/Core/positive/")

toTestDescr :: Test -> TestDescr
toTestDescr Test {..} =
  let Eval.PosTest {..} = _testEval
      tRoot = root <//> _relDir
      file' = tRoot <//> _file
      expected' = tRoot <//> _expectedFile
   in TestDescr
        { _testName = _name,
          _testRoot = tRoot,
          _testAssertion = Steps $ coreCompileAssertion tRoot file' expected' ""
        }

coreCompileAssertion' ::
  EntryPoint ->
  Int ->
  InfoTable ->
  Path Abs File ->
  Path Abs File ->
  Text ->
  (String -> IO ()) ->
  Assertion
coreCompileAssertion' entryPoint optLevel tab mainFile expectedFile stdinText step = do
  step "Translate to JuvixAsm"
  case run . runReader entryPoint' . runError $ storedCoreToAsm (moduleFromInfoTable tab) of
    Left err -> assertFailure (prettyString (fromJuvixError @GenericError err))
    Right md -> do
      length (fromText (Asm.ppPrint md (computeCombinedInfoTable md)) :: String) `seq`
        Asm.asmCompileAssertion' entryPoint' optLevel md mainFile expectedFile stdinText step
  where
    entryPoint' = entryPoint {_entryPointOptimizationLevel = optLevel}

coreCompileAssertion ::
  Path Abs Dir ->
  Path Abs File ->
  Path Abs File ->
  Text ->
  (String -> IO ()) ->
  Assertion
coreCompileAssertion root' mainFile expectedFile stdinText step = do
  step "Parse"
  r <- parseFile mainFile
  case r of
    Left err -> assertFailure (prettyString (fromJuvixError @GenericError err))
    Right (_, Nothing) -> do
      step "Empty program: compare expected and actual program output"
      expected <- readFile expectedFile
      assertEqDiffText ("Check: EVAL output = " <> toFilePath expectedFile) "" expected
    Right (tabIni, Just node) -> do
      entryPoint <-
        set entryPointPipeline (Just EntryPoint.PipelineExec)
          <$> testDefaultEntryPointIO root' mainFile
      step "Transform JuvixCore"
      let tab0 = setupMainFunction defaultModuleId tabIni node
      case run . runReader entryPoint . runError $ toStored (moduleFromInfoTable tab0) of
        Left err -> assertFailure (prettyString (fromJuvixError @GenericError err))
        Right m -> do
          let tab = computeCombinedInfoTable m
          coreCompileAssertion' entryPoint 3 tab mainFile expectedFile stdinText step
