module Tree.Compile.Base where

import Asm.Compile.Base qualified as Asm
import Base
import GHC.Base (seq)
import Juvix.Compiler.Asm.Pretty qualified as Asm
import Juvix.Compiler.Pipeline.EntryPoint qualified as EntryPoint
import Juvix.Compiler.Tree.Data.Module
import Juvix.Compiler.Tree.Translation.FromSource
import Juvix.Data.PPOutput
import Tree.Eval.Positive qualified as Eval

newtype Test = Test
  { _testEval :: Eval.PosTest
  }

fromTest :: Test -> TestTree
fromTest = mkTest . toTestDescr

root :: Path Abs Dir
root = relToProject $(mkRelDir "tests/Tree/positive/")

toTestDescr :: Test -> TestDescr
toTestDescr Test {..} =
  let Eval.PosTest {..} = _testEval
      tRoot = root <//> _relDir
      file' = tRoot <//> _file
      expected' = tRoot <//> _expectedFile
   in TestDescr
        { _testName = _name,
          _testRoot = tRoot,
          _testAssertion = Steps $ treeCompileAssertion tRoot file' expected' ""
        }

treeCompileAssertion' ::
  EntryPoint ->
  Int ->
  InfoTable ->
  Path Abs File ->
  Path Abs File ->
  Text ->
  (String -> IO ()) ->
  Assertion
treeCompileAssertion' entryPoint optLevel tab mainFile expectedFile stdinText step = do
  step "Translate to JuvixAsm"
  case run . runReader entryPoint' . runError $ treeToAsm (moduleFromInfoTable tab) of
    Left err -> assertFailure (prettyString (fromJuvixError @GenericError err))
    Right md -> do
      length (fromText (Asm.ppPrint md (computeCombinedInfoTable md)) :: String) `seq`
        Asm.asmCompileAssertion' entryPoint' optLevel md mainFile expectedFile stdinText step
  where
    entryPoint' = entryPoint {_entryPointOptimizationLevel = optLevel}

treeCompileAssertion ::
  Path Abs Dir ->
  Path Abs File ->
  Path Abs File ->
  Text ->
  (String -> IO ()) ->
  Assertion
treeCompileAssertion root' mainFile expectedFile stdinText step = do
  step "Parse"
  s <- readFile mainFile
  case runParser mainFile s of
    Left err -> assertFailure (prettyString err)
    Right md -> do
      entryPoint <-
        set entryPointPipeline (Just EntryPoint.PipelineExec)
          <$> testDefaultEntryPointIO root' mainFile
      let tab = computeCombinedInfoTable md
      treeCompileAssertion' entryPoint 3 tab mainFile expectedFile stdinText step
