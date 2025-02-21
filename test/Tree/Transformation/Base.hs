module Tree.Transformation.Base where

import Base
import Juvix.Compiler.Tree.Data.Module
import Juvix.Compiler.Tree.Transformation
import Tree.Eval.Base
import Tree.Eval.Positive qualified as Eval

data Test = Test
  { _testTransformations :: [TransformationId],
    _testAssertion :: Module -> Assertion,
    _testEval :: Eval.PosTest
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
          _testAssertion = Steps $ treeEvalAssertion file' expected' _testTransformations _testAssertion
        }
