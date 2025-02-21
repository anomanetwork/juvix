module Tree.Compile.Positive where

import Base
import Tree.Compile.Base
import Tree.Eval.Positive qualified as Eval

testDescr :: Eval.PosTest -> TestDescr
testDescr Eval.PosTest {..} =
  let tRoot = Eval.root <//> _relDir
      file' = tRoot <//> _file
      expected' = tRoot <//> _expectedFile
   in TestDescr
        { _testName = _name,
          _testRoot = tRoot,
          _testAssertion = Steps $ treeCompileAssertion tRoot file' expected' ""
        }

ignoredTests :: [String]
ignoredTests = ["Test040: ByteArray"]

allTests :: TestTree
allTests =
  testGroup
    "JuvixTree compilation positive tests"
    (map (mkTest . testDescr) (Eval.filterOutTests ignoredTests Eval.tests))
