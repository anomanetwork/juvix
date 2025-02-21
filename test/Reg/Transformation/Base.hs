module Reg.Transformation.Base where

import Base
import Juvix.Compiler.Reg.Data.Module
import Juvix.Compiler.Reg.Transformation
import Reg.Parse.Positive qualified as Parse
import Reg.Run.Base

data Test = Test
  { _testTransformations :: [TransformationId],
    _testAssertion :: Module -> Assertion,
    _testRun :: Parse.PosTest
  }

fromTest :: Test -> TestTree
fromTest = mkTest . toTestDescr

root :: Path Abs Dir
root = relToProject $(mkRelDir "tests/Reg/positive/")

toTestDescr :: Test -> TestDescr
toTestDescr Test {..} =
  let Parse.PosTest {..} = _testRun
      tRoot = root <//> _relDir
      file' = tRoot <//> _file
      expected' = tRoot <//> _expectedFile
   in TestDescr
        { _testName = _name,
          _testRoot = tRoot,
          _testAssertion = Steps $ regRunAssertion file' expected' _testTransformations _testAssertion
        }
