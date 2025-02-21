module Asm.Transformation.Base where

import Asm.Run.Base
import Asm.Run.Positive qualified as Run
import Base
import Juvix.Compiler.Asm.Data.Module
import Juvix.Compiler.Asm.Error

data Test = Test
  { _testTransformation :: Module -> Either AsmError Module,
    _testAssertion :: Module -> Assertion,
    _testEval :: Run.PosTest
  }

fromTest :: Test -> TestTree
fromTest = mkTest . toTestDescr

troot :: Path Abs Dir
troot = relToProject $(mkRelDir "tests/Asm/positive/")

toTestDescr :: Test -> TestDescr
toTestDescr Test {..} =
  let Run.PosTest {..} = _testEval
      tRoot = troot <//> _relDir
      file' = tRoot <//> _file
      expected' = tRoot <//> _expectedFile
   in TestDescr
        { _testName = _name,
          _testRoot = tRoot,
          _testAssertion = Steps $ asmRunAssertion file' expected' _testTransformation _testAssertion
        }
