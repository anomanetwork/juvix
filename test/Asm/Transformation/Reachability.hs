module Asm.Transformation.Reachability (allTests) where

import Asm.Run.Positive qualified as Run
import Asm.Transformation.Base
import Base
import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Asm.Options
import Juvix.Compiler.Asm.Transformation
import Juvix.Compiler.Asm.Transformation.Base
import Juvix.Compiler.Tree.Options qualified as Tree

data ReachabilityTest = ReachabilityTest
  { _reachabilityTestReachable :: [Text],
    _reachabilityTestEval :: Run.PosTest
  }

allTests :: TestTree
allTests =
  testGroup "Reachability" $
    map liftTest rtests

rtests :: [ReachabilityTest]
rtests =
  [ ReachabilityTest
      { _reachabilityTestReachable = ["f", "f'", "g'", "h", "h'", "main"],
        _reachabilityTestEval =
          Run.PosTest
            "Test001: Reachability"
            $(mkRelDir "reachability")
            $(mkRelFile "test001.jva")
            $(mkRelFile "out/test001.out")
      },
    ReachabilityTest
      { _reachabilityTestReachable = ["f", "g", "id", "sum", "main"],
        _reachabilityTestEval =
          Run.PosTest
            "Test002: Reachability with loops & closures"
            $(mkRelDir "reachability")
            $(mkRelFile "test002.jva")
            $(mkRelFile "out/test002.out")
      }
  ]

liftTest :: ReachabilityTest -> TestTree
liftTest ReachabilityTest {..} =
  fromTest
    Test
      { _testTransformation = runTransformation (runReader opts . filterUnreachable),
        _testAssertion = \md -> unless (nubSort (map (^. functionName) (HashMap.elems (md ^. moduleInfoTable . infoFunctions))) == nubSort _reachabilityTestReachable) (error "check reachable"),
        _testEval = _reachabilityTestEval
      }
  where
    opts =
      Options
        { _optDebug = True,
          _optLimits = getLimits TargetCWasm32Wasi True,
          _optTreeOptions = Tree.defaultOptions
        }
