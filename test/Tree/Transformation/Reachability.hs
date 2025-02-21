module Tree.Transformation.Reachability (allTests) where

import Base
import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Tree.Transformation as Tree
import Tree.Eval.Positive qualified as Eval
import Tree.Transformation.Base

data ReachabilityTest = ReachabilityTest
  { _reachabilityTestReachable :: [Text],
    _reachabilityTestEval :: Eval.PosTest
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
          Eval.PosTest
            "Test001: Reachability"
            $(mkRelDir "reachability")
            $(mkRelFile "test001.jvt")
            $(mkRelFile "out/test001.out")
      },
    ReachabilityTest
      { _reachabilityTestReachable = ["f", "g", "id", "sum", "main"],
        _reachabilityTestEval =
          Eval.PosTest
            "Test002: Reachability with loops & closures"
            $(mkRelDir "reachability")
            $(mkRelFile "test002.jvt")
            $(mkRelFile "out/test002.out")
      }
  ]

liftTest :: ReachabilityTest -> TestTree
liftTest ReachabilityTest {..} =
  fromTest
    Test
      { _testTransformations = [Tree.FilterUnreachable],
        _testAssertion = \md -> unless (nubSort (map (^. functionName) (HashMap.elems (md ^. moduleInfoTable . infoFunctions))) == nubSort _reachabilityTestReachable) (error "check reachable"),
        _testEval = _reachabilityTestEval
      }
