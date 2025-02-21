module Tree.Compile where

import Base
import Tree.Compile.Positive qualified as P

allTests :: TestTree
allTests = testGroup "JuvixTree compilation tests" [P.allTests]
