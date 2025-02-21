module Tree where

import Base
import Tree.Asm qualified as Asm
import Tree.Compile qualified as Compile
import Tree.Eval qualified as Eval
import Tree.Parse qualified as Parse
import Tree.Transformation qualified as Transformation

allTests :: TestTree
allTests = testGroup "JuvixTree tests" [Parse.allTests, Eval.allTests, Asm.allTests, Transformation.allTests, Compile.allTests]
