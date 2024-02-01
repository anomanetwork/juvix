module Nockma.Compile.Tree.Positive where

import Base
import Juvix.Compiler.Nockma.EvalCompiled
import Juvix.Compiler.Nockma.Evaluator qualified as NockmaEval
import Juvix.Compiler.Nockma.Language
import Juvix.Compiler.Nockma.Pretty qualified as Nockma
import Juvix.Compiler.Nockma.Translation.FromTree
import Juvix.Compiler.Tree
import Tree.Eval.Base
import Tree.Eval.Positive qualified as Tree

runNockmaAssertion :: Handle -> Symbol -> InfoTable -> IO ()
runNockmaAssertion hout _main tab = do
  compiled@(Nockma.Cell nockSubject nockMain) <-
    runM
      . runErrorIO' @JuvixError
      . runReader opts
      $ treeToNockma' tab
  writeFileEnsureLn (relToProject $(mkRelFile "compiled.nockma")) (Nockma.ppPrint compiled)
  res <-
    runM
      . runOutputSem @(Term Natural)
        (embed . hPutStrLn hout . Nockma.ppPrint)
      . runReader NockmaEval.defaultEvalOptions
      . evalCompiledNock' nockSubject
      $ nockMain
  let ret = getReturn res
  hPutStrLn hout (Nockma.ppPrint ret)
  where
    opts :: CompilerOptions
    opts =
      CompilerOptions
        { _compilerOptionsEnableTrace = True
        }

    getReturn :: Term Natural -> Term Natural
    getReturn = id

testDescr :: Tree.PosTest -> TestDescr
testDescr Tree.PosTest {..} =
  let tRoot = Tree.root <//> _relDir
      file' = tRoot <//> _file
      expected' = tRoot <//> _expectedFile
   in TestDescr
        { _testName = _name,
          _testRoot = tRoot,
          _testAssertion = Steps $ treeEvalAssertionParam runNockmaAssertion file' expected' [] (const (return ()))
        }

testsSlow :: [Int]
testsSlow = []

testsAdt :: [Int]
testsAdt = [9, 15, 18, 25, 26, 29, 35]

testsNegativeInteger :: [Int]
testsNegativeInteger = [16, 31]

testsHopeless :: [Int]
testsHopeless =
  [ 5,
    6,
    14,
    24,
    37
  ]

testsBugged :: [Int]
testsBugged =
  []

testsToIgnore :: [Int]
testsToIgnore = testsHopeless ++ testsBugged ++ testsSlow ++ testsAdt ++ testsNegativeInteger

shouldRun :: Tree.PosTest -> Bool
shouldRun Tree.PosTest {..} = testNum `notElem` map to3DigitString testsToIgnore
  where
    testNum :: String
    testNum = take 3 (drop 4 _name)
    to3DigitString :: Int -> String
    to3DigitString n
      | n < 10 = "00" ++ show n
      | n < 100 = "0" ++ show n
      | n < 1000 = show n
      | otherwise = impossible

allTests :: TestTree
allTests =
  testGroup
    "Nockma Tree compile positive tests"
    (map (mkTest . testDescr) (filter shouldRun Tree.tests))
