module Nockma.Compile.Tree.Positive where

import Base
import Juvix.Compiler.Nockma.Anoma
import Juvix.Compiler.Nockma.EvalCompiled
import Juvix.Compiler.Nockma.Evaluator qualified as NockmaEval
import Juvix.Compiler.Nockma.Language qualified as Nockma
import Juvix.Compiler.Nockma.Pretty qualified as Nockma
import Juvix.Compiler.Nockma.Translation.FromTree
import Juvix.Compiler.Tree
import Tree.Eval.Base
import Tree.Eval.Positive qualified as Tree

runNockmaAssertion :: Path Abs Dir -> Handle -> Symbol -> Module -> IO ()
runNockmaAssertion root hout _main md = do
  entryPoint <- testDefaultEntryPointNoFileIO root
  let entryPoint' = entryPoint {_entryPointDebug = True}
  anomaRes :: AnomaResult <-
    runM
      . runErrorIO' @JuvixError
      . runReader entryPoint'
      $ treeToAnoma md
  res <-
    runM
      . runOutputSem @(Nockma.Term Natural)
        (hPutStrLn hout . Nockma.ppTest)
      . runReader NockmaEval.defaultEvalOptions
      . NockmaEval.ignoreOpCounts
      $ evalCompiledNock' (anomaRes ^. anomaClosure) (anomaCall [])
  let ret = getReturn res
  whenJust ret (hPutStrLn hout . Nockma.ppTest)
  where
    getReturn :: Nockma.Term Natural -> Maybe (Nockma.Term Natural)
    getReturn = \case
      Nockma.TermAtom Nockma.Atom {..}
        | _atomInfo ^. Nockma.atomInfoHint == Just Nockma.AtomHintVoid -> Nothing
      t -> Just t

testDescr :: Tree.PosTest -> TestDescr
testDescr Tree.PosTest {..} =
  let tRoot = Tree.root <//> _relDir
      file' = tRoot <//> _file
      expected' = tRoot <//> _expectedFile
   in TestDescr
        { _testName = _name,
          _testRoot = tRoot,
          _testAssertion = Steps $ treeEvalAssertionParam (runNockmaAssertion tRoot) file' expected' [] (const (return ()))
        }

-- | Tests which require Nockma-specific expected output files
testsConstr :: [Int]
testsConstr = [9, 28, 35, 40]

testsNegativeInteger :: [Int]
testsNegativeInteger = [16]

-- Tests involving Strings and IO
testsUnsupported :: [Int]
testsUnsupported =
  [ 5,
    6,
    37
  ]

testsToIgnore :: [Int]
testsToIgnore = testsUnsupported ++ testsNegativeInteger

convertTest :: Tree.PosTest -> Maybe (Tree.PosTest)
convertTest p = do
  guard (testNum `notElem` map to3DigitString testsToIgnore)
  return $
    if
        | testNum `elem` map to3DigitString testsConstr -> over Tree.expectedFile go p
        | otherwise -> p
  where
    go :: Base.Path Rel File -> Base.Path Rel File
    go = replaceExtensions' [".nockma", ".out"]

    testNum :: Text
    testNum = pack (take 3 (drop 4 (p ^. Tree.name)))

allTests :: TestTree
allTests =
  testGroup
    "Nockma Tree compile positive tests"
    (map (mkTest . testDescr) (mapMaybe convertTest Tree.tests))
