module Reachability.Positive where

import Base
import Data.HashSet qualified as HashSet
import Juvix.Compiler.Builtins (iniState)
import Juvix.Compiler.Internal.Language qualified as Internal
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Data.Context qualified as Internal
import Juvix.Compiler.Pipeline

data PosTest = PosTest
  { _name :: String,
    _relDir :: Path Rel Dir,
    _stdlibMode :: StdlibMode,
    _file :: Path Rel File,
    _reachable :: HashSet String
  }

makeLenses ''PosTest

root :: Path Abs Dir
root = relToProject $(mkRelDir "tests/positive")

testDescr :: PosTest -> TestDescr
testDescr PosTest {..} =
  let tRoot = root <//> _relDir
      file' = tRoot <//> _file
   in TestDescr
        { _testName = _name,
          _testRoot = tRoot,
          _testAssertion = Steps $ \step -> do
            let noStdlib = _stdlibMode == StdlibExclude
                entryPoint =
                  (defaultEntryPoint tRoot file')
                    { _entryPointRoot = tRoot,
                      _entryPointNoStdlib = noStdlib
                    }

            step "Pipeline up to reachability"
            p :: Internal.InternalTypedResult <- snd <$> runIO' iniState entryPoint upToInternalReachability

            step "Check reachability results"
            let names = concatMap getNames (p ^. Internal.resultModules)
            mapM_ check names
        }
  where
    check n = assertBool ("unreachable not filtered: " ++ unpack n) (HashSet.member (unpack n) _reachable)

getNames :: Internal.Module -> [Text]
getNames m = concatMap getDeclName (m ^. (Internal.moduleBody . Internal.moduleStatements))
  where
    getDeclName :: Internal.Statement -> [Text]
    getDeclName = \case
      Internal.StatementInductive i -> [i ^. (Internal.inductiveName . Internal.nameText)]
      Internal.StatementFunction (Internal.MutualBlock f) -> map (^. Internal.funDefName . Internal.nameText) (toList f)
      Internal.StatementForeign {} -> []
      Internal.StatementAxiom ax -> [ax ^. (Internal.axiomName . Internal.nameText)]
      Internal.StatementInclude i -> getNames (i ^. Internal.includeModule)

allTests :: TestTree
allTests =
  testGroup
    "Reachability positive tests"
    (map (mkTest . testDescr) tests)

tests :: [PosTest]
tests =
  [ PosTest
      "Reachability with modules"
      $(mkRelDir "Reachability")
      StdlibInclude
      $(mkRelFile "M.juvix")
      ( HashSet.fromList
          ["f", "g", "h", "Bool", "Maybe"]
      ),
    PosTest
      "Reachability with modules and standard library"
      $(mkRelDir "Reachability")
      StdlibInclude
      $(mkRelFile "N.juvix")
      ( HashSet.fromList
          ["test", "Unit"]
      ),
    PosTest
      "Reachability with public imports"
      $(mkRelDir "Reachability")
      StdlibInclude
      $(mkRelFile "O.juvix")
      ( HashSet.fromList
          ["f", "g", "h", "k", "Bool", "Maybe"]
      )
  ]
