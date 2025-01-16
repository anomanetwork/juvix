module Anoma.Client.Positive where

import Anoma.Client.Base
import Anoma.Effect
import Base
import Juvix.Compiler.Nockma.Language hiding (Path)
import Juvix.Compiler.Nockma.Translation.FromTree (anomaClosure)
import Juvix.Prelude.Pretty

root :: Path Abs Dir
root = relToProject $(mkRelDir "tests/Anoma/Client")

data ClientTest = ClientTest
  { _clientTestTag :: Text,
    _clientRelRoot :: Path Rel Dir,
    _clientMainFile :: Path Rel File,
    _clientAssertion :: forall r. (Members '[Logger, Error SimpleError, Anoma, EmbedIO, TestStep] r) => Term Natural -> Sem r ()
  }

makeLenses ''ClientTest

withRootCopy :: (Path Abs Dir -> IO a) -> IO a
withRootCopy = withRootTmpCopy root

fromClientTest :: ClientTest -> TestTree
fromClientTest t = testCaseSteps (t ^. clientTestTag) assertion
  where
    assertion :: (Text -> IO ()) -> Assertion
    assertion stepFun = runM . runProcess . runSimpleErrorHUnit . ignoreLogger . runStep stepFun $ do
      step "Compiling"
      res :: AnomaResult <- liftIO $ withRootCopy (compileMain False (t ^. clientRelRoot) (t ^. clientMainFile))
      let program :: Term Natural = (res ^. anomaClosure)
      p <- envAnomaPath
      runAnomaEphemeral p ((t ^. clientAssertion) program)

-- | Run prove with the given arguements and submit the result to the mempool.
-- Returns the traces from the prove endpoint
proveAndSubmit ::
  (Members '[Logger, Error SimpleError, Anoma, EmbedIO, TestStep] r) =>
  Term Natural ->
  [Term Natural] ->
  Sem r [Term Natural]
proveAndSubmit program proveArgs = do
  step "Proving"
  resProve <-
    runNockma
      RunNockmaInput
        { _runNockmaProgram = program,
          _runNockmaArgs = proveArgs
        }
  step "Submitting transaction candidate"
  addTransaction
    AddTransactionInput
      { _addTransactionInputCandidate = resProve ^. runNockmaResult
      }
  return (resProve ^. runNockmaTraces)

isListUnrevealedCommitsAvailable :: ListUnrevealedCommitsResult -> Bool
isListUnrevealedCommitsAvailable l = not (null (l ^. listUnrevealedCommitsResultCommits))

clientTests :: [ClientTest]
clientTests =
  [ ClientTest
      { _clientTestTag = "Submit swap transaction",
        _clientRelRoot = $(mkRelDir "."),
        _clientMainFile = $(mkRelFile "Swap.juvix"),
        _clientAssertion = \program -> do
          proveTraces <- proveAndSubmit program []
          step "fetching unrevealed commits"
          resList <- pollForOutput 10000 isListUnrevealedCommitsAvailable listUnrevealedCommits
          case (proveTraces, resList ^. listUnrevealedCommitsResultCommits) of
            ([proveCommitment], [listCommitment]) ->
              liftIO $
                assertBool
                  "expected commitment from prove and list to be equal"
                  (nockmaEq proveCommitment listCommitment)
            _ ->
              throw
                ( SimpleError
                    ( mkAnsiText @Text
                        "Expected exactly one commitment to be traced by prove and one commitment to be listed by listUnrevealedCommitments"
                    )
                )
      }
  ]

allTests :: TestTree
allTests =
  testGroup
    "Anoma Client positive tests"
    (map fromClientTest clientTests)
