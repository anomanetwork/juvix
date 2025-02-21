module Base
  ( module Test.Tasty,
    module Test.Tasty.HUnit,
    module Juvix.Prelude,
    module Base,
    module Juvix.Extra.Paths,
    module Juvix.Prelude.Env,
    module Juvix.Compiler.Pipeline.Run,
    module Juvix.Compiler.Pipeline.EntryPoint.IO,
  )
where

import Anoma.Effect.Base
import Control.Exception qualified as E
import Control.Monad.Extra as Monad
import Data.Algorithm.Diff
import Data.Algorithm.DiffOutput
import GHC.Generics qualified as GHC
import Juvix.Compiler.Backend (Target (TargetAnoma))
import Juvix.Compiler.Core qualified as Core
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.Termination
import Juvix.Compiler.Nockma.Language hiding (Path)
import Juvix.Compiler.Nockma.Translation.FromTree (anomaClosure)
import Juvix.Compiler.Pipeline.EntryPoint.IO
import Juvix.Compiler.Pipeline.Loader.PathResolver
import Juvix.Compiler.Pipeline.Modular
import Juvix.Compiler.Pipeline.Modular.Run qualified as Pipeline.Modular
import Juvix.Compiler.Pipeline.Run
import Juvix.Data.Effect.TaggedLock
import Juvix.Data.SHA256 qualified as SHA256
import Juvix.Extra.Paths hiding (rootBuildDir)
import Juvix.Prelude hiding (assert, readProcess)
import Juvix.Prelude.Env
import Juvix.Prelude.Pretty
import System.Process qualified as P
import Test.Tasty
import Test.Tasty.HUnit hiding (assertFailure, testCase, testCaseSteps)
import Test.Tasty.HUnit qualified as HUnit

data AssertionDescr
  = Single Assertion
  | Steps ((String -> IO ()) -> Assertion)

data TestDescr = TestDescr
  { _testName :: String,
    _testRoot :: Path Abs Dir,
    -- | relative to root
    _testAssertion :: AssertionDescr
  }

newtype WASMInfo = WASMInfo
  { _wasmInfoActual :: Path Abs File -> IO Text
  }

makeLenses ''TestDescr

data StdlibMode
  = StdlibInclude
  | StdlibExclude
  deriving stock (Show, Eq)

data CompileMode
  = WASI StdlibMode
  | WASM WASMInfo

mkTest :: TestDescr -> TestTree
mkTest TestDescr {..} = case _testAssertion of
  Single assertion -> testCase _testName (withCurrentDir _testRoot assertion)
  Steps steps -> HUnit.testCaseSteps _testName (withCurrentDir _testRoot . steps)

withPrecondition :: Assertion -> IO TestTree -> IO TestTree
withPrecondition assertion ifSuccess = do
  E.catch (assertion >> ifSuccess) $ \case
    E.SomeException e -> return (testCase @String "Precondition failed" (assertFailure (show e)))

assertEqDiffText :: String -> Text -> Text -> Assertion
assertEqDiffText = assertEqDiff unpack

assertEqDiff :: (Eq a) => (a -> String) -> String -> a -> a -> Assertion
assertEqDiff show_ msg a b
  | a == b = return ()
  | otherwise = do
      putStrLn (pack $ ppDiff (getGroupedDiff pa pb))
      putStrLn "End diff"
      Monad.fail msg
  where
    pa = lines (show_ a)
    pb = lines (show_ b)

assertEqDiffShow :: (Eq a, Show a) => String -> a -> a -> Assertion
assertEqDiffShow = assertEqDiff show

assertCmdExists :: Path Rel File -> Assertion
assertCmdExists cmd =
  assertBool ("Command: " <> toFilePath cmd <> " is not present on $PATH")
    . isJust
    =<< findExecutable cmd

testTaggedLockedToIO :: (MonadIO m) => Sem PipelineAppEffects a -> m a
testTaggedLockedToIO =
  runM
    . ignoreLogger
    . runReader testPipelineOptions
    . runTaggedLock LockModeExclusive

testRunIO ::
  forall a m.
  (MonadIO m) =>
  EntryPoint ->
  Sem (PipelineEff PipelineAppEffects) a ->
  m (ResolverState, PipelineResult a)
testRunIO e =
  testTaggedLockedToIO
    . runIO defaultGenericOptions e

testRunIOModular ::
  forall a m.
  (MonadIO m) =>
  Maybe Core.TransformationId ->
  EntryPoint ->
  (forall r. Core.ModuleTable -> Sem (ModularEff r) a) ->
  m (Either JuvixError (ModuleId, a))
testRunIOModular checkId entry f = do
  entry' <- setEntryPointSHA256 entry
  testTaggedLockedToIO $
    Pipeline.Modular.runIOEitherModular checkId entry' f

setEntryPointSHA256 :: (MonadIO m) => EntryPoint -> m EntryPoint
setEntryPointSHA256 entry =
  case entry ^. entryPointModulePath of
    Nothing -> return entry
    Just sourceFile -> do
      sha256 <-
        runM
          . runFilesIO
          . SHA256.digestFile
          $ sourceFile
      return $ set entryPointSHA256 (Just sha256) entry

testDefaultEntryPointIO :: (MonadIO m) => Path Abs Dir -> Path Abs File -> m EntryPoint
testDefaultEntryPointIO cwd mainFile =
  testTaggedLockedToIO $
    defaultEntryPointIO' cwd (Just mainFile)

testDefaultEntryPointNoFileIO :: Path Abs Dir -> IO EntryPoint
testDefaultEntryPointNoFileIO cwd = testTaggedLockedToIO (defaultEntryPointIO' cwd Nothing)

testRunIOEither ::
  EntryPoint ->
  Sem (PipelineEff PipelineAppEffects) a ->
  IO (Either JuvixError (ResolverState, PipelineResult a))
testRunIOEither entry =
  testTaggedLockedToIO
    . runIOEither entry

testRunIOEitherTermination ::
  EntryPoint ->
  Sem (Termination ': PipelineEff PipelineAppEffects) a ->
  IO (Either JuvixError (ResolverState, PipelineResult a))
testRunIOEitherTermination entry =
  testRunIOEither entry
    . evalTermination iniTerminationState

assertFailure :: (MonadIO m) => String -> m a
assertFailure = liftIO . HUnit.assertFailure

runSimpleErrorHUnit :: (Members '[EmbedIO] r) => Sem (Error SimpleError ': r) a -> Sem r a
runSimpleErrorHUnit m = do
  res <- runError m
  case res of
    Left (SimpleError msg) -> assertFailure (toPlainString msg)
    Right r -> return r

wantsError ::
  forall err b.
  (Generic err, GenericHasConstructor (GHC.Rep err)) =>
  (b -> err) ->
  Path Abs File ->
  err ->
  Maybe String
wantsError wanted file actualErr
  | genericSameConstructor wantedErr actualErr = Nothing
  | otherwise =
      Just
        ( "In "
            <> prettyString file
            <> "\nExpected "
            <> genericConstructorName wantedErr
            <> "\nFound    "
            <> genericConstructorName actualErr
        )
  where
    wantedErr :: err
    wantedErr = wanted impossible

-- | The same as `P.readProcess` but instead of inheriting `stderr` redirects it
-- to the child's `stdout`.
readProcess :: FilePath -> [String] -> Text -> IO Text
readProcess = readProcessCwd' Nothing Nothing

readProcessWithEnv :: [(String, String)] -> FilePath -> [String] -> Text -> IO Text
readProcessWithEnv env = readProcessCwd' (Just env) Nothing

readProcessCwd :: FilePath -> FilePath -> [String] -> Text -> IO Text
readProcessCwd cwd = readProcessCwd' Nothing (Just cwd)

readProcessCwd' :: Maybe [(String, String)] -> Maybe FilePath -> FilePath -> [String] -> Text -> IO Text
readProcessCwd' menv mcwd cmd args stdinText =
  withTempDir'
    ( \dirPath -> do
        (_, hin) <- openTempFile dirPath "stdin"
        (_, hout) <- openTempFile dirPath "stdout"
        hPutStr hin stdinText
        hSeek hin AbsoluteSeek 0
        (_, _, _, ph) <-
          P.createProcess_
            "readProcess"
            (P.proc cmd args)
              { P.std_in = P.UseHandle hin,
                P.std_out = P.UseHandle hout,
                P.std_err = P.UseHandle hout,
                P.cwd = mcwd,
                P.env = menv
              }
        P.waitForProcess ph
        hSeek hout AbsoluteSeek 0
        r <- hGetContents hout
        hClose hin
        hClose hout
        return r
    )

to3DigitString :: Int -> Text
to3DigitString n
  | n < 10 = "00" <> show n
  | n < 100 = "0" <> show n
  | n < 1000 = show n
  | otherwise = error ("The given number has more than 3 digits. Given number = " <> prettyText n)

-- | E.g. Test001: str
numberedTestName :: Int -> Text -> Text
numberedTestName i str = "Test" <> to3DigitString i <> ": " <> str

testCase :: (HasTextBackend str) => str -> Assertion -> TestTree
testCase name = HUnit.testCase (toPlainString name)

testCaseSteps :: (HasTextBackend str) => str -> ((Text -> IO ()) -> Assertion) -> TestTree
testCaseSteps name f = HUnit.testCaseSteps (toPlainString name) (\sf -> f (sf . unpack))

withRootTmpCopy :: Path Abs Dir -> (Path Abs Dir -> IO a) -> IO a
withRootTmpCopy root action = withSystemTempDir "test" $ \tmpRootDir -> do
  copyDirRecur root tmpRootDir
  action tmpRootDir

compileMain :: Bool -> Path Rel Dir -> Path Rel File -> Path Abs Dir -> IO AnomaResult
compileMain enableDebug relRoot mainFile rootCopyDir = do
  let testRootDir = rootCopyDir <//> relRoot
  entryPoint <-
    set entryPointPipeline (Just PipelineExec)
      . set entryPointTarget (Just TargetAnoma)
      . set entryPointDebug enableDebug
      <$> testDefaultEntryPointIO testRootDir (testRootDir <//> mainFile)
  (over anomaClosure removeInfoUnlessDebug) . (^. pipelineResult) . snd <$> testRunIO entryPoint upToAnoma
  where
    removeInfoUnlessDebug :: Term Natural -> Term Natural
    removeInfoUnlessDebug
      | enableDebug = id
      | otherwise = removeInfoRec

envAnomaPath :: (MonadIO m) => m AnomaPath
envAnomaPath = AnomaPath <$> getAnomaPathAbs
